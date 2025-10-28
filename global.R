#########################
#####   Libraries   #####

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinyauthr))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(DBI))
suppressPackageStartupMessages(library(RSQLite))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(tools))
suppressPackageStartupMessages(library(sodium))

#########################
#####   Functions   #####

# https://www.r-bloggers.com/2022/03/closing-database-connections-in-r-packages/
# Add constructs for proper database interaction
DBMP_ENV <- new.env()
DBMP_DRIVER <- RSQLite::SQLite()
DBMP_DBNAME <- "dbmp.sqlite"

# When starting, good idea to check if already connected
if (is.null(DBMP_ENV$conn)) {
    DBMP_ENV$conn <- DBI::dbConnect(DBMP_DRIVER, DBMP_DBNAME)
} else {
    # Closing the existing connection if already open
    if (DBI::dbIsValid(DBMP_ENV$conn)) {
        DBI::dbDisconnect(DBMP_ENV$conn)
    }
    DBMP_ENV$conn <- DBI::dbConnect(DBMP_DRIVER, DBMP_DBNAME)
}

# Disconnect from the database when done using the dashboard
shiny::onStop(function() {
    message("Session ended, closing connection to dbMP ...")
    if (DBI::dbIsValid(DBMP_ENV$conn)) {
        DBI::dbDisconnect(DBMP_ENV$conn)
    }
})

# credit: https://paulc91.github.io/shinyauthr/
# This function must accept two parameters: user and sessionid. It will be called whenever the user
# successfully logs in with a password. This function saves to your database.
add_sessionid_to_audit <- function(user, sessionid, conn = DBMP_ENV$conn) {
    # Capture new login details
    new_session <- data.frame("user" = user, "sessionid" = sessionid, "logintime" = as.character(now()), stringsAsFactors = FALSE)
    
    # Now add to table
    #DBI::dbAppendTable(conn = conn, name = "audit", value = new_session)
    tryCatch({
      DBI::dbAppendTable(conn = conn, name = "audit", value = new_session)
    }, error = function(e) {
      # This will print a detailed error to your R console if the database write fails
      cli::cli_alert_danger("DATABASE ERROR in add_sessionid_to_audit: ")
      print(e)
    })
}

# This function must return a data.frame with columns "user" and "sessionid" Other columns are also okay
# and will be made available to the app after log in as columns in credentials()$user_auth
get_sessionids_from_audit <- function(database = DBMP_ENV$conn, expiry = cookie_expiry) {
    audit_table <- DBI::dbReadTable(conn = database, name = "audit") %>%
        dplyr::mutate(logintime = lubridate::ymd_hms(logintime)) %>%
        dplyr::as_tibble() %>%
        dplyr::filter(logintime > now() - lubridate::days(expiry))
    return(audit_table)
}

# Get the next available Patient Master ID (pmid)
#
# Queries the database to find the highest existing pmid, parses the numeric
# portion, increments it by one, and formats it back into the standard
# "DBMP#####" format.
get_next_pmid <- function(database) {
  # Get the last patient master ID and isolate the numeric index
  # Use parameterized queries to prevent SQL injection
  query <- "SELECT pmid FROM patients ORDER BY pmid DESC LIMIT 1"
  last_pmid_df <- DBI::dbGetQuery(conn = database, statement = query)
  
  # Sanity checks
  if (nrow(last_pmid_df) == 0) {
    # If the table is empty, start the sequence at 1
    new_pmid_num <- 1
  } else {
    # This prevents an error if strsplit returns unexpected results
    parts <- strsplit(x = last_pmid_df$pmid, split = "DBMP")[[1]]
    last_pmid_num <- as.numeric(parts[2])
    # Iterate the last pmid numeric index and create output string
    new_pmid_num <- last_pmid_num + 1
  }
  
  new_pmid_string <- sprintf("DBMP%05d", new_pmid_num)
  return(new_pmid_string)
}

# This function gets the last PMID in the table, then creates sequential new PMIDs for bulk upload
generate_pmids_bulk <- function(database, n) {
  query <- "SELECT pmid FROM patients ORDER BY pmid DESC LIMIT 1"
  last_pmid_df <- DBI::dbGetQuery(conn = database, statement = query)
  
  if (nrow(last_pmid_df) == 0) {
    start_num <- 1
  } else {
    last_pmid_num <- as.numeric(sub("DBMP", "", last_pmid_df$pmid))
    start_num <- last_pmid_num + 1
  }
  
  # Create a sequence of numbers
  new_pmid_nums <- seq(from = start_num, by = 1, length.out = n)
  
  # Format them all into strings
  sapply(new_pmid_nums, function(num) sprintf("DBMP%05d", num))
}

# Create the Morgan lab specific sample ID for each dbMP patient, it will be a combination of the numeric portion of the patient ID plus the sample type
generate_sid <- function(database, patient_id, sample_type) {
  # Use parameterized queries to prevent SQL injection
  query <- "SELECT COUNT(*) FROM samples WHERE pmid = ? AND type = ?"
  query_count <- DBI::dbGetQuery(conn = database, statement = query, params = list(patient_id, sample_type))
  unique_sample_count < query_count[[1]] + 1
  
  unique_sample_count_string <- sprintf("%02d", unique_sample_count)
  
  # Prep the sample ID string
  patient_num <- sub("DBMP", "", patient_id)
  new_sid <- paste("ML", patient_num, sample_type, unique_sample_count_string, sep = "-")
  return(new_sid)
}

# Generate unique Sample IDs (sid) in bulk.
#
# This is an efficient version for file uploads. It queries the database once
# to get all existing sample counts for the relevant patients and then
# generates the new SIDs in R, avoiding slow row-by-row database queries.
generate_sids_bulk <- function(database, data) {
  # Get the unique pmids from the input data frame
  pmids_to_check <- unique(data$pmid)
  
  # Let glue_sql handle the interpolation of the vector directly.
  # By putting an asterisk (*) after the variable, we tell glue to
  # collapse the vector with commas.
  sql_statement <- glue::glue_sql(
    "SELECT pmid, type, COUNT(*) as existing_count 
     FROM samples 
     WHERE pmid IN ({pmids_to_check*}) 
     GROUP BY pmid, type",
    .con = database
  )
  
  # Count the number of entries
  existing_counts_df <- DBI::dbGetQuery(conn = database, statement = sql_statement)
  
  # Join the existing counts back to the new data
  data_with_counts <- data %>%
    left_join(existing_counts_df, by = c("pmid", "type")) %>%
    mutate(existing_count = ifelse(is.na(existing_count), 0, existing_count)) %>% # If a pmid/type combo didn't exist, its count is NA. Change NAs to 0.
    group_by(pmid, type) %>%  # Calculate the new sequential number for each row
    mutate(new_seq = existing_count + row_number()) %>% # The new sequence number is the existing count + a cumulative count within the uploaded data
    ungroup()
  
  # Generate the final SID strings
  new_sids <- purrr::pmap_chr(
    list(
      data_with_counts$pmid,
      data_with_counts$type,
      data_with_counts$new_seq
    ),
    function(pmid, type, seq) {
      patient_num <- sub("DBMP", "", pmid)
      seq_string <- sprintf("%02d", seq)
      paste("ML", patient_num, type, seq_string, sep = "-")
    }
  )

  return(new_sids)
}

#########################
#####   Execution   #####

# Add authentication layer
# set up login credentials
user_creds <- readRDS("user_creds.rds")

# Table descriptions and definitions for display
table_descript <- list(
    patients = "Patient-centric core of database, includes: Patient Master ID (pmid), Institution, Institution ID (iid), Race, and Sex",
    samples = "Sample-centric core of database, includes: Patient Master ID (pmid), Sample ID (sid), Institution ID (iid), Visit Date, Timepoint (tpt), Age, Diagnosis, Sample Type, and Institution-specific Specimen ID",
    study = "Patient-centric view and tracking of involvement in studies, includes: Patient Master ID (pmid), Sample ID (sid), Institution ID (iid), T-cell Flow Panel completion record, Microbiome completion record, Dietary Questionnaire completion record",
    flow = "Sample-centric tracking of flow cytometry data, includes: Sample ID (sid), Institution ID (iid), Sample Type and FlowJo Data Readout",
    audit = "Audit log of usage: Username, Session ID, Login Time and Date"
)

patients_table_def <- data.frame(
    "column_id" = c("pmid", "institution", "iid", "race", "sex"),
    "name" = c("Patient Master ID", "Institution", "Institution ID", "Race", "Sex"),
    "data_type" = c("Unique ID", "String", "Unique ID", "String", "String"),
    "example" = c("DBMP00001", "NYU", "NYU00001", "White", "Female")
)

patient_template_info <- data.frame(
  `Column Name` = c("institution", "iid", "race", "sex"),
  `Description` = c(
    "The institution where the patient is being seen or 'Public' if from published dataset.",
    "The unique Institution ID for the patient. CANNOT already exist in the database.",
    "The patient's self-reported race.",
    "The patient's sex."
  ),
  `Example` = c("NYU", "NYU5678", "White", "Female"),
  `Allowed Values` = c(
    "NYU, UAB, UMiami, MDAnderson, MSKCC, Public",
    "Free text",
    "Asian, Black or African American, Hispanic, White, Other, or NA",
    "Female, Male, or NA"
  ),
  check.names = FALSE
)

samples_table_def <- data.frame(
    "column_id" = c("pmid", "sid", "iid", "visit_date", "tpt", "age", "diagnosis", "type", "specimenid"),
    "name" = c("Patient Master ID", "Sample ID", "Institution ID", "Visit Date", "Timepoint", "Age", "Diagnosis", "Type", "Specimen ID"),
    "data_type" = c("Unique ID", "Unique ID", "Unique ID", "YYYY-MM-DD", "String", "Integer", "String", "String", "String"),
    "example" = c("DBMP00001", "ML-00001-BM-01", "NYU00001", "2022-11-03", "Baseline", "67", "MGUS", "BM", "NA")
)

sample_template_info <- data.frame(
  `Column Name` = c("iid", "visit_date", "tpt", "age", "diagnosis", "type", "specimenid"),
  `Description` = c(
    "Institution ID for the patient. MUST already exist in the 'patients' table.",
    "Date the sample was collected at site.",
    "Study timepoint for the sample at collection (e.g., Baseline, Progression).",
    "Patient's age in years at the time of the visit.",
    "Patient's diagnosis at the time of sample collection. One of MGUS, SMM, MM, RRMM, and PCL",
    "The type of biological sample. One of Bone Marrow (BM), Peripheral Blood (PB), or Stool (ST)",
    "Institution's unique aliquot ID for the physical specimen (use 'NA' if not applicable)."
  ),
  `Example` = c("NYU1234", "2023-10-27", "Baseline", 65, "MGUS", "BM", "SAMP-001"),
  `Allowed Values` = c(
    "Any existing IID",
    "YYYY-MM-DD format",
    "Free text",
    "Numeric",
    "MGUS, SMM, MM, RRMM, PCL",
    "BM, PB, ST",
    "Free text or NA"
  ),
  # This makes sure the column names are not converted to `Column.Name`
  check.names = FALSE 
)

flow_table_def <- data.frame(
  "column_id" = c("sid", "iid", "type", "CD3_pos", "CD4", "CD8", "Th22", "total_Th2", "total_Th17", "Th2_EM", "Th17_EM", "Th2_CM", "Th17_CM", "CD4_CM", "CD4_EM", "CD4_naive", "CD4_effector", "CD8_CM", "CD8_EM", "CD8_naive", "CD8_effector"),
  "name" = c("Sample ID", "Type", "Institution ID", "CD3 positive fraction", "CD4 fraction", "CD8 fraction", "Th22 fraction", "Total Th2 fraction", "Total Th17 fraction", "Effector memory Th2 fraction", "Effector memory Th17 fraction", "Central memory Th2 fraction", "Central memory Th17 fraction", "Central memory CD4 fraction", "Effector memory CD4 fraction", "Naive CD4 fraction", "Effector CD4 fraction", "Central memory CD8 fraction", "Effector memory CD8 fraction", "Naive CD8 fraction", "Effector CD8 fraction"),
  "data_type" = c("Unique ID", "Unique ID", "String", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float", "Float"),
  "example" = c("ML-00001-PB-01", "NYU00001", "PB", "2.72", "63.6", "29.5", "0.084", "3.64", "3.22", "1.74", "2.97", "1.9", "0.25", "9.91", "20.7", "28.2", "4.79", "0.27", "11.1", "3.04", "15")
)

audit_table_def <- data.frame(
  "column_id" = c("user", "sessionid", "logintime"),
  "name" = c("Username", "Session ID", "Login Time and Date"),
  "data_type" = c("Unique ID", "String", "YYYY-MM-DD h:m:s:ms"),
  "example" = c("admin", "4jYrb4=lkL0Lm9nCu2cevxlshwe3", "2024-12-12 10:09:50.209595")
)

# For login audit, the session cookie will expire in seven days
cookie_expiry <- 7

# Custom CSS to mimic cBioPortal's look and feel
custom_css <- tags$head(
  tags$style(HTML("
      /* Main header */
      .skin-blue .main-header .navbar {
        background-color: #2c3e50; /* A dark blue-gray */
      }
      .skin-blue .main-header .logo {
        background-color: #2c3e50;
        font-weight: bold;
        font-size: 24px;
      }
      .skin-blue .main-header .logo:hover {
        background-color: #34495e;
      }

      /* Navigation tabs in header */
      .nav-tabs-custom < .nav-tabs < li < a {
        font-weight: bold;
        font-size: 16px;
      }
      .nav-tabs-custom < .nav-tabs < li.active < a,
      .nav-tabs-custom < .nav-tabs < li.active:hover < a {
        background-color: #ecf0f5; /* Light gray to match body */
        border-bottom-color: transparent;
      }

      /* Body content */
      .content-wrapper {
        background-color: #ecf0f5; /* Light gray background */
      }
      
      /* Boxes/Cards */
      .box {
        border-top-width: 3px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
      }
    "))
)

# The login UI
login_ui <- shinyauthr::loginUI(
  id = "login",
  title = "dbMP: Database of Myelomas & Precursors",
  user_title = "Username",
  pass_title = "Password",
  login_title = "Log in",
  error_message = "Invalid username or password!",
  additional_ui = NULL,
  cookie_expiry = cookie_expiry
)

# the logout button for the UI
logout_ui <- shinyauthr::logoutUI(id = "logout",
                                  label = "Log Out",
                                  icon = NULL,
                                  class = "btn-danger",
                                  style = "color: white;")

# The main dashboard UI that will be shown after login
dashboard_ui <- dashboardPage(
  skin = "blue",
  header = dashboardHeader(
    title = "dbMP: Database of Myelomas & Precursors",
    titleWidth = 600,
    tags$li(
      class = "dropdown",
      style = "padding: 10px;",
      # This is our placeholder for the dynamic user info
      uiOutput("user_info_header")
    ),
    # Dropdown menu for user info and logout
    tags$li(
      class = "dropdown",
      style = "padding: 8px;",
      shinyauthr::logoutUI("logout")
    )
  ),
  # We are not using a sidebar in this design
  sidebar = dashboardSidebar(disable = TRUE),
  
  body = dashboardBody(
    useShinyjs(), # Enable shinyjs for the app
    custom_css, # Apply our custom styles
    
    # Main content area with tabs, similar to cBioPortal
    tabsetPanel(
      id = "main_tabs",
      type = "tabs",
      
      # --- Home/Dashboard Tab ---
      tabPanel(
        "Dashboard",
        icon = icon("dashboard"),
        fluidRow(
          h2("dbMP Overview", style = "padding-left: 15px;"),
          p("Summary statistics for all patients and samples.", style = "padding-left: 15px; margin-bottom: 20px;"),
          # Placeholders for summary boxes
          valueBoxOutput("total_patients_box", width = 3),
          valueBoxOutput("total_samples_box", width = 3)
        )
      ),
      
      # --- Data Exploration Tab ---
      tabPanel(
        "Explore Data",
        icon = icon("table"),
        fluidRow(
          box(
            title = "Select and View Table",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("pick_table_to_view", "Select Table:", choices = c("Patients" = "patients", "Samples" = "samples", "Flow Cytometry" = "flow")),
            DT::dataTableOutput("main_datatable")
          )
        )
      ),
      
      # --- Data Entry Tab ---
      tabPanel(
        "Add New Entry",
        icon = icon("plus"),
        # Use a sub-tabset to organize different entry types
        tabsetPanel(
          id = "add_entry_tabs",
          type = "pills",
          
          # ===================================================================
          #  Add New Patient Tab
          # ===================================================================
          tabPanel(
            "New Patient Record",
            # We'll use another tabset to switch between Manual and File upload
            tabsetPanel(
              id = "patient_add_method_tabs",
              
              # --- Manual Entry Tab ---
              tabPanel(
                "Manual Entry",
                fluidRow(
                  box(
                    title = "Step 1: Enter Patient Details",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    
                    # A row of input fields
                    fluidRow(
                      column(3,
                             # This will be populated from the server
                             selectInput("patient_pmid", "Patient Master ID", choices = NULL)
                      ),
                      column(3,
                             selectInput("patient_institution", "Institution", choices = c("UMiami", "MDAnderson", "MSKCC", "NYU", "UAB","Public"))
                      ),
                      column(3,
                             textInput("patient_iid", "Institution ID", placeholder = "e.g., NYU1234")
                      )
                    ),
                    fluidRow(
                      column(3,
                             selectInput("patient_race", "Race", choices = c("Asian", "Black or African American", "Hispanic", "White", "Other", "NA"))
                      ),
                      column(3,
                             selectInput("patient_sex", "Sex", choices = c("Female", "Male", "NA"))
                      )
                    )
                  ),
                  
                  # Preview button
                  actionButton("action_preview_patient", "Step 2: Preview Record", 
                               icon = icon("eye"), class = "btn-info", 
                               style = "margin-left: 15px; margin-bottom: 20px;"),
                  
                  # Placeholder for the preview and submission UI
                  uiOutput("patient_manual_preview_ui")
                )
              ),
              
              # --- File Upload Tab ---
              tabPanel(
                "File Upload",
                fluidRow(
                  box(
                    title = "Step 1: Download 'New Patient' record template and Upload File",
                    status = "primary", solidHeader = TRUE, width = 12,
                    p("To add multiple patient records at once, please download the template, fill it out, and upload it below. All columns are required."),
                    
                    # Buttons for template and upload
                    downloadButton("download_patient_template", "Download Template (.csv)"),
                    hr(),
                    
                    h4("Template Column Reference"),
                    tableOutput("patient_template_reference_table"),
                    
                    hr(),
                    fileInput("patient_file_upload", "Upload Completed New Patient File",
                              multiple = FALSE,
                              accept = c("text/csv", ".csv"))
                  ),
                  
                  # Preview button
                  actionButton("action_preview_patient_upload", "Step 2: Validate and Preview Records",
                               icon = icon("eye"), class = "btn-info",
                               style = "margin-left: 15px; margin-bottom: 20px;"),
                  
                  # Placeholder for the preview UI
                  uiOutput("patient_upload_preview_ui")
                )
              )
            )
          ),
          
          # ===================================================================
          #  Add New Sample Tab
          # ===================================================================
          tabPanel(
            "New Sample Record",
            # We'll use another tabset to switch between Manual and File upload
            tabsetPanel(
              id = "sample_add_method_tabs",
              
              # --- Manual Entry Tab ---
              tabPanel(
                "Manual Entry",
                fluidRow(
                  box(
                    title = "Step 1: Enter Sample Details",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    
                    # Select an existing sample
                    h4("Select an Existing Institution ID"),
                    selectInput("sample_iid_select", "Filter by Institution ID:", 
                                choices = NULL), # We'll populate this from the server
                    
                    hr(), # A horizontal line to separate sections
                    
                    # This placeholder will be filled with a dropdown of sample IDs
                    uiOutput("sample_pmid_select_ui"),
                    
                    # --- Sample Details ---
                    # Show the PMID of the selected IID
                    fluidRow(
                      column(3, dateInput("sample_visit_date", "Visit Date")),
                      column(3, textInput("sample_tpt", "Timepoint", placeholder = "e.g., Baseline")),
                      column(3, numericInput("sample_age", "Age at Visit", value = NA, min = 18)),
                      column(3, selectInput("sample_diagnosis", "Diagnosis", choices = c("MGUS", "SMM", "MM", "RRMM", "PCL")))
                    ),
                    fluidRow(
                      column(3, selectInput("sample_type", "Sample Type", choices = c("BM", "PB", "ST"))),
                      column(3, textInput("sample_specimenid", "Specimen ID (optional)", value = "NA"))
                    )
                  ),
                  
                  # Preview Button
                  actionButton("action_preview_sample", "Step 2: Preview Record", 
                               icon = icon("eye"), class = "btn-info", 
                               style = "margin-left: 15px; margin-bottom: 20px;"),
                  
                  # Placeholder for the preview and submission UI
                  uiOutput("sample_manual_preview_ui")
                )
              ),
              
              # --- File Upload Tab ---
              tabPanel(
                "File Upload",
                fluidRow(
                  box(
                    title = "Step 1: Download Template and Upload File",
                    status = "primary", solidHeader = TRUE, width = 12,
                    p("To add multiple sample records at once, please download the template, fill it out, and upload it below. All columns are required."),
                    
                    # Buttons for template and upload
                    downloadButton("download_sample_template", "Download Template (.csv)"),
                    hr(),
                    
                    h4("Template Column Reference"),
                    tableOutput("sample_template_reference_table"),
                    
                    hr(),
                    fileInput("sample_file_upload", "Upload Completed Sample File",
                              multiple = FALSE,
                              accept = c("text/csv", ".csv"))
                  ),
                  
                  # Preview button
                  actionButton("action_preview_sample_upload", "Step 2: Validate and Preview Records",
                               icon = icon("eye"), class = "btn-info",
                               style = "margin-left: 15px; margin-bottom: 20px;"),
                  
                  # Placeholder for the preview UI
                  uiOutput("sample_upload_preview_ui")
                )
              )
            )
          ),
          
          # ===================================================================
          #  Add New Flow Tab
          # ===================================================================
          tabPanel(
            "Flow Cytometry Data",
            fluidRow(
              box(
                title = "Step 1: Select Sample and Upload File",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                
                # Select an existing sample
                h4("Select an Existing Sample"),
                selectInput("flow_iid_select", "Filter by Institution ID:", 
                            choices = NULL), # We'll populate this from the server
                
                # This placeholder will be filled with a dropdown of sample IDs
                uiOutput("flow_sid_select_ui"),
                
                hr(), # A horizontal line to separate sections
                
                # Upload the CSV file
                h4("Upload FlowJo CSV File"),
                fileInput("flow_file_upload", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"))
              ),
              
              # The preview button
              actionButton("action_preview_flow", "Step 2: Preview Data", 
                           icon = icon("eye"), class = "btn-info", 
                           style = "margin-left: 15px; margin-bottom: 20px;"),
              
              # This placeholder will contain the preview table and the final submit button
              uiOutput("flow_preview_ui")
            )
          )
          
        )
      )
    )
  )
)

#study_table_def <- data.frame(
#    "column_id" = c("pmid", "sid", "iid", "flow_date", "flow", "microbiome_contact_date", "microbiome_confirm_date", "microbiome_mskcc_date", "microbiome", "diet_date", "diet"),
#    "name" = c("Patient Master ID", "Sample ID", "Institution ID", "T Cell Flow Panel Completion Date", "T Cell Flow Panel", "Microbiome Contact Date", "Microbiome Confirm Date", "Microbiome MSKCC Date", "Microbiome", "Dietary Questionnaire Completion Date", "Dietary Questionnaire"),
#    "data_type" = c("Unique ID", "Unique ID", "Unique ID", "YYYY-MM-DD", "String", "YYYY-MM-DD", "YYYY-MM-DD", "YYYY-MM-DD", "String", "YYYY-MM-DD", "String"),
#    "example" = c("DBMP00001", "ML-00001-BM", "NYU00001", "2022-07-01", "Done", "2022-03-29", "2022-03-29", "2022-03-29", "Done", "NA", "Not Done")
#)
