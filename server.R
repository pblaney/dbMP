# Build the back-end server to interact and react to the user's commands in the UI
function(input, output, session) {
  # --- 1. Authentication ---
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  credentials <- shinyauthr::loginServer(
    id = "login",
    data = user_creds,
    user_col = "user",
    pwd_col = "password",
    sodium_hashed = TRUE,
    cookie_logins = TRUE,
    sessionid_col = "sessionid",
    cookie_getter = get_sessionids_from_audit,
    cookie_setter = add_sessionid_to_audit,
    log_out = reactive(logout_init())
  )
  
  # --- 2. Main UI Rendering ---
  output$dashboard_ui_holder <- renderUI({
    if (isTruthy(credentials()$user_auth)) { # Use isTruthy() for robustness
      # If authenticated, show the main dashboard UI
      dashboard_ui
    } else {
      # Otherwise, show the login screen
      login_ui
    }
  })
  
  output$user_info_header <- renderUI({
    # Wait until the user is authenticated
    req(credentials()$user_auth)
    
    # Get username from the credentials()$info data frame
    # Make sure the column name 'user' matches your user_creds file
    username <- credentials()$info$user
    
    # Create the UI element with some styling for the header
    div(
      style = "padding: 14px; padding-top: 16px; color: black; font-size: 16px;",
            icon("user-check"),
            username
        )
  })
  
  # --- 3. Dashboard Tab Logic ---
  # Reactive poll to periodically check for new data
  db_data <- reactivePoll(5000, session,
                          checkFunc = function() {
                            # A simple check: count rows in patients and samples tables
                            list(
                              p_count = dbGetQuery(DBMP_ENV$conn, "SELECT COUNT(*) FROM patients")[[1]],
                              s_count = dbGetQuery(DBMP_ENV$conn, "SELECT COUNT(*) FROM samples")[[1]]
                            )
                          },
                          valueFunc = function() {
                            # Return a list of the full tables
                            list(
                              patients = dbReadTable(DBMP_ENV$conn, "patients"),
                              samples = dbReadTable(DBMP_ENV$conn, "samples"),
                              flow = dbReadTable(DBMP_ENV$conn, "flow")
                            )
                          }
  )
  
  output$total_patients_box <- renderValueBox({
    req(credentials()$user_auth) # Ensure user is logged in
    total_patients <- nrow(db_data()$patients)
    valueBox(
      value = total_patients,
      subtitle = "Total Patients",
      icon = icon("user-injured"),
      color = "aqua"
    )
  })
  
  output$total_samples_box <- renderValueBox({
    req(credentials()$user_auth)
    total_samples <- nrow(db_data()$samples)
    valueBox(
      value = total_samples,
      subtitle = "Total Samples",
      icon = icon("vial"),
      color = "teal"
    )
  })
  
  # --- 4. Explore Data Tab Logic ---
  output$main_datatable <- DT::renderDataTable({
    req(credentials()$user_auth, input$pick_table_to_view)
    
    # Get the selected table from our reactive data list
    table_to_show <- db_data()[[input$pick_table_to_view]]
    
    DT::datatable(
      table_to_show,
      options = list(scrollX = TRUE, pageLength = 15),
      rownames = FALSE,
      filter = 'top' # Add column filters
    )
  })

  # ===================================================================
  #  Manual "Add New Patient" Workflow
  # ===================================================================
  
  # --- A. Reactive Values and Setup ---
  # A reactiveVal to store the patient data frame for preview
  rv_patient <- reactiveValues(preview_df = NULL)
  
  # Populate the Patient Master ID dropdown once and keep it updated
  observe({
    req(credentials()$user_auth, db_data())
    updateSelectInput(session, "patient_pmid", 
                      choices = get_next_pmid(database = DBMP_ENV$conn))
  })
  
  # --- B. Generate Preview on Button Click (with Validation) ---
  observeEvent(input$action_preview_patient, {
    
    # --- Validation Checks ---
    iid_input <- trimws(input$patient_iid)
    
    # 1. Check for an empty string, even if the user only entered spaces
    if (nchar(iid_input) == 0) {
      showNotification("Error: Institution ID cannot be an empty string or only contain spaces.", type = "error")
      return() # Stop execution
    }
    
    # 2. Check if the input is 'NA' (case-insensitive)
    if (toupper(iid_input) == "NA") {
      showNotification("Error: Institution ID cannot be 'NA'.", type = "error")
      return() # Stop execution
    }
    
    # 3. Check for forbidden characters using a regular expression
    if (grepl("[!@#$%^&*()+={}|:;'`~,<>?/]", iid_input)) {
      showNotification("Error: Institution ID contains invalid characters.", type = "error")
      return() # Stop execution
    }
    
    # 4. Check if the Institution ID already exists in the database
    is_duplicate <- dbGetQuery(
      DBMP_ENV$conn, 
      "SELECT COUNT(*) FROM patients WHERE iid = ?", 
      params = list(iid_input) # Use the trimmed input
    )[[1]] > 0
    
    if (is_duplicate) {
      showNotification("Error: This Institution ID already exists in the database.", type = "error")
      return() # Stop execution
    }
    
    # --- If all checks pass, proceed to create the preview ---
    
    df <- data.frame(
      pmid = input$patient_pmid,
      institution = input$patient_institution,
      iid = iid_input, # Use the trimmed and validated input
      race = input$patient_race,
      sex = input$patient_sex,
      stringsAsFactors = FALSE
    )
    
    rv_patient$preview_df <- df
  })
  
  # --- C. Render the Preview UI ---
  output$patient_manual_preview_ui <- renderUI({
    req(rv_patient$preview_df)
    
    box(
      title = "Step 3: Confirm and Submit Record",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      p("Please review the record below. This is the exact row that will be added to the database."),
      
      # Placeholder for the preview table
      DT::dataTableOutput("patient_manual_preview_table"),
      br(),
      
      # Final submission button
      actionButton("action_submit_patient", "Submit to Database", 
                   icon = icon("check"), class = "btn-success")
    )
  })
  
  # Server-side renderer for the preview table
  output$patient_manual_preview_table <- DT::renderDataTable({
    req(rv_patient$preview_df)
    DT::datatable(
      rv_patient$preview_df,
      options = list(scrollX = TRUE, dom = 't'),
      rownames = FALSE
    )
  })
  
  # --- D. Submit Data to Database on Final Button Click ---
  observeEvent(input$action_submit_patient, {
    req(rv_patient$preview_df)
    
    tryCatch({
      # Append the prepared data frame to the SQLite table
      DBI::dbAppendTable(DBMP_ENV$conn, "patients", rv_patient$preview_df)
      
      showNotification("Success! New patient record has been added.", type = "message")
      
      # --- Reset the Form ---
      # 1. Hide the preview box
      rv_patient$preview_df <- NULL
      
      # 2. Reset all input fields using shinyjs
      shinyjs::reset("patient_institution")
      shinyjs::reset("patient_iid")
      shinyjs::reset("patient_race")
      shinyjs::reset("patient_sex")
      
      # 3. Manually update the PMID to the *next* available ID
      updateSelectInput(session, "patient_pmid", 
                        choices = get_next_pmid(database = DBMP_ENV$conn))
      
    }, error = function(e) {
      showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
    })
  })
  
  # ///////////////////////////////////////////////////////////////////
  
  # ===================================================================
  #  File Upload "Add New Patient" Workflow
  # ===================================================================
  
  # --- A. Reactive Values ---
  rv_patient_upload <- reactiveValues(preview_df = NULL)
  
  # --- B. Template Download and Reference Table ---
  output$download_patient_template <- downloadHandler(
    filename = function() {
      paste0("dbMP_patient_template_", Sys.Date(), ".csv")
    },
    content = function(file) {
      template_df <- data.frame(
        institution = "",
        iid = "",
        race = "",
        sex = ""
      )
      write.csv(template_df, file, row.names = FALSE)
    }
  )
  
  output$patient_template_reference_table <- renderTable({
    patient_template_info
  })
  
  # --- C. Generate Preview from Uploaded File ---
  observeEvent(input$action_preview_patient_upload, {
    # Reset preview on every click
    rv_patient_upload$preview_df <- NULL
    
    tryCatch({
      req(input$patient_file_upload, message = "A file must be uploaded.")
      
      # 1. --- READ AND VALIDATE FILE STRUCTURE ---
      # Read in the data and check for errors from extra blank lines in the CSV file. (Especially important for Excel usage)
      uploaded_data <- readr::read_csv(input$patient_file_upload$datapath, show_col_types = FALSE) %>%
        filter(if_any(everything(), ~ !is.na(.)))
      
      # If, after removing empty rows, there is no data left, stop.
      if (nrow(uploaded_data) == 0) {
        stop("The uploaded file contains no valid data rows.")
      }
      
      # Check for required columns
      expected_cols <- c("institution", "iid", "race", "sex")
      if (!all(expected_cols %in% names(uploaded_data))) {
        missing_cols <- setdiff(expected_cols, names(uploaded_data))
        stop(paste("File is missing required columns:", paste(missing_cols, collapse = ", ")))
      }
      
      # 2. --- PRE-VALIDATE IIDs ---
      # Check for duplicate IIDs WITHIN the uploaded file
      duplicated_iids_infile <- uploaded_data$iid[duplicated(uploaded_data$iid)]
      if (length(duplicated_iids_infile) > 0) {
        stop(paste("The uploaded file contains duplicate Institution IDs:", paste(unique(duplicated_iids_infile), collapse = ", ")))
      }
      
      # Check for IIDs that ALREADY EXIST in the database
      existing_iids_in_db <- intersect(uploaded_data$iid, db_data()$patients$iid)
      if (length(existing_iids_in_db) > 0) {
        stop(paste("The following Institution IDs already exist in the database:", paste(existing_iids_in_db, collapse = ", ")))
      }
      
      # 3. --- VALIDATION CHECKS ---
      # Check each row of the bulk upload
      # Initialize the allowed values
      allowed_institution <- c("NYU","UAB","UMiami","MDAnderson","MSKCC","Public")
      forbidden_chars_pattern <- "[!@#$%^&*()+={}|:;'`~,<>?/]"
      allowed_race <- c("Asian","Black or African American","Hispanic","White","Other",NA)
      allowed_sex <- c("Female","Male",NA)
      
      # Initialize a list to store formatted error messages
      all_errors <- list()
      
      for(i in 1:nrow(uploaded_data)) {
        # Initialize the row to check and the errors
        row <- uploaded_data[i, ]
        row_errors <- c()
        
        # Check for an IID empty string, even if the user only entered spaces
        if (nchar(row$iid) == 0) {
          row_errors <- c(row_errors, "'iid' Institution ID cannot be an empty string or only contain spaces.")
        }
        
        # Check if the input is 'NA' (case-insensitive)
        if (toupper(row$iid) == "NA") {
          row_errors <- c(row_errors, "'iid' Institution ID cannot be 'NA'.")
        }
        
        # Check for forbidden characters using a regular expression
        if (grepl(forbidden_chars_pattern, row$iid)) {
          row_errors <- c(row_errors, "'iid' Institution ID contains invalid characters..")
        }
        
        # Check institution
        if (!(row$institution %in% allowed_institution)) {
          row_errors <- c(row_errors, paste0("'institution' must be one of: ", paste(allowed_institution, collapse=", "), "."))
        }
        
        # Check race
        if (!(row$race %in% allowed_race)) {
          row_errors <- c(row_errors, paste0("'race' must be one of: ", paste(allowed_race, collapse=", "), "."))
        }
        
        # Check sex
        if (!(row$sex %in% allowed_sex)) {
          row_errors <- c(row_errors, paste0("'sex' must be one of: ", paste(allowed_sex, collapse=", "), "."))
        }
        
        if (length(row_errors) > 0) {
          all_errors[[length(all_errors) + 1]] <- paste0("Row ", i + 1, ": ", paste(row_errors, collapse = " | "))
        }
      }
      
      # After the loop, check if the error list is populated
      if (length(all_errors) > 0) {
        error_message <- paste0(
          "Found ", length(all_errors), " errors in the uploaded file:\n",
          paste(all_errors, collapse = "")
        )
        # Use stop() to send the formatted HTML to the tryCatch error function
        stop(HTML(error_message))
      }
      
      # 4. --- DATA PREPARATION ---
      # If checks pass, add newly created PMIDs
      new_pmids <- generate_pmids_bulk(database = DBMP_ENV$conn, n = nrow(uploaded_data))
      
      final_df <- uploaded_data %>%
        mutate(pmid = new_pmids) %>%
        select(pmid, all_of(expected_cols))
      
      # Store the prepared data frame
      rv_patient_upload$preview_df <- final_df
      
    }, error = function(e) {
      # This single block now catches any error from file reading, validation, or data prep
      showNotification(
        ui = HTML(e$message), # Display the formatted error message
        type = "error",
        duration = 20
      )
    })
  })
  
  # --- D. Render the Upload Preview UI ---
  output$patient_upload_preview_ui <- renderUI({
    req(rv_patient_upload$preview_df)
    
    box(
      title = "Step 3: Confirm and Submit Records",
      status = "success", solidHeader = TRUE, width = 12,
      p("Please review the records below. PMIDs have been automatically generated. These exact rows will be added to the database."),
      DT::dataTableOutput("patient_upload_preview_table"),
      br(),
      actionButton("action_submit_patient_upload", "Submit All Records to Database", 
                   icon = icon("check-double"), class = "btn-success")
    )
  })
  
  output$patient_upload_preview_table <- DT::renderDataTable({
    req(rv_patient_upload$preview_df)
    DT::datatable(
      rv_patient_upload$preview_df,
      options = list(scrollX = TRUE, pageLength = 5),
      rownames = FALSE
    )
  })
  
  # --- E. Submit Uploaded Data to Database ---
  observeEvent(input$action_submit_patient_upload, {
    req(rv_patient_upload$preview_df)
    
    tryCatch({
      DBI::dbAppendTable(DBMP_ENV$conn, "patients", rv_patient_upload$preview_df)
      
      showNotification(paste("Success!", nrow(rv_patient_upload$preview_df), "new patient records have been added."), type = "message")
      
      # Reset the form
      rv_patient_upload$preview_df <- NULL
      shinyjs::reset("patient_file_upload")
      
    }, error = function(e) {
      showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
    })
  })
  
  # ///////////////////////////////////////////////////////////////////
  
  # ===================================================================
  #  Manual "Add New Sample" Workflow
  # ===================================================================
  
  # --- A. Reactive Values and Setup ---
  # A reactiveVal to store the sample data frame for preview
  rv_sample <- reactiveValues(preview_df = NULL)
  
  # Populate the Institution ID dropdown for the sample form
  observe({
    req(credentials()$user_auth, db_data())
    
    # Get all IIDs that exist in the patients table
    iids <- unique(db_data()$patients$iid)
    
    if (length(iids) > 0) {
      updateSelectInput(session, "sample_iid_select", choices = c("Select an Institution ID" = "", sort(iids)))
    }
  })
  
  # --- B. Dependent Display for IID and PMID ---
  # This text output shows the PMID that corresponds to the selected IID
  output$sample_pmid_select_ui <- renderUI({
    req(input$sample_iid_select, db_data())
    
    # Filter samples based on the selected IID
    associated_pmid <- db_data()$patients %>%
      filter(iid == input$sample_iid_select) %>%
      pull(pmid)

    selectInput("sample_pmid_select", "Associated PMID:", choices = sort(associated_pmid))
  })
  
  # --- C. Generate Preview on Button Click ---
  observeEvent(input$action_preview_sample, {
    # --- Validation ---
    req(input$sample_iid_select, db_data()$patients %>%
          filter(.data$iid == input$sample_iid_select) %>%
          pull(pmid))

    # --- 1. GATHER AND VALIDATE INPUTS ---
    # Use a list of validation checks
    validation_passed <- TRUE
    error_messages <- c()
    
    # Check 1: A patient must be selected
    if (!isTruthy(input$sample_iid_select)) {
      validation_passed <- FALSE
      error_messages <- c(error_messages, "An Institution ID must be selected.")
    }
    
    # Check 2: Visit Date cannot be in the future
    if (input$sample_visit_date > Sys.Date()) {
      validation_passed <- FALSE
      error_messages <- c(error_messages, "Visit Date cannot be in the future.")
    }
    
    # Check 3: Timepoint field cannot be blank
    if (!isTruthy(trimws(input$sample_tpt))) {
      validation_passed <- FALSE
      error_messages <- c(error_messages, "The Timepoint field cannot be blank.")
    }
    
    # Check 4: Age must be populated and cannot be younger than 18 and cannot be older than expected
    if (!isTruthy(input$sample_age) | input$sample_age < 18 | input$sample_age > 125) {
      validation_passed <- FALSE
      error_messages <- c(error_messages, "Age at Visit must be older than 18 and within normal human life expectancy.")
    }
    
    # Check 5: Specimen ID cannot be blank
    if (!isTruthy(trimws(input$sample_specimenid))) {
      validation_passed <- FALSE
      error_messages <- c(error_messages, "The Specimen ID field cannot be blank (use 'NA' if not applicable).")
    }
    
    # If any validation failed, show all errors and stop
    if (!validation_passed) {
      showNotification(
        HTML(paste("Please correct the following errors:",
                   "<ul>",
                   paste0("<li>", error_messages, "</li>", collapse = ""),
                   "</ul>")),
        type = "error",
        duration = 10
      )
      return() # Stop execution
    }
    
    # --- 2. If validation passes, proceed to create the preview ---
    tryCatch({
      # Generate the new Sample ID (sid)
      new_sid <- generate_sid(
        database = DBMP_ENV$conn,
        patient_id = input$sample_pmid_select,
        sample_type = input$sample_type
      )

      # --- Assemble Data Frame ---
      df <- data.frame(
        pmid = input$sample_pmid_select,
        sid = new_sid,
        iid = input$sample_iid_select,
        visit_date = as.character(input$sample_visit_date),
        tpt = trimws(input$sample_tpt),
        age = input$sample_age,
        diagnosis = input$sample_diagnosis,
        type = input$sample_type,
        specimenid = trimws(input$sample_specimenid),
        stringsAsFactors = FALSE
      )

      # Store the data frame to trigger the preview
      rv_sample$preview_df <- df

    }, error = function(e) {
      showNotification(paste("Error generating preview:", e$message), type = "error", duration = 10)
      rv_sample$preview_df <- NULL
    })
  })
  
  # --- D. Render the Preview UI ---
  output$sample_manual_preview_ui <- renderUI({
    req(rv_sample$preview_df)

    box(
      title = "Step 3: Confirm and Submit Record",
      status = "success", solidHeader = TRUE, width = 12,
      p("Please review the record below. The PMID and Sample ID have been automatically generated."),
      DT::dataTableOutput("sample_manual_preview_table"),
      br(),
      actionButton("action_submit_sample", "Submit to Database",
                   icon = icon("check"), class = "btn-success")
    )
  })

  # Server-side renderer for the preview table
  output$sample_manual_preview_table <- DT::renderDataTable({
    req(rv_sample$preview_df)
    DT::datatable(
      rv_sample$preview_df,
      options = list(scrollX = TRUE, dom = 't'),
      rownames = FALSE
    )
  })

  # --- E. Submit Data to Database on Final Button Click ---
  observeEvent(input$action_submit_sample, {
    req(rv_sample$preview_df)

    tryCatch({
      DBI::dbAppendTable(DBMP_ENV$conn, "samples", rv_sample$preview_df)

      showNotification("Success! New sample record has been added.", type = "message")

      # --- Reset the Form ---
      rv_sample$preview_df <- NULL
      shinyjs::reset("sample_pmid_select")
      shinyjs::reset("sample_visit_date")
      shinyjs::reset("sample_tpt")
      shinyjs::reset("sample_age")
      shinyjs::reset("sample_diagnosis")
      shinyjs::reset("sample_type")
      shinyjs::reset("sample_specimenid")
      
      # Resetting the IID dropdown
      updateSelectInput(session, "sample_iid_select", selected = "")

    }, error = function(e) {
      showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
    })
  })
  
  # ///////////////////////////////////////////////////////////////////
  
  # ===================================================================
  #  File Upload "Add New Sample" Workflow
  # ===================================================================
  
  # --- A. Reactive Values for File Upload ---
  rv_sample_upload <- reactiveValues(preview_df = NULL)
  
  # --- B. Template Download Handler ---
  output$download_sample_template <- downloadHandler(
    filename = function() {
      paste0("dbMP_sample_template_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Define the required columns for the template
      template_df <- data.frame(
        iid = "",
        visit_date = "",
        tpt = "",
        age = "",
        diagnosis = "",
        type = "",
        specimenid = ""
      )
      write.csv(template_df, file, row.names = FALSE)
    }
  )
  
  output$sample_template_reference_table <- renderTable({
    # Printing the data frame without row names gives a clean, aligned text output.
    sample_template_info
  })
  
  # --- C. Generate Preview from Uploaded File ---
  observeEvent(input$action_preview_sample_upload, {
    # Reset preview on every click
    rv_sample_upload$preview_df <- NULL
    
    tryCatch({
      req(input$sample_file_upload, message = "A file must be uploaded.")
      
      # 1. --- READ AND VALIDATE FILE STRUCTURE ---
      # Read in the data and check for errors from extra blank lines in the CSV file. (Especially important for Excel usage)
      uploaded_data <- readr::read_csv(input$sample_file_upload$datapath, show_col_types = FALSE) %>%
        filter(if_any(everything(), ~ !is.na(.)))
      
      # If, after removing empty rows, there is no data left, stop.
      if (nrow(uploaded_data) == 0) {
        stop("The uploaded file contains no valid data rows.")
      }
      
      # Check for required columns
      expected_cols <- c("iid", "visit_date", "tpt", "age", "diagnosis", "type", "specimenid")
      if (!all(expected_cols %in% names(uploaded_data))) {
        missing_cols <- setdiff(expected_cols, names(uploaded_data))
        stop(paste("File is missing required columns:", paste(missing_cols, collapse = ", ")))
      }
      
      # 2. --- PRE-VALIDATE IIDs ---
      # Check that all provided 'iid's exist in the patients table
      invalid_iids <- setdiff(uploaded_data$iid, db_data()$patients$iid)
      if (length(invalid_iids) > 0) {
        stop(paste("The following Institution IDs do not exist in the database:", paste(unique(invalid_iids), collapse = ", ")))
      }
      
      # 3. --- VALIDATION CHECKS ---
      # Check each row of the bulk upload
      # Initialize the allowed values
      allowed_diagnoses <- c("MGUS","SMM","MM","RRMM","PCL")
      allowed_types <- c("BM","PB","ST")
      
      # Initialize a list to store formatted error messages
      all_errors <- list()
      
      for(i in 1:nrow(uploaded_data)) {
        # Initialize the row to check and the errors
        row <- uploaded_data[i, ]
        row_errors <- c()
        
        # Check visit_date
        visit_date <- dmy(row$visit_date, quiet = TRUE)
        if (is.na(visit_date)) {
          row_errors <- c(row_errors, "'visit_date' is not a valid date.")
        }
        if (!is.na(visit_date) & visit_date > Sys.Date()) {
          row_errors <- c(row_errors, "'visit_date' cannot be in the future.")
        }
        
        # Check tpt
        if (!isTruthy(trimws(row$tpt))) {
          row_errors <- c(row_errors, "'tpt' cannot be blank.")
        }
        
        # Check age
        if (is.na(row$age) | !is.numeric(row$age) | row$age < 18 | row$age > 120) {
          row_errors <- c(row_errors, "'age' must be a number between 18 and 120.")
        }
        
        # Check specimenid
        if (!is.na(row$specimenid) & nchar(trimws(row$specimenid)) == 0) {
          row_errors <- c(row_errors, "'specimenid' cannot be blank (use 'NA' if not applicable).")
        }
        
        # Check diagnosis
        if (!(row$diagnosis %in% allowed_diagnoses)) {
          row_errors <- c(row_errors, paste0("'diagnosis' must be one of: ", paste(allowed_diagnoses, collapse=", "), "."))
        }
        
        # Check type
        if (!(row$type %in% allowed_types)) {
          row_errors <- c(row_errors, paste0("'type' must be one of: ", paste(allowed_types, collapse=", "), "."))
        }
        
        if (length(row_errors) > 0) {
          all_errors[[length(all_errors) + 1]] <- paste0("Row ", i + 1, ": ", paste(row_errors, collapse = " | "))
        }
      }
      
      # After the loop, check if the error list is populated
      if (length(all_errors) > 0) {
        error_message <- paste0(
          "Found ", length(all_errors), " errors in the uploaded file:\n",
          paste(all_errors, collapse = "")
        )
        # Use stop() to send the formatted HTML to the tryCatch error function
        stop(HTML(error_message))
      }
      
      # 4. --- DATA PREPARATION ---
      # If checks pass, covert the date for all records
      uploaded_data$visit_date <- dmy(uploaded_data$visit_date, quiet = TRUE)
      
      # Now join the uploaded data with the associated PMIDs and check for NAs as specimen ID
      data_with_pmid <- uploaded_data %>%
        mutate(specimenid = ifelse(is.na(specimenid), "NA", specimenid)) %>%
        left_join(db_data()$patients %>% select(pmid, iid), by = "iid")
      
      new_sids <- generate_sids_bulk(database = DBMP_ENV$conn, data = data_with_pmid)

      final_df <- data_with_pmid %>%
        mutate(sid = new_sids) %>%
        select(pmid, sid, all_of(expected_cols))

      # Store the prepared data frame
      rv_sample_upload$preview_df <- final_df
      
    }, error = function(e) {
      # This single block now catches any error from file reading, validation, or data prep
      showNotification(
        ui = HTML(e$message), # Display the formatted error message
        type = "error",
        duration = 20
      )
    })
  })
  
  # --- D. Render the Upload Preview UI ---
  output$sample_upload_preview_ui <- renderUI({
    req(rv_sample_upload$preview_df)
    
    box(
      title = "Step 3: Confirm and Submit Records",
      status = "success", solidHeader = TRUE, width = 12,
      p("Please review the records below. PMIDs and Sample IDs have been automatically generated. These exact rows will be added to the database."),
      DT::dataTableOutput("sample_upload_preview_table"),
      br(),
      actionButton("action_submit_sample_upload", "Submit All Records to Database", 
                   icon = icon("check-double"), class = "btn-success")
    )
  })
  
  output$sample_upload_preview_table <- DT::renderDataTable({
    req(rv_sample_upload$preview_df)
    DT::datatable(
      rv_sample_upload$preview_df,
      options = list(scrollX = TRUE, pageLength = 5),
      rownames = FALSE
    )
  })
  
  # --- E. Submit Uploaded Data to Database ---
  observeEvent(input$action_submit_sample_upload, {
    req(rv_sample_upload$preview_df)
    
    tryCatch({
      DBI::dbAppendTable(DBMP_ENV$conn, "samples", rv_sample_upload$preview_df)
      
      showNotification(paste("Success!", nrow(rv_sample_upload$preview_df), "new sample records have been added."), type = "message")
      
      # Reset the form
      rv_sample_upload$preview_df <- NULL
      shinyjs::reset("sample_file_upload")
      
    }, error = function(e) {
      showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
    })
  })
  
  # ///////////////////////////////////////////////////////////////////
  
  # ===================================================================
  #  File Upload "Add new Flow data" Workflow
  # ===================================================================
  
  # --- A. Reactive Values and Setup ---
  # A reactiveVal to store the prepared data frame for preview and submission
  rv_flow <- reactiveValues(preview_df = NULL)
  
  # Populate the Institution ID dropdown once
  observe({
    req(credentials()$user_auth, db_data())
    
    # Using the reactive db_data() ensures this list is always up to date
    eligible_iids <- unique(db_data()$samples$iid)
    
    # Also good practice to check that we actually found some IDs
    if (length(eligible_iids) > 0) {
      updateSelectInput(session, "flow_iid_select", choices = c("Select an Institution ID" = "", sort(eligible_iids)))
    }
  })
  
  # --- B. Dependent Dropdown for Sample ID ---
  # This UI is rendered based on the selected Institution ID
  output$flow_sid_select_ui <- renderUI({
    req(input$flow_iid_select, db_data())
    
    # Filter samples based on the selected IID
    available_sids <- db_data()$samples %>%
      filter(iid == input$flow_iid_select, type %in% c("BM", "PB")) %>% # Flow is only for BM or PB
      pull(sid)
    
    selectInput("flow_sid_select", "Select Sample ID:", choices = sort(available_sids))
  })
  
  # --- C. Generate Preview on Button Click ---
  observeEvent(input$action_preview_flow, {
    tryCatch({
      flow_file_ext <- tools::file_ext(input$flow_file_upload$name)
      if(flow_file_ext != "csv") {
        stop("Invalid file type. Please upload a .csv file.")
      }
      
      # This is your optimized file reading logic
      flow_colnames <- c("flowjo_id","CD3_pos", "CD4", "CD8", "Th22", "total_Th2", "total_Th17", "Th2_EM", "Th17_EM", "Th2_CM", "Th17_CM", "CD4_CM", "CD4_EM", "CD4_naive", "CD4_effector", "CD8_CM", "CD8_EM", "CD8_naive", "CD8_effector")
      
      # Read the uploaded file
      flow_data_raw <- readr::read_csv(
        file = input$flow_file_upload$datapath,
        skip = 1,
        col_names = flow_colnames,
        show_col_types = FALSE
      )
      
      # Prepare the data for the database table
      # Add the selected Sample ID and reorder columns to match the database schema
      final_df <- flow_data_raw %>%
        head(1) %>% # Keep only the first row of data from the file
        mutate(sid = input$flow_sid_select) %>% # Add the Sample ID from the UI selector
        left_join(y = db_data()$samples %>%
                    select(sid,iid,type),
                  by = "sid") %>% # Select columns in the correct order for the `flow` table
        select(sid, iid, type, all_of(flow_colnames[-1]))
      
      # Store the prepared data frame in our reactive value
      rv_flow$preview_df <- final_df
      
    }, error = function(e) {
      # If an error occurs, show a notification and clear the preview
      showNotification(paste("Error processing file:", e$message), type = "error", duration = 10)
      rv_flow$preview_df <- NULL
    })
  })
  
  # --- D. Render the Preview UI ---
  output$flow_preview_table <- DT::renderDataTable({
    # This block is only responsible for creating the datatable.
    # It automatically uses the same data as the renderUI block because
    # it also depends on rv_flow$preview_df.
    req(rv_flow$preview_df)
    
    DT::datatable(
      rv_flow$preview_df,
      options = list(scrollX = TRUE, dom = 't'), # 'dom = 't'' shows only the table
      rownames = FALSE
    )
  })
  
  output$flow_preview_ui <- renderUI({
    # Only show this UI if the preview data frame exists
    req(rv_flow$preview_df)
    
    box(
      title = "Step 3: Confirm and Submit Data",
      status = "success",
      solidHeader = TRUE,
      width = 12,
      p("Please review the data below. This is the exact row that will be added to the database."),
      DT::dataTableOutput("flow_preview_table"),
      # Add a little space for aesthetics
      br(), 
      actionButton("action_submit_flow", "Submit to Database", 
                   icon = icon("check"), class = "btn-success")
    )
  })
  
  # --- E. Submit Data to Database on Final Button Click ---
  observeEvent(input$action_submit_flow, {
    req(rv_flow$preview_df)
    
    tryCatch({
      # Append the prepared data frame to the SQLite table
      DBI::dbAppendTable(DBMP_ENV$conn, "flow", rv_flow$preview_df)
      showNotification("Success! Flow data has been added to the database.", type = "message")
      
      # Reset the preview data frame to NULL to hide the preview box
      rv_flow$preview_df <- NULL
      # Use shinyjs to reset the file input. This clears the "file chosen" display.
      shinyjs::reset("flow_file_upload")
      # Reset the first dropdown to be blank. This will trigger the reactive cascade.
      updateSelectInput(session, "flow_iid_select", selected = "")
      updateSelectInput(session, "flow_sid_select", selected = "")
      
    }, error = function(e) {
      showNotification(paste("Database Error:", e$message), type = "error", duration = 10)
    })
  })
  
  # ///////////////////////////////////////////////////////////////////
  
  
}
