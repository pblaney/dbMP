# The UI stack that handles the login screen
ui_stack <- fluidPage(
  div(class = "pull-right", logout_ui, style = "padding: 10px"),
  # Call the login UI 
  login_ui,
  # This is where the main dashboard will be rendered after login
  uiOutput("dashboard_ui_holder")
)

# Final UI object
ui_stack
