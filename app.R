library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(DT)
library(shinyTime)

# Helper functions ----------------

# Function to determine the rate multiplier based on time
get_rate_multiplier <- function(datetime) {
  # Get day of the week (1 = Monday, 7 = Sunday)
  day <- wday(datetime, week_start = 1)
  # Get hour of the day
  hour <- hour(datetime)
  
  # Sunday (7) rule
  if (day == 7) {
    return(1.75)
  }
  # Monday morning rule
  if (day == 1 && hour < 8) {
    return(1.75)
  }
  # Saturday (6) rule
  if (day == 6) {
    return(1.5)
  }
  # Weekday overnight rule
  if (day >= 1 && day <= 5 && hour >= 0 && hour <= 7) {
    return(1.25)  # Weekday overnight rate
  }
  # Weekday evening rule
  if (day >= 1 && day <= 5 && hour >= 18 && hour <= 23) {
    return(1.2)  # Weekday evening rate
  }
  # Default weekday rate
  return(1.0) 
}

# Function to calculate shift pay with detailed breakdown
calculate_shift_pay_detailed <- function(date_start, date_end, base_rate = 45) {
  # Ensure dates are in POSIXct format
  date_start <- as_datetime(date_start)
  date_end <- as_datetime(date_end)
  
  # Create a sequence of timestamps from start to end
  # We'll use 15-minute segments to calculate pay
  minute_seq <- seq(from = date_start, to = date_end, by = "15 mins")
  
  # If the end time isn't exactly on a 15-minute mark, add it to ensure we cover the full shift
  if (date_end != last(minute_seq)) {
    minute_seq <- c(minute_seq, date_end)
  }
  
  # Initialize dataframe to store segment details
  segments <- data.frame(
    segment_start = as.POSIXct(character()),
    segment_end = as.POSIXct(character()),
    hours = numeric(),
    rate_multiplier = numeric(),
    segment_pay = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Process each 15-minute segment
  for (i in 1:(length(minute_seq) - 1)) {
    segment_start <- minute_seq[i]
    segment_end <- minute_seq[i + 1]
    
    # Duration of this segment in hours (may be fractional)
    segment_hours <- as.numeric(difftime(segment_end, segment_start, units = "hours"))
    
    # Determine the pay rate for this segment
    rate_multiplier <- get_rate_multiplier(segment_start)
    
    # Calculate pay for this segment
    segment_pay <- base_rate * rate_multiplier * segment_hours
    
    # Add segment details to dataframe
    segments <- rbind(segments, data.frame(
      segment_start = segment_start,
      segment_end = segment_end,
      hours = segment_hours,
      rate_multiplier = rate_multiplier,
      segment_pay = segment_pay
    ))
  }
  
  # Add a reason column to explain each multiplier
  segments$reason <- sapply(segments$segment_start, function(dt) {
    day <- wday(dt, week_start = 1)
    hour <- hour(dt)
    
    if (day == 7) {
      return("Sunday")
    } else if (day == 1 && hour < 8) {
      return("Monday morning")
    } else if (day == 6) {
      return("Saturday")
    } else if (day >= 1 && day <= 5 && hour >= 0 && hour <= 7) {
      return("Weekday overnight")
    } else if (day >= 1 && day <= 5 && hour >= 18 && hour <= 23) {
      return("Weekday evening")
    } else {
      return("Regular weekday")
    }
  })
  
  # Summarize by rate multiplier
  summary <- segments %>%
    group_by(rate_multiplier, reason) %>%
    summarize(
      total_hours = sum(hours),
      total_pay = sum(segment_pay),
      .groups = "drop"
    ) %>%
    arrange(desc(rate_multiplier))
  
  # Calculate total pay and hours
  total_pay <- sum(segments$segment_pay)
  total_hours <- sum(segments$hours)
  
  # Return a list with all the details
  return(list(
    segments = segments,
    summary = summary,
    total_pay = total_pay,
    total_hours = total_hours
  ))
}

# Define UI -------------------
ui <- fluidPage(
  titlePanel("Shift Pay Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("base_rate", 
                   "Base Hourly Rate ($):", 
                   value = 45, 
                   min = 0),
      
      dateInput("start_date",
                "Shift Start Date:",
                value = Sys.Date()),
      
      timeInput("start_time",
                "Shift Start Time:",
                value = strptime("22:00:00", "%H:%M:%S")),
      
      dateInput("end_date",
                "Shift End Date:",
                value = Sys.Date() + 1),
      
      timeInput("end_time",
                "Shift End Time:",
                value = strptime("08:30:00", "%H:%M:%S")),
      
      actionButton("calculate", "Calculate Pay", 
                   class = "btn-primary", 
                   style = "margin-top: 15px;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Summary", 
                 br(),
                 htmlOutput("shift_info"),
                 br(),
                 htmlOutput("total_pay"),
                 br(),
                 DT::dataTableOutput("summary_table")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Reactive expression to combine date and time inputs
  shift_start <- reactive({
    date_time <- paste(input$start_date, format(input$start_time, "%H:%M:%S"))
    as_datetime(date_time)
  })
  
  shift_end <- reactive({
    date_time <- paste(input$end_date, format(input$end_time, "%H:%M:%S"))
    as_datetime(date_time)
  })
  
  # Calculate pay when button is clicked
  pay_results <- eventReactive(input$calculate, {
    calculate_shift_pay_detailed(shift_start(), shift_end(), input$base_rate)
  })
  
  # Display shift information
  output$shift_info <- renderUI({
    req(pay_results())
    
    start <- format(shift_start(), "%a, %b %d, %Y %H:%M")
    end <- format(shift_end(), "%a, %b %d, %Y %H:%M")
    hours <- round(pay_results()$total_hours, 2)
    
    HTML(paste(
      "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
      "<h4>Shift Information</h4>",
      "<p><strong>Start:</strong> ", start, "</p>",
      "<p><strong>End:</strong> ", end, "</p>",
      "<p><strong>Total Hours:</strong> ", hours, "</p>",
      "</div>"
    ))
  })
  
  # Display total pay
  output$total_pay <- renderUI({
    req(pay_results())
    
    total <- formatC(pay_results()$total_pay, format = "f", digits = 2, big.mark = ",")
    
    HTML(paste(
      "<div style='background-color: #e8f4f8; padding: 15px; border-radius: 5px;'>",
      "<h3>Total Pay: $", total, "</h3>",
      "</div>"
    ))
  })
  
  # Display rate summary table
  output$summary_table <- DT::renderDataTable({
    req(pay_results())
    
    summary_df <- pay_results()$summary
    summary_df$rate_multiplier <- paste0(summary_df$rate_multiplier, "x")
    summary_df$total_hours <- round(summary_df$total_hours, 2)
    summary_df$total_pay <- round(summary_df$total_pay, 2)
    
    DT::datatable(
      summary_df,
      colnames = c("Rate Multiplier", "Type", "Hours", "Pay ($)"),
      options = list(dom = 't', pageLength = 10),
      rownames = FALSE
    ) %>%
      DT::formatCurrency(columns = "total_pay", currency = "$")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
