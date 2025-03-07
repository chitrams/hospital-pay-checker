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

# Function to determine the overtime multiplier
get_overtime_multiplier <- function(datetime) {
  # Get day of the week (1 = Monday, 7 = Sunday)
  day <- wday(datetime, week_start = 1)
  
  # Sunday (7) rule for overtime
  if (day == 7) {
    return(1.75)
  }
  # Default overtime multiplier
  return(1.5)
}

# Function to calculate shift pay with detailed breakdown
calculate_shift_pay <- function(
    date_start, date_end, base_rate = 45.1977,
    include_overtime = FALSE, 
    overtime_start = NULL, overtime_end = NULL) {
  
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
  
  # Initialise dataframe to store segment details
  segments <- data.frame(
    segment_start = as.POSIXct(character()),
    segment_end = as.POSIXct(character()),
    hours = numeric(),
    rate_multiplier = numeric(),
    segment_pay = numeric(),
    is_overtime = logical(),
    stringsAsFactors = FALSE
  )
  
  # Process each 15-minute segment
  for (i in 1:(length(minute_seq) - 1)) {
    segment_start <- minute_seq[i]
    segment_end <- minute_seq[i + 1]
    
    # Duration of this segment in hours (may be fractional)
    segment_hours <- as.numeric(difftime(segment_end, segment_start, units = "hours"))
    
    # Check if this segment is within overtime period
    is_overtime <- FALSE
    if (include_overtime && !is.null(overtime_start) && !is.null(overtime_end)) {
      overtime_start <- as_datetime(overtime_start)
      overtime_end <- as_datetime(overtime_end)
      is_overtime <- segment_start >= overtime_start && segment_end <= overtime_end
    }
    
    # Determine the pay rate for this segment
    if (is_overtime) {
      rate_multiplier <- get_overtime_multiplier(segment_start)
    } else {
      rate_multiplier <- get_rate_multiplier(segment_start)
    }
    
    # Calculate pay for this segment
    segment_pay <- base_rate * rate_multiplier * segment_hours
    
    # Add segment details to dataframe
    segments <- rbind(segments, data.frame(
      segment_start = segment_start,
      segment_end = segment_end,
      hours = segment_hours,
      rate_multiplier = rate_multiplier,
      segment_pay = segment_pay,
      is_overtime = is_overtime
    ))
  }
  
  # Add a reason column to explain each multiplier
  segments$reason <- sapply(1:nrow(segments), function(i) {
    dt <- segments$segment_start[i]
    is_overtime <- segments$is_overtime[i]
    
    if (is_overtime) {
      day <- wday(dt, week_start = 1)
      if (day == 7) {
        return("Overtime on Sundays")
      } else {
        return("Overtime")
      }
    } else {
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
    }
  })
  
  # Summarize by rate multiplier and reason
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
  pay_output <- list(
    segments = segments,
    summary = summary,
    total_pay = total_pay,
    total_hours = total_hours
  )
  
  return(pay_output)
}

# Define UI -------------------
ui <- fluidPage(
  titlePanel("Shift Pay Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("base_rate", 
                   "Base hourly rate ($)", 
                   value = 45.1977, 
                   min = 0),
      
      dateInput("start_date",
                "Shift start date",
                value = Sys.Date()),
      
      timeInput("start_time",
                "Shift start time",
                value = strptime("07:00", "%H:%M"),
                seconds = FALSE),
      
      dateInput("end_date",
                "Shift end date",
                value = Sys.Date()),
      
      timeInput("end_time",
                "Shift end time",
                value = strptime("17:00", "%H:%M"),
                seconds = FALSE),
      
      checkboxInput("include_overtime", 
                    "Include overtime?", 
                    value = FALSE),
      
      conditionalPanel(
        condition = "input.include_overtime == true",
        
        dateInput("overtime_start_date",
                  "Overtime start date",
                  value = Sys.Date()),
        
        timeInput("overtime_start_time",
                  "Overtime start time",
                  value = strptime("17:00", "%H:%M"),
                  seconds = FALSE),
        
        dateInput("overtime_end_date",
                  "Overtime end date",
                  value = Sys.Date()),
        
        timeInput("overtime_end_time",
                  "Overtime end time",
                  value = strptime("21:00", "%H:%M"),
                  seconds = FALSE)
      ),
      
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
    # Create datetime from separate date and time inputs with explicit formatting
    date_part <- format(input$start_date, "%Y-%m-%d")
    time_part <- format(input$start_time, "%H:%M:%S")
    timestamp <- paste(date_part, time_part)
    as_datetime(timestamp)
  })
  
  shift_end <- reactive({
    date_part <- format(input$end_date, "%Y-%m-%d")
    time_part <- format(input$end_time, "%H:%M:%S")
    timestamp <- paste(date_part, time_part)
    as_datetime(timestamp)
  })
  
  overtime_start <- reactive({
    req(input$include_overtime)
    date_part <- format(input$overtime_start_date, "%Y-%m-%d")
    time_part <- format(input$overtime_start_time, "%H:%M:%S")
    timestamp <- paste(date_part, time_part)
    as_datetime(timestamp)
  })
  
  overtime_end <- reactive({
    req(input$include_overtime)
    date_part <- format(input$overtime_end_date, "%Y-%m-%d")
    time_part <- format(input$overtime_end_time, "%H:%M:%S")
    timestamp <- paste(date_part, time_part)
    as_datetime(timestamp)
  })
  
  # Calculate pay when button is clicked
  pay_results <- eventReactive(input$calculate, {
    if (input$include_overtime) {
      calculate_shift_pay(
        shift_start(), 
        shift_end(), 
        input$base_rate, 
        include_overtime = TRUE, 
        overtime_start = overtime_start(), 
        overtime_end = overtime_end()
      )
    } else {
      calculate_shift_pay(
        shift_start(), 
        shift_end(), 
        input$base_rate
      )
    }
  })
  
  # Display shift information
  output$shift_info <- renderUI({
    req(pay_results())
    
    # Format shift times
    shift_start_str <- format(shift_start(), "%A, %-d %b %Y, at %H:%M")
    shift_end_str <- format(shift_end(), "%A, %-d %b %Y, at %H:%M")
    
    # Calculate hours depending on overtime
    regular_hours <- 0
    overtime_hours <- 0
    
    # Calculating pay depending on overtime
    if (input$include_overtime) {
      
      segments <- pay_results()$segments
      
      # Sum up regular and overtime hours
      regular_hours <- sum(segments$hours[!segments$is_overtime])
      overtime_hours <- sum(segments$hours[segments$is_overtime])
      
      # Format overtime times
      ot_start_str <- format(overtime_start(), "%A, %-d %b %Y, at %H:%M")
      ot_end_str <- format(overtime_end(), "%A, %-d %b %Y, at %H:%M")
      
      overtime_info <- paste(
        "<h4>Overtime Period</h4>",
        "<p><strong>Start:</strong> ", ot_start_str, "</p>",
        "<p><strong>End:</strong> ", ot_end_str, "</p>",
        "<p><strong>Overtime Hours:</strong> ", round(overtime_hours, 2), "</p>"
      )
    } else {
      regular_hours <- pay_results()$total_hours
    }
    
    # Showing HTML content depending on overtime
    
    if (input$include_overtime) {
      html_content <- paste(
        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h4>Shift Information</h4>",
        "<p><strong>Start:</strong> ", shift_start_str, "</p>",
        "<p><strong>End:</strong> ", ot_end_str, "</p>", 
        "<p><strong>Regular Hours:</strong> ", round(regular_hours, 2), "</p>",
        "<p><strong>Total Hours:</strong> ", round(pay_results()$total_hours, 2), "</p>",
        "<hr>",
        overtime_info,
        "</div>"
      )
    } else {
      html_content <- paste(
        "<div style='background-color: #f8f9fa; padding: 15px; border-radius: 5px;'>",
        "<h4>Shift Information</h4>",
        "<p><strong>Start:</strong> ", shift_start_str, "</p>",
        "<p><strong>End:</strong> ", shift_end_str, "</p>",
        "<p><strong>Regular Hours:</strong> ", round(regular_hours, 2), "</p>",
        "<p><strong>Total Hours:</strong> ", round(pay_results()$total_hours, 2), "</p>",
        "</div>"
      )
    }
    
    HTML(html_content)
    
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