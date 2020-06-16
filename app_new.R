# Load packages
library(dplyr)
library(tidyselect)
library(tidyr)
library(readr)
library(stringr)
library(here)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(tm)
library(httr) #for reading from github


#***************
# NOTES/TO DO
# 1. modify "data" scenario so data can be shown behind the model data (use opacity command)-maybe add a new column for this to seperate this 
# 2. add confidence interval data for future predictions
# 3. clean up scenario and data stacking

#prevent shiny from overwriting our error message
#not used right now, using safeError below instead
#options(shiny.sanitize.errors = FALSE)

#################################
# functions
#################################

github_api <- function(path) {
  url <- httr::modify_url("https://api.github.com", path = path)
  httr::GET(url)
}


#################################
# Load all data
# should be online so things update automatically
#for speed, we only get data from the online scenario if the data is old, otherwise we load locally
#################################

# to ensure data gets refreshed on server, we need this
get_data <- function()
{
  filename = here("data",paste0("clean_data_",Sys.Date(),'.rds')) #if the data file for today is here, load then return from function
  if (file.exists(filename)) {
     all_data <- readRDS(file = filename)    
     return(all_data)  
  }
  #if data file is not here, go through all of the below
  
   all_data = list() #will save and return all datasets as list
  
   
  #################################
  #pull data from repo, combine and process
  #################################
  message('starting data processing')
  
  #code to pull each state CSV - not currently used, doing a combined one as part of the other workflow  
  #resp <- github_api("/repos/CEIDatUGA/COVID-stochastic-fitting/git/trees/master?recursive=1")
  #filelist <- unlist(lapply(content(resp)$tree, "[", "path"), use.names = F)
  #filenames = grep("output/current", filelist, value = TRUE, fixed = TRUE) 
  #csvfiles = filenames[stringr::str_which(filenames,"csv")]
  
  #all_state_data = NULL
  #for (n in 1:length(csvfiles))
  #{
  #  curfile = paste0("https://raw.githubusercontent.com/CEIDatUGA/COVID-stochastic-fitting/master/",csvfiles[n])  
  #  state_data <- readr::read_csv(curfile)
  #  all_state_data = dplyr::bind_rows(all_state_data,state_data)
  #}

  us_popsize <- readRDS(here("data","us_popsize.rds")) %>% rename(state_abr = state, location = state_full, pop_size = total_pop)
     
  us_dat_raw <- readr::read_csv("https://raw.githubusercontent.com/CEIDatUGA/COVID-stochastic-fitting/master/output/us_current_results.csv") 

  #Notes on current data format:
  #sim_type contains 3 different future scenarios, also includes data
  #period is Past or Future, based on date up to which data is fit. Only applies to model results
  #variable includes latent_trend and mobility_trend. Those are stored in mean_value. Data are stored in mean_value as well.
  
  #Data has these columns: location, sim_type (for simulations only, data should be moved into variable column), period, date, variable (including "data", "latent_trend","mobility_trend"), var_type (lower_95, mean_value, etc.) and value (the only column with a number/value in it)
  # **** PRIORITY ONE-work on data restructuring as described above (plots will not function again until restructured)****
  
  us_dat <- us_dat_raw %>% left_join(us_popsize, by = "location") %>%
                           rename(populationsize = pop_size, scenario = sim_type) %>%
                           mutate(variable = recode(variable, daily_cases = "Daily_Cases", 
                                                     daily_hosps = "Daily_Hospitalized", 
                                                     daily_deaths = "Daily_Deaths",
                                                     cumulative_cases = "Total_Cases", 
                                                     cumulative_hosps = "Total_Hospitalized", 
                                                     cumulative_deaths = "Total_Deaths")) 
                          
    
    
    
  #combine data in list  
  #currently only US, but set up for future use
  all_data$us_dat = us_dat
  
  message('Data cleaning done.')
  
  #save the data
  saveRDS(all_data, filename)    
  return(all_data)
} # end the get-data function which pulls data from the various online scenarios and processes/saves  

###########################################
# function that re-reads the data every so often
###########################################
all_data <- reactivePoll(intervalMillis = 1000*60*60*3, # pull new data every N hours
                         session = NULL,
                         checkFunc = function() {Sys.time()}, #this will always return a different value, which means at intervals specified by intervalMillis the new data will be pulled
                         valueFunc = function() {get_data()} )

#read data is reactive, doesn't work for rest below 
all_dat = isolate(all_data())

# pull data out of list 
us_dat = all_dat$us_dat 


#define variables for location and scenario selectors
state_var = sort(unique(us_dat$location))  
state_var = c("US",state_var[!state_var=="US"]) #move US to front

scenario_var = sort(unique(us_dat$scenario))

#################################
# Define UI
#################################
ui <- fluidPage(
  tags$head(includeHTML(here("www","google-analytics.html"))), #this is for Google analytics tracking.
  includeCSS(here("www","appstyle.css")),
  #main tabs
  navbarPage(
    sidebarLayout(
                         sidebarPanel(
                           shinyWidgets::pickerInput("state_selector", "Select State(s)", state_var, multiple = FALSE,options = list(`actions-box` = TRUE), selected = c("Georgia") ),
                           shiny::div("US is at start of state list."),
                           br(),
                           shinyWidgets::pickerInput("scenario_selector", "Select Scenarios(s)", scenario_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("status_quo") ),
                           shiny::div("Choose potential future scenarios."),
                           br(),
                           shiny::selectInput("conf_int", "Show forecast confidence interval", c("Yes" = "Yes", "No" = "No" ), selected = "No"),
                           shiny::div("Show 95% confidence interval for forecast data."),
                           br(),
                           shiny::selectInput("daily_tot", "Daily or cumulative numbers", c("Daily" = "Daily", "Total" = "Total" )),
                           shiny::div("Modify all plots to show daily or cumulative data."),
                           br(),
 #Test dropping smoother #  shiny::selectInput("show_smoother", "Add trend line", c("No" = "No", "Yes" = "Yes")),
                         #  shiny::div("Shows a trend line for cases/hospitalizations/deaths plot."),
                         #  br(),
                           shiny::selectInput( "absolute_scaled","Absolute or scaled values",c("Absolute Number" = "absolute", "Per 100,000 persons" = "scaled") ),
                           shiny::div("Modify the top two plots to display total counts or values scaled by the state/territory population size."),
                           br(),
                           shiny::selectInput("xscale", "Set x-axis to calendar date or days since a specified total number of cases/hospitalizations/deaths", c("Calendar Date" = "x_time", "Days since N cases/hospitalizations/deaths" = "x_count")),
                           sliderInput(inputId = "x_limit", "Select a date or outcome value from which to start the plots.", min = as.Date("2020-01-22","%Y-%m-%d"),  max = Sys.Date(), value = as.Date("2020-02-01","%Y-%m-%d") ),
                           shiny::div("Modify all three plots to begin at a specified starting date or outcome value designated in the slider above."),
                           br(),
                           shiny::selectInput(  "yscale", "Y-scale", c("Linear" = "lin", "Logarithmic" = "log")),
                           shiny::div("Modify the top two plots to show data on a linear or logarithmic scale."),
                           br()
                         ),         #end sidebar panel
                         # Output:
                         mainPanel(
                           #change to plotOutput if using static ggplot object
                           plotlyOutput(outputId = "transstrength_plot", height = "300px"),
                           plotlyOutput(outputId = "case_plot", height = "300px"),
                           plotlyOutput(outputId = "death_plot", height = "300px"),
                           plotlyOutput(outputId = "allinf_plot", height = "300px")
                         ) #end main panel
                       ) #end sidebar layout     
  ), #close NavBarPage
  tags$div(
    id = "bigtext",
    "These interactive plots are part of CEID @ UGA's work on COVID-19 modeling. If you ended up on this site outside our main page, see", a("the link to this website", href = "https://www.covid19.uga.edu", target = "_blank" ),
    "for more information and explanations."
  )
) #end fluidpage and UI part of shiny app
#end UI of shiny app
###########################################


###########################################
# Define server functions
###########################################
server <- function(input, output, session) {

  #watch the choice for the x-scale and choose what to show underneath accordingly
  observeEvent(input$xscale,
               {
                 if (input$xscale == 'x_count')
                 {
                   #Add a reactive range to slider
                   shiny::updateSliderInput(session, "x_limit", min = 1,  max = 500, step = 10, value = 1 )
                 } else
                 {
                   shiny::updateSliderInput(session, "x_limit", min = as.Date("2020-01-22","%Y-%m-%d"),  max = Sys.Date(), value = as.Date("2020-02-01","%Y-%m-%d") )
                 }
               }) #end observe event  
  
  #watch the choice for the x-scale and choose what to show underneath accordingly
  observeEvent(input$xscale_w,
               {
                 if (input$xscale_w == 'x_count')
                 {
                   #Add a reactive range to slider
                   shiny::updateSliderInput(session, "x_limit_w", min = 1,  max = 500, step = 10, value = 1 )
                 } else
                 {
                   shiny::updateSliderInput(session, "x_limit_w", min = as.Date("2020-01-22","%Y-%m-%d"),  max = Sys.Date(), value = as.Date("2020-02-01","%Y-%m-%d") )
                 }
               }) #end observe event 
  
  #watch the choice for the x-scale and choose what to show underneath accordingly
  observeEvent(input$xscale_c,
               {
                 if (input$xscale_c == 'x_count')
                 {
                   #Add a reactive range to slider
                   shiny::updateSliderInput(session, "x_limit_c", min = 1,  max = 500, step = 10, value = 1 )
                 } else
                 {
                   shiny::updateSliderInput(session, "x_limit_c", min = as.Date("2020-01-22","%Y-%m-%d"),  max = Sys.Date(), value = as.Date("2020-02-01","%Y-%m-%d") )
                 }
               }) #end observe event  
  
  #watch state_selector_c to reduce the picker options in the county dropdown limited to those within the selected state(s)
  observeEvent(input$state_selector_c,
               {
                  #redesignate county_dat to match the state selector input
                   county_dat_sub <- county_dat %>% filter(state %in% input$state_selector_c)
                   county_var_sub = sort(unique(county_dat_sub$location))
                   shinyWidgets::updatePickerInput(session, "county_selector", "Select counties", county_var_sub, selected = county_var_sub[1])
             })
  

  ###########################################
  # function that takes data generated by above function and makes plots
  # uses plotly
  ###########################################
  make_plotly <- function(all_plot_dat, location_selector, scenario_selector,case_death, daily_tot,
                              xscale, yscale, absolute_scaled, x_limit, current_tab,  
                              show_smoother, conf_int, ylabel, outtype)
  {

    #outcome to plot/process for non-test
    outcome = paste(daily_tot,case_death,sep='_') #make string from UI inputs that correspond with variable names
    
    if (outtype == "Test_All") 
    {
      outcome = paste(daily_tot,outtype,sep="_")
    }
    if (outtype == "Positive_Prop") 
    {
      outcome = paste(daily_tot,outtype,sep="_") 
    }

      #filter data based on user selections
      #keep all outcomes/variables for now so we can do x-axis adjustment
      #filtering of only the outcome to plot is done after x-scale adjustment
      plot_dat <- all_plot_dat %>%   filter(location %in% location_selector) %>%      
                                     filter(scenario %in% scenario_selector) %>%
                                     group_by(scenario,location) %>%
                                     arrange(date) %>%
                                     ungroup()

    #adjust x-axis as needed 
    if (xscale == 'x_count')
    {
      #filter by count limit
      out_type2 = paste0("Total_",case_death) #make string from UI inputs that correspond to total and selected outcome
      start_dates <- plot_dat %>% 
        filter(variable == out_type2) %>% #get the quantity (cases/hosp/death) which is used to define start
        filter( value >= x_limit) %>% #remove all values that are below threshold
        group_by(scenario,location) %>%   #group by states
        summarize(start_date = first(date))   #get first date for each state after filtering. for this to work right, the data needs to be sorted by date for each scenario/location combination
      
      plot_dat <-  plot_dat %>% left_join(start_dates, by = c("scenario", "location")) %>% #add start dates to data
                   filter(variable %in% outcome) %>% #retain only outcome variable
                   filter(date >= start_date)  %>%      
                   mutate(time = as.numeric(date)) %>%
                   group_by(scenario, location) %>% 
                   mutate(time = time - min(time)) %>%
                   ungroup()
    }
    else
    {
      #filter by date limit
      plot_dat <- plot_dat %>% mutate(time = date) %>%
                  filter(variable %in% outcome) %>%
                  filter(date >= x_limit) 
    }
    
    
    #set labels and tool tips based on input - entries 2 and 3 are ignored for world plot
    y_labels <- c("Cases", "Tests", "Positive Test Proportion")
    y_labels[1] <- case_death #fill that automatically with either Case/Hosp/Death
    y_labels <- paste(daily_tot, y_labels, sep = " ")
    
    tool_tip <- c("Date", "Cases", "Tests", "Positive Test Proportion")
    tool_tip[2] <- case_death #fill that automatically with either Case/Hosp/Death
    
       
    #if we want scaling by 100K, do extra scaling 
    # don't apply to test proportion
    if ((absolute_scaled == 'scaled') && (outtype != "Positive_Prop"))
    {
      plot_dat <- plot_dat %>% mutate(value = value / populationsize * 100000) 
      y_labels[1] <- paste0(y_labels[1], " per 100K")
      y_labels[2] <- paste0(y_labels[2], " per 100K")
    } #end scaling function
     
    

    p_dat <- plot_dat
    #the US test plots can only be created using the COVIDtracking data
    if (current_tab == "us" && (outtype == "Test_All" || outtype == "Positive_Prop")) 
    {
      p_dat <- plot_dat %>% filter(scenario == "COVIDTracking")
      
    }
    #the world test plots can only be created using the OWID data
    if (current_tab == "world" && (outtype == "Test_All" || outtype == "Positive_Prop"))
    {
      p_dat <- plot_dat %>% filter(scenario == "OWID")
    }
       
    linesize = 1.5
    ncols = max(3,length(unique(p_dat$location))) #number of colors for plotting
    
    # create text to show in hover-over tooltip
    tooltip_text = paste(paste0("Location: ", p_dat$location), 
                         paste0(tool_tip[1], ": ", p_dat$date), 
                         paste0(tool_tip[ylabel+1],": ", outcome, sep ="\n")) 
    
    # make plot
    pl <- plotly::plot_ly(p_dat) %>% 
          plotly::add_trace(x = ~time, y = ~value, type = 'scatter', 
                                 mode = 'lines+markers', 
                                 linetype = ~scenario, symbol = ~location,
                                 line = list(width = linesize), text = tooltip_text, 
                                 color = ~scenario, colors = brewer.pal(ncols, "Dark2")) %>%
                          layout(yaxis = list(title=y_labels[ylabel], type = yscale, size = 18)) %>%
                          layout(legend = list(orientation = "h", x = 0.2, y = -0.3))

    #need to eliminate NAs in value and upper_95 to fix issues with Current Date bar bugs
    if(conf_int == "Yes"){
      #add confidence interval ranges
      pl <- pl %>% add_ribbons(x = ~time, ymin = ~lower_95, ymax = ~upper_95) %>%
        plotly::add_segments(x = Sys.Date(), xend = Sys.Date(), 
                             y = 0, yend = ~max(upper_95)+100, name = "Current Date",
                             color = I("black"), alpha = 0.5)
    }
    else
    {
      #adds a verical line at the current date
      pl <- pl %>% plotly::add_segments(x = Sys.Date(), xend = Sys.Date(), 
                           y = 0, yend = ~max(value)+100, name = "Current Date",
                           color = I("black"), alpha = 0.5)
    }
    #Test remove smoother functionality
    
    # if requested by user, apply and show a smoothing function 
#    if (show_smoother == "Yes")
#    {
#      if (any(location_selector %in% p_dat$location))
#      {
#        p_dat2 <- p_dat  %>% select(location,scenario,value,time) %>% drop_na() %>%
#          group_by(location) %>%
#          filter(n() >= 2) %>%  
#          mutate(smoother = loess(value ~ as.numeric(time), span = .4)$fitted) %>%    
#          ungroup()
        
#        pl <- pl %>% plotly::add_lines(x = ~time, y = ~smoother, 
#                                       color = ~location, data = p_dat2, 
#                                       line = list( width = 2*linesize),
#                                       opacity=0.3,
#                                       showlegend = FALSE) 
#     }
#     else
#     {
#        stop(safeError("Please select a different data scenario or location. The selected location(s) is not present in the chosen scenario"))
#     }
#    } #end smoother if statement
    return(pl)
  }
  
  ###########################################
  #function that makes case plot 
  ###########################################
  output$case_plot <- renderPlotly({
    pl <- NULL
    if (!is.null(input$scenario_selector))
    {
    #create plot
    pl <- make_plotly(us_dat, input$state_selector, input$scenario_selector, "Cases", input$daily_tot,
                              input$xscale, input$yscale, input$absolute_scaled, input$x_limit, input$current_tab,
                              input$show_smoother, input$conf_int, ylabel = 1, outtype = '')
    }
    return(pl)
  }) #end function making case plot

  ###########################################
  #function that makes all infected plot 
  ###########################################
  output$allinf_plot <- renderPlotly({
    pl <- NULL
    if (!is.null(input$scenario_selector))
    {
      #create plot
      pl <- make_plotly(us_dat, input$state_selector, input$scenario_selector, "Allinfected", input$daily_tot,
                        input$xscale, input$yscale, input$absolute_scaled, input$x_limit, input$current_tab,
                        input$show_smoother, input$conf_int, ylabel = 1, outtype = '')
    }
    return(pl)
  }) #end function making all infected plot
  
  ###########################################
  #function that makes death plot 
  ###########################################
  output$death_plot <- renderPlotly({
    pl <- NULL
    if (!is.null(input$scenario_selector))
    {
      #create plot
      pl <- make_plotly(us_dat, input$state_selector, input$scenario_selector, "Deaths", input$daily_tot,
                        input$xscale, input$yscale, input$absolute_scaled, input$x_limit, input$current_tab,
                        input$show_smoother, input$conf_int, ylabel = 1, outtype = '')
    }
    return(pl)
  }) #end function making death plot
  
  ###########################################
  #function that makes transmission strength plot 
  ###########################################
  output$transstrength_plot <- renderPlotly({
    pl <- NULL
    if (!is.null(input$scenario_selector))
    {
      #create plot
      pl <- make_plotly(us_dat, input$state_selector, input$scenario_selector, "Transstrength", input$daily_tot,
                        input$xscale, input$yscale, "absolute", input$x_limit, input$current_tab,
                        input$show_smoother, input$conf_int, ylabel = 1, outtype = '')
    }
    return(pl)
  }) #end function making death plot
  

} #end server function

# Create Shiny object
shinyApp(ui = ui, server = server)