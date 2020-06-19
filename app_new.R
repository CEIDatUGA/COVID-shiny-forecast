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
# 3. remove R pacakges above that we are not actually using


#################################
#define some variables used often
#################################

#starting date for date slider and default starting date to show
mindate = as.Date("2020-02-01","%Y-%m-%d")
defaultdate = as.Date("2020-03-01","%Y-%m-%d")


#################################
# functions
#################################


#################################
# Load all data
# should be online so things update automatically
# for speed, we only get data from the online scenario if the data is old, otherwise we load locally
#################################

# to ensure data gets refreshed on server, we need this
get_data <- function()
{
  filename = here("data",paste0("clean_data_",Sys.Date(),'.rds')) #if the data file for today is here, load then return from function
  if (file.exists(filename)) 
  {
     all_data <- readRDS(file = filename)    
     return(all_data)  
  }
  #if data file is not here, go through all of the below
  
  all_data = list() #will save and return all datasets as list
  
  #################################
  #pull data from repo, combine and process
  #################################
  message('starting data processing')
  
  us_popsize <- readRDS(here("data","us_popsize.rds")) %>% rename(state_abr = state, location = state_full, pop_size = total_pop)
  us_dat_raw <- readr::read_csv("https://raw.githubusercontent.com/CEIDatUGA/COVID-stochastic-fitting/master/output/us_current_results.csv")
  

  #Notes on current data format:
  #sim_type contains 3 different future scenarios, has NA for data
  #period is Past or Future, based on date up to which data is fit. Only applies to model results
  #variable includes latent_trend and mobility_trend and combined_trend. Those are stored in mean_value. Data are stored in mean_value as well.
  us_dat <- us_dat_raw %>% 
            filter(variable != c("daily_hosps","cumulative_hosps")) %>% #not using hosp right now
            mutate(variable = recode(variable, daily_cases = "Daily_Cases", 
                                     daily_all_infections = "Daily_Allinfected", 
                                    daily_deaths = "Daily_Deaths",
                                    cumulative_deaths = "Cumulative_Deaths",
                                    cumulative_cases = "Cumulative_Cases",                         
                                    cumulative_all_infections = "Cumulative_Allinfected", 
                                    combined_trend = "Transmissionstrength"
                                    )) %>%
            select(-c(lower_80,lower_90,upper_80,upper_90)) %>% #currently only using 95CI
            left_join(us_popsize, by = "location") %>%
            rename(populationsize = pop_size, scenario = sim_type)
  
#Temporary fix to make transstrenght plot to work until next iteration of data is done with correction
  us_dat <- us_dat %>% mutate(median_value = ifelse(variable == "Transmissionstrength", mean_value, median_value))

  #add actual data to a new column to add new plotly layer (there is likely a tidy-er way to do this, revist once working) 
  us_dat <- us_dat %>% mutate(Actual_Daily_Cases = case_when(variable == "actual_daily_cases" ~ median_value)) %>% 
    mutate(Actual_Daily_Deaths = case_when(variable == "actual_daily_deaths" ~ median_value))
 
   #assuming NA values for actual_daily_x are zero and elimiates any values beyond the current date 
  us_dat$Actual_Daily_Cases[is.na(us_dat$Actual_Daily_Cases) & us_dat$date < Sys.Date()] <- 0
  us_dat$Actual_Daily_Deaths[is.na(us_dat$Actual_Daily_Deaths) & us_dat$date < Sys.Date()] <- 0
  
  add_actual_case <- us_dat %>% group_by(date, location) %>%
    summarize(actual_daily_cases = sum(Actual_Daily_Cases)) %>%
    group_by(location) %>%
    mutate(actual_cumulative_cases = cumsum(actual_daily_cases))
  
  add_actual_death <- us_dat %>% group_by(date, location) %>%
    summarize(actual_daily_deaths = sum(Actual_Daily_Deaths)) %>%
    group_by(location) %>%
    mutate(actual_cumulative_deaths = cumsum(actual_daily_deaths))
  
  
  
  us_dat <- us_dat %>% left_join(add_actual_case, by = c("date", "location")) %>%
    left_join(add_actual_death, by = c("date", "location"))%>%
    select(-c(Actual_Daily_Cases, Actual_Daily_Deaths))
  
  #combine data in list  
  #currently only US, but set up for future use
  all_data$us_dat = us_dat
  
  message('Data cleaning done.')
  
  #save the data
  saveRDS(all_data, filename)    
  return(all_data)
} # end the get-data function which pulls data from the various online scenarios and processes/saves  



###########################################
# Define server functions
###########################################
server <- function(input, output, session) 
{

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
  scenario_var = sort(unique(us_dat$scenario))
  
  ################################################################################################
  #create the following UI elements on server and then add to UI since they depend on the variables above 
  #those variables are only defined on server
  ################################################################################################
  output$state_selector = renderUI({
    shinyWidgets::pickerInput("state_selector", "Select State(s)", state_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("Georgia") )
  })
  output$scenario_selector = renderUI({
  shinyWidgets::pickerInput("scenario_selector", "Select Scenario(s)", scenario_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("status_quo") )
  })

    
  #watch the choice for the x-scale and choose what to show underneath accordingly
  observeEvent(input$xscale,
  {
   if (input$xscale == 'x_count')
   {
     #Add a reactive range to slider
     shiny::updateSliderInput(session, "x_limit", min = 1,  max = 500, step = 10, value = 1 )
   } else
   {
     shiny::updateSliderInput(session, "x_limit", min = mindate,  max = Sys.Date(), value = defaultdate )
   }
  }) #end observe event  

  

  ###########################################
  # function that takes data and makes plots with plotly
  ###########################################
  make_plotly <- function(all_dat, location_selector, scenario_selector, daily_tot,
                          xscale, yscale, absolute_scaled, x_limit, conf_int, ylabel, outtype)
  {
    
    #create name for outcome
    outcome = paste0(daily_tot,"_",outtype)
    ylabel = paste0(daily_tot," ",outtype)
        
    #daily/cumulative does not exist for Transmissionstrength, so adjust for that
    if (outtype == 'Transmissionstrength') {outcome = outtype}
    
    #filter data based on user selections
    #keep all outcomes/variables for now so we can do x-axis adjustment
    #filtering of only the outcome to plot is done after x-scale adjustment
    plot_dat <- all_dat %>%   filter(location %in% location_selector) %>%      
                                     filter(scenario %in% scenario_selector) %>%
                                     group_by(scenario,location) %>%
                                     arrange(date) %>%
                                     ungroup()
    #adjust x-axis as needed 
    {
      #filter by date limit
      plot_dat <- plot_dat %>%
                  filter(date >= x_limit) 
    }
       
    # if we want scaling by 100K, do extra scaling 
    # this needs to be applied to all values, including CI, so need to figure out how to do
    # don't apply to the transmissionstrength since it's an input
    if ((absolute_scaled == 'scaled') && (outtype != "Transmissionstrength"))
    {
      #plot_dat <- plot_dat %>% mutate(outcome = outcome / populationsize * 100000) 
    } #end scaling function
 
    linesize = 1.5
    ncols = max(3,length(unique(plot_dat$location))) #number of colors for plotting
    
    # make plot
    if(outtype != "Transmissionstrength")
    {
      pl <- plot_dat %>% 
          filter(variable == outcome) %>%
          group_by(scenario,location) %>%
          arrange(date) %>%
          plotly::plot_ly() %>% 
          plotly::add_trace(x = ~date, y = ~median_value, type = 'scatter', 
                                 mode = 'lines', 
                                 linetype = ~scenario, 
                                 line = list(width = linesize), #text = tooltip_text, 
                                 color = ~location, colors = brewer.pal(ncols, "Dark2")) %>%
                          layout(yaxis = list(title=ylabel, type = yscale, size = 18)) %>%
                          layout(legend = list(orientation = "h", x = 0.2, y = -0.3))
      
      #add actual data on top of model data
      #need to fix cumulative data and possibly stremline code 
      if((outtype == "Cases") && (daily_tot == "Daily")){
        pl <- pl %>% plotly::add_trace(x = ~date, y = ~actual_daily_cases, type = 'scatter',
                                       mode = 'lines',
                                       linetype = "Reported Cases",
                                       line = list(width = linesize))
      }
      
      if((outtype == "Deaths") && (daily_tot == "Daily")){
        pl <- pl %>% plotly::add_trace(x = ~date, y = ~actual_daily_deaths, type = 'scatter',
                                       mode = 'lines',
                                       linetype = "Reported Deaths",
                                       line = list(width = linesize))
      }
      
      if((outtype == "Cases") && (daily_tot == "Cumulative")){
        pl <- pl %>% plotly::add_trace(x = ~date, y = ~actual_cumulative_cases, type = 'scatter',
                                       mode = 'lines',
                                       linetype = "Reported Deaths",
                                       line = list(width = linesize))
      }
      
      if((outtype == "Deaths") && (daily_tot == "Cumulative")){
        pl <- pl %>% plotly::add_trace(x = ~date, y = ~actual_cumulative_deaths, type = 'scatter',
                                       mode = 'lines',
                                       linetype = "Reported Deaths",
                                       line = list(width = linesize))
      }
      
      #Adds confidence interval ribbons and/or current date bar
      if(conf_int == "Yes")
      {
        #add confidence interval ranges
        pl <- pl %>% add_ribbons(x = ~date, ymin = ~lower_95, ymax = ~upper_95, 
                                 name = "95% Condifence Interval", color = ~location,
                                 showlegend = FALSE) 
        #adds a verical line at the current date to all plots
        pl <- pl %>% plotly::add_segments(x = Sys.Date(), xend = Sys.Date(), 
                                          y = 0, yend = ~max(upper_95), name = "Current Date",
                                          color = I("black"), alpha = 0.5)
      }
      else #need to call else to ensure date line remains if conf_int = "No"
      {
        pl <- pl %>% plotly::add_segments(x = Sys.Date(), xend = Sys.Date(), 
                                          y = 0, yend = ~max(median_value), name = "Current Date",
                                          color = I("black"), alpha = 0.5)
      }
    } #end non-transmissions strength plots
    
    if(outtype == "Transmissionstrength")
    {
      pl <- plot_dat %>% 
        filter(variable == outcome) %>%
        group_by(scenario,location) %>%
        arrange(date) %>%
        plotly::plot_ly() %>% 
        plotly::add_trace(x = ~date, y = ~mean_value, type = 'scatter', 
                          mode = 'lines', 
                          linetype = ~scenario, 
                          line = list(width = linesize), #text = tooltip_text, 
                          color = ~scenario, colors = brewer.pal(ncols, "Dark2")) %>%
         layout(yaxis = list(title="Transmission strength", type = yscale, size = 18)) %>%
        layout(legend = list(orientation = "h", x = 0.2, y = -0.3))
      
      #adds a verical line at the current date to all plots-need to add both locations to call max(median_value) here. 
      pl <- pl %>% plotly::add_segments(x = Sys.Date(), xend = Sys.Date(), 
                                        y = 0, yend = ~max(median_value), name = "Current Date",
                                        color = I("black"), alpha = 0.5)
    }


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
    pl <- make_plotly(us_dat, input$state_selector, input$scenario_selector, input$daily_tot,
                              input$xscale, input$yscale, input$absolute_scaled, input$x_limit, input$conf_int, ylabel = 1, outtype = "Cases")
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
      pl <- make_plotly(us_dat, input$state_selector, input$scenario_selector, input$daily_tot,
                        input$xscale, input$yscale, input$absolute_scaled, input$x_limit, input$conf_int, ylabel = 1, outtype = "Allinfected")
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
      pl <- make_plotly(us_dat, input$state_selector, input$scenario_selector, input$daily_tot,
                        input$xscale, input$yscale, input$absolute_scaled, input$x_limit, input$conf_int, ylabel = 1, outtype = "Deaths")
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
      pl <- make_plotly(us_dat, input$state_selector, input$scenario_selector, "Daily",
                        input$xscale, input$yscale, "absolute", input$x_limit,  input$conf_int, ylabel = 1, outtype = "Transmissionstrength")
    }
    return(pl)
  }) #end function making death plot
  

} #end server function


#################################
# Define UI
#################################
ui <- fluidPage(
  tags$head(includeHTML(here("www","google-analytics.html"))), #this is for Google analytics tracking.
  includeCSS(here("www","appstyle.css")),
  #main tabs
  sidebarLayout(
      sidebarPanel(
        uiOutput('state_selector'),
        br(),
        uiOutput('scenario_selector'),
        shiny::div("Choose potential future scenarios."),
        br(),
        shiny::selectInput("conf_int", "Show forecast confidence interval", c("Yes" = "Yes", "No" = "No" ), selected = "Yes"),
        shiny::div("Show 95% confidence interval for forecast data."),
        br(),
        shiny::selectInput("daily_tot", "Daily or cumulative numbers", c("Daily" = "Daily", "Cumulative" = "Cumulative" )),
        shiny::div("Modify all plots to show daily or cumulative data."),
        br(),
        shiny::selectInput( "absolute_scaled","Absolute or scaled values",c("Absolute Number" = "absolute", "Per 100,000 persons" = "scaled") ),
        shiny::div("Modify the top two plots to display total counts or values scaled by the state/territory population size."),
        br(),
        sliderInput(inputId = "x_limit", "Select a date from which to start the plots.", min = mindate,  max = Sys.Date(), value = defaultdate),
        shiny::div("Modify plots to begin at a specified starting date or outcome value designated in the slider above."),
        br(),
        shiny::selectInput(  "yscale", "Y-scale", c("Linear" = "lin", "Logarithmic" = "log")),
        shiny::div("Modify outcome plots to show data on a linear or logarithmic scale."),
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
    ), #end sidebar layout     
  tags$div(
    "These interactive plots are part of CEID @ UGA's work on COVID-19 modeling. If you ended up on this site outside our main page, see", a("the link to this website", href = "https://www.covid19.uga.edu", target = "_blank" ),
    "for more information and explanations."
  )
) #end fluidpage and UI part of shiny app
#end UI of shiny app
###########################################



# Create Shiny object
shinyApp(ui = ui, server = server)

