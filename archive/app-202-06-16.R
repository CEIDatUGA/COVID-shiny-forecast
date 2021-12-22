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
     
   
  us_dat_raw <- readr::read_csv("https://raw.githubusercontent.com/CEIDatUGA/COVID-stochastic-fitting/master/output/us_current_results.csv") %>%
    #fix NAs on "data" scenario by recoding mean to median
    mutate(median_value = ifelse(sim_type == "data", mean_value, median_value))
  
  us_dat <- us_dat_raw %>% select(location,date,variable,sim_type,median_value,lower_95,upper_95) %>%
                           left_join(us_popsize, by = "location") %>%
                           rename(populationsize = pop_size, value = median_value, scenario = sim_type) %>%
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
  navbarPage( title = "COVID-19 Forecast", id = 'current_tab', selected = "us", header = "",
              tabPanel(title = "US States", value = "us",
                       sidebarLayout(
                         sidebarPanel(
                           shinyWidgets::pickerInput("state_selector", "Select State(s)", state_var, multiple = FALSE,options = list(`actions-box` = TRUE), selected = c("Georgia") ),
                           shiny::div("US is at start of state list."),
                           br(),
                           shinyWidgets::pickerInput("scenario_selector", "Select Scenarios(s)", scenario_var, multiple = TRUE,options = list(`actions-box` = TRUE), selected = c("data", "status_quo") ),
                           shiny::div("Choose potential future scenarios (see 'About' tab for details)."),
                           br(),
                           shiny::selectInput( "case_death",   "Outcome",c("Cases" = "Cases", "Hospitalizations" = "Hospitalized", "Deaths" = "Deaths")),
                           shiny::div("Modify the top plot to display cases, hospitalizations, or deaths."),
                           br(),
                           shiny::selectInput("conf_int", "Show forecast confidence interval", c("Yes" = "Yes", "No" = "No" ), selected = "No"),
                           shiny::div("Show 95% confidence interval for forecast data."),
                           br(),
                           shiny::selectInput("daily_tot", "Daily or cumulative numbers", c("Daily" = "Daily", "Total" = "Total" )),
                           shiny::div("Modify all three plots to show daily or cumulative data."),
                           br(),
                           shiny::selectInput("show_smoother", "Add trend line", c("No" = "No", "Yes" = "Yes")),
                           shiny::div("Shows a trend line for cases/hospitalizations/deaths plot."),
                           br(),
                           shiny::selectInput( "absolute_scaled","Absolute or scaled values",c("Absolute Number" = "actual", "Per 100,000 persons" = "scaled") ),
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
                           plotlyOutput(outputId = "case_death_plot", height = "500px"),
                         ) #end main panel
                       ) #end sidebar layout     
              ), #close US tab
              
              tabPanel( title = "About", value = "about",
                        tagList(    
                          fluidRow( #all of this is the header
                            tags$div(
                              id = "bigtext",
                              "This COVID-19 tracker is brought to you by the",
                              a("Center for the Ecology of Infectious Diseases",  href = "https://ceid.uga.edu", target = "_blank" ),
                              "and the",
                              a("College of Public Health", href = "https://publichealth.uga.edu", target = "_blank"),
                              "at the",
                              a("University of Georgia.", href = "https://www.uga.edu", target = "_blank"),
                              "It was developed by",
                              a("Robbie Richards,", href = "https://rlrichards.github.io", target =  "_blank"),
                              a("William Norfolk", href = "https://github.com/williamnorfolk", target = "_blank"),
                              "and ",
                              a("Andreas Handel.", href = "https://www.andreashandel.com/", target = "_blank"),
                              'scenario code for this project can be found',
                              a( "in this GitHub repository.", href = "https://github.com/CEIDatUGA/COVID-shiny-tracker", target = "_blank" ),
                              'We welcome feedback and feature requests, please send them as a',
                              a( "GitHub Issue", href = "https://github.com/CEIDatUGA/COVID-shiny-tracker/issues", target = "_blank" ),
                              'or contact',
                              a("Andreas Handel.", href = "https://www.andreashandel.com/", target = "_blank")
                            ),# and tag
                            tags$div(
                              id = "bigtext",
                              "We currently include 4 different data scenarios for US states.", 
                              a("The Covid Tracking Project",  href = "https://covidtracking.com/", target = "_blank" ),
                              "data scenario reports all and positive tests, hospitalizations (some states) and deaths. We interpret positive tests as corresponding to new cases. The",
                              a("New York Times (NYT),", href = "https://github.com/nytimes/covid-19-data", target = "_blank" ),
                              a("USA Facts", href = "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/", target = "_blank" ),
                              "and",
                              a("Johns Hopkins University Center for Systems Science and Engineering (JHU)", href = "https://github.com/CSSEGISandData/COVID-19", target = "_blank" ),
                              "scenarios report cases and deaths."
                              ), 
                            tags$div(
                              id = "bigtext",
                              "For the county level plots, we use JHU data. (NY Times and USA Facts also provide data on the county level, however all 3 data scenarios are very similar and for speed/memory purposes we decided to only display one county level data scenario.)"         
                              ), 
                            tags$div(
                              id = "bigtext",
                              "World data comes from 2 different scenarios. One scenario is the", 
                              a("Johns Hopkins University Center for Systems Science and Engineering (JHU)", href = "https://github.com/CSSEGISandData/COVID-19", target = "_blank" ),
                              "the other scenario is",
                              a("Our World in Data (OWID).", href = "https://github.com/owid/covid-19-data/tree/master/public/data", target = "_blank" ),
                              "Both scenarios provide case and death data, OWID also provides testing data for some countries. For OWID, we assume reported cases correspond to positive tests."
                            ),
                            tags$div(
                              id = "bigtext",
                              "For more details on each data scenario, see their respective websites. Note that some data scenarios only report some data. Also, numbers might not be reliable, which can lead to nonsensical graphs (e.g. negative new daily cases/deaths or the fraction of positive tests being greater than 1). We make no attempt at cleaning/fixing the data, we only display it."
                            ),              
                            tags$div(
                              id = "bigtext",
                              a( "The Center for the Ecology of Infectious Diseases", href = "https://ceid.uga.edu", target = "_blank" ),
                              'has several additional projects related to COVID-19, which can be found on the',
                              a( "CEID COVID-19 Portal.", href = "http://2019-coronavirus-tracker.com/", target = "_blank" )
                            ), #Close the bigtext text div
                            tags$div(
                              id = "bigtext",
                              "If you are interested in learning more about infectious disease epidemiology and modeling, check out", 
                              a("our (slightly advanced) interactive modeling software and tutorial.", href = "https://shiny.ovpr.uga.edu/DSAIDE/", target = "_blank" )
                            ) #Close the bigtext text div
                          ), #close fluidrow
                          fluidRow( #all of this is the footer
                            column(3,
                                   a(href = "https://ceid.uga.edu", tags$img(src = "ceidlogo.png", width = "100%"), target = "_blank"),
                            ),
                            column(6,
                                   p('All text and figures are licensed under a ',
                                     a("Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.",
                                       href = "http://creativecommons.org/licenses/by-nc-sa/4.0/", target = "_blank"),
                                     'Software/Code is licensed under ',
                                     a("GPL-3.", href = "https://www.gnu.org/licenses/gpl-3.0.en.html" , target =  "_blank"),
                                     'See scenario data sites for licenses governing data.',
                                     a("UGA's Privacy Policy.", href = "https://eits.uga.edu/access_and_security/infosec/pols_regs/policies/privacy/" , target =  "_blank"),
                                     align = "center",
                                     style = "font-size:small"
                                   ) #end paragraph
                            ), #end middle column
                            column(3,
                                   a(href = "https://publichealth.uga.edu", tags$img(src = "cphlogo.png", width = "100%"), target = "_blank")
                            ) #end left column
                          ) #end fluidrow
                        ) #end taglist
              ) #close about tab
  ) #close NavBarPage
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
    
    ######FUTURE NOTE: right now plotly is plotting only the median values for "daily/total cases/deaths/hosp" may be better to change to max/mean values to match CEID's current patterm
    # make plot
    pl <- plotly::plot_ly(p_dat) %>% 
          plotly::add_trace(x = ~time, y = ~value, type = 'scatter', 
                                 mode = 'lines+markers', 
                                 linetype = ~scenario, symbol = ~location,
                                 line = list(width = linesize), text = tooltip_text, 
                                 color = ~scenario, colors = brewer.pal(ncols, "Dark2")) %>%
                          layout(yaxis = list(title=y_labels[ylabel], type = yscale, size = 18)) %>%
                          layout(legend = list(orientation = "h", x = 0.2, y = -0.3))
    
    if(conf_int == "Yes"){
      #add confidence interval ranges
      pl <- pl %>% add_ribbons(x = ~time, ymin = ~lower_95, ymax = ~upper_95) %>%
        plotly::add_segments(x = Sys.Date(), xend = Sys.Date(), 
                             y = 0, yend = ~max(upper_95)+100, name = "Current Date",
                             color = I("black"), alpha = 1)
    }
    else
    {
      #adds a verical line at the current date
      pl <- pl %>% plotly::add_segments(x = Sys.Date(), xend = Sys.Date(), 
                           y = 0, yend = ~max(value)+100, name = "Current Date",
                           color = I("black"), alpha = 0.5)
    }
    # if requested by user, apply and show a smoothing function 
    if (show_smoother == "Yes")
    #if (outname == "outcome" && show_smoother == "Yes")
    {
      if (any(location_selector %in% p_dat$location))
      {
        p_dat2 <- p_dat  %>% select(location,scenario,value,time) %>% drop_na() %>%
          group_by(location) %>%
          filter(n() >= 2) %>%  
          mutate(smoother = loess(value ~ as.numeric(time), span = .4)$fitted) %>%    
          ungroup()
        
        pl <- pl %>% plotly::add_lines(x = ~time, y = ~smoother, 
                                       color = ~location, data = p_dat2, 
                                       line = list( width = 2*linesize),
                                       opacity=0.3,
                                       showlegend = FALSE) 
     }
     else
     {
        stop(safeError("Please select a different data scenario or location. The selected location(s) is not present in the chosen scenario"))
     }
    } #end smoother if statement
    return(pl)
  }
  
  ###########################################
  #function that makes case/death plot for US tab
  ###########################################
  output$case_death_plot <- renderPlotly({
    pl <- NULL
    if (!is.null(input$scenario_selector))
    {
    #create plot
    pl <- make_plotly(us_dat, input$state_selector, input$scenario_selector, input$case_death, input$daily_tot,
                              input$xscale, input$yscale, input$absolute_scaled, input$x_limit, input$current_tab,
                              input$show_smoother, input$conf_int, ylabel = 1, outtype = '')
    }
    return(pl)
  }) #end function making case/deaths plot


} #end server function

# Create Shiny object
shinyApp(ui = ui, server = server)