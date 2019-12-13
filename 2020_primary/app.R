#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)
library(cowplot)
library(tidyverse)

# Read in rds file with cleaned up data

clean_data <- readRDS("data.rds")


# I defined input choices for factors that represent media type and outcomes outside of my UI.

factors <- c("Mean Online Media Mentions" = "stories", 
             "Mean Cable News Mentions" = "clips")

outcomes <- c("Mean Polling Percentage" = "percent", 
              "Mean Share Price" = "price")

ui <-
    
    fluidPage(
        
        # I chose a white and blue theme and titled my app
        
        theme = shinytheme("cerulean"),
        
        titlePanel("What Effect Does Media Mentions Have On 2020 Democratic Presidential Primary Polls and Betting?"),
        
        #Created a sidebar that allows the user to select media type and outcome variable. 
        
        sidebarLayout(
            sidebarPanel(
                selectInput("Media",
                            "Select Media Type:",
                            choices = factors,
                            selected = "Mean Online Media Mentions"),
                
                selectInput("outcome",
                            "Select an outcome variable:",
                            choices = outcomes,
                            selected = "Average Polling Percentage")),
            
            # I created three tabs in the main panel, each with plot or text outputs
            
            mainPanel(
                tabsetPanel(type = "tabs",
                            tabPanel("Summary Plots", 
                                     h3("Candidate Cable vs. Online Mentions"), 
                                     plotOutput("plot2"),
                                     p("These two plots show the average cable and online media mentions
                                 in 2019 for each major 2020 Democratic Presidential Candidate."),
                                     h3("Candidate Betting Share Price vs. Polling"),
                                     plotOutput("plot3"),
                                     p("These two plots show the average betting share price and polling percentage
                         in 2019 for each major 2020 Democratic Presidential Candidate.")),
                            
                            tabPanel("Explore the Relationship", 
                                     plotOutput("plot"),
                                     h3("Results"),
                                     p("The data confirmed my hypothesis. 
                                   Overall, as average media and online mentions increases, 
                                   poll percentage and betting price also increases.
                                   However, the relationship between both online and cable mentions and 
                                   share price is not as strong as mentions and polling percentage.
                                   The key difference between polling and share price is that polls
                                   answer the question of who voters support for the nomination, whereas
                                   share prices reflect who the public believes will win the nomination.
                                   Creating the  linear models revealed that as cable news clips increased, 
                                   share price increases by 0.0001723. As online news stories increases, share
                                   price increases by 0.0002737. This leads to the conclusion that online
                                   news mentions has a greater effect on share price than cable mentions.
                                   Looking at polling percentage, as cable news increases, polling percent
                                   increases by 0.01778. As online news stories increases, polling percent
                                   increases by 0.0259. This means that online news stories also has a greater
                                   impact on polling percentage than cable mentions.")),
                            tabPanel("About", htmlOutput("about")),
                            tabPanel("Video", tags$video(id="videoID", type = "video/mov",src = "2020_Primary.mov", 
                                                         controls = "controls"))
                )
            )))


#I used reactives for x and y to get the desired input

server <- function(input, output) {
    
    x_label <- reactive({
        req(input$Media)
        if(input$Media == "stories"){
            x_label <- "Mean Online Media Mentions"
        } else if(input$Media == "clips"){
            x_label <- "Mean Cable News Mentions"
        }})
    
    y_label <- reactive({
        req(input$outcome)
        if(input$outcome == "percent"){
            y_label <- "Average Polling Percentage"
        } else if(input$outcome == "price"){
            y_label <- "Mean Share Price"
        }})
    
    #I pasted strings of text describing the project
    
    
    output$about <- renderUI({
        
        str1 <- paste("Background")
        str2 <- paste("There are currently over a dozen candidates running for the 2020 Democratic nomination 
        for president of the United States. In the early stages of the nomination process, there has been lots of 
        contention over the role of the mainstream media and it's role in shaping public opinion and ultimately the 
        outcome of the primary. The mainstream media has long been victim to attacks from politicians
                      and their supporters for coverage that is seen as unfair. Recently, the media
                      has been accused of picking a favorite candidate and centering their political 
                      coverage around that candidate. This begs the question: How does media coverage 
                      effect a candidates performance? Does modern political media coverage even effect voters?")
        str3 <- paste("Data")
        str4 <- paste("This project explores this with cable mentions data, online mentions data, and polling data 
                      from FiveThirtyEight and betting data from PredictIt. The cable mentions data contains the mentions 
                      of currently declared major candidates for the 2020 Democratic primary since December 30, 2018 across 
                      CNN, Fox News, and MSNBC. Coverage is measured by splitting daily news footage into 15-second clips and
                      finding the clips that contain a mention of the candidates' first and last names. The online mentions data 
                      contains the mentions of currently declared major candidates in online news stories. The polling data is 
                      measured by aggregating national polling from several credible polling agencies on the major candidates. 
                      The betting data represents the latest 'Yes' price offered for 24 declared and undeclared presidential candidates. 
                      This data allows for the evaluation of the 2020 media coverage by comparing the coverage to public opinion 
                      through the polling and betting data.")
        str5 <- paste("Hypotheses")
        str6 <- paste("I hypothesized that there will be a positive correlation between both types of
                      media coverage and betting and polling outcomes.")
        str7 <- paste("Contact")
        str8 <- paste("This project was completed by Hoda Abdalla. I can be contacted at hodaabdalla@college.harvard.edu
                      and my github repo can be accessed at https://github.com/hodaeabdalla/2020-primary.")

        
        HTML(paste(h3(str1), p(str2), h3(str3), p(str4), h3(str5), p(str6), h3(str7), p (str8)))
    })
    
    #I piped the clean data into a ggplot of outcome vs. media for the data tab. 
    
    
    output$plot <- renderPlot({
        
        clean_data %>% 
            group_by(name) %>%
            ggplot(aes_string(x = input$Media, y = input$outcome)) + 
            geom_point() +
            labs(x = x_label(),
                 y = y_label()) +
            geom_smooth(method = "lm", se = FALSE) +
            labs(title = "The Effect of Media Mentions on Candidate Outcomes")
        
    })
    
    
    # I piped the clean data into a ggplot with all four combinations # of relationships. 
    # I made a scatter plot because it is the best method to show correlation. 
    # I used geom_smooth to add a  regression line.
    
    p1 <- clean_data %>%
        drop_na() %>%
        group_by(name) %>%
        ggplot(aes(x = reorder(name, -clips), y = clips)) +
        geom_point(size=2) + 
        geom_segment(aes(x=name, 
                         xend=name, 
                         y=0, 
                         yend=clips)) +
        theme(axis.text.x = element_text(angle=70, vjust=0.6)) +
        labs(title = "Cable Mentions", 
             x = "Candidate",
             y = "Mean Number of Mentions")
    
    p2 <- clean_data %>%
        group_by(name) %>% 
        drop_na() %>%
        ggplot(aes(x = reorder(name, -stories), y = stories)) +
        geom_point(size=2) + 
        geom_segment(aes(x=name, 
                         xend=name, 
                         y=0, 
                         yend=stories)) +
        theme(axis.text.x = element_text(angle=70, vjust=0.6)) +
        labs(title = "Online Mentions", 
             x = "Candidate",
             y = "Mean Number of Mentions")
    
    output$plot2 <- renderPlot({
        
        plot_grid(p1, p2)
    })
    
    p3 <- clean_data %>%
        group_by(name) %>%
        drop_na() %>%
        ggplot(aes(x = reorder(name, -price), y = price)) +
        geom_point(size=2) + 
        geom_segment(aes(x=name, 
                         xend=name, 
                         y=0, 
                         yend=price)) +
        theme(axis.text.x = element_text(angle=70, vjust=0.6)) +
        labs(title = "Share Price", 
             x = "Candidate",
             y = "Mean Price")
    
    p4 <- clean_data %>%
        group_by(name) %>%
        drop_na() %>%
        ggplot(aes(x = reorder(name, -percent), y = percent)) +
        geom_point(size=2) + 
        geom_segment(aes(x=name, 
                         xend=name, 
                         y=0, 
                         yend=percent)) +
        theme(axis.text.x = element_text(angle=70, vjust=0.6)) +
        labs(title = "Polling Percent", 
             x = "Candidate",
             y = "Mean Percent")
    
    
    output$plot3 <- renderPlot({
        
        
        # I used plot_grid from the cowplot package to place the plots side by side.
        
        plot_grid(p3, p4)
    })
}

#Use shinyApp function to create shiny app with ui and server

shinyApp(ui, server)


#I followed the path of Morgan Townsend's project in the fall of 2018 to figure
#out how to use reactives and how to create a sidebar
