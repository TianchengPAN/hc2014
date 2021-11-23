library(shinydashboard)
library(shiny)
library(DT)
library(ggplot2)
library(tidyverse)


#load data

load("hc2014.RData")
hc2014_pos <- hc2014 %>%
    filter(netincome > 0)

# create plots in EDA


ui <- dashboardPage(
    dashboardHeader(title = "Hospital Ranking"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "Introduction", icon = icon("th")),
            menuItem("EDA", tabName = "EDA", icon = icon("th")),
            menuItem("Model", tabName = "Model", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            # Introduction
            tabItem(tabName = "Introduction",
                    tags$div(class = "header",
                             align="center",
                             style = "font-size:30px;",
                             tags$p("Introductio")),
                    h4("In our dataset from the Centers for Medicare and Medicaid Service on hospital costs and profit from the 2014 fiscal year, we have 6248 samples and 5 variables.By checking the sample sizes of each state, we observe there is a huge discrepancy where the smallest sample size is 1 (in state GU and MP) and the largest sample size is 609 (in state TX). Thus we use the random effects of `state` variable for information sharing between states to have more accurate estimate. ")
            ),
            
            
            # EDA
            # include plots of netincome, log netincome, log netincome by control, log netincome vs ownership type and log netincome vs state
            tabItem(tabName = "EDA",
                    fluidRow(
                        plotOutput("EDA_plot", height = 250),
                        
                        column(5,
                               h2("Choose EDA plots"),
                               selectInput("plot_select", label = h3("Select Plot"), 
                                           choices = list("netincome" = 1, 
                                                          "log netincome" = 2,
                                                          "number of beds" = 3,
                                                          "log number of beds" = 4,
                                                          "log uterus vs ownership type" = 5,
                                                          "log uterus vs state" = 6), 
                                           selected = 1)
                        )
                        
                    ),
                    
                    h2('Insights'),
                    textOutput("description")
            ),
            
            tabItem(tabName = "Model",
                    h2('Final model'),
                    uiOutput("formula"),
                    h4('The final model is just the model include all the variables in the data with the random intercept as we have done in lab4')
            )
            
        )
    )
)

server <- function(input, output) {
     
    
    # EDA
    plot <- reactive({
        if (input$plot_select == 1) return(ggplot(data = hc2014, aes(x = netincome)) +
                                               geom_histogram(na.rm = TRUE, bins = 20) +
                                               xlim(-1e+8, 1e+8) + 
                                               theme_classic())
        if (input$plot_select == 2) return(netincome_log <- ggplot(data = hc2014_pos, aes(x = log(netincome))) +
                                               geom_histogram(na.rm = TRUE, bins = 20) + 
                                               theme_classic() )
        if (input$plot_select == 3) return(ggplot(data = hc2014_pos, aes(x = numbeds)) +
                                               geom_histogram(na.rm = TRUE, bins = 20) + 
                                               theme_classic() )
        if (input$plot_select == 4) return(ggplot(data = hc2014_pos, aes(x = log(numbeds))) +
                                               geom_histogram(na.rm = TRUE, bins = 20) + 
                                               theme_classic())
        if (input$plot_select == 5) return(ggplot(data = hc2014_pos, 
                                                  aes(y = log(netincome), x = log(numbeds), color = control)) +
                                               geom_point(alpha = 0.2) +
                                               ggtitle("By Ownership Type") +
                                               theme_classic() +
                                               theme(legend.position = "none") )
        if (input$plot_select == 6) return(ggplot(data = hc2014_pos, 
                                                  aes(y = log(netincome), x = log(numbeds), color = state)) +
                                               geom_point(alpha = 0.2) +
                                               ggtitle("By States") + 
                                               theme_classic() + 
                                               theme(legend.position = "none"))
    })
    
    des <- reactive({
        if (input$plot_select == 1) return('The distribution of net income is extremely skewed. It needs some transfermation to make the target normal')
        if (input$plot_select == 2) return('Log of negative values is undefined, we decide to delete datapoints with negative netincome value, and only use positive netincome to create the model. We now have 4174 positive netincome observations. After log transformation on positive netincome, the plot is more symmetric.')
        if (input$plot_select == 3) return('Again, the distribution of number of beds are also skewed, we might do some transfermation')
        if (input$plot_select == 4) return('We decide to log transform this variable, and the distribution of log transformed numbeds is more symmetric.')
        if (input$plot_select == 5) return('We see the general trends of log(netincome) vs log(numbeds) by ownership type and by states are similar, so we choose not to include interaction terms of log(numbeds) with control or state.')
        if (input$plot_select == 6) return('We see the general trends of log(netincome) vs log(numbeds) by ownership type and by states are similar, so we choose not to include interaction terms of log(numbeds) with control or state.')
    })
    
    output$EDA_plot <- renderPlot({
        dataplots = plot()
        print(dataplots)
    })
    
    output$description <- renderText({ 
        des()
    })
    
    # model
    output$formula <- renderUI({
        b0 = 'b_{0j} \\sim N (0,\\tau^2)\\\\'
        tau = 'p(\\tau)  = \\frac{2}{5\\pi}\\frac{1}{1+\\frac{\\tau^2}{25}}\\\\'
        beta = '\\beta_{1k}, \\beta_{2} = p(1)\\\\'
        epsilon = '\\epsilon_{ijk} \\sim N(0,\\sigma^2)' 
        withMathJax(paste0("$$log(netincome_{ijk}) = \\beta_0+b_{0j}+\\beta_{1k}ownership_{ijk}+\\beta_{2}numbeds_{ijk}+\\epsilon_{ijk}\\\\",b0,tau,beta,epsilon,"$$"))
    })
}

shinyApp(ui, server)