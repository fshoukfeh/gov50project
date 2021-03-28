
library(shiny)
library(readr)
library(ggplot2)
library(ggforce)
library(tidyverse)
library(readxl)
library(ggthemes)
library(stargazer)
library(sjPlot)

scotus <- read_csv("merged_scopol.csv")

ui <- navbarPage(
    "Polarization and the Supreme Court (Checkpoint 3)",
    tabPanel("Home", 
             titlePanel("Home"),
             h3("Project Background and Motivations"),
             p("***IN PROGRESS*** In this project, I seek to determine whether levels of political polarization across the United States
               affect the Supreme Court, whether with regard to the confirmation process or the voting patterns of the Court. Go to the
               'Model' page to see the work that has been done this far."),
             h3(""),
             p("", a(href = ""), 
               ""),
             a(href = ""),
             p(),
             a(href = "")
             
    ),
    tabPanel("Model",
             fluidPage(
                 h3("Summary Description of Outcome Variable & Explanatory Variable"),
                 h4("Outcome Variable: Avg. Dissenting Votes on Supreme Court"),
                 textOutput("text2"),
                 plotOutput("plot69"),
                 h4("Explanatory Variable: Federal Partisan Conflict Index"),
                 textOutput("text1"),
                 plotOutput("plot420"),
                 h3("Summary Description of Bivariate Relationship Between Varaibles"),
                 plotOutput("plot3"),
                 textOutput("text420"),
                 h3("Hypothesis Test"),
                 textOutput("texthyp1"),
                 textOutput("texthyp2"),
                 tableOutput("table1"),
                 textOutput("texthyp4"),
                 textOutput("texthyp5"),
                 textOutput("texthyp6")
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")
    ),
    tabPanel("About", 
             titlePanel("About"),
             h3("About Me"),
             p("My name is Fawwaz Shoukfeh and I study History & Goverment. 
             You can reach me at fawwazshoukfeh@college.harvard.edu.")
    )
)

server <- function(input, output, session) {

    output$plot3 <- renderPlot({
        ggplot(scotus, aes(x = part_conflict, 
                           y = avg_dissent)) +
            geom_point() +
            labs(title = "Slight Negative Correlation Between Federal Partisan
                 Conflict and Dissenting Votes on the Supreme Court",
                 x = "Federal Partisan Conflict Index",
                 y = "Avg. Dissents on the Supreme Court",
                 caption = "Sources: The Supreme Court Database &\nFederal Reserve Bank of Philadelphia") +
            geom_smooth(se = FALSE, method = "lm", color = "coral1") +
            annotate("text", x = 200, y = 2.1, label = paste0("Correlation = ", 
                                                              round(cor(x = scotus$part_conflict, 
                                                                        y = scotus$avg_dissent, 
                                                                        method = "pearson", 
                                                                        use = "pairwise.complete.obs"), 2))) +
            theme_hc()
    }, res = 96)
    
    output$text1 <- renderText("I measure political polarization using the Federal Reserve Bank of Philadelphia's index for federal partisan conflict. 
                               It is based upon the level of disagreement among US politicians at the federal level from January 1981 to February 2021. 
                               Federal partisan conflict is measured at the interval level and ranges from 34.74 to 271.29. 
                               The mean value is 1.429113 dissenting votes per case in a month and the median is 98.44. The standard deviation is 33.7104.
                               Higher values indicate higher levels of polarization, and vice versa.")
    
    output$plot69 <- renderPlot({
        ggplot(scotus, aes(x = avg_dissent)) +
            geom_histogram(binwidth = 0.25, color = "black") +
            labs(title = "Summary Statistic Visual for Average SCOTUS Dissent",
                 y = "Frequency",
                 x = "Average SCOTUS Dissenting Votes",
                 caption = "Source: The Supreme Court Database")
    }, res = 96)
    
    output$plot420 <- renderPlot({
        ggplot(scotus, aes(x = part_conflict)) +
            geom_histogram(binwidth = 10, color = "black", fill = "cyan4") +
            labs(title = "Summary Statistic Visual for Federal Partisan Conflict",
                 y = "Frequency",
                 x = "Federal Partisan Conflict",
                 caption = "Source: The Federal Reserve of Philadelphia")
    }, res = 96)
    
    output$text2 <- renderText("I measure polarization on the Supreme Court by calculating the average number of dissenting votes per case in a month. 
                               It is based upon the level of disagreement among US politicians at the federal level from January 1981 to February 2021. 
                               Federal partisan conflict is measured at the interval level and ranges from 0 to 4. 
                               The mean value is 1.429113 dissenting votes per case in a month and the median is 1.4083.
                               The standard deviation is 0.8884982. Higher values indicate higher levels of disagreement on the Supreme Court, and vice versa.")
                              
    output$texthyp1 <- renderText("Null Hypothesis: correlation = 0")
    output$texthyp2 <- renderText("Alternative Hypothesis: correlation DNE 0")
    output$texthyp4 <- renderText("T-statistic: -3.0772")
    output$texthyp5 <- renderText("P-value: p < 0.01")
    output$texthyp6 <- renderText("Conclusion: Since the absolute value of the t-statistic is greater than 1.96 and the p-value
                                 is less than 0.01, this correlation of -0.16 is statistically significant. Accordingly, this would imply that increased political polarization 
                                 between politicians on the federal level very slightly decreases the average number of dissenting votes per case in a month. Put differently,
                                 the hypothesis test shows that political polarization has little to no effect on disagreement
                                 levels in the Supreme Court.")
    
    output$table1 <- renderTable(
        stargazer(lm(avg_dissent ~ part_conflict, data = scotus),
                  type = "text",
                  title = "Linear Regression Model of Dissenting SCOTUS Votes",
                  dep.var.labels = "Avg. Dissenting Votes",
                  covariate.labels = c("Federal Partisan Conflict")))
    
    output$text420 <- renderText("As can be seen by the graph above, the average number of dissenting votes per Supreme Case in a month
                                 is negatively correlated with the federal partisan conflict, though very weakly so. After all the Pearson
                                 correlation coefficient is only -0.16. This would mean that increased political polarization between politicians on
                                 the federal level very slightly decreases the average number of dissenting votes per case in a month. Put differently,
                                 this coeffecient and corresponding graphic would imply that political polarization has little to no effect on disagreement
                                 levels in the Supreme Court.")
}

shinyApp(ui = ui, server = server)
