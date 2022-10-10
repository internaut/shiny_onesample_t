#
# TODO:
#   - reset on load
#   - deactivate when no data loaded
#   - specify N to load sample
#

library(shiny)
library(openintro)   # for age_at_mar
library(ggplot2)


N_HEAD <- 10


ui <- fluidPage(
    titlePanel('Tests of Significance on univariate data'),
    sidebarLayout(
        sidebarPanel(
            selectInput('datasource', 'Data',
                        choices = c('age_at_mar', 'rivers', 'synthetic')),
            numericInput('samplesize', 'Sample size', value = 50, step = 1),
            actionButton('load', 'Load data')
        ),
        mainPanel(
           h3('Data description'),
           h3('Assumptions'),
           h3('Hypotheses'),
           textOutput('true_mean_label'),
           numericInput('true_mean', '', value = 0),
           p('Alternative: True mean is'),
           selectInput('alt_comparison', '',
                       choices = c('≠' = 'two.sided', '<' = 'less', '>' = 'greater')),
           textOutput('true_mean'),
           h3(sprintf('Data (first %d rows)', N_HEAD)),
           tableOutput('table'),
           h3('Descriptive statistics and plots'),
           verbatimTextOutput('summary'),
           plotOutput('hist_dens'),
           h3('t-test: step-by-step calculations'),
           textOutput('ttest_step1'),
           textOutput('ttest_step2'),
           plotOutput('ttest_plot'),
           textOutput('ttest_step3'),
           #verbatimTextOutput('ttest_steps'),
           h3("R's t.test result"),
           verbatimTextOutput('ttest'),
           h3('Interpretation'),
           textOutput('interpretation')
        )
    )
)

server <- function(input, output) {
    calc_results <- reactiveValues(y = NULL, se = NULL, t = NULL, dof = NULL, p = NULL)
  
    loaded_data <- eventReactive(input$load, {
        if (input$datasource == 'age_at_mar') {
            df <- age_at_mar
            unit <- 'years'
        } else if (input$datasource == 'rivers') {
            df <- data.frame(length = rivers * 1.609344)  # miles to km
            unit <- 'km'
        } else {   # synthetic
            
        }
      
        df <- df[sample(1:nrow(df), input$samplesize), ]
        
        calc_results$y <- unlist(df[,1])
        calc_results$dof <- nrow(df) - 1
        
      
        list(df = df, unit = unit)
    })
    
    output$true_mean <- renderText({ input$true_mean })
    
    output$true_mean_label <- renderText({
      if (input$alt_comparison == 'two.sided') {
        comp <- 'equals'
      } else if (input$alt_comparison == 'less') {
        comp <- 'greater than or equal'
      } else {
        comp <- 'less than or equal'
      }
      paste('Null: True mean', comp)
    })
    
    output$table <- renderTable({
      head(loaded_data()$df, N_HEAD)
    })
    
    output$summary <- renderPrint({
      req(calc_results$y)
      summary(calc_results$y)
    })
    
    output$hist_dens <- renderPlot({
      y <- req(calc_results$y)
      df <- loaded_data()$df
      mean_y <- mean(y)
      median_y <- median(y)
      
      # TODO: labels for mean / median lines on plot
      
      var <- colnames(df)[1]
      ggplot(df, aes(!!sym(var))) + 
        geom_histogram(aes(y = ..density..), bins = min(round(c(length(y) / 3), 30)),
                       color = 'black', fill = 'white') + 
        geom_density() +
        geom_vline(xintercept = mean_y, linetype = 'dashed') +
        geom_vline(xintercept = median_y, linetype = 'dotted') +
        theme_minimal()
    })
    
    output$ttest_step1 <- renderText({
      y <- req(calc_results$y)
      
      n <- length(y)
      s <- sd(y)
      se <- s / sqrt(n)
      
      calc_results$se <- se
      
      sprintf('1. We have N = %d samples and a standard deviation of SD = %.5f. The standard error is hence SE = SD/sqrt(N) = %.5f.',
              n, s, se)
    })
    
    output$ttest_step2 <- renderText({
      req(calc_results$se)
      y <- req(calc_results$y)
      
      obs_mean <- mean(y)
      t <- (obs_mean - input$true_mean) / calc_results$se
      calc_results$t <- t
      
      if (input$alt_comparison == 'less') {
        comp <- 'greater than or equal'
      } else if (input$alt_comparison == 'greater') {
        comp <- 'less than or equal'
      } else {
        comp <- ''
      }
      
      sprintf('2. We expect the true mean to be %s y = %.4f and the observed mean is calculated as y_hat = %.4f. The difference y_hat - y divided by the SE gives t = %.5f.',
              comp, input$true_mean, obs_mean, t)
    })
    
    output$ttest_step3 <- renderText({
      t <- req(calc_results$t)
      dof <- req(calc_results$dof)
      
      p <- pt(t, df = dof,
              lower.tail = ifelse(input$alt_comparison == 'two.sided',
              t < 0,
              input$alt_comparison == 'less'))
      
      if (input$alt_comparison == 'two.sided') {
        p <- 2 * p
      }
      
      calc_results$p <- p
      
      sprintf('3. The area under the t-curve (with %d degrees of freedom) %s of t = %.5f is p = %.5f.',
              dof,
              ifelse(input$alt_comparison == 'two.sided', 'left and right',
                     ifelse(input$alt_comparison == 'less', 'left', 'right')),
              t, p)
    })
    
    output$ttest_plot <- renderPlot({
      t <- req(calc_results$t)
      dof <- req(calc_results$dof)
      lim <- max(c(abs(t) + 1, 5))
      
      if (input$alt_comparison == 'two.sided') {
        t_line <- c(-t, t)
      } else {
        t_line <- t
      }
      
      ggplot() + 
        xlim(-lim, lim) +
        geom_function(fun = dt, args = list(df = dof)) + 
        geom_vline(xintercept = t_line) +
        xlab('t') +
        ylab('density') +
        theme_minimal()
    })
    
    output$ttest <- renderPrint({
      y <- req(calc_results$y)
      mu <- req(input$true_mean)
      t.test(y, mu = mu, alternative = input$alt_comparison)
    })
    
    output$interpretation <- renderText({
      mu <- req(input$true_mean)
      p <- req(calc_results$p)
      y <- req(calc_results$y)
      mu_hat <- mean(y)
      
      if (input$alt_comparison == 'less') {
        null_comp <- 'greater than or equal'
        extreme <- 'less'
      } else if (input$alt_comparison == 'greater') {
        null_comp <- 'less than or equal'
        extreme <- 'more'
      } else {
        null_comp <- 'equal'
        extreme <- ifelse(mu_hat < mu, 'less', 'more')
      }
      
       sprintf('Given that the null hypothesis is true (i.e. that the true mean is %s %.2f), the probability of observing a mean of %.4f or %s is p = %.5f.',
               null_comp, mu, mu_hat, extreme, p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
