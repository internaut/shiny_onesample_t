# A Shiny app for visualizing how the one-sample t-test works.
#
# author: Markus Konrad <post@mkonrad.net>
#

library(shiny)
library(openintro)   # for age_at_mar
library(tibble)
library(ggplot2)


N_HEAD <- 10


ui <- fluidPage(
  shinyjs::useShinyjs(),   # to enable/disable inputs
  withMathJax(),           # for formulas
  titlePanel('The one-sample t-test'),
  sidebarLayout(
    sidebarPanel(
      selectInput('datasource', 'Data',
                  choices = c('age_at_mar', 'rivers', 'synthetic')),
      numericInput('samplesize', 'Sample size', value = 50, step = 1),
      actionButton('load', 'Load data')
    ),
    mainPanel(
      h3('Assumptions'),
      HTML('<ul>
                   <li>the samples must be independent</li>
                   <li>the samples must be approximately normally distributed</li>
                   <li>the sample size must be reasonable large</li>
                </ul>'),
      h3('Data description'),
      textOutput('data_descr'),
      h3('Hypotheses'),
      uiOutput('true_mean_label'),
      shinyjs::disabled(numericInput('true_mean', '', value = 0)),
      p('Alternative: \\(\\mu\\)'),
      shinyjs::disabled(selectInput('alt_comparison', '',
                                    choices = c('≠' = 'two.sided', '<' = 'less', '>' = 'greater'))),
      textOutput('true_mean'),
      h3(sprintf('Data (first %d rows)', N_HEAD)),
      tableOutput('table'),
      h3('Descriptive statistics and plots'),
      verbatimTextOutput('summary'),
      plotOutput('hist_dens'),
      h3('t-test: step-by-step calculations'),
      uiOutput('ttest_step1'),
      uiOutput('ttest_step2'),
      plotOutput('ttest_plot'),
      uiOutput('ttest_step3'),
      h3("R's t.test result"),
      verbatimTextOutput('ttest'),
      h3('Interpretation'),
      uiOutput('interpretation')
    )
  )
)

server <- function(input, output) {
  calc_results <- reactiveValues(y = NULL, se = NULL, t = NULL, dof = NULL, p = NULL)
  
  loaded_data <- eventReactive(input$load, {
    if (input$datasource == 'age_at_mar') {
      df <- age_at_mar
      unit <- 'years'
      descr <- 'Age at first marriage of 5,534 US women who responded to the National Survey of Family Growth 
                (NSFG) conducted by the CDC in the 2006 and 2010 cycle.'
    } else if (input$datasource == 'rivers') {
      df <- tibble(length = rivers * 1.609344)  # miles to km
      unit <- 'km'
      descr <- 'This data set gives the lengths (in km) of 141 “major” rivers in North America, as compiled by the 
                US Geological Survey.'
    } else {   # synthetic
      # TODO
    }
    
    # take a sample
    df <- df[sample(1:nrow(df), input$samplesize), ]
    
    calc_results$y <- unlist(df[,1])
    calc_results$dof <- nrow(df) - 1
    
    shinyjs::enable('true_mean')
    shinyjs::enable('alt_comparison')
    
    list(df = df, unit = unit, descr = descr)
  })
  
  output$true_mean <- renderText({ input$true_mean })
  
  output$true_mean_label <- renderUI({
    if (input$alt_comparison == 'two.sided') {
      comp <- '='
    } else if (input$alt_comparison == 'less') {
      comp <- '\\ge'
    } else {
      comp <- '\\le'
    }
    withMathJax(sprintf('Null: True mean \\(\\mu %s\\)', comp))
  })
  
  output$data_descr <- renderText({
    loaded_data()$descr
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
    
    # take the first (and only) column as the variable for which we make the histogram
    var <- colnames(df)[1]
    ggplot(df, aes(!!sym(var))) + 
      geom_histogram(aes(y = ..density..), bins = min(c(round(length(y) / 3), 30)),
                     color = 'black', fill = 'white') + 
      geom_density() +
      geom_vline(xintercept = mean_y, linetype = 'dashed') +
      geom_vline(xintercept = median_y, linetype = 'dotted') +
      theme_minimal()
  })
  
  output$ttest_step1 <- renderUI({
    y <- req(calc_results$y)
    
    # calculate the SE for a simple random sample
    n <- length(y)
    s <- sd(y)
    se <- s / sqrt(n)
    
    calc_results$se <- se
    
    withMathJax(sprintf('1. We have \\(N = %d\\) samples and a standard deviation of \\(\\hat\\sigma = %.5f\\). 
                          The standard error is hence \\(\\text{SE} = \\frac{\\hat\\sigma}{\\sqrt{N}} = %.5f\\).',
                        n, s, se))
  })
  
  output$ttest_step2 <- renderUI({
    req(calc_results$se)
    y <- req(calc_results$y)
    
    # calculate the t-score
    obs_mean <- mean(y)
    t <- (obs_mean - input$true_mean) / calc_results$se
    calc_results$t <- t
    
    if (input$alt_comparison == 'less') {
      comp <- '\\ge'
    } else if (input$alt_comparison == 'greater') {
      comp <- '\\le'
    } else {
      comp <- '='
    }
    
    withMathJax(sprintf('2. We expect the true mean to be \\(\\mu %s %.4f\\) and the observed mean was calculated 
                        as \\(\\hat \\mu = %.4f\\). This gives \\(t = \\frac{\\hat \\mu - \\mu}{\\text{SE}} = %.5f\\).',
                        comp, input$true_mean, obs_mean, t))
  })
  
  output$ttest_step3 <- renderUI({
    t <- req(calc_results$t)
    dof <- req(calc_results$dof)
    
    # calculate the p-value using the t-distribution with `dof` degrees of freedom
    p <- pt(t, df = dof,
            lower.tail = ifelse(input$alt_comparison == 'two.sided',
                                t < 0,
                                input$alt_comparison == 'less'))
    
    if (input$alt_comparison == 'two.sided') {
      p <- 2 * p
    }
    
    calc_results$p <- p
    
    withMathJax(sprintf('3. The area under the t-curve (with %d degrees of freedom) %s of \\(t = %.5f\\) is 
                        \\(p = %.5f\\).', dof,
                        ifelse(input$alt_comparison == 'two.sided', 'left and right',
                               ifelse(input$alt_comparison == 'less', 'left', 'right')), t, p))
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
    
    # plot the t-distribution
    # TODO: shade the regions left or right to the t value(s)
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
  
  output$interpretation <- renderUI({
    mu <- req(input$true_mean)
    p <- req(calc_results$p)
    y <- req(calc_results$y)
    mu_hat <- mean(y)
    
    if (input$alt_comparison == 'less') {
      null_comp <- '\\ge'
      alt_comp <- '\\le'
    } else if (input$alt_comparison == 'greater') {
      null_comp <- '\\le'
      alt_comp <- '\\ge'
    } else {
      null_comp <- '='
      alt_comp <- ifelse(mu_hat < mu, '\\le', '\\ge')
    }
    
    withMathJax(sprintf('Given that the null hypothesis is true (i.e. that the true mean is \\(\\mu %s %.4f\\)), the 
                        probability of observing a mean of \\(\\hat \\mu %s %.4f\\) is \\(p = %.5f\\).',
                        null_comp, mu, alt_comp, mu_hat, p))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
