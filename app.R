library(shiny)
library(dplyr)
library(plotly)
library(DT)
### TODO
# 1. Remove the ribbon
# 2. Optional show table
# 3. Remove the legend
# 4. Weird value$col thing
# 5. Get rid of xlab when n > 50 (or adjust dynamically)

## Global constants
starting.n <- 10
max.n <- 2000
num.ticks <- 20
starting.p <- 0.5
fixed.width <- 0.2
auto.draw.lines <- 200
transition.to.lines <- 300
min.prob.to.show <- 0.0001
starting.output <- "Click on a bar, or drag over many bars, to compute the probability.  Double click to reset."



#
### Functions for printing probability strings with and without MathJax
#

bar.width <- function(n){
  ifelse(n <= 20,
         0.3,
         0.2)
}

prob.str.begin <- function(with.mathjax) {
  paste(ifelse(with.mathjax, 
               "\\(P\\left(",
               "P("))
}

prob.str.close.paren <- function(with.mathjax){
  ifelse(with.mathjax,
         "\\right) = ",
         ") = ")
}

prob.str.prob <- function(p, with.mathjax, digits) {
  paste(round(p, digits), 
        ifelse(with.mathjax, "\\)", ""))
}

prob.str.outer <- function(inner.str, p, with.mathjax, digits) {
  paste(prob.str.begin(with.mathjax), 
        inner.str,
        prob.str.close.paren(with.mathjax),
        prob.str.prob(p, with.mathjax, digits),
        sep = "")
}

prob.equal.str <- function(x, p, with.mathjax = FALSE, digits = 3) {
  inner <- paste("X =", x)
  prob.str.outer(inner, p, with.mathjax, digits)
}

prob.one.tail.str <- function(x, p, with.mathjax = FALSE, lower.tail = TRUE, digits = 3) {
  inner <- paste("X",
                 ifelse(lower.tail,
                        ifelse(with.mathjax,"\\le "," <= "),
                        ifelse(with.mathjax,"\\ge "," >= ")),
                 x)
  prob.str.outer(inner, p, with.mathjax, digits)
}

prob.two.tail.str <- function(lower, upper, p, with.mathjax = FALSE, digits = 3) {
  inner <- paste("X", 
                 ifelse(with.mathjax, "\\le", "<= "),
                 lower,
                 ifelse(with.mathjax,
                        "\\text{ or } X",
                        "or X"),
                 ifelse(with.mathjax,  "\\ge ", " >= "),
                 upper)
  prob.str.outer(inner, p, with.mathjax, digits)
}

prob.between.str <- function(lower, upper, p, with.mathjax = FALSE, digits = 3) {
  inner <- paste(lower,
                 ifelse(with.mathjax,
                        "\\le X\\le ",
                        " <= X <= "),
                 upper)
  ifelse(lower == upper,
         prob.equal.str(lower, p, with.mathjax, digits),
         prob.str.outer(inner, p, with.mathjax, digits))
}

#
### Creating the data frame with binomial probabilites
#

data.binomial <- function(n, p) {
  data.frame(x = 0:n,
               #factor(0:n,
               #         levels = as.character(0:n)),
             Probability  = dbinom(0:n, n, p),
             width = rep(fixed.width, n + 1)) %>%
    mutate(text = prob.equal.str(x, Probability)) 
}

#
### Other utility functions
#

two.tail.limits <- function(value, n, p) {
  exp.val <- n*p
  if (value <= exp.val) {
    lower <- value
    d <- exp.val - value
    upper <- ceiling(exp.val + d)
   } else {
    upper <- value
    d <- upper - exp.val
    lower <- floor(exp.val - d)
   }
  return(list(lower = lower, upper = upper))
}

update.values <- function(session, n, p) {
  
    updateNumericInput(session, 
                       "left", 
                       value = qbinom(0.05, n, p),
                       min = 0,
                       max = n)
    
    updateNumericInput(session, 
                       "right", 
                       value = qbinom(0.95, n, p),
                       min = 0,
                       max = n)
    
    updateNumericInput(session, 
                       "two_tail", 
                       value = qbinom(0.025, n, p),
                       min = 0,
                       max = n)
    updateNumericInput(session, 
                       "left_drag", 
                       value = NULL,
                       min = 0,
                       max = n)
    
    updateNumericInput(session, 
                       "right_drag", 
                       value = NULL,
                       min = 0,
                       max = n)
} 
#
### User Interface
#


#if (interactive()) {
  options(device.ask.default = FALSE)
  
  # Define UI
  ui <- fluidPage(
    
    # Application title
    titlePanel("Exact Binomial Probability"),
    
    sidebarLayout(
      
      # Sidebar with a slider input
      sidebarPanel(
        withMathJax(),
        numericInput("n", "Number of Trials:", starting.n, min = 1, max = max.n),
        numericInput("p", "P(Success)", starting.p, min = 0, max = 1),
          # TO DO ... Add input boxes for the drag?
        radioButtons("type", h3(" Selection Method"), 
                    choices = list("Drag" = 1, 
                                   "Left-tail" = 2,
                                   "Right-tail" = 3,
                                   "Two-tail" = 4),
                                   # TODO ? "Left- and Right-tail", 5), 
                    selected = 1,
                    inline = TRUE),
        #verbatimTextOutput("click"),
        conditionalPanel(
          condition = "input.type == 1",
          # TO DO ... Add input boxes for the drag?
          numericInput("left_drag", "Lower Bound:", NULL, min = 0, max = max.n),
          numericInput("right_drag", "Upper Bound:", NULL, min = 0, max = max.n)),
        conditionalPanel(
          condition = "input.type == 2",
          numericInput("left", "Upper Bound:", qbinom(0.05, starting.n, starting.p), min = 0, max = max.n)),
        conditionalPanel(
          condition = "input.type == 3",
          numericInput("right", "Lower Bound:", qbinom(0.95, starting.n, starting.p), min = 0, max = max.n)),
        conditionalPanel(
          condition = "input.type == 4",
          numericInput("two_tail", "Value", qbinom(0.95, starting.n, starting.p), min = 0, max = max.n)),
        h3("Probability"),
        uiOutput("new_prob"),
        textOutput("new_lower"),
        textOutput("new_upper"),
        checkboxInput("table", "Show Table", value = FALSE, width = NULL),
        conditionalPanel(
          condition = "input.table == true",
          numericInput("table_filter", "Hide probabilities smaller than:", 0.005, min = 0, max = 1))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("plot"),
        conditionalPanel(
          condition = "input.table == true",
          dataTableOutput('table'))
      )
    )
  )
  #}


#
### Server
#
server <- shinyServer(function(input, output, session) {
  
  # Reactive structure to track changes to bar color
  values <- 
    reactiveValues()
  
  observeEvent(input$tail,
               updateCheckboxInput(session, "itail", value = input$tail))
  
  observeEvent(input$n, {
    update.values(session, input$n, input$p)
  })

  observeEvent(input$p, {
    update.values(session, input$n, input$p)
  })
  
  
  output$new_prob <- renderUI({
    if (input$type == 2) {
        values$selected <- 0:input$left
        withMathJax(prob.one.tail.str(input$left, 
                                      values$p,
                                      with.mathjax = TRUE))
    } else if (input$type == 3) {
        values$selected <- input$right:input$n
        withMathJax(prob.one.tail.str(input$right,
                                      values$p,
                                      with.mathjax = TRUE,
                                      lower.tail = FALSE))
  } else if (input$type == 4) {
        exp.val <- input$n*input$p
        limits <- two.tail.limits(input$two_tail, input$n, input$p)
        values$selected <- c(0:limits$lower,limits$upper:input$n)
        withMathJax(prob.two.tail.str(limits$lower,
                                      limits$upper,
                                      values$p,
                                      with.mathjax = TRUE))
  } else {
        d <- event_data("plotly_selected")
        if (!is.null(d)) {
            values$d<- d
            lower_drag <- min(values$d$x)
            lower_box <- input$left_drag
            values$new_lower <- ifelse(lower_box != lower_drag,
                                       lower_box,
                                       lower_drag)
            upper_drag <- min(values$d$x)
            upper_box <- input$right_drag
            values$new_upper <- ifelse(upper_box != upper_drag,
                                       upper_box,
                                       upper_drag)
            values$selected <- values$new_lower:values$new_upper
        }
        withMathJax(ifelse(is.null(d),
                           "Drag to calculate the probability for any number of bars (double-click to clear)",
               prob.between.str(min(values$selected), max(values$selected), values$p, with.mathjax = TRUE)))
  }
  })
  
  output$plot <- renderPlotly({
    data <- 
      data.binomial(input$n, input$p) %>%
      mutate(col = x %in% values$selected)
    all.selected <- sum(data$col) == nrow(data)
    if (input$n > 20) {
      data <-
        data %>%
        filter(Probability > min.prob.to.show) 
    }
    if (input$n <= transition.to.lines) {
            p <- 
              ggplot(data, aes(x = x, 
                               y = Probability, 
                               fill = col,
                               text = text)) + 
              geom_bar(stat="identity", 
                       width = bar.width(input$n)) +
              geom_point(aes(alpha = 0), shape = 20, size = 0.1) +
              scale_fill_manual(values=c(ifelse(all.selected,"red", "black"), "red")) + 
              theme(legend.position="none") +
              scale_x_continuous(breaks = seq(min(data$x), max(data$x), length.out = min(nrow(data), num.ticks)) %>% floor)
    } else {
            d <- data %>% 
              mutate(color = ifelse(col,
                                    "red",
                                    "grey"))
            
            p <- ggplot(data) + 
                 geom_histogram(aes(x = x, 
                                    y = ..density.., 
                                    weight = Probability),
                                bins = nrow(data),
                                fill=d$color) +
                 geom_point(aes(x = x, y = Probability, alpha = 0), shape = 20, size = 0.1) +
                 theme(legend.position="none")
            
    }
    ggplotly(p, tooltip = "text") %>% layout(dragmode = "select") %>% config(displayModeBar = F) 
  })
 
  observeEvent(values$selected,
    values$p <- values$selected %>% unique %>% dbinom(input$n, input$p) %>% sum)
  
  output$table <- renderDataTable({
      datatable(data.binomial(input$n, input$p) %>% 
                select(x, Probability) %>% 
                mutate(Cumulative.x.or.lower = cumsum(Probability),
                       Cumulative.x.or.above = 1 - Cumulative.x.or.lower + Probability) %>%
                filter(Probability >= input$table_filter) %>%
                round(3), colnames = c("X", "P(X)", "P(X or lower)", "P(X or above)"))
  })
  
  output$left_drag <- 
    renderUI({
    if (is.null(values$d)) {
      renderText("")
    } else {
      numericInput("left_drag", "Upper Bound:", min(values$d$x), min = 0, max = max.n)
    }}) 
  
#  observeEvent(input$left_drag,
#               values$selected <- input$left_drag:max(values$selected))
  
  output$right_drag <- 
    renderUI({
    if (is.null(values$d)) {
      renderText("")
    } else {
      numericInput("right_drag", "Upper Bound:", max(values$d$x), min = 0, max = max.n)
    }}) 
  
  observeEvent(values$d,{
      lower <- ifelse(is.null(values$d),
                      NULL,
                      min(values$d$x))
      upper <- ifelse(is.null(values$d),
                      NULL,
                      max(values$d$x))
      updateNumericInput(session, 
                         "left_drag", 
                         value = lower,
                         min = 0,
                         max = upper)
      
      updateNumericInput(session, 
                         "right_drag", 
                         value = upper,
                         min = lower,
                         max = n)
  })
  
  
observeEvent(input$right_drag,
      updateNumericInput(session, 
                         "left_drag", 
                         value = input$left_drag,
                         min = 0,
                         max = input$right_drag))

observeEvent(input$left_drag,
      updateNumericInput(session, 
                         "right_drag", 
                         value = input$right_drag,
                         min = input$left_drag,
                         max = input$n))
  
  output$brush <-  renderUI({
    d <- event_data("plotly_selected")
    values$d<- d
    out2 <- ifelse(is.null(d),
                   "",
                   prob.between.str(min(values$selected), max(values$selected), values$p, with.mathjax = TRUE))
    if (is.null(d)) "Click and drag events (i.e., select/lasso) appear here (double-click to clear)" else withMathJax(out2)
  })
})

shinyApp(ui, server)