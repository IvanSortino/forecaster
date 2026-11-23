# app/view/

box::use(
  shiny[div, moduleServer, NS, tagList, tags, hr],
  shiny.router[route_link],
)
box::use(
  app / logic / general_utils[...],
  app / styles / colors[custom_blue],
)

heading_color <- "#1f2933"
text_color <- "#52606d"

# One card per tool; the button is omitted for tools not released yet.
tool_card <- function(title, text, route = NULL, label = NULL, width = "col-md-6") {
  div(class = paste(width, "mb-4"),
      div(class = "h-100 p-4 d-flex flex-column",
          style = "border: 1px solid #dde3ea; border-radius: 8px;",
          tags$h4(title, class = "mb-2", style = sprintf("color: %s; font-weight: 600;", heading_color)),
          tags$p(text, class = "mb-3", style = sprintf("color: %s; line-height: 1.6;", text_color)),
          if (!is.null(route)) {
            tags$a(href = route_link(route), label,
                   class = "btn mt-auto align-self-start px-3",
                   style = sprintf("background: %s; color: #fff; border: none; border-radius: 6px; font-weight: 500;", custom_blue))
          }
      )
  )
}

section <- function(title, ...) {
  div(class = "mb-5",
      tags$h3(title, class = "mb-3", style = sprintf("color: %s; font-weight: 600;", heading_color)),
      div(class = "row", ...)
  )
}


#' @export
ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "jumbotron text-center",
        tags$h1("Forecaster", class = "display-4"),
        tags$p("Financial time series analysis and forecasting", class = "lead"),
        hr(class = "my-4")
    ),

    div(class = "container mb-5",
        tags$h2("Introduction", class = "mb-3"),
        tags$p("Forecaster analyzes stock price data and forecasts it with time series models."),
        tags$p("With Forecaster you can:"),
        tags$ul(
          tags$li("Select stocks from several markets and plot their prices"),
          tags$li("Run statistical tests on financial time series"),
          tags$li("Fit GARCH and Prophet forecasting models"),
          tags$li("Backtest models to measure forecast accuracy"),
          tags$li("Forecast future stock prices")
        ),
        hr(class = "my-4")
    ),

    div(class = "container",
        section("Data selection and analysis",
                tool_card("Stock selection", "Browse stocks from several markets and exchanges, then pick one to analyze.",
                          "stockInfo", "Explore stocks"),
                tool_card("Stock analysis", "Run statistics and plots on the selected stock.",
                          "stockAnalysis", "Analyze data")
        ),
        section("Prophet modeling",
                tool_card("Prophet forecasting", "Forecast with Facebook's Prophet model. Prophet detects trend and seasonality automatically.",
                          "prophetForecast", "Build Prophet model", width = "col-12")
        ),
        section("GARCH analysis",
                tool_card("Fit GARCH model", "Fit a GARCH model to capture volatility clustering and heteroscedasticity in financial data.",
                          "garchFit", "Fit model", width = "col-md-4"),
                tool_card("Backtest GARCH", "Backtest a fitted GARCH model and score it on several validation metrics.",
                          "garchBacktest", "Backtest", width = "col-md-4"),
                tool_card("GARCH forecast", "Forecast volatility and prices from a fitted GARCH model, with confidence intervals.",
                          "garchForecast", "Forecast", width = "col-md-4")
        ),
        section("ARIMA analysis",
                tool_card("ARIMA modeling", "ARIMA stands for autoregressive integrated moving average. ARIMA is under development and will ship in the next release.",
                          width = "col-12")
        )
    )
  )
}



#' @export
server <- function(id, ...) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
