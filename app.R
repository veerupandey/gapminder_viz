library(tidyverse)
library(gapminder)
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(plotly)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(purrr)
library(ggthemes)

app <- Dash$new(external_stylesheets = dbcThemes$SKETCHY)

# Variables
year_breaks <- c(1952, 1957, 1962, 1967, 1972, 1977, 1982, 1987, 1992, 1997, 2002, 2007)

app$layout(
  dbcContainer(
    list(
      htmlH4(
        "Our Changing World!",
        className = "h1 bg-secondary bg-gradient-secondary p-2 mb-2 text-white text-center",
      ),
      dbcLabel("What do you want to know?", className = "h4"),
      dbcRadioItems(
        id = "x_axis",
        options = list(
          list(label = "Life Expectancy", value = "lifeExp"),
          list(label = "Population", value = "pop"),
          list(label = "GDP per Capita", value = "gdpPercap")
        ),
        value = "pop",
        className = "mb-4",
        inline = TRUE
      ),
      htmlHr(),
      dbcLabel("Your Year Of Interest?", className = "h4"),
      dccSlider(
        id = "year_id",
        min = 1952,
        max = 2007,
        step = 5,
        marks = list(
          "1952" = "1952",
          "1957" = "1957",
          "1962" = "1962",
          "1967" = "1967",
          "1972" = "1972",
          "1977" = "1977",
          "1982" = "1982",
          "1987" = "1987",
          "1992" = "1992",
          "1997" = "1997",
          "2002" = "2002",
          "2007" = "2007"
        ),
        value = 2002,
        className = "mb-4"
      ),
      htmlHr(),
      htmlH2("World Trend"),
      htmlBr(),
      dccGraph(id = "plot-area")
    ),
    className = "g-0"
  )
)

app$callback(
  output("plot-area", "figure"),
  list(input("x_axis", "value"), input("year_id", "value")),
  function(xcol, year_filter) {
    if (xcol == "pop") {
      p <- ggplot(gapminder, aes(
        x = year,
        y = !!sym(xcol),
        color = continent
      )) +
        geom_line(stat = "summary", fun = sum) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = year_breaks) +
        scale_x_log10() +
        labs(x = "Year", y = "Population") +
        geom_vline(xintercept = year_filter, linetype = "longdash")
    } else if (xcol == "lifeExp") {
      p <- ggplot(gapminder, aes(
        x = year,
        y = !!sym(xcol),
        color = continent
      )) +
        geom_line(stat = "summary", fun = mean) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = year_breaks) +
        scale_x_log10() +
        labs(x = "Year", y = "Life Expectancy") +
        geom_vline(xintercept = year_filter, linetype = "longdash")
    } else if (xcol == "gdpPercap") {
      p <- ggplot(gapminder, aes(
        x = year,
        y = !!sym(xcol),
        color = continent
      )) +
        geom_line(stat = "summary", fun = mean) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = year_breaks) +
        scale_x_log10() +
        labs(x = "Year", y = "GDP Per Capita") +
        geom_vline(xintercept = year_filter, linetype = "longdash")
    } else {
      p <- ggplot(gapminder, aes(
        x = year,
        y = !!sym(xcol),
        color = continent
      )) +
        geom_line(stat = "summary", fun = mean) +
        scale_y_continuous(labels = scales::comma) +
        scale_x_continuous(breaks = year_breaks) +
        scale_x_log10() +
        geom_vline(xintercept = year_filter, linetype = "longdash")
    }
    p <- p + theme_solarized() + scale_colour_solarized()

    ggplotly(p)
  }
)

app$run_server(host = "0.0.0.0")