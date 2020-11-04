library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(plotly)
library(LDAvis)
library(data.table)


sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebar",
    menuItem("Dashboard", icon = icon(name = "dashboard"),
             tabName = "dashboard"),
    menuItem("data", icon = icon(name = "th"), tabName = "data"),
    menuItem("hyperparams", icon = icon(name = "th"), tabName = "hyperparams"),
    menuItem("LDAvis", icon = icon(name = 'bar-chart-o'), tabName = "LDAvis"),
    menuItem("topwords", icon = icon(name = "font", lib = "glyphicon"),
             tabName = "topwords"),
    menuItem("distribution", icon = icon(name = 'bar-chart-o'),
             tabName = "distribution"),
    menuItem("wordcloud", icon = icon(name = "cloud", lib = "glyphicon"),
             tabName = "wordcloud"),
    menuItem("chektext", icon = icon(name = "glyphicon glyphicon-pencil",
                                     lib = "glyphicon"), tabName = "checktext",
             badgeLabel = "experimental", badgeColor = "purple"),
    selectizeInput(inputId = 'topic',
                   label = 'Number of topics:',
                   multiple = FALSE,
                   choices = optimal_hyperparams$k,
                   selected = optimal_hyperparams$k[1])
  )
)

body <- dashboardBody(
  shinyDashboardThemes(theme = "onenote"),
  tabItems(
    tabItem(tabName = "dashboard", includeMarkdown(path = 'www/index.md')
    ),
    tabItem(tabName = "data",
            fluidRow(
          column(
        width = 12,
        dataTableOutput('data')
          )
            )
    ),
    tabItem(tabName = "hyperparams",
            fluidRow(
              tabBox(
                title = "",
                width = 12,
                id = "tabhyperparams",
                tabPanel("hyperparams", dataTableOutput('optimal_hyperparams')),
                tabPanel("perplexity and optimal k", plotlyOutput("perplexity"))
              )
            )
    ),
    tabItem(tabName = "LDAvis",
            fluidRow(visOutput('ldavis'))
    ),
    tabItem(tabName = "topwords",
            plotlyOutput('topwords', height = 800)),
    tabItem(tabName = "distribution",
            fluidRow(
              tabBox(
                title = "",
                width = 12,
                id = "tabdistribution",
                tabPanel("number of documents", plotlyOutput("distributionpermonth")),
                tabPanel("proportion", plotlyOutput("distributionpermonthproportion"))
              )
            )
    ),
    tabItem(tabName = "checktext",
            fluidRow(
              column(
                width = 4,
                textAreaInput(inputId = 'checktext',
                              label = "Check text to see which topic belongs:",
                              height = 800, width = 500)
              ),
              column(
                width = 1,
                offset = 1,
                actionButton(inputId = 'check_topic_probability',
                             label = 'Check topics!',
                             icon = icon(name = "arrow-alt-circle-right"),
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              ),
              column(width = 4,
                     offset = 1,
                     dataTableOutput(outputId = 'checktopic'))
            )
    ),
    tabItem(tabName = "wordcloud",
            fluidRow(
              uiOutput(outputId = "wordcludselectizer"),
            )
    )
  )
)

ui <- dashboardPage(
  header = dashboardHeader(title = 'LDA TM in R'),
  sidebar = sidebar,
  body = body
)
