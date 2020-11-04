source(file = 'shiny-func.R')

server <- function(input, output, session) {
  output$data <- renderDataTable(models_results[[input$topic]])
  output$optimal_hyperparams <- renderDataTable(optimal_hyperparams)
  output$perplexity <- renderPlotly({
    withProgress(message = 'Preparing plot', value = 0, {
                 perplexity_graph(hyperparams = hyperparams,
                                  optimalk = optimalk)
          })
  })
  output$ldavis <- renderVis({json_viss[[input$topic]]})
  output$topwords <- renderPlotly({
    withProgress(message = 'Preparing plot', value = 0, {
      top_word_distribution(
        lda_fit = lda_models[[input$topic]],
        json_vis = json_viss[[input$topic]]
      )
    })
 })
 output$distributionpermonth <- renderPlotly({
   withProgress(message = 'Preparing plot', value = 0, {
     distribution_per_month_graph(data = models_results[[input$topic]])
   })
 })
 output$distributionpermonthproportion <- renderPlotly({
   withProgress(message = 'Preparing plot', value = 0, {
     distribution_per_month_proportion_graph(data = models_results[[input$topic]])
   })
 })
 checktext <- eventReactive(input$check_topic_probability, {input$checktext})
 output$checktopic <- renderDataTable({
   withProgress(message = 'Preparing DataTable', value = 0, {
     apply_model(lda_fit = lda_models[[input$topic]],
                 text = checktext(),
                 vocabulary = vocabulary,
                 json_vis = json_viss[[input$topic]],
                 seed = optimal_hyperparams[optimal_hyperparams$k == input$topic,]$seed)
   })
 })
 output$wordcludselectizer <- renderUI({
     lapply(1:input$topic, function(x) {
       box(title = paste0("topic: ", x),
       status = "primary", solidHeader = TRUE,
       collapsible = TRUE,
       loadEChartsLibrary(),
       tags$div(id=paste0("wordcloud", x), style="width:100%;height:500px;"),
       deliverChart(div_id = paste0("wordcloud", x)))
     })
 })
 observeEvent(input$topic, {
   lapply(1:input$topic, function(i) {
     withProgress(message = 'Preparing wordcloud', value = 0, {
       output_topic_wordcloud(model = lda_models[[input$topic]], i, output)
     })
   })
 })
}
