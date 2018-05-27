shinyServer(function(input, output, session) {
  
  selectedData <- reactive({
    index1 = data$day == input$day
    return(data[index1])
  })
  
  geoData <- reactive({
    data = selectedData()
    if(input$target == "Target 1")
      t = data[, list(Freq = mean(target1), Count = length(customer_ID)),by="state"]
    else
      t = data[, list(Freq = mean(target2),Count = length(customer_ID)),by="state"]
    return(t)
  })
  
  output$geoChart <- renderGvis({
    data = geoData()
    g = gvisGeoChart(data = data, locationvar = "state", colorvar = "Freq", sizevar = "Count",
                     options = list(region="US", displayMode="region", resolution="provinces", width = 900, height = 500, keepAspectRatio="false"))
    return(g)
  })
  
  output$datatable <- renderDataTable(
    geoData(),
    options = list(pageLength = 10),
  )
  
  convertToFactor <- function(X)
  {
    for(i in 1:ncol(X))
    {
      if( class(X[[i]]) == "character")
        X[[i]] = as.factor(X[[i]])
    }
    return(X)
  }
  
  learningData <- reactive({
    target = "target1"
    if(input$predicted_target == "Target 2")
      target = "target2"
    train = convertToFactor(data[, c(target, input$var), with=F])
    colnames(train) = c("target", colnames(train)[2:ncol(train)])
    index = sample(1:nrow(train), size = round(input$proportion * nrow(train)))
    tr = train[index]
    te = train[-index]
    return(list(tr=tr,te=te))
  })
  
  calculGBM <- reactive({
    data = learningData()
    param = c(input$ntrees, input$interaction, input$learning)
    g = gbm(target ~ ., data = data$tr, distribution = "bernoulli", n.trees = param[1], interaction.depth = param[2], shrinkage = param[3])
    p = predict(g, data$te, n.trees = param[1], type = "response")
    return(list(p=p, model=g, te=data$te))
  })
  
  output$roc <- renderPlot({
    data = calculGBM()
    roc.plot(data$te$target, data$p)
  })
  
  output$importance <- renderDataTable({
    data = calculGBM()
    return(summary(data$model, plotit=F))
  })
})
