#  ----------------------------------------------------------------------------
#  GGPLOT FOR PREDICTION MODELS
#  File: ggmodel.R
#  (c) 2016 - Enrique Pérez Herrero
#  Apache License Version 2.0, January 2004
#  Start: 10/Jan/2016
#  End:   12/Nov/2016
#  ----------------------------------------------------------------------------

# CODE WORKS WITH MODELS FROM "randomForest" and "caret" packages

suppressMessages(require(scales))
require(ggplot2)
require(reshape2)

col2hex <- function(cname)
{
    colMat <- col2rgb(cname)
    rgb(red   = colMat[1,] / 255,
        green = colMat[2,] / 255,
        blue  = colMat[3,] / 255)
}

# PLOT RF MODEL ERROR AND OOB
ggplotModelRF <- function(model,...) {
    model.data <- as.data.frame(model$err.rate)
    model.data <-
        cbind("Tree" = as.numeric(row.names(model.data)), model.data)
    model.data <-
        melt(model.data, id.vars = "Tree", value.name = "Error")
    num.colors <- ncol(model$err.rate) - 1
    color.values <- c(rainbow(num.colors), col2hex("black"))
    
    p <- ggplot() +
        ggtitle("Random Forest Model Error") +
        geom_line(data = model.data[model.data$variable != "OOB",],
                  aes(x = Tree, y = Error, colour = variable)) +
        geom_line(data = model.data[model.data$variable == "OOB",],
                  aes(x = Tree, y = Error, colour = variable), size  = 1) +
        scale_color_manual(values = color.values) +
      ylab("") +
      theme_minimal()
    return(p)
}

# PLOT MODEL MeanDecreaseGini vs MeanDecreaseAccuracy
# Only works if importance = TRUE
ggGiniAccPlot <- function(model, names_lim = 100) {
    imp <- as.data.frame(model$importance)
    p <- ggplot(data = imp, aes(x = MeanDecreaseGini,
                                y = MeanDecreaseAccuracy)) +
        geom_point() +
        geom_text(aes(label = ifelse(
            MeanDecreaseGini >= names_lim, row.names(imp), ""
        )),
        hjust = 0, vjust = -1) +
        stat_smooth()
    return(p)
}


# PLOT RF MeanDecreaseGini
ggMeanDecreaseGiniPlot <- function(model, num = 25) {
    df <- as.data.frame(model$importance)
    df <- cbind("Variable" = as.factor(row.names(df)), df)
    df <-
        head(df[with(df, order(-MeanDecreaseGini, Variable)),], num)
    p <-
        ggplot(data = df, aes(
            x = reorder(Variable, MeanDecreaseGini),
            y = MeanDecreaseGini,
            fill = reorder(Variable, MeanDecreaseGini)
        )) +
        coord_flip() +
        xlab("") +
        guides(fill = FALSE) +
        geom_bar(stat = "identity", width = .90) +
        theme_minimal()
    return(p)
}


# PLOT RF MeanDecreaseAccuracy
# Only works if importance = TRUE
ggMeanDecreaseAccuracy <- function(model, num = 25) {
  m <- as.data.frame(model$importance)
  m <- cbind("Variable" = as.factor(row.names(m)), m)
  m <-
    head(m[with(m, order(-MeanDecreaseAccuracy, Variable)),], num)
  p <-
    ggplot(data = m, aes(
      x = reorder(Variable, MeanDecreaseAccuracy),
      y = MeanDecreaseAccuracy,
      fill = reorder(Variable, MeanDecreaseAccuracy)
    )) +
    coord_flip() +
    xlab("") +
    guides(fill = FALSE) +
    geom_bar(stat = "identity", width = .90) +
    theme_minimal()
  return(p)
}


# PLOT CONFUSION MATRIX
ggplotConfusionMatrix <- function(x,...){
  UseMethod("ggplotConfusionMatrix")
}

# Confusion matrix for package "randomForest"
ggplotConfusionMatrix.randomForest <- function(m){
  # Calculate confusion matrix and accuracy with caret
  m <- as.table(m$confusion[,-11])
  names(dimnames(m)) <- c("Reference", "Prediction")
  m <- confusionMatrix(m)
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
                   "Kappa", percent_format()(m$overall[2]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

# Confusion matrix for package "caret"
ggplotConfusionMatrix.train <- function(m){
    mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
                     "Kappa", percent_format()(m$overall[2]))
    p <-
        ggplot(data = as.data.frame(m$table) ,
               aes(x = Reference, y = Prediction)) +
        geom_tile(aes(fill = log(Freq)), colour = "white") +
        scale_fill_gradient(low = "white", high = "steelblue") +
        geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
        theme(legend.position = "none") +
        ggtitle(mytitle)
    return(p)
}

# Confusion matrix for package "caret"
ggplotConfusionMatrix.confusionMatrix <- function(m){
  mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
                   "Kappa", percent_format()(m$overall[2]))
  p <-
    ggplot(data = as.data.frame(m$table) ,
           aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low = "white", high = "steelblue") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    theme(legend.position = "none") +
    ggtitle(mytitle)
  return(p)
}

# PLOT MODEL CLASS ERRORS RANDOM FOREST

ggplotModelClassErrorsRF <- function(m) {
  m <- as.data.frame(m$confusion[, "class.error"])
  names(m) <- "class.error"
  m$class.names <- rownames(m)
  p <-
    ggplot(m, aes(x = class.names , y = class.error, fill = class.names)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = round(100 * class.error, 2)),
      vjust = 1.6,
      color = "white",
      size = 3.5
    ) +
    ggtitle("Model Class Errors") +
    guides(fill = guide_legend(title = "")) +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank()
  )
  return(p)
}


# PLOT knn Accuracy vs k
ggplotKnnAccuracy <- function(mymodel){
    myaccuracy <- mymodel$results[2]$Accuracy[mymodel$bestTune[,1]]
    mytitle <- paste("Accuracy", percent(myaccuracy))
    p <-
        ggplot(data = mymodel$results) +
        geom_line(aes(x = mymodel$results[,1], y = mymodel$results[,2])) +
        #geom_line(aes(x = mymodel$results[,1], y = mymodel$results[,3])) +
        geom_point(aes(
            x = mymodel$bestTune[,1],
            y = myaccuracy,
            size = 2,
            colour = "red"
        )) +
        xlab(colnames(mymodel$results[1])) +
        ylab(colnames(mymodel$results[2])) +
        theme(legend.position = "none") +
        ggtitle(mytitle)
    return(p)
}


