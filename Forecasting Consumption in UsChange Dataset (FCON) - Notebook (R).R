# FCON - Data Loading & Integration

options(warn = -1)
suppressPackageStartupMessages({
  library(ggplot2)
  library(magick)
  library(forecast)
  library(fpp2)
  library(GGally)
  library(gridExtra)})

uschange
uschangeTraining <- window(uschange, end = c(2014,4))
uschangeTest <- window(uschange, start = c(2015, 1), end = c(2016, 3))
uschangeTraining
uschangeTest



# FCON - Data Preprocessing & Exploratory Data Analysis

start(uschangeTraining)
end(uschangeTraining)
cycle(uschangeTraining)
summary(uschangeTraining)

consumption <- c(-2.2741, 0.4159, 0.7888, 0.7493, 1.1083, 2.3183)
income      <- c(-4.2652, 0.2833, 0.7232, 0.7185, 1.1727, 4.5365)
production  <- c(-6.8510, 0.1429, 0.6979, 0.5377, 1.3420, 4.1496)
savings     <- c(-68.788, -4.820, 1.133, 1.215, 7.065, 50.758)
unemployment<- c(-0.90000, -0.20000, 0.00000, 0.01167, 0.10000, 1.40000)
iqrtraining <- c(
  consumption[5] - consumption[2],
  income[5]      - income[2],
  production[5]  - production[2],
  savings[5]     - savings[2],
  unemployment[5]- unemployment[2])
print(iqrtraining)


## Data Preprocessing & Exploratory Data Analysis - Pearson Correlation Coefficient

cor(uschangeTraining)

options(repr.plot.width = 12, repr.plot.height = 10)
library(GGally)
GGally::ggpairs(
  as.data.frame(uschangeTraining),
  title = "Pairwise Correlation Matrix for Training Dataset",
  progress = FALSE
) + theme_minimal(base_family = "Garamond") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text  = element_text(size = 12, family = "Garamond"))


## Data Preprocessing & Exploratory Data Analysis - Autocorrelation

options(repr.plot.width = 10, repr.plot.height = 5)
ggAcf(uschangeTraining[, "Consumption"]) +
  ggtitle("ACF Correlogram for Consumption") +
  theme_minimal(base_family = "Garamond") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text  = element_text(size = 12, family = "Garamond"))

options(repr.plot.width = 10, repr.plot.height = 5)
ggAcf(uschangeTraining[, "Income"]) +
  ggtitle("ACF Correlogram for Income") +
  theme_minimal(base_family = "Garamond") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text  = element_text(size = 12, family = "Garamond"))

options(repr.plot.width = 10, repr.plot.height = 5)
ggAcf(uschangeTraining[, "Unemployment"]) +
  ggtitle("ACF Correlogram for Unemployment") +
  theme_minimal(base_family = "Garamond") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text  = element_text(size = 12, family = "Garamond"))

options(repr.plot.width = 10, repr.plot.height = 5)
ggAcf(uschangeTraining[, "Production"]) +
  ggtitle("ACF Correlogram for Production") +
  theme_minimal(base_family = "Garamond") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text  = element_text(size = 12, family = "Garamond"))

options(repr.plot.width = 10, repr.plot.height = 5)
ggAcf(uschangeTraining[, "Savings"]) +
  ggtitle("ACF Correlogram for Savings") +
  theme_minimal(base_family = "Garamond") +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text  = element_text(size = 12, family = "Garamond"))

options(repr.plot.width = 12, repr.plot.height = 10)
ggAcf(uschangeTraining) +
  ggtitle("ACF Matrix for Training Dataset") + 
  theme_minimal(base_family = "Garamond") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text  = element_text(size = 12, family = "Garamond"))


## Data Preprocessing & Exploratory Data Analysis - Trend & Seasonality

uschangedata <- data.frame(
  Time = time(uschangeTraining),
  Consumption   = uschangeTraining[, "Consumption"],
  Income        = uschangeTraining[, "Income"],
  Production    = uschangeTraining[, "Production"],
  Savings       = uschangeTraining[, "Savings"],
  Unemployment  = uschangeTraining[, "Unemployment"])

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings(
  print(
    ggplot(uschangedata, aes(x = Time, y = Consumption)) +
      geom_line(color = "blue") +
      labs(
        title = "Consumption (1970 Q1 - 2014 Q4)",
        x = "Time (Year)",
        y = "Percentage Change in Personal Consumption Expenditure"
      ) +
      theme_minimal(base_family = "Garamond") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text  = element_text(size = 12, family = "Garamond")))))

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings(
  print(
    ggplot(uschangedata, aes(x = Time, y = Income)) +
      geom_line(color = "darkgreen") +
      labs(
        title = "Income (1970 Q1 - 2014 Q4)",
        x = "Time (Year)",
        y = "Percentage Change in Personal Income"
      ) +
      theme_minimal(base_family = "Garamond") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text  = element_text(size = 12, family = "Garamond")))))

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings(
  print(
    ggplot(uschangedata, aes(x = Time, y = Production)) +
      geom_line(color = "orange") +
      labs(
        title = "Production (1970 Q1 - 2014 Q4)",
        x = "Time (Year)",
        y = "Percentage Change in Industrial Production"
      ) +
      theme_minimal(base_family = "Garamond") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text  = element_text(size = 12, family = "Garamond")))))

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings(
  print(
    ggplot(uschangedata, aes(x = Time, y = Savings)) +
      geom_line(color = "purple") +
      labs(
        title = "Savings (1970 Q1 - 2014 Q4)",
        x = "Time (Year)",
        y = "Percentage Change in Personal Savings"
      ) +
      theme_minimal(base_family = "Garamond") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text  = element_text(size = 12, family = "Garamond")))))

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings(
  print(
    ggplot(uschangedata, aes(x = Time, y = Unemployment)) +
      geom_line(color = "darkred") +
      labs(
        title = "Unemployment (1970 Q1 - 2014 Q4)",
        x = "Time (Year)",
        y = "Percentage Change in Unemployment Rate"
      ) +
      theme_minimal(base_family = "Garamond") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text  = element_text(size = 12, family = "Garamond")))))

decompose(uschangeTraining)$seasonal[2]
decompose(uschangeTraining[, "Consumption"])$seasonal[2]
decompose(uschangeTraining[, "Income"])$seasonal[2]
decompose(uschangeTraining[, "Production"])$seasonal[2]
decompose(uschangeTraining[, "Savings"])$seasonal[2]
decompose(uschangeTraining[, "Unemployment"])$seasonal[2]

decompose(uschangeTraining)$seasonal[3]
decompose(uschangeTraining[, "Consumption"])$seasonal[3]
decompose(uschangeTraining[, "Income"])$seasonal[3]
decompose(uschangeTraining[, "Production"])$seasonal[3]
decompose(uschangeTraining[, "Savings"])$seasonal[3]
decompose(uschangeTraining[, "Unemployment"])$seasonal[3]

decompose(uschangeTraining)$seasonal[4]
decompose(uschangeTraining[, "Consumption"])$seasonal[4]
decompose(uschangeTraining[, "Income"])$seasonal[4]
decompose(uschangeTraining[, "Production"])$seasonal[4]
decompose(uschangeTraining[, "Savings"])$seasonal[4]
decompose(uschangeTraining[, "Unemployment"])$seasonal[4]

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  decomposedsavings <- decompose(uschangeTraining[, "Savings"])
  op <- par(family = "Garamond")
  plot(decomposedsavings, ann = FALSE)  
  title(main = "Decomposition of Savings (1970 Q1 - 2014 Q4)",
        cex.main = 1.4, font.main = 2, family = "Garamond")
  title(xlab = "Time", cex.lab = 1.1, family = "Garamond")
  par(op)}))

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  decomposedincome <- decompose(uschangeTraining[, "Income"])
  op <- par(family = "Garamond")
  plot(decomposedincome, ann = FALSE)
  title(main = "Decomposition of Income (1970 Q1 - 2014 Q4)",
        cex.main = 1.4, font.main = 2, family = "Garamond")
  title(xlab = "Time", cex.lab = 1.1, family = "Garamond")
  par(op)}))

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  decomposedproduction <- decompose(uschangeTraining[, "Production"])
  op <- par(family = "Garamond")
  plot(decomposedproduction, ann = FALSE)
  title(main = "Decomposition of Production (1970 Q1 - 2014 Q4)",
        cex.main = 1.4, font.main = 2, family = "Garamond")
  title(xlab = "Time", cex.lab = 1.1, family = "Garamond")
  par(op)}))

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  decomposedunemployment <- decompose(uschangeTraining[, "Unemployment"])
  op <- par(family = "Garamond")
  plot(decomposedunemployment, ann = FALSE)
  title(main = "Decomposition of Unemployment (1970 Q1 - 2014 Q4)",
        cex.main = 1.4, font.main = 2, family = "Garamond")
  title(xlab = "Time", cex.lab = 1.1, family = "Garamond")
  par(op)}))

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  decomposedconsumption <- decompose(uschangeTraining[, "Consumption"])
  op <- par(family = "Garamond")
  plot(decomposedconsumption, ann = FALSE)
  title(main = "Decomposition of Consumption (1970 Q1 - 2014 Q4)",
        cex.main = 1.4, font.main = 2, family = "Garamond")
  title(xlab = "Time", cex.lab = 1.1, family = "Garamond")
  par(op)}))


## Data Preprocessing & Exploratory Data Analysis - Stationarity

library(tseries)
adf.test(na.omit(decomposedsavings$random))
adf.test(na.omit(decomposedconsumption$random))
adf.test(na.omit(decomposedincome$random))
adf.test(na.omit(decomposedproduction$random))
adf.test(na.omit(decomposedunemployment$random))

library(tseries)
adf.test(uschangeTraining[,"Consumption"])
adf.test(uschangeTraining[,"Savings"])
adf.test(uschangeTraining[,"Income"])
adf.test(uschangeTraining[,"Production"])
adf.test(uschangeTraining[,"Unemployment"])

library(tseries)
kpss.test(decomposedsavings$random)
kpss.test(decomposedconsumption$random)
kpss.test(decomposedincome$random)
kpss.test(decomposedproduction$random)
kpss.test(decomposedunemployment$random)

library(tseries)
kpss.test(uschangeTraining[,"Consumption"])
kpss.test(uschangeTraining[,"Savings"])
kpss.test(uschangeTraining[,"Income"])
kpss.test(uschangeTraining[,"Production"])
kpss.test(uschangeTraining[,"Unemployment"])



# FCON - Statistical & Time Series Modelling


## Statistical & Time Series Modelling - FCON Model 1

fconmodel1 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[, 2:5])
summary(fconmodel1)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel1res <- residuals(fconmodel1)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))  
  forecast::tsdisplay(fconmodel1res, main = "")        
  mtext("Residual Diagnostics for FCON Model 1", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel1res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel1res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel1res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel1res, fitdf = 5, lag = 60, type = "Lj")


## Statistical & Time Series Modelling - FCON Model 2

fconmodel2 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,2])
summary(fconmodel2)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel2res <- residuals(fconmodel2)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel2res, main = "")
  mtext("Residual Diagnostics for FCON Model 2", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel2res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel2res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel2res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel2res, fitdf = 5, lag = 60, type = "Lj")


## Statistical & Time Series Modelling - FCON Model 3

fconmodel3 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,3])
summary(fconmodel3)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel3res <- residuals(fconmodel3)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel3res, main = "")
  mtext("Residual Diagnostics for FCON Model 3", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel3res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel3res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel3res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel3res, fitdf = 5, lag = 60, type = "Lj")


## Statistical & Time Series Modelling - FCON Model 4

fconmodel4 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,4])
summary(fconmodel4)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel4res <- residuals(fconmodel4)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel4res, main = "")
  mtext("Residual Diagnostics for FCON Model 4", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel4res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel4res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel4res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel4res, fitdf = 5, lag = 60, type = "Lj")


## Statistical & Time Series Modelling - FCON Model 5

fconmodel5 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,5])
summary(fconmodel5)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel5res <- residuals(fconmodel5)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel5res, main = "")
  mtext("Residual Diagnostics for FCON Model 5", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel5res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel5res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel5res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel5res, fitdf = 5, lag = 60, type = "Lj")


## Statistical & Time Series Modelling - FCON Model 6

fconmodel6 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,2:3])
summary(fconmodel6)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel6res <- residuals(fconmodel6)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel6res, main = "")
  mtext("Residual Diagnostics for FCON Model 6", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel6res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel6res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel6res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel6res, fitdf = 5, lag = 60, type = "Lj")


## Statistical & Time Series Modelling - FCON Model 7

fconmodel7 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,2:4])
summary(fconmodel7)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel7res <- residuals(fconmodel7)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel7res, main = "")
  mtext("Residual Diagnostics for FCON Model 7", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel7res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel7res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel7res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel7res, fitdf = 5, lag = 60, type = "Lj")


## Statistical & Time Series Modelling - FCON Model 8

fconmodel8 <- auto.arima(uschangeTraining[,1], 
                         xreg = uschangeTraining[,2, drop = FALSE] + 
                         uschangeTraining[,5, drop = FALSE])
summary(fconmodel8)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel8res <- residuals(fconmodel8)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel8res, main = "")
  mtext("Residual Diagnostics for FCON Model 8", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel8res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel8res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel8res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel8res, fitdf = 5, lag = 60, type = "Lj")


## Statistical & Time Series Modelling - FCON Model 9

fconmodel9 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,3:4])
summary(fconmodel9)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel9res <- residuals(fconmodel9)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel9res, main = "")
  mtext("Residual Diagnostics for FCON Model 9", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel9res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel9res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel9res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel9res, fitdf = 5, lag = 60, type = "Lj")


## Statistical & Time Series Modelling - FCON Model 10

fconmodel10 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,3:5])
summary(fconmodel10)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel10res <- residuals(fconmodel10)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel10res, main = "")
  mtext("Residual Diagnostics for FCON Model 10", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel10res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel10res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel10res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel10res, fitdf = 5, lag = 60, type = "Lj")


## Statistical & Time Series Modelling - FCON Model 11

fconmodel11 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,c(2,5)])
summary(fconmodel11)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel11res <- residuals(fconmodel11)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel11res, main = "")
  mtext("Residual Diagnostics for FCON Model 11", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel11res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel11res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel11res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel11res, fitdf = 5, lag = 60, type = "Lj")

suppressMessages(suppressWarnings(
  print(
    ggplot(data.frame(fconmodel11res), aes(x = fconmodel11res)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
      geom_density(color = "red") +
      ggtitle("Histogram of Residuals for FCON Model 11") +
      labs(x = "Residuals", y = "Density") +
      theme_minimal(base_family = "Garamond") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text  = element_text(size = 12, family = "Garamond")))))

suppressMessages(suppressWarnings(
  print(
    ggplot(data.frame(sample = fconmodel11res), aes(sample = sample)) +
      stat_qq() +
      stat_qq_line() +
      labs(
        title = "Q–Q Plot of Residuals for FCON Model 11",
        x = "Theoretical Quantiles (Normal)",
        y = "Sample Quantiles (Residuals)"
      ) +
      theme_minimal(base_family = "Garamond") +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 13, face = "bold"),
        axis.text  = element_text(size = 12, family = "Garamond")))))

shapiro.test(residuals(fconmodel11))


## Statistical & Time Series Modelling - FCON Model 12

fconmodel12 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,c(3,5)])
summary(fconmodel12)

options(repr.plot.width = 12, repr.plot.height = 5)
suppressMessages(suppressWarnings({
  fconmodel12res <- residuals(fconmodel12)
  op <- par(family = "Garamond", oma = c(0, 0, 2, 0))
  forecast::tsdisplay(fconmodel12res, main = "")
  mtext("Residual Diagnostics for FCON Model 12", side = 3, line = 0.5,
        cex = 1.4, font = 2, family = "Garamond", outer = TRUE)
  par(op)}))

Box.test(fconmodel12res, fitdf = 5, lag = 12, type = "Lj")
Box.test(fconmodel12res, fitdf = 5, lag = 24, type = "Lj")
Box.test(fconmodel12res, fitdf = 5, lag = 48, type = "Lj")
Box.test(fconmodel12res, fitdf = 5, lag = 60, type = "Lj")



# FCON - Forecasting


## Forecasting - Decomposition & FCON Model 11

fconmodel11 <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,c(2,5)])
summary(fconmodel11)

fconmodel11s <- auto.arima(uschangeTraining[,1], xreg = uschangeTraining[,c(2,5)], 
                           d = 1, D = 1, seasonal = TRUE)
summary(fconmodel11s)

decompose(uschangeTraining[, "Income"])$seasonal[1]
decompose(uschangeTraining[, "Income"])$seasonal[2]
decompose(uschangeTraining[, "Income"])$seasonal[3]
decompose(uschangeTraining[, "Income"])$seasonal[4]

decompose(uschangeTraining[, "Unemployment"])$seasonal[1]
decompose(uschangeTraining[, "Unemployment"])$seasonal[2]
decompose(uschangeTraining[, "Unemployment"])$seasonal[3]
decompose(uschangeTraining[, "Unemployment"])$seasonal[4]

incomedec <- decompose(uschangeTraining[, "Income"])
seasonalincome <- incomedec$seasonal
tapply(seasonalincome, cycle(seasonalincome), mean)

unemploymentdec <- decompose(uschangeTraining[, "Unemployment"])
seasonalunemployment <- unemploymentdec$seasonal
tapply(seasonalunemployment, cycle(seasonalunemployment), mean)


## Forecasting - Holt-Winters’ Exponential Smoothing (Additive Seasonality)

forecastincomehw <- hw(uschangeTraining[,"Income"], h = 7, 
                       seasonal = "additive")
forecastunemploymenthw <- hw(uschangeTraining[,"Unemployment"], h = 7, 
                             seasonal = "additive")

options(repr.plot.width = 13, repr.plot.height = 6)
autoplot(forecastincomehw) +
  ggtitle("Actual vs Forecasted Income (1970 Q1 – 2016 Q3)") +
  xlab("Time (Year)") +
  ylab("Percentage Change in Personal Income") +
  theme_minimal(base_family = "Garamond") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12, family = "Garamond"))

options(repr.plot.width = 13, repr.plot.height = 6)
autoplot(forecastunemploymenthw) +
  ggtitle("Actual vs Forecasted Unemployment (1970 Q1 – 2016 Q3)") +
  xlab("Time (Year)") +
  ylab("Percentage Change in Unemployment Rate") +
  theme_minimal(base_family = "Garamond") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12, family = "Garamond"))

accuracy(forecastunemploymenthw)
accuracy(forecastincomehw)
forecastunemploymenthw$mean
forecastincomehw$mean


## Forecasting - Forecasting Consumption

newxreg <- cbind(income = as.numeric(forecastincomehw$mean), 
                 unemployment = as.numeric(forecastunemploymenthw$mean))
print(newxreg)
colnames(newxreg) <- c("Income", "Unemployment")
print(newxreg)

forecastconsumption <- forecast(fconmodel11, xreg = newxreg, h = 7)
print(forecastconsumption)

options(repr.plot.width = 13, repr.plot.height = 6)
autoplot(uschange[,"Consumption"], series = "Actual") +
  autolayer(forecastconsumption, series = "Forecast", PI = TRUE) +
  ggtitle("Actual vs Forecasted Consumption (1970 Q1 - 2016 Q3)") +
  xlab("Time (Year)") + ylab("Percentage Change in Personal Consumption Expenditure") +
  guides(colour = guide_legend(title = "Series")) +
  scale_colour_manual(values = c("black", "lightblue")) + 
  theme_minimal(base_family = "Garamond") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text  = element_text(size = 12, family = "Garamond"))

accuracy(forecastconsumption, c(0.5962400, 0.7081439, 0.6649696, 0.5616798, 
                                0.4046822, 1.0477074, 0.7295978))