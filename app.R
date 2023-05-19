library(shiny)
library(shinydashboard)
library(car)
library(lmtest)
library(GGally)
library(MASS)
library(broom)
library(caret)
library(glmnet)
library(tidyverse) 
library(DT)
library(pROC)


ui <- dashboardPage(dashboardHeader(title = "Regression assumptions"),
                    
                    dashboardSidebar(
                      hr(),
                      sidebarMenu(id = "tabs",
                                  menuItem(text = "Home", tabName = "home"),
                                  inputPanel(fileInput(inputId = "file", label = "File upload:", accept = ".csv")),
                                  selectInput("x_var", "Select X variable", character(0), multiple = T),
                                  selectInput("y_var", "Select Y variable", character(0)),
                                  sliderInput("train", "Choose a % of train data", 0, 1, .8, 0.05),
                                  textInput("seed", "Type a set.seed() number for train/test split", 111),
                                  menuItem(text = "Data description", tabName = "description"),
                                  menuItem(text = "Linear Regression", tabName = "lin"),
                                  menuItem(text = "Logistic Regression", tabName = "log"),
                                  menuItem(text = "Ridge/Lasso Regression", tabName = "glm"),
                                  sliderInput("k", "Choose k cross validation number", 3, 20, 10),
                                  sliderInput("lambda_seq", "Choose a sequence of Lambda values from 1 with a step 0.1", 0, 50, 10),
                                  radioButtons("glm_type", "Choose regression type", choiceNames=c("Ridge", "Lasso"), choiceValues=c(0,1)),
                                  menuItem("Codes",
                                           menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                                           menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))))),
                    
                    dashboardBody(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                      tabItems( 
                        tabItem(tabName = "home",
                                h1("Application description"),
                                h3("This application has been written as a part of Master thesis in Prague Economics University. The application runs a Linear, Logistic or Ridge/Lasso regression model, checking the assumptions of the model.")
                        ),
                        tabItem(tabName = "description",
                                h1("Description of the Data"),
                                tabBox(width = "100%", title = "Head of the data", 
                                       tabPanel("Full dataset", dataTableOutput("data_head")),
                                       tabPanel("Train data", dataTableOutput("data_head_train")),
                                       tabPanel("Test data",dataTableOutput("data_head_test"))),
                                fluidRow(infoBoxOutput("dim_data"),
                                infoBoxOutput("dim_train"),
                                infoBoxOutput("dim_test")),
                                box(DTOutput("summary_table"), status = "warning", 
                                    collapsible = T, footer = "", width = "100%"),
                                fluidRow(
                                  box(plotOutput("hist"), status = "warning", 
                                      collapsible = T, footer = "",  width = "100%"),
                                  box(plotOutput("model_corr_matrix"), status = "warning",
                                      collapsible = T, title="Correlation matrix plot", footer = "",  width = "100%"),
                                  box(plotOutput("pairs"), status = "warning",  width = "100%",
                                      collapsible = T, title="Correlation matrix plot", footer = "Graphs show scatter plots between each pair of variables"))
                        ),
                        tabItem(tabName = "lin",
                                column(width = 6,
                                       box(verbatimTextOutput("regression_summary"), width = 12, status = "warning",
                                           collapsible = T, footer = "Regression model output"),
                                       
                                       box(width = 12, plotOutput("residuals_plot"),  status = "warning",
                                           collapsible = T,  title="Linearity assumption", footer = "If the residuals randomly distributed around the 0 line the assumption that the relationship is linear is reasonable. This suggests that the variances of the error terms are equal and that there are no outliers."),
                                       
                                       tabBox(title = "Normality of residuals", id = "normality", width = 12, side = "left",
                                              tabPanel("Q-Q plot", "Normally distributed residuals should lie on a straight line", plotOutput("qq_plot")),
                                              tabPanel("Shapiro-Wilk normality test", "H0: Residuals follow a Normal distribution", br(),"H1: Residuals don`t follow a Normal distribution", verbatimTextOutput("shapiro"))),
                                       
                                       box(verbatimTextOutput("vif"), width = 12, status = "warning",
                                           collapsible = T, title = "VIF", p("H0: There is no correlation among the residuals."),
                                           footer = "VIF coefficient above 10 reveals the presence of a multicollinearity in the data"),
                                       
                                       tabBox(title = "Influencial values", id ="influentials", width = 12, side = "left",
                                              tabPanel("Cook`s distance plot", "Values above thresdhold blue line (4/n) are considered to be influential.", plotOutput("cooks")),
                                              tabPanel("Cook`s distance plot 2", "Values above dashed red line are considered to be influential.", plotOutput("cooks2")),
                                              tabPanel("Influencial values plot", "Observations to the right of the vertical line are high-leverage points and are potentially influential in fitting the regression model.", plotOutput("influencial")),
                                              tabPanel("DFBETAS", "DFBETAS shows the difference between a regression coefficient from the model with all data points, and the regression coefficient of the model with a deleted point", verbatimTextOutput("DFBETAS"), footer = "If DBETAS value greater than a threshold of 2/√n a point is considered to be influencial."),
                                              tabPanel("DFFITS", "DFFITS shows how much does a fitted value changes, when we remove the i-th observation from the data", verbatimTextOutput("DFFITS"), footer = "If DFFITS value greater than a threshold of 2/√n a point is considered to be influencial."),
                                              footer = "Influential means that deleting that observation can result in a relatively large change in the regression parameter estimates.")),
                                column(width = 6,
                                       box(width = 12, plotOutput("fitted_actual"), status = "warning",
                                           collapsible = T, title="Fitted VS Actual plot", footer = "For a model that fits the data well, the points will be close to the diagonal line."),
                                       
                                       tabBox(title = "Homoskedasticity", id = "homoskedasticity", width = 12,  side = "left",
                                              tabPanel("Breusch-Pagan Test", verbatimTextOutput("BP_test")),
                                              tabPanel("Goldfeld-Quandt test",verbatimTextOutput("GQ_test")),
                                              footer = "H0: Residuals have a constant variance (Homoskedasticity)"),
                                       
                                       box(width = 12, verbatimTextOutput("DW"), status = "warning",
                                           collapsible = T, title="Durbin Watson test", footer = "D-W test statistics should lie within <-2,2>, so we can conclude that the residuals are not autocorrelated"),
                                       
                                       tabBox(title = "Outliers", id = "outliers", width = 12, side = "left",
                                              tabPanel("Externally studentized residuals VS fitted", plotOutput("Outliers1")),
                                              tabPanel("Externally studentized residuals VS Dependent variable", plotOutput("Outliers2")),
                                              tabPanel("residualPlots", plotOutput("residualPlots")),
                                              tabPanel("Tukey test", "H0: Coefficient for quadratic term is 0.", br(), "H1: Coefficient for quadratic term is not 0.", verbatimTextOutput("tukey_test")),
                                              footer = "The observations whose studentized residuals exceed ±2 can be considered outliers."),
                                       
                                       tabBox(title = "Model evaluation", id = "evaluation", width = 12,  side = "left",
                                              tabPanel("Sufficient number of predictors", "output shows the sufficient number of predictors, which are marcked with asterisk (*)", verbatimTextOutput("lin_evaluation")),
                                              tabPanel("Evaluation metrics", "Evaluation metrics  of the model", verbatimTextOutput("lin_evaluation2")),
                                              tabPanel("Evaluation metrics", "", verbatimTextOutput("lin_evaluation3")),
                                              footer = "Choose a k number of folds for cross validation on the menu side"))
                        ),
                        tabItem(tabName = "log",
                                column(width = 6,
                                       tabBox(title = "Model", id = "model", width = 12, side = "left",
                                              tabPanel("Output", verbatimTextOutput("log_regression_summary")),
                                              tabPanel("exp(coefficients)", verbatimTextOutput("log_coef"))),
                                       
                                       box(verbatimTextOutput("log_vif"), width = 12, status = "warning",
                                           collapsible = T, title = "VIF", p("H0: There is no correlation among the residuals."),
                                           footer = "VIF coefficient above 10 reveals the presence of a multicollinearity in the data"),
                                       
                                       box(plotOutput("log_linearity"), width = 12, status = "warning",
                                           collapsible = T, title = "Linearity assumption", footer = "Checking the linear relationship between continuous variables and the logit of the outcome"),
                                       
                                       box(width = 12, plotOutput("roc"), status = "warning",
                                           collapsible = T, title = "ROC curve"),
                                       
                                       tabBox(title = "Influencial values", id ="log_influentials", width = 12, side = "left",
                                              tabPanel("Cook`s distance plot",plotOutput("log_cooks")),
                                              tabPanel("DFBETAS", "DFBETAS shows the difference between a regression coefficient from the model with all data points, and the regression coefficient of the model with a deleted point", verbatimTextOutput("log_DFBETAS"), footer = "If DBETAS value greater than a threshold of 2/√n/p a point is considered to be influencial."),
                                              tabPanel("DFFITS", "DFFITS shows how much does a fitted value changes, when we remove the i-th observation from the data", verbatimTextOutput("log_DFFITS"), footer = "If DFFITS value greater than a threshold of 2/√n/p a point is considered to be influencial."),
                                              footer = "")),
                              column(width = 6,
                                       box(width = 12, verbatimTextOutput("confusion_matrix"), status = "warning",
                                           collapsible = T, title = "Logistic regression evaluation"),
                                     
                                     box(width = 12, verbatimTextOutput("log_tables"), status = "warning",
                                         collapsible = T, title = "Contingency tables between categorical predictor and DV"),
                                     
                                     box(verbatimTextOutput("log_stepAIC"), width = 12, status = "warning",
                                         collapsible = T, title = "Backwards elimination", footer = "Based on a AIC choose the best subset of predictors"))
                                ),
                        tabItem(tabName = "glm",
                                fluidRow(
                                       tabBox(title = "Model", id = "model", width = 6, 
                                              tabPanel("Model", verbatimTextOutput("glm_regression_summary")),
                                              tabPanel("Coefficients", verbatimTextOutput("glm_coefficients")),
                                              footer=""),

                                       tabBox(title = "Lambda", id = "lambda", width = 6, 
                                              tabPanel("Lambda plot", plotOutput("glm_plot")),
                                              tabPanel("Trace plot","We want to find the value where the lines begin to flatten out, where the coefficients are no longer changing", plotOutput("lambda_trace")),
                                              footer=""),
                                       
                                       box(verbatimTextOutput("evaluation"), width = 6, status = "warning",
                                           collapsible = T, footer = "Model evaluation metrics"),
                                       
                                       box(plotOutput("feature_sel"), width = 6, status = "warning",
                                           collapsible = T, footer = "Feature selection plot"))
                        ))
                    )
)

# define server
server <- function(session, input, output) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  observeEvent(data(), {
    if (!is.null(names(data()))) {
      updateSelectInput(session, "x_var", choices = names(data()))
      updateSelectInput(session, "y_var", choices = names(data()))
    }
  })
  
  # Data split for Logistic and Ridge/Lasso regression
  user_data <- reactive({
    req(data())
    data <- data() %>%
      na.omit()
    as.data.frame(cbind(data[, c(input$y_var, input$x_var)]))
  })
  
  train <- reactive({
    set.seed(input$seed)
    i <- sample(1:nrow(user_data()), input$train*nrow(user_data()))
    as.data.frame(user_data()[i, ])
  })  
  
  test <- reactive({
    set.seed(input$seed)
    i <- sample(1:nrow(user_data()), input$train*nrow(user_data()))
    as.data.frame(user_data()[-i, ])
  })
  
  # Data table
  output$data_head <- renderDataTable({
    user_data()
  })
  output$data_head_train <- renderDataTable({
    train()
  })
  output$data_head_test <- renderDataTable({
    test()
  })
  
  # Data dimension 
  output$dim_data <- renderInfoBox({
    infoBox(
      "Dimension of Full dataset", paste("Rows",dim(user_data())[1], "Columns", dim(user_data())[2]), icon = icon("list"),
      color = "orange"
    )
  })
  
  output$dim_train <- renderInfoBox({
    infoBox(
      "Dimension of train", paste("Rows",dim(train())[1], "Columns", dim(train())[2]), icon = icon("list"),
      color = "orange"
    )
  })
  
  output$dim_test <- renderInfoBox({
    infoBox(
      "Dimension of test", paste("Rows",dim(test())[1], "Columns", dim(test())[2]), icon = icon("list"),
      color = "orange"
    )
  })
  
  output$summary_table <- renderDataTable({
     summary(user_data())
  })
  
  output$pairs <- renderPlot({
    ggpairs(user_data())
  })
  
  output$hist <- renderPlot({
    data <- user_data() %>%                     
      pivot_longer(colnames(user_data())) %>% 
      as.data.frame()
    
    ggplot(data, aes(x = value)) +
      geom_histogram(aes(y = ..density..)) + 
      geom_density(col = "#ee712b", size = 2) + 
      facet_wrap(~ name, scales = "free")+
      theme_bw()
  })
  
  output$model_corr_matrix <- renderPlot({
    model_corr_matrix <- cor(user_data(), use = "pairwise.complete.obs")
    corrplot::corrplot(model_corr_matrix)
  })
  
  # Linear regression model
  mod_lin <- reactive({
    if (dim(user_data())[2]>2) {
      lm(user_data()[[1]]~., data = user_data()[-1])
    } else {  
      lm(user_data()[[1]]~user_data()[[2]])
    }
  })
  
  # Logistic regression model
  mod_log <- reactive({
    if (dim(user_data())[2]>2) {
      glm(user_data()[[1]]~., data = user_data()[-1], family = "binomial")
    } else {  
      glm(user_data()[[1]]~user_data()[[2]], family = "binomial")
    }
  })

  # Ridge Lasso regression model
  glm_mod <- reactive({
    if (dim(user_data())[2]>2) {
      set.seed(input$seed)
      cv.glmnet(y = as.matrix(train()[[1]]), x = as.matrix(train()[-1], ncol=ncol(train())-1), alpha = input$glm_type, standardize = TRUE, nfolds = input$k, lambda=seq(0, input$lambda_seq, by=0.1))
    } else {  
      set.seed(input$seed)
      cv.glmnet(y = as.matrix(train()[[1]]), x = as.matrix(train()[[2]]), alpha = input$glm_type, standardize = TRUE, nfolds = input$k, lambda=seq(0, input$lambda_seq, by=0.1))
    }
  })
  
  ############################# Linear Regression ###################################  
  output$regression_summary <- renderPrint({
    summary(mod_lin())
  }) 
  
  output$fitted_actual <- renderPlot({
    ggplot(na.omit(user_data()), aes(y=user_data()[[1]],x=predict(mod_lin())))+
      geom_point(col="#ee712b")+
      theme_light()+
      geom_abline(slope = 1, intercept = 0, lwd=.5)+
      xlab("Predicted")+
      ylab("Observed")+
      labs(title = "Observed vs Fitted")
  })
  
  output$residuals_plot <- renderPlot({
    ggplot(na.omit(user_data()), aes(y=mod_lin()$residuals, x=predict(mod_lin())))+
      geom_point(col="#ee712b")+
      theme_light()+
      geom_abline(slope = 0, lwd=.5)+
      xlab("Predicted")+
      ylab("Residuals")+
      labs(title = "Residuals vs Fitted", subtitle = "If the residuals are spread equally around a horizontal line without distinct pattern, that is a sign of a linear relationship.")
  })
  
  
  output$qq_plot <- renderPlot({
    qqPlot(mod_lin()$residuals, main="Residuals Q-Q plot", ylab="Studentized residuals", xlab="Theoretical quantiles")
  })
  
  
  output$shapiro <- renderPrint({
    ifelse(shapiro.test(mod_lin()$residuals)$p.value>0.05,
           paste0("Assumption of Normally distributed residuals is met on a 5% alpha level, p-value=",shapiro.test(mod_lin()$residuals)$p.value), 
           paste0("Assumption of Normally distributed residuals is NOT met on a 5% alpha level, p-value=", shapiro.test(mod_lin()$residuals)$p.value))
  })
  
  output$BP_test <- renderPrint({
    ifelse(bptest(mod_lin())$p.value>0.05,
           paste0("Assumption of Homogenity is met on a 5% alpha level, p-value=",bptest(mod_lin())$p.value), 
           paste0("Assumption of Homogenity is NOT met on a 5% alpha level, p-value=", bptest(mod_lin())$p.value))
  })
  
  output$GQ_test <- renderPrint({
    ifelse(gqtest(mod_lin())$p.value>0.05,
           paste0("Assumption of Homogenity is met on a 5% alpha level, p-value=",gqtest(mod_lin())$p.value), 
           paste0("Assumption of Homogenity is NOT met on a 5% alpha level, p-value=", gqtest(mod_lin())$p.value))
  })
  
  
  output$vif <- renderPrint({
    if(dim(user_data())[2]>2) {
      vif_vals <- round(vif(lm(user_data()[[1]]~., data = user_data()[-1])),)
      print(vif_vals)
    } else {
      print("There is only one predictor in the data")
    }
  })
  
  output$DW <- renderPrint({
    car::durbinWatsonTest(mod_lin(), max.lag=10)
  })
  
  output$Outliers1 <- renderPlot({
    par(mar = par("mar") + c(0, .3, 0, 0))
    plot(fitted(mod_lin()), rstudent(mod_lin()), ylab="Studentized Residuals", xlab="Fitted")
    abline(h = c(2,-2), col="red", lty=2)
  })
  
  output$Outliers2 <- renderPlot({
    par(mar = par("mar") + c(0, .3, 0, 0))
    plot(na.omit(user_data())[[1]], rstudent(mod_lin()), xlab=input$y_var, ylab="Studentized Residuals")
    abline(h = c(2,-2), col="red", lty=2)
  })
  
  output$residualPlots <- renderPlot({
    residualPlots(mod_lin(), tests = TRUE, type = "rstudent")
  })
  
  output$tukey_test <- renderPrint({
    residualPlots(mod_lin(), tests = TRUE, type = "rstudent")
  })
  
  output$cooks <- renderPlot({
    plot(mod_lin(),4)
    abline(h=4/nrow(user_data()), lwd=3, col="blue")
  })
  
  output$cooks2 <- renderPlot({
    plot(mod_lin(),5)
  })
  
  output$influencial <- renderPlot({
    influencePlot(mod_lin(),4)
  })
  
  output$DFBETAS <- renderPrint({
    dfbetas <- as.data.frame(dfbetas(mod_lin()))
    paste("There are", sum(dfbetas>2/sqrt(nrow(user_data())) | dfbetas<(-2/sqrt(nrow(user_data())))), "influencial values.")
  })
  
  output$DFFITS <- renderPrint({
    dffits <- as.data.frame(dffits(mod_lin()))
    paste("There are", sum(dffits>2/sqrt(nrow(user_data())) | dffits<(-2/sqrt(nrow(user_data())))), "influencial values.")
  })
  
  output$lin_evaluation <- renderPrint({
    ctrl <- trainControl(method = "cv", number = input$k)
    set.seed(input$seed)
    step.model <- caret::train(x = as.data.frame(user_data()[-1]),  y=user_data()[[1]], method = "leapBackward",
                               tuneGrid = data.frame(nvmax = 1:ncol(user_data())), trControl = ctrl)
    step.model$results
    paste("Best number of predictors is", step.model$bestTune[[1]])
    summary(step.model$finalModel)
  })
  
  output$lin_evaluation2 <- renderPrint({
    ctrl <- trainControl(method = "cv", number = input$k)
    set.seed(input$seed)
    step.model <- caret::train(x = as.data.frame(user_data()[-1]),  y=user_data()[[1]], method = "leapBackward",
                               tuneGrid = data.frame(nvmax = 1:ncol(user_data())), trControl = ctrl)
    step.model$results
  })
  
  output$lin_evaluation3 <- renderPrint({
    lin_model <- if (dim(train())[2]>2) {
      lm(train()[[1]]~., data = train()[-1])
    } else {  
      lm(train()[[1]]~train()[[2]])
    }
    pred <- predict(lin_model, as.data.frame(test()[-1]))
    data.frame(
      RMSE = RMSE(pred, test()[[1]]),
      R_2 = R2(pred, test()[[1]]),
      MAE = MAE(pred, test()[[1]]))
  })
  
  ############### Log regression #######################
  
  
  mod_log_train <- reactive({
    if (dim(user_data())[2]>2) {
      glm(train()[[1]]~., data = train()[-1], family = "binomial")
    } else {  
      glm(train()[[1]]~train()[[2]], family = "binomial")
    }
  })
  
  output$log_regression_summary <- renderPrint({
    summary(mod_log())
  }) 
  
  output$log_coef <- renderPrint({
    exp(mod_log()$coefficients)
  }) 
  
  output$log_tables <- renderPrint({
    l <- lapply(user_data(), function(z)
      if((sum(unique(z))<10)==T) {
        print(table(z, user_data()[[1]]))
      })
    
    l[lapply(l, is.table)==T]
  })
  
  output$confusion_matrix <- renderPrint({
    pred <- predict(mod_log_train(), test(), type = "response")
    pred <- as.factor(ifelse(pred > 0.5, 1, 0))
    confusionMatrix(reference=as.factor(test()[[1]]), data=pred)
  })
  
  output$log_vif <- renderPrint({
    if(dim(user_data())[2]>2) {
      vif_vals <- round(vif(lm(user_data()[[1]]~., data = user_data()[-1])),3)
      print(vif_vals)
    } else {
      print("There is only one predictor in the data")
    }
  })
  
  output$log_linearity <- renderPlot({
    probabilities <- predict(mod_log(), type = "response")
    predictors <- colnames(user_data())

    mydata <- user_data() %>%
      mutate(logit = log(probabilities/(1-probabilities))) %>%
      gather(key = "predictors", value = "predictor.value", -logit)
    
    ggplot(mydata, aes(logit, predictor.value))+
      geom_point(size = 0.5, alpha = 0.5) +
      geom_smooth(method = "loess") + 
      theme_bw() + 
      facet_wrap(~predictors, scales = "free_y")
  })
  
  output$roc <- renderPlot({
    pred <- predict(mod_log_train(), test(), type = "response")
    roc <- roc(test()[[1]], pred)
    plot(roc, main = "ROC Curve", print.auc = TRUE)
  })
  
  output$log_stepAIC <- renderPrint({
    stepAIC(mod_log())
  })
  
  output$log_cooks <- renderPlot({
    plot(mod_log(),4)
    abline(h=4/nrow(train()), lwd=3, col="blue")
  })
  
  
  output$log_cooks2 <- renderPlot({
    plot(mod_log(),5)
  })

  output$log_DFBETAS <- renderPrint({
    dfbetas <- as.data.frame(dfbetas(mod_log()))
    paste("There are", sum(dfbetas>2/sqrt(nrow(user_data())) | dfbetas<(-2/sqrt(nrow(user_data())))), "influencial values.")
  })
  
  output$log_DFFITS <- renderPrint({
    dffits <- as.data.frame(dffits(mod_log()))
    paste("There are", sum(dffits>2/sqrt(nrow(user_data())) | dffits<(-2/sqrt(nrow(user_data())))), "influencial values.")
  })
  
  
  
  ####################### Ridge and Lasso regression ##################################
  
  
  output$glm_regression_summary <- renderPrint({
    print(glm_mod())
  })
  
  output$glm_plot <- renderPlot({
    plot(glm_mod())
  })
  
  output$glm_coefficients <- renderPrint({
    best_mod <- glmnet(y = as.matrix(train()[[1]]), x = as.matrix(train()[-1], ncol=ncol(train()[-1])), alpha = input$glm_type, standardize = TRUE, lambda = glm_mod()$lambda.1se)
    coef(best_mod)
  })
  
  output$evaluation <- renderPrint({
    set.seed(input$seed)
    best_mod <- glmnet(y = as.matrix(train()[[1]]), x = as.matrix(train()[-1], ncol=ncol(train()[-1])), alpha = input$glm_type, standardize = TRUE, lambda = glm_mod()$lambda.1se)
    pred <- predict(best_mod, as.matrix(test()[-1], ncol=ncol(test()[-1])), s = "lambda.1se")
    data.frame(
      RMSE = RMSE(pred, test()[[1]]),
      R_2 = R2(pred, test()[[1]]),
      MAE = MAE(pred, test()[[1]]))
  })
  
  output$lambda_trace <- renderPlot({
    best_mod <- glmnet(y = train()[[1]], x = as.matrix(train()[-1], ncol=ncol(train()[-1])), alpha = input$glm_type, standardize = TRUE)
    plot(best_mod, xvar = "lambda")
  })
  
  output$feature_sel <- renderPlot({ glm_mod
    model <- cv.glmnet(as.matrix(train()[-1], ncol=ncol(train())), as.matrix(train()[[1]]), alpha=input$glm_type, lambda = seq(0, input$lambda_seq, by=0.1))
    coef <- model$glmnet.fit$beta[, model$glmnet.fit$lambda == model$lambda.1se]
    
    coef_l = data.table::data.table(lasso = coef) 
    coef_l[, feature := names(coef)]       
    table = data.table::melt(coef_l                      
                                 , id.vars='feature'
                                 , variable.name = 'model'
                                 , value.name = 'coefficient')
    ggplot(data=table,                       
           aes(x=feature, y=coefficient)) +
      coord_flip() +         
      geom_bar(stat='identity', fill="orange", color='#ee712b') +
      facet_wrap(~ model) + guides(fill=FALSE) +
      theme_bw()
  })
}

shinyApp(ui = ui, server = server)