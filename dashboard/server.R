shinyServer(function(input, output) {
    
    library(shiny)
    library(data.table)
    library(shinydashboard)
    library(ggplot2)
    library(DT)
    library(corrplot)
    library(shinyr)
    library(shinydashboard)
    library(shiny)
    library(dplyr)
    library(plotly)
    library(htmlwidgets)
    library(shinyWidgets)
    library(corrplot)
    library(wordcloud)
    library(DT)
    library(GGally)
    library(e1071)
    library(factoextra)
    library(FactoMineR)
    library(wordcloud)
    library(data.table)
    library(nnet)
    library(randomForest)
    library(caret)
    library(DMwR)
    library(RColorBrewer)
    
    dt <- fread('https://raw.githubusercontent.com/ernestang98/Customer-Churn/main/data.csv')
    total = dim(dt)[1]
    exit = dt[Exited==1]
    totalExit = dim(exit)[1]
    noExit = dt[Exited==0]
    totalNoExit = dim(noExit)[1]
    percentExit = totalExit/total
    perecentNoExit = totalNoExit/total
    dt1 = dt[, status:=ifelse (Exited == '1', "Churned", "Retained")]
    dt1$status <- factor(dt1$status, levels=c('Churned', 'Retained'))
    
    output$plot1 <- renderPlot({
        plot1 <- ggplot(dt, aes(x = dt1$status, fill = dt1$status)) +
            geom_bar(position = "dodge", fill = c("#052547", "#2273c9"), color = c("#052547", "#2273c9")) +
            coord_flip() +
            geom_text(aes(label = paste(round(..count../nrow(dt)*100,2), "%")),
                      stat = "count",
                      position = 'dodge',
                      hjust = -0.5,
                      size = 3,
                      inherit.aes = TRUE) +
            theme_minimal() +
            theme(axis.title=element_blank())
        plot1
    })
    
    output$Box1 <- renderInfoBox({
        x = format(total, big.mark = ",")
        infoBox("Total number of customers", x, color = "teal", icon = icon("users"))
    })
    output$Box2 <- renderInfoBox({
        x = format(totalNoExit, big.mark = ",")
        infoBox("Customers retained", x, color = "blue", icon = icon("user-check"))
    })
    output$Box3 <- renderInfoBox({
        x = format(totalExit, big.mark = ",")
        infoBox("Customer churned", x, color = "navy", icon = icon("door-open"))
    })
    output$Box4 <- renderInfoBox({
        x = format(round(total/total*100, 2))
        infoBox("Percent of Customers", paste(x, "%"), color = "teal", icon = icon("percentage"), fill = TRUE)
    })
    output$Box5 <- renderInfoBox({
        x = format(round(totalNoExit/total*100, 2))
        infoBox("Percent of Customers", paste(x, "%"), color = "blue", icon = icon("percentage"), fill = TRUE)
    })
    output$Box6<- renderInfoBox({
        x = format(round(totalExit/total*100, 2))
        infoBox("Percent of Customers", paste(x, "%"), color = "navy", icon = icon("percentage"), fill = TRUE)
    })
    
    my_data <- fread('https://raw.githubusercontent.com/ernestang98/Customer-Churn/main/data.csv')
    output$about_data <- renderUI({
            fluidRow(
                valueBox(nrow(my_data[,8:29]),
                         width = 6,
                         "Total number of rows",
                         icon = icon("grip-horizontal")),
                valueBox(ncol(my_data[,8:29]), 
                         width = 6,
                         "Total number of columns",
                         icon = icon("columns")),
            )
        # )
    })
    
    output$filtered_data <- DT::renderDataTable({
        DT::datatable(my_data[,8:29],
                      class = 'cell-border stripe',
                      selection=list(mode="multiple", target="row"),
                      rownames=FALSE,
                      options = list(scrollX = TRUE,
                                     autoWidth = FALSE)
        )
    })
    
    output$xaxis_col_1d <- renderUI({
        dat <- my_data[,11:29]
        x <- do.call("rbind", lapply(names(dat), FUN = function(x){c(x, class(dat[[x]]))})) %>% as.data.frame()
        names(x) <- c("name", "type")
        x <- x[x$type %in% c("integer", "numeric", "factor", "character"), ]$name %>% as.vector()
        selectInput(inputId = "xaxis_col_1d", label = "Select column/s", multiple = TRUE, choices = x, selected = x[1])
    })
    
    
    output$width_of_tables <- renderUI({
        radioButtons("plot_size", "Plot Size", choices = c("Medium", "Large"), selected = "Medium", inline = TRUE)
    })
    
    
    dt <- fread('https://raw.githubusercontent.com/ernestang98/Customer-Churn/main/data.csv')
    dt$HasCrCard<- factor(dt$HasCrCard)
    dt$IsActiveMember<- factor(dt$IsActiveMember)
    dt$Exited<- factor(dt$Exited)
    dt$PersonalAdvisor<- factor(dt$PersonalAdvisor)
    dt$FinancialLiteracy<- factor(dt$FinancialLiteracy)
    dt$UnresolvedComplaint<- factor(dt$UnresolvedComplaint)
    output$single_dimension_plot_box <- renderPlotly({
        dat <- dt[,11:29]
        col_pos_x <- which(names(dat) %in% input$xaxis_col_1d)
        p <- plotly::plot_ly()
            for(j in c(1:length(col_pos_x))) {
                
                p <- plotly::add_boxplot(p, data = dat, type = "box",
                                         y=dat[[col_pos_x[j]]], name = paste0(input$xaxis_col_1d[j]))
            }
            p   
    })
    
    output$single_dimension_plot_histogram <- renderPlotly({
        dat <- dt[,11:29]
        col_pos_x <- which(names(dat) %in% input$xaxis_col_1d)
        p <- plotly::plot_ly()
        for(j in c(1:length(col_pos_x))) {
            
            p <- plotly::add_histogram(p, data = dat, type = "histogram",
                                       x=dat[[col_pos_x[j]]], name = paste0(input$xaxis_col_1d[j]))
        }
        p   
    })
    
    output$single_dimension_plot_scatter <- renderPlotly({
        dat <- dt[,11:29]
        col_pos_x <- which(names(dat) %in% input$xaxis_col_1d)
        p <- plotly::plot_ly()
        for(j in c(1:length(col_pos_x))) {
            
            p <- plotly::add_trace(p, data = dat, type = "scatter",
                                   y=dat[[col_pos_x[j]]], name = paste0(input$xaxis_col_1d[j]))
        }
        p   
    })
    
    output$single_dimension_plot_line <- renderPlotly({
        dat <- dt[,11:29]
        col_pos_x <- which(names(dat) %in% input$xaxis_col_1d)
        f <- list(
            family = "Courier New, monospace",
            size = 18,
            color = "black"
        )
        x <- list(
            title = "Index",
            titlefont = f
        )
        y <- list(
            title = paste0(input$xaxis_col_1d),
            titlefont = f
        )
        p <- plotly::plot_ly()
        for(j in 1:length(col_pos_x)) {
            p <- plotly::add_trace(p, data = dat, y=dat[[col_pos_x[j]]],
                                   type = "scatter", mode = "line",
                                   name = paste0(input$xaxis_col_1d[j]))
        }
        p <- p %>% plotly::layout(xaxis = x, yaxis = y)
        p
    })
    
    output$single_dimention_plot_ui <- renderUI({
        if(input$plot_size == "Medium") {
            fluidPage(
                box(width = 6, plotlyOutput("single_dimension_plot_box"), title = "Box plot", collapsible = TRUE),
                box(width = 6, plotlyOutput("single_dimension_plot_histogram"), title = "Histogram", collapsible = TRUE),
                box(width = 6, plotlyOutput("single_dimension_plot_scatter"), title = "Scatter plot", collapsible = TRUE),
                box(width = 6, plotlyOutput("single_dimension_plot_line"), title = "Line plot", collapsible = TRUE),
            )
        } else {
            fluidPage(
                box(width = 12, plotlyOutput("single_dimension_plot_box"), title = "Box plot", collapsible = TRUE),
                box(width = 12, plotlyOutput("single_dimension_plot_histogram"), title = "Histogram", collapsible = TRUE),
                box(width = 12, plotlyOutput("single_dimension_plot_scatter"), title = "Scatter plot", collapsible = TRUE),
                box(width = 12, plotlyOutput("single_dimension_plot_line"), title = "Line plot", collapsible = TRUE),
            )
        }
    })
    
    
    
    
    
    output$xaxis_col <- renderUI({
        dat <- dt[,11:29]
        selectInput(inputId = "xaxis_col",
                    label = "Select X axis",
                    multiple = FALSE,
                    choices = names(dat))
    })
    
    output$yaxis_col <- renderUI({
        dat <- dt[,11:29]
        selectInput(inputId = "yaxis_col",
                    label = "Select Y axis",
                    multiple = FALSE,
                    choices = names(dat))
    })
    
    output$color_plot_type <- renderUI({
        dat <- dt[,11:29]
        selectInput(inputId = "color_column",
                    label = "Group/Color",
                    multiple = FALSE,
                    choices = names(dat))
    })
    
    output$plot_type <- renderUI({
        radioButtons(inputId = "plot_type",
                     label = "Select Plot type",
                     choices = c("Bar", "Scatter", "Line"),
                     selected = "Bar",
                     inline = TRUE)
    })
    
    
    output$plot_me <- renderPlotly({
        dat <- dt[,11:29]
        col_pos_x <- which(names(dat) == input$xaxis_col)
        col_pos_y <- which(names(dat) == input$yaxis_col)
        color_pos <- which(names(dat) == input$color_column)
        
        if(input$plot_type == "Scatter") {
            p <- plot_ly(data = dat,
                         # dat[[col_pos_x[j]]]
                         x = dat[[col_pos_x]],
                         y = dat[[col_pos_y]],
                         type = 'scatter',
                         color = dat[[color_pos]]) %>%
                layout(xaxis = list(title = input$xaxis_col),
                       yaxis = list(title = input$yaxis_col))
        }
        if(input$plot_type == "Bar") {
            p <- plot_ly(data = dat,
                         x = dat[[col_pos_x]],
                         y = dat[[col_pos_y]],
                         type = 'bar',
                         color = dat[[color_pos]])  %>%
                layout(xaxis = list(title = input$xaxis_col),
                       yaxis = list(title = input$yaxis_col))
        }
        if(input$plot_type == "Line") {
            p <- plot_ly(data = dat,
                         x = dat[[col_pos_x]],
                         y = dat[[col_pos_y]],
                         mode = 'line',
                         type = "scatter",
                         color = dat[[color_pos]])  %>%
                layout(xaxis = list(title = input$xaxis_col),
                       yaxis = list(title = input$yaxis_col))
        }
        if(is.character(dat[[col_pos_x]]) && is.character(dat[[col_pos_y]])) {
            p <- plotly::plot_ly(data = dat, x = dat[[col_pos_x]], y = dat[[col_pos_y]])
        }
        p
    })

    
    
    
    
    
    
    output$xaxis_col_3d <- renderUI({
        dat <- dt[,11:29]
        selectInput(inputId = "xaxis_col_3d", label = "Select X axis", multiple = FALSE, choices = names(dat))
    })
    
    output$yaxis_col_3d <- renderUI({
        dat <- dt[,11:29]
        selectInput(inputId = "yaxis_col_3d", label = "Select Y axis", multiple = FALSE, choices = names(dat))
    })
    output$zaxis_col_3d <- renderUI({
        dat <- dt[,11:29]
        selectInput(inputId = "zaxis_col_3d", label = "Select z axis", multiple = FALSE, choices = names(dat))
    })
    
    output$color_plot_type_3d <- renderUI({
        dat <- dt[,11:29]
        selectInput(inputId = "color_column_3d", label = "Group/Color", multiple = FALSE, choices = names(dat))
    })
    
    output$plot_type_3d <- renderUI({
        radioButtons(inputId = "plot_type_3d", label = "Select Plot type", choices = c("Scatter", "Line"), selected = "Scatter", inline = TRUE)
    })
    
    
    output$plot_me_3d <- renderPlotly({
        dat <- dt[,11:29]
        col_pos_x <- which(names(dat) == input$xaxis_col_3d)
        col_pos_y <- which(names(dat) == input$yaxis_col_3d)
        col_pos_z <- which(names(dat) == input$zaxis_col_3d)
        color_pos <- which(names(dat) == input$color_column_3d)
        
        
        if(input$plot_type_3d == "Scatter") {
            p <- plot_ly(data = dat, x = dat[[col_pos_x]], y = dat[[col_pos_y]], z = dat[[col_pos_z]], color = dat[[color_pos]], type = "scatter3d") %>% layout(xaxis = list(title = input$xaxis_col), yaxis = list(title = input$yaxis_col), zaxis = list(title = input$zaxis_col) )
        }
        if(input$plot_type == "Line") {
            p <- plot_ly(data = dat, x = dat[[col_pos_x]], y = dat[[col_pos_y]], z = dat[[col_pos_z]], color = dat[[color_pos]], type = "scatter3d", mode = "lines") %>% layout(xaxis = list(title = input$xaxis_col), yaxis = list(title = input$yaxis_col), zaxis = list(title = input$zaxis_col) )
        }
        p
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    output$regression_predictor_cols <- renderUI({
        res <- dt[,11:29]
        chs <- getnumericCols(res)
        chs <- names(res)
        selectInput(inputId = "regression_predictor", label = "Select Predictors/Indipendent variables", choices = chs, multiple = TRUE, selected = chs[1:(length(chs)-1)])
    })
    
    output$regression_dependent_variable_col <- renderUI({
        res <- dt[,11:29]
        chs <- getnumericCols(res)
        chs <- names(res)
        selectInput(inputId = "regression_dependent_variable", label = "Select Dependent Variable", choices = chs, multiple = FALSE, selected = chs[length(chs)])
    })
    
    output$set_seed_regression <- renderUI({
        textInput(inputId = "set_seed", label = "Set seed",
                  value = 777)
    })
    
    output$train_data_set_size <- renderUI({
        textInput(inputId = "train_data_set_size_regression", label = "Train data size in pecentage", value = 80)
    })
    
    output$possible_models_to_train <- renderUI({
        
        req(input$regression_dependent_variable)
        
        res <- dt[,11:29]
        dep_col <- res[[input$regression_dependent_variable]]
        unique_items <- length(unique(dep_col))
        
        if(detectClass(dep_col) == "numeric" && unique_items <= 4) {
            
            radioButtons(inputId = "Model_type",
                         label = "Choose model type",
                         choices = c("Linear Regression",
                                     "Logistic Regression"),
                         selected = "Linear Regression")
            
        } else if(detectClass(dep_col) == "numeric" && unique_items > 5){
            
            radioButtons(inputId = "Model_type", 
                         label = "Choose model type",
                         choices = c("Linear Regression"),
                         selected = "Linear Regression")
            
        }  else if(detectClass(dep_col) %in% c("character", "factor")) {
            
            radioButtons(inputId = "Model_type",
                         label = "Choose model type",
                         choices = c("Logistic Regression"),
                         selected = "Logistic Regression")
            
        }
        
    })
    
    output$regression_build_action <- renderUI({
        actionBttn(inputId = "regression_build", label = "Train model")
    })
    
    
    
    # dataPartition(iris, 80)
    data_sampling_regression <- reactive({
        req(input$set_seed)
        set.seed(as.numeric(input$set_seed))
        res <- dt[,11:29]
        dataPartition(df = res, train_data_perc = input$train_data_set_size_regression)
    })
    
    output$regression_train_data <- DT::renderDataTable({
        x <- data_sampling_regression()
        DT::datatable(x$Train,
                      class = 'cell-border stripe',
                      selection=list(mode="multiple", target="row"),
                      rownames=FALSE,
                      options = list(scrollX = TRUE, autoWidth = FALSE))
    })
    
    output$regression_test_data <- DT::renderDataTable({
        req(data_sampling_regression())
        x <- data_sampling_regression()
        DT::datatable(x$Test,
                      class = 'cell-border stripe',
                      selection=list(mode="multiple", target="row"),
                      rownames=FALSE,
                      options = list(scrollX = TRUE, autoWidth = FALSE))
    })
    
    
    regression_model <- eventReactive(input$regression_build, {
        req(input$regression_dependent_variable)
        req(input$regression_predictor)
        validate(need(!is.null(input$regression_predictor) && !is.null(input$regression_dependent_variable),
                      "please select Dependent and Indipendent variables and click on 'Build Model' button"))
        res <- dt[,11:29]
        res <- na.omit(res)
        
        items <- length(unique(res[[input$regression_dependent_variable]]))
        eqn <-   as.formula(paste(input$regression_dependent_variable,
                                  paste(input$regression_predictor, collapse=" + "),
                                  sep=" ~ "))
        if(input$Model_type == "Linear Regression") {
            mod <- lm(formula = eqn, data = res)
        } else if(input$Model_type == "Logistic Regression") {
            dependent_variable <- input$regression_dependent_variable
            res[[dependent_variable]] <- factor(res[[dependent_variable]],
                                               levels = unique(res[[dependent_variable]]))
            if(items == 2) {
                mod <- glm(formula = eqn, family = binomial(link = "logit"), data = res)
            } else if(items > 2) {
                mod <- multinomial(eqn, df = res)
            }
        } else if(input$Model_type == "Random Forest") {
            missing_values_count <- missing_count(res)
            if(missing_values_count > 0) {
                mod <- "Please remove all Missing values from data"
            } else {
                eqn <-   as.formula(paste(input$regression_dependent_variable,
                                          paste(input$regression_predictor, collapse=" + "),
                                          sep=" ~ "))
                dependent_variable <- input$regression_dependent_variable
                res[,dependent_variable] <- factor(res[[dependent_variable]], levels = unique(res[[dependent_variable]]))
                res <- na.omit(res)
                mod <- caret::train(eqn, res)
            }
            
        }
        return(mod)
    })
    
    regression_validation <- reactive({
        validate(
            need(!is.null(regression_model()), "Please build the model first.")
        )
        mod <- regression_model()
        selectInput("validate_method", label = "Select data to validate the model", choices = c("Test data", "Upload new data"))
        
        if(input$validate_method == "Test data") {
            sampling_data <- data_sampling_regression()
            predictions <- predict(mod, sampling_data$Test[[input$regression_predictor]])
            actuals <- sampling_data$Test[[input$regression_dependent_variable]]
            pred <- data.frame(Actuals = actuals, Predictions = predictions)
            x <- cbind(Row_Id = 1:nrow(sampling_data$Test), pred)
            x <- pred
            p <- plot_ly(data = x, x = x$Row_Id, y = x$Actuals, type = "scatter",
                         name = "Actuals", mode = "line") %>%
                add_trace(y = x$Predictions, name = "Predictions", mode = "line")
            
        } else {
            
            data <- new_data_uploaded_to_predict()
            
            test <- data[[input$regression_predictor]]
            predictions <- predict(mod, test)
            actuals <- data[[input$regression_dependent_variable]]
            pred <- data.frame(Actuals = actuals, Predictions = predictions)
            x <- cbind(Row_Id = 1:nrow(data), pred)
            
            p <- plot_ly(data = x, x = x$Row_Id, y = x$Actuals, type = "scatter",
                         name = "Actuals", mode = "line") %>%
                add_trace(y = x$Predictions, name = "Predictions", mode = "line")
        }
        
        return(list(Table = pred, Plot = p))
    })
    
    output$conf <- renderUI({
        if(input$Model_type == "Linear Regression") {
            return()
        } else {
            verbatimTextOutput("confMatrix")
        }
    })
    
    output$confMatrix <- renderPrint({
        if(input$Model_type == "Linear Regression") {
            return()
        } else {
            mod <- regression_model()
            if(input$validate_method == "Test data") {
                sampling_data <- data_sampling_regression()
                predictions <- predict(mod, sampling_data$Test[[input$regression_predictor]])
                actuals <- sampling_data$Test[[input$regression_dependent_variable]]
                x <- confmatrix(preds = predictions, actuals = actuals)
            } else {
                data <- new_data_uploaded_to_predict()
                test <- data[[input$regression_predictor]]
                predictions <- predict(mod, test)
                actuals <- data[[input$regression_dependent_variable]]
                x <- confmatrix(preds = predictions, actuals = actuals)
            }
            
        }
        return(x)
    })
    
    output$regression_validation_table <- DT::renderDataTable({
        validate(
            need(!is.null(regression_model()) && !is.null(input$regression_predictor), "Please select the features and build/re-build the models")
        )
        res <- regression_validation()$Table
        DT::datatable(res,
                      class = 'cell-border stripe',
                      selection=list(mode="multiple", target="row"),
                      rownames=FALSE,
                      options = list(scrollX = TRUE, autoWidth = FALSE))
    })
    
    output$regression_validation_plot <- renderPlotly({
        validate(
            need(!is.null(regression_model()) && !is.null(input$regression_predictor), "Please select the features and build/re-build the models")
        )
        res <- regression_validation()$Plot
        res
    })
    
    output$regression_model_build_status_text <-  renderText({
        req(regression_model())
        mod <- regression_model()
        if(is.list(mod)) {
            return("Model building finished!")
        }
    })
    
    output$regression_model_summary <- renderPrint({
        validate(need(!is.null(regression_model()) && !is.null(input$regression_predictor),
                      "Please select the features and build/re-build the models"))
        req(regression_model())
        if(input$Model_type != "Linear Regression") {
            return(regression_model())
        } else {
            return(summary(regression_model()))
        }
    })
    
    output$linear_regression_model_details <- renderUI({
        fluidPage(DT::dataTableOutput("regression_coefficients_table"), br(),
                  uiOutput("regression_model_info"), br(),
                  DT::dataTableOutput("regression_model_metrics_interpretations"))
    })
    
    output$clasification_model_details <- renderPrint({
        mod <- regression_model()
        sampling_data <- data_sampling_regression()
        predictions <- predict(mod, sampling_data$Test[,input$regression_predictor])
        actuals <- sampling_data$Test[[input$regression_dependent_variable]]
        x <- confmatrix(preds = predictions, actuals = actuals)
        x
    })
    
    output$model_details <- renderUI({
        if(input$Model_type == "Linear Regression") {
            uiOutput("linear_regression_model_details")
        } else if(input$Model_type == "Logistic Regression" | input$Model_type == "Random Forest") {
            verbatimTextOutput("clasification_model_details")
        }
    })
    
    output$regression_validate_method <- renderUI({
        req(regression_model())
        if(is.null(regression_model())) {
            return()
        } else {
            selectInput("validate_method", label = "Select data to validate the model",
                        choices = c("Test data", "Upload new data"))
        }
    })
    
    output$file_to_predict_regression <- renderUI({
        req(input$validate_method)
        if(is.null(input$validate_method) || input$validate_method  == "Test data") {
            return()
        } else {
            fileInput(inputId = "file_for_prediction_regression", label = "Upload a csv file")
        }
    })
    
    output$regression_coefficients_table <- DT::renderDataTable({
        mod <- regression_model()
        res <- getCoefficients(mod)
        
        brks <- quantile(res[,3], probs = seq(.05, .95, .05), na.rm = TRUE)
        clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
            {paste0("rgb(255,", ., ",", ., ")")}
        DT::datatable(res,
                      class = 'cell-border stripe',
                      selection=list(mode="multiple", target="row"),
                      rownames=FALSE,
                      options = list(scrollX = TRUE, autoWidth = FALSE)) %>%
            DT::formatStyle(names(res)[3], backgroundColor = styleInterval(brks, clrs))
    })
    
    output$regression_model_info <- renderUI({
        mod <- regression_model()
        
        validations <- regression_validation()
        acts <- validations$Table$Actuals
        preds <- validations$Table$Predictions
        
        res <- regressionModelMetrics(model = mod, actuals = acts, predictions = preds)
        fluidRow(infoBox("R Squared", value = res$r.squared, width = 4, fill = TRUE),
                 infoBox("Adj R Squared", value = res$adj.r.squared, width = 4, fill = TRUE),
                 infoBox("AIC", value = res$AIC, width = 4, fill = TRUE),
                 infoBox("BIC", value = res$BIC, width = 4, fill = TRUE),
                 infoBox("MSE", value    = res$MSE, width = 4, fill = TRUE),
                 infoBox("RMSE", value = res$RMSE, width = 4, fill = TRUE),
                 infoBox("MAE", value = res$MAE, width = 4, fill = TRUE),
                 infoBox("MAPE", value = res$MAPE, width = 4, fill = TRUE),
                 infoBox("Corelation", value = res$Corelation, width = 4,
                         fill = TRUE))
    })
    
    output$regression_model_metrics_interpretations <- DT::renderDataTable({
        res <- data.frame(Statistic = c("R-Squared",
                                        "Adj R squared",
                                        "F-Statistic",
                                        "Std. Error",
                                        "t-statistic",
                                        "AIC",
                                        "BIC",
                                        "MAPE (Mean absolute percentage error)",
                                        "MSE (Mean squared error)"),
                          Interpretation = c("Higher the better (> 0.70)",
                                             "Higher the better",
                                             "Higher the better",
                                             "Closer to zero the better",
                                             "Should be greater 1.96 for p-value to be less than 0.05",
                                             "Lower the better",
                                             "Lower the better",
                                             "Lower the better",
                                             "Lower the better"))
        DT::datatable(res,
                      class = 'cell-border stripe',
                      selection=list(mode="multiple", target="row"),
                      rownames=FALSE,
                      options = list(scrollX = TRUE, autoWidth = FALSE))
    })
    
    new_data_uploaded_to_predict <- reactive({
        req(input$validate_method)
        input_file <- input$file_for_prediction_regression
        input_file_path <- input_file$datapath
        x <- read.csv(input_file_path)
        return(x)
    })
    
    output$files_tar_button <- downloadHandler(
        filename <- function() {
            paste("inputFiles", "tar", sep=".")
        },
        
        content <- function(file) {
            tar(file, "input_files/")
        }
    )
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    as_valid_set_for_clustering <- function(x){
        dat <- x
        dat <- na.omit(dat)
        res <- lapply(names(dat), FUN = function(x) {
            
            cls <- class(dat[,x])
            
            if(cls == "factor"){
                numOrchar <- grepl("a-zA-Z", x)
                if(numOrchar) {
                    cls <- "numeric"
                } else {
                    cls <- "character"
                }
            }
            cls
        }) %>% unlist
        flag <- grepl("character", res)
        return(list(types = res, column_ischar = flag, valid_set_for_clustering = ifelse(sum(flag) >=1, FALSE, TRUE)))
    }
    
    # as_valid_set_for_clustering(iris[1:4])
    
    output$clustering_columns_selection_ui <- renderUI({
        res <- dt[,11:29]
        chs <- getnumericCols(res)
        chs <- names(res)
        selectInput(inputId = "input_cols_for_kmeans_clustering", 
                    label = "Select Input columns", 
                    choices = chs, multiple = TRUE, 
                    selected = chs[1:(length(chs)-1)])
    })
    
    isPackageInstalled <- function(package) {
        installed <- system.file(package = package)
        return(installed != "")
    }
    
    output$clustering_status <- renderUI({
        
        if(isPackageInstalled('factoextra')){
            return()
        } else {
            h1("Please install factoextra package to use this feature")
        }
        
    })
    
    output$partition_cluster_analysis_method <- renderUI({
        radioButtons(inputId = 'partition_cluster_method', 
                     label = "Select clustering method", 
                     choices = c('K-means', 
                                 "Hierarchical"), 
                     selected = "K-means", inline = TRUE)
    })
    
    output$Cluster_analysis_inputs_ui <- renderUI({
        fluidPage(
            box(width = 12, 
                # title = "K-means Clustering",
                uiOutput("clustering_columns_selection_ui"),
                uiOutput("partition_cluster_analysis_method"),
                uiOutput('kmeans_ncluster_input_ui'),
                uiOutput('nclust_method_input'),
                actionButton("submitt_button_clustering", "Perform Cluster analysis")
            )
        )
    })
    
    
    output$kmeans_cluster_plot <- renderPlot({
        p <- kmeans_analysis()$plot
        p
    })
    
    output$hierarchical_cluster_plot <- renderPlot({
        p <- hierarchical_clustering_analysis()
        p
    })
    output$n_optima_clusters_plot <- renderPlot({
        optimal_number_of_clusters()
    })
    
    output$cluster_analysis_ui <- renderUI({
        if(length(input$input_cols_for_kmeans_clustering) < 1) {
            h2("Selected columns are not sufficient to cluster the data")
        } else {
            if(input$partition_cluster_method == 'K-means') {
                box(width = 12, title = "K-means clustering",
                    box(plotOutput('n_optima_clusters_plot'), width = 12),
                    box(plotOutput('kmeans_cluster_plot'), width = 12),
                    DT::dataTableOutput('data_with_cluster_id_mapped'),
                    downloadBttn("downloadData_with_cluster_id_mapped", 
                                 "Download data mapped to cluster ids", 
                                 size = "sm", 
                                 color = "royal", 
                                 style = "stretch", 
                                 block = T),
                    plotOutput('ggpairs_plot_for_clusters'))
            } else {
                box(width = 12, title = "Hierarchical clustering",
                    plotOutput('hierarchical_cluster_plot'))
            }
        }
    })
    
    output$kmeans_ncluster_input_ui <- renderUI({
        shiny::textInput(inputId = "kmeans_n_clusters", 
                         label = 'Specify number of clusters', 
                         value = 2, 
                         placeholder = 'Enter a numeric value')
    })
    
    clustering_data <- reactive({
        df <- dt[,11:29]
        input$submitt_button_clustering
        isolate({
            df <- scale(na.omit(df[[input$input_cols_for_kmeans_clustering]]))
        })
        browser
        df
    })
    
    kmeans_analysis <- reactive({
        df <- clustering_data()
        # 2. Compute k-means
        set.seed(123)
        input$submitt_button_clustering
        isolate({
            cluster_analysis <- km.res <- kmeans(df, as.numeric(input$kmeans_n_clusters), nstart = 25)
        })
        
        # 3. Visualize
        plot <- fviz_cluster(km.res, data = df,
                             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
                             ggtheme = theme_minimal(),
                             main = "K-means Partitioning Clustering Plot"
        )
        
        return(list(plot = plot, 
                    cluster_analysis = cluster_analysis))
        
    })
    
    clusters_table <- reactive({
        df <- dt[,11:29]
        input$submitt_button_clustering
        isolate({
            df <- na.omit(df[[input$input_cols_for_kmeans_clustering]])
        })
        res <- kmeans_analysis()$cluster_analysis
        res <- as.numeric(res$cluster)
        df$Cluster_id <-  res
        df
    })
    
    output$data_with_cluster_id_mapped <-  DT::renderDataTable({
        res <- clusters_table()
        DT::datatable(res, class = 'cell-border stripe',
                      selection=list(mode="multiple", target="row"),
                      rownames=TRUE,
                      options = list(scrollX = TRUE,
                                     autoWidth = FALSE)
        )
        
    })
    
    output$downloadData_with_cluster_id_mapped <- downloadHandler(
        filename = function() {
            paste("Data_with_cluster_id_mapped_", 
                  Sys.time(),
                  ".csv", 
                  sep = "")
        },
        
        content = function(file) {
            res <- clusters_table()
            write.csv(res, file, row.names = FALSE)
        }
    )
    
    output$nclust_method_input <- renderUI({
        radioButtons(inputId = 'nclust_calc_method', 
                     label = 'Method to calculate optimal number of clusters', 
                     choices = c('Average of silhouette', 
                                 'Total within sum of square', 
                                 'Gap statistics'), 
                     selected = 'Gap statistics', 
                     inline = TRUE)
    })
    
    optimal_number_of_clusters <- reactive({
        my_data <- clustering_data()
        if(input$nclust_calc_method == 'Average of silhouette') {
            method = 'silhouette'
        } 
        if(input$nclust_calc_method == 'Total within sum of square') {
            method = 'wss'
        }
        if(input$nclust_calc_method == 'Gap statistics') {
            method = 'gap_stat'
        }
        fviz_nbclust(my_data, kmeans, method = method)
    })
    
    hierarchical_clustering_analysis <- reactive({
        # Compute hierarchical clustering and cut into 4 clusters
        res <- hcut(clustering_data(), k = as.numeric(input$kmeans_n_clusters), stand = TRUE)
        
        # Visualize
        fviz_dend(res, rect = TRUE, cex = 0.5,
                  k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))
        
    })
    
    output$ggpairs_plot_for_clusters <- renderPlot({
        # res <- clusters_table()
        x <- clustering_data()
        data <- scale(x[[input$input_cols_for_kmeans_clustering]])
        input$submitt_button_clustering
        isolate({
            final <- kmeans(x, as.numeric(input$kmeans_n_clusters), 25)
        })
        df <- as.data.frame(data)
        df$cluster <- final$cluster
        df$cluster <- as.character(df$cluster)
        ggpairs(df, 1:5, mapping = ggplot2::aes(color = cluster, alpha = 0.5), 
                diag = list(continuous = wrap("densityDiag")), 
                lower=list(continuous = wrap("points", alpha=0.9)))
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
})
