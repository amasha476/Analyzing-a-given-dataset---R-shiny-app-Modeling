library(shiny)
library(dplyr)
library (DescTools)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret)
library(glmnet)
library(knitr)
library(recipes) 
library(MASS)
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

shinyServer(function(input,output,session) {
  
  
  ## Side bar select input widget coming through renderUI() ##
  output$selectfile <- renderUI({
    if(is.null(input$file)) {return()}
    list(hr(),
         helpText("Select the files for which you need to see data and summary stats"),
         selectInput("Select", "Select", choices=input$file$name))
    })

  
  ## Summary Stats code ##
  output$summ <- renderPrint({
    if(is.null(input$file)){return("")}
    df=read.table(file=input$file$datapath[input$file$name==input$Select], 
                  sep=input$sep, 
                  header = input$header)
    summary(df)
    })
  
  
  ## Data set code ##
  output$table <- renderTable({ 
    if(is.null(input$file)){return()}
    read.table(file=input$file$datapath[input$file$name==input$Select], sep=",", header = input$header)
    })
  
  
  ## Quantitative Columns ##
  output$quan <- renderTable({ 
    if(is.null(input$file)){return()}
    df=read.table(file=input$file$datapath[input$file$name==input$Select],sep=input$sep, header = input$header)
    df_numeric=select_if(df, is.numeric)
    if ((ncol(df_numeric)!=0)){
      return(df_numeric)
    }
    else{
      return("")
    }
    
  })
  
  
  ## Qualitative Columns ##
  output$qual <- renderTable({ 
    if(is.null(input$file)){return()}
    df=read.table(file=input$file$datapath[input$file$name==input$Select], 
                  sep=input$sep, header = input$header)
    df_non_numeric=df[, sapply(df, class) == 'character']
    df_non_numeric=df_non_numeric %>%
      mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
    if ((ncol(df_non_numeric)!=0)){
      return(df_non_numeric)
    }
    else{
      return("")
    }
    })
  
  
  ## Missing values
  output$miss <- renderPrint({
    if(is.null(input$file)){return("")}
    df=read.table(file=input$file$datapath[input$file$name==input$Select], 
                  sep=input$sep, header = input$header)
    df[sapply(df, is.character)]=df[sapply(df, is.character)] %>%
      mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
    sapply(df,function(x)sum(is.na(x)))
  })

  ### Imputing Missing

  output$missimn <- renderPrint({
    if(is.null(input$file)){return("")}
    df=read.table(file=input$file$datapath[input$file$name==input$Select], 
                  sep=input$sep, header = input$header)
    df_numeric=select_if(df, is.numeric)
    df_non_numeric=df[, sapply(df, class) == 'character']
    if ((ncol(df_numeric)!=0)){
      if (sum(is.na(df_numeric)!=0)){
        for(i in 1:ncol(df_numeric)) {                                   # Replace NA in all columns
          df_numeric[ , i][is.na(df_numeric[ , i])] <- mean(df_numeric[ , i], na.rm = TRUE)
        }
        cat("Missing after imputation in  Numeric Columns ",sum(is.na(df_numeric)),"\n")
      }
      else{
        cat("No Missing values in the  Numeric Varaibles","\n")
        
      }
    }
    else{
      cat("No  Numeric Varaibles")}
  }) 
  

  output$missimc <- renderPrint({
    if(is.null(input$file)){return("")}
    df=read.table(file=input$file$datapath[input$file$name==input$Select], 
                  sep=input$sep, header = input$header)
    df_non_numeric=df[, sapply(df, class) == 'character']
    df_non_numeric=df_non_numeric %>%
      mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
    
  if ((ncol(df_non_numeric)!=0)){
    if (sum(is.na(df_non_numeric)!=0)){
      df_non_numeric =df_non_numeric %>% mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))
      cat("Missing after imputation in Non Numeric columns",sum(is.na(df_non_numeric)),"\n")
    }
    else{
      cat("No Missing values in the Non Numeric Varaibles","\n")
    }
  }
  else{
    cat("No  Non Numeric Varaibles","\n")
  }
  })
  
  ### outliers
  
  output$out <- renderPrint({
    if(is.null(input$file)){return("")}
    df=read.table(file=input$file$datapath[input$file$name==input$Select], 
                  sep=input$sep, header = input$header)
    df_numeric=select_if(df, is.numeric)
    if ((ncol(df_numeric)!=0)){
      if (sum(is.na(df_numeric)!=0)){
        for(i in 1:ncol(df_numeric)) {                                   # Replace NA in all columns
          df_numeric[ , i][is.na(df_numeric[ , i])] <- mean(df_numeric[ , i], na.rm = TRUE)
        }
      }
    }
    if ((ncol(df_numeric)!=0)){
      for (i in 1:ncol(df_numeric)) {
        cat("Outliers in ", names(df_numeric[i]),"  ")
        print(boxplot.stats(df_numeric[,i])$out)}}
    else{
      cat("No numeric columns to check outliers or visualize")
    }
    })
  
  ### plot numeric variables
  
  output$nplot <- renderPlot({
    if(is.null(input$file)){return("")}
    df=read.table(file=input$file$datapath[input$file$name==input$Select], 
                  sep=input$sep, header = input$header)
    df_numeric <- df %>%
      as_data_frame() %>%
      select_if(is.numeric) %>%
      gather(key = "variable", value = "value")
    if ((ncol(df_numeric)!=0)){
      if (sum(is.na(df_numeric)!=0)){
        for(i in 1:ncol(df_numeric)) {                                   # Replace NA in all columns
          df_numeric[ , i][is.na(df_numeric[ , i])] <- mean(df_numeric[ , i], na.rm = TRUE)
        }
      }
    }
   
    if ((ncol(df_numeric)!=0)){
      print(ggplot(df_numeric, aes(value)) +
        geom_boxplot(fill = '#0288d1')+
        facet_wrap(~variable))}
  })
  
  ### plot categorical variables
  output$cplot <- renderPlot({
    if(is.null(input$file)){return("")}
    df=read.table(file=input$file$datapath[input$file$name==input$Select], 
                  sep=input$sep, header = input$header)
    df_non_numeric=df[, sapply(df, class) == 'character']
    if ((ncol(df_non_numeric)!=0)){
      if (sum(is.na(df_non_numeric)!=0)){
        df_non_numeric =df_non_numeric %>% mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))
      }
    }
    par(mfrow = c(2,4))
    if ((ncol(df_non_numeric)!=0)){
      for (i in 1:ncol(df_non_numeric)){
        counts <- table(df_non_numeric[,i])
        print(barplot(counts, main=names(df_non_numeric[i]),col="#69b3a2", las=2,font.axis=1, cex.names=0.8))
      }}
  })
  
  ### get response
  output$response <- renderPrint({
    if(is.null(input$file)){return("")}
    cat("Your Response Variable is - ",input$ResponseVariable,"\n")
    df=read.table(file=input$file$datapath[input$file$name==input$Select], 
                  sep=input$sep, header = input$header)
    df_numeric=select_if(df, is.numeric)
    df_non_numeric=df[, sapply(df, class) == 'character']
    if ((ncol(df_numeric)!=0)){
      if (sum(is.na(df_numeric)!=0)){
        for(i in 1:ncol(df_numeric)) {                                   # Replace NA in all columns
          df_numeric[ , i][is.na(df_numeric[ , i])] <- mean(df_numeric[ , i], na.rm = TRUE)
        }}}
    df_non_numeric=df_non_numeric %>%
      mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))
    
    if ((ncol(df_non_numeric)!=0)){
      if (sum(is.na(df_non_numeric)!=0)){
        df_non_numeric =df_non_numeric %>% mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))
      }}
    combined_df=cbind(df_numeric,df_non_numeric)
    combined_df[sapply(combined_df, is.character)] <- lapply(combined_df[sapply(combined_df, is.character)], as.factor)
    
    ## removing the ID variables if present and variables with only one category
    cols_drop =c()
    for (i in 1:ncol(combined_df)) {
      if (nlevels(combined_df[,i])==nrow(combined_df) | (nlevels(combined_df[,i])==1)){
        cols_drop=append(cols_drop,i)
        
      }}
    if (length(cols_drop)!=0){combined_df_mod=combined_df[,-cols_drop]}
    else{combined_df_mod=combined_df}
    
    ## drop duplicates
    combined_df_mod=combined_df_mod %>% distinct()
    
    if(is.numeric(combined_df_mod[,input$ResponseVariable])==FALSE){
      combined_df_mod[,input$ResponseVariable]<-factor(combined_df_mod[,input$ResponseVariable],labels = c(0,1))
    }
    ## split data set
    n=nrow(combined_df_mod)
    set.seed(100)
    train=sample(1:n,n*0.8)
    traindata=combined_df_mod[train,]
    testdata=combined_df_mod[-train,]
    
    ## model fitting
    if (is.numeric(combined_df_mod[,input$ResponseVariable])==TRUE){
      cat("Response Variable is Numeric","\n")
      mlr = lm(formula(paste(input$ResponseVariable, "~ .")),data = traindata)
      step.model <- stepAIC(mlr, direction = "both",trace = FALSE)
      print(summary(step.model))
      # par(mfrow = c(2,2))
      # plot(step.model)
      predictions <-  predict(step.model,testdata)
      err=sqrt(sum((testdata[,input$ResponseVariable]-predictions)**2)/nrow(testdata))
      cat("Mean Squared Error -", err)
    }
    else{
      cat("Response Variable is Categorical","\n")
      LRm1 = glm(formula(paste(input$ResponseVariable, "~ .")), family = binomial(link = "logit"),data = traindata)
      step.model <- stepAIC(LRm1, direction = "both",trace = FALSE)
      print(summary(step.model))
      # par(mfrow = c(2,2))
      # plot(step.model)
      probabilities <-  predict(step.model,testdata, type="response")
      predicted.classes <- ifelse(probabilities < 0.5, 0,1)
      acc=mean(predicted.classes == testdata[,input$ResponseVariable])
      cat("Accuracy of the model",acc)}

    
  })
  
  
  ## Main Panel tabset renderUI code ##
  output$tb <- renderUI({
    if(is.null(input$file)) {return()}
    else
      tabsetPanel(
        tabPanel("View Dataset", tableOutput("table")),
        tabPanel("Summary Stats", verbatimTextOutput("summ")),
        tabPanel("Quantitative Columns", tableOutput("quan")),
        tabPanel("Qualitative Columns", tableOutput("qual")),
        tabPanel("Missing Values", verbatimTextOutput("miss")),
        tabPanel("Missing Values -After Imputation - Numeric ", verbatimTextOutput("missimn")),
        tabPanel("Missing Values -After Imputation - Categorical", verbatimTextOutput("missimc")),
        tabPanel("Outliers ", verbatimTextOutput("out")),
        tabPanel("Plots - Numeric", plotOutput("nplot")),
        tabPanel("Plots - Categorical", plotOutput("cplot")),
        tabPanel("Modeling ", verbatimTextOutput("response")))
  })
})
