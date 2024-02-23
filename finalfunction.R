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

## get mode
calc_mode <- function(x){
  
  # List the distinct / unique values
  distinct_values <- unique(x)
  
  # Count the occurrence of each distinct value
  distinct_tabulate <- tabulate(match(x, distinct_values))
  
  # Return the value with the highest occurrence
  distinct_values[which.max(distinct_tabulate)]
}

data_pre_process<- function(data,response) {
  # read the data set
  data=read.csv(data,header=TRUE,na.strings = c(""))
  df_numeric=select_if(data, is.numeric) # select only numeric columns
  df_numeric_null=select_if(df_numeric, anyNA) # select the numeric columns which have null values
  df_non_numeric=data[,!(names(data) %in% names(df_numeric))] # select non numeric columns
  df_non_numeric_null=select_if(df_non_numeric, anyNA) # select non numeric columns with null values
  cat("Quantitaive variables in the dataset -",names(df_numeric),"\n")
  cat("Qulitatitive variables in the dataset -",names(df_non_numeric_null),"\n")
  cat("Quantitaive variables that contain NA -",names(df_numeric_null),"\n")
  cat("Qualitative  variables that contain NA -",names(df_non_numeric_null),"\n")
  
  
  # imputing missing values by mean
  df_numeric_null_imputed <- df_numeric_null
  if (sum(is.na(df_numeric_null_imputed))!=0){
    for(i in 1:ncol(df_numeric_null)) {                                   # Replace NA in all columns
      df_numeric_null_imputed[ , i][is.na(df_numeric_null_imputed[ , i])] <- mean(df_numeric_null_imputed[ , i], na.rm = TRUE)
    }
    cat("Missing after imputation",sum(is.na(df_numeric_null_imputed)),"\n")
  }
  else{
    cat("Numeric Missing - 0","\n")
  }
  
  # imputing categorical missing values by mode
  if (sum(is.na(df_non_numeric_null))!=0){
    df_non_numeric_null_imputed =df_non_numeric_null %>% 
      mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))
    cat("Missing after imputation",sum(is.na(df_non_numeric_null_imputed)),"\n")
  }
  else{
    df_non_numeric_null_imputed =df_non_numeric_null
    cat("Non Numeric Missing - 0","\n")
  }
  
  options(repr.plot.width =3, repr.plot.height =5)
  # outlier identification and quantitative variable visualization
  if (ncol(df_numeric)!=0){ # consider only the numeric fields
    common_cols_num=intersect(names(df_numeric), names(df_numeric_null_imputed))
    df_numeric[common_cols_num]=df_numeric_null_imputed[common_cols_num]
    for (i in 1:ncol(df_numeric)) {
      cat("Outliers in ", names(df_numeric[i]))
      print(boxplot.stats(df_numeric[,i])$out)
      print(ggplot(data = df_numeric, aes(y = df_numeric[,i])) +
              geom_boxplot(fill = '#0288d1')+xlab(NULL)+ylab(NULL)+ theme(axis.text.x = element_blank(),
                                                                          axis.ticks.x = element_blank())+ ggtitle(names(df_numeric[i])))
    }
  }
  else{
    print("No numeric Columns to plot and check outliers")
  }
  # qualitative variable visualization
  if (ncol(df_non_numeric)!=0){
    common_cols_non_num=intersect(names(df_non_numeric), names(df_non_numeric_null_imputed))
    df_non_numeric[common_cols_non_num]=df_non_numeric_null_imputed[common_cols_non_num]
    for (i in 1:ncol(df_non_numeric)){
      counts <- table(df_non_numeric[,i])
      barplot(counts, main=names(df_non_numeric[i]),col="#69b3a2", las=2,font.axis=1, cex.names=0.8)
    }}
  else{
    print("No non numeric Columns to plot")
  }
  
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
  
  if(is.numeric(combined_df_mod[,response])==FALSE){
    combined_df_mod[,response]<-factor(combined_df_mod[,response],labels = c(0,1))
  }
  
  
  ## split data set
  n=nrow(combined_df_mod)
  set.seed(100)
  train=sample(1:n,n*0.8)
  traindata=combined_df_mod[train,]
  testdata=combined_df_mod[-train,]

  
  ## model fitting
  if (is.numeric(combined_df_mod[,response])==TRUE){
    print("numeric response")
    mlr = lm(formula(paste(response, "~ .")),data = traindata)
    step.model <- stepAIC(mlr, direction = "both",trace = FALSE)
    summary(step.model)
    par(mfrow = c(2,2))
    plot(step.model)
    predictions <-  predict(step.model,testdata)
    err=sqrt(sum((testdata[,response]-predictions)**2)/nrow(testdata))
    cat("Mean Squared Error -", err)
  }
  else{
    print("non numeric response")
    LRm1 = glm(formula(paste(response, "~ .")), family = binomial(link = "logit"),data = traindata)
    step.model <- stepAIC(LRm1, direction = "both",trace = FALSE)
    summary(step.model)
    par(mfrow = c(2,2))
    plot(step.model)
    probabilities <-  predict(step.model,testdata, type="response")
    predicted.classes <- ifelse(probabilities < 0.5, 0,1)
    acc=mean(predicted.classes == testdata[,response])
    cat("Accuracy of the model",acc)}
  
  
  
}

#data_pre_process("Train_Loan_Home.csv","Loan_Status")

