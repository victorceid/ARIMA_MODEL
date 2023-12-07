########################
#
# This function get the mean WIS for each U.S. State for N weeks ahead  
# using the AUTO ARIMA or the ENSEMBLE ARIMA model and not splited dataset.
#
########################

get_wis<-function(US_STATE=3, auto=TRUE, my_n_ahead=1,n_previous_weeks=104){
  all_data=list(grouped_data[[US_STATE]])
  state_name<- state_codes_population[state_codes_population$location == grouped_data[[US_STATE]]$location[1], ]$location_name
  
  my_preds_list<-listenv()
  plan(multisession)
  #plan(multicore)
  #my_preds_list<-listenv()
  com=c(0,1,2,3)
  my_order_params<-permutations(4,3,com, repeats.allowed = TRUE)
  for(i in 1:NROW(all_data) ){
    my_preds_list[[i]]%<-% PredictByIteration2(all_data, my_n_ahead=my_n_ahead, look_back_amount = n_previous_weeks, order_params=my_order_params,auto_seasonal = FALSE, auto=auto, my_seasonal=list(order = c(1,0,1), period = 0) ) %packages% "forecast" 
  }
  
  for(i in 1:NROW(my_preds_list)){
    suppressWarnings(invisible(resolved(my_preds_list[[i]])))
  }
  
  #put pred and quantiles into seperate lists
  list_all_preds<-list()
  list_all_quantiles<- list()
  list_all_gofs<- list()
  for(i in 1:NROW(my_preds_list) ){
    list_all_preds[[i]]<- my_preds_list[[i]][[1]][[1]]#I think the very last[[1]] is for the state
    list_all_quantiles[[i]]<-my_preds_list[[i]][[2]][[1]]#I think the very last[[1]] is for the state
  }
  
  #Ensemble prediction
  all_preds<-list()
  all_preds[[1]]<-Reduce(function(x, y) merge(x, y, all=TRUE), list_all_preds)#this doesn't work if listas are not all the same size
  
  final_quantiles<-list(list())
  for(i in 1:(NROW(list_all_quantiles))){
    print(paste0("i ", i ))
    for(a in 1:(NROW(list_all_quantiles[[i]]))){
      for(a in names(list_all_quantiles[[i]]))
        final_quantiles[[1]][[a]]<-list_all_quantiles[[i]][[a]]
    }
  }
  
  my_tibble_quantiles<- FormatForScoring_correct(pred_intervals=final_quantiles, state_codes_population, grouped_data, model_name = "TestModel", my_n_week_ahead = my_n_ahead, state_number=US_STATE)
  
  #get true values and put quantiles into a tibble
  
  grouped_data_copy_<-grouped_data
  grouped_data_df<-NULL
  grouped_data_df<-as.data.frame(grouped_data[[US_STATE]])
  grouped_data_df["target_variable"]<-"cases"
  grouped_data_df["model"]<-my_tibble_quantiles[1,"model"]#"TestModel"
  grouped_data_df<- grouped_data_df %>% rename_at("cases", ~'value')
  
  my_forecast_scores<-score_forecasts(my_tibble_quantiles, grouped_data_df)
  
  wis<-round(mean(my_forecast_scores[,"wis"]$wis), 3)
  final_result<-paste0(US_STATE,"-",state_name,", WIS:", wis, ", WEEK AHEAD:", my_n_ahead)
  return(final_result)
}

########################
#
# This function get the mean WIS for each U.S. State for N weeks ahead  
# using the AUTO ARIMA or the ENSEMBLE ARIMA model and the splited dataset.
#
########################

get_wis_splits<-function(US_STATE=3, auto=TRUE, my_n_ahead=1){
  
  all_data=grouped_data[[US_STATE]]
  state_name<- state_codes_population[state_codes_population$location == grouped_data[[US_STATE]]$location[1], ]$location_name
  
  ##-- USE THE SLIPTED DATASET TO SAVE COMPUTATIONAL POWER --#
  my_splits<-GetSplits(NROW(all_data), 6, over_lap=105, min_split_length = 116)
                                          #over_lap=105
  reduced_grouped_data_list<-list()
  for(i in 1:NROW(my_splits) ){
    temp_list<-list()
    temp_list[[1]]<-all_data[my_splits[[i]],]
    reduced_grouped_data_list[[i]]<-temp_list
  }
  
  plan(multisession)
  
  ##--------- Define the order of params using the permutation of [0,1,2,3] --------##
  my_preds_list<-listenv()
  com=c(0,1,2,3)
  my_order_params<-permutations(4,3,com, repeats.allowed = TRUE)
  
  ##--------- Run the PredictByIteration2 function for each of the slices --------##
  ##--------- look_back_amount defines the number of weeks we use to predict a day ahead (set to 104 weeks = 2 years) 
  
  for(i in 1:NROW(reduced_grouped_data_list) ){
    my_preds_list[[i]]%<-% PredictByIteration2(reduced_grouped_data_list[[i]], my_n_ahead=my_n_ahead, look_back_amount = 104, order_params=my_order_params, auto_seasonal = FALSE, auto=auto, my_seasonal=list(order = c(1,0,1), period = 0)) %packages% "forecast" 
  }
  
  for(i in 1:NROW(my_preds_list)){
    suppressWarnings(invisible(resolved(my_preds_list[[i]])))
  }
  
  #---- Get the predictions and quantiles results ----# 
  list_all_preds<-list()
  list_all_quantiles<- list()
  list_all_gofs<- list()
  for(i in 1:NROW(my_preds_list) ){
    list_all_preds[[i]]<- my_preds_list[[i]][[1]][[1]]#I think the very last[[1]] is for the state
    list_all_quantiles[[i]]<-my_preds_list[[i]][[2]][[1]]#I think the very last[[1]] is for the state
  }
  
  all_preds<-list()
  all_preds[[1]]<-Reduce(function(x, y) merge(x, y, all=TRUE), list_all_preds)#this doesn't work if listas are not all the same size
  
  final_quantiles<-list(list())
  for(i in 1:(NROW(list_all_quantiles))){
    print(paste0("i ", i ))
    for(a in 1:(NROW(list_all_quantiles[[i]]))){
      for(a in names(list_all_quantiles[[i]]))
        final_quantiles[[1]][[a]]<-list_all_quantiles[[i]][[a]]
    }
  }
  
  #---- Format the quantiles and calculate the WIS ----#   
  my_tibble_quantiles<- FormatForScoring_correct(pred_intervals=final_quantiles, state_codes_population, grouped_data, model_name = "TestModel", my_n_week_ahead = my_n_ahead, state_number=US_STATE)
  
  grouped_data_copy_<-grouped_data
  grouped_data_df<-NULL
  grouped_data_df<-as.data.frame(grouped_data[[US_STATE]])
  grouped_data_df["target_variable"]<-"cases"
  grouped_data_df["model"]<-my_tibble_quantiles[1,"model"]#"TestModel"
  grouped_data_df<- grouped_data_df %>% rename_at("cases", ~'value')
  
  my_forecast_scores<-score_forecasts(my_tibble_quantiles, grouped_data_df)
  
  wis<-round(mean(my_forecast_scores[,"wis"]$wis), 3)
  final_result<-paste0(US_STATE,"-",state_name,", WIS:", wis, ", WEEK AHEAD:", my_n_ahead)
  return(final_result)
}

####################
#
# This function formats each forecast quantiles so we can after calculate the WIS.
#
###################

FormatForScoring_correct <- function(pred_intervals, state_number=NULL, state_codes_population, grouped_data, model_name, my_n_week_ahead=1, my_temporal_resolution="wk", my_target_variable="cases") {
  my_tibble<- NULL
  my_tibble<-tibble(model=c(""),forecast_date=c(""), location=c(double() ), horizon=c(double() ),
                    temporal_resolution=c(""), target_variable=c(""), target_end_date=c(as.Date(c()) ), type= c(""), quantile=c(double() ),
                    value =c(double()))
  
  for(i in 1:NROW(pred_intervals) ){
    dates_to_get<- names(pred_intervals[[i]])
    my_location_name<- state_codes_population[state_codes_population$location==state_number,]$location_name
    my_location<-grouped_data[[state_number]]$location[1]
    
    for(dates_ in dates_to_get){
      
      my_target_end_date<-as.Date(dates_)-7
      my_tibble<- my_tibble%>%add_row(model=model_name,forecast_date=dates_, location=my_location, horizon=my_n_week_ahead,
                                      temporal_resolution=my_temporal_resolution, target_variable=my_target_variable, target_end_date=my_target_end_date, type= "point", quantile=NA,
                                      value = expm1(pred_intervals[[1]][[dates_]]$point_forecast[1]) )
      for(quantile_level in pred_intervals[[i]][dates_]){

        my_quantile_value<-expm1(quantile_level$quantile)

        my_tibble<-my_tibble%>%add_row(model=model_name,forecast_date=dates_, location=my_location, horizon=my_n_week_ahead,
                                       temporal_resolution=my_temporal_resolution, target_variable=my_target_variable, target_end_date=my_target_end_date, type= "quantile",
                                       quantile=quantile_level$pi_level, value = my_quantile_value)
      }
    }
  }
  return(my_tibble)
}

#################################
#
# This function gets the Absolute Error for each State and by each predicted week.
# 
#################################

GetErrors_Each_State<- function(my_data = NULL, prediction = NULL, US_STATE = 1){
  i=US_STATE
  error_list<-list()
  prediction_row_iter<- 1
  error<-0
  error_df<- data.frame("Error_For_Date"= NULL, "residuals" = NULL)
  if(nrow(prediction[[1]]) !=0 ){
    for(j in 1:length(my_data[[i]]$cases) ){
      if(!(prediction_row_iter <= nrow(prediction[[1]]) ) )
        break
      my_row<- my_data[[i]][j,]
      #print(paste0(my_row$target_end_date," ", prediction[[i]]$Pred_For_Date[prediction_row_iter], " prediction_row_iter ", prediction_row_iter, " i", i) )
      
      if(my_row$target_end_date != prediction[[1]]$Pred_For_Date[prediction_row_iter]){
        #print(paste0(my_row$target_end_date," pred date ", prediction[[i]]$Pred_For_Date[prediction_row_iter]," i ",i," start" ) )
        
        if(year(my_row$target_end_date) != year(prediction[[1]]$Pred_For_Date[prediction_row_iter]) && j>5){
          if(year(my_row$target_end_date) > year(prediction[[1]]$Pred_For_Date[prediction_row_iter]) && j>5){
            prediction_row_iter<- prediction_row_iter + 1
          }
        }
        else{
          while(my_row$target_end_date > prediction[[1]]$Pred_For_Date[prediction_row_iter] && j>5){
            prediction_row_iter<- prediction_row_iter + 1
          }
          if(my_row$target_end_date < prediction[[1]]$Pred_For_Date[prediction_row_iter]){
            j<- j-1
          }
        }
        
        #print(paste0(my_row$target_end_date," error date ", prediction[[i]]$Pred_For_Date[prediction_row_iter]," end" ) )
      }
      
      if(my_row$target_end_date == prediction[[1]]$Pred_For_Date[prediction_row_iter]){
        #error<-mase(my_row$cases, expm1(prediction[[1]]$Prediction[prediction_row_iter]), step_size = 1)
        error<- my_row$cases - expm1(prediction[[1]]$Prediction[prediction_row_iter])
        tmp_df<- data.frame("Error_For_Date"= prediction[[1]]$Pred_For_Date[prediction_row_iter], "residuals" = error)
        error_df<-rbind(error_df, tmp_df)
        prediction_row_iter<- prediction_row_iter + 1 
      }
    }
  }
  else{
    print(paste0("nrow(prediction[[i]]) !=0 ", i) )
  }
  
  error_list[[1]]<- error_df
  return(error_list)
  
}

#################################
#
# THIS FUNCTION GETS PREDICTIONS FOR N-WEEKS AHEAD TO CALCULATE THE ERRORS.
#
#################################

get_all_preds<-function(US_STATE=3, auto=TRUE, my_n_ahead=1){
  all_data=grouped_data[[US_STATE]]
  
  state_name<- state_codes_population[state_codes_population$location == grouped_data[[US_STATE]]$location[1], ]$location_name
  
  ##-- USE THE SLIPTED DATASET TO SAVE COMPUTATIONAL POWER --#
  my_splits<-GetSplits(NROW(all_data), 6, over_lap=105, min_split_length = 116)
  
  reduced_grouped_data_list<-list()
  start_time = Sys.time()
  for(i in 1:NROW(my_splits) ){
    temp_list<-list()
    temp_list[[1]]<-all_data[my_splits[[i]],]
    reduced_grouped_data_list[[i]]<-temp_list
  }
  
  my_preds_list<-listenv()
  
  plan(multisession)
  com=c(0,1,2,3)
  my_order_params<-permutations(4,3,com, repeats.allowed = TRUE)
  for(i in 1:NROW(reduced_grouped_data_list) ){
    my_preds_list[[i]]%<-% PredictByIteration2(reduced_grouped_data_list[[i]], my_n_ahead=my_n_ahead, look_back_amount = 104, order_params=my_order_params,auto_seasonal = FALSE, auto=auto, my_seasonal=list(order = c(1,0,1), period = 0) ) %packages% "forecast" 
  }
  
  for(i in 1:NROW(my_preds_list)){
    suppressWarnings(invisible(resolved(my_preds_list[[i]])))
  }
  
  #put pred and quantiles into seperate lists
  list_all_preds<-list()
  list_all_quantiles<- list()
  list_all_gofs<- list()
  for(i in 1:NROW(my_preds_list) ){
    list_all_preds[[i]]<- my_preds_list[[i]][[1]][[1]]#I think the very last[[1]] is for the state
    list_all_quantiles[[i]]<-my_preds_list[[i]][[2]][[1]]#I think the very last[[1]] is for the state
  }
  
  #Ensemble prediction
  all_preds<-list()
  all_preds[[1]]<-Reduce(function(x, y) merge(x, y, all=TRUE), list_all_preds)#this doesn't work if listas are not all the same size
  
return(all_preds)
}

####################################################################################
#                                                                                  #
# THIS FUNCTION GETS THE WIS TIME SERIES FOR EACH STATE BY N WEEK AHEAD PREDICTION #
# USING THE AUTO ARIMA OR THE ENSEMBLE ARIMA METHOD                                #
#                                                                                  #
####################################################################################

get_weekly_wis<-function(US_STATE=3, auto=TRUE, my_n_ahead=1){
  all_data=grouped_data[[US_STATE]]
  
  state_name<- state_codes_population[state_codes_population$location == grouped_data[[US_STATE]]$location[1], ]$location_name
  
  ##-- USE THE SLIPTED DATASET TO SAVE COMPUTATIONAL POWER --#
  my_splits<-GetSplits(NROW(all_data), 6, over_lap=105, min_split_length = 116)
  
  reduced_grouped_data_list<-list()
  start_time = Sys.time()
  for(i in 1:NROW(my_splits) ){
    temp_list<-list()
    temp_list[[1]]<-all_data[my_splits[[i]],]
    reduced_grouped_data_list[[i]]<-temp_list
  }
  
  my_preds_list<-listenv()
  
  plan(multisession)
  com=c(0,1,2,3)
  my_order_params<-permutations(4,3,com, repeats.allowed = TRUE)
  for(i in 1:NROW(reduced_grouped_data_list) ){
    my_preds_list[[i]]%<-% PredictByIteration2(reduced_grouped_data_list[[i]], my_n_ahead=my_n_ahead, look_back_amount = 104, order_params=my_order_params,auto_seasonal = FALSE, auto=auto, my_seasonal=list(order = c(1,0,1), period = 0) ) %packages% "forecast" 
  }
  
  for(i in 1:NROW(my_preds_list)){
    suppressWarnings(invisible(resolved(my_preds_list[[i]])))
  }
  
  #put pred and quantiles into seperate lists
  list_all_preds<-list()
  list_all_quantiles<- list()
  list_all_gofs<- list()
  for(i in 1:NROW(my_preds_list) ){
    list_all_preds[[i]]<- my_preds_list[[i]][[1]][[1]]#I think the very last[[1]] is for the state
    list_all_quantiles[[i]]<-my_preds_list[[i]][[2]][[1]]#I think the very last[[1]] is for the state
  }
  
  #Ensemble prediction
  all_preds<-list()
  all_preds[[1]]<-Reduce(function(x, y) merge(x, y, all=TRUE), list_all_preds)#this doesn't work if listas are not all the same size
  
  final_quantiles<-list(list())
  for(i in 1:(NROW(list_all_quantiles))){
    print(paste0("i ", i ))
    for(a in 1:(NROW(list_all_quantiles[[i]]))){
      for(a in names(list_all_quantiles[[i]]))
        final_quantiles[[1]][[a]]<-list_all_quantiles[[i]][[a]]
    }
  }
  
  my_tibble_quantiles<- FormatForScoring_correct(pred_intervals=final_quantiles, state_codes_population, grouped_data, model_name = "TestModel", my_n_week_ahead = my_n_ahead, state_number=US_STATE)
  
  #get true values and put quantiles into a tibble
  
  grouped_data_copy_<-grouped_data
  grouped_data_df<-NULL
  grouped_data_df<-as.data.frame(grouped_data[[US_STATE]])
  grouped_data_df["target_variable"]<-"cases"
  grouped_data_df["model"]<-my_tibble_quantiles[1,"model"]#"TestModel"
  grouped_data_df<- grouped_data_df %>% rename_at("cases", ~'value')
  
  my_forecast_scores<-score_forecasts(my_tibble_quantiles, grouped_data_df)
  weekly_wis<-data.frame(c(my_forecast_scores[,"forecast_date"]$forecast_date),c(my_forecast_scores[,"wis"]$wis)) 
  return(weekly_wis)
}

###########################################################################################
#' Title
#'
#' @param length_data_size int: the length(nrows) of the data that is being split
#' @param number_of_splits int: how many ways to split up the data
#' @param over_lap int: how much should the splits over lap: default=-1 meand no overlap
#' @param min_split_length int a minimum length(nrow) to be in each split
#'
#' @return splits list: a list of ranges for each split
#' @export
#'
#' @examples
#'
#' 
GetSplits<-function(length_data_size, number_of_splits, over_lap = 105, min_split_length=0){
  splits<-list()
  length_of_split<-trunc(length_data_size/number_of_splits)
  #print(paste0("length_of_split ",length_of_split))
  if(length_of_split < min_split_length)
    length_of_split<-min_split_length
  length_of_splits<-0
  for(i in 1:number_of_splits[1]){
    if(i !=1){
      splits[[i]]<-(splits[[i-1]][NROW(splits[[i-1]])]-over_lap):( splits[[i-1]][NROW(splits[[i-1]])]+length_of_split+1)
      
      #print(splits[[i]])
      if(splits[[i]][NROW(splits[[i-1]])] >=length_data_size )
        break
    }
    else{
      foo<- 1:length_of_split
      #print(paste0("1:length_of_split ", foo))
      splits[[i]]<- 1:length_of_split
    }
  }
  length_of_splits<-NROW(splits )
  #remove and values > length_data_size
  splits[[length_of_splits]] <- splits[[length_of_splits]][! splits[[length_of_splits]] %in% (length_data_size+1):splits[[length_of_splits]][NROW(splits[[length_of_splits]])] ]
  #print(paste0("After ", splits[[length_of_splits]]) )
  
  #does last split meet min_split_length
  if(NROW(splits[[length_of_splits]]) < min_split_length ){
    short_fall<- min_split_length- (splits[[length_of_splits]][NROW(splits[[length_of_splits]])] - splits[[length_of_splits]][1] )
    splits[[length_of_splits]]<-(splits[[length_of_splits]][1] -short_fall):splits[[length_of_splits]][NROW(splits[[length_of_splits]])]
    #print(paste0("Did we make up for short fall ", splits[[length_of_splits]]) )
  }
  return(splits)
}

########################
# PredictByIteration2 # look_back_amount=104
######################

PredictByIteration2 <- function(grouped_data, my_n_ahead=1, look_back_amount = 104, order_params=NULL, my_seasonal=list(order = c(1,0,1), period = 0), auto=FALSE,auto_seasonal=FALSE, test_value="Test") {
  
  models<-list()#models[[state]][[model for data it was trained on]]
  prediction<-list()#[[state]][[df containing date and predictions]]
  prediction_quantile<-list()
  model_gofs<-list(list())#[[state]][[df containing goodness of fit statistics]]
  #print(test_value)
  for(i in 1:length(grouped_data) ){
    temp_<-list()
    prediction_df<- data.frame("Prediction_For_Date"= NULL, "Prediction" = NULL)
    prediction_df_quantile<- data.frame("pi_level"= NULL, "lower" = NULL, "uppper" = NULL, "quantile"= NULL, "mid point" = NULL)
    prediction_quantile_ls<- list()
    model_gofs_df<- data.frame("Date"= NULL, "R2"= NULL, "AIC" = NULL, "BIC" = NULL)
    
    for(iter in 1:NROW(order_params) ){
      model_gofs[[i]][[iter]]<- data.frame("Date"= NULL, "R2"= NULL, "AIC" = NULL, "BIC" = NULL)
    }
    for(iter in  1:(NROW(grouped_data[[i]])-look_back_amount) ){
      #if(iter >5)
      #  break;
      sample_data<- iter:(look_back_amount+iter)# we may not be looking back the full time period when I split up the data????
      #print(paste0("iter ", iter) )
      fit<- NULL
      model_aic_scores<-c(length = 64)
      model_id<-1
      #cl<-makeCluster(4, type = "FORK")
      
      #registerDoParallel(cl)
      checker<-FALSE
      if(n_unique(log(grouped_data[[i]]$cases[sample_data]+1)) >10   ){
        
        #if()
        
        #start_time = Sys.time()
        for(j in 1:nrow(order_params) ){#seq_along(order_params[,1]) ){
          #r<-foreach(j = seq_along(order_params[,1]), .combine = list,
          #          .multicombine = TRUE, .packages = c("forecast"))%do%{#1:nrow(order_params) ){
          #print(paste0("here0 ",j) )
          fit1p<-NULL
          fit<- NULL
          doh<-FALSE
          tryCatch(
            expr = {
              
              if(!auto){
                #fit<-arima(log1p(grouped_data[[i]]$cases[sample_data]), order = order_params[j,], seasonal=my_seasonal, method = "CSS-ML") #, method = c("CSS") )# method=ML cause problem at order=(3,0,3)
                fit<-arima(log1p(grouped_data[[i]]$cases[sample_data]), order = order_params[j,], method = "CSS-ML") #, method = c("CSS") )# method=ML cause problem at order=(3,0,3)
              }
              else{
                fit<-invisible(auto.arima(ts(log1p(grouped_data[[i]]$cases[sample_data]), deltat = 1/52) ,stepwise=TRUE,approximation=FALSE,
                                seasonal=auto_seasonal, # This will extent to SARIMA
                                allowdrift=FALSE,
                                parallel = TRUE,  # speeds up computation, but tracing not available
                                trace=TRUE))
              }
              
              
              #fit<-arima(log(grouped_data[[i]]$cases[sample_data]+1), order = order_params[j,], seasonal=my_seasonal, method = "CSS-ML") #, method = c("CSS") )# method=ML cause problem at order=(3,0,3)
              temp_[[j]]<-fit#by doing this here there if arima throws and error we still have the last working model of that parameter set
              model_aic_scores[model_id]<- fit$aic
              #model_aic_scores<- fit$aic
              if(is.na(fit$aic) ){
                print("fit$aic is na")
              }
              #Note model_id we now lose track of model_id associated with the index of its order_params
              #model_id<-model_id+1
              #return(TRUE)
            }
            ,error = function(e){ 
              #print(e)
              #print("did we have an error")
              #  print(order_params[j,] )
              
              #print(paste0("Date ",grouped_data[[i]]$target_end_date[sample_data[NROW(sample_data)]], " param index ", j) )
            }
            
          )#end try cathc
          #temp_[[j]]<-fit#by doing this here there if arima throws and error we still have the last working model of that parameter set
          if(is.null(fit) || is.null(temp_[[j]]) ){
            temp_[[j]]<-NA
            model_aic_scores[model_id]<- NA
            checker<-TRUE
            #print("what")
          }
          else
            temp_[[j]]<-fit
          #Note model_id we now lose track of model_id associated with the index of its order_params
          model_id<-model_id+1
          if(any(is.na(sqrt(diag(fit$var.coef)) ) ) ){
            #print(paste0("i ", i," ",sqrt(diag(fit$var.coef))) )
            #find what what param migth cause the problem, it is probablty the seasonl thing 
            #par<-list(seasonal=list(order = models[[1]][[1]]$arma[4:6], period = models[[1]][[1]]$arma[7]) )
            #fit<-arima(log(train_test_validate[[i]][[sample_data]]$cases[weekly_cases]+1), order = par$order, seasonal=par$seasonal )
            
          }
          #uncomment the below to remove models that throw an error
          #temp_[[j]]<-fit
          #return(fit)
          
          if(auto)
            break#here for auto.arima only
        }
        
        predicted_value<- numeric(my_n_ahead)# list()
        n_models<- 0
        my_quantiles_total<-0
        pi<-numeric(my_n_ahead)
        m<- numeric(my_n_ahead)
        s<- numeric(my_n_ahead)
        model_id<-1
        
        min_aic<- min(model_aic_scores, na.rm = TRUE)
        total_aic<-sum(exp(-.5*(model_aic_scores-min_aic) ), na.rm =TRUE )
        model_weights<- c()
        flu_dates<- grouped_data[[i]]$target_end_date[sample_data]
        last_date <- max(flu_dates)
        prediction_date <- seq.Date(from = last_date + 7 , by = "week", length.out = my_n_ahead)
        
        for(my_model in temp_){
          if(length(my_model)>0 && !is.na(my_model[1])){
            model_weights_<- exp(-.5*(my_model$aic - min_aic))/total_aic
            predicted_value<- model_weights_*predict(my_model, n.ahead = my_n_ahead)$pred[my_n_ahead] + predicted_value
            pi<-forecast(my_model, h = my_n_ahead, level =  c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99))
            ### correct !!! 
            if(is.na(predicted_value[my_n_ahead])){
              print("predicted_value is na")
            }
            
            fc <- forecast(my_model, h=my_n_ahead, level=99) ### forecast for 99% confidence
            m <- model_weights_*fc$mean[my_n_ahead] +m ## fc$mean[1] or fc$mean[my_n_ahead] 
            s <- model_weights_*((fc$upper[my_n_ahead]-fc$lower[my_n_ahead])/2.58/2) + s # fc$upper[1] fc$lower[my_n_ahead]                   for 95%(fc$upper-fc$lower)/1.96/2 + s
            r_2<- R2(fitted.values(my_model),log1p(grouped_data[[i]]$cases[sample_data]), na.rm = TRUE ) # dados do modelo x dados reais
            model_gofs[[1]][[model_id]]<-rbind(model_gofs[[1]][[model_id]], data.frame("Date"= prediction_date, "R2"= r_2, "AIC" = my_model$aic, "loglik" = my_model$loglik))
            ####### correct !!!
            n_models<- n_models +1
            
          }
          else{
            model_gofs[[i]][[model_id]]<-rbind(model_gofs[[i]][[model_id]], data.frame("Date"= prediction_date[my_n_ahead], "R2"= NA, "AIC" = NA, "loglik" = NA))
          }
          model_id<-model_id+1
        }
        
        if((NROW(sample_data)+1) <= nrow(grouped_data[[i]]) ){
          tmp_df<- data.frame("Pred_For_Date"= prediction_date[my_n_ahead], "Prediction" = predicted_value[my_n_ahead])
          prediction_df<-rbind(prediction_df, tmp_df)
          my_quantiles<- qnorm(c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99), m[1], s[1])
          prediction_df_quantile<- data.frame("pi_level"= NULL, "lower" = NULL, "uppper" = NULL, "quantile"= NULL, "mid point" = NULL)
          
          for(j in 1:23){ 
            
            tmp_df_quantile<- data.frame("pi_level"= pi[["level"]][j]*.01, "lower" = pi[["lower"]][[j+(23*(my_n_ahead-1))]], "uppper" = pi[["upper"]][[j+(23*(my_n_ahead-1))]],"quantile"= my_quantiles[j], "point_forecast" = predicted_value[my_n_ahead])
            prediction_df_quantile<-rbind(prediction_df_quantile, tmp_df_quantile)
            
          }
          
          prediction_quantile_ls[[toString(prediction_date[my_n_ahead])]]<-prediction_df_quantile
          
        }
        
      }
      else
        print(paste0("not enough unique values ", i, " sample_data ",sample_data ) )
    }
    print("...")
    prediction[[i]]<-prediction_df
    prediction_quantile[[i]]<- prediction_quantile_ls
    #models[[i]]<-temp_
    
    #remove after testing
    if(i>=1)
      break;
  }
  
  #stopCluster(cl)
  
  return(list("Point_ForeCast "=prediction, "Quantiles"=prediction_quantile, "Gofs"=model_gofs) )
}
