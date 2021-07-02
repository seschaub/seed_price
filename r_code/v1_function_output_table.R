prediction_table <- function(plant_div, # indicates measure of diversity 
                             number_coef, # indicates coefficients in the model
                             col_number, # indicates which column should be filled
                             pred_data, # data containing predictions
                             conf_data, # data containing confidence intervals
                             table_boot_results # result table
){
  
  table_boot_results <- table_boot_results
  for (i in 1:number_coef) {
    
    
    if (i == 1) { # for intercept
      table_boot_results[i,col_number] <- paste0(round(pred_data[i,1],2),
                                                 ifelse(pred_data[i,4]<0.001,"***",ifelse(pred_data[i,4]<0.01,"**",ifelse(pred_data[i,4]<0.05,"*",""))),
                                                 " (",
                                                 as.numeric(round(conf_data[i,1],2)),
                                                 " to ",
                                                 as.numeric(round(conf_data[i,2],2)),
                                                 ")")
      table_boot_results[i,col_number+1] <- ""
      
    } else if (i == 2) { # for plant diversity 
      
      if (plant_div == "numb") {
        
        table_boot_results[i,col_number] <- paste0(round(pred_data[i,1],2),
                                                   ifelse(pred_data[i,4]<0.001,"***",ifelse(pred_data[i,4]<0.01,"**",ifelse(pred_data[i,4]<0.05,"*",""))),
                                                   " (",
                                                   as.numeric(round(conf_data[i,1],2)),
                                                   " to ",
                                                   as.numeric(round(conf_data[i,2],2)),
                                                   ")")
        table_boot_results[i,col_number+1] <- paste0(ifelse(pred_data[i,1] < 0, "","+"),round(exp(pred_data[i,1])-1,2)*100,"%")}
      
      if (plant_div == "shan") {
        
        table_boot_results[i+1,col_number] <- paste0(round(pred_data[i,1],2),
                                                     ifelse(pred_data[i,4]<0.001,"***",ifelse(pred_data[i,4]<0.01,"**",ifelse(pred_data[i,4]<0.05,"*",""))),
                                                     " (",
                                                     as.numeric(round(conf_data[i,1],2)),
                                                     " to ",
                                                     as.numeric(round(conf_data[i,2],2)),
                                                     ")")
        table_boot_results[i+1,col_number+1] <- paste0(ifelse(pred_data[i,1] < 0, "","+"),round(exp(pred_data[i,1])-1,2)*100,"%")}
    } else {
      
      table_boot_results[i+1,col_number] <- paste0(round(pred_data[i,1],2),
                                                   ifelse(pred_data[i,4]<0.001,"***",ifelse(pred_data[i,4]<0.01,"**",ifelse(pred_data[i,4]<0.05,"*",""))),
                                                   " (",
                                                   as.numeric(round(conf_data[i,1],2)),
                                                   " to ",
                                                   as.numeric(round(conf_data[i,2],2)),
                                                   ")")
      table_boot_results[i+1,col_number+1] <- paste0(ifelse(pred_data[i,1] < 0, "","+"),round(exp(pred_data[i,1])-1,2)*100,"%")
      
    }
    
  }
  return(table_boot_results)
}
