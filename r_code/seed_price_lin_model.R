################################################################################
# R code related to                                                            #
# "The costs of diversity: higher prices for more diverse grassland seed       #
# mixtures" by Sergei Schaub, Robert Finger, Vera Steiner, Nina Buchmann,      #
# Valentin H. Klaus                                                            #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub and Vera Steiner                                  #
# -----------------------------------------------------------------------------#
# R script that runs the linear analysis of the relationship between seed      #
# prices and plant diversity                                                   #
################################################################################

# table of contents:
# 1. data and bootstrapping set-up
# 2. linear correlation model [with correlation for shop] 
# 3. linear correlation model [with shops and other confounding variables] 
# 4. quadratic correlation model [with correlation for shop]  
# 5. quadratic correlation model [with shops and other confounding variables]  
# 6. linear correlation model [with correlation for shop] only for shannon and 
#     with all observations 
# 7. linear correlation model [with shops and other confounding variables] only 
#     for shannon and with all observations
# 8. save figures and tables


################################################################################
# 1. data and bootstrapping set-up
################################################################################

n_boot <- 500 # set number of bootstraps

# create range of number of species or shannon in the dataset for 
## number of species
min_max <- data_1_short %>% summarise(min = min(number_species), max = max(number_species))
range_numb <- seq(min_max$min,min_max$max,by=1) %>% as.data.frame() %>% rename(number_species = 1)
## shannon
min_max <- data_1_short %>% summarise(min = min(shannon), max = max(shannon))
range_shan <- seq(min_max$min,min_max$max,by=0.001) %>% as.data.frame() %>% rename(shannon = 1)



count_shops <- data_1_short %>% group_by(online_shop) %>% summarise(n()) %>% 
  mutate(share = `n()`/265)


data_2_short <- data_1_short %>%
  mutate(native_ecotype = as.factor(ifelse(native_ecotype == "Yes",1,0)),
         permanent_grassland_or_others = as.factor(ifelse(permanent_grassland_or_others == "Yes",1,0)),
         organic_d = as.factor(ifelse(organic_d == "Yes",1,0)))

data_2_short <- fastDummies::dummy_cols(data_2_short, select_columns = "online_shop") %>% 
  rename(online_shop_agrarshop = 18,
         online_shop_eric = 19,
         online_shop_rieger = 20,
         online_shop_zeller = 21,
         online_shop_saatgut = 22,
         online_shop_ufa = 23) %>% 
  mutate(online_shop_agrarshop = as.factor(online_shop_agrarshop),
         online_shop_eric = as.factor(online_shop_eric),
         online_shop_rieger = as.factor(online_shop_rieger),
         online_shop_zeller = as.factor(online_shop_zeller),
         online_shop_saatgut = as.factor(online_shop_saatgut),
         online_shop_ufa = as.factor(online_shop_ufa),
         overswoing_d = as.factor(overswoing_d),
         native_and_permanent = ifelse(native_ecotype == 1 & permanent_grassland_or_others == 1, 1, 0),
         native_and_permanent = as.factor(native_and_permanent),
         permanent_not_native = ifelse(native_ecotype == 0 & permanent_grassland_or_others == 1, 1, 0),
         permanent_not_native = as.factor(permanent_not_native),
         price_kg_ln = log(price_kg),
         price_ha_ln = log(price_ha),
         online_shop = as.factor(online_shop))
  #bind_cols(data_1_short %>% dplyr::select(overswoing_d, equal_seed ))

data_2_short_shan_cor <- data_2_short %>% filter(equal_seed == 0)


# compute average values for model paramenters
## non-regional mixtures
av_values <- data_2_short %>% 
  summarise(mean_native_and_permanent_dummy = mean(as.numeric(as.character(native_and_permanent))),
            mean_permanent_grassland_or_others_dummy = mean(as.numeric(as.character(permanent_grassland_or_others))),
            mean_organic_d = mean(as.numeric(as.character(organic_d))),
            mean_overswoing_d= mean(as.numeric(as.character(overswoing_d))))

av_values_cor <- data_2_short_shan_cor %>% 
  summarise(mean_native_and_permanent_dummy = mean(as.numeric(as.character(native_and_permanent))),
            mean_permanent_grassland_or_others_dummy = mean(as.numeric(as.character(permanent_grassland_or_others))),
            mean_organic_d = mean(as.numeric(as.character(organic_d))),
            mean_overswoing_d= mean(as.numeric(as.character(overswoing_d))))

################################################################################
# 2. linear correlation model [with correlation for shop]
################################################################################

# call function that procudes output tables
setwd(wd_code)
source("v1_function_output_table.R")

##################--------------------------------------------------------------
# 2.1 number of species + price per kg
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 2.1.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_kg_ln ~ number_species + online_shop, data = data_2_short)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]


# predict values depending on plant diversity
pre_data_1_aux90 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.90 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                         max(data_2_short$number_species),
                                                                                                                                         by = 1))
pre_data_1_aux95 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.95 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                         max(data_2_short$number_species),
                                                                                                                                         by = 1))
pre_data_1_aux99 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.99 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                         max(data_2_short$number_species),
                                                                                                                                         by = 1))
pre_data_1 <- bind_cols(pre_data_1_aux90 %>% rename(number_species = 1, Prediction = 2, CI_upper_90 = 3, CI_lower_90 = 4),
                        pre_data_1_aux95 %>% rename(number_species = 1, Prediction = 2, CI_upper_95 = 3, CI_lower_95 = 4) %>% dplyr::select(CI_upper_95,CI_lower_95),
                        pre_data_1_aux99 %>% rename(number_species = 1, Prediction = 2, CI_upper_99 = 3, CI_lower_99 = 4) %>% dplyr::select(CI_upper_99,CI_lower_99))



# add the average values of the dataset 
pre_data_1 <- pre_data_1 %>% 
  mutate(average_values = 
           -(pre_data_1$Prediction[1]-as.numeric(reg_model$coefficients[2])) +
           as.numeric(reg_model$coefficients[1]) +
           as.numeric(reg_model$coefficients[3]) * as.numeric(count_shops[2,3]) +                           # weight of share of Eric Schweizer AG of all mixture
           as.numeric(reg_model$coefficients[4]) * as.numeric(count_shops[3,3]) +                           # weight of share of Rieger-Hofmann of all mixture
           as.numeric(reg_model$coefficients[5]) * as.numeric(count_shops[4,3]) +                           # weight of share of saaten-zeller of all mixture
           as.numeric(reg_model$coefficients[6]) * as.numeric(count_shops[5,3]) +                           # weight of share of saatgut-shop AG of all mixture
           as.numeric(reg_model$coefficients[7]) * as.numeric(count_shops[6,3]),                            # weight of share of UFA Samen AG of all mixture         Prediction = Prediction_x + average_values,
         Prediction = Prediction + average_values,
         CI_lower_90 = CI_lower_90 + average_values,
         CI_upper_90 = CI_upper_90 + average_values,
         CI_lower_95 = CI_lower_95 + average_values,
         CI_upper_95 = CI_upper_95 + average_values,
         CI_lower_99 = CI_lower_99 + average_values,
         CI_upper_99 = CI_upper_99 + average_values)


# get average predicted values
## 1 species
exp(pre_data_1[1,2]) # average absolute price of seed mixture with one species
## 10 species
exp(pre_data_1[10,2]) - exp(pre_data_1[1,2]) # average absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[10,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
## 30 species
exp(pre_data_1[30,2]) - exp(pre_data_1[1,2]) # average absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[30,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
## 60 species
exp(pre_data_1[60,2]) - exp(pre_data_1[1,2]) # average absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[60,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increae of seed mixture with 10 species compared to seed mixtures with one species


#-------------------------------------------------------------------------------
# 2.1.2 visualize predicted values
#-------------------------------------------------------------------------------

theme_set(theme_bw()) # set main theme of the plots
plot_predict_numb_kg <- 
  ggplot() +
  geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_99, ymax = CI_upper_99), stat = "identity", alpha = 1, color = "#000000", fill = "#deebf7") +
  geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_95, ymax = CI_upper_95), stat = "identity", alpha = 1, color = "#000000", fill = "#9ecae1") +
  #geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_90, ymax = CI_upper_90), stat = "identity", alpha = 1, color = "#000000", fill = "#2171b5") +
  theme(axis.title = element_text(size=21),
        axis.text  = element_text(size=18),
        title  = element_text(size=21),
        legend.text = element_text(size=21),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Price per kg (Euro; log)",limits = c(1.5,5.5), breaks = c(seq(1.5,5.5,.5))) +
  scale_x_continuous(name = "Number of Species") +
  guides(linetype = "none", alpha = "none", color = "none", shape = "none")
plot_predict_numb_kg




#-------------------------------------------------------------------------------
# 2.1.3 store results in table 
#-------------------------------------------------------------------------------

# create data frame
table_boot_results <- data.frame(Estimate = c("Intercept",
                                                     "Number of Species",
                                                     "Shannon Index",
                                                     "Online Shop = Eric Schweizer (CH)",
                                                     "Online Shop = Rieger-Hofmann (DE)",
                                                     "Online Shop = Saaten Zeller (DE)",
                                                     "Online Shop = Saatgut Shop (DE)",
                                                     "Online Shop = UFA Samen (CH)"), 
                                        Price_kg_numb = "", 
                                        Price_kg_numb_percent = "",
                                        Price_ha_numb = "",
                                        Price_ha_numb_percent = "",
                                        Price_kg_shan = "",
                                        Price_kg_shan_percent = "",
                                        Price_ha_shan = "",
                                        Price_ha_shan_percent = "",
                                        stringsAsFactors=FALSE)



table_boot_results_linear <- prediction_table(plant_div = "numb", number_coef = 7, col_number = 2, pred_data = pred_values,
                                              conf_data = conf_values, table_boot_results = table_boot_results)

  

##################--------------------------------------------------------------
# 2.2 number of species + price per ha
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 2.2.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_ha_ln ~ number_species + online_shop, data = data_2_short)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]


# predict values depending on plant diversity
pre_data_1_aux90 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.90 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                    max(data_2_short$number_species),
                                                                                                                                    by = 1))
pre_data_1_aux95 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.95 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                    max(data_2_short$number_species),
                                                                                                                                    by = 1))
pre_data_1_aux99 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.99 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                    max(data_2_short$number_species),
                                                                                                                                    by = 1))
pre_data_1 <- bind_cols(pre_data_1_aux90 %>% rename(number_species = 1, Prediction = 2, CI_upper_90 = 3, CI_lower_90 = 4),
                        pre_data_1_aux95 %>% rename(number_species = 1, Prediction = 2, CI_upper_95 = 3, CI_lower_95 = 4) %>% dplyr::select(CI_upper_95,CI_lower_95),
                        pre_data_1_aux99 %>% rename(number_species = 1, Prediction = 2, CI_upper_99 = 3, CI_lower_99 = 4) %>% dplyr::select(CI_upper_99,CI_lower_99))



# add the average values of the dataset 
pre_data_1 <- pre_data_1 %>% 
  mutate(average_values = 
           -(pre_data_1$Prediction[1]-as.numeric(reg_model$coefficients[2])) +
           as.numeric(reg_model$coefficients[1]) +
           as.numeric(reg_model$coefficients[3]) * as.numeric(count_shops[2,3]) +                           # weight of share of Eric Schweizer AG of all mixture
           as.numeric(reg_model$coefficients[4]) * as.numeric(count_shops[3,3]) +                           # weight of share of Rieger-Hofmann of all mixture
           as.numeric(reg_model$coefficients[5]) * as.numeric(count_shops[4,3]) +                           # weight of share of saaten-zeller of all mixture
           as.numeric(reg_model$coefficients[6]) * as.numeric(count_shops[5,3]) +                           # weight of share of saatgut-shop AG of all mixture
           as.numeric(reg_model$coefficients[7]) * as.numeric(count_shops[6,3]),                            # weight of share of UFA Samen AG of all mixture         Prediction = Prediction_x + average_values,
         Prediction = Prediction + average_values,
         CI_lower_90 = CI_lower_90 + average_values,
         CI_upper_90 = CI_upper_90 + average_values,
         CI_lower_95 = CI_lower_95 + average_values,
         CI_upper_95 = CI_upper_95 + average_values,
         CI_lower_99 = CI_lower_99 + average_values,
         CI_upper_99 = CI_upper_99 + average_values)


# get average predicted values
## 1 species
exp(pre_data_1[1,2]) # average absolute price of seed mixture with one species
## 10 species
exp(pre_data_1[10,2]) - exp(pre_data_1[1,2]) # average absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[10,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
## 30 species
exp(pre_data_1[30,2]) - exp(pre_data_1[1,2]) # average absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[30,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
## 60 species
exp(pre_data_1[60,2]) - exp(pre_data_1[1,2]) # average absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[60,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increae of seed mixture with 10 species compared to seed mixtures with one species


# get confidence intervals
## 1 species
exp(pre_data_1[1,5]) # upper 95% CI
exp(pre_data_1[1,6]) # lower 95% CI
## 10 species
### absolute
exp(pre_data_1[10,5]) - exp(pre_data_1[1,5]) # upper 95% CI
exp(pre_data_1[10,6]) - exp(pre_data_1[1,6]) # lower 95% CI
### relative
(exp(pre_data_1[10,5]) - exp(pre_data_1[1,5]))/exp(pre_data_1[1,5]) # upper 95% CI
(exp(pre_data_1[10,6]) - exp(pre_data_1[1,6]))/exp(pre_data_1[1,6]) # lower 95% CI
## 30 species
### absolute
exp(pre_data_1[30,5]) - exp(pre_data_1[1,5]) # upper 95% CI
exp(pre_data_1[30,6]) - exp(pre_data_1[1,6]) # lower 95% CI
### relative
(exp(pre_data_1[30,5]) - exp(pre_data_1[1,5]))/exp(pre_data_1[1,5]) # upper 95% CI
(exp(pre_data_1[30,6]) - exp(pre_data_1[1,6]))/exp(pre_data_1[1,6]) # lower 95% CI
## 60 species
### absolute
exp(pre_data_1[60,5]) - exp(pre_data_1[1,5]) # upper 95% CI
exp(pre_data_1[60,6]) - exp(pre_data_1[1,6]) # lower 95% CI
### relative
(exp(pre_data_1[60,5]) - exp(pre_data_1[1,5]))/exp(pre_data_1[1,5]) # upper 95% CI
(exp(pre_data_1[60,6]) - exp(pre_data_1[1,6]))/exp(pre_data_1[1,6]) # lower 95% CI


#-------------------------------------------------------------------------------
# 2.2.2 visualize predicted values
#-------------------------------------------------------------------------------

theme_set(theme_bw()) # set main theme of the plots
plot_predict_numb_ha <- 
  ggplot() +
  geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_99, ymax = CI_upper_99), stat = "identity", alpha = 1, color = "#000000", fill = "#deebf7") +
  geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_95, ymax = CI_upper_95), stat = "identity", alpha = 1, color = "#000000", fill = "#9ecae1") +
  #geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_90, ymax = CI_upper_90), stat = "identity", alpha = 1, color = "#000000", fill = "#2171b5") +
  theme(axis.title = element_text(size=21),
        axis.text  = element_text(size=18),
        title  = element_text(size=21),
        legend.text = element_text(size=21),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Price per ha (Euro; log)",limits = c(5,9.5), breaks = c(seq(5,9.5,0.5))) +
  scale_x_continuous(name = "Number of Species") +
  guides(linetype = "none", alpha = "none", color = "none", shape = "none")
plot_predict_numb_ha




#-------------------------------------------------------------------------------
# 2.2.3 store results in table 
#-------------------------------------------------------------------------------

table_boot_results_linear <- prediction_table(plant_div = "numb", number_coef = 7, col_number = 4, pred_data = pred_values, 
                                              conf_data = conf_values, table_boot_results = table_boot_results_linear)


# percentage confidence interval 
exp(conf_values[2,])-1 # plant diversity


##################--------------------------------------------------------------
# 2.3 shannon index + price per kg
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 2.3.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_kg_ln ~ shannon + online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]


# predict values depending on plant diversity
pre_data_1_aux90 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.90 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                                    max(data_2_short_shan_cor$shannon),
                                                                                                                                    by = 1))
pre_data_1_aux95 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.95 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                                    max(data_2_short_shan_cor$shannon),
                                                                                                                                    by = 1))
pre_data_1_aux99 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.99 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                                    max(data_2_short_shan_cor$shannon),
                                                                                                                                    by = 1))
pre_data_1 <- bind_cols(pre_data_1_aux90 %>% rename(shannon = 1, Prediction = 2, CI_upper_90 = 3, CI_lower_90 = 4),
                        pre_data_1_aux95 %>% rename(shannon = 1, Prediction = 2, CI_upper_95 = 3, CI_lower_95 = 4) %>% dplyr::select(CI_upper_95,CI_lower_95),
                        pre_data_1_aux99 %>% rename(shannon = 1, Prediction = 2, CI_upper_99 = 3, CI_lower_99 = 4) %>% dplyr::select(CI_upper_99,CI_lower_99))



# add the average values of the dataset 
pre_data_1 <- pre_data_1 %>% 
  mutate(average_values = 
           -(pre_data_1$Prediction[1]-as.numeric(reg_model$coefficients[2])) +
           as.numeric(reg_model$coefficients[1]) +
           as.numeric(reg_model$coefficients[3]) * as.numeric(count_shops[2,3]) +                           # weight of share of Eric Schweizer AG of all mixture
           as.numeric(reg_model$coefficients[4]) * as.numeric(count_shops[3,3]) +                           # weight of share of Rieger-Hofmann of all mixture
           as.numeric(reg_model$coefficients[5]) * as.numeric(count_shops[4,3]) +                           # weight of share of saaten-zeller of all mixture
           as.numeric(reg_model$coefficients[6]) * as.numeric(count_shops[5,3]) +                           # weight of share of saatgut-shop AG of all mixture
           as.numeric(reg_model$coefficients[7]) * as.numeric(count_shops[6,3]),                            # weight of share of UFA Samen AG of all mixture         Prediction = Prediction_x + average_values,
         Prediction = Prediction + average_values,
         CI_lower_90 = CI_lower_90 + average_values,
         CI_upper_90 = CI_upper_90 + average_values,
         CI_lower_95 = CI_lower_95 + average_values,
         CI_upper_95 = CI_upper_95 + average_values,
         CI_lower_99 = CI_lower_99 + average_values,
         CI_upper_99 = CI_upper_99 + average_values)



#-------------------------------------------------------------------------------
# 2.3.2 visualize predicted values
#-------------------------------------------------------------------------------

theme_set(theme_bw()) # set main theme of the plots
plot_predict_shan_kg <- 
  ggplot() +
  geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_99, ymax = CI_upper_99), stat = "identity", alpha = 1, color = "#000000", fill = "#deebf7") +
  geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_95, ymax = CI_upper_95), stat = "identity", alpha = 1, color = "#000000", fill = "#9ecae1") +
  #geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_90, ymax = CI_upper_90), stat = "identity", alpha = 1, color = "#000000", fill = "#2171b5") +
  theme(axis.title = element_text(size=21),
        axis.text  = element_text(size=18),
        title  = element_text(size=21),
        legend.text = element_text(size=21),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Price per kg (Euro; log)",limits = c(1.5,5.5), breaks = c(seq(1.5,5.5,.5))) +
  scale_x_continuous(name = "Shannon Index") +
  guides(linetype = "none", alpha = "none", color = "none", shape = "none")
plot_predict_shan_kg




#-------------------------------------------------------------------------------
# 2.3.3 store results in table 
#-------------------------------------------------------------------------------

table_boot_results_linear <- prediction_table(plant_div = "shan", number_coef = 7, col_number = 6, pred_data = pred_values, 
                                              conf_data = conf_values, table_boot_results = table_boot_results_linear)




##################--------------------------------------------------------------
# 2.4 shannon index + price per ha
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 2.4.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_ha_ln ~ shannon + online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]


# predict values depending on plant diversity
pre_data_1_aux90 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.90 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                             max(data_2_short_shan_cor$shannon),
                                                                                                                             by = 1))
pre_data_1_aux95 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.95 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                             max(data_2_short_shan_cor$shannon),
                                                                                                                             by = 1))
pre_data_1_aux99 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.99 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                             max(data_2_short_shan_cor$shannon),
                                                                                                                             by = 1))
pre_data_1 <- bind_cols(pre_data_1_aux90 %>% rename(shannon = 1, Prediction = 2, CI_upper_90 = 3, CI_lower_90 = 4),
                        pre_data_1_aux95 %>% rename(shannon = 1, Prediction = 2, CI_upper_95 = 3, CI_lower_95 = 4) %>% dplyr::select(CI_upper_95,CI_lower_95),
                        pre_data_1_aux99 %>% rename(shannon = 1, Prediction = 2, CI_upper_99 = 3, CI_lower_99 = 4) %>% dplyr::select(CI_upper_99,CI_lower_99))



# add the average values of the dataset 
pre_data_1 <- pre_data_1 %>% 
  mutate(average_values = 
           -(pre_data_1$Prediction[1]-as.numeric(reg_model$coefficients[2])) +
           as.numeric(reg_model$coefficients[1]) +
           as.numeric(reg_model$coefficients[3]) * as.numeric(count_shops[2,3]) +                           # weight of share of Eric Schweizer AG of all mixture
           as.numeric(reg_model$coefficients[4]) * as.numeric(count_shops[3,3]) +                           # weight of share of Rieger-Hofmann of all mixture
           as.numeric(reg_model$coefficients[5]) * as.numeric(count_shops[4,3]) +                           # weight of share of saaten-zeller of all mixture
           as.numeric(reg_model$coefficients[6]) * as.numeric(count_shops[5,3]) +                           # weight of share of saatgut-shop AG of all mixture
           as.numeric(reg_model$coefficients[7]) * as.numeric(count_shops[6,3]),                            # weight of share of UFA Samen AG of all mixture         Prediction = Prediction_x + average_values,
         Prediction = Prediction + average_values,
         CI_lower_90 = CI_lower_90 + average_values,
         CI_upper_90 = CI_upper_90 + average_values,
         CI_lower_95 = CI_lower_95 + average_values,
         CI_upper_95 = CI_upper_95 + average_values,
         CI_lower_99 = CI_lower_99 + average_values,
         CI_upper_99 = CI_upper_99 + average_values)



#-------------------------------------------------------------------------------
# 2.4.2 visualize predicted values
#-------------------------------------------------------------------------------

theme_set(theme_bw()) # set main theme of the plots
plot_predict_shan_ha <- 
  ggplot() +
  geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_99, ymax = CI_upper_99), stat = "identity", alpha = 1, color = "#000000", fill = "#deebf7") +
  geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_95, ymax = CI_upper_95), stat = "identity", alpha = 1, color = "#000000", fill = "#9ecae1") +
  #geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_90, ymax = CI_upper_90), stat = "identity", alpha = 1, color = "#000000", fill = "#2171b5") +
  theme(axis.title = element_text(size=21),
        axis.text  = element_text(size=18),
        title  = element_text(size=21),
        legend.text = element_text(size=21),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Price per ha (Euro; log)",limits = c(5,9.5), breaks = c(seq(5,9.5,0.5))) +
  scale_x_continuous(name = "Shannon Index") +
  guides(linetype = "none", alpha = "none", color = "none", shape = "none")
plot_predict_shan_ha




#-------------------------------------------------------------------------------
# 2.4.3 store results in table 
#-------------------------------------------------------------------------------

table_boot_results_linear <- prediction_table(plant_div = "shan", number_coef = 7, col_number = 8, pred_data = pred_values, 
                                              conf_data = conf_values, table_boot_results = table_boot_results_linear)

table_boot_results_without_controls <- table_boot_results_linear # store results 


# percentage confidence interval 
exp(conf_values[2,])-1 # plant diversity

##################--------------------------------------------------------------
# 2.5 combine plots
##################--------------------------------------------------------------


# create plot only for extracting a common legend
dat_legend <- pre_data_1 %>% dplyr::select(shannon, Prediction:CI_lower_99) %>% pivot_longer(!shannon, names_to = "type", values_to = "value") %>% 
  mutate(type = ifelse(str_detect(type, "99"),"99%-Confidence Band",ifelse(str_detect(type, "95"),"95%-Confidence Band",ifelse(str_detect(type, "90"),"90%-Confidence Band","Prediction"))))

plot_for_legend<-  ggplot() +
  geom_bar(data = dat_legend %>% filter(type != "Prediction" & type != "90%-Confidence Band" ), aes(x = factor(type), y = value, fill=factor(type)),  stat="identity") +
  geom_line(data = dat_legend %>% filter(type == "Prediction"), aes(x = factor(type), y = value, color=factor(type)), size = 2) +
  theme(axis.title = element_text(size=15),
        axis.text  = element_text(size=13),
        title  = element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "horizontal") +
  scale_fill_manual(values=c( "#9ecae1", "#deebf7")) +
  scale_color_manual(values=c("#000000")) 

# obtain legend
legend <- get_legend(plot_for_legend)  


# combine plots

fig_1_x <- ggarrange(ggarrange(plot_predict_numb_ha + labs(tag = "A)") + theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm")),  
                               plot_predict_shan_ha + labs(tag = "B)") + theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm")),  
                               plot_predict_numb_kg + labs(tag = "C)") + theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm")),  
                               plot_predict_shan_kg + labs(tag = "D)") + theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm")),
                               ncol=2, nrow=2, align = "hv"),
                     as_ggplot(legend),
                     ncol=1, nrow=2,
                     heights = c(20, 3))


 fig_price_diversity_without_controls <- fig_1_x


 plot_predict_numb_kg_without <- plot_predict_numb_kg
 plot_predict_numb_ha_without <- plot_predict_numb_ha
 plot_predict_shan_kg_without <- plot_predict_shan_kg
 plot_predict_shan_ha_without <- plot_predict_shan_ha
 




################################################################################
# 3. linear correlation model [with shops and other confounding variables]
################################################################################

##################--------------------------------------------------------------
# 3.1 number of species + price per kg
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 3.1.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_kg_ln ~ number_species + 
                  native_and_permanent + permanent_grassland_or_others + organic_d + overswoing_d +
                  online_shop, data = data_2_short)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]
vcov_boot[,9] <- boot_cov[,9]
vcov_boot[,10] <- boot_cov[,10]
vcov_boot[,11] <- boot_cov[,11]


# predict values depending on plant diversity
pre_data_1_aux90 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.90 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                    max(data_2_short$number_species),
                                                                                                                                    by = 1))
pre_data_1_aux95 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.95 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                    max(data_2_short$number_species),
                                                                                                                                    by = 1))
pre_data_1_aux99 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.99 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                    max(data_2_short$number_species),
                                                                                                                                    by = 1))
pre_data_1 <- bind_cols(pre_data_1_aux90 %>% rename(number_species = 1, Prediction = 2, CI_upper_90 = 3, CI_lower_90 = 4),
                        pre_data_1_aux95 %>% rename(number_species = 1, Prediction = 2, CI_upper_95 = 3, CI_lower_95 = 4) %>% dplyr::select(CI_upper_95,CI_lower_95),
                        pre_data_1_aux99 %>% rename(number_species = 1, Prediction = 2, CI_upper_99 = 3, CI_lower_99 = 4) %>% dplyr::select(CI_upper_99,CI_lower_99))



# add the average values of the dataset 
pre_data_1 <- pre_data_1 %>% 
  mutate(average_values = 
           -(pre_data_1$Prediction[1]-as.numeric(reg_model$coefficients[2])) +
           as.numeric(reg_model$coefficients[1]) +
           as.numeric(reg_model$coefficients[3]) * av_values$mean_native_and_permanent_dummy +                 # ecotype native (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[4]) * av_values$mean_permanent_grassland_or_others_dummy +  # permanent grassland (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[5]) * av_values$mean_organic_d +                            # organic mixtures (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[6]) * av_values$mean_overswoing_d +                         # over-sowing (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[7]) * as.numeric(count_shops[2,3]) +                        # weight of share of Eric Schweizer AG of all mixture
           as.numeric(reg_model$coefficients[8]) * as.numeric(count_shops[3,3]) +                        # weight of share of Rieger-Hofmann of all mixture
           as.numeric(reg_model$coefficients[9]) * as.numeric(count_shops[4,3]) +                        # weight of share of saaten-zeller of all mixture
           as.numeric(reg_model$coefficients[10]) * as.numeric(count_shops[5,3]) +                       # weight of share of saatgut-shop AG of all mixture
           as.numeric(reg_model$coefficients[11]) * as.numeric(count_shops[6,3]),                        # weight of share of UFA Samen AG of all mixture         Prediction = Prediction_x + average_values,
         Prediction = Prediction + average_values,
         CI_lower_90 = CI_lower_90 + average_values,
         CI_upper_90 = CI_upper_90 + average_values,
         CI_lower_95 = CI_lower_95 + average_values,
         CI_upper_95 = CI_upper_95 + average_values,
         CI_lower_99 = CI_lower_99 + average_values,
         CI_upper_99 = CI_upper_99 + average_values)


# get average predicted values
## 1 species
exp(pre_data_1[1,2]) # average absolute price of seed mixture with one species
## 10 species
exp(pre_data_1[10,2]) - exp(pre_data_1[1,2]) # average absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[10,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
## 30 species
exp(pre_data_1[30,2]) - exp(pre_data_1[1,2]) # average absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[30,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
## 60 species
exp(pre_data_1[60,2]) - exp(pre_data_1[1,2]) # average absolute price increae of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[60,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increae of seed mixture with 10 species compared to seed mixtures with one species




#-------------------------------------------------------------------------------
# 3.1.2 visualize predicted values
#-------------------------------------------------------------------------------

theme_set(theme_bw()) # set main theme of the plots
plot_predict_numb_kg <- 
  ggplot() +
  geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_99, ymax = CI_upper_99), stat = "identity", alpha = 1, color = "#000000", fill = "#deebf7") +
  geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_95, ymax = CI_upper_95), stat = "identity", alpha = 1, color = "#000000", fill = "#9ecae1") +
  #geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_90, ymax = CI_upper_90), stat = "identity", alpha = 1, color = "#000000", fill = "#2171b5") +
  theme(axis.title = element_text(size=21),
        axis.text  = element_text(size=18),
        title  = element_text(size=21),
        legend.text = element_text(size=21),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Price per kg (Euro; log)",limits = c(1.5,5.5), breaks = c(seq(1.5,5.5,.5))) +
  scale_x_continuous(name = "Number of Species") +
  guides(linetype = "none", alpha = "none", color = "none", shape = "none")
plot_predict_numb_kg




#-------------------------------------------------------------------------------
# 3.1.3 store results in table 
#-------------------------------------------------------------------------------

# create data frame
table_boot_results <- data.frame(Estimate = c("Intercept",
                                              "Number of Species",
                                              "Shannon Index",
                                              "Ecotype (Native = 1, Not Native = 0)",
                                              "Grassland (Permanent = 1, Not Permanent = 0)",
                                              "Organic (Organic = 1, Conventional = 0)",
                                              "Oversowing (Yes = 1, No = 0)",
                                              "Online Shop = Eric Schweizer (CH)",
                                              "Online Shop = Rieger-Hofmann (DE)",
                                              "Online Shop = Saaten Zeller (DE)",
                                              "Online Shop = Saatgut Shop (DE)",
                                              "Online Shop = UFA Samen (CH)"), 
                                 Price_kg_numb = "", 
                                 Price_kg_numb_percent = "",
                                 Price_ha_numb = "",
                                 Price_ha_numb_percent = "",
                                 Price_kg_shan = "",
                                 Price_kg_shan_percent = "",
                                 Price_ha_shan = "",
                                 Price_ha_shan_percent = "",
                                 stringsAsFactors=FALSE)



table_boot_results_linear <- prediction_table(plant_div = "numb", number_coef = 11, col_number = 2, pred_data = pred_values,
                                              conf_data = conf_values, table_boot_results = table_boot_results)



##################--------------------------------------------------------------
# 3.2 number of species + price per ha
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 3.2.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_ha_ln ~ number_species + 
                  native_and_permanent + permanent_grassland_or_others + organic_d + overswoing_d +
                  online_shop, data = data_2_short)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]
vcov_boot[,9] <- boot_cov[,9]
vcov_boot[,10] <- boot_cov[,10]
vcov_boot[,11] <- boot_cov[,11]


# predict values depending on plant diversity
pre_data_1_aux90 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.90 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                    max(data_2_short$number_species),
                                                                                                                                    by = 1))
pre_data_1_aux95 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.95 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                    max(data_2_short$number_species),
                                                                                                                                    by = 1))
pre_data_1_aux99 <- cplot(reg_model,  vcov = vcov_boot, "number_species", what = "prediction", level = 0.99 , draw = F, xvals = seq(min(data_2_short$number_species),
                                                                                                                                    max(data_2_short$number_species),
                                                                                                                                    by = 1))
pre_data_1 <- bind_cols(pre_data_1_aux90 %>% rename(number_species = 1, Prediction = 2, CI_upper_90 = 3, CI_lower_90 = 4),
                        pre_data_1_aux95 %>% rename(number_species = 1, Prediction = 2, CI_upper_95 = 3, CI_lower_95 = 4) %>% dplyr::select(CI_upper_95,CI_lower_95),
                        pre_data_1_aux99 %>% rename(number_species = 1, Prediction = 2, CI_upper_99 = 3, CI_lower_99 = 4) %>% dplyr::select(CI_upper_99,CI_lower_99))



# add the average values of the dataset 
pre_data_1 <- pre_data_1 %>% 
  mutate(average_values = 
           -(pre_data_1$Prediction[1]-as.numeric(reg_model$coefficients[2])) +
           as.numeric(reg_model$coefficients[1]) +
           as.numeric(reg_model$coefficients[3]) * av_values$mean_native_and_permanent_dummy +                 # ecotype native (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[4]) * av_values$mean_permanent_grassland_or_others_dummy +  # permanent grassland (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[5]) * av_values$mean_organic_d +                            # organic mixtures (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[6]) * av_values$mean_overswoing_d +                         # over-sowing (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[7]) * as.numeric(count_shops[2,3]) +                        # weight of share of Eric Schweizer AG of all mixture
           as.numeric(reg_model$coefficients[8]) * as.numeric(count_shops[3,3]) +                        # weight of share of Rieger-Hofmann of all mixture
           as.numeric(reg_model$coefficients[9]) * as.numeric(count_shops[4,3]) +                        # weight of share of saaten-zeller of all mixture
           as.numeric(reg_model$coefficients[10]) * as.numeric(count_shops[5,3]) +                       # weight of share of saatgut-shop AG of all mixture
           as.numeric(reg_model$coefficients[11]) * as.numeric(count_shops[6,3]),                        # weight of share of UFA Samen AG of all mixture         Prediction = Prediction_x + average_values,
         Prediction = Prediction + average_values,
         CI_lower_90 = CI_lower_90 + average_values,
         CI_upper_90 = CI_upper_90 + average_values,
         CI_lower_95 = CI_lower_95 + average_values,
         CI_upper_95 = CI_upper_95 + average_values,
         CI_lower_99 = CI_lower_99 + average_values,
         CI_upper_99 = CI_upper_99 + average_values)


# get average predicted values
## 1 species
exp(pre_data_1[1,2]) # average absolute price of seed mixture with one species
## 10 species
exp(pre_data_1[10,2]) - exp(pre_data_1[1,2]) # average absolute price increase of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[10,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increase of seed mixture with 10 species compared to seed mixtures with one species
## 30 species
exp(pre_data_1[30,2]) - exp(pre_data_1[1,2]) # average absolute price increase of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[30,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increase of seed mixture with 10 species compared to seed mixtures with one species
## 60 species
exp(pre_data_1[60,2]) - exp(pre_data_1[1,2]) # average absolute price increase of seed mixture with 10 species compared to seed mixtures with one species
(exp(pre_data_1[60,2]) - exp(pre_data_1[1,2]))/exp(pre_data_1[1,2]) # relative absolute price increase of seed mixture with 10 species compared to seed mixtures with one species

# get confidence intervals
## 1 species
exp(pre_data_1[1,5]) # upper 95% CI
exp(pre_data_1[1,6]) # lower 95% CI
## 10 species
### absolute
exp(pre_data_1[10,5]) - exp(pre_data_1[1,5]) # upper 95% CI
exp(pre_data_1[10,6]) - exp(pre_data_1[1,6]) # lower 95% CI
### relative
(exp(pre_data_1[10,5]) - exp(pre_data_1[1,5]))/exp(pre_data_1[1,5]) # upper 95% CI
(exp(pre_data_1[10,6]) - exp(pre_data_1[1,6]))/exp(pre_data_1[1,6]) # lower 95% CI
## 30 species
### absolute
exp(pre_data_1[30,5]) - exp(pre_data_1[1,5]) # upper 95% CI
exp(pre_data_1[30,6]) - exp(pre_data_1[1,6]) # lower 95% CI
### relative
(exp(pre_data_1[30,5]) - exp(pre_data_1[1,5]))/exp(pre_data_1[1,5]) # upper 95% CI
(exp(pre_data_1[30,6]) - exp(pre_data_1[1,6]))/exp(pre_data_1[1,6]) # lower 95% CI
## 60 species
### absolute
exp(pre_data_1[60,5]) - exp(pre_data_1[1,5]) # upper 95% CI
exp(pre_data_1[60,6]) - exp(pre_data_1[1,6]) # lower 95% CI
### relative
(exp(pre_data_1[60,5]) - exp(pre_data_1[1,5]))/exp(pre_data_1[1,5]) # upper 95% CI
(exp(pre_data_1[60,6]) - exp(pre_data_1[1,6]))/exp(pre_data_1[1,6]) # lower 95% CI



# compute price mark up of ecotypes
463.88 *  (exp(as.numeric(reg_model$coefficients[3]))-1)


#-------------------------------------------------------------------------------
# 3.2.2 visualize predicted values
#-------------------------------------------------------------------------------

theme_set(theme_bw()) # set main theme of the plots
plot_predict_numb_ha <- 
  ggplot() +
  geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_99, ymax = CI_upper_99), stat = "identity", alpha = 1, color = "#000000", fill = "#deebf7") +
  geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_95, ymax = CI_upper_95), stat = "identity", alpha = 1, color = "#000000", fill = "#9ecae1") +
  #geom_smooth(data = pre_data_1, aes(x = number_species, y = Prediction, ymin = CI_lower_90, ymax = CI_upper_90), stat = "identity", alpha = 1, color = "#000000", fill = "#2171b5") +
  theme(axis.title = element_text(size=21),
        axis.text  = element_text(size=18),
        title  = element_text(size=21),
        legend.text = element_text(size=21),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Price per ha (Euro; log)",limits = c(5,9.5), breaks = c(seq(5,9.5,.5))) +
  scale_x_continuous(name = "Number of Species") +
  guides(linetype = "none", alpha = "none", color = "none", shape = "none")
plot_predict_numb_ha




#-------------------------------------------------------------------------------
# 3.2.3 store results in table 
#-------------------------------------------------------------------------------

table_boot_results_linear <- prediction_table(plant_div = "numb", number_coef = 11, col_number = 4, pred_data = pred_values, 
                                              conf_data = conf_values, table_boot_results = table_boot_results_linear)

# percentage confidence interval 
exp(conf_values[2,])-1 # species diversity
exp(conf_values[3,])-1 # native ecotypes
exp(conf_values[4,])-1 # permanent
exp(conf_values[5,])-1 # organic
exp(conf_values[6,])-1 # overseeding

##################--------------------------------------------------------------
# 3.3 shannon index + price per kg
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 3.3.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_kg_ln ~ shannon + 
                  native_and_permanent + permanent_grassland_or_others + organic_d + overswoing_d +
                  online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]
vcov_boot[,9] <- boot_cov[,9]
vcov_boot[,10] <- boot_cov[,10]
vcov_boot[,11] <- boot_cov[,11]


# predict values depending on plant diversity
pre_data_1_aux90 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.90 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                             max(data_2_short_shan_cor$shannon),
                                                                                                                             by = 1))
pre_data_1_aux95 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.95 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                             max(data_2_short_shan_cor$shannon),
                                                                                                                             by = 1))
pre_data_1_aux99 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.99 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                             max(data_2_short_shan_cor$shannon),
                                                                                                                             by = 1))
pre_data_1 <- bind_cols(pre_data_1_aux90 %>% rename(shannon = 1, Prediction = 2, CI_upper_90 = 3, CI_lower_90 = 4),
                        pre_data_1_aux95 %>% rename(shannon = 1, Prediction = 2, CI_upper_95 = 3, CI_lower_95 = 4) %>% dplyr::select(CI_upper_95,CI_lower_95),
                        pre_data_1_aux99 %>% rename(shannon = 1, Prediction = 2, CI_upper_99 = 3, CI_lower_99 = 4) %>% dplyr::select(CI_upper_99,CI_lower_99))



# add the average values of the dataset 
pre_data_1 <- pre_data_1 %>% 
  mutate(average_values = 
           -(pre_data_1$Prediction[1]-as.numeric(reg_model$coefficients[2])) +
           as.numeric(reg_model$coefficients[1]) +
           as.numeric(reg_model$coefficients[3]) * av_values_cor$mean_native_and_permanent_dummy +                 # ecotype native (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[4]) * av_values_cor$mean_permanent_grassland_or_others_dummy +  # permanent grassland (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[5]) * av_values_cor$mean_organic_d +                            # organic mixtures (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[6]) * av_values_cor$mean_overswoing_d +                         # over-sowing (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[7]) * as.numeric(count_shops[2,3]) +                        # weight of share of Eric Schweizer AG of all mixture
           as.numeric(reg_model$coefficients[8]) * as.numeric(count_shops[3,3]) +                        # weight of share of Rieger-Hofmann of all mixture
           as.numeric(reg_model$coefficients[9]) * as.numeric(count_shops[4,3]) +                        # weight of share of saaten-zeller of all mixture
           as.numeric(reg_model$coefficients[10]) * as.numeric(count_shops[5,3]) +                       # weight of share of saatgut-shop AG of all mixture
           as.numeric(reg_model$coefficients[11]) * as.numeric(count_shops[6,3]),                        # weight of share of UFA Samen AG of all mixture         Prediction = Prediction_x + average_values,
         Prediction = Prediction + average_values,
         CI_lower_90 = CI_lower_90 + average_values,
         CI_upper_90 = CI_upper_90 + average_values,
         CI_lower_95 = CI_lower_95 + average_values,
         CI_upper_95 = CI_upper_95 + average_values,
         CI_lower_99 = CI_lower_99 + average_values,
         CI_upper_99 = CI_upper_99 + average_values)



#-------------------------------------------------------------------------------
# 3.3.2 visualize predicted values
#-------------------------------------------------------------------------------

theme_set(theme_bw()) # set main theme of the plots
plot_predict_shan_kg <- 
  ggplot() +
  geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_99, ymax = CI_upper_99), stat = "identity", alpha = 1, color = "#000000", fill = "#deebf7") +
  geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_95, ymax = CI_upper_95), stat = "identity", alpha = 1, color = "#000000", fill = "#9ecae1") +
  #geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_90, ymax = CI_upper_90), stat = "identity", alpha = 1, color = "#000000", fill = "#2171b5") +
  theme(axis.title = element_text(size=21),
        axis.text  = element_text(size=18),
        title  = element_text(size=21),
        legend.text = element_text(size=21),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Price per kg (Euro; log)",limits = c(1.5,5.5), breaks = c(seq(1.5,5.5,.5))) + 
  scale_x_continuous(name = "Shannon Index") +
  guides(linetype = "none", alpha = "none", color = "none", shape = "none")
plot_predict_shan_kg




#-------------------------------------------------------------------------------
# 3.3.3 store results in table 
#-------------------------------------------------------------------------------

table_boot_results_linear <- prediction_table(plant_div = "shan", number_coef = 11, col_number = 6, pred_data = pred_values, 
                                              conf_data = conf_values, table_boot_results = table_boot_results_linear)




##################--------------------------------------------------------------
# 3.4 shannon index + price per ha
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 3.4.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_ha_ln ~ shannon + 
                  native_and_permanent + permanent_grassland_or_others + organic_d + overswoing_d +
                  online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]
vcov_boot[,9] <- boot_cov[,9]
vcov_boot[,10] <- boot_cov[,10]
vcov_boot[,11] <- boot_cov[,11]


# predict values depending on plant diversity
pre_data_1_aux90 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.90 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                             max(data_2_short_shan_cor$shannon),
                                                                                                                             by = 1))
pre_data_1_aux95 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.95 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                             max(data_2_short_shan_cor$shannon),
                                                                                                                             by = 1))
pre_data_1_aux99 <- cplot(reg_model,  vcov = vcov_boot, "shannon", what = "prediction", level = 0.99 , draw = F, xvals = seq(min(data_2_short_shan_cor$shannon),
                                                                                                                             max(data_2_short_shan_cor$shannon),
                                                                                                                             by = 1))
pre_data_1 <- bind_cols(pre_data_1_aux90 %>% rename(shannon = 1, Prediction = 2, CI_upper_90 = 3, CI_lower_90 = 4),
                        pre_data_1_aux95 %>% rename(shannon = 1, Prediction = 2, CI_upper_95 = 3, CI_lower_95 = 4) %>% dplyr::select(CI_upper_95,CI_lower_95),
                        pre_data_1_aux99 %>% rename(shannon = 1, Prediction = 2, CI_upper_99 = 3, CI_lower_99 = 4) %>% dplyr::select(CI_upper_99,CI_lower_99))



# add the average values of the dataset 
pre_data_1 <- pre_data_1 %>% 
  mutate(average_values = 
           -(pre_data_1$Prediction[1]-as.numeric(reg_model$coefficients[2])) +
           as.numeric(reg_model$coefficients[1]) +
           as.numeric(reg_model$coefficients[3]) * av_values_cor$mean_native_and_permanent_dummy +                 # ecotype native (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[4]) * av_values_cor$mean_permanent_grassland_or_others_dummy +  # permanent grassland (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[5]) * av_values_cor$mean_organic_d +                            # organic mixtures (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[6]) * av_values_cor$mean_overswoing_d +                         # over-sowing (no = 0, yes = 1) - at the average level
           as.numeric(reg_model$coefficients[7]) * as.numeric(count_shops[2,3]) +                        # weight of share of Eric Schweizer AG of all mixture
           as.numeric(reg_model$coefficients[8]) * as.numeric(count_shops[3,3]) +                        # weight of share of Rieger-Hofmann of all mixture
           as.numeric(reg_model$coefficients[9]) * as.numeric(count_shops[4,3]) +                        # weight of share of saaten-zeller of all mixture
           as.numeric(reg_model$coefficients[10]) * as.numeric(count_shops[5,3]) +                       # weight of share of saatgut-shop AG of all mixture
           as.numeric(reg_model$coefficients[11]) * as.numeric(count_shops[6,3]),                        # weight of share of UFA Samen AG of all mixture         Prediction = Prediction_x + average_values,
         Prediction = Prediction + average_values,
         CI_lower_90 = CI_lower_90 + average_values,
         CI_upper_90 = CI_upper_90 + average_values,
         CI_lower_95 = CI_lower_95 + average_values,
         CI_upper_95 = CI_upper_95 + average_values,
         CI_lower_99 = CI_lower_99 + average_values,
         CI_upper_99 = CI_upper_99 + average_values)



#-------------------------------------------------------------------------------
# 3.4.2 visualize predicted values
#-------------------------------------------------------------------------------

theme_set(theme_bw()) # set main theme of the plots
plot_predict_shan_ha <- 
  ggplot() +
  geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_99, ymax = CI_upper_99), stat = "identity", alpha = 1, color = "#000000", fill = "#deebf7") +
  geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_95, ymax = CI_upper_95), stat = "identity", alpha = 1, color = "#000000", fill = "#9ecae1") +
  #geom_smooth(data = pre_data_1, aes(x = shannon, y = Prediction, ymin = CI_lower_90, ymax = CI_upper_90), stat = "identity", alpha = 1, color = "#000000", fill = "#2171b5") +
  theme(axis.title = element_text(size=21),
        axis.text  = element_text(size=18),
        title  = element_text(size=21),
        legend.text = element_text(size=21),
        legend.title = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Price per ha (Euro; log)",limits = c(5,9.5), breaks = c(seq(5,9.5,.5))) +
  scale_x_continuous(name = "Shannon Index") +
  guides(linetype = "none", alpha = "none", color = "none", shape = "none")
plot_predict_shan_ha




#-------------------------------------------------------------------------------
# 3.4.3 store results in table 
#-------------------------------------------------------------------------------

table_boot_results_linear <- prediction_table(plant_div = "shan", number_coef = 11, col_number = 8, pred_data = pred_values, 
                                              conf_data = conf_values, table_boot_results = table_boot_results_linear)

table_boot_results_with_controls <- table_boot_results_linear # store results

# percentage confidence interval 
exp(conf_values[2,])-1 # species diversity
exp(conf_values[3,])-1 # native ecotypes
exp(conf_values[4,])-1 # permanent
exp(conf_values[5,])-1 # organic
exp(conf_values[6,])-1 # overseeding



##################--------------------------------------------------------------
# 3.5 combine plots
##################--------------------------------------------------------------


# create plot only for extracting a common legend
dat_legend <- pre_data_1 %>% dplyr::select(shannon, Prediction:CI_lower_99) %>% pivot_longer(!shannon, names_to = "type", values_to = "value") %>% 
  mutate(type = ifelse(str_detect(type, "99"),"99%-Confidence Band",ifelse(str_detect(type, "95"),"95%-Confidence Band",ifelse(str_detect(type, "90"),"90%-Confidence Band","Prediction"))))

plot_for_legend<-  ggplot() +
  geom_bar(data = dat_legend %>% filter(type != "Prediction" & type != "90%-Confidence Band" ), aes(x = factor(type), y = value, fill=factor(type)),  stat="identity") +
  geom_line(data = dat_legend %>% filter(type == "Prediction"), aes(x = factor(type), y = value, color=factor(type)), size = 2) +
  theme(axis.title = element_text(size=15),
        axis.text  = element_text(size=13),
        title  = element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "vertical",
        legend.box = "horizontal") +
  scale_fill_manual(values=c("#9ecae1", "#deebf7")) +
  scale_color_manual(values=c("#000000")) 

# obtain legend
legend <- get_legend(plot_for_legend)  


# combine plots

fig_1_x <- ggarrange(ggarrange(plot_predict_numb_ha + labs(tag = "A)") + theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm")),  
                               plot_predict_shan_ha + labs(tag = "B)") + theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm")),  
                               plot_predict_numb_kg + labs(tag = "C)") + theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm")),  
                               plot_predict_shan_kg + labs(tag = "D)") + theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm")),
                               ncol=2, nrow=2, align = "hv"),
                     as_ggplot(legend),
                     ncol=1, nrow=2,
                     heights = c(20, 3))


fig_price_diversity_with_controls <- fig_1_x





################################################################################
# 4. quadratic correlation model [with correlation for shop]
################################################################################



##################--------------------------------------------------------------
# 4.1 quadratic model - number of species + price per kg
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 4.1.1 run model and wild cluster bootstrapping
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_kg_ln ~ number_species + I(number_species^2) +
                  online_shop, data = data_2_short)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standarderrors and confidence levels (95%)
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)

vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]


#-------------------------------------------------------------------------------
# 4.1.2 store results in table 
#-------------------------------------------------------------------------------

# create data frame
table_boot_results <- data.frame(AME = c("Number of Species",
                                         "Shannon Index"),
                                 Price_kg_numb_without_controls = "", 
                                 Price_ha_numb_without_controls = "",
                                 Price_kg_shan_without_controls = "",
                                 Price_ha_shan_without_controls = "",
                                 Price_kg_numb_with_controls = "", 
                                 Price_ha_numb_with_controls = "",
                                 Price_kg_shan_with_controls = "",
                                 Price_ha_shan_with_controls = "",
                                 stringsAsFactors=FALSE)

m <- summary(margins(reg_model,  vcov = vcov_boot, variables = "number_species"))
table_boot_results[1,2] <-
  paste0(round(as.numeric(m[1,2]),2),
         ifelse(m[1,5]<0.001,"***",ifelse(m[1,5]<0.01,"**",ifelse(m[1,5]<0.05,"*",""))))



##################--------------------------------------------------------------
# 4.2 quadratic model - number of species + price per ha
##################--------------------------------------------------------------


#-------------------------------------------------------------------------------
# 4.2.1 run model and wild cluster bootstrapping
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_ha_ln ~ number_species + I(number_species^2) +
                  online_shop, data = data_2_short)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standarderrors and confidence levels (95%)
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)

vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]


#-------------------------------------------------------------------------------
# 4.2.3 store results in table 
#-------------------------------------------------------------------------------

m <- summary(margins(reg_model,  vcov = vcov_boot, variables = "number_species"))
table_boot_results[1,3] <-
  paste0(round(as.numeric(m[1,2]),2),
         ifelse(m[1,5]<0.001,"***",ifelse(m[1,5]<0.01,"**",ifelse(m[1,5]<0.05,"*",""))))





##################--------------------------------------------------------------
# 4.3 quadratic model - shannon + price per kg
##################--------------------------------------------------------------


#-------------------------------------------------------------------------------
# 4.3.1 run model and wild cluster bootstrapping
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_kg_ln ~ shannon + I(shannon^2) +
                  online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standarderrors and confidence levels (95%)
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)

vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]


#-------------------------------------------------------------------------------
# 4.3.3 store results in table 
#-------------------------------------------------------------------------------

m <- summary(margins(reg_model,  vcov = vcov_boot, variables = "shannon"))
table_boot_results[2,4] <-
  paste0(round(as.numeric(m[1,2]),2),
         ifelse(m[1,5]<0.001,"***",ifelse(m[1,5]<0.01,"**",ifelse(m[1,5]<0.05,"*",""))))



##################--------------------------------------------------------------
# 4.4 quadratic model - shannon + price per ha
##################--------------------------------------------------------------


#-------------------------------------------------------------------------------
# 4.4.1 run model and wild cluster bootstrapping
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_ha_ln ~ shannon + I(shannon^2) +
                  online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standarderrors and confidence levels (95%)
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)

vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]


#-------------------------------------------------------------------------------
# 4.4.3 store results in table 
#-------------------------------------------------------------------------------

m <- summary(margins(reg_model,  vcov = vcov_boot, variables = "shannon"))
table_boot_results[2,5] <-
  paste0(round(as.numeric(m[1,2]),2),
         ifelse(m[1,5]<0.001,"***",ifelse(m[1,5]<0.01,"**",ifelse(m[1,5]<0.05,"*",""))))







################################################################################
# 5. quadratic correlation model [with shops and other confounding variables]
################################################################################

##################--------------------------------------------------------------
# 5.1 quadratic model - number of species + price per kg
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 5.1.1 run model and wild cluster bootstrapping
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_kg_ln ~ number_species + I(number_species^2) +
                  native_and_permanent + permanent_grassland_or_others + organic_d + overswoing_d +
                  online_shop, data = data_2_short)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standarderrors and confidence levels (95%)
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)

vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]
vcov_boot[,9] <- boot_cov[,9]
vcov_boot[,10] <- boot_cov[,10]
vcov_boot[,11] <- boot_cov[,11]
vcov_boot[,12] <- boot_cov[,12]


#-------------------------------------------------------------------------------
# 5.1.2 store results in table 
#-------------------------------------------------------------------------------

m <- summary(margins(reg_model,  vcov = vcov_boot, variables = "number_species"))
table_boot_results[1,6] <-
  paste0(round(as.numeric(m[1,2]),2),
         ifelse(m[1,5]<0.001,"***",ifelse(m[1,5]<0.01,"**",ifelse(m[1,5]<0.05,"*",""))))



##################--------------------------------------------------------------
# 5.2 quadratic model - number of species + price per ha
##################--------------------------------------------------------------


#-------------------------------------------------------------------------------
# 5.2.1 run model and wild cluster bootstrapping
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_ha_ln ~ number_species + I(number_species^2) +
                  native_and_permanent + permanent_grassland_or_others + organic_d + overswoing_d +
                  online_shop, data = data_2_short)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standarderrors and confidence levels (95%)
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)

vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]
vcov_boot[,9] <- boot_cov[,9]
vcov_boot[,10] <- boot_cov[,10]
vcov_boot[,11] <- boot_cov[,11]
vcov_boot[,12] <- boot_cov[,12]


#-------------------------------------------------------------------------------
# 5.2.2 store results in table 
#-------------------------------------------------------------------------------

m <- summary(margins(reg_model,  vcov = vcov_boot, variables = "number_species"))
table_boot_results[1,7] <-
  paste0(round(as.numeric(m[1,2]),2),
         ifelse(m[1,5]<0.001,"***",ifelse(m[1,5]<0.01,"**",ifelse(m[1,5]<0.05,"*",""))))





##################--------------------------------------------------------------
# 5.3 quadratic model - shannon + price per kg
##################--------------------------------------------------------------


#-------------------------------------------------------------------------------
# 4.3.1 run model and wild cluster bootstrapping
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_kg_ln ~ shannon + I(shannon^2) +
                  native_and_permanent + permanent_grassland_or_others + organic_d + overswoing_d +
                  online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standarderrors and confidence levels (95%)
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)

vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]
vcov_boot[,9] <- boot_cov[,9]
vcov_boot[,10] <- boot_cov[,10]
vcov_boot[,11] <- boot_cov[,11]
vcov_boot[,12] <- boot_cov[,12]


#-------------------------------------------------------------------------------
# 5.3.2 store results in table 
#-------------------------------------------------------------------------------

m <- summary(margins(reg_model,  vcov = vcov_boot, variables = "shannon"))
table_boot_results[2,8] <-
  paste0(round(as.numeric(m[1,2]),2),
         ifelse(m[1,5]<0.001,"***",ifelse(m[1,5]<0.01,"**",ifelse(m[1,5]<0.05,"*",""))))



##################--------------------------------------------------------------
# 5.3 quadratic model - shannon + price per ha
##################--------------------------------------------------------------


#-------------------------------------------------------------------------------
# 5.3.1 run model and wild cluster bootstrapping
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_ha_ln ~ shannon + I(shannon^2) +
                  native_and_permanent + permanent_grassland_or_others + organic_d + overswoing_d +
                  online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standarderrors and confidence levels (95%)
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)

vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]
vcov_boot[,9] <- boot_cov[,9]
vcov_boot[,10] <- boot_cov[,10]
vcov_boot[,11] <- boot_cov[,11]
vcov_boot[,12] <- boot_cov[,12]


#-------------------------------------------------------------------------------
# 5.3.2 store results in table 
#-------------------------------------------------------------------------------

m <- summary(margins(reg_model,  vcov = vcov_boot, variables = "shannon"))
table_boot_results[2,9] <-
  paste0(round(as.numeric(m[1,2]),2),
         ifelse(m[1,5]<0.001,"***",ifelse(m[1,5]<0.01,"**",ifelse(m[1,5]<0.05,"*",""))))
table_marginal <- table_boot_results




################################################################################
# 6. linear correlation model [with correlation for shop] only for shannon and 
#     with all observations
################################################################################

##################--------------------------------------------------------------
# 6.1 shannon index + price per kg
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 6.1.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_kg_ln ~ shannon + online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]



#-------------------------------------------------------------------------------
# 6.2.1 store results in table 
#-------------------------------------------------------------------------------

# create data frame
table_boot_results <- data.frame(Estimate = c("Intercept",
                                              "Number of Species",
                                              "Shannon Index",
                                              "Online Shop = Eric Schweizer (CH)",
                                              "Online Shop = Rieger-Hofmann (DE)",
                                              "Online Shop = Saaten Zeller (DE)",
                                              "Online Shop = Saatgut Shop (DE)",
                                              "Online Shop = UFA Samen (CH)"), 
                                 Price_kg_shan = "",
                                 Price_kg_shan_percent = "",
                                 Price_ha_shan = "",
                                 Price_ha_shan_percent = "",
                                 stringsAsFactors=FALSE)

table_boot_results_linear <- prediction_table(plant_div = "shan", number_coef = 7, col_number = 2, pred_data = pred_values, 
                                              conf_data = conf_values, table_boot_results = table_boot_results)




##################--------------------------------------------------------------
# 6.2 shannon index + price per ha
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 6.2.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_ha_ln ~ shannon + online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]


#-------------------------------------------------------------------------------
# 6.2.1 store results in table 
#-------------------------------------------------------------------------------

table_boot_results_linear <- prediction_table(plant_div = "shan", number_coef = 7, col_number = 4, pred_data = pred_values, 
                                              conf_data = conf_values, table_boot_results = table_boot_results_linear)


table_boot_results_without_controls_sens <- table_boot_results_linear



################################################################################
# 7. linear correlation model [with shops and other confounding variables] only 
#     for shannon and with all observations
################################################################################

##################--------------------------------------------------------------
# 7.1 shannon index + price per kg
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 7.1.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_kg_ln ~ shannon + 
                  native_and_permanent + permanent_grassland_or_others + organic_d + overswoing_d +
                  online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]
vcov_boot[,9] <- boot_cov[,9]
vcov_boot[,10] <- boot_cov[,10]
vcov_boot[,11] <- boot_cov[,11]


#-------------------------------------------------------------------------------
# 7.1.2 store results in table 
#-------------------------------------------------------------------------------

# create data frame
table_boot_results <- data.frame(Estimate = c("Intercept",
                                              "Number of Species",
                                              "Shannon Index",
                                              "Ecotype (Native = 1, Not Native = 0)",
                                              "Grassland (Permanent = 1, Not Permanent = 0)",
                                              "Organic (Organic = 1, Conventional = 0)",
                                              "Oversowing (Yes = 1, No = 0)",
                                              "Online Shop = Eric Schweizer (CH)",
                                              "Online Shop = Rieger-Hofmann (DE)",
                                              "Online Shop = Saaten Zeller (DE)",
                                              "Online Shop = Saatgut Shop (DE)",
                                              "Online Shop = UFA Samen (CH)"), 
                                 Price_kg_shan = "",
                                 Price_kg_shan_percent = "",
                                 Price_ha_shan = "",
                                 Price_ha_shan_percent = "",
                                 stringsAsFactors=FALSE)

table_boot_results_linear <- prediction_table(plant_div = "shan", number_coef = 11, col_number = 2, pred_data = pred_values, 
                                              conf_data = conf_values, table_boot_results = table_boot_results)




##################--------------------------------------------------------------
# 7.2 shannon index + price per ha
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 7.2.1 run model and predict results 
#-------------------------------------------------------------------------------

# run model
reg_model <- lm(price_ha_ln ~ shannon + 
                  native_and_permanent + permanent_grassland_or_others + organic_d + overswoing_d +
                  online_shop, data = data_2_short_shan_cor)
# summary(reg_model) 

# adjust covariance matrix using wild cluster bootstrapping and compute standard errors and confidence levels 
boot_cov <- cluster.boot(model = reg_model, cluster =   ~ online_shop, R = n_boot, boot_type = "wild", wild_type = "norm")
pred_values <- coeftest(reg_model, vcov. = boot_cov)
conf_values <- coefci(reg_model, level = 0.95, vcov. = boot_cov)
vcov_boot <- stats::vcov(reg_model)
vcov_boot[,1] <- boot_cov[,1]
vcov_boot[,2] <- boot_cov[,2]
vcov_boot[,3] <- boot_cov[,3]
vcov_boot[,4] <- boot_cov[,4]
vcov_boot[,5] <- boot_cov[,5]
vcov_boot[,6] <- boot_cov[,6]
vcov_boot[,7] <- boot_cov[,7]
vcov_boot[,8] <- boot_cov[,8]
vcov_boot[,9] <- boot_cov[,9]
vcov_boot[,10] <- boot_cov[,10]
vcov_boot[,11] <- boot_cov[,11]


#-------------------------------------------------------------------------------
# 7.2.2 store results in table 
#-------------------------------------------------------------------------------

table_boot_results_linear <- prediction_table(plant_div = "shan", number_coef = 11, col_number = 4, pred_data = pred_values, 
                                              conf_data = conf_values, table_boot_results = table_boot_results_linear)

table_boot_results_with_controls_sens <- table_boot_results_linear






################################################################################
# 8. save figures and tables
################################################################################
# set working directory
setwd(wd_output)

# save
## figures
ggsave(fig_price_diversity_without_controls, filename = "v1_fig_s5.pdf", width=21, height=17.535/3*2)
ggsave(fig_price_diversity_with_controls,    filename = "v1_fig_s6s.pdf", width=21, height=17.535/3*2)

## tables
write.csv(table_boot_results_without_controls, file = "table_boot_results_without_controls.csv")
write.csv(table_boot_results_with_controls,    file = "table_boot_results_with_controls.csv")
write.csv(table_marginal,    file = "table_quadratic_margins.csv")
write.csv(table_boot_results_without_controls_sens, file = "table_boot_results_without_controls_sens.csv")
write.csv(table_boot_results_with_controls_sens,    file = "table_boot_results_with_controls_sens.csv")
