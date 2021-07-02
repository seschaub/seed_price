################################################################################
# R code related to                                                            #
# "The costs of diversity: higher prices for more diverse grassland seed       #
# mixtures" by Sergei Schaub, Robert Finger, Vera Steiner, Nina Buchmann,      #
# Valentin H. Klaus                                                            #
# -----------------------------------------------------------------------------#
# 2021, ETH Zurich                                                             #
# developed by Sergei Schaub and Vera Steiner                                  #
# -----------------------------------------------------------------------------#
# R script that runs the descriptive analysis, including the Upset approach    #
################################################################################

# table of contents:
# 1. main data adjustments
# 2. basic data visualization 
# 3. data summary
# 4. upset r
# 5. save figures and tables


################################################################################
# 1. main data adjustments
################################################################################

# if loaded manually and not directly from eth research collection 
if (!exists("data_1")) {
  setwd(wd_input)
  data_1 <- read.csv("seed_price_data.csv", header=TRUE, sep= ",", stringsAsFactors = F) %>% dplyr::select(-X)
} else{
  data_1 <- data_1 %>% dplyr::select(-X)
}

# reduce the dataset and re-order variables
data_1_short <- data_1 %>% 
  dplyr::select(seedmix_name:equal_seed,legumes_dummy,overswoing_d) %>% 
  mutate(native_ecotype = factor(native_ecotype, levels = c("No","Yes")),
         permanent_grassland_or_others = factor(permanent_grassland_or_others, levels = c("No","Yes")),
         organic_d = factor(organic_d, levels = c("No","Yes")),
         country = factor(country, levels = c("DE","CH")))   


################################################################################
# 2. basic data visualization - figure 2, s1, and s2 
################################################################################

theme_set(theme_bw()) # set main theme of the plots


##################--------------------------------------------------------------
# 2.1 figure 2: absolute frequencies of seed mixtures with a specific plant
#     diversity level 
##################--------------------------------------------------------------

fig_hist_eco_grass_numb <- data_1_short %>% mutate(native_grass = paste(native_ecotype, permanent_grassland_or_others, sep = " + ")) %>% 
  ggplot(aes(number_species, fill=native_grass))+
  geom_histogram( bins=30)+
  theme(axis.title = element_text(size=21),
        axis.text  = element_text(size=18),
        title  = element_text(size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        legend.position = c(0.8,0.84),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Absolute Frequency", breaks = c(0,25,50,75), limits = c(0,80)) +
  scale_x_continuous(name = "Number of Species", breaks =  seq(0,60,10)) +
  scale_fill_viridis(option="D",discrete = TRUE,direction = 1, name = "Native Ecotype + \nPermanent Grassland Use:", guide = guide_legend(reverse=TRUE))
# fig_hist_eco_grass_numb

fig_hist_eco_grass_shan <- data_1_short %>% filter(equal_seed == 0) %>% 
  mutate(native_grass = paste(native_ecotype, permanent_grassland_or_others, sep = " + ")) %>% 
  ggplot(aes(shannon, fill=native_grass))+
  geom_histogram( bins=30)+
  theme(axis.title = element_text(size=21),
        axis.text  = element_text(size=18),
        title  = element_text(size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_y_continuous(name = "Absolute Frequency", breaks = c(0,25,50,75), limits = c(0,80)) +
  scale_x_continuous(name = "Shannon Index", breaks =  seq(0,4,1)) +
  scale_fill_viridis(option="D",discrete = TRUE,direction = 1, name = "Native Ecotype + \nPermanent Grassland Use:", guide = guide_legend(reverse=TRUE))
#fig_hist_eco_grass_shan

# combine plots
fig_hist_eco_grass <- ggarrange(fig_hist_eco_grass_numb + labs(tag = "A)") + theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm")), 
                            fig_hist_eco_grass_shan     + labs(tag = "B)") + theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm")),
                            align = "hv") 
# fig_hist_eco_grass


##################--------------------------------------------------------------
# 2.2 figure 3: seed mixture price per ha and plant diversity with an indication 
#     of native ecotypes
##################--------------------------------------------------------------

# set colors 
colors <- scico(3, palette = 'batlow')
color_eco_y <- colors[1]
color_eco_n <- colors[2]

data_1_short <- data_1_short %>% 
  mutate(native_ecotype = factor(native_ecotype, levels = c("Yes","No")))

#-------------------------------------------------------------------------------
# 2.2.1 number of species
#-------------------------------------------------------------------------------

# price per kg
p1_numb <-   ggplot() + 
  geom_point(data=data_1_short,aes(number_species,price_kg,color=native_ecotype, shape=native_ecotype), alpha = 0.5, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Number of Species")+
  ylab("Price per kg (Euro)")+
  scale_color_manual(values = c(color_eco_y,color_eco_n), name = "Native Ecotype:")+
  scale_shape_manual(values = c(15,19), name = "Native Ecotype:") + 
  scale_y_continuous(trans = log_trans(), breaks = c(3, 10,30,100), limits =  c(2.8,160))  + 
  geom_smooth(data=data_1_short,aes(x = number_species, y = price_kg), method = "lm", formula = y ~ x, se = FALSE, color = "black") 


# price per ha
p2_numb <- ggplot()+
  geom_point(data=data_1_short, aes(number_species,price_ha,color=native_ecotype, shape=native_ecotype), alpha = 0.5, size = 3.5)+
  geom_smooth(data=data_1_short,aes(x = number_species, y = price_ha), method = "lm", formula = y ~ x, se = FALSE, color = "black") + 
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Number of Species")+
  ylab("Price per ha (Euro)")+
  scale_color_manual(values = c(color_eco_y,color_eco_n), name = "Native Ecotype:")+
  scale_shape_manual(values = c(15,19), name = "Native Ecotype:") + 
  scale_y_continuous(trans = log_trans(), breaks = c(100, 300, 1000, 3000), limits =  c(80, 6000)) +
  coord_cartesian(ylim=c(80, 5400))


#-------------------------------------------------------------------------------
# 2.2.2 shannon index
#-------------------------------------------------------------------------------

# price per kg
p1_shannon <-   ggplot() + 
  geom_point(data=data_1_short %>%  filter(equal_seed == 0), aes(shannon,price_kg,color=native_ecotype, shape=native_ecotype), alpha = 0.5, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Shannon Index")+
  ylab("Price per kg (Euro)")+
  scale_color_manual(values = c(color_eco_y,color_eco_n), name = "Native Ecotype:")+
  scale_shape_manual(values = c(15,19), name = "Native Ecotype:") + 
  scale_y_continuous(trans = log_trans(), breaks = c(3, 10,30,100), limits =  c(2.8,160))  + 
  geom_smooth(data=data_1_short,aes(x = shannon, y = price_kg), method = "lm", formula = y ~ x, se = FALSE, color = "black") 

# price per ha
p2_shannon <- ggplot()+
  geom_point(data=data_1_short %>%  filter(equal_seed == 0), aes(shannon,price_ha,color=native_ecotype, shape=native_ecotype), alpha = 0.5, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Shannon Index")+
  ylab("Price per ha (Euro)")+
  scale_color_manual(values = c(color_eco_y,color_eco_n), name = "Native Ecotype:")+
  scale_shape_manual(values = c(15,19), name = "Native Ecotype:") +
  scale_y_continuous(trans = log_trans(), breaks = c(100, 300, 1000, 3000), limits =  c(80, 6000)) +
  coord_cartesian(ylim=c(80, 5400)) +
  geom_smooth(data=data_1_short,aes(x = shannon, y = price_ha), method = "lm", formula = y ~ x, se = FALSE, color = "black")


#-------------------------------------------------------------------------------
# 2.2.3 combine plots
#-------------------------------------------------------------------------------

# price per ha
fig_2_price_eco_ha <- ggarrange(p2_numb    + labs(tag = "A)") + 
                                  theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"),
                                        legend.position = c(0, 1), 
                                        legend.justification = c(0, 1),
                                        legend.background = element_rect(colour = NA, fill = NA)),
                            p2_shannon + labs(tag = "B)") + 
                              theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"),
                                    legend.position = "none")) 
# fig_2_price_eco_ha

# price per kg
fig_2_price_eco_kg<- ggarrange(p1_numb    + labs(tag = "A)") +
                                 theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"),
                                       legend.position = c(0, 1), 
                                       legend.justification = c(0, 1),
                                       legend.background = element_rect(colour = NA, fill = NA)),
                           p1_shannon + labs(tag = "B)") + 
                             theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"),
                                   legend.position = "none")) 
# fig_2_price_eco_kg



##################--------------------------------------------------------------
# 2.3 figure s1: seed mixture price per ha and plant diversity with an indication 
#     of native ecotypes and organic certification
##################--------------------------------------------------------------

# set colors 
colors <- scico(2, palette = 'roma')
color_org_y <- colors[1]
color_org_n <- colors[2]

data_1_short <- data_1_short %>% 
  mutate(organic_d = factor(organic_d, levels = c("Yes","No")))

#-------------------------------------------------------------------------------
# 2.3.1 number of species
#-------------------------------------------------------------------------------

# price per kg
p1_numb <-   ggplot() + 
  geom_point(data=data_1_short,aes(number_species,price_kg,color=organic_d, shape=native_ecotype), alpha = 0.5, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Number of Species")+
  ylab("Price per kg (Euro)")+
  scale_color_manual(values = c(color_org_y,color_org_n), name = "Organic:")+
  scale_shape_manual(values = c(15,19), name = "Native Ecotype:") + 
  scale_y_continuous(trans = log_trans(), breaks = c(3, 10,30,100), limits =  c(2.8,160))  + 
  geom_smooth(data=data_1_short,aes(x = number_species, y = price_kg), method = "lm", formula = y ~ x, se = FALSE, color = "black") 

# price per ha
p2_numb <- ggplot()+
  geom_point(data=data_1_short, aes(number_species,price_ha,color=organic_d, shape=native_ecotype), alpha = 0.5, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Number of Species")+
  ylab("Price per ha (Euro)")+
  scale_color_manual(values = c(color_org_y,color_org_n), name = "Organic:")+
  scale_shape_manual(values = c(15,19), name = "Native Ecotype:") + 
  scale_y_continuous(trans = log_trans(), breaks = c(100, 300, 1000, 3000), limits =  c(80, 6000)) +
  coord_cartesian(ylim=c(80, 5400)) +
  geom_smooth(data=data_1_short,aes(x = number_species, y = price_ha), method = "lm", formula = y ~ x, se = FALSE, color = "black")


#-------------------------------------------------------------------------------
# 2.3.2 shannon index
#-------------------------------------------------------------------------------

# price per kg
p1_shannon <-   ggplot() + 
  geom_point(data=data_1_short %>%  filter(equal_seed == 0), aes(shannon,price_kg,color=organic_d, shape=native_ecotype), alpha = 0.5, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Shannon Index")+
  ylab("Price per kg (Euro)")+
  scale_color_manual(values = c(color_org_y,color_org_n), name = "Organic:")+
  scale_shape_manual(values = c(15,19), name = "Native Ecotype:") + 
  scale_y_continuous(trans = log_trans(), breaks = c(3, 10,30,100), limits =  c(2.8,160))  + 
  geom_smooth(data=data_1_short,aes(x = shannon, y = price_kg), method = "lm", formula = y ~ x, se = FALSE, color = "black") 

# price per ha
p2_shannon <- ggplot()+
  geom_point(data=data_1_short %>%  filter(equal_seed == 0), aes(shannon,price_ha,color=organic_d, shape=native_ecotype), alpha = 0.5, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Shannon Index")+
  ylab("Price per ha (Euro)")+
  scale_color_manual(values = c(color_org_y,color_org_n), name = "Organic:")+
  scale_shape_manual(values = c(15,19), name = "Native Ecotype:") +
  scale_y_continuous(trans = log_trans(), breaks = c(100, 300, 1000, 3000), limits =  c(80, 6000)) +
  coord_cartesian(ylim=c(80, 5400)) +
  geom_smooth(data=data_1_short,aes(x = shannon, y = price_ha), method = "lm", formula = y ~ x, se = FALSE, color = "black")


#-------------------------------------------------------------------------------
# 2.3.3 combine plots
#-------------------------------------------------------------------------------

# all
fig_2_both_org <- ggarrange(p2_numb    + labs(tag = "A)") + 
                              theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"),
                                    legend.position = c(0, 1), 
                                    legend.justification = c(0, 1),
                                    legend.background = element_rect(colour = NA, fill = NA),
                                    legend.direction = "horizontal"),
                            p1_numb    + labs(tag = "B)") +  theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"),
                                                                   legend.position = "none"), 
                            p2_shannon + labs(tag = "C)") +  theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"),
                                                                   legend.position = "none"), 
                            p1_shannon + labs(tag = "D)") +  theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm"),
                                                                   legend.position = "none")) 
# fig_2_both_org


##################--------------------------------------------------------------
# 2.4 figure s2: seed mixture price per ha and plant diversity with an indication 
#     of online shops
##################--------------------------------------------------------------

# set colours
colors6 <- viridis(6)
colors6[5] <- viridis(6)[6]
colors6[6] <- viridis(6)[5]

data_1_short <- data_1_short%>%
  mutate(price_ha = price_kg * kg_area,    # compute per area price
         `Online Shop` = ifelse(online_shop == "agrarshop-online","Agrarshop Online (DE)",
                                ifelse(online_shop == "Rieger-Hofmann","Rieger Hofmann (DE)",
                                       ifelse(online_shop == "Eric Schweizer AG","Eric Schweizer (CH)",
                                              ifelse(online_shop == "saaten-zeller","Saaten Zeller (DE)",
                                                     ifelse(online_shop == "saatgut-shop","Saatgut Shop (DE)",
                                                            ifelse(online_shop == "UFA Samen","UFA Samen (CH)",online_shop)))))))

#-------------------------------------------------------------------------------
# 2.4.1 number of species
#-------------------------------------------------------------------------------

# price per kg
p1_numb <-   ggplot() + 
  geom_point(data=data_1_short,aes(number_species,price_kg,color=`Online Shop`, shape=`Online Shop`), alpha = 0.7, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Number of Species")+
  ylab("Price per kg (Euro)")+
  scale_color_viridis(discrete = TRUE, name = "Online Shop:")+
  scale_shape_manual(values = c(19,15,19,19,19,15), name = "Online Shop:") +
  scale_y_continuous(trans = log_trans(), breaks = c(3, 10,30,100), limits =  c(2.8,160))  + 
  geom_smooth(data=data_1_short,aes(x = number_species, y = price_kg), method = "lm", formula = y ~ x, se = FALSE, color = "black") 

# price per ha
p2_numb <- ggplot()+
  geom_point(data=data_1_short, aes(number_species,price_ha,color=`Online Shop`, shape=`Online Shop`), alpha = 0.7, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Number of Species")+
  ylab("Price per ha (Euro)")+
  scale_color_viridis(discrete = TRUE, name = "Online Shop:")+
  scale_shape_manual(values = c(19,15,19,19,19,15), name = "Online Shop:") +
  scale_y_continuous(trans = log_trans(), breaks = c(100, 300, 1000, 3000), limits =  c(80, 6000)) +
  coord_cartesian(ylim=c(80, 5400)) +
  geom_smooth(data=data_1_short,aes(x = number_species, y = price_ha), method = "lm", formula = y ~ x, se = FALSE, color = "black")


#-------------------------------------------------------------------------------
# 2.4.2 shannon index
#-------------------------------------------------------------------------------

# price per kg
p1_shannon <-   ggplot() + 
  geom_point(data=data_1_short %>%  filter(equal_seed == 0),aes(shannon,price_kg,color=`Online Shop`, shape=`Online Shop`), alpha = 0.7, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Shannon Index")+
  ylab("Price per kg (Euro)")+
  scale_color_viridis(discrete = TRUE, name = "Online Shop:")+
  scale_shape_manual(values = c(19,15,19,19,19,15), name = "Online Shop:") +
  scale_y_continuous(trans = log_trans(), breaks = c(3, 10,30,100), limits =  c(2.8,160))  + 
  geom_smooth(data=data_1_short,aes(x = shannon, y = price_kg), method = "lm", formula = y ~ x, se = FALSE, color = "black") 

# price per ha
p2_shannon <- ggplot()+
  geom_point(data=data_1_short %>%  filter(equal_seed == 0), aes(shannon,price_ha,color=`Online Shop`, shape=`Online Shop`), alpha = 0.7, size = 3.5)+
  theme(axis.title = element_text( size=21),
        axis.text  = element_text( size=18),
        title  = element_text( size=21),
        legend.text = element_text(size=21),
        legend.title = element_text(size=21),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Shannon Index")+
  ylab("Price per ha (Euro)")+
  scale_color_viridis(discrete = TRUE, name = "Online Shop:")+
  scale_shape_manual(values = c(19,15,19,19,19,15), name = "Online Shop:") +
  scale_y_continuous(trans = log_trans(), breaks = c(100, 300, 1000, 3000), limits =  c(80, 6000)) +
  coord_cartesian(ylim=c(80, 5400)) +
  geom_smooth(data=data_1_short,aes(x = shannon, y = price_ha), method = "lm", formula = y ~ x, se = FALSE, color = "black")


#-------------------------------------------------------------------------------
# 2.4.3 combine plots
#-------------------------------------------------------------------------------

fig_2_both_shop <- ggarrange(p2_numb    + labs(tag = "A)") + theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm")), 
                             p1_numb    + labs(tag = "B)") + theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm")),
                             p2_shannon + labs(tag = "C)") + theme(plot.margin = margin(0.1,0.5,0.1,0.1, "cm")), 
                             p1_shannon + labs(tag = "D)") + theme(plot.margin = margin(0.1,0.1,0.1,0.5, "cm")),
                             common.legend = TRUE, legend = "bottom") 
# fig_2_both_shop


################################################################################
# 3. data summary
################################################################################

##################--------------------------------------------------------------
# 3.1 range of plant diversity measures
##################--------------------------------------------------------------

min(data_1_short$number_species)
max(data_1_short$number_species)
min(data_1_short$shannon)
max(data_1_short$shannon)


##################--------------------------------------------------------------
# 3.2 overview of shops, seed mixtures, and species 
##################--------------------------------------------------------------

summary_overview <- data.frame(Variable = c("Number of Shops", "Number of Seed Mixtures", "Number of Different Species"), Total = 0, DE = 0, CH = 0)

# summarize number of shops 
summary_overview[1,2] <- data_1_short %>% dplyr::select(country,online_shop) %>% distinct() %>% summarise(N = n()) %>% as.numeric()
summary_overview[1,3] <- (data_1_short %>% dplyr::select(country,online_shop) %>% distinct() %>% group_by(country) %>% summarise(N = n()))[1,2] %>% as.numeric()
summary_overview[1,4] <- (data_1_short %>% dplyr::select(country,online_shop) %>% distinct() %>% group_by(country) %>% summarise(N = n()))[2,2] %>% as.numeric()
# summarize number of mixtures 
summary_overview[2,2] <- data_1_short %>% distinct() %>% summarise(N = n()) %>% as.numeric()
summary_overview[2,3] <- (data_1_short %>% distinct() %>% group_by(country) %>% summarise(N = n()))[1,2] %>% as.numeric()
summary_overview[2,4] <- (data_1_short %>% distinct() %>% group_by(country) %>% summarise(N = n()))[2,2] %>% as.numeric()
# summarize number of different species 
dif_aux <- data_1 %>% dplyr::select(Achillea.millefolium:Triticum.aestivum,-grasses) 
dif_aux[dif_aux > 0 ] <- 1
dif_aux <- bind_cols(data_1 %>% dplyr::select(country), dif_aux)

dif_seed_t  <- dif_aux %>% dplyr::select(-country) %>% colSums( na.rm = TRUE) %>% as.data.frame() %>% 
  rename(species = 1) %>% mutate(species = ifelse(species > 0, 1, 0)) %>% colSums( na.rm = TRUE)

dif_seed_de <- dif_aux %>% filter(country == "DE") %>%  dplyr::select(-country) %>% colSums( na.rm = TRUE) %>% as.data.frame() %>% 
  rename(species = 1) %>% mutate(species = ifelse(species > 0, 1, 0)) %>% colSums( na.rm = TRUE)

dif_seed_ch <- dif_aux %>% filter(country == "CH") %>%  dplyr::select(-country) %>% colSums( na.rm = TRUE) %>% as.data.frame() %>% 
  rename(species = 1) %>% mutate(species = ifelse(species > 0, 1, 0)) %>% colSums( na.rm = TRUE)

summary_overview[3,2] <- dif_seed_t  %>% as.numeric()
summary_overview[3,3] <- dif_seed_de %>% as.numeric()
summary_overview[3,4] <- dif_seed_ch %>% as.numeric()


##################--------------------------------------------------------------
# 3.3 summary statistic
##################--------------------------------------------------------------

summary_stats <- data.frame(Variable = c("Price per Kg", "Price per Ha", "Number of Species", "Shannon Index"), Total = 0, DE = 0, CH = 0)


stats_all <- data_1_short %>% 
  summarise(meanPrice_kg      = mean(price_kg, na.rm = TRUE),
            sdPrice_kg        = sd(price_kg, na.rm = TRUE),
            meanPrice_ha      = mean(price_ha, na.rm = TRUE),
            sdPrice_ha        = sd(price_ha, na.rm = TRUE),
            meannumber_species = mean(number_species, na.rm = TRUE),
            sdnumber_species   = sd(number_species, na.rm = TRUE),
            meanshannon       = mean(shannon, na.rm = TRUE),
            sdshannon         = sd(shannon, na.rm = TRUE))

stats_all_reduced <- data_1_short %>% filter(equal_seed == 0) %>% 
  summarise(meanPrice_kg      = mean(price_kg, na.rm = TRUE),
            sdPrice_kg        = sd(price_kg, na.rm = TRUE),
            meanPrice_ha      = mean(price_ha, na.rm = TRUE),
            sdPrice_ha        = sd(price_ha, na.rm = TRUE),
            meannumber_species = mean(number_species, na.rm = TRUE),
            sdnumber_species   = sd(number_species, na.rm = TRUE),
            meanshannon       = mean(shannon, na.rm = TRUE),
            sdshannon         = sd(shannon, na.rm = TRUE))

stats_country <- data_1_short %>% group_by(country) %>% 
  summarise(meanPrice_kg      = mean(price_kg, na.rm = TRUE),
            sdPrice_kg        = sd(price_kg, na.rm = TRUE),
            meanPrice_ha      = mean(price_ha, na.rm = TRUE),
            sdPrice_ha        = sd(price_ha, na.rm = TRUE),
            meannumber_species = mean(number_species, na.rm = TRUE),
            sdnumber_species   = sd(number_species, na.rm = TRUE),
            meanshannon       = mean(shannon, na.rm = TRUE),
            sdshannon         = sd(shannon, na.rm = TRUE))

stats_country_reduced <- data_1_short %>% group_by(country) %>% filter(equal_seed == 0) %>%
  summarise(meanPrice_kg      = mean(price_kg, na.rm = TRUE),
            sdPrice_kg        = sd(price_kg, na.rm = TRUE),
            meanPrice_ha      = mean(price_ha, na.rm = TRUE),
            sdPrice_ha        = sd(price_ha, na.rm = TRUE),
            meannumber_species = mean(number_species, na.rm = TRUE),
            sdnumber_species   = sd(number_species, na.rm = TRUE),
            meanshannon       = mean(shannon, na.rm = TRUE),
            sdshannon         = sd(shannon, na.rm = TRUE))

summary_stats[1,2] <- paste0(round(stats_all$meanPrice_kg,2)," (",round(stats_all$sdPrice_kg,2),")")
summary_stats[2,2] <- paste0(round(stats_all$meanPrice_ha,2)," (",round(stats_all$sdPrice_ha,2),")")
summary_stats[3,2] <- paste0(round(stats_all$meannumber_species,2)," (",round(stats_all$sdnumber_species,2),")")
summary_stats[4,2] <- paste0(round(stats_all_reduced$meanshannon,2)," (",round(stats_all_reduced$sdshannon,2),")")

summary_stats[1,3] <- paste0(round(stats_country$meanPrice_kg[2],2)," (",round(stats_country$sdPrice_kg[2],2),")")
summary_stats[2,3] <- paste0(round(stats_country$meanPrice_ha[2],2)," (",round(stats_country$sdPrice_ha[2],2),")")
summary_stats[3,3] <- paste0(round(stats_country$meannumber_species[2],2)," (",round(stats_country$sdnumber_species[2],2),")")
summary_stats[4,3] <- paste0(round(stats_country_reduced$meanshannon[2],2)," (",round(stats_country_reduced$sdshannon[2],2),")")

summary_stats[1,4] <- paste0(round(stats_country$meanPrice_kg[1],2)," (",round(stats_country$sdPrice_kg[1],2),")")
summary_stats[2,4] <- paste0(round(stats_country$meanPrice_ha[1],2)," (",round(stats_country$sdPrice_ha[1],2),")")
summary_stats[3,4] <- paste0(round(stats_country$meannumber_species[1],2)," (",round(stats_country$sdnumber_species[1],2),")")
summary_stats[4,4] <- paste0(round(stats_country_reduced$meanshannon[1],2)," (",round(stats_country_reduced$sdshannon[1],2),")")


##################--------------------------------------------------------------
# 3.4 mixtures with legumes
##################--------------------------------------------------------------

data_1_short %>% group_by(legumes_dummy) %>% summarise(N_legumes = n()) # overall 
type_mixture_no_legume <- data_1_short %>% filter(legumes_dummy == "no") %>% group_by(description_others) %>% summarise(N_legumes = n()) # by use without legumes
type_mixture <- data_1_short %>%  group_by(description_others) %>% summarise(N = n()) # by use all mixtures
data_1_short %>%   group_by(legumes_dummy, country) %>% summarise(N_legumes = n()) # by country
data_1_short %>% filter(number_species== 1) %>% group_by(description_others) %>%  summarize(N = n()) # with only one species

# table s5
table( cut(data_1_short$number_species, b = c(0,10,20,30,40,50,100)))
table( cut(data_1_short$number_species, b = c(0,1.1,2.1,3.1,4.1,5.1,6.1,7.1,8.1,9.1,10.1,11.1,100)))
table( cut(data_1_short$shannon, b = c(-1,0.5,1,1.5,2,2.5,3,100)))


##################--------------------------------------------------------------
# 3.5 table 1: overview of the main characteristics of the seed mixtures.
##################--------------------------------------------------------------

table_des <- data_1_short %>% mutate(native_ecotype_d = (ifelse(native_ecotype == "Yes",1,0)),
                        permanent_grassland_or_others = (ifelse(permanent_grassland_or_others == "Yes",1,0)),
                        organic_d = (ifelse(organic_d == "Yes",1,0))) %>% 
  summarize(mean_price_ha = mean(price_ha),
            sd_price_ha = sd(price_ha),
            mean_price_kg = mean(price_kg),
            sd_price_kg = sd(price_kg),
            mean_density = mean(kg_area),
            sd_density = sd(kg_area),
            mean_number = mean(number_species),
            sd_number = sd(number_species),
            mean_native_ecotype = mean(native_ecotype_d),
            sd_native_ecotype = sd(native_ecotype_d),
            mean_permanent_grassland_or_others = mean(permanent_grassland_or_others),
            sd_permanent_grassland_or_others = sd(permanent_grassland_or_others),
            mean_organic_d = mean(organic_d),
            sd_organic_d = sd(organic_d),
            mean_overswoing_d = mean(overswoing_d),
            sd_overswoing_d = sd(overswoing_d),
            N = n()) 


data_1_short %>%  filter(equal_seed == 0) %>% 
  summarize(mean_shannon = mean(shannon),
            sd_shannon = sd(shannon),
            N = n())

data_1_short %>% mutate(native_ecotype_d = (ifelse(native_ecotype == "Yes",1,0)),
                        permanent_grassland_or_others = (ifelse(permanent_grassland_or_others == "Yes",1,0)),
                        organic_d = (ifelse(organic_d == "Yes",1,0))) %>%
  group_by(native_ecotype_d) %>% 
  summarize(N_native_ecotype = n())
data_1_short %>% mutate(native_ecotype_d = (ifelse(native_ecotype == "Yes",1,0)),
                        permanent_grassland_or_others = (ifelse(permanent_grassland_or_others == "Yes",1,0)),
                        organic_d = (ifelse(organic_d == "Yes",1,0))) %>%
  group_by(permanent_grassland_or_others) %>% 
  summarize(N_permanent_grassland_or_others = n()) 

data_1_short %>% mutate(native_ecotype_d = (ifelse(native_ecotype == "Yes",1,0)),
                        permanent_grassland_or_others = (ifelse(permanent_grassland_or_others == "Yes",1,0)),
                        organic_d = (ifelse(organic_d == "Yes",1,0))) %>%
  group_by(organic_d) %>% 
  summarize(N_organic_d = n())

data_1_short %>% mutate(native_ecotype_d = (ifelse(native_ecotype == "Yes",1,0)),
                        permanent_grassland_or_others = (ifelse(permanent_grassland_or_others == "Yes",1,0)),
                        organic_d = (ifelse(organic_d == "Yes",1,0))) %>%
  group_by(overswoing_d) %>% 
  summarize(N_overswoing_d = n())


################################################################################
# 4. upset r
################################################################################

##################--------------------------------------------------------------
# 4.1 figure 1: seed mixture composition and their relationship with prices
##################--------------------------------------------------------------

#-------------------------------------------------------------------------------
# 4.1.1 upper part - upset r
#-------------------------------------------------------------------------------

dat1_aux2 <- data_1

n_loop <- which(colnames(dat1_aux2)=="Achillea.millefolium")
start_loop <- which(colnames(dat1_aux2)=="Triticum.aestivum")


# the loop creates new column for each species with 0 or 1
for(i in start_loop:n_loop){
  dat1_aux2[is.na(dat1_aux2[,i]),i] <- 0
  dat1_aux2$seed1<- ifelse(dat1_aux2[ ,i]==0,0,1)
  colname <- paste(colnames(dat1_aux2)[i],"new",sep = "_")
  colnames(dat1_aux2)[length(dat1_aux2)] <- colname}

# create new datasat with only the new columns
dat2 <-  dat1_aux2 %>% dplyr::select(seedmix_name, ends_with("new"))

# calculate the frequency of the varieties
frequency <- dat2 %>% gather(`Achillea.millefolium_new`:`Triticum.aestivum_new`, value = "value", key = "name", -seedmix_name) %>% as.data.frame() %>% 
  group_by(name) %>% summarise(sum = sum(value, na.rm = T)) %>% 
  filter(sum>0)  %>%  #exclude varietis with value 0
  arrange(desc(sum))


# design upset with the 20 most frequent species
# define the less frequent species as others
top_20 <- frequency %>% top_n(20, sum) %>% dplyr::select(name)
aux_others <- dat2 %>% dplyr::select(-c(as.vector(top_20$name),"seedmix_name"))

aux_others$others <- rowSums(aux_others)
aux_others$others_new <- ifelse(aux_others[,"others"]>0,1,0)

#create dataset for upset
dat3 <- cbind(dat2,aux_others$others_new,data_1$price_kg,data_1$price_ha) %>% 
  rename(`Price per Kg` = `data_1$price_kg`,
         `Price per Ha` = `data_1$price_ha`,
         `Other Species` = `aux_others$others_new`)

# upset with boxplot.summary
colnames(dat3) <- sub("\\.", " ", colnames(dat3))
colnames(dat3) <- sub("\\_new", "", colnames(dat3))
dat3 <- dat3 %>% rename('Lolium hybridum' = 'Lolium x.hybridum', 
                        "Lolium perenne" = "Lolium Perenne")


upset_plot <- upset(dat3,nsets=21, mb.ratio = c(0.5, 0.5),order.by = "freq", decreasing = c(TRUE,FALSE),
                    mainbar.y.label = "Species Combination\nAbsolute Frequency",
                    sets.x.label = "Species Absolute\nFrequency",
                    point.size = 3.5, line.size = 1.2,
                    text.scale = 2,
                    nintersects = 60) # only include mixtures with at least 2 observations


##################--------------------------------------------------------------
# 4.1.2 lower part - boxplot
##################--------------------------------------------------------------

# order boxplot accordingly to upset presentation:
dat3x <- dat3 %>%  dplyr::select(`Lolium perenne`,
                                 `Poa pratensis`,
                                 `Trifolium repens`,
                                 `Trifolium pratense`,
                                 `Phleum pratense`,
                                 `Festuca rubra`,
                                 `Festuca pratensis`,
                                 `Dactylis glomerata`, 
                                 `Other Species`,
                                 `Lolium multiflorum`,
                                 `Cynosurus cristatus`,
                                 `Lotus corniculatus`,
                                 `Lolium hybridum`,
                                 `Alopecurus pratensis`,
                                 `Medicago sativa`,
                                 `Achillea millefolium`,
                                 `Plantago lanceolata`,
                                 `Anthoxanthum odoratum`,
                                 `Trifolium alexandrinum`,
                                 `Trisetum flavescens`,
                                 `Centaurea jacea`,
                                 `Price per Kg`,`Price per Ha`) %>% 
  unite( ID, -c(`Price per Kg`,`Price per Ha`), sep = "", remove = T)

check2 <- dat3x %>%   group_by(ID) %>% summarise(N = n())

dat3x <- left_join(dat3x, check2, by = "ID" ) %>% filter(N > 1) %>% arrange(desc(N))

xx <- dat3x %>% dplyr::select(ID,N) %>% 
  mutate(ID2 = as.numeric(ID),
         ID2 = format(ID2,scientific=FALSE)) %>% distinct() %>% 
  arrange(desc(N), (ID2))

print(xx %>% dplyr::select(ID,N))

dat3x <- dat3x %>% 
  mutate(ID2 = factor(ID, 
                      levels = c(
                        "111111000000000000000", # 14
                        "100000000000000000000", # 10
                        "111111010000000000000", #  8
                        "000000001100000000100", #  7
                        "101110110000000000000", #  7
                        "111000000000000000000", #  6
                        "000000000100001000000", #  5
                        "000000000100100000000", #  4
                        "000100000100000000000", #  4
                        "101110110100000000100", #  4
                        "111111110000000000010", #  4
                        "000000001100000000000", #  3
                        "000100000100000000100", #  3
                        "000100001100000000000", #  3
                        "001100010100000000000", #  3
                        "101000000000000000000", #  3
                        "101100000100100000000", #  3
                        "101111100000000000000", #  3
                        "111001010000000000000", #  3
                        "111001100000010000000", #  3
                        "111010000000000000000", #  3
                        "111100000000100000000", #  3
                        "111100000100100000000", #  3
                        "111111000000000000100", #  3
                        "111111010000000000100", #  3
                        "000000000000001000000", #  2
                        "000001001011000101011", #  2
                        "000100000100100000000", #  2
                        "000110110000001000000", #  2
                        "010001111011000000010", #  2
                        "100001001011000111001", #  2
                        "100010000000101000000", #  2
                        "100010110000000000000", #  2
                        "100110000000101000000", #  2
                        "100110100000001000000", #  2
                        "100110100000100000000", #  2
                        "101011100000010000000", #  2
                        "101100100100100000000", #  2
                        "101110100000000000000", #  2
                        "101110110000001000000", #  2
                        "101110110000100000000", #  2
                        "110000000000000000000", #  2
                        "110010100000000000000", #  2
                        "110011000000000000000", #  2
                        "110011001010000000000", #  2
                        "110011010000000000000", #  2
                        "110100010000000000000", #  2
                        "111000000000010000000", #  2
                        "111000000100100000000", #  2
                        "111000001000000000000", #  2
                        "111000010000010000000", #  2
                        "111000011000000000000", #  2
                        "111001010000010000000", #  2
                        "111011001010000000000", #  2
                        "111011111011010000000", #  2
                        "111101001010000000000", #  2
                        "111110111000000000000", #  2
                        "111111000000000010000", #  2
                        "111111000100000000000", #  2
                        "111111110010010000000")))  #  2

                        
                       
theme_set(theme_bw()) # set main theme of the plots

# plot
bb_price_ha <- dat3x %>% ggplot()+
  geom_boxplot(aes(x=factor(ID2), y=`Price per Ha`))+
  theme(axis.title = element_text( size=16),
        axis.text  = element_text( size=16),
        title  = element_text( size=16),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position="none")+
  scale_y_continuous(name = "Price per ha (Euro)")
  


##################--------------------------------------------------------------
# 4.2 figure s3: german online shops – seed mixture composition and their 
#                relationship with prices
##################--------------------------------------------------------------

dat1_de <- data_1 %>% filter(country == "DE")
dat1_aux2 <- dat1_de

n_loop <- which(colnames(dat1_aux2)=="Achillea.millefolium")
start_loop <- which(colnames(dat1_aux2)=="Triticum.aestivum")


#the loop creates new column for each species with 0 or 1
for(i in start_loop:n_loop){
  dat1_aux2[is.na(dat1_aux2[,i]),i] <- 0
  dat1_aux2$seed1<- ifelse(dat1_aux2[ ,i]==0,0,1)
  colname <- paste(colnames(dat1_aux2)[i],"new",sep = "_")
  colnames(dat1_aux2)[length(dat1_aux2)] <- colname}

# create new datasat with only the new columns
dat2 <-  dat1_aux2 %>% dplyr::select(seedmix_name, ends_with("new"))

# calculate the frequency of the varieties
frequency <- dat2 %>% gather(`Achillea.millefolium_new`:`Triticum.aestivum_new`, value = "value", key = "name", -seedmix_name) %>% as.data.frame() %>% 
  group_by(name) %>% summarise(sum = sum(value, na.rm = T)) %>% 
  filter(sum>0)  %>%  #exclude varietis with value 0
  arrange(desc(sum))

# design upset with the 20 most frequent species
# define the less frequent species as others
top_20 <- frequency %>% top_n(20, sum) %>% dplyr::select(name)
aux_others <- dat2 %>% dplyr::select(-c(as.vector(top_20$name),"seedmix_name"))

aux_others$others <- rowSums(aux_others)
aux_others$others_new <- ifelse(aux_others[,"others"]>0,1,0)

#create dataset for upset
dat3 <- cbind(dat2,aux_others$others_new,dat1_de$price_kg,dat1_de$price_ha) %>% 
  rename(`Price per Kg` = `dat1_de$price_kg`,
         `Price per Ha` = `dat1_de$price_ha`,
         `Other Species` = `aux_others$others_new`)

# upset with boxplot.summary
colnames(dat3) <- sub("\\.", " ", colnames(dat3))
colnames(dat3) <- sub("\\_new", "", colnames(dat3))
dat3 <- dat3 %>% rename('Lolium hybridum' = 'Lolium x.hybridum',
                        "Lolium perenne" = "Lolium Perenne")

upset_plot_de <- upset(dat3,nsets=21, mb.ratio = c(0.5, 0.5),order.by = "freq", decreasing = c(TRUE,FALSE),
                       mainbar.y.label = "Species Combination\nAbsolute Frequency",
                       sets.x.label = "Species Absolute\nFrequency",
                       point.size = 3.5, line.size = 1.2,
                       text.scale = 2,
                       nintersects = 7) # only include mixtures with at least two combinations  
# upset_plot_de


##################--------------------------------------------------------------
# 4.3 figure s4: swiss online shops – seed mixture composition and their 
#                relationship with prices
##################--------------------------------------------------------------
dat1_ch <- data_1 %>% filter(country == "CH")
dat1_aux2 <- dat1_ch

n_loop <- which(colnames(dat1_aux2)=="Achillea.millefolium")
start_loop <- which(colnames(dat1_aux2)=="Triticum.aestivum")


#the loop creates new column for each species with 0 or 1
for(i in start_loop:n_loop){
  dat1_aux2[is.na(dat1_aux2[,i]),i] <- 0
  dat1_aux2$seed1<- ifelse(dat1_aux2[ ,i]==0,0,1)
  colname <- paste(colnames(dat1_aux2)[i],"new",sep = "_")
  colnames(dat1_aux2)[length(dat1_aux2)] <- colname}

# create new datasat with only the new columns
dat2 <-  dat1_aux2 %>% dplyr::select(seedmix_name, ends_with("new"))

# calculate the frequency of the varieties
frequency <- dat2 %>% gather(`Achillea.millefolium_new`:`Triticum.aestivum_new`, value = "value", key = "name", -seedmix_name) %>% as.data.frame() %>% 
  group_by(name) %>% summarise(sum = sum(value, na.rm = T)) %>% 
  filter(sum>0)  %>%  #exclude varietis with value 0
  arrange(desc(sum))


# design upset with the 20 most frequent species
# define the less frequent species as others
top_20 <- frequency %>% top_n(20, sum) %>% dplyr::select(name)
aux_others <- dat2 %>% dplyr::select(-c(as.vector(top_20$name),"seedmix_name"))

aux_others$others <- rowSums(aux_others)
aux_others$others_new <- ifelse(aux_others[,"others"]>0,1,0)

#create dataset for upset
dat3 <- cbind(dat2,aux_others$others_new,dat1_ch$price_kg,dat1_ch$price_ha) %>% 
  rename(`Price per Kg` = `dat1_ch$price_kg`,
         `Price per Ha` = `dat1_ch$price_ha`,
         `Other Species` = `aux_others$others_new`)

# upset with boxplot.summary
colnames(dat3) <- sub("\\.", " ", colnames(dat3))
colnames(dat3) <- sub("\\_new", "", colnames(dat3))
dat3 <- dat3 %>% rename('Lolium hybridum' = 'Lolium x.hybridum',
                        "Lolium perenne" = "Lolium Perenne")

upset_plot_ch <- upset(dat3,nsets=21, mb.ratio = c(0.5, 0.5),order.by = "freq", decreasing = c(TRUE,FALSE),
                       mainbar.y.label = "Species Combination\nAbsolute Frequency",
                       sets.x.label = "Species Absolute\nFrequency",
                       point.size = 3.5, line.size = 1.2,
                       text.scale = 2,
                       nintersects = 54) # only include mixtures with at least two combination  
# upset_plot_ch


################################################################################
# 5. save figures and tables
################################################################################

# set working directory
setwd(wd_output)

# save
## figures
pdf(file="v1_figure_1_part1.pdf", width=21, height=9)
upset_plot
dev.off()
ggsave(bb_price_ha, filename = "v1_figure_1_part2.pdf", width=21, height=(17.535/4*3-7)/2)
ggsave(fig_hist_eco_grass, filename = "v1_figure_2.pdf", width=21, height=17.535/3*1.35)
ggsave(fig_2_price_eco_ha, filename = "v1_fig_3_price_ha.pdf", width=21, height=17.535/3*1.35)
ggsave(fig_2_both_org, filename = "v1_figure_s1.pdf", width=21, height=17.535/3*2)
ggsave(fig_2_both_shop, filename = "v1_figure_s2.pdf", width=21, height=17.535/3*2)
pdf(file="v1_figure_s3.pdf", width=21/3*1.5, height=9)
upset_plot_de
dev.off()
pdf(file="v1_figure_s4.pdf", width=21/3*2.8, height=9)
upset_plot_ch
dev.off()

## tables
write.csv(summary_overview, file = "v1_table_s2.csv")
write.csv(summary_stats, file = "v1_table_s3.csv")
write.csv(type_mixture, file = "v1_table_s4_part1.csv")
write.csv(type_mixture_no_legume, file = "v1_table_s4_part2.csv")