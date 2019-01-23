# load 
x<-c("tidyverse","broom","ggpubr","hydroGOF","data.table")
lapply(x, require, character.only = TRUE)


#### I/ DATALOAD  ##############################################################################
# setwd("~/Documents/CLAND/files_txt")
# temp = list.files(pattern="*16.txt")
# Yield = lapply(temp, function(x) read.table(x,header=TRUE , sep=";", dec=".", na.strings="NA") %>%
#                  mutate(sp=rep(sub('_data_1900-2016.txt', '', x))))%>% 
#   bind_rows()%>%
#   mutate(department  = tolower(department),
#          ID          = paste0(department, sp)) %>%
#   filter(complete.cases(yield))     

Yield <- fread("~/Documents/2018Drought/Yield_Europe.csv") %>% 
  mutate(ID =paste0(Country, NUTS3, Crop)) 
summary(Yield)


## NUTS 2
Yield <- fread("~/Documents/2018Drought/Yield_Europe_NUTS2.csv") %>% 
  mutate(ID =paste0(Country, NUTS2, Crop)) 
summary(Yield)
names(Yield)[1]<-"Year"
Yield<- Yield %>%  filter(!is.na(Yield))
Yield<- Yield %>%  filter(Yield=="0")


#####

## Define the types of models
model_lin     <-function(DAT) {lm(Yield~Year, data=DAT)}
model_quad    <-function(DAT) {lm(Yield~Year+I(Year^2), data=DAT)}
model_cub     <-function(DAT) {lm(Yield~Year+I(Year^2)+I(Year^3), data=DAT)}
model_loess   <-function(DAT) {loess(Yield~Year, data=DAT)}
model_spline  <-function(DAT) {smooth.spline(DAT$Year,DAT$Yield)}

# identify number of yield data per Departemnt X sp
FILTRE <- Yield  %>%     filter(complete.cases(Yield)) %>% group_by(NUTS2,Crop,Country) %>%  summarise(n = n_distinct(Year)) %>% 
  mutate(ID =paste0(Country, NUTS2, Crop)) 

## linear models
tab_lin <- Yield  %>% 
  filter(ID %in% FILTRE[FILTRE$n>4,]$ID)  %>%
  group_by(Country,NUTS2,Crop)            %>%
  nest()                                  %>%
  mutate(lin           = purrr::map(data, model_lin),             # linear model
         AIC_lin       = purrr::map(lin, stats::AIC),
         pred_lin      = purrr::map(lin, augment),
         quad          = purrr::map(data, model_quad),            # quadratic model
         AIC_quad      = purrr::map(quad, stats::AIC),
         pred_quad     = purrr::map(quad, augment),
         cub           = purrr::map(data, model_cub),             # cub model
         AIC_cub       = purrr::map(cub, stats::AIC),
         pred_cub      = purrr::map(cub, augment))

# Table of AIC (for the linear models)
AIC_lin  <- tab_lin  %>% unnest(AIC_lin, .drop = TRUE)
AIC_cub  <- tab_lin  %>% unnest(AIC_cub, .drop = TRUE)
AIC_quad <- tab_lin  %>% unnest(AIC_quad, .drop = TRUE)
TAB_AIC  <- data.frame(AIC_lin[,c(1,2,3,4)],AIC_cub[,4],AIC_quad[,4])
TAB_AIC  <- TAB_AIC %>%
  mutate(mini = pmin(AIC_lin,AIC_cub,AIC_quad))

# Table of residuals    
tab_lin  <- data.frame(
  tab_lin    %>%   unnest(pred_lin, .drop = TRUE)   %>% select(Country),
  tab_lin    %>%   unnest(pred_lin, .drop = TRUE)   %>% select(NUTS2),
  tab_lin    %>%   unnest(pred_lin, .drop = TRUE)   %>% select(Year),
  tab_lin    %>%   unnest(pred_lin, .drop = TRUE)   %>% select(Crop),
  tab_lin    %>%   unnest(pred_lin, .drop = TRUE)   %>% select(.resid,.fitted),
  tab_lin    %>%   unnest(pred_quad, .drop = TRUE)  %>% select(.resid,.fitted),
  tab_lin    %>%   unnest(pred_cub, .drop = TRUE)   %>% select(.resid,.fitted))
names(tab_lin)<- c("Country", "NUTS2", "Year", "Crop", "anomaly_lin", "prediction_lin", "anomaly_quad", "prediction_quad", "anomaly_cub", "prediction_cub")
tab_lin     <-full_join(tab_lin,TAB_AIC,by=c("Country", "NUTS2","Crop"))

## We add a column with result for the best model
tab_lin     <- tab_lin %>%
  mutate(anomaly_poly = case_when( AIC_lin  == mini ~ anomaly_lin,
                                   AIC_cub  == mini ~ anomaly_cub,
                                   AIC_quad == mini ~ anomaly_quad))
tab_lin     <- tab_lin %>%
  mutate(prediction_poly = case_when( AIC_lin  == mini ~ prediction_lin,
                                      AIC_cub  == mini ~ prediction_cub,
                                      AIC_quad == mini ~ prediction_quad))

## Loes model
tab_loess <- Yield                        %>% 
  filter(ID %in% FILTRE[FILTRE$n>4,]$ID)  %>%
  group_by(Country, NUTS2,Crop)           %>%
  nest()%>%
  mutate(loess        = purrr::map(data, model_loess),         
         pred_loess   = purrr::map(loess, augment))

tab_loess<-tab_loess  %>%   unnest(pred_loess, .drop = TRUE) %>% select(NUTS2, Crop,Year,.resid,.fitted)
names(tab_loess)<-c("NUTS2", "Crop","Year","anomaly_loess", "prediction_loess")

# Spline model
#GE oats rye durum_wheat
tab_spline <- Yield                      %>% 
  filter(ID %in% FILTRE[FILTRE$n>5,]$ID) %>%
  filter(complete.cases(Yield))          %>% 
  group_by(Country,NUTS2,Crop)           %>%
  nest()%>%
  mutate(spline         = purrr::map(data, model_spline),  # spline model
         pred_spline    = purrr::map(spline, augment))

tab_spline<-  tab_spline %>%   unnest(pred_spline, .drop = TRUE)%>% select(Country, NUTS2, Crop,x, .resid,.fitted)
names(tab_spline)<-c("Country","NUTS2", "Crop","Year","anomaly_spline", "prediction_spline")


# Join all data together
RES<-full_join(full_join(tab_lin,tab_loess, by=c("NUTS2", "Crop","Year")), tab_spline)
RES<-full_join(RES, Yield) %>% select(-contains("AIC"), -mini)

# calculate mean and sd value 
SUM<-RES %>% group_by(NUTS2,Crop)     %>%
  summarize(sd_lin= sd(anomaly_lin,na.rm=TRUE),       mean_lin =mean(prediction_lin,na.rm=TRUE),
            sd_cub= sd(anomaly_cub,na.rm=TRUE),       mean_cub = mean(prediction_cub,na.rm=TRUE),
            sd_quad= sd(anomaly_quad,na.rm=TRUE),     mean_quad =mean(prediction_quad,na.rm=TRUE),
            sd_spline= sd(anomaly_spline,na.rm=TRUE), mean_spline = mean(prediction_spline,na.rm=TRUE),
            sd_loess = sd(anomaly_loess,na.rm=TRUE),  mean_loess= mean(prediction_loess,na.rm=TRUE),
            sd_poly= sd(anomaly_poly,na.rm=TRUE),     mean_poly= mean(prediction_poly,na.rm=TRUE))
## il y a des Na : vérifier

VERIF <- SUM[rowSums(is.na(SUM)) > 0,] %>%
  mutate(ID= paste0(NUTS2, Crop))
AA<-FILTRE %>% filter(ID %in% VERIF$ID)


# calculate standardize and normalize residuals
RES<-full_join(RES,SUM)%>%
  mutate(anomaly_lin_stand    = anomaly_lin/ sd_lin,
         anomaly_lin_norm     = anomaly_lin/ prediction_lin*100,
         anomaly_cub_stand    = anomaly_cub/ sd_cub,
         anomaly_cub_norm     = anomaly_cub/ prediction_cub*100,
         anomaly_quad_stand   = anomaly_quad/ sd_quad,
         anomaly_quad_norm    = anomaly_quad/ prediction_quad*100,
         anomaly_slpine_stand = anomaly_spline/ sd_spline,
         anomaly_spline_norm  = anomaly_spline/ prediction_spline*100,
         anomaly_loess_stand  = anomaly_loess/ sd_loess,
         anomaly_loess_norm   = anomaly_loess/ prediction_loess*100,
         anomaly_poly_stand   = anomaly_poly/ sd_poly,
         anomaly_poly_norm    = anomaly_poly/ prediction_poly*100) 
#%>%    select(-contains("sd_"), -contains("mean_"))


rm(list=setdiff(ls(), "RES"))

DD<- RES %>%  filter(NUTS3=="ain")
DD<- DD %>%  filter(Crop=="barley_spring")

ggplot(DD)+ geom_line(aes(Year, Yield))  + theme_pubr(base_size = 30)+
  geom_line(aes(Year,prediction_lin), color="red")+
  geom_line(aes(Year,prediction_spline), color="blue")+
  geom_line(aes(Year,prediction_loess), color="green")

  
###### Analyse graphique ####################################################

# 1. qualité des fits
SUB<- RES  %>%  filter(Country=="FR")
ggplot(data=SUB)+ geom_point(aes(prediction_lin, Yield))


##### Calculate indicator of quality  ######
QUAL <- function(x) {
  QUAL <- gof(x$pred, x$Yield)
  return(QUAL)
}

RES_QUAL<- RES %>%  select(Country,NUTS3, Year, Crop,prediction_lin,prediction_quad,prediction_cub,prediction_poly,prediction_loess,prediction_spline, Yield )
RES_QUAL<- RES_QUAL  %>%  filter(Country=="FR")
RES_QUAL<- RES_QUAL %>%  gather(variable, pred, -Yield, - Country, -NUTS3, -Year, -Crop) 
RES_QUAL<- RES_QUAL %>%  filter(!is.na(pred))


NAMES<-row.names(QUAL(RES_QUAL))
QUALI<-RES_QUAL                       %>% 
  group_by(Country, Crop, variable)   %>% 
  do(as.data.frame(QUAL(.)))          %>% 
  mutate(IND=NAMES )%>%
  spread(IND, V1) 

QUALI<- data.table(QUALI)
QUALI[ , .SD[which.min(MSE)], by = c("Country", "Crop")]

DAT1<- RES_QUAL %>%
  filter(variable=="prediction_spline")
ggplot(DAT1)+ geom_point(aes(x=pred,y=Yield )) +theme_pubr() + facet_wrap(~Crop, scales="free", ncol=6)

#2. on fait un graphique des anomalies


ANOM<- RES   %>%  select(Country,NUTS3, Year, Crop,anomaly_lin_norm, Yield )
ANOM<- ANOM  %>%  filter(Country=="GE")
ANOM<- ANOM  %>%  gather(variable, anom, -Yield, - Country, -NUTS3, -Year, -Crop) 
ANOM$cyear_class<-cut(ANOM$Year, breaks= c(1900,1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), 
                    labels=c("1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020"))

library(RColorBrewer)
my_orange = brewer.pal(n = 9, "Oranges")[3:9] #there are 9, I exluded the two lighter hues
ggplot(ANOM)+ geom_density(aes(x=anom , color=cyear_class))+ facet_wrap(.~Crop, scales="free", ncol=6)+
  theme_pubr()+
  scale_colour_manual(values=c("#74C476" ,"#41AB5D", "#238B45","#FD8D3C" ,"#F16913" ,"#D94801", "#A63603" ,"black"))



ANOM %<>%  group_by(Year,Country,Crop) %>%summarise_all(funs(mean), na.rm=TRUE)
ANOM$cyear_class<-cut(ANOM$Year, breaks= c(1900,1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020), 
                      labels=c("1950", "1960", "1970", "1980", "1990", "2000", "2010", "2020"))

ggplot(ANOM)+ geom_density(aes(x=anom , color=cyear_class))+ facet_wrap(.~Crop, scales="free", ncol=6)+
  theme_pubr()+
  scale_colour_manual(values=c("#74C476" ,"#41AB5D", "#238B45","#FD8D3C" ,"#F16913" ,"#D94801", "#A63603" ,"black"))


ggplot(ANOM)+ geom_density(aes(x=anom ))+ facet_wrap(.~Crop, scales="free", ncol=6)+
  theme_pubr()+
geom_abline(aes(intercept=0, slope=999), color="gray", linetype=2)+
  geom_point(data=ANOM %>% filter(Year=="2018"), aes(x=anom,y=0.001), color="red")


ggplot(ANOM)+ geom_point(aes(x=anom, y=Yield))+ facet_wrap(.~Crop, scales="free", ncol=6)+
  theme_pubr()+
  geom_point(data=ANOM %>% filter(Year=="2018"), aes(x=anom,y=Yield), color="red")


ANOM_Wheat<-ANOM %>%  filter(Crop=="wheat_winter")
ggplot(ANOM_Wheat)+geom_boxplot(aes(x=Year, y=anom, group=Year, fill=(Year=="2016")))+theme_pubr()
ggplot(RES %>%  filter(NUTS3 =="ain"))+geom_line(aes(x=Year, y=Yield, fill=(Year=="2016")))+theme_pubr()
+
  geom_line(aes(x=Year, y=Yield, fill=(Year=="2016")))
  


RES_Wheat<- RES %>%   filter(grepl("heat",Crop))
RES_Wheat %<>%  group_by(Year,Country,Crop) %>%summarise_all(funs(mean), na.rm=TRUE)

ggplot(RES_Wheat)+geom_line(aes(x=Year, y=anomaly_spline_norm))+theme_pubr()+
  geom_hline(yintercept=c(10,-10), linetype=2) +
  geom_point(data= RES_Wheat %>% filter(anomaly_spline_norm< -10),aes(Year, y=anomaly_spline_norm), color="red", size=3) +
  geom_point(data= RES_Wheat %>% filter(anomaly_spline_norm> 10),aes(Year, y=anomaly_spline_norm), color="green", size=3)+ 
  facet_grid(Crop~Country)
  

RES_Wheat2<- RES_Wheat %>%   filter(grepl("wheat_winter",Crop))
DD <- RES_Wheat2 %>%  filter(Country=="GE") 
DD <- DD %>%  filter(NUTS3=="Goerlitz") 
plot(DD$Year, DD$anomaly_spline_norm)


PRED<-smooth.spline(DD$Year,DD$Yield)
PRED<-predict(PRED)

plot(PRED$y, DD$prediction_spline)
  
  %>%  group_by(Year,Country,Crop) %>%summarise_all(funs(mean))
plot(DD$Year, DD$Yield)
lines(DD$Year, DD$prediction_spline)
lines(DD$Year, DD$prediction_poly)


plot(DD$Year, DD$anomaly_spline_norm)

