x<-c("data.table","tidyverse","magrittr")
lapply(x, require, character.only = TRUE)

setwd("~/Documents/2018Drought/Raw_data_yield")



######## NUTS3 scale ################
# join yield data
LIST<-list.files(path="~/Documents/2018Drought/Raw_data_yield",pattern = "Yield", recursive = TRUE)

# NUTS`
# NUTS<-fread(LIST[4], header=TRUE)
# setnames(NUTS, old= c("NUTS 3 ID (2010)","NUTS 3 2010 code and name"),
#                new= c("NUTS3_CODE","NUTS3"))

######## Netherlands     @#####
NL           <- fread(LIST[4])
NL$Periods   <- sub("JJ00", "", NL$Periods)
setnames(NL, old = c("ArableCrops","Regions"   ,'Periods','AreaUnderCultivation_1','HarvestedArea_2',"GrossYieldPerHa_3","GrossYieldTotal_4"), 
             new = c("Code_Crop",'Code_NUTS','Year'   ,'Area_S'                ,'Area'           ,"Yield"            ,"Production"))
NL          %<>%  select(-ID)
NL          %<>%  mutate_at(vars(Year,Area_S,Area,Yield,Production), as.numeric)
NL          %<>%  mutate(Country= "NL")

# Change crop code 
NL_code_crop <- read.csv("~/Documents/2018Drought/Raw_data_yield/NL_code_crop.csv", sep=";")
names(NL_code_crop)<- c("Code_Crop",'Crop')
NL %<>% left_join(.,NL_code_crop)
NL %<>% select(-Code_Crop)

# Change NUTS Code
NL_code_NUTS <- read.csv("~/Documents/2018Drought/Raw_data_yield/NL_code_NUTS.csv", sep=";")
names(NL_code_NUTS)<- c("Code_NUTS",'NUTS3')
NL %<>% left_join(.,NL_code_NUTS)
NL %<>% select(-Code_NUTS)

NL %<>%     mutate(Crop = fct_recode(Crop,
                                            "wheat_total"    = "Total wheat",
                                            "wheat_spring"   = "Spring wheat",
                                            "wheat_winter"   ="Winter wheat"))

NL$NUTS3<-gsub("(.*)[ ].*", "\\1", NL$NUTS3)

# on associe les codes NUTS3 aux codes NUTS2
Code_geo_time  <-  read.delim("~/Documents/2018Drought/Raw_data_yield/Code_NUTS2.csv", sep=';')
Code_geo_time %<>%  filter(NUTS1== "Netherlands")
names(NL)[8] <- "region"
names(Code_geo_time)[4]<- "region"

NL  %<>%  left_join(., Code_geo_time, by=c("region"))
names(NL)[8] <- "NUTS2"

#description
# 28 crops
# 17 regions: NUTS2. 
# 25 ans : 1994-2018
# Production, area, yield

head(NL)

# Netherlands : national mean
NL2<-fread(LIST[3])

# Poland
PO   <-fread(LIST[6])
setnames(PO, old = c("Name"   ,'Types of crop','Data coverage','Forms of ownership'), 
         new = c('Region',    'Crop'          ,'type'        ,'type2'              ))
PO   %<>% select(-"Measure Unit",-"Attribute", -"V10", -"Code", -"type2")
PO   %<>% spread(type, Value)
setnames(PO, old = c("Region", "crops from 1 ha", "area","harvest"), 
         new = c('NUTS3','Yield',"Area","Production"))
PO   %<>%  mutate(Country= "PO") %>% 
           mutate(Crop = fct_recode(Crop,
                             "wheat_total"    = "wheat"))
head(PO)

# on associe les codes NUTS3 aux codes NUTS2
Code_geo_time  <-  read.delim("~/Documents/2018Drought/Raw_data_yield/Code_NUTS2.csv", sep=';')
Code_geo_time %<>%  filter(NUTS1== "Poland")
names(PO)[1] <- "region"
names(Code_geo_time)[4]<- "region"
PO$region<- tolower(PO$region)

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}
Code_geo_time$region<-Unaccent(Code_geo_time$region)
PO$region<-Unaccent(PO$region)
Code_geo_time$region<- tolower(Code_geo_time$region)



PO  %<>%  left_join(., Code_geo_time, by=c("region"))
names(NL)[8] <- "NUTS2"






DD<- PO %>%  filter(Crop=="wheat_total")
PO$NUTS3<-tolower(PO$NUTS3)

Unaccent <- function(text) {
  text <- gsub("['`^~\"]", " ", text)
  text <- iconv(text, to="ASCII//TRANSLIT//IGNORE")
  text <- gsub("['`^~\"]", "", text)
  return(text)
}
PO$NUTS3<-Unaccent(PO$NUTS3)

unique(PO$NUTS3)

# 1999-2017
# Production, area, yield

# Poland
PO2<-fread(LIST[7])
PO2 %<>% select(-"Code")
PO2<- melt(PO2)

PO2 %<>%  separate(variable, c("Crop","variable","type","year", "hh"), ";")
PO2 %<>%  select(-"hh")
PO2 %<>%  filter(!is.na(type))
PO2 %<>%  spread( variable, value)

head(PO2)
# Same data as PO!!!

# Italy
IT    <-fread(LIST[2])
setnames(IT, old = c("year", "yield","area", "production","crop"), 
         new = c('Year',"Yield","Area","Production",'Crop'))
IT   %<>%  mutate(Country= "IT") %>% 
  mutate(Crop = fct_recode(Crop,
                           "durum_wheat"    = "wheat_durum"))
head(IT)
unique(IT$NUTS3)


unique(IT$Crop)
DD<- IT %>% filter(Crop=="rye")
unique(DD$Year)
# Year : 2006: 2018
# crop: 9

# Sweden
SW    <-fread(LIST[8])
setnames(SW, old = c("year", "yield","crop"), 
         new = c('Year',"Yield",'Crop'))
setnames(SW, old = c("region"), 
         new = c('NUTS3'))
SW    %<>%  mutate(Country= "SW") %>% 
            mutate(Crop = fct_recode(Crop,
                              "wheat_winter"    = "winter wheat",
                              "wheat_spring"    = "spring wheat"))


head(SW)
SW$NUTS3<-sub(".*? (.+)", "\\1",  SW$NUTS3)

SW$NUTS3<- gsub(' county','',SW$NUTS3)

unique(SW$NUTS3)


# France 


FR<-fread("~/Documents/2018Drought/Raw_data_yield/France_yield.csv", sep=";")

#setwd("~/Documents/CLAND/files_txt")
# temp = list.files(pattern="*16.txt")
# FR   = lapply(temp, function(x) read.table(x,header=TRUE , sep=";", dec=".", na.strings="NA") %>%
#                  mutate(sp=rep(sub('_data_1900-2016.txt', '', x))))%>% 
#   bind_rows()%>%
#   mutate(department  = tolower(department)) %>% 
#   filter(complete.cases(yield))     
# 
# setnames(FR, old = c("department","year", "yield","sp", "area","production"), 
#          new = c("NUTS3",'Year',"Yield",'Crop', 'Area',"Producion"))
# FR   %<>%  mutate(Country= "FR") %>% 
#   mutate(Crop = fct_recode(Crop,
#                            "durum_wheat"    = "wheat_durum_total"))
# 
# head(FR)
# fwrite(FR,"France_yield.csv")

setwd("~/Documents/2018Drought/Raw_data_yield")
## Germany
GE    <-fread(LIST[1])
setnames(GE, old = c("year", "yield","crop", "area","production"), 
         new = c('Year',"Yield",'Crop', 'Area',"Producion"))
GE    %<>%  mutate(Country= "GE")

DD <- GE %>%  filter(Crop=="wheat_winter")
DD %<>% group_by(Year) %>%summarise_all(funs(mean))
plot(DD$Year,DD$Yield)

unique(GE$NUTS3)



### autriche
AU_Area    <-fread(LIST[1])
AU_Area<-AU_Area[,-18]
AU_Area %<>% gather(Crop, Area,- Year,- "Provinces (Laender)")
head(AU_Area)

AU_Prod    <-fread(LIST[2])
AU_Prod<-AU_Prod[,-18]
AU_Prod %<>% gather(Crop, Production,- Year,- "Provinces (Laender)")
head(AU_Prod)


AU<- merge(AU_Area, AU_Prod, by= c("Crop","Year", "Provinces (Laender)"))
AU$Area <- as.numeric(as.character(AU$Area))
AU$Production <- as.numeric(as.character(AU$Production))
AU$Yield<- AU$Production/AU$Area

# denmark

DN   <-fread(LIST[3], header=TRUE)
DN %<>% gather(Year, value, -Variable, -Crop,-NUTS3)

DN<-unique(DN)
DN %<>% spread( Variable, value)


# Merge all files
  ALL<-bind_rows(list(FR,GE, NL, IT,PO,SW))
setwd("~/Documents/2018Drought")
fwrite(ALL, "Yield_Europe.csv")


