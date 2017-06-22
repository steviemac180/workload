library(tidyr)
library(tidyverse)
library(lubridate)
head(Haemostasis_Workload_AMR_2017)
work <- Haemostasis_Workload

#change the column names for MONTH to Month
colnames(work)[1] <- "Month"

# change format of the date
work$Month <- dmy(work$Month)

###################
# section comparisons
###################

#dplyr select the summary columns of each section
sections <- select(work, Month, Haemophilia, Thrombophilia, Miscellaneous, AcuStar)

# tidy up the format for ggplot
sections <- gather(sections, Section, Workload, 2:5)

# split the date field into the year, month and day - factor - then use for ggplot
s.sections <- separate(sections, Month, c("Year", "Month", "Day"),  sep= "-")

#filter out only haemophilia and thrombophilia rows
ht.wkld.sections <- filter(s.sections, Section == "Haemophilia" | Section == "Thrombophilia")
ggplot(data=ht.wkld.sections, aes(x=Year, y=Workload, color=Section)) + geom_boxplot() # deliverable
# do the same for miscellaneous and for acustar
am.wkld.sections <- filter(s.sections, Section == "AcuStar" | Section == "Miscellaneous")
ggplot(data=am.wkld.sections, aes(x=Year, y=Workload, color=Section)) + geom_boxplot() # deliverable

ggplot(data=s.sections, aes(x=Year, y=Workload)) +
  geom_boxplot() +
  facet_wrap(~Section, scale = "free")




ggplot(data=ht.wkld.sections, aes(x=Month, y=Workload, color=Section)) + 
  geom_boxplot() +
  facet_wrap(~Year) # deliverable

ggplot(data=am.wkld.sections, aes(x=Month, y=Workload, color=Section)) + 
  geom_boxplot() +
  facet_wrap(~Year) # deliverable

###############################
#  specific assay comparisons #
###############################

assay.wkld <- gather(work, Assay, Workload, 3:65)
s.assays <- separate(assay.wkld, Month, c("Year", "Month", "Day"),  sep= "-")
s.assays <- select(s.assays, Year, Month, Day, Assay, Workload)
s.assays <- filter(s.assays, Year > 2013 & Workload > 0)
head(s.assays)

# code columns as factors
s.assays$Assay <- as.factor(s.assays$Assay)

summary(s.assays)
head(s.assays)

# select platelet aggs
plts <- filter(s.assays, Assay == "Multiplate aggregation")
ggplot(data=plts, aes(x=Year, y= Workload)) + geom_boxplot()
ggplot(data=plts, aes(x=Month, y= Workload)) + geom_boxplot() # by month

# select chromogenic FVIII
CFVIII <- filter(s.assays, Assay == "FVIII Chromogenic")
ggplot(data=CFVIII, aes(x=Year, y= Workload)) + geom_boxplot() + geom_point(aes(color=Month))
ggplot(data=CFVIII, aes(x=Month, y= Workload)) + geom_boxplot() # by month

# select ADAMTS13
a13 <- filter(s.assays, Assay == "ADAMST13")
ggplot(data=a13, aes(x=Year, y= Workload)) + geom_boxplot()
ggplot(data=a13, aes(x=Month, y= Workload)) + geom_boxplot() # by month

# select FVIII
fviii <- filter(s.assays, Assay == "FVIII")
ggplot(data=fviii, aes(x=Year, y= Workload)) + geom_boxplot()
ggplot(data=fviii, aes(x=Month, y= Workload)) + geom_boxplot() # by month

# select FVII
fvii <- filter(s.assays, Assay == "FVII")
ggplot(data=fvii, aes(x=Year, y= Workload)) + geom_boxplot() # by year
ggplot(data=fvii, aes(x=Month, y= Workload)) + geom_boxplot() # by month

# select FVIII inhibitor assays
fviii.ia <- filter(s.assays, Assay == "FVIII Inhibitor Assay")
ggplot(data=fviii.ia, aes(x=Year, y= Workload)) + geom_boxplot() # by year
ggplot(data=fviii.ia, aes(x=Month, y= Workload)) + geom_boxplot() # by month

# all assays together for data exploration purposes
ggplot(data=s.assays, aes(x=Year, y= Workload)) + 
  geom_boxplot() +
  facet_wrap(~Assay, scales = "free")

# select only representative examples of assays
selected.assays <- filter(s.assays, Assay == "HIT SCREEN" | Assay == "ADAMST13" | Assay == "CAT-ETP" | Assay == "Rivaroxaban" | Assay == "Protein C" | Assay == "AT" | Assay == "APTT Lupus Screen Ratio" | Assay == "Liq AT" | Assay == "FVIII" | Assay == "FVII" | Assay == "FXIII Antigen" | Assay == "FVIII Inhibitor Assay" | Assay == "Multiplate aggregation" | Assay == "PFA" | Assay == "ROTEM" | Assay == "Platelet Nucleotides") %>% droplevels
selected.assays
summary(selected.assays)
levels(selected.assays$Assay)

classify <- function(x){
  if (x == "AT" | x == "Protein C" | x == "APTT Lupus Screen Ratio"| x == "Liq AT"){
    return("Thrombotic")
  }else{
    if(x == "HIT SCREEN"| x == "ADAMST13"| x == "CAT-ETP" | x == "Rivaroxaban"){
      return("Miscellaneous")
    }else
      if(x == "FVIII" | x == "FVII"| x == "FXIII Antigen"| x == "FVIII Inhibitor Assay"){
        return("Factors")
      }else{
        return("Platelets")
      }
  }
}


vec.new <- sapply(selected.assays$Assay, classify)
vec.new

df.assays.classified <- cbind(selected.assays, vec.new)
head(df.assays.classified)

as.factor(vec.new)

head(df.assays.classified)

ggplot(data=df.assays.classified, aes(x=Assay, y=Workload)) +
  geom_boxplot() +
  geom_jitter(aes(color=Year)) +
  facet_wrap(~vec.new, scale="free") # deliverable

########
##  filter on specific categories to make graphs clearer
########

#thrombotics
df.assays.thromb <- filter(df.assays.classified, vec.new == "Thrombotic") %>% droplevels
ggplot(data=df.assays.thromb, aes(x=Year, y=Workload)) +
  geom_boxplot() +
  geom_point(aes(color=Month)) +
  facet_wrap(~Assay, scale = "free") # deliverable


#factors
df.assays.factors <- filter(df.assays.classified, vec.new == "Factors") %>% droplevels
ggplot(data=df.assays.factors, aes(x=Year, y=Workload)) +
  geom_boxplot() +
  geom_point(aes(color=Month)) +
  facet_wrap(~Assay, scale = "free") # deliverable

#miscellaneous
df.assays.misc <- filter(df.assays.classified, vec.new == "Miscellaneous") %>% droplevels
ggplot(data=df.assays.misc, aes(x=Year, y=Workload)) +
  geom_boxplot() +
  geom_point(aes(color=Month)) +
  facet_wrap(~Assay, scale = "free") # deliverable

#platelets
df.assays.plts <- filter(df.assays.classified, vec.new == "Platelets") %>% droplevels
ggplot(data=df.assays.plts, aes(x=Year, y=Workload)) +
  geom_boxplot() +
  geom_point(aes(color=Month)) +
  facet_wrap(~Assay, scale = "free") # deliverable

#################################################################################
# build dataset classified into quarters - comparison by quarter v same last year
#################################################################################




