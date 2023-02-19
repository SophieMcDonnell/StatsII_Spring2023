#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

library(mgcv)
library(dplyr)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))

# Regularize data

df <- mutate(climateSupport,
  # choice column
  choice = case_when(
   choice == "Supported" ~ 1,
   TRUE ~ 0 
  ),
  
  # countries column 
  countries = sub(" .*", "", countries),
  countries = as.numeric(countries) / 192,
  
  #sanctions column 
  sanctions = sub("None", 0, sanctions),
  sanctions = sub("%", "", sanctions),
  sanctions = as.numeric(sanctions) / 100
)

head(df) 


# GLM
glm_climate <- glm(choice ~ countries + sanctions, 
                family = binomial(link = 'logit'),
                data = df
            )
summary(glm_climate)















