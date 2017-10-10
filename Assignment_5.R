
# Business analytics with R - Assignments for Session 5

# Pre-requisite: download and read the CSV file - Olympic.csv
library(readr) # using library function read_delim for reading the csv file

#reading the file by  
od_fullRead <- read_delim(file.choose(),'\t', escape_double = FALSE, trim_ws = TRUE, 
                          col_names =c('Athlete','Age','Country','Year', 'Closing Date',
                                    'Sport', 'Gold Medals', 'Silver Medals', 'Bronze Medals', 
                                    'Total Medals'))

dim(od_fullRead)
#View(od_fullRead)


# Question-1: Consider only those participants who have all the data points
# Solution Steps: Display an output where all the columns posses values and discard 
# the having empty values for any of hte columns
od_cc<- od_fullRead[complete.cases(od_fullRead),] 
#View(od_cc);dim(od_cc)
summary(od_cc)
str(od_cc)
library('psych')
describe(od_cc)

# Question-2: Rank the participants in terms : . Swimming . Table Tennis . Shooting . Gymnastics . Total Medal
# Solution Steps:  Extract the data set for vqarious sports separately and then perform the ranking on them.
od_swm<-subset(od_fullRead, Sport == 'Swimming')
od_swm_rank<-od_swm[order(od_swm$`Total Medals`, decreasing = TRUE),] 
dim(od_swm_rank)
#View(od_swm_rank)

od_TT<-subset(od_fullRead, Sport == 'Table Tennis')
od_TT_rank<-od_TT[order(od_TT$`Total Medals`, decreasing = TRUE),] 
dim(od_TT_rank)
#View(od_TT_rank)

od_sd<-subset(od_fullRead, Sport == 'Shooting')
od_sd_rank<-od_sd[order(od_sd$`Total Medals`, decreasing = TRUE),] 
dim(od_sd_rank)
#View(od_sd_rank)
## View(subset(od_sd_rank, od_sd_rank$Year==2012

od_Gym<-subset(od_fullRead, Sport == 'Gymnastics')
od_gym_rank<-od_Gym[order(od_Gym$`Total Medals`, decreasing = TRUE),] 
dim(od_gym_rank)
#View(od_gym_rank)

od_fd<-od_fullRead
od_fd_rank<-od_fd[order(od_fd$`Total Medals`, decreasing = TRUE),] 
dim(od_fd_rank)
#View(od_fd_rank)

# Question-3: Rank the Categories in terms of Age.(Higher the Age,Higher the Rank)
# Solution Steps:  Make use of order functions and descending order.
od_AgeRanking<-od_fullRead[order(od_fullRead$Age, decreasing = TRUE),]
dim(od_AgeRanking)
#View(od_AgeRanking)

# Question-4: Identify Year wise top participants in terms of : . Swimming . Table Tennis . Shooting .
# Gymnastics . Total Medal
# Solution Steps:  Make use of order functions and descending order.

range(od_fullRead$Year, na.rm = TRUE) # discover the range of year values for the data set
odfr_unq<-subset(od_fullRead,!duplicated(Year)) # remove the duplicate yearfrom the whole data set
dim(odfr_unq) #check the dimensions

## Please note the variable used in these equations is already ranked in question no 2, 
# thus the tope first row is the topper for the sport already availale all we need to do is use the duplicated() 
# function to eliminate all the duplicated years following the first occurence, the following series of commands 
# will yield topper for the sport -  yearwise
# 
swm_toppers<-subset(od_swm_rank,!duplicated(Year));#swm_toppers
TT_toppers<-subset(od_TT_rank,!duplicated(Year));#TT_toppers
sd_toppers<-subset(od_sd_rank,!duplicated(Year));#sd_toppers 
Gym_toppers<-subset(od_gym_rank,!duplicated(Year));#Gym_toppers
All_toppers<-subset(od_fd_rank,!duplicated(Year));#All_toppers
