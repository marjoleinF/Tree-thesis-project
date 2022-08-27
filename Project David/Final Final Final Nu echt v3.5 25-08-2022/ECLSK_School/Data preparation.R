##################################################################\
# 
# 04-10-2018
# Thesis
#
##################################################################

### Read in ECLSK data:
library(data.table)
# -> set working directory
ICPSR <- fread("28023-0001-Data.tsv")

## Load theta errata data:
# -> set working directory
theta_err <- read.csv2("eclsk_theta_errata.csv", header = TRUE)

## Check if CHILDID is identical between the two:
table(theta_err$CHILDID == ICPSR$CHILDID)
## All are identical, so we can quite easily overwrite
old_names <- c('C2R4RTHT')
errata_names <- c("C2R4RTHT_R")
table(is.na(theta_err[ , errata_names]))
table(is.na(ICPSR[ , old_names, with = FALSE]))
## Amount of missingness is not exactly the same

## Replace old scale values with revised theta values:
ICPSR[ , old_names] <- theta_err[ , errata_names]

## Check means and sds:
sapply(ICPSR[ , old_names, with = FALSE], mean, na.rm = TRUE)
sapply(ICPSR[ , old_names, with = FALSE], sd, na.rm = TRUE)
## Seems sensible: all scores seem on the same theta scale, with increasing values over the years
## SDs are not equal to 1 within each year, but the SD over all reading scores is probably 1.

## Create mean gender per school
library(dplyr)
data_gender <- ICPSR[, c("S2_ID", "CHILDID", "GENDER")]
data2 <- data_gender %>%
  group_by(S2_ID) %>%
  mutate(school_pct.male = mean(GENDER == "1"))
mean_gender_school <- data2[,-1]
mean_gender_school <- mean_gender_school[,-2] 
summary(mean_gender_school)           
#pct.male is the percentage of the students per school that is male

## Create mean SES per school
data_SES <- ICPSR[, c("S2_ID", "CHILDID", "WKSESL")]
data_SES$S2_ID[data_SES$S2_ID == NA] <- NA
data_SES$WKSESL[data_SES$WKSESL == NA] <- NA
data_SES <- na.omit(data_SES)

data3 <- data_SES %>%
  group_by(S2_ID) %>%
  mutate(school_SES = mean(WKSESL))
mean_SES_school <- data3[, c(2, 4)]

## Create mean race per school
data_Race <- ICPSR[, c("S2_ID", "CHILDID", "W8RACETH")]
data_Race$W8RACETH[data_Race$W8RACETH %in% c(-1, -9)] <- NA
data_Race <- na.omit(data_Race)
data_Race$W8RACETH[data_Race$W8RACETH %in% c(3, 4, 5, 6, 7, 8)] <- 2
#child level
colnames(data_Race)[colnames(data_Race) == 'W8RACETH'] <- 'child_race'
#school level
datarace <- data_Race %>%
  group_by(S2_ID) %>%
  mutate(school_race = mean(child_race == "1"))
child_race <- datarace[, c(2, 3)]
school_race <- datarace[, c(2, 4)]

## Create certification
data_cert <- ICPSR[, c("S2_ID", "CHILDID", "J72TCERT")]
data_cert$J72TCERT[data_cert$J72TCERT == 6] <- 1
data_cert$J72TCERT[data_cert$J72TCERT == -9] <- NA
data_cert$J72TCERT[data_cert$J72TCERT %in% c(3, 4, 5, 7)] <- 2
data_cert <- na.omit(data_cert)
colnames(data_cert)[colnames(data_cert) == 'J72TCERT'] <- 'child_cert'
datacert <- data_cert %>%
  group_by(S2_ID) %>%
  mutate(school_cert = mean(child_cert == "1"))
child_cert <- datacert[, c(2, 3)]
school_cert <- datacert[, c(2, 4)]

## Create dataframe to be used
Data <- ICPSR[, c( 'CHILDID', "GENDER", "WKSESL", "S2_ID", "P1READBO", "C2R4RTHT")]
Data <- merge(Data, mean_gender_school, by = "CHILDID")
Data <- merge(Data, mean_SES_school, by = "CHILDID")
Data <- merge(Data, child_race, by = "CHILDID")
Data <- merge(Data, school_race, by = "CHILDID")
Data <- merge(Data, child_cert, by = "CHILDID")
Data <- merge(Data, school_cert, by = "CHILDID")

## Delete NA
Data$child_race <- as.factor(Data$child_race)
Data$child_cert <- as.factor(Data$child_cert)
summary(Data$GENDER)
Data$GENDER <- as.factor(Data$GENDER)
Data$GENDER[Data$GENDER == -9] <- NA
summary(Data$CHILDID)
summary(Data$WKSESL)
summary(Data$S2_ID)
summary(Data$P1READBO)
Data$P1READBO[Data$P1READBO %in% c(-7, -8, -9)] <- NA
Data$P1READBO <- as.factor(Data$P1READBO)
summary(Data)
summary(Data$pct.male)
summary(Data$C2R4RTHT)
Data$C2R4RTHT[Data$C2R4RTHT == -9] <- NA
Data <- na.omit(Data)

## Changing names to distinguish between child and school levels
colnames(Data)[colnames(Data) == 'CHILDID'] <- 'child_ID'
colnames(Data)[colnames(Data) == 'S2_ID'] <- 'school_ID'
colnames(Data)[colnames(Data) == 'GENDER'] <- 'child_gender'
colnames(Data)[colnames(Data) == 'WKSESL'] <- 'child_SES'
colnames(Data)[colnames(Data) == 'P1READBO'] <- 'child_read'
colnames(Data)[colnames(Data) == 'CHILDID'] <- 'child_ID'
colnames(Data)[colnames(Data) == 'C2R4RTHT'] <- 'child_readscore'

save(Data, file = "ECLSK_school.Rda")
load(file = "ECLSK_school.Rda")
