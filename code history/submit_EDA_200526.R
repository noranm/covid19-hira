##########################################################################
############################################### Data Load & Preprocessing 
##########################################################################

#--------------------------------------
# 1. Install and load packages to read excel files
#--------------------------------------
# This package allows us to easily read and modify excel files
if (! require("readxl")) install.packages("readxl")
library(readxl)

# This package allows us to easily manipulate tables
if (! require("data.table")) install.packages("data.table")
library(data.table)

#--------------------------------------
# 2. Load excel files
#--------------------------------------

# Load external files
setwd("~/")
download.file(url = "https://github.com/gp2u/opendata4covid19/archive/master.zip",
              destfile = "covid19-master.zip")

# unzip the .zip file
unzip(zipfile = file.path("./covid19-master.zip"), exdir="./choi")

# Corona claim data
data_path <- "./choi/opendata4covid19-master/Data/HIRA COVID-19 Sample Data_20200325.xlsx"
co19_t200_trans_dn = read_excel(data_path, sheet=2)
co19_t300_trans_dn = read_excel(data_path, sheet=3)
co19_t400_trans_dn = read_excel(data_path, sheet=4)
co19_t530_trans_dn = read_excel(data_path, sheet=5)

# Medical use history data
co19_t200_twjhe_dn = read_excel(data_path, sheet=6)
co19_t300_twjhe_dn = read_excel(data_path, sheet=7)
co19_t400_twjhe_dn = read_excel(data_path, sheet=8)
co19_t530_twjhe_dn = read_excel(data_path, sheet=9)

# External files for mapping
ExFiles_path <- "./choi/opendata4covid19-master/Korean_Codes"

# GNL_CD [약 관련] mapping to generic name
gnl_cd_map = read_excel(file.path(ExFiles_path,"GNL_CD-all-codes-drug.xlsx"), sheet=1)

# SEX_TP_CD to sex
sex_tp_cd_map = read_excel(file.path(ExFiles_path,"SEX_TP_CD.xlsx"), sheet=1)

# DGRSLT_TP_CD [진료 결과 구분] to outcome
dgrslt_tp_cd_map = read_excel(file.path(ExFiles_path,"DGRSLT_TP_CD.xlsx"), sheet=1)

# DGSBJT_CD [진료 과목] to hospital department name
dgsbjt_cd_map = read_excel(file.path(ExFiles_path,"DGSBJT_CD.xlsx"), sheet=1)

# CL_CD [요양기관종별구분] to clinic (hospital) type
cl_cd_map = read_excel(file.path(ExFiles_path,"CL_CD.xlsx"), sheet=1)

# FOM_TP_CD [의·치과·한방·보건기관 등의 입원,외래 서식] to event type
fom_tp_cd_map = read_excel(file.path(ExFiles_path,"FOM_TP_CD.xlsx"), sheet=1)

# MAIN_SICK [주상병] to KCD-7 string diagnosis
main_sick_map = read_excel(file.path(ExFiles_path,"MAIN_SICK_KCD-7.xlsx"), sheet=1)

# Remove the duplicate values from KCD-7
main_sick_map = main_sick_map[!duplicated(main_sick_map$MAIN_SICK),]

#--------------------------------------
# 3. Create relevant tables from data  
#--------------------------------------

#--------------------------------------
# 3.i. Table 1 - Demographic data      
#--------------------------------------
demographic_covid = co19_t200_trans_dn[,c(
  "MID", "JID", "SEX_TP_CD", "PAT_AGE" 
)]

demographic_phx = co19_t200_twjhe_dn[,c(
  "MID", "JID", "SEX_TP_CD", "PAT_AGE" 
)]

# Get only the unique entries
# there may be some individuals with >1 hospital record
demographic_covid = unique(demographic_covid)
demographic_phx = unique(demographic_phx)

#--------------------------------------   
# 3.ii. Table 2 - Care information - COVID
#--------------------------------------   

care_info_covid = co19_t200_trans_dn[,c(
  "MID", "JID", "CL_CD","FOM_TP_CD","MAIN_SICK","SUB_SICK","DGSBJT_CD",
  "RECU_FR_DD","RECU_TO_DD","FST_DD","VST_DDCNT","RECU_DDCNT",
  "DGRSLT_TP_CD"
)] 

# get only the medical records which match with table 1 demographic data
# MID 를 이용해서 명세서가 있는 care_info 만 
sprintf("nrow = %d", nrow(care_info_covid))
care_info_covid = care_info_covid[which(care_info_covid$MID %in% demographic_covid$MID),] # 진료내역
sprintf("nrow = %d", nrow(care_info_covid))

#--------------------------------------
# 3.iii. Table 3 - Care information - PAST HISTORY
#--------------------------------------

care_info_phx = co19_t200_twjhe_dn[,c(
  "MID", "JID", "CL_CD","FOM_TP_CD","MAIN_SICK","SUB_SICK","DGSBJT_CD",
  "RECU_FR_DD","RECU_TO_DD","FST_DD","VST_DDCNT","RECU_DDCNT",
  "DGRSLT_TP_CD"
)]

# get only the medical records which match with table 1 demographic data
sprintf("nrow = %d", nrow(care_info_phx))
care_info_phx = care_info_phx[which(care_info_phx$MID %in% demographic_phx$MID),]
sprintf("nrow = %d", nrow(care_info_phx))
#--------------------------------------
# 3.iv. Table 4 - Medication information - COVID
#--------------------------------------

med_info_covid = co19_t530_trans_dn[,c(
  "MID", "PRSCP_GRANT_NO",
  "FQ1_MDCT_QTY", "DY1_MDCT_QTY", "TOT_INJC_DDCNT_EXEC_FQ", "TOT_USE_QTY_OR_EXEC_FQ",
  "GNL_CD"
)]

# get only the medical records which match with table 1 demographic data
sprintf("nrow = %d", nrow(med_info_covid))
med_info_covid = med_info_covid[which(med_info_covid$MID %in% demographic_covid$MID),]
sprintf("nrow = %d", nrow(med_info_covid))

# 처방전 교부번호
# Get the number of characters in the number/string
nch = nchar(med_info_covid$PRSCP_GRANT_NO[1])

# separate PRSCP_GRANT_NO by YYMMDD, year, month, day
med_info_covid$YYMMDD  = substring(med_info_covid$PRSCP_GRANT_NO, 3, 8)

#--------------------------------------
# 3.v. Table 5 - Medication information - PAST HISTORY
#--------------------------------------

med_info_phx = co19_t530_twjhe_dn[,c(
  "MID", "PRSCP_GRANT_NO",
  "FQ1_MDCT_QTY", "DY1_MDCT_QTY", "TOT_INJC_DDCNT_EXEC_FQ", "TOT_USE_QTY_OR_EXEC_FQ",
  "GNL_CD"
)]

# get only the medical records which match with table 1 demographic data
# we expect there to be no records or overlap for the saple dataset
sprintf("nrow = %d", nrow(med_info_phx))
med_info_phx = med_info_phx[which(med_info_phx$MID %in% demographic_phx$MID),]
sprintf("nrow = %d", nrow(med_info_phx))

# Get the number of characters in the number/string
nch = nchar(med_info_phx$PRSCP_GRANT_NO[1])

# separate PRSCP_GRANT_NO by year, month, day
med_info_phx$YYMMDD       = substring(med_info_phx$PRSCP_GRANT_NO, 3, 8)

#----------------------------------------------
# 3.vi. Table 6 - Medication Treatment - COVID
#----------------------------------------------

med_trt_covid <- co19_t300_trans_dn[,c("MID", "DIV_CD", "GNL_CD", 
                                       "FQ1_MDCT_QTY", 
                                       "DY1_MDCT_QTY", "DY1_INJC_QTY_EXEC_FQ", 
                                       "TOT_INJC_DDCNT_EXEC_FQ", "TOT_USE_QTY_OR_EXEC_FQ")]
sprintf("nrow = %d", nrow(med_trt_covid))
med_trt_covid = med_trt_covid[which(med_trt_covid$MID %in% demographic_covid$MID),]
sprintf("nrow = %d", nrow(med_trt_covid))

#-----------------------------------------------------
# 3.vii. Table 7 - Medication Treatment - past histroy
#-----------------------------------------------------

med_trt_phx <- co19_t300_twjhe_dn[,c("MID", "DIV_CD", "GNL_CD", 
                                     "FQ1_MDCT_QTY", 
                                     "DY1_MDCT_QTY", "DY1_INJC_QTY_EXEC_FQ", 
                                     "TOT_INJC_DDCNT_EXEC_FQ", "TOT_USE_QTY_OR_EXEC_FQ")]

sprintf("nrow = %d", nrow(med_trt_phx))
med_trt_phx = med_trt_phx[which(med_trt_phx$MID %in% demographic_phx$MID),] # 799->5
sprintf("nrow = %d", nrow(med_trt_phx))

#--------------------------------------
# 3.viii. Table 8 - disease - COVID
#--------------------------------------
disease_covid <- co19_t400_trans_dn

sprintf("nrow = %d", nrow(disease_covid))
disease_covid = disease_covid[which(disease_covid$MID %in% demographic_covid$MID),]
sprintf("nrow = %d", nrow(disease_covid))

#--------------------------------------
# 3.ix. Table 9 - disease - past history
#--------------------------------------
disease_phx <- co19_t400_twjhe_dn

sprintf("nrow = %d", nrow(disease_phx))
disease_phx = disease_phx[which(disease_phx$MID %in% demographic_phx$MID),] # 1223-> 7
sprintf("nrow = %d", nrow(disease_phx))

#--------------------------------------
# 4. Merge datasets
#--------------------------------------

# The all.x=T component forces the fuction to keep all records in the first merged file,
# even if there are no matches in the second file.
# We are using GNL_CD the merge the files.

demographic_covid = merge(demographic_covid, sex_tp_cd_map, by="SEX_TP_CD", all.x=T) # sex

care_info_covid  = merge(care_info_covid, dgrslt_tp_cd_map, by="DGRSLT_TP_CD", all.x=T) # outcome
care_info_covid  = merge(care_info_covid, dgsbjt_cd_map, by="DGSBJT_CD", all.x=T) # department
care_info_covid  = merge(care_info_covid, cl_cd_map, by="CL_CD", all.x=T) # clinic_type
care_info_covid  = merge(care_info_covid, fom_tp_cd_map, by="FOM_TP_CD", all.x=T) # event_type
care_info_covid  = merge(care_info_covid, main_sick_map, by="MAIN_SICK", all.x=T) # main_dx

care_info_phx  = merge(care_info_phx, dgrslt_tp_cd_map, by="DGRSLT_TP_CD", all.x=T) # outcome
care_info_phx  = merge(care_info_phx, dgsbjt_cd_map, by="DGSBJT_CD", all.x=T) # department
care_info_phx  = merge(care_info_phx, cl_cd_map, by="CL_CD", all.x=T) # clinic_type
care_info_phx  = merge(care_info_phx, fom_tp_cd_map, by="FOM_TP_CD", all.x=T) # event_type
care_info_phx  = merge(care_info_phx, main_sick_map, by="MAIN_SICK", all.x=T) # main_dx

med_info_covid = merge(med_info_covid, gnl_cd_map, by="GNL_CD", all.x=T) # 약
med_info_phx = merge(med_info_phx, gnl_cd_map, by="GNL_CD", all.x=T) # 약

# modify names in main_sick_map to reuse on SUB_SICK and merge
names(main_sick_map)[names(main_sick_map) == "MAIN_SICK"] <- "SUB_SICK"
names(main_sick_map)[names(main_sick_map) == "MAIN_DX"] <- "SUB_DX"
care_info_covid  = merge(care_info_covid, main_sick_map, by="SUB_SICK", all.x=T)
care_info_phx  = merge(care_info_phx, main_sick_map, by="SUB_SICK",  all.x=T)

#--------------------------------------
# 5. Rename columns
#--------------------------------------

# demographics
names(demographic_covid)[names(demographic_covid) == "PAT_AGE"] <- "AGE"

# med_info
names(med_info_covid)[names(med_info_covid) == "FQ1_MDCT_QTY"] <- "DOES_PER_EACH" # 1회 투약량
names(med_info_covid)[names(med_info_covid) == "DY1_MDCT_QTY"] <- "DOES_COUNT_aDAY" # 1일 투여 횟수
names(med_info_covid)[names(med_info_covid) == "TOT_INJC_DDCNT_EXEC_FQ"] <- "DAYS_RX" # 총 투여일수
names(med_info_covid)[names(med_info_covid) =="TOT_USE_QTY_OR_EXEC_FQ"] <- "DOES_TOTAL" # 총 사용량

names(med_info_phx)[names(med_info_phx) == "FQ1_MDCT_QTY"] <- "DOES_PER_EACH" # 1회 투약량
names(med_info_phx)[names(med_info_phx) == "DY1_MDCT_QTY"] <- "DOES_COUNT_aDAY" # 1일 투여 횟수
names(med_info_phx)[names(med_info_phx) == "TOT_INJC_DDCNT_EXEC_FQ"] <- "DAYS_RX" # 총 투여일수
names(med_info_phx)[names(med_info_phx) =="TOT_USE_QTY_OR_EXEC_FQ"] <- "DOES_TOTAL" # 총 사용량

# med_trt
# FQ1_MDCT_QTY, DY1_MDCT_QTY, DY1_INJC_QTY_EXEC_FQ, TOT_INJC_DDCNT_EXEC_FQ, TOT_USE_QTY_OR_EXEC_FQ
names(med_trt_covid)[names(med_trt_covid) == "FQ1_MDCT_QTY"] <- "DOES_PER_EACH" # 1회 투약량
names(med_trt_covid)[names(med_trt_covid) == "DY1_MDCT_QTY"] <- "DOES_COUNT_aDAY" # 1일 투여 횟수
names(med_trt_covid)[names(med_trt_covid) == "DY1_INJC_QTY_EXEC_FQ"] <- "DOES_AMOUNT_aDAY" #1일 투약량
names(med_trt_covid)[names(med_trt_covid) == "TOT_INJC_DDCNT_EXEC_FQ"] <- "DAYS_RX" # 총 투여일수
names(med_trt_covid)[names(med_trt_covid) =="TOT_USE_QTY_OR_EXEC_FQ"] <- "DOES_TOTAL" # 총 사용량

names(med_trt_phx)[names(med_trt_phx) == "FQ1_MDCT_QTY"] <- "DOES_PER_EACH" # 1회 투약량
names(med_trt_phx)[names(med_trt_phx) == "DY1_MDCT_QTY"] <- "DOES_COUNT_aDAY" # 1일 투여 횟수
names(med_trt_phx)[names(med_trt_phx) == "DY1_INJC_QTY_EXEC_FQ"] <- "DOES_AMOUNT_aDAY" #1일 투약량
names(med_trt_phx)[names(med_trt_phx) == "TOT_INJC_DDCNT_EXEC_FQ"] <- "DAYS_RX" # 총 투여일수
names(med_trt_phx)[names(med_trt_phx) =="TOT_USE_QTY_OR_EXEC_FQ"] <- "DOES_TOTAL" # 총 사용량

#--------------------------------------
# 6. Get the age range breakdown
#--------------------------------------

agebreaks <- c(0,20,30,40,50,60,70,80,500)
agelabels <- c("0-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")

setDT(demographic_covid)[, AGE_RANGE := cut(AGE,
                                       breaks = agebreaks, 
                                       right = FALSE, 
                                       labels = agelabels)]
#--------------------------------------
# 7. Set Final data
#--------------------------------------

demographic_covid = demographic_covid[,c(
  "MID","JID","AGE_RANGE","SEX"
)]
care_info_covid = care_info_covid[,c(
  "MID","JID","MAIN_SICK","MAIN_DX","SUB_SICK","SUB_DX","RECU_FR_DD","RECU_TO_DD","FST_DD","VST_DDCNT","RECU_DDCNT","CLINIC_TYPE","EVENT_TYPE","DEPARTMENT","OUTCOME"
)]
care_info_phx = care_info_phx[,c(
  "MID","JID","MAIN_SICK","MAIN_DX","SUB_SICK","SUB_DX","RECU_FR_DD","RECU_TO_DD","FST_DD","VST_DDCNT","RECU_DDCNT","CLINIC_TYPE","EVENT_TYPE","DEPARTMENT","OUTCOME"
)]
med_info_covid = med_info_covid[,c(
  "MID","YYMMDD","DOES_PER_EACH","DOES_COUNT_aDAY","DAYS_RX","DOES_TOTAL","GNL_CD","GEN_SHORT","GEN_LONG"
)]
med_info_phx = med_info_phx[,c(
  "MID","YYMMDD","DOES_PER_EACH","DOES_COUNT_aDAY","DAYS_RX","DOES_TOTAL","GNL_CD","GEN_SHORT","GEN_LONG"
)]

##########################################################################
################################################# Data Summary Statistics 
##########################################################################

#--------------------------------------
# 1. demographic_covid
#--------------------------------------

print("==============================")
print("[TABLE] demographic - covid ")
print("==============================")
sprintf("(N = %d)", nrow(demographic_covid))

demographicsDT = setDT(demographic_covid)
summary_demographic = demographicsDT[, .(n = .N), keyby = .(AGE_RANGE, SEX)]
summary_sex = demographicsDT[, .(n = .N), keyby = .(SEX)]
summary_age_range = demographicsDT[, .(n = .N), keyby = .(AGE_RANGE)]
summary_JID = demographicsDT[,(n = .N), keyby = .(JID)]

print(" *** JID *** ")
print(summary_JID[order(-V1),][1:100,]) # 1~100?
print(summary(summary_JID$V1))
print(" *** demographics *** ")
print(summary_demographic)
print(" *** sex *** ")
print(summary_sex)
print(" *** age *** ")
print(summary_age_range)

#--------------------------------------
# 2. care_info_covid
#--------------------------------------

sprintf("=================================================")
sprintf("[TABLE] care_info_covid - 코로나 환자 명세서 내역")
sprintf("=================================================")

sprintf("(N = %d)", nrow(care_info_covid))

care_info_covid$MAIN_SICK2 <- substr(care_info_covid$MAIN_SICK, 1, 3)
care_info_covid$SUB_SICK2 <- substr(care_info_covid$SUB_SICK, 1, 3)

care_info_covidDT = setDT(care_info_covid)

# 변수 1개
summary_covid_mainsick = care_info_covidDT[, .(n = .N), keyby = .(MAIN_SICK2)]
summary_covid_subsick = care_info_covidDT[, .(n = .N), keyby = .(SUB_SICK2)]
summary_covid_clinic = care_info_covidDT[, .(n = .N), keyby = .(CLINIC_TYPE)]
summary_covid_event = care_info_covidDT[, .(n = .N), keyby = .(EVENT_TYPE)]
summary_covid_department = care_info_covidDT[, .(n = .N), keyby = .(DEPARTMENT)]
summary_covid_outcome = care_info_covidDT[, .(n = .N), keyby = .(OUTCOME)]

print(" *** MAIN_SICK Top 100 *** ")
print(summary_covid_mainsick[order(-n),][1:100,]) # 1~100?
print(" *** SUB_SICK Top 100 *** ")
print(summary_covid_subsick[order(-n),][1:100,])
print(" *** Clinic Type *** ")
print(summary_covid_clinic)
print(" *** Event Type *** ")
print(summary_covid_event)
print(" *** Department *** ")
print(summary_covid_department)
print(" *** Outcome *** ")
print(summary_covid_outcome) # $ : NA

# 변수 2개
summary_covid_sick = care_info_covidDT[, .(n=.N), keyby = .(MAIN_SICK2, SUB_SICK2)]
summary_covid_SickOutcome =  care_info_covidDT[, .(n=.N), keyby = .(MAIN_SICK2, OUTCOME)]

summary_covid_SickOutcome  = merge(summary_covid_SickOutcome, summary_covid_mainsick, by="MAIN_SICK2", all.x=T)
summary_covid_SickOutcome$n.y <- paste0((summary_covid_SickOutcome$n.x / summary_covid_SickOutcome$n.y) * 100, "%")
colnames(summary_covid_SickOutcome) <- c("MAIN_SICK2", "OUTCOME", "n", "per")

print(" *** Main Sick & Sub Sick Top 100 *** ")
print(summary_covid_sick[order(-n),][1:100,])
print(" *** Main Sick & Outcome ***")
print(summary_covid_SickOutcome[order(-n),][1:100])
print(" *** Main Sick & Death Top 100 *** ")
summary_covid_death = summary_covid_SickOutcome[which(summary_covid_SickOutcome$OUTCOME %in% "Death"),]
print(summary_covid_death[order(-n),][1:100,])

#--------------------------------------
# 3. care_info_phx
#--------------------------------------
# (?) demograpchic 의 mid 와 따져 중복 체크 하였기에 3만 남음.... 꼭 해야하는 건가

sprintf("===============================================")
sprintf("[TABLE] care_info_phx - 환자의 과거 명세서 내역")
sprintf("===============================================")
sprintf("(N = %d)", nrow(care_info_phx))

care_info_phx$MAIN_SICK2 <- substr(care_info_phx$MAIN_SICK, 1, 3)
care_info_phx$SUB_SICK2 <- substr(care_info_phx$SUB_SICK, 1, 3)

care_info_phxDT = setDT(care_info_phx)

# 변수 1개
summary_phx_mainsick = care_info_phxDT[, .(n = .N), keyby = .(MAIN_SICK2)]
summary_phx_subsick = care_info_phxDT[, .(n = .N), keyby = .(SUB_SICK2)]
summary_phx_clinic = care_info_phxDT[, .(n = .N), keyby = .(CLINIC_TYPE)]
summary_phx_event = care_info_phxDT[, .(n = .N), keyby = .(EVENT_TYPE)]
summary_phx_department = care_info_phxDT[, .(n = .N), keyby = .(DEPARTMENT)]
summary_phx_outcome = care_info_phxDT[, .(n = .N), keyby = .(OUTCOME)]

print(" *** MAIN_SICK Top 100 *** ")
print(summary_phx_mainsick[order(-n),][1:100,]) # 1~100?
print(" *** SUB_SICK Top 100 *** ")
print(summary_phx_subsick[order(-n),][1:100,])
print(" *** Clinic Type *** ")
print(summary_phx_clinic)
print(" *** Event Type *** ")
print(summary_phx_event)
print(" *** Department *** ")
print(summary_phx_department)
print(" *** Outcome *** ")
print(summary_phx_outcome) # $ : NA


# 변수 2개
summary_phx_sick = care_info_phxDT[, .(n=.N), keyby = .(MAIN_SICK2, SUB_SICK2)]
summary_phx_SickOutcome =  care_info_phxDT[, .(n=.N), keyby = .(MAIN_SICK2, OUTCOME)]

summary_phx_SickOutcome  = merge(summary_phx_SickOutcome, summary_phx_mainsick, by="MAIN_SICK2", all.x=T)
summary_phx_SickOutcome$n.y <- paste0((summary_phx_SickOutcome$n.x / summary_phx_SickOutcome$n.y) * 100, "%")
colnames(summary_phx_SickOutcome) <- c("MAIN_SICK2", "OUTCOME", "n", "per")

print(" *** Main Sick & Sub Sick Top 100 *** ")
print(summary_phx_sick[order(-n),][1:100,])
print(" *** Main Sick & Outcome ***")
print(summary_phx_SickOutcome[order(-n),][1:100])

#--------------------------------------
# 4. med_info_covid
#--------------------------------------

sprintf("======================================================")
sprintf("[TABLE] med_info_covid - 코로나 환자의 처방내역 (원외)")
sprintf("======================================================")
sprintf("(N = %d)", nrow(med_info_covid))

head(med_info_covid)

med_info_covidDT = setDT(med_info_covid)

summary_covid_medGnl = med_info_covidDT[, .(n = .N), keyby = .(GNL_CD)]
# summary_phx_GnlDoes = med_info_phxDT[, .(n = .N), keyby = .(GNL_CD, DOES_TOTAL)]

print(" *** GNL_CD *** ")
print(summary_covid_medGnl[order(-n),][1:100,])
#summary_phx_GnlDoes

#--------------------------------------
# 5. med_info_phx
#--------------------------------------

sprintf("==================================================")
sprintf("[TABLE] med_info_phx - 환자의 과거 처방내역 (원외)")
sprintf("==================================================")
sprintf("(N = %d)", nrow(med_info_phx))

head(med_info_phx)

med_info_phxDT = setDT(med_info_phx)

summary_phx_medGnl = med_info_phxDT[, .(n = .N), keyby = .(GNL_CD)]
#summary_phx_medGnlDoes = med_info_phxDT[, .(n = .N), keyby = .(GNL_CD, DOES_TOTAL)]

print(" *** GNL_CD *** ")
print(summary_phx_medGnl[order(-n),][1:100,])
#summary_phx_medGnlDoes

#--------------------------------------
# 6. med_trt_covid
#--------------------------------------

sprintf("=======================================================")
sprintf("[TABLE] med_trt_covid - 코로나 환자의 세부 진료 (원내)")
sprintf("=======================================================")
sprintf("(N = %d)", nrow(med_trt_covid))

med_trt_phxDT = setDT(med_trt_covid)
summary_covid_trtGnl = med_trt_phxDT[, .(n = .N), keyby = .(GNL_CD)]

print(" *** GNL_CD *** ")
print(summary_covid_trtGnl[order(-n),][1:100,])

#--------------------------------------
# 7. med_trt_phx
#--------------------------------------

sprintf("===================================================")
sprintf("[TABLE] med_trt_phx - 환자의 과거 세부 진료 (원내)")
sprintf("===================================================")
sprintf("(N = %d)", nrow(med_trt_phx))

head(med_trt_phx)
table(med_trt_phx$GNL_CD)

med_trt_phxDT = setDT(med_trt_phx)

summary_phx_TrtGnl = med_trt_phxDT[, .(n = .N), keyby = .(GNL_CD)]

print(" *** GNL_CD *** ")
print(summary_phx_TrtGnl[order(-n),][1:100,])

#--------------------------------------
# 8. disease_covid
#--------------------------------------

disease_covid$SICK_CD2 <- substr(disease_covid$SICK_CD, 1, 3)

sprintf("================================================")
sprintf("[TABLE] disease_covid - 코로나 환자의 상병내역")
sprintf("================================================")
sprintf("(N = %d)", nrow(disease_covid))

head(disease_covid)

disease_covidDT = setDT(disease_covid)

summary_covid_D_Sick = disease_covidDT[, .(n = .N), keyby = .(SICK_CD2)]
# summary_covid_D_Department = disease_covidDT[, .(n = .N), keyby = .(DMD_DGSBJT_CD)]

print(" *** SICK_CD *** ")
print(summary_covid_D_Sick[order(-n),][1:100,])

#--------------------------------------
# 8. disease_phx
#--------------------------------------

disease_phx$SICK_CD2 <- substr(disease_phx$SICK_CD, 1, 3)

sprintf("==========================================")
sprintf("[TABLE] disease_phx - 환자의 과거 상병내역")
sprintf("==========================================")
sprintf("(N = %d)", nrow(disease_phx))

head(disease_phx)

disease_phxDT = setDT(disease_phx)

summary_phx_D_Sick = disease_phxDT[, .(n = .N), keyby = .(SICK_CD2)]

print(summary_phx_D_Sick[order(-n),][1:100,])


print(dd)

print(" *** SICK_CD *** ")
print(summary_phx_D_Sick[order(-n),][1:100,])
