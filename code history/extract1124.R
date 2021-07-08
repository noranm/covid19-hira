#######################################
########## [1] Preparation ############
#######################################
# external data:
# 1. code-books-200721.xlsx
# 2. ATCcode_mapping.xlsx

library(readxl)
# library(dplyr)
library(data.table)

dir.create("./resultsCNLLS/", showWarnings=FALSE)
dir.create("./resultsCNLLS/frequency", showWarnings=FALSE)
dir.create("./resultsCNLLS/frequency2", showWarnings=FALSE)
dir.create("./resultsCNLLS/frequency3", showWarnings=FALSE)
dir.create("./resultsCNLLS/model", showWarnings=FALSE)
dir.create("./resultsCNLLS/hospital", showWarnings=FALSE)
dir.create("./resultsCNLLS/ps", showWarnings=FALSE)
dir.create("./resultsCNLLS/table", showWarnings=FALSE)

# raw data (PLEASE REPLACE sample_schema.xlsx WITH THE RAW DATA)
data_path <- "./sample_schema.xlsx"
co19_t200 = data.frame(read_excel(data_path, sheet=4))
co19_t300 = data.frame(read_excel(data_path, sheet=5))
co19_t400 = data.frame(read_excel(data_path, sheet=6))
co19_t530 = data.frame(read_excel(data_path, sheet=7))

# Medical use history data
co19_twjhe200 = data.frame(read_excel(data_path, sheet=8))
co19_twjhe300 = data.frame(read_excel(data_path, sheet=9))
co19_twjhe400 = data.frame(read_excel(data_path, sheet=10))
co19_twjhe530 = data.frame(read_excel(data_path, sheet=11))

# 
co19_t200$AGE_cal <- 2020-as.numeric(substr(co19_t200$PAT_BTH, 1, 4))

testJID <- unique(co19_t200$JID)

targetJID <- unique(co19_t200[(co19_t200$CONFIRM=="Y")&(co19_t200$AGE_cal > 18), ]$JID)
targetJID <- setdiff(targetJID, NA)

age40 <- unique(co19_t200[(co19_t200$AGE_cal >= 40), ]$JID)
age40JID <- intersect(targetJID, age40)

hospital <- unique(co19_t200[(co19_t200$FOM_TP_CD == "021") & (co19_t200$VST_DDCNT >= 2), ]$JID)
hospitalJID <- intersect(targetJID, hospital)

# 배제상병 제외
co19_t400[co19_t400$SICK_TY_CD == '3', ]$SICK_CD <- "0"
co19_twjhe400[co19_twjhe400$SICK_TY_CD == '3', ]$SICK_CD <- "0"

# -------------------------------------------------- #
co19_t200_trans_dn <- co19_t200[(co19_t200$JID %in% targetJID), c("JID", "MID", "INSUP_TP_CD", "SEX_TP_CD", "RECU_FR_DD", "PAT_BTH", "MAIN_SICK", "SUB_SICK", "DEATH", "CONFIRM")]
co19_t200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_t200_trans_dn$RECU_FR_DD), "%Y%m%d")

co19_t300_trans_dn <- co19_t300[co19_t300$JID %in% targetJID, c("JID", "MID", "DIV_CD")]
co19_t400_trans_dn <- co19_t400[co19_t400$JID %in% targetJID, c("JID", "MID", "SICK_CD")]
co19_t530_trans_dn <- co19_t530[co19_t530$JID %in% targetJID, c("JID", "MID", "DIV_CD", "GNL_CD")]

co19_twjhe200_trans_dn <- co19_twjhe200[co19_twjhe200$JID %in% targetJID, c("JID", "MID", "SEX_TP_CD", "RECU_FR_DD", "PAT_STC_AGE", "MAIN_SICK", "SUB_SICK", "RECU_FR_DD", "PRCL_SYM_TP_CD")]
co19_twjhe200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_twjhe200_trans_dn$RECU_FR_DD), "%Y%m%d")

co19_twjhe300_trans_dn <- co19_twjhe300[co19_twjhe300$JID %in% targetJID,c("JID", "MID", "DIV_CD", "GNL_CD")]
co19_twjhe400_trans_dn <- co19_twjhe400[co19_twjhe400$JID %in% targetJID,c("JID", "MID", "SICK_CD")]
co19_twjhe530_trans_dn <- co19_twjhe530[co19_twjhe530$JID %in% targetJID,c("JID", "MID", "DIV_CD", "GNL_CD")]

# External files (code_book)
codebook_path <- "./"
codebook <- "code-books-201127.xlsx"

code_ICD10 <- read_excel(file.path(codebook_path, codebook), sheet = 2)
code_ATC <- read_excel(file.path(codebook_path, codebook), sheet = 3)
code_outcomes <- read_excel(file.path(codebook_path, codebook), sheet = 4)
code_covid <- read_excel(file.path(codebook_path, codebook), sheet = 5)
ATC_mapping <- read_excel(file.path(codebook_path, "ATCcode_mapping.xlsx"))

# -------------------------------------------------- #
co19_t200 <- NULL
co19_t300 <- NULL
co19_t400 <- NULL
co19_t530 <- NULL

co19_twjhe200 <- NULL
co19_twjhe300 <- NULL
co19_twjhe400 <- NULL
co19_twjhe530 <- NULL
# -------------------------------------------------- #

#######################################################
########## THE END OF IMPORT EXTERNAL DATA ############
#######################################################
code_ICD10_df <- data.frame(code_ICD10[,c("uniqueID", "name", "diagnosis", "code", "codeType", "searchFor", "statin study", "antiplatelet anticoagulant study", "immune suppressant study")])
code_ATC_df <- data.frame(code_ATC[,c("uniqueID", "name", "use", "code", "codeType", "searchFor", "statin study", "antiplatelet anticoagulant study", "immune suppressant study")])
code_outcomes_df <- data.frame(code_outcomes[,c("uniqueID",  "name", "code", "codeType", "searchFor")])
code_covid_df <- data.frame(code_covid[,c("uniqueID", "name", "code", "codeType", "searchFor")])

code_ICD10_df$uniqueCode <- paste0("ID", code_ICD10_df$uniqueID, ".", code_ICD10_df$code)
code_ATC_df$uniqueCode <- paste0("ID", code_ATC_df$uniqueID, ".", code_ATC_df$code)
code_outcomes_df$uniqueCode <- paste0("ID", code_outcomes_df$uniqueID, ".", code_outcomes_df$code)
code_covid_df$uniqueCode <- paste0("ID", code_covid_df$uniqueID, ".", code_covid_df$code)

ATC_mapping_df <- data.frame(ATC_mapping)[,c("주성분코드", "제품코드", "ATC코드")]
colnames(ATC_mapping_df) <- c("GNL_CD", "DIV_CD", "ATC")

# -------------------------------------------------- #
code_ICD10 <- NULL
code_ATC <- NULL
code_outcomes <- NULL
code_covid <- NULL
ATC_mapping <- NULL
# -------------------------------------------------- #
names(co19_t200_trans_dn)
# co19_t200_trans_dn
co19_T200DT <- data.table(co19_t200_trans_dn)
patient_dt <- co19_T200DT[, .(first_covid=min(RECU_FR_DD)), by=.(JID, SEX_TP_CD, AGE_cal, INSUP_TP_CD)]
patient_info <- as.data.frame(patient_dt)

############################################
############## print Info.txt ##############
############################################
sink("./resultsCNLLS/Info.txt")
cat("\nlength : test JID \n")
cat(length(testJID))

cat("\nlength : target JID, statin \n")
cat(length(age40JID))
cat("\nlength : target JID, anti \n")
cat(length(targetJID))
cat("\nlength : target JID, immuno \n")
cat(length(targetJID))

cat("\nlength : hospital JID, statin \n")
cat(length(intersect(age40JID, hospitalJID)))
cat("\nlength : hospital JID, anti \n")
cat(length(hospitalJID))
cat("\nlength : hospital JID, immuno \n")
cat(length(hospitalJID))

cat("\n nrow(patient_info) \n")
print(nrow(patient_info))

patient_info$AGE <- ifelse(patient_info$AGE_cal<=64, 0, 1)

# 열 삭제
# (def) SEX_TP_CD 1:male, 2:female, 9:etc
patient_info <- patient_info[!(patient_info$SEX_TP_CD == 9),]
print(nrow(patient_info))

patient_info$GENDER <- patient_info$SEX_TP_CD - 1

# (def) 건강보험 : medical insurance, 의료급여&보훈 : medical aid
patient_info <- patient_info[(patient_info$INSUP_TP_CD == 4 | patient_info$INSUP_TP_CD == 5 | patient_info$INSUP_TP_CD == 7),]
patient_info$MedInfo <- ifelse(patient_info$INSUP_TP_CD == 4, 0, 1)
print(nrow(patient_info))

patient_info <- as.data.frame(patient_info[,c("JID", "first_covid", "AGE_cal", "AGE", "GENDER", "MedInfo")])

cat("All statin\n")
cat("Age table \n")
print(table(patient_info[patient_info$JID %in% age40JID,]$AGE_cal))
print(table(patient_info[patient_info$JID %in% age40JID,]$AGE))
cat("Gender table \n")
print(table(patient_info[patient_info$JID %in% age40JID,]$GENDER))
cat("Medical Info table \n")
print(table(patient_info[patient_info$JID %in% age40JID,]$MedInfo))

cat("\nAll anti and immuno\n")
cat("Age table \n")
print(table(patient_info$AGE_cal))
print(table(patient_info$AGE))
cat("Gender table \n")
print(table(patient_info$GENDER))
cat("Medical Info table \n")
print(table(patient_info$MedInfo))

cat("\n\nHospital statin\n")
cat("Age table \n")
print(table(patient_info[patient_info$JID %in% intersect(age40JID, hospitalJID),]$AGE_cal))
print(table(patient_info[patient_info$JID %in% intersect(age40JID, hospitalJID),]$AGE))
cat("Gender table \n")
print(table(patient_info[patient_info$JID %in% intersect(age40JID, hospitalJID),]$GENDER))
cat("Medical Info table \n")
print(table(patient_info[patient_info$JID %in% intersect(age40JID, hospitalJID),]$MedInfo))

cat("\nHospital  anti and immuno\n")
cat("Age table \n")
print(table(patient_info[patient_info$JID %in% hospitalJID, ]$AGE_cal))
print(table(patient_info[patient_info$JID %in% hospitalJID, ]$AGE))
cat("Gender table \n")
print(table(patient_info[patient_info$JID %in% hospitalJID, ]$GENDER))
cat("Medical Info table \n")
print(table(patient_info[patient_info$JID %in% hospitalJID, ]$MedInfo))

sink()


####################################################################
#####################      Function Def      #######################
####################################################################
verbose.period = 50

gen_dummies <- function(codeBook, study = NULL, Bdays = 0){
  time1 = Sys.time()
  df_rule <- NULL
  print("[START] > Bdays for loop; jid in patient_info$JID \n")
  if (Bdays!=0) {
    for (jid in patient_info$JID) {
      verbose = (jid %% verbose.period == 1)
      
      df_temp <- co19_twjhe200_trans_dn[which(co19_twjhe200_trans_dn$JID == jid),]
      
      if(verbose) cat(sprintf(" [V] nrow(df_temp) = %d \n", nrow(df_temp)))
      if(!nrow(df_temp)) next
      
      first <- patient_info[patient_info$JID == jid,]$first_covid
      exposure_rule <- (df_temp$RECU_FR_DD <= first) & (df_temp$RECU_FR_DD >= first - Bdays)
      if (unique(codeBook$codeType) == "ATC") {
        # ATC 는 d-240 ~ d- 90 or 120
        confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays) & (df_temp$RECU_FR_DD >= first - 240)
      } else{
        # 상병은 -3y ~ d- 90 or 120
        confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays) & df_temp$RECU_FR_DD >= first - 1096
      }
      
      # rule 추가가능 : 탐색 기준
      df_rule_temp <- data.frame("JID" = jid,
                                 "MID" = df_temp$MID,
                                 "EXPOSURE.itself" = exposure_rule,
                                 "exposure.and.outcome.related" = confounder_rule,
                                 "outcome.related" = confounder_rule,
                                 "x" = confounder_rule,
                                 "x.exposure.only" = confounder_rule)
      
      df_rule <- rbind(df_rule, df_rule_temp)
      df_temp <- NULL
      # print(colSums(df_rule[df_rule$JID ==jid, c(3,4)]))
    }
  }
  
  final <- data.frame(patient_info)
  print("[END] > Bdays for loop; jid in patient_info$JID\n")
  print("[START] > codeBook for loop; i in 1:norw(codeBook)\n")
  
  for (i in 1:nrow(codeBook)) {
    verbose = (i %% verbose.period == 1)
    if (verbose) cat(sprintf("%d\n",i))
    
    code_val <- codeBook[i,"code"]
    uniqueCode <- codeBook[i,"uniqueCode"]
    code_len <- nchar(code_val)
    if (verbose) cat("\n - * - * - * - * - * - * - \n")
    if (verbose) cat(sprintf(" (%d) uniqueCode [%s]\n", i, uniqueCode))
    
    if (!is.null(study)) {
      r.v <- codeBook[i, study]
      if (verbose) cat(sprintf(" - %s in %s.\n\n", r.v, study))
      
      # "x", "x.exposure.only" 없애는 코드
      #if (r.v %in% c("outcome.related", "x", "x.exposure.only")) {
      #  next
      #}
      
    }
    
    if (codeBook[i,"codeType"] == "ATC") {
      rule = (toupper(substr(ATC_mapping_df[ ,"ATC"],1,code_len)) == toupper(code_val))
      #--------- (if -2-) "ATC" mapping X ---------#
      if (sum(rule)==0){ 
        cat(sprintf(" ATC code %s : mapping X\n", uniqueCode ))
        next
      }
    }
    
    sF_split1 <- strsplit(codeBook[i,"searchFor"], "; ")
    
    if (verbose) print("[START] >> sF_split1 while loop;\n")
    
    col <- colnames(final)
    final$Code <- rep(0, nrow(final))
    colnames(final) <- c(col, uniqueCode)
    
    j = 1; dummy <- NULL
    while(!is.na(sF_split1[[1]][j])){
      sF_split2 <- strsplit(sF_split1[[1]][j], "/")
      
      df_name <- sF_split2[[1]][1]
      var_name <- sF_split2[[1]][2]
      df_all <- data.frame(get(df_name))
      
      #--------- df_rule 이 존재 : Bdays 설정 ---------#
      if (is.null(df_rule)){
        df <- df_all 
        # outcome이면 t200에서 특정 시점을 기준으로 자를 필요 없이, 다 가져와야 함.
      } else {
        if (r.v %in% names(df_rule)){ # names(df_rule) : JID, MID, EXPOSURE.itself, exposure.and.outcome.related
          df <- df_all[df_all$MID %in% df_rule[df_rule[,r.v]==1,"MID"], ]
        }
        else{
          df <- df_all
        }
      }
      #------------------------------------------------#
      
      if (verbose) cat(sprintf("[%d] before : %d, after : %d\n",j, nrow(df_all), nrow(df)))
      df_all <- NULL
      
      #--------- (if -1-) codeBook$codeType == "ATC" ---------#
      
      if (codeBook[i,"codeType"] == "ATC") {
        mapped_var <- toupper(ATC_mapping_df[rule, var_name])
        searchJID <- df[ifelse(toupper(df[,var_name]) %in% mapped_var, TRUE, FALSE),]$JID
        
      } else { #--------- (else -1-) codeBook$codeType != "ATC" ---------#
        searchJID <- df[ifelse(toupper(substr(df[,var_name],1,code_len)) == toupper(code_val), TRUE, FALSE),]$JID
        
      }
      j = j+1
      if (!length(searchJID)) {
        next()
      }
      final[final$JID %in% searchJID, uniqueCode] <- 1
      
      
    } # while loop exit
    if (verbose) print("[END] >> sF_split1 while loop;\n")
    
  }
  print("[END] > codeBook for loop; i in 1:norw(codeBook)\n")
  # [!] 추가함
  final[is.na(final)] <- 0
  
  time2 = Sys.time()
  cat("\n [Total Time] : ", time2-time1)
  
  return (data.frame(final))
}
save_freq <- function(df_name) {
  cat(" <----------------------------> \n")
  df <- get(sprintf("%s",df_name))
  cat(sprintf("  (Dim) # of JID = %d, code = %d \n", dim(df)[1], dim(df)[2]))
  
  code_freq <- colSums(df[,-excl.cols])
  code_freq_per <- round(code_freq/nrow(df[,-excl.cols]),6)
  
  cat(sprintf(" [save] %s frequency \n", df_name))
  write.csv(cbind(code_freq, code_freq_per), sprintf("./resultsCNLLS/frequency/%s_frequency.csv", df_name), row.names=T)
}

####################################################################
#####################       data frame       #######################
####################################################################
excl.cols = (1:ncol(patient_info))
# [1] outcomes (sheet4)
# outcomes code book 에 중복된 셀 존재
cat(" -1- outcomes (sheet4) \n")
covid_outcomes <- gen_dummies(code_outcomes_df)
covid_outcomes2 <- covid_outcomes[covid_outcomes$JID %in% age40JID, ]

# -- codewise frequency outcomes 
save_freq("covid_outcomes")
save_freq("covid_outcomes2")

# [3] comorbidties ICD10 (sheet2)
como_info_statin90 <- gen_dummies(code_ICD10_df,  "statin.study",90)
como_info_statin90 <- como_info_statin90[como_info_statin90$JID %in% age40JID, ]

como_info_Anticoagulants90 <- gen_dummies(code_ICD10_df, "antiplatelet.anticoagulant.study",90)
como_info_immunosuppressant90 <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 90)

como_info_statin120 <- gen_dummies(code_ICD10_df, "statin.study",120)
como_info_statin120 <- como_info_statin120[como_info_statin120$JID %in% age40JID, ]

como_info_Anticoagulants120 <- gen_dummies(code_ICD10_df, "antiplatelet.anticoagulant.study",120)
como_info_immunosuppressant120 <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 120)

# -- codewise frequency comorbidties -- #
cat("\n -3- comorbidties ICD10 (sheet2)  \n")
save_freq("como_info_statin90")
save_freq("como_info_statin120")

save_freq("como_info_Anticoagulants90")
save_freq("como_info_Anticoagulants120")

save_freq("como_info_immunosuppressant90")
save_freq("como_info_immunosuppressant120")

# [4] medications ATC code (sheet3)
# [code book(code_ATC)의 code column] = [ATC_mapping_df의 ATC코드 column] 
# [ATC_mapping_df의 주성분코드] = [T530의 GNL_CD]
# [ATC_mapping_df의 제품코드]  = T530의 DIV_CD  (T300의 DIV_CD도 일부 존재)
ATC_med_statin90 <- gen_dummies(code_ATC_df, "statin.study",90)
ATC_med_statin90 <- ATC_med_statin90[ATC_med_statin90$JID %in% age40JID, ]

ATC_med_Anticoagulants90 <- gen_dummies(code_ATC_df, "antiplatelet.anticoagulant.study",90)
ATC_med_immunosuppressant90 <- gen_dummies(code_ATC_df, "immune.suppressant.study", 90)

ATC_med_statin120 <- gen_dummies(code_ATC_df, "statin.study",120)
ATC_med_statin120 <- ATC_med_statin120[ATC_med_statin120$JID %in% age40JID, ]

ATC_med_Anticoagulants120 <- gen_dummies(code_ATC_df, "antiplatelet.anticoagulant.study",120)
ATC_med_immunosuppressant120 <- gen_dummies(code_ATC_df, "immune.suppressant.study", 120)

cat(" ATC_med R data save \n")

cat(" -4- medication ATC (sheet3) \n")
save_freq("ATC_med_statin90")
save_freq("ATC_med_statin120")

save_freq("ATC_med_Anticoagulants90")
save_freq("ATC_med_Anticoagulants120")

save_freq("ATC_med_immunosuppressant90")
save_freq("ATC_med_immunosuppressant120")

cat(" codewise data (finish) \n")

# [2] COVID19 code (sheet5)
covid_diag <- gen_dummies(codeBook=code_covid_df)

# -- codewise frequency covid19
#cat(" -4- covid (sheet5) \n")
save_freq("covid_diag")

#######################
sink("./resultsCNLLS/frequency/Confirm_vs_code.txt")
for (j in (ncol(patient_info)+2):ncol(covid_diag)) {
  cat(sprintf(" → Confirm by HIRA (Y) versus code [%s]\n", colnames(covid_diag)[j]))
  print(table("Y" = covid_diag[ ,"ID1001.Y"], "code" = covid_diag[ ,j]))
  cat("\n")
}
sink()
#######################

# covid_diag_agg
covid_diag_agg = covid_diag[,-excl.cols]
colnames(covid_diag_agg)[colnames(covid_diag_agg) == "Y"] = "Confirm"

# 각 스터디에 대해서
make_df <- function(como_df, ATC_df, outcome_df, study) {
  # como_info
  como_df2 = como_df[ ,-excl.cols]
  #head(como_df2)
  if (ncol(como_df2) != nrow(code_ICD10_df)) stop("[1] Code names do not match!!!")
  #print("ok")
  vec_or = function(x) {ifelse(sum(x) > 0,1,0)}
  
  como_df_agg = t( apply(como_df2, 1, function(x) tapply(x, code_ICD10_df$name, vec_or) ) )
  como_df_agg <- data.frame(como_df_agg)
  
  # ATC_med
  ATC_df2 = ATC_df[ ,-excl.cols]
  code_ATC_df_tmp = code_ATC_df[code_ATC_df$uniqueCode %in% colnames(ATC_df2), ]
  
  if (ncol(ATC_df2) != nrow(code_ATC_df_tmp)) stop("[2] Code names do not match!!!")

  ATC_df_agg = t( apply(ATC_df2, 1, function(x) tapply(x, code_ATC_df_tmp$name, vec_or) ) )
  ATC_df_agg <- data.frame(ATC_df_agg)
  
  # covid_outcomes_agg
  if (ncol(outcome_df[ ,-excl.cols]) != nrow(code_outcomes_df)) stop("Code names do not match!!!")
  vec_or = function(x) {ifelse(sum(x) > 0,1,0)}
  outcomes_df_agg = t( apply(outcome_df[ ,-excl.cols], 1, function(x) tapply(x, code_outcomes_df$name, vec_or) ) )
  outcomes_df_agg <- data.frame(outcomes_df_agg)
  
  x <- cbind(como_df_agg, ATC_df_agg)
  y <- outcomes_df_agg
  
  asso.mat = matrix(0, nrow=ncol(x), ncol=ncol(y))
  
  rownames(asso.mat) <- colnames(x)
  colnames(asso.mat) <- colnames(y)
  
  #print("ok")
  for (i in 1:ncol(x)) {
    for (j in 1:ncol(y)) {
      myx = factor(x[,i], levels=c(0,1))
      myy = factor(y[,j], levels=c(0,1))
      res = chisq.test( xtabs( ~ myx + myy) )$p.value
      asso.mat[i,j] = res
    }
  }
  z <- cbind(x,y)
  name_freq <- colSums(z)
  name_freq_per <- round(colMeans(z),6)
  cat(sprintf(" (1) [save] %s frequency \n", study))
  #print(colMeans(x))
  write.csv(cbind(name_freq, name_freq_per), sprintf("./resultsCNLLS/frequency2/name_freq_%s.csv", study))

  return (data.frame(cbind(como_df[ ,excl.cols],x,y)))
}

#### make df ####
# [1] statin 90
print(" <-------------- statin 90 --------------> \n")
statin90 <- make_df(como_df = como_info_statin90, 
                    ATC_df = ATC_med_statin90,
                    outcome_df=covid_outcomes2,
                    study = "statin90")

# [2] statin 120
print(" <-------------- statin 120 --------------> \n")
statin120 <- make_df(como_df = como_info_statin120, 
                     ATC_df = ATC_med_statin120,
                     outcome_df=covid_outcomes2,
                     study = "statin120")

# [3] antiplatelet.anticoagulant.study 90
print(" <-------------- anti 90 --------------> \n")
Anticoagulants90 <- make_df(como_df = como_info_Anticoagulants90,
                    ATC_df = ATC_med_Anticoagulants90,
                    outcome_df=covid_outcomes,
                    study = "Anticoagulants90")

# [4] antiplatelet.anticoagulant.study 120
print(" <-------------- anti 120 --------------> \n")
Anticoagulants120 <- make_df(como_df = como_info_Anticoagulants120,
                     ATC_df = ATC_med_Anticoagulants120,
                     outcome_df=covid_outcomes,
                     study = "Anticoagulants120")

# [5] immune 90
print(" <-------------- immune 90 --------------> \n")
immunosuppressant90 <- make_df(como_df = como_info_immunosuppressant90,
                    ATC_df = ATC_med_immunosuppressant90,
                    outcome_df=covid_outcomes,
                    study = "immunosuppressant90")

# [6] immune 120
print(" <-------------- immune 120 --------------> \n")
immunosuppressant120 <- make_df(como_df = como_info_immunosuppressant120,
                     ATC_df = ATC_med_immunosuppressant120,
                     outcome_df=covid_outcomes,
                     study = "immunosuppressant120")

frequency3 <- function(df, study.exposure){
  #study.exposure <- as.character(study[study$type == "Exposure","var"])
  #study.exposure <- as.character(study.exposure)
  for (EachName in colnames(df)[3:ncol(df)]) {
    cat(sprintf(" --- %s --- \n",EachName))
    print(table("Exposure"=df[,study.exposure], df[,EachName]))
    cat("\n\n")
  }
}

sink("./resultsCNLLS/frequency/check-df1.txt")
cat(" ** STATIN 90 ** \n\n")
frequency3(statin90, "statin")
cat("\n\n ** STATIN 120 ** \n\n")
frequency3(statin120, "statin")
cat("\n\n ** Anticoagulants 90 ** \n\n")
frequency3(Anticoagulants90, "Anticoagulants")
cat("\n\n ** Anticoagulants 120 ** \n\n")
frequency3(Anticoagulants120, "Anticoagulants")
cat("\n\n ** Immunosuppressant 90 ** \n\n")
frequency3(immunosuppressant90, "immunosuppressant")
cat("\n\n ** Immunosuppressant 120 ** \n\n")
frequency3(immunosuppressant120, "immunosuppressant")
sink()

#### Modeling 순서대로 하자 ####
#### Statin ####
# Lipid Lowering agents excluding statin 에서 including statin 은 왜 exposure.itself? check.
statin90$Lipid.lowering.agents.excluding.statin <- statin90$Lipid.lowering.agents.including.statin - statin90$Lipid.lowering.agents.including.statin * statin90$statin2
statin120$Lipid.lowering.agents.excluding.statin <- statin120$Lipid.lowering.agents.including.statin - statin120$Lipid.lowering.agents.including.statin * statin120$statin2

statin90$V_cancer <- 0 ; statin90[statin90$JID %in% age40.cancer_jid,]$V_cancer <- 1
statin120$V_cancer <- 0 ; statin120[statin120$JID %in% age40.cancer_jid,]$V_cancer <- 1

# malignancy
statin90$Malignancy <- statin90$Malignancy * statin90$V_cancer
statin120$Malignancy <- statin120$Malignancy * statin120$V_cancer

statin90$V_cancer <- NULL
statin120$V_cancer <- NULL

statin90$first_covid <- NULL
statin120$first_covid <- NULL

statin.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA")
statin.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA", "Use.of.mechanical.ventilation.ICU.admission")

statin90$outcome1 <- ifelse(rowSums(statin90[,statin.outcome1])>0,1,0)
statin120$outcome1 <-  ifelse(rowSums(statin120[,statin.outcome1])>0,1,0)

statin90$Use.of.mechanical.ventilation.ICU.admission <- as.numeric(statin90$Use.of.mechanical.ventilation + statin90$ICU.admission > 0)
statin120$Use.of.mechanical.ventilation.ICU.admission <- as.numeric(statin120$Use.of.mechanical.ventilation + statin120$ICU.admission > 0)

statin90$Hypertension <- statin90$Hypertension * statin90$anitihypertensive.use
statin120$Hypertension <- statin120$Hypertension * statin120$anitihypertensive.use

statin90$arrhythmias.or.antiarrhythmias.use <- as.numeric(statin90$anti.arrhythmias.use+statin90$Other.arrhythmias>0)
statin120$arrhythmias.or.antiarrhythmias.use <- as.numeric(statin120$anti.arrhythmias.use+statin120$Other.arrhythmias>0)

statin90$autoimmune.disease <- statin90$autoimmune.disease * statin90$immunosuppressant
statin120$autoimmune.disease <- statin120$autoimmune.disease * statin120$immunosuppressant

# statin variable
statin.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$statin.study == "exposure.and.outcome.related",
                                            c("name", "statin.study")])$name,
                       unique(code_ATC_df[code_ATC_df$statin.study == "exposure.and.outcome.related",
                                          c("name", "statin.study")])$name)

statin.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$statin.study == "outcome.related",
                                         c("name", "statin.study")])$name,
                    unique(code_ATC_df[code_ATC_df$statin.study == "outcome.related",
                                       c("name", "statin.study")])$name)

statin.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", statin.Ex.Out.Rel), gsub("[-,/ ]", ".", statin.Out.Rel), "Lipid.lowering.agents.excluding.statin", "arrhythmias.or.antiarrhythmias.use")
statin.confounder <- setdiff(statin.confounder, c("anitihypertensive.use", "statin2", "Lipid.lowering.agents.including.statin","anti.arrhythmias.use", "Other.arrhythmias"))

# “TIA”, “Stroke”, “Coronary artery disease ”, “Atherosclerosis”, “Peripheral vascular disease”
statin90$TIA.Stroke.Coronary.Atherosclerosis.Peripheral <- 
  ifelse(statin90$stroke.or.TIA.history + statin90$Coronary.artery.disease + statin90$Peripheral.vascular.disease>0,1,0)

statin120$TIA.Stroke.Coronary.Atherosclerosis.Peripheral <- 
  ifelse(statin120$stroke.or.TIA.history + statin120$Coronary.artery.disease + statin120$Peripheral.vascular.disease>0,1,0)

statin.Emodifier <- c("AGE", "GENDER", "Hyperlipidemia", "Hypertension", "Diabetes.mellitus", "TIA.Stroke.Coronary.Atherosclerosis.Peripheral")

statin.exposure <- "statin"

statin.study <- rbind(data.frame(var = statin.exposure, type = "Exposure"),
                      data.frame(var = statin.confounder, type = rep("Confounder", length(statin.confounder))),
                      data.frame(var = statin.outcome1, type = rep("Outcome1", length(statin.outcome1))),
                      data.frame(var = statin.outcome2, type = rep("Outcome2", length(statin.outcome2))), 
                      data.frame(var = statin.Emodifier, type = rep("Effect.Modifier",length(statin.Emodifier))))

#### anticoagulants.antiplatelets ####
Anticoagulants90$first_covid <- NULL
Anticoagulants120$first_covid <- NULL

# malignancy
Anticoagulants90$V_cancer <- 0
Anticoagulants120$V_cancer <- 0
Anticoagulants90[Anticoagulants90$JID %in% cancer_jid,]$V_cancer <- 1
Anticoagulants120[Anticoagulants120$JID %in% cancer_jid,]$V_cancer <- 1

Anticoagulants90$Malignancy <- Anticoagulants90$Malignancy * Anticoagulants90$V_cancer
Anticoagulants120$Malignancy <- Anticoagulants120$Malignancy * Anticoagulants120$V_cancer

Anticoagulants90$V_cancer <- NULL
Anticoagulants120$V_cancer <- NULL

# antiplatelets variable
Anticoagulants.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$antiplatelet.anticoagulant.study == "exposure.and.outcome.related",
                                            c("name", "antiplatelet.anticoagulant.study")])$name,
                       unique(code_ATC_df[code_ATC_df$antiplatelet.study == "exposure.and.outcome.related",
                                          c("name", "antiplatelet.anticoagulant.study")])$name)

Anticoagulants.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$antiplatelet.anticoagulant.study == "outcome.related",
                                         c("name", "antiplatelet.anticoagulant.study")])$name,
                    unique(code_ATC_df[code_ATC_df$antiplatelet.anticoagulant.study == "outcome.related",
                                       c("name", "antiplatelet.anticoagulant.study")])$name)

Anticoagulants.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", Anticoagulants.Ex.Out.Rel), gsub("[-,/ ]", ".", Anticoagulants.Out.Rel), "arrhythmias.or.antiarrhythmias.use")
Anticoagulants.confounder <- setdiff(Anticoagulants.confounder, c("statin", "anitihypertensive.use", "anti.arrhythmias.use", "Other.arrhythmias"))

Anticoagulants.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA", "Thromboembolism", 
                     "ischemic.stroke.and.TIA", "TIA.transient.ischemic.attack.", "Stroke", "hemorragic.stroke", "Use.of.mechanical.ventilation.ICU.admission")

Anticoagulants90$Use.of.mechanical.ventilation.ICU.admission <- as.numeric(Anticoagulants90$Use.of.mechanical.ventilation + Anticoagulants90$ICU.admission > 0)
Anticoagulants120$Use.of.mechanical.ventilation.ICU.admission <- as.numeric(Anticoagulants120$Use.of.mechanical.ventilation + Anticoagulants120$ICU.admission > 0)

Anticoagulants90$Hypertension <- Anticoagulants90$Hypertension * Anticoagulants90$anitihypertensive.use
Anticoagulants120$Hypertension <- Anticoagulants120$Hypertension * Anticoagulants120$anitihypertensive.use

Anticoagulants90$arrhythmias.or.antiarrhythmias.use <- as.numeric(Anticoagulants90$anti.arrhythmias.use+Anticoagulants90$Other.arrhythmias>0)
Anticoagulants120$arrhythmias.or.antiarrhythmias.use <- as.numeric(Anticoagulants120$anti.arrhythmias.use+Anticoagulants120$Other.arrhythmias>0)

Anticoagulants90$autoimmune.disease <- Anticoagulants90$autoimmune.disease * Anticoagulants90$immunosuppressant
Anticoagulants120$autoimmune.disease <- Anticoagulants120$autoimmune.disease * Anticoagulants120$immunosuppressant

# - outcome1 - 
# "myocardial infarction, ischemic stroke and TIA", "Thromboembolism"
# "bleeding", "transfusion"
Anticoagulants90$myocardial.storke.TIA.Thromboembolism <- ifelse(Anticoagulants90$myocardial.infarction.ischemic.stroke.and.TIA + Anticoagulants90$Thromboembolism>0,1,0)
Anticoagulants90$bleeding.transfusion <- ifelse(Anticoagulants90$bleeding + Anticoagulants90$transfusion>0,1,0)

Anticoagulants120$myocardial.storke.TIA.Thromboembolism <- ifelse(Anticoagulants120$myocardial.infarction.ischemic.stroke.and.TIA + Anticoagulants120$Thromboembolism>0,1,0)
Anticoagulants120$bleeding.transfusion <- ifelse(Anticoagulants120$bleeding + Anticoagulants120$transfusion>0,1,0)

Anticoagulants.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.storke.TIA.Thromboembolism", "bleeding.transfusion", "Use.of.mechanical.ventilation.ICU.admission")

Anticoagulants90$outcome1 <- ifelse(rowSums(Anticoagulants90[,Anticoagulants.outcome1])>0,1,0)
Anticoagulants120$outcome1 <- ifelse(rowSums(Anticoagulants120[,Anticoagulants.outcome1])>0,1,0)

# “TIA”, “Stroke”, “Coronary artery disease ”, “Peripheral vascular disease”
# “Atrial Fibrillation”, “Thromboembolism” 
Anticoagulants90$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(Anticoagulants90$stroke.or.TIA.history + Anticoagulants90$Coronary.artery.disease + Anticoagulants90$Peripheral.vascular.disease>0,1,0)
Anticoagulants90$Atrial.Thromboembolism <- ifelse(Anticoagulants90$Atrial.fibrillation + Anticoagulants90$Thromboembolism>0,1,0)

Anticoagulants120$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(Anticoagulants120$stroke.or.TIA.history + Anticoagulants120$Coronary.artery.disease + Anticoagulants120$Peripheral.vascular.disease>0,1,0)
Anticoagulants120$Atrial.Thromboembolism <- ifelse(Anticoagulants120$Atrial.fibrillation + Anticoagulants120$Thromboembolism>0,1,0)

Anticoagulants.Emodifier <- c("AGE", "GENDER", "Atrial.Thromboembolism", "TIA.Stroke.Coronary.Peripheral")
Anticoagulants.exposure <- "Anticoagulants"

Anticoagulants.study <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                      data.frame(var = Anticoagulants.confounder, type = rep("Confounder", length(Anticoagulants.confounder))),
                      data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                      data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                      data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))

#### immunosuppressant ####
immunosuppressant90$first_covid <- NULL
immunosuppressant120$first_covid <- NULL
immunosuppressant90$statin <- NULL
immunosuppressant120$statin <- NULL

immunosuppressant90$V_cancer <- 0
immunosuppressant120$V_cancer <- 0
immunosuppressant90[immunosuppressant90$JID %in% cancer_jid,]$V_cancer <- 1
immunosuppressant120[immunosuppressant120$JID %in% cancer_jid,]$V_cancer <- 1

immunosuppressant90$Malignancy <- immunosuppressant90$Malignancy * immunosuppressant90$V_cancer
immunosuppressant120$Malignancy <- immunosuppressant120$Malignancy * immunosuppressant120$V_cancer

immunosuppressant90$V_cancer <- NULL
immunosuppressant120$V_cancer <- NULL 

immunosuppressant90$autoimmune.disease <- immunosuppressant90$autoimmune.disease * immunosuppressant90$immunosuppressant.for.subanalysis
immunosuppressant120$autoimmune.disease <- immunosuppressant120$autoimmune.disease * immunosuppressant120$immunosuppressant.for.subanalysis

immuno_status90 <- as.numeric( sum(como_info_immunosuppressant90$ID2374.B20, como_info_immunosuppressant90$ID2375.B21, como_info_immunosuppressant90$ID2374.B22, como_info_immunosuppressant90$ID2375.B23, como_info_immunosuppressant90$ID2374.B24,
                                                    como_info_immunosuppressant90$ID2296.z94, como_info_immunosuppressant90$ID2297.T86,
                                                    como_info_immunosuppressant90$ID2285.K50, como_info_immunosuppressant90$ID2286.K51, como_info_immunosuppressant90$ID2287.K52) >= 1)
immuno_status120 <- as.numeric( sum(como_info_immunosuppressant120$ID2374.B20, como_info_immunosuppressant120$ID2375.B21, como_info_immunosuppressant120$ID2374.B22, como_info_immunosuppressant120$ID2375.B23, como_info_immunosuppressant120$ID2374.B24,
                                                     como_info_immunosuppressant120$ID2296.z94, como_info_immunosuppressant120$ID2297.T86,
                                                     como_info_immunosuppressant120$ID2285.K50, como_info_immunosuppressant120$ID2286.K51, como_info_immunosuppressant120$ID2287.K52) >= 1)

immunosuppressant90$immuno.status <- immuno_status90
immunosuppressant120$immuno.status <- immuno_status120

# immunosuppressant variable
immunosuppressant.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$immune.suppressant.study == "exposure.and.outcome.related",
                                            c("name", "immune.suppressant.study")])$name,
                       unique(code_ATC_df[code_ATC_df$immune.suppressant.study == "exposure.and.outcome.related",
                                          c("name", "immune.suppressant.study")])$name)

immunosuppressant.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$immune.suppressant.study == "outcome.related",
                                         c("name", "immune.suppressant.study")])$name,
                    unique(code_ATC_df[code_ATC_df$immune.suppressant.study == "outcome.related",
                                       c("name", "immune.suppressant.study")])$name)

immunosuppressant.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", immunosuppressant.Ex.Out.Rel), gsub("[-,/ ]", ".", immunosuppressant.Out.Rel), "arrhythmias.or.antiarrhythmias.use", "immuno.status")
immunosuppressant.confounder <- setdiff(immunosuppressant.confounder, c("statin", "Malignancy", "anitihypertensive.use", "HIV", "Antineoplastic", "anti.arrhythmias.use", "Other.arrhythmias"))

immunosuppressant.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission")
immunosuppressant.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "Use.of.mechanical.ventilation.ICU.admission")

immunosuppressant90$outcome1 <- ifelse(rowSums(immunosuppressant90[,immunosuppressant.outcome1])>0,1,0)
immunosuppressant120$outcome1 <- ifelse(rowSums(immunosuppressant120[,immunosuppressant.outcome1])>0,1,0)

immunosuppressant.Emodifier <- c("AGE", "GENDER", "immuned.deficiency.including.Cacner.HIV", "autoimmune.disease")

immunosuppressant90$immuned.deficiency.including.Cacner.HIV <- as.numeric(immunosuppressant90$HIV + immunosuppressant90$autoimmune.disease + immunosuppressant90$Antineoplastic + immunosuppressant90$Malignancy > 1)
immunosuppressant120$immuned.deficiency.including.Cacner.HIV <- as.numeric(immunosuppressant120$HIV + immunosuppressant120$autoimmune.disease + immunosuppressant120$Antineoplastic + immunosuppressant120$Malignancy > 1)

immunosuppressant90$Use.of.mechanical.ventilation.ICU.admission <- as.numeric(immunosuppressant90$Use.of.mechanical.ventilation + immunosuppressant90$ICU.admission > 0)
immunosuppressant120$Use.of.mechanical.ventilation.ICU.admission <- as.numeric(immunosuppressant120$Use.of.mechanical.ventilation + immunosuppressant120$ICU.admission > 0)

immunosuppressant90$Hypertension <- immunosuppressant90$Hypertension * immunosuppressant90$anitihypertensive.use
immunosuppressant120$Hypertension <- immunosuppressant120$Hypertension * immunosuppressant120$anitihypertensive.use

immunosuppressant90$arrhythmias.or.antiarrhythmias.use <- as.numeric(immunosuppressant90$anti.arrhythmias.use+immunosuppressant90$Other.arrhythmias>0)
immunosuppressant120$arrhythmias.or.antiarrhythmias.use <- as.numeric(immunosuppressant120$anti.arrhythmias.use+immunosuppressant120$Other.arrhythmias>0)

immunosuppressant.exposure <- "immunosuppressant"

immunosuppressant.study <- rbind(data.frame(var = immunosuppressant.exposure, type = "Exposure"),
                      data.frame(var = immunosuppressant.confounder, type = rep("Confounder", length(immunosuppressant.confounder))),
                      data.frame(var = immunosuppressant.outcome1, type = rep("Outcome1", length(immunosuppressant.outcome1))),
                      data.frame(var = immunosuppressant.outcome2, type = rep("Outcome2", length(immunosuppressant.outcome2))), 
                      data.frame(var = immunosuppressant.Emodifier, type = rep("Effect.Modifier",length(immunosuppressant.Emodifier))))

sink("./resultsCNLLS/frequency/check-df2.txt")
cat(" ** STATIN 90 ** \n\n")
frequency3(statin90, "statin")
cat("\n\n ** STATIN 120 ** \n\n")
frequency3(statin120, "statin")
cat("\n\n ** Anticoagulants 90 ** \n\n")
frequency3(Anticoagulants90, "Anticoagulants")
cat("\n\n ** Anticoagulants 120 ** \n\n")
frequency3(Anticoagulants120, "Anticoagulants")
cat("\n\n ** Immunosuppressant 90 ** \n\n")
frequency3(immunosuppressant90, "immunosuppressant")
cat("\n\n ** Immunosuppressant 120 ** \n\n")
frequency3(immunosuppressant120, "immunosuppressant")
sink()

ps_summary <- function(exposure, ps, wt, name){
  User <- ps[exposure==0]
  NonUser <- ps[exposure==1]
  
  cat(sprintf("- User \n (<0.01) : %d\n (>0.99) : %d\n", 
              sum(User<0.01), sum(User > 0.99) ))
  
  cat(sprintf("- NonUser \n (<0.01) : %d\n (>0.99) : %d\n", 
              sum(NonUser<0.01), sum(NonUser>0.99) )) 
  
  cat("\n\n")
  cat(" - weight dist\n\n")
  print(summary(wt[exposure==0]))
  print(summary(wt[exposure==1]))
  cat("\n\n")
  
  path_name1 <- sprintf("./resultsCNLLS/ps/%s_User.csv", name)
  path_name2 <- sprintf("./resultsCNLLS/ps/%s_NonUser.csv", name)
  
  UserFreq <- data.frame(table(round(User,4)))
  NonUserFreq <- data.frame(table(round(NonUser,4)))
  
  write.csv(UserFreq, path_name1)
  write.csv(NonUserFreq, path_name2)
}
outcome.model <- function(Y="outcome1", wt, df, study, name){
  save_path <- sprintf("./resultsCNLLS/model/coef_%s/", name)
  dir.create(save_path, showWarnings=FALSE)
  cat("\n\n ================= \n ================= \n\n")
  cat(name); cat("\n"); cat(Y)
  cat("\n\n ================= \n ================= \n\n")
  
  study.exposure <- as.character(study[study$type == "Exposure","var"])
  study.confounder <- as.character(study[study$type == "Confounder","var"])
  study.Effect.Modifier <- as.character(study[study$type == "Effect.Modifier", "var"])
  
  Exposure <- df[,study.exposure]
  Confounder <- df[,study.confounder]
  Outcome <- df[,as.character(Y)]
  X <- df[,c(study.exposure, "AGE", "GENDER", "MedInfo", "Hypertension", "Diabetes.mellitus")]
  
  # [0-1] outcome ~ exposure (NAIVE)
  fit0.1 <- glm(Outcome ~ Exposure, family="binomial")
  
  cat(" [0-1] Naive Primary Outcome Model \n\n")
  print(summary(fit0.1))
  
  Coef_0.1 <- data.frame("coef" = exp(summary(fit0.1)$coef[,1]), # 계수  
                         "s.e" = summary(fit0.1)$coef[,2], # 표준 오차
                         "lower.ci" = exp(summary(fit0.1)$coef[,1] - 1.96 * summary(fit0.1)$coef[,2]), # CI
                         "upper.ci" = exp(summary(fit0.1)$coef[,1] + 1.96 * summary(fit0.1)$coef[,2]),
                         "p-value" = summary(fit0.1)$coef[,4])
  path0.1 <- paste0(save_path, sprintf("%s_Naive.csv", Y))
  write.csv(Coef_0.1, path0.1)
  
  # [0-2] Naive Multivariate model  outcome1 ~ exposure + confounder : wt (X)
  fit0.2 <- glm(Outcome~., data=X, family="binomial")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [0-2] Naive Multivariate Primary Outcome Model \n\n")
  print(summary(fit0.2))
  
  Coef_0.2 <- data.frame("coef" = exp(summary(fit0.2)$coef[,1]), # 계수  
                         "s.e" = summary(fit0.2)$coef[,2], # 표준 오차
                         "lower.ci" = exp(summary(fit0.2)$coef[,1] - 1.96 * summary(fit0.2)$coef[,2]), # CI
                         "upper.ci" = exp(summary(fit0.2)$coef[,1] + 1.96 * summary(fit0.2)$coef[,2]),
                         "p-value" = summary(fit0.2)$coef[,4])
  path0.2 <- paste0(save_path, sprintf("%s_MultiNaive.csv", Y))
  write.csv(Coef_0.2, path0.2)
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  
  # [2] outcome model (main)
  #   (a) outcome1 ~ exposure (wt 1a)
  fit2a <- glm(Outcome~Exposure, weight = wt, family="binomial")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [2] Primary Outcome Model (a)\n\n")
  print(summary(fit2a))
  cat("\n\n")
  
  Coef_2a <- data.frame("coef" = exp(summary(fit2a)$coef[,1]), # 계수  
                        "s.e" = summary(fit2a)$coef[,2], # 표준 오차
                        "lower.ci" = exp(summary(fit2a)$coef[,1] - 1.96 * summary(fit2a)$coef[,2]), # CI
                        "upper.ci" = exp(summary(fit2a)$coef[,1] + 1.96 * summary(fit2a)$coef[,2]),
                        "p-value" = summary(fit2a)$coef[,4])
  
  path2a <- paste0(save_path, sprintf("%s_IPTW.csv", Y))
  write.csv(Coef_2a, path2a)
  
  # ---------- table 2 ---------- #
  # 6. Risk of adverse clinical outcomes associated with Exposure ~ .
  # (1) # of patients (2) # of events (3) Event Rates (%)
  df.User <- df[Exposure==1,]
  df.NonUser <- df[Exposure==0,]
  
  OutcomeTable <- data.frame(Index=c("NonUser", "User"),
                        N_Patients=c(nrow(df.NonUser), nrow(df.User)),
                        N_Events=c(sum(df.NonUser[, Y]),sum(df.User[, Y])),
                        EventRates=c(sum(df.NonUser[, Y])/nrow(df.NonUser), sum(df.User[, Y])/nrow(df.User) ) )
  print(OutcomeTable)
  for (v in study.Effect.Modifier){
    cat(sprintf("\n\n [3] Effect Modifier Primary Outcome Model : %s \n\n", v))
    #cat(sprintf(" # of patients = %d \n "))
    
    EffectModifier <- df[,c(study.exposure, v)]
    EffectModifier[,"A*V"] <- Exposure * df[,v]
    
    df.UserO <- df.User[df.User[,v]==1,]
    df.UserX <- df.User[df.User[,v]==0,]
    
    df.NonUserO <- df.NonUser[df.NonUser[,v]==1,]
    df.NonUserX <- df.NonUser[df.NonUser[,v]==0,]
    
    tmpTable <- data.frame(Index=c(paste0("User:",v,":O"), paste0("User:",v,":X"), 
                                   paste0("NonUser:",v,":O"), paste0("NonUser:",v,":X")),
                           N_Patients=c( nrow(df.UserO), nrow(df.UserX), 
                                         nrow(df.NonUserO), nrow(df.NonUserX)),
                           N_Events=c( sum(df.UserO[, Y]), sum(df.UserX[, Y]), 
                                       sum(df.NonUserO[, Y]), sum(df.NonUserX[, Y])),
                           EventRates=c( sum(df.UserO[, Y])/nrow(df.UserO), sum(df.UserX[, Y])/nrow(df.UserX),
                                         sum(df.NonUserO[, Y])/nrow(df.NonUserO), sum(df.NonUserX[, Y])/nrow(df.NonUserX)) )
    OutcomeTable <- rbind(OutcomeTable, tmpTable)
    print(tmpTable)
    # outcome1 ~ exposure + v + exposure*v (iptw.wt)
    fit3_v <- glm(Outcome~., data=EffectModifier, weight =wt, family="binomial")
    print(summary(fit3_v))
    Coef_3_v <- data.frame("coef" = exp(summary(fit3_v)$coef[,1]), # 계수  
                           "s.e" = summary(fit3_v)$coef[,2], # 표준 오차
                           "lower.ci" = exp(summary(fit3_v)$coef[,1] - 1.96 * summary(fit3_v)$coef[,2]), # CI
                           "upper.ci" = exp(summary(fit3_v)$coef[,1] + 1.96 * summary(fit3_v)$coef[,2]),
                           "p-value" = summary(fit3_v)$coef[,4])
    
    E.mod1 <- sprintf("%s_EffectModifier_%s.csv", Y, v)
    E.mod2 <- sprintf("Cov_%s_EffectModifier_%s.csv", Y,v)
    write.csv(Coef_3_v, paste0(save_path, E.mod1))
    write.csv(vcov(fit3_v), paste0(save_path, E.mod2))
  }
  
  outcome_path <- paste0(save_path, Y,"_Incidence.csv")
  write.csv(OutcomeTable, outcome_path)
}

# Print Table #####
### IPTW table ###
cal.aSD <- function(g1.m, g2.m, g1.var, g2.var){return( (g1.m - g2.m)/ sqrt(g1.var/2+g2.var/2) )}
#### table 1,2. print ####
print_table <- function(df.all, wt, df.study, save_name){
  
  agebreaks <- c(0, 30, 40, 50, 60, 70, 80, 90, 500)
  agelabels <- c("30-", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")
  
  df.all$AGE2 = cut(df.all$AGE_cal, breaks = agebreaks,
                    right = FALSE, labels = agelabels)
  
  df.all <- data.frame(df.all)
  
  exposure <- as.character( df.study[df.study$type=="Exposure","var"] )
  confounder <- as.character( df.study[df.study$type=="Confounder","var"] )
  
  df.User <- df.all[df.all[,exposure]==1,]
  df.NonUser <- df.all[df.all[,exposure]==0,]
  
  User.wt <- wt[df.all[,exposure]==1]
  NonUser.wt <- wt[df.all[,exposure]==0]
  # ---------- table 1 ---------- #
  # 0. N
  N <- data.frame(Index="N", 
                  all=nrow(df.all), 
                  User=nrow(df.User),
                  NonUser=nrow(df.NonUser),
                  AfterIPTW.User=sum(User.wt),
                  AfterIPTW.NonUser=sum(NonUser.wt))
  print(N)
  
  # 1. Age
  mean <- data.frame(Index="mean", 
                     all=mean(df.all$AGE_cal), 
                     User=mean(df.User$AGE_cal), 
                     NonUser=mean(df.NonUser$AGE_cal),
                     AfterIPTW.User=sum(df.User$AGE_cal*User.wt)/sum(User.wt),
                     AfterIPTW.NonUser=sum(df.NonUser$AGE_cal*NonUser.wt)/sum(NonUser.wt))
  print(mean) 
  
  w.sd <- function(xi, xbar, w){
    s1 <- sum( (xi-rep(xbar,length(xi)))^2 * w )
    a <- sum(w) / ( (sum(w))^2 - sum(w^2) )
    return ( sqrt( s1 * a) )
  }
  
  sd <- data.frame(Index="sd", 
                   all=sd(df.all$AGE_cal),
                   User=sd(df.User$AGE_cal), 
                   NonUser=sd(df.NonUser$AGE_cal),
                   AfterIPTW.User=w.sd(df.User$AGE_cal, mean$AfterIPTW.User, User.wt),
                   AfterIPTW.NonUser=w.sd(df.NonUser$AGE_cal, mean$AfterIPTW.NonUser, NonUser.wt))
  print(sd)
  
  AgeTable1 <- data.frame("Index"=c("<=65", "65<"),
                          "all"=c(sum(df.all$AGE==0), sum(df.all$AGE)),
                          "User"=c(sum(df.User$AGE==0), sum(df.User$AGE)),
                          "NonUser"=c(sum(df.NonUser$AGE==0), sum(df.NonUser$AGE)),
                          "AfterIPTW.User"=c(sum(as.numeric(df.User$AGE==0) * User.wt), 
                                             sum(df.User$AGE * User.wt)),
                          "AfterIPTW.NonUser"=c(sum(as.numeric(df.NonUser$AGE==0) * NonUser.wt),
                                                sum(df.NonUser$AGE * NonUser.wt)))
  
  print(AgeTable1)
  
  AgeTable2 <- data.frame(Index= agelabels,
                          all= c(sum(df.all$AGE2=="30-"), sum(df.all$AGE2=="30-39"), 
                                 sum(df.all$AGE2=="40-49"), sum(df.all$AGE2=="50-59"), 
                                 sum(df.all$AGE2=="60-69"), sum(df.all$AGE2=="70-79"), 
                                 sum(df.all$AGE2=="80-89"), sum(df.all$AGE2=="90+")),
                          User= c(sum(df.User$AGE2=="30-"), sum(df.User$AGE2=="30-39"),
                                  sum(df.User$AGE2=="40-49"), sum(df.User$AGE2=="50-59"), 
                                  sum(df.User$AGE2=="60-69"), sum(df.User$AGE2=="70-79"),
                                  sum(df.User$AGE2=="80-89"), sum(df.User$AGE2=="90+")),
                          NonUser= c(sum(df.NonUser$AGE2=="30-"), sum(df.NonUser$AGE2=="30-39"), 
                                     sum(df.NonUser$AGE2=="40-49"), sum(df.NonUser$AGE2=="50-59"),
                                     sum(df.NonUser$AGE2=="60-69"), sum(df.NonUser$AGE2=="70-79"), 
                                     sum(df.NonUser$AGE2=="80-89"), sum(df.NonUser$AGE2=="90+")),
                          AfterIPTW.User= c(sum(as.numeric(df.User$AGE2=="30-") * User.wt), 
                                            sum(as.numeric(df.User$AGE2=="30-39") * User.wt), 
                                            sum(as.numeric(df.User$AGE2=="40-49") * User.wt), 
                                            sum(as.numeric(df.User$AGE2=="50-59") * User.wt),
                                            sum(as.numeric(df.User$AGE2=="60-69") * User.wt), 
                                            sum(as.numeric(df.User$AGE2=="70-79") * User.wt),
                                            sum(as.numeric(df.User$AGE2=="80-89") * User.wt),
                                            sum(as.numeric(df.User$AGE2=="90+") * User.wt) ),
                          AfterIPTW.NonUser = c(sum(as.numeric(df.NonUser$AGE2=="30-") * NonUser.wt), 
                                                sum(as.numeric(df.NonUser$AGE2=="30-39") * NonUser.wt), 
                                                sum(as.numeric(df.NonUser$AGE2=="40-49") * NonUser.wt), 
                                                sum(as.numeric(df.NonUser$AGE2=="50-59") * NonUser.wt), 
                                                sum(as.numeric(df.NonUser$AGE2=="60-69") * NonUser.wt), 
                                                sum(as.numeric(df.NonUser$AGE2=="70-79") * NonUser.wt), 
                                                sum(as.numeric(df.NonUser$AGE2=="80-89") * NonUser.wt),
                                                sum(as.numeric(df.NonUser$AGE2=="90+") * NonUser.wt)) )
  print(AgeTable2)
  
  # 2. Sex
  SexTable <- data.frame("Index"=c("Male", "Female"),
                         "all"=c(sum(df.all$GENDER==0), 
                                 sum(df.all$GENDER)),
                         "User"=c(sum(df.User$GENDER==0), 
                                  sum(df.User$GENDER)),
                         "NonUser"=c(sum(df.NonUser$GENDER==0), 
                                     sum(df.NonUser$GENDER)),
                         "AfterIPTW.User"=c(sum(as.numeric(df.User$GENDER==0) * User.wt), 
                                            sum(df.User$GENDER * User.wt)),
                         "AfterIPTW.NonUser"=c(sum(as.numeric(df.NonUser$GENDER==0) * NonUser.wt),
                                               sum(df.NonUser$GENDER * NonUser.wt)))
  
  print(SexTable)
  
  # 3. MedInfo
  MedTable <- data.frame("Index"=c("Medical Insurance", "Medical Aid"),
                         "all"=c(sum(df.all$MedInfo==0), sum(df.all$MedInfo)),
                         "User"=c(sum(df.User$MedInfo==0), sum(df.User$MedInfo)),
                         "NonUser"=c(sum(df.NonUser$MedInfo==0), sum(df.NonUser$MedInfo)),
                         "AfterIPTW.User"=c(sum(as.numeric(df.User$MedInfo==0) * User.wt), sum(df.User$MedInfo * User.wt)),
                         "AfterIPTW.NonUser"=c(sum(as.numeric(df.NonUser$MedInfo==0) * NonUser.wt), sum(df.NonUser$MedInfo * NonUser.wt) ))
  print(MedTable)
  
  # 4. Confounder
  con_names <- names(df.all)
  con_names2 <- con_names[con_names %in% confounder]
  
  conTable <- NULL;
  for (name in con_names2) {
    conTmp <- data.frame("Index"=name, 
                         "all"=sum(df.all[,name]), 
                         "User"=sum(df.User[,name]), 
                         "NonUser"=sum(df.NonUser[,name]), 
                         "AfterIPTW.User"=sum(df.User[,name] * User.wt), 
                         "AfterIPTW.NonUser"=sum(df.NonUser[,name] * NonUser.wt) )
    
    conTable <- rbind(conTable, conTmp)
  }
  print(conTable)
  
  t1 <- rbind(N, mean, sd, AgeTable1, AgeTable2, SexTable, MedTable, conTable)
  filename1 <- paste0("./resultsCNLLS/table/[Table1]", save_name, ".csv")
  write.csv(t1, filename1)
  
  # ---------- table 2 ---------- #
  # 6. Risk of adverse clinical outcomes associated with Exposure ~ .
  # (1) # of patients (2) # of events (3) Event Rates (%)
  Primary <- data.frame(Index=c("NonUser-Primary", "User-Primary"),
                        N_Patients=c(nrow(df.NonUser), nrow(df.User)),
                        N_Events=c(sum(df.NonUser$outcome1),sum(df.User$outcome1) ),
                        EventRates=c(sum(df.NonUser$outcome1)/nrow(df.NonUser),
                                     sum(df.User$outcome1)/nrow(df.User) ) )
  
  outcomes2 <- as.character( df.study[df.study$type=="Outcome2","var"] )
  
  Secondary <- NULL
  for (outcome2 in outcomes2){
    ind <- paste0(c("NonUser-", "User-"), outcome2)
    secondaryTmp <- data.frame(Index=ind,
                               N_Patients=c(nrow(df.NonUser), nrow(df.User)),
                               N_Events=c(sum(df.NonUser[,outcome2]),sum(df.User[,outcome2]) ),
                               EventRates=c(sum(df.NonUser[,outcome2])/nrow(df.NonUser),
                                            sum(df.User[,outcome2])/nrow(df.User) )  )
    
    Secondary <- rbind(Secondary, secondaryTmp)
  }
  
  t2 <- Secondary
  
  filename1 <- paste0("./resultsCNLLS/table/[Table2]Primary_", save_name, ".csv")
  filename2 <- paste0("./resultsCNLLS/table/[Table2]Secondary_", save_name, ".csv")
  
  write.csv(Primary, filename1)
  write.csv(t2, filename2)
  
  # ---------- aSD -------------- #
  # Confounder
  m.vec1 <- conTable$User / nrow(df.User)
  m.vec2 <- conTable$NonUser / nrow(df.NonUser)
  v.vec1 <- m.vec1*(1-m.vec1)
  v.vec2 <- m.vec2*(1-m.vec2)
  con.aSD1 <- cal.aSD(m.vec1, m.vec2, v.vec1, v.vec2)
  
  # weighted prevalance 's sd?
  mm.vec1 <- conTable$AfterIPTW.User / sum(User.wt)
  mm.vec2 <- conTable$AfterIPTW.NonUser / sum(NonUser.wt)
  vv.vec1 <- m.vec1*(1-m.vec1)
  vv.vec2 <- m.vec2*(1-m.vec2)
  con.aSD2 <- cal.aSD(mm.vec1, mm.vec2, vv.vec1, vv.vec2)
  
  print(con.aSD1)
  print(con.aSD2)
  
  aSD.name <- paste0("./resultsCNLLS/table/[aSD]", save_name, ".csv")
  aSD.df <- data.frame("name"=con_names2, 
                       "Before.IPTW"=con.aSD1,
                       "After.IPTW"=con.aSD2)
  # "name"=c("AGE", "SEX", "MedInfo", con_names2),
  write.csv(aSD.df, aSD.name)
}

###############################################
############    All statin      ###############
###############################################

statin120.con <- statin120[,statin.confounder]
statin120.fit <- glm(statin120$statin ~ ., data = statin120.con, family="binomial")

statin120$ps <- statin120.fit$fitted.values
statin120$iptw.wt <- ( 1/statin120$ps )*statin120$statin + ( 1/(1-statin120$ps) )*( 1-statin120$statin )

sink("./resultsCNLLS/model/[ALL_IPTW]results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin120))
print(statin.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(statin120$statin, statin120$ps, statin120$iptw.wt, "statin120")
outcome.model(Y="outcome1", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")

outcome.model(Y="Death", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")
outcome.model(Y="ICU.admission", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")

print_table(df.all=statin120, wt=statin120$iptw.wt, df.study=statin.study, save_name="statin120")
sink()

sink("./resultsCNLLS/model/[ALL_IPTW_trim]results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin120))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")
print(nrow(statin120))
statin120$trim.wt <- statin120$iptw.wt
statin120$trim.wt[(statin120$ps<0.01 | statin120$ps>0.99)] <- 0

# ps_summary 결과 재확인 필요
ps_summary(statin120$statin[statin120$trim.wt!=0], statin120$ps[statin120$trim.wt!=0], statin120$trim.wt[statin120$trim.wt!=0], "trim.statin120")
outcome.model(Y="outcome1", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")

outcome.model(Y="Death", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")
outcome.model(Y="ICU.admission", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")

print_table(df.all=statin120, wt=statin120$trim.wt, df.study=statin.study, save_name="trim.statin120")
sink()

sink("./resultsCNLLS/model/[ALL_IPTW_stabilized]results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin120))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.statin <- sum(statin120$statin) / nrow(statin120)
statin120$st.iptw.wt <- ( p.statin/statin120$ps )*statin120$statin + ( (1-p.statin)/(1-statin120$ps) )*( 1-statin120$statin )
ps_summary(statin120$statin, statin120$ps, statin120$st.iptw.wt, "stabilized.statin120")
outcome.model(Y="outcome1", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")

outcome.model(Y="Death", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")
outcome.model(Y="ICU.admission", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")

print_table(df.all=statin120, wt=statin120$st.iptw.wt, df.study=statin.study, save_name="stabilized.statin120")

sink()

sink("./resultsCNLLS/model/[ALL_Deciles]results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin120))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

statin120$ps.dec <- cut(statin120$ps, 
                        breaks=c(quantile(statin120$ps, probs=seq(0,1,0.1))),
                        labels=seq(1:10), include.lowest=TRUE)
statin120$dec_info <- paste0(statin120$statin, ":", statin120$ps.dec)

statin120.dec <- data.frame(table(statin120$ps.dec, statin120$statin))
colnames(statin120.dec) <- c("ps.dec", "statin", "n")
statin120.dec$dec_info <- paste0(statin120.dec$statin, ":", statin120.dec$ps.dec)
statin120.dec$n[statin120.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
statin120.dec$dec.wt <- 1/statin120.dec$n

statin120 <- merge(statin120, statin120.dec[,c("dec_info", "dec.wt")], by="dec_info")

ps_summary(statin120$statin, statin120$ps, statin120$dec.wt, "dec.statin120")
outcome.model(Y="outcome1", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")

outcome.model(Y="Death", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")
outcome.model(Y="ICU.admission", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")

print_table(df.all=statin120, wt=statin120$dec.wt, df.study=statin.study, save_name="dec.statin120")

sink()

sink("./resultsCNLLS/model/[ALL_Matching]results_statin120.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
ind.trt0 <- which(statin120$statin == 0)
ind.trt1 <- which(statin120$statin == 1)

trt0.ps = statin120$ps[ind.trt0]
trt1.ps = statin120$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
statin120$match.wt = rep(0,nrow(statin120))
statin120$match.wt[ind.trt1] = 1
statin120$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(statin120$statin, statin120$ps, statin120$match.wt, "matching.statin120")
outcome.model(Y="outcome1", statin120$match.wt, df=statin120, study=statin.study, name="matching.statin120")

outcome.model(Y="Death", statin120$match.wt, df=statin120, study=statin.study, name="matching.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$match.wt, df=statin120, study=statin.study, name="matching.statin120")
outcome.model(Y="ICU.admission", statin120$match.wt, df=statin120, study=statin.study, name="matching.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$match.wt, df=statin120, study=statin.study, name="matching.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$match.wt, df=statin120, study=statin.study, name="matching.statin120")

print_table(df.all=statin120, wt=statin120$match.wt, df.study=statin.study, save_name="match.statin120")

sink()

sink("./resultsCNLLS/model/[ALL_Kernel]results_statin120.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")

propDiffMat2 = propDiffMat/0.06  

rownames(propDiffMat2) = ind.trt1
colnames(propDiffMat2) = ind.trt0

matchMat2 = exp(- propDiffMat2^2 / 2)
matchMat2 <- matchMat2 / rowSums(matchMat2)
dim(matchMat2)
length(rowSums(matchMat2))

statin120$kernel.wt = rep(0,nrow(statin120))
statin120$kernel.wt[ind.trt0] = colSums(matchMat2)
statin120$kernel.wt[ind.trt1] = 1

ps_summary(statin120$statin, statin120$ps, statin120$kernel.wt, "kernel.statin120")
outcome.model(Y="outcome1", statin120$match.wt, df=statin120, study=statin.study, name="kernel.statin120")

outcome.model(Y="Death", statin120$kernel.wt, df=statin120, study=statin.study, name="kernel.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$kernel.wt, df=statin120, study=statin.study, name="kernel.statin120")
outcome.model(Y="ICU.admission", statin120$kernel.wt, df=statin120, study=statin.study, name="kerneling.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$kernel.wt, df=statin120, study=statin.study, name="kernel.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$kernel.wt, df=statin120, study=statin.study, name="kernel.statin120")

print_table(df.all=statin120, wt=statin120$kernel.wt, df.study=statin.study, save_name="kernel.statin120")

sink()