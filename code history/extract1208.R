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


co19_t200$AGE_cal <- 2020-as.numeric(substr(co19_t200$PAT_BTH, 1, 4))

testJID <- unique(co19_t200$JID)

targetJID <- unique(co19_t200[(co19_t200$CONFIRM=="Y")&(co19_t200$AGE_cal > 18), ]$JID)
targetJID <- setdiff(targetJID, NA)

age40 <- unique(co19_t200[(co19_t200$AGE_cal >= 40), ]$JID)
age40JID <- intersect(targetJID, age40)

# 021:의과입원, 031:의과외래, 041:치과입원, 051:치과외래, 061:조산원입원, 
# 071:보건기관입원의과, 072:보건기관입원치과, 073:보건기관입원한방, 
# 081:보건기관외래의과, 082:보건기관외래치과, 083:보건기관외래한방, 

hospital <- unique(co19_t200[(co19_t200$FOM_TP_CD %in% c("021", "071")) & (co19_t200$VST_DDCNT >= 2), ]$JID)
hospitalJID <- intersect(targetJID, hospital)

cancer_jid1 <- unique(co19_twjhe200[co19_twjhe200$PRCL_SYM_TP_CD %in% c("V027", "V193", "V194"), ]$JID)
cancer_jid2 <- unique(co19_t200[co19_t200$PRCL_SYM_TP_CD %in% c("V027", "V193", "V194"), ]$JID)
cancer_jid <- unique(c(cancer_jid1, cancer_jid2))

age40.cancer_jid <- intersect(cancer_jid, age40JID)

# 배제상병 제외
co19_t400[(co19_t400$SICK_TY_CD == '3') & (!is.na(co19_t400$SICK_TY_CD)), ]$SICK_CD <- "0"
co19_twjhe400[(co19_twjhe400$SICK_TY_CD == '3') & (!is.na(co19_twjhe400$SICK_TY_CD)), ]$SICK_CD <- "0"
# -------------------------------------------------- #
co19_t200_trans_dn <- co19_t200[(co19_t200$JID %in% targetJID), c("JID", "MID", "INSUP_TP_CD", "SEX_TP_CD", "RECU_FR_DD", "PAT_BTH", "AGE_cal", "MAIN_SICK", "SUB_SICK", "DEATH", "CONFIRM")]
co19_t200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_t200_trans_dn$RECU_FR_DD), "%Y%m%d")

co19_t300_trans_dn <- co19_t300[co19_t300$JID %in% targetJID, c("JID", "MID", "DIV_CD")]
co19_t400_trans_dn <- co19_t400[co19_t400$JID %in% targetJID, c("JID", "MID", "SICK_CD")]
co19_t530_trans_dn <- co19_t530[co19_t530$JID %in% targetJID, c("JID", "MID", "DIV_CD", "GNL_CD")]

co19_twjhe200_trans_dn <- co19_twjhe200[co19_twjhe200$JID %in% targetJID, c("JID", "MID", "SEX_TP_CD", "RECU_FR_DD", "MAIN_SICK", "SUB_SICK")]
co19_twjhe200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_twjhe200_trans_dn$RECU_FR_DD), "%Y%m%d")

co19_twjhe300_trans_dn <- co19_twjhe300[co19_twjhe300$JID %in% targetJID,c("JID", "MID", "DIV_CD", "GNL_CD")]
co19_twjhe400_trans_dn <- co19_twjhe400[co19_twjhe400$JID %in% targetJID,c("JID", "MID", "SICK_CD")]
co19_twjhe530_trans_dn <- co19_twjhe530[co19_twjhe530$JID %in% targetJID,c("JID", "MID", "DIV_CD", "GNL_CD")]

# External files (code_book)
codebook_path <- "./"
codebook <- "code-books-201126.xlsx"

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
# co19_t200_trans_dn
co19_T200DT <- data.table(co19_t200_trans_dn)

patient_dt <- co19_T200DT[, .(first_covid=min(RECU_FR_DD)), by=.(JID, SEX_TP_CD, AGE_cal, INSUP_TP_CD)]
patient_info <- as.data.frame(patient_dt)
head(patient_info)
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
verbose.period = 2000

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
      
      if (study=="statin.study"){
        if (unique(codeBook$codeType) == "ATC") {
          # (statin) ATC 는 d-720 ~ d- 240 or 360
          confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays) & (df_temp$RECU_FR_DD >= first - 720)
        } else{
          # (statin) 상병은 -3y ~ d- 90 or 120
          confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays) & (df_temp$RECU_FR_DD >= first - 1096)
        }
      } else {
        if (unique(codeBook$codeType) == "ATC") {
          # ATC 는 d-240 ~ d- 90 or 120
          confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays) & (df_temp$RECU_FR_DD >= first - 240)
        } else{
          # 상병은 -3y ~ d- 90 or 120
          confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays) & (df_temp$RECU_FR_DD >= first - 1096)
        }
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
        mapped_var1 <- toupper(ATC_mapping_df[rule, var_name])
        searchJID1 <- df[ifelse(toupper(df[,var_name]) %in% mapped_var1, TRUE, FALSE),]$JID
        
        mapped_var2 <- toupper(ATC_mapping_df[rule, "DIV_CD"])
        searchJID2 <- df[ifelse(toupper(df[,"DIV_CD"]) %in% mapped_var2, TRUE, FALSE),]$JID
        
        searchJID <- unique( c(searchJID1, searchJID2) )
        
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
como_info_statin90 <- gen_dummies(code_ICD10_df,  "statin.study", 240) #90 -> 240
como_info_statin90 <- como_info_statin90[como_info_statin90$JID %in% age40JID, ]

como_info_Anticoagulants90 <- gen_dummies(code_ICD10_df, "antiplatelet.anticoagulant.study",90)
como_info_immunosuppressant90 <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 90)

como_info_statin120 <- gen_dummies(code_ICD10_df, "statin.study",360) #120 -> 360
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
ATC_med_statin90 <- gen_dummies(code_ATC_df, "statin.study", 240)
ATC_med_statin90 <- ATC_med_statin90[ATC_med_statin90$JID %in% age40JID, ]

ATC_med_Anticoagulants90 <- gen_dummies(code_ATC_df, "antiplatelet.anticoagulant.study",90)
ATC_med_immunosuppressant90 <- gen_dummies(code_ATC_df, "immune.suppressant.study", 90)

ATC_med_statin120 <- gen_dummies(code_ATC_df, "statin.study", 360)
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

Anticoagulants.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction", "Thromboembolism", 
                             "TIA.transient.ischemic.attack.", "Stroke")

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
Anticoagulants90$myocardial.storke.TIA.Thromboembolism <- ifelse(Anticoagulants90$myocardial.infarction + Anticoagulants90$Stroke + Anticoagulants90$TIA.transient.ischemic.attack. + Anticoagulants90$Thromboembolism>0,1,0)
Anticoagulants90$bleeding.transfusion <- ifelse(Anticoagulants90$bleeding + Anticoagulants90$transfusion>0,1,0)

Anticoagulants120$myocardial.storke.TIA.Thromboembolism <- ifelse(Anticoagulants120$myocardial.infarction + Anticoagulants120$Stroke + Anticoagulants120$TIA.transient.ischemic.attack. + Anticoagulants120$Thromboembolism>0,1,0)
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

#### Outcome Modeling #####

ps_summary <- function(exposure, ps, wt, name){
  User <- ps[exposure==0]
  NonUser <- ps[exposure==1]
  
  cat(sprintf("- User \n (<ps.cut) : %d\n (>(1-ps.cut)) : %d\n", 
              sum(User<ps.cut), sum(User > (1-ps.cut)) ))
  
  cat(sprintf("- NonUser \n (<ps.cut) : %d\n (>(1-ps.cut)) : %d\n", 
              sum(NonUser<ps.cut), sum(NonUser>(1-ps.cut)) )) 
  
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
cal.aSD <- function(g1.m, g2.m){
  g1.var <- g1.m*(1-g1.m)
  g2.var <- g2.m*(1-g2.m)
  return( abs(g1.m - g2.m) / sqrt( ( g1.var + g2.var )/2 ) )
}

#### table 1,2. print ####
# df.all = statin120; wt ; df.study=statin.study; save_name="statin120" 
print_table <- function(df.all, wt, df.study, save_name){
  
  agebreaks <- c(0, 30, 40, 50, 60, 70, 80, 90, 500)
  agelabels <- c("30-", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")
  
  df.all$AGE2 = cut(df.all$AGE_cal, breaks = agebreaks,
                    right = FALSE, labels = agelabels)
  
  df.all <- data.frame(df.all)
  
  # 이름 받아오기
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
  
  con.aSD1 <- cal.aSD(m.vec1, m.vec2)
  
  # weighted prevalance 's sd?
  mm.vec1 <- conTable$AfterIPTW.User / sum(User.wt)
  mm.vec2 <- conTable$AfterIPTW.NonUser / sum(NonUser.wt)
  
  con.aSD2 <- cal.aSD(mm.vec1, mm.vec2)
  
  cat("\n Before Weight\n")
  print(con.aSD1)
  cat("\n After Weight\n")
  print(con.aSD2)
  
  aSD.name <- paste0("./resultsCNLLS/table/[aSD]", save_name, ".csv")
  aSD.df <- data.frame("name"=con_names2, 
                       "Before.IPTW"=con.aSD1,
                       "After.IPTW"=con.aSD2)
  
  write.csv(aSD.df, aSD.name)
}

# Glucocorticoid 90
immunosuppressant90$Glucocorticoid <- ATC_med_immunosuppressant90$ID3110.H02
immunosuppressant120$Glucocorticoid <- ATC_med_immunosuppressant120$ID3110.H02

hos.statin90 <- statin90[statin90$JID %in% hospitalJID,]
hos.statin120 <- statin120[statin120$JID %in% hospitalJID,]
hos.Anticoagulants90 <- Anticoagulants90[Anticoagulants90$JID %in% hospitalJID,]
hos.Anticoagulants120 <- Anticoagulants120[Anticoagulants120$JID %in% hospitalJID,]
hos.immunosuppressant90 <- immunosuppressant90[immunosuppressant90$JID %in% hospitalJID,]
hos.immunosuppressant120 <- immunosuppressant120[immunosuppressant120$JID %in% hospitalJID,]

## Glucocorticoid
Glucocorticoid.confounder <- c(immunosuppressant.confounder, "immunosuppressant.for.subanalysis")
Glucocorticoid.study <- rbind(data.frame(var = "Glucocorticoid", type = "Exposure"),
                    data.frame(var = immunosuppressant.confounder, type = rep("Confounder", length(immunosuppressant.confounder))),
                    data.frame(var = immunosuppressant.outcome1, type = rep("Outcome1", length(immunosuppressant.outcome1))),
                    data.frame(var = immunosuppressant.outcome2, type = rep("Outcome2", length(immunosuppressant.outcome2))), 
                    data.frame(var = immunosuppressant.Emodifier, type = rep("Effect.Modifier",length(immunosuppressant.Emodifier))))
Glucocorticoid90 <- immunosuppressant90[( immunosuppressant90$immunosuppressant == 0) | ( immunosuppressant90$Glucocorticoid == 1 ),]
Glucocorticoid120 <- immunosuppressant120[( immunosuppressant120$immunosuppressant == 0) | ( immunosuppressant120$Glucocorticoid == 1 ),]

hos.Glucocorticoid90 <- hos.immunosuppressant90[( hos.immunosuppressant90$immunosuppressant == 0) | ( hos.immunosuppressant90$Glucocorticoid == 1 ),]
hos.Glucocorticoid120 <- hos.immunosuppressant120[( hos.immunosuppressant120$immunosuppressant == 0) | ( hos.immunosuppressant120$Glucocorticoid == 1 ),]


sink("./resultsCNLLS/frequency/check-df3.txt")
cat(" ** hos.STATIN 90 ** \n\n")
frequency3(hos.statin90, "statin")
cat("\n\n ** hos.STATIN 120 ** \n\n")
frequency3(hos.statin120, "statin")
cat("\n\n ** hos.Anticoagulants 90 ** \n\n")
frequency3(hos.Anticoagulants90, "Anticoagulants")
cat("\n\n ** hos.Anticoagulants 120 ** \n\n")
frequency3(hos.Anticoagulants120, "Anticoagulants")
cat("\n\n ** hos.Immunosuppressant 90 ** \n\n")
frequency3(hos.immunosuppressant90, "immunosuppressant")
cat("\n\n ** hos.Immunosuppressant 120 ** \n\n")
frequency3(hos.immunosuppressant120, "immunosuppressant")
cat("\n\n ** Glucocorticoid 90 ** \n\n")
frequency3(Glucocorticoid90, "immunosuppressant")
cat("\n\n ** Glucocorticoid 120 ** \n\n")
frequency3(Glucocorticoid120, "immunosuppressant")
cat("\n\n ** hos.Glucocorticoid 90 ** \n\n")
frequency3(hos.Glucocorticoid90, "immunosuppressant")
cat("\n\n ** hos.Glucocorticoid 120 ** \n\n")
frequency3(hos.Glucocorticoid120, "immunosuppressant")
sink()

###  아래 부터 복붙 구간 입니다 ###
### statin120 -> / Anticoagulants120 은 절대 지우면 안됨 : 기본틀 ###
### statin120 -> immuno 복붙 할 때 "myocar~ secondary outcome 지워야 함
### All -> Hos 로 복붙 할 때, (1) All -> Hos (2) statin90, statin120 -> hos.statin90, hos.statin120 으로

# ps cut 설정
ps.cut = 0.01
# kernel matching h 설정
h = 0.06 # (1203) ps.cut 해보기

###############################################
############  All statin 120    ###############
###############################################

statin120.con <- statin120[,statin.confounder]
statin120.fit <- glm(statin120$statin ~ ., data = statin120.con, family="binomial")

statin120$ps <- statin120.fit$fitted.values
ind.trt0 <- which(statin120$statin == 0)
ind.trt1 <- which(statin120$statin == 1)

statin120$iptw.wt <- ( 1/statin120$ps )*statin120$statin + ( 1/(1-statin120$ps) )*( 1-statin120$statin )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW]results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin120))
print(statin.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(statin120$statin, statin120$ps, statin120$iptw.wt, "statin120")
outcome.model(Y="outcome1", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120") # 0.6249

outcome.model(Y="Death", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")
outcome.model(Y="ICU.admission", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=statin120$iptw.wt, df=statin120, study=statin.study, name="statin120")

print_table(df.all=statin120, wt=statin120$iptw.wt, df.study=statin.study, save_name="statin120")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim]results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin120))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

statin120$trim.wt <- statin120$iptw.wt
statin120$trim.wt[(statin120$ps<ps.cut | statin120$ps>(1-ps.cut))] <- 0
print(nrow(statin120[statin120$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(statin120$statin[statin120$trim.wt!=0], statin120$ps[statin120$trim.wt!=0], statin120$trim.wt[statin120$trim.wt!=0], "trim.statin120")
outcome.model(Y="outcome1", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120") # 0.6802

outcome.model(Y="Death", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")
outcome.model(Y="ICU.admission", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=statin120$trim.wt, df=statin120, study=statin.study, name="trim.statin120")

print_table(df.all=statin120, wt=statin120$trim.wt, df.study=statin.study, save_name="trim.statin120")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized]results_statin120.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.statin <- sum(statin120$statin) / nrow(statin120)
# wt : ATE
statin120$st.iptw.wt <- ( p.statin/statin120$ps )*statin120$statin + ( (1-p.statin)/(1-statin120$ps) )*( 1-statin120$statin )

ps_summary(statin120$statin, statin120$ps, statin120$st.iptw.wt, "stabilized.statin120")

outcome.model(Y="outcome1", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120") # -1.21

outcome.model(Y="Death", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")
outcome.model(Y="ICU.admission", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$st.iptw.wt, df=statin120, study=statin.study, name="stabilized.statin120")

print_table(df.all=statin120, wt=statin120$st.iptw.wt, df.study=statin.study, save_name="stabilized.statin120")

sink()

#### IPTW : ATT ####

statin120$iptw.att <- statin120$ps/(1-statin120$ps)
statin120$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[ALL_IPTW_ATT]results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin120))
print(statin.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(statin120$statin, statin120$ps, statin120$iptw.att, "statin120.att")
outcome.model(Y="outcome1", statin120$iptw.att, df=statin120, study=statin.study, name="statin120.att") # 0.6249

outcome.model(Y="Death", statin120$iptw.att, df=statin120, study=statin.study, name="statin120.att")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$iptw.att, df=statin120, study=statin.study, name="statin120.att")
outcome.model(Y="ICU.admission", statin120$iptw.att, df=statin120, study=statin.study, name="statin120.att")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$iptw.att, df=statin120, study=statin.study, name="statin120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$iptw.att, df=statin120, study=statin.study, name="statin120.att")

print_table(df.all=statin120, statin120$iptw.att, df.study=statin.study, save_name="statin120.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim_ATT]results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin120))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

statin120$trim.att <- statin120$iptw.att
statin120$trim.att[(statin120$ps<ps.cut | statin120$ps>(1-ps.cut))] <- 0
print(nrow(statin120[statin120$trim.att!=0,]))

ps_summary(statin120$statin[statin120$trim.att!=0], statin120$ps[statin120$trim.att!=0], statin120$trim.att[statin120$trim.att!=0], "trim.statin120.att")
outcome.model(Y="outcome1", statin120$trim.att, df=statin120, study=statin.study, name="trim.statin120.att") # 0.6802

outcome.model(Y="Death", statin120$trim.att, df=statin120, study=statin.study, name="trim.statin120.att")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$trim.att, df=statin120, study=statin.study, name="trim.statin120.att")
outcome.model(Y="ICU.admission", statin120$trim.att, df=statin120, study=statin.study, name="trim.statin120.att")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$trim.att, df=statin120, study=statin.study, name="trim.statin120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$trim.att, df=statin120, study=statin.study, name="trim.statin120.att")

print_table(df.all=statin120, statin120$trim.att, df.study=statin.study, save_name="trim.statin120.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized_ATT]results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin120))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
statin120$st.iptw.att <- ( (1-p.statin)/p.statin ) * ( statin120$ps / (1-statin120$ps) )  *  (1-statin120$statin)
statin120$st.iptw.att[ind.trt1] <- 1
ps_summary(statin120$statin, statin120$ps, statin120$st.iptw.att, "stabilized.att.statin120")

outcome.model(Y="outcome1", statin120$st.iptw.att, df=statin120, study=statin.study, name="stabilized.att.statin120") # -1.21

outcome.model(Y="Death", statin120$st.iptw.att, df=statin120, study=statin.study, name="stabilized.att.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$st.iptw.att, df=statin120, study=statin.study, name="stabilized.att.statin120")
outcome.model(Y="ICU.admission", statin120$st.iptw.att, df=statin120, study=statin.study, name="stabilized.att.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$st.iptw.att, df=statin120, study=statin.study, name="stabilized.att.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$st.iptw.att, df=statin120, study=statin.study, name="stabilized.att.statin120")

print_table(df.all=statin120, wt=statin120$st.iptw.att, df.study=statin.study, save_name="stabilized.att.statin120")

sink()

#### Deciles Matching ####
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
cat("\n"); cat(table(statin120$ps.dec)); cat("\n")
statin120$dec_info <- paste0(statin120$statin, ":", statin120$ps.dec)

cat("\n"); cat(table(statin120$dec_info)); cat("\n"); 
statin120.dec <- data.frame(table(statin120$ps.dec, statin120$statin))
cat("\n"); print(statin120.dec); cat("\n"); 

colnames(statin120.dec) <- c("ps.dec", "statin", "n")
statin120.dec$dec_info <- paste0(statin120.dec$statin, ":", statin120.dec$ps.dec)
statin120.dec$n[statin120.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
statin120.dec$dec.wt <- 1/statin120.dec$n

if (is.null(statin120$dec.wt)) {
  statin120 <- merge(statin120, statin120.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(statin120$statin, statin120$ps, statin120$dec.wt, "dec.statin120")
outcome.model(Y="outcome1", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120") # -0.03765

outcome.model(Y="Death", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")
outcome.model(Y="ICU.admission", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$dec.wt, df=statin120, study=statin.study, name="dec.statin120")

print_table(df.all=statin120, wt=statin120$dec.wt, df.study=statin.study, save_name="dec.statin120")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[ALL_Matching]results_statin120.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
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

ps_summary(statin120$statin, statin120$ps, statin120$match.wt, "match.statin120")
outcome.model(Y="outcome1", statin120$match.wt, df=statin120, study=statin.study, name="match.statin120") # 1.6468

outcome.model(Y="Death", statin120$match.wt, df=statin120, study=statin.study, name="match.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$match.wt, df=statin120, study=statin.study, name="match.statin120")
outcome.model(Y="ICU.admission", statin120$match.wt, df=statin120, study=statin.study, name="match.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$match.wt, df=statin120, study=statin.study, name="match.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$match.wt, df=statin120, study=statin.study, name="match.statin120")

print_table(df.all=statin120, wt=statin120$match.wt, df.study=statin.study, save_name="match.statin120")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[ALL_Kernel]results_statin120.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

statin120$kernel.wt = rep(0,nrow(statin120))
statin120$kernel.wt[ind.trt0] = colSums(matchMat.k)
statin120$kernel.wt[ind.trt1] = 1

ps_summary(statin120$statin, statin120$ps, statin120$kernel.wt, "kernel.statin120")
outcome.model(Y="outcome1", statin120$kernel.wt, df=statin120, study=statin.study, name="kernel.statin120")

outcome.model(Y="Death", statin120$kernel.wt, df=statin120, study=statin.study, name="kernel.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", statin120$kernel.wt, df=statin120, study=statin.study, name="kernel.statin120")
outcome.model(Y="ICU.admission", statin120$kernel.wt, df=statin120, study=statin.study, name="kernel.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin120$kernel.wt, df=statin120, study=statin.study, name="kernel.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin120$kernel.wt, df=statin120, study=statin.study, name="kernel.statin120")

print_table(df.all=statin120, wt=statin120$kernel.wt, df.study=statin.study, save_name="kernel.statin120")

sink()


###############################################
############  All statin 90    ###############
###############################################

statin90.con <- statin90[,statin.confounder]
statin90.fit <- glm(statin90$statin ~ ., data = statin90.con, family="binomial")

statin90$ps <- statin90.fit$fitted.values
ind.trt0 <- which(statin90$statin == 0)
ind.trt1 <- which(statin90$statin == 1)

statin90$iptw.wt <- ( 1/statin90$ps )*statin90$statin + ( 1/(1-statin90$ps) )*( 1-statin90$statin )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW]results_statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(statin90))
print(statin.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(statin90$statin, statin90$ps, statin90$iptw.wt, "statin90")
outcome.model(Y="outcome1", wt=statin90$iptw.wt, df=statin90, study=statin.study, name="statin90") # 0.6249

outcome.model(Y="Death", wt=statin90$iptw.wt, df=statin90, study=statin.study, name="statin90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=statin90$iptw.wt, df=statin90, study=statin.study, name="statin90")
outcome.model(Y="ICU.admission", wt=statin90$iptw.wt, df=statin90, study=statin.study, name="statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt=statin90$iptw.wt, df=statin90, study=statin.study, name="statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=statin90$iptw.wt, df=statin90, study=statin.study, name="statin90")

print_table(df.all=statin90, wt=statin90$iptw.wt, df.study=statin.study, save_name="statin90")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim]results_statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(statin90))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

statin90$trim.wt <- statin90$iptw.wt
statin90$trim.wt[(statin90$ps<ps.cut | statin90$ps>(1-ps.cut))] <- 0
print(nrow(statin90[statin90$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(statin90$statin[statin90$trim.wt!=0], statin90$ps[statin90$trim.wt!=0], statin90$trim.wt[statin90$trim.wt!=0], "trim.statin90")
outcome.model(Y="outcome1", wt=statin90$trim.wt, df=statin90, study=statin.study, name="trim.statin90") # 0.6802

outcome.model(Y="Death", wt=statin90$trim.wt, df=statin90, study=statin.study, name="trim.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=statin90$trim.wt, df=statin90, study=statin.study, name="trim.statin90")
outcome.model(Y="ICU.admission", wt=statin90$trim.wt, df=statin90, study=statin.study, name="trim.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt=statin90$trim.wt, df=statin90, study=statin.study, name="trim.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=statin90$trim.wt, df=statin90, study=statin.study, name="trim.statin90")

print_table(df.all=statin90, wt=statin90$trim.wt, df.study=statin.study, save_name="trim.statin90")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized]results_statin90.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.statin <- sum(statin90$statin) / nrow(statin90)
# wt : ATE
statin90$st.iptw.wt <- ( p.statin/statin90$ps )*statin90$statin + ( (1-p.statin)/(1-statin90$ps) )*( 1-statin90$statin )

ps_summary(statin90$statin, statin90$ps, statin90$st.iptw.wt, "stabilized.statin90")

outcome.model(Y="outcome1", statin90$st.iptw.wt, df=statin90, study=statin.study, name="stabilized.statin90") # -1.21

outcome.model(Y="Death", statin90$st.iptw.wt, df=statin90, study=statin.study, name="stabilized.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", statin90$st.iptw.wt, df=statin90, study=statin.study, name="stabilized.statin90")
outcome.model(Y="ICU.admission", statin90$st.iptw.wt, df=statin90, study=statin.study, name="stabilized.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin90$st.iptw.wt, df=statin90, study=statin.study, name="stabilized.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin90$st.iptw.wt, df=statin90, study=statin.study, name="stabilized.statin90")

print_table(df.all=statin90, wt=statin90$st.iptw.wt, df.study=statin.study, save_name="stabilized.statin90")

sink()

#### IPTW : ATT ####

statin90$iptw.att <- statin90$ps/(1-statin90$ps)
statin90$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[ALL_IPTW_ATT]results_statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(statin90))
print(statin.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(statin90$statin, statin90$ps, statin90$iptw.att, "statin90.att")
outcome.model(Y="outcome1", statin90$iptw.att, df=statin90, study=statin.study, name="statin90.att") # 0.6249

outcome.model(Y="Death", statin90$iptw.att, df=statin90, study=statin.study, name="statin90.att")
outcome.model(Y="Use.of.mechanical.ventilation", statin90$iptw.att, df=statin90, study=statin.study, name="statin90.att")
outcome.model(Y="ICU.admission", statin90$iptw.att, df=statin90, study=statin.study, name="statin90.att")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin90$iptw.att, df=statin90, study=statin.study, name="statin90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin90$iptw.att, df=statin90, study=statin.study, name="statin90.att")

print_table(df.all=statin90, statin90$iptw.att, df.study=statin.study, save_name="statin90.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim_ATT]results_statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(statin90))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

statin90$trim.att <- statin90$iptw.att
statin90$trim.att[(statin90$ps<ps.cut | statin90$ps>(1-ps.cut))] <- 0
print(nrow(statin90[statin90$trim.att!=0,]))

ps_summary(statin90$statin[statin90$trim.att!=0], statin90$ps[statin90$trim.att!=0], statin90$trim.att[statin90$trim.att!=0], "trim.statin90.att")
outcome.model(Y="outcome1", statin90$trim.att, df=statin90, study=statin.study, name="trim.statin90.att") # 0.6802

outcome.model(Y="Death", statin90$trim.att, df=statin90, study=statin.study, name="trim.statin90.att")
outcome.model(Y="Use.of.mechanical.ventilation", statin90$trim.att, df=statin90, study=statin.study, name="trim.statin90.att")
outcome.model(Y="ICU.admission", statin90$trim.att, df=statin90, study=statin.study, name="trim.statin90.att")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin90$trim.att, df=statin90, study=statin.study, name="trim.statin90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin90$trim.att, df=statin90, study=statin.study, name="trim.statin90.att")

print_table(df.all=statin90, statin90$trim.att, df.study=statin.study, save_name="trim.statin90.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized_ATT]results_statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(statin90))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
statin90$st.iptw.att <- ( (1-p.statin)/p.statin) * (statin90$ps / (1-statin90$ps) )  *  ( 1-statin90$statin )
statin90$st.iptw.att[ind.trt1] <- 1
ps_summary(statin90$statin, statin90$ps, statin90$st.iptw.att, "stabilized.att.statin90")

outcome.model(Y="outcome1", statin90$st.iptw.att, df=statin90, study=statin.study, name="stabilized.att.statin90") # -1.21

outcome.model(Y="Death", statin90$st.iptw.att, df=statin90, study=statin.study, name="stabilized.att.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", statin90$st.iptw.att, df=statin90, study=statin.study, name="stabilized.att.statin90")
outcome.model(Y="ICU.admission", statin90$st.iptw.att, df=statin90, study=statin.study, name="stabilized.att.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin90$st.iptw.att, df=statin90, study=statin.study, name="stabilized.att.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin90$st.iptw.att, df=statin90, study=statin.study, name="stabilized.att.statin90")

print_table(df.all=statin90, wt=statin90$st.iptw.att, df.study=statin.study, save_name="stabilized.att.statin90")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[ALL_Deciles]results_statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(statin90))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

statin90$ps.dec <- cut(statin90$ps, 
                       breaks=c(quantile(statin90$ps, probs=seq(0,1,0.1))),
                       labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(statin90$ps.dec)); cat("\n")
statin90$dec_info <- paste0(statin90$statin, ":", statin90$ps.dec)

cat("\n"); cat(table(statin90$dec_info)); cat("\n"); 
statin90.dec <- data.frame(table(statin90$ps.dec, statin90$statin))
cat("\n"); print(statin90.dec); cat("\n"); 

colnames(statin90.dec) <- c("ps.dec", "statin", "n")
statin90.dec$dec_info <- paste0(statin90.dec$statin, ":", statin90.dec$ps.dec)
statin90.dec$n[statin90.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
statin90.dec$dec.wt <- 1/statin90.dec$n

if (is.null(statin90$dec.wt)) {
  statin90 <- merge(statin90, statin90.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(statin90$statin, statin90$ps, statin90$dec.wt, "dec.statin90")
outcome.model(Y="outcome1", statin90$dec.wt, df=statin90, study=statin.study, name="dec.statin90") # -0.03765

outcome.model(Y="Death", statin90$dec.wt, df=statin90, study=statin.study, name="dec.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", statin90$dec.wt, df=statin90, study=statin.study, name="dec.statin90")
outcome.model(Y="ICU.admission", statin90$dec.wt, df=statin90, study=statin.study, name="dec.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin90$dec.wt, df=statin90, study=statin.study, name="dec.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin90$dec.wt, df=statin90, study=statin.study, name="dec.statin90")

print_table(df.all=statin90, wt=statin90$dec.wt, df.study=statin.study, save_name="dec.statin90")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[ALL_Matching]results_statin90.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = statin90$ps[ind.trt0]
trt1.ps = statin90$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
statin90$match.wt = rep(0,nrow(statin90))
statin90$match.wt[ind.trt1] = 1
statin90$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(statin90$statin, statin90$ps, statin90$match.wt, "match.statin90")
outcome.model(Y="outcome1", statin90$match.wt, df=statin90, study=statin.study, name="match.statin90") # 1.6468

outcome.model(Y="Death", statin90$match.wt, df=statin90, study=statin.study, name="match.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", statin90$match.wt, df=statin90, study=statin.study, name="match.statin90")
outcome.model(Y="ICU.admission", statin90$match.wt, df=statin90, study=statin.study, name="match.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin90$match.wt, df=statin90, study=statin.study, name="match.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin90$match.wt, df=statin90, study=statin.study, name="match.statin90")

print_table(df.all=statin90, wt=statin90$match.wt, df.study=statin.study, save_name="match.statin90")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[ALL_Kernel]results_statin90.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

statin90$kernel.wt = rep(0,nrow(statin90))
statin90$kernel.wt[ind.trt0] = colSums(matchMat.k)
statin90$kernel.wt[ind.trt1] = 1

ps_summary(statin90$statin, statin90$ps, statin90$kernel.wt, "kernel.statin90")
outcome.model(Y="outcome1", statin90$kernel.wt, df=statin90, study=statin.study, name="kernel.statin90")

outcome.model(Y="Death", statin90$kernel.wt, df=statin90, study=statin.study, name="kernel.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", statin90$kernel.wt, df=statin90, study=statin.study, name="kernel.statin90")
outcome.model(Y="ICU.admission", statin90$kernel.wt, df=statin90, study=statin.study, name="kernel.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", statin90$kernel.wt, df=statin90, study=statin.study, name="kernel.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", statin90$kernel.wt, df=statin90, study=statin.study, name="kernel.statin90")

print_table(df.all=statin90, wt=statin90$kernel.wt, df.study=statin.study, save_name="kernel.statin90")

sink()

###############################################
############  Hos statin 120    ###############
###############################################

hos.statin120.con <- hos.statin120[,statin.confounder]
hos.statin120.fit <- glm(hos.statin120$statin ~ ., data = hos.statin120.con, family="binomial")

hos.statin120$ps <- hos.statin120.fit$fitted.values
ind.trt0 <- which(hos.statin120$statin == 0)
ind.trt1 <- which(hos.statin120$statin == 1)

hos.statin120$iptw.wt <- ( 1/hos.statin120$ps )*hos.statin120$statin + ( 1/(1-hos.statin120$ps) )*( 1-hos.statin120$statin )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW]results_hos.statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(hos.statin120))
print(statin.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.statin120$statin, hos.statin120$ps, hos.statin120$iptw.wt, "hos.statin120")
outcome.model(Y="outcome1", wt=hos.statin120$iptw.wt, df=hos.statin120, study=statin.study, name="hos.statin120") # 0.6249

outcome.model(Y="Death", wt=hos.statin120$iptw.wt, df=hos.statin120, study=statin.study, name="hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.statin120$iptw.wt, df=hos.statin120, study=statin.study, name="hos.statin120")
outcome.model(Y="ICU.admission", wt=hos.statin120$iptw.wt, df=hos.statin120, study=statin.study, name="hos.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt=hos.statin120$iptw.wt, df=hos.statin120, study=statin.study, name="hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.statin120$iptw.wt, df=hos.statin120, study=statin.study, name="hos.statin120")

print_table(hos.statin120, wt=hos.statin120$iptw.wt, df.study=statin.study, save_name="hos.statin120")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim]results_hos.statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(hos.statin120))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.statin120$trim.wt <- hos.statin120$iptw.wt
hos.statin120$trim.wt[(hos.statin120$ps<ps.cut | hos.statin120$ps>(1-ps.cut))] <- 0
print(nrow(hos.statin120[hos.statin120$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(hos.statin120$statin[hos.statin120$trim.wt!=0], hos.statin120$ps[hos.statin120$trim.wt!=0], hos.statin120$trim.wt[hos.statin120$trim.wt!=0], "trim.hos.statin120")
outcome.model(Y="outcome1", wt=hos.statin120$trim.wt, df=hos.statin120, study=statin.study, name="trim.hos.statin120") # 0.6802

outcome.model(Y="Death", wt=hos.statin120$trim.wt, df=hos.statin120, study=statin.study, name="trim.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.statin120$trim.wt, df=hos.statin120, study=statin.study, name="trim.hos.statin120")
outcome.model(Y="ICU.admission", wt=hos.statin120$trim.wt, df=hos.statin120, study=statin.study, name="trim.hos.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt=hos.statin120$trim.wt, df=hos.statin120, study=statin.study, name="trim.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.statin120$trim.wt, df=hos.statin120, study=statin.study, name="trim.hos.statin120")

print_table(hos.statin120, wt=hos.statin120$trim.wt, df.study=statin.study, save_name="trim.hos.statin120")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized]results_hos.statin120.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.statin <- sum(hos.statin120$statin) / nrow(hos.statin120)
# wt : ATE
hos.statin120$st.iptw.wt <- ( p.statin/hos.statin120$ps )*hos.statin120$statin + ( (1-p.statin)/(1-hos.statin120$ps) )*( 1-hos.statin120$statin )

ps_summary(hos.statin120$statin, hos.statin120$ps, hos.statin120$st.iptw.wt, "stabilized.hos.statin120")

outcome.model(Y="outcome1", hos.statin120$st.iptw.wt, df=hos.statin120, study=statin.study, name="stabilized.hos.statin120") # -1.21

outcome.model(Y="Death", hos.statin120$st.iptw.wt, df=hos.statin120, study=statin.study, name="stabilized.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin120$st.iptw.wt, df=hos.statin120, study=statin.study, name="stabilized.hos.statin120")
outcome.model(Y="ICU.admission", hos.statin120$st.iptw.wt, df=hos.statin120, study=statin.study, name="stabilized.hos.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin120$st.iptw.wt, df=hos.statin120, study=statin.study, name="stabilized.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin120$st.iptw.wt, df=hos.statin120, study=statin.study, name="stabilized.hos.statin120")

print_table(hos.statin120, wt=hos.statin120$st.iptw.wt, df.study=statin.study, save_name="stabilized.hos.statin120")

sink()

#### IPTW : ATT ####

hos.statin120$iptw.att <- hos.statin120$ps/(1-hos.statin120$ps)
hos.statin120$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[Hos_IPTW_ATT]results_hos.statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(hos.statin120))
print(statin.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.statin120$statin, hos.statin120$ps, hos.statin120$iptw.att, "hos.statin120.att")
outcome.model(Y="outcome1", hos.statin120$iptw.att, df=hos.statin120, study=statin.study, name="hos.statin120.att") # 0.6249

outcome.model(Y="Death", hos.statin120$iptw.att, df=hos.statin120, study=statin.study, name="hos.statin120.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin120$iptw.att, df=hos.statin120, study=statin.study, name="hos.statin120.att")
outcome.model(Y="ICU.admission", hos.statin120$iptw.att, df=hos.statin120, study=statin.study, name="hos.statin120.att")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin120$iptw.att, df=hos.statin120, study=statin.study, name="hos.statin120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin120$iptw.att, df=hos.statin120, study=statin.study, name="hos.statin120.att")

print_table(hos.statin120, hos.statin120$iptw.att, df.study=statin.study, save_name="hos.statin120.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim_ATT]results_hos.statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(hos.statin120))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.statin120$trim.att <- hos.statin120$iptw.att
hos.statin120$trim.att[(hos.statin120$ps<ps.cut | hos.statin120$ps>(1-ps.cut))] <- 0
print(nrow(hos.statin120[hos.statin120$trim.att!=0,]))

ps_summary(hos.statin120$statin[hos.statin120$trim.att!=0], hos.statin120$ps[hos.statin120$trim.att!=0], hos.statin120$trim.att[hos.statin120$trim.att!=0], "trim.hos.statin120.att")
outcome.model(Y="outcome1", hos.statin120$trim.att, df=hos.statin120, study=statin.study, name="trim.hos.statin120.att") # 0.6802

outcome.model(Y="Death", hos.statin120$trim.att, df=hos.statin120, study=statin.study, name="trim.hos.statin120.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin120$trim.att, df=hos.statin120, study=statin.study, name="trim.hos.statin120.att")
outcome.model(Y="ICU.admission", hos.statin120$trim.att, df=hos.statin120, study=statin.study, name="trim.hos.statin120.att")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin120$trim.att, df=hos.statin120, study=statin.study, name="trim.hos.statin120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin120$trim.att, df=hos.statin120, study=statin.study, name="trim.hos.statin120.att")

print_table(hos.statin120, hos.statin120$trim.att, df.study=statin.study, save_name="trim.hos.statin120.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized_ATT]results_hos.statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(hos.statin120))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
hos.statin120$st.iptw.att <- ( (1-p.statin)/p.statin) * (hos.statin120$ps / (1-hos.statin120$ps) )  *  (1-hos.statin120$statin)
hos.statin120$st.iptw.att[ind.trt1] <- 1
ps_summary(hos.statin120$statin, hos.statin120$ps, hos.statin120$st.iptw.att, "stabilized.att.hos.statin120")

outcome.model(Y="outcome1", hos.statin120$st.iptw.att, df=hos.statin120, study=statin.study, name="stabilized.att.hos.statin120") # -1.21

outcome.model(Y="Death", hos.statin120$st.iptw.att, df=hos.statin120, study=statin.study, name="stabilized.att.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin120$st.iptw.att, df=hos.statin120, study=statin.study, name="stabilized.att.hos.statin120")
outcome.model(Y="ICU.admission", hos.statin120$st.iptw.att, df=hos.statin120, study=statin.study, name="stabilized.att.hos.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin120$st.iptw.att, df=hos.statin120, study=statin.study, name="stabilized.att.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin120$st.iptw.att, df=hos.statin120, study=statin.study, name="stabilized.att.hos.statin120")

print_table(hos.statin120, wt=hos.statin120$st.iptw.att, df.study=statin.study, save_name="stabilized.att.hos.statin120")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[Hos_Deciles]results_hos.statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(hos.statin120))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

hos.statin120$ps.dec <- cut(hos.statin120$ps, 
                            breaks=c(quantile(hos.statin120$ps, probs=seq(0,1,0.1))),
                            labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(hos.statin120$ps.dec)); cat("\n")
hos.statin120$dec_info <- paste0(hos.statin120$statin, ":", hos.statin120$ps.dec)

cat("\n"); cat(table(hos.statin120$dec_info)); cat("\n"); 
hos.statin120.dec <- data.frame(table(hos.statin120$ps.dec, hos.statin120$statin))
cat("\n"); print(hos.statin120.dec); cat("\n"); 

colnames(hos.statin120.dec) <- c("ps.dec", "statin", "n")
hos.statin120.dec$dec_info <- paste0(hos.statin120.dec$statin, ":", hos.statin120.dec$ps.dec)
hos.statin120.dec$n[hos.statin120.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.statin120.dec$dec.wt <- 1/hos.statin120.dec$n

if (is.null(hos.statin120$dec.wt)) {
  hos.statin120 <- merge(hos.statin120, hos.statin120.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(hos.statin120$statin, hos.statin120$ps, hos.statin120$dec.wt, "dec.hos.statin120")
outcome.model(Y="outcome1", hos.statin120$dec.wt, df=hos.statin120, study=statin.study, name="dec.hos.statin120") # -0.03765

outcome.model(Y="Death", hos.statin120$dec.wt, df=hos.statin120, study=statin.study, name="dec.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin120$dec.wt, df=hos.statin120, study=statin.study, name="dec.hos.statin120")
outcome.model(Y="ICU.admission", hos.statin120$dec.wt, df=hos.statin120, study=statin.study, name="dec.hos.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin120$dec.wt, df=hos.statin120, study=statin.study, name="dec.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin120$dec.wt, df=hos.statin120, study=statin.study, name="dec.hos.statin120")

print_table(hos.statin120, wt=hos.statin120$dec.wt, df.study=statin.study, save_name="dec.hos.statin120")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[Hos_Matching]results_hos.statin120.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = hos.statin120$ps[ind.trt0]
trt1.ps = hos.statin120$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.statin120$match.wt = rep(0,nrow(hos.statin120))
hos.statin120$match.wt[ind.trt1] = 1
hos.statin120$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(hos.statin120$statin, hos.statin120$ps, hos.statin120$match.wt, "match.hos.statin120")
outcome.model(Y="outcome1", hos.statin120$match.wt, df=hos.statin120, study=statin.study, name="match.hos.statin120") # 1.6468

outcome.model(Y="Death", hos.statin120$match.wt, df=hos.statin120, study=statin.study, name="match.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin120$match.wt, df=hos.statin120, study=statin.study, name="match.hos.statin120")
outcome.model(Y="ICU.admission", hos.statin120$match.wt, df=hos.statin120, study=statin.study, name="match.hos.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin120$match.wt, df=hos.statin120, study=statin.study, name="match.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin120$match.wt, df=hos.statin120, study=statin.study, name="match.hos.statin120")

print_table(hos.statin120, wt=hos.statin120$match.wt, df.study=statin.study, save_name="match.hos.statin120")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[Hos_Kernel]results_hos.statin120.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

hos.statin120$kernel.wt = rep(0,nrow(hos.statin120))
hos.statin120$kernel.wt[ind.trt0] = colSums(matchMat.k)
hos.statin120$kernel.wt[ind.trt1] = 1

ps_summary(hos.statin120$statin, hos.statin120$ps, hos.statin120$kernel.wt, "kernel.hos.statin120")
outcome.model(Y="outcome1", hos.statin120$kernel.wt, df=hos.statin120, study=statin.study, name="kernel.hos.statin120")

outcome.model(Y="Death", hos.statin120$kernel.wt, df=hos.statin120, study=statin.study, name="kernel.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin120$kernel.wt, df=hos.statin120, study=statin.study, name="kernel.hos.statin120")
outcome.model(Y="ICU.admission", hos.statin120$kernel.wt, df=hos.statin120, study=statin.study, name="kernel.hos.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin120$kernel.wt, df=hos.statin120, study=statin.study, name="kernel.hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin120$kernel.wt, df=hos.statin120, study=statin.study, name="kernel.hos.statin120")

print_table(hos.statin120, wt=hos.statin120$kernel.wt, df.study=statin.study, save_name="kernel.hos.statin120")

sink()


###############################################
############  Hos statin 90    ###############
###############################################

hos.statin90.con <- hos.statin90[,statin.confounder]
hos.statin90.fit <- glm(hos.statin90$statin ~ ., data = hos.statin90.con, family="binomial")

hos.statin90$ps <- hos.statin90.fit$fitted.values
ind.trt0 <- which(hos.statin90$statin == 0)
ind.trt1 <- which(hos.statin90$statin == 1)

hos.statin90$iptw.wt <- ( 1/hos.statin90$ps )*hos.statin90$statin + ( 1/(1-hos.statin90$ps) )*( 1-hos.statin90$statin )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW]results_hos.statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(hos.statin90))
print(statin.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.statin90$statin, hos.statin90$ps, hos.statin90$iptw.wt, "hos.statin90")
outcome.model(Y="outcome1", wt=hos.statin90$iptw.wt, df=hos.statin90, study=statin.study, name="hos.statin90") # 0.6249

outcome.model(Y="Death", wt=hos.statin90$iptw.wt, df=hos.statin90, study=statin.study, name="hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.statin90$iptw.wt, df=hos.statin90, study=statin.study, name="hos.statin90")
outcome.model(Y="ICU.admission", wt=hos.statin90$iptw.wt, df=hos.statin90, study=statin.study, name="hos.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt=hos.statin90$iptw.wt, df=hos.statin90, study=statin.study, name="hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.statin90$iptw.wt, df=hos.statin90, study=statin.study, name="hos.statin90")

print_table(hos.statin90, wt=hos.statin90$iptw.wt, df.study=statin.study, save_name="hos.statin90")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim]results_hos.statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(hos.statin90))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.statin90$trim.wt <- hos.statin90$iptw.wt
hos.statin90$trim.wt[(hos.statin90$ps<ps.cut | hos.statin90$ps>(1-ps.cut))] <- 0
print(nrow(hos.statin90[hos.statin90$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(hos.statin90$statin[hos.statin90$trim.wt!=0], hos.statin90$ps[hos.statin90$trim.wt!=0], hos.statin90$trim.wt[hos.statin90$trim.wt!=0], "trim.hos.statin90")
outcome.model(Y="outcome1", wt=hos.statin90$trim.wt, df=hos.statin90, study=statin.study, name="trim.hos.statin90") # 0.6802

outcome.model(Y="Death", wt=hos.statin90$trim.wt, df=hos.statin90, study=statin.study, name="trim.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.statin90$trim.wt, df=hos.statin90, study=statin.study, name="trim.hos.statin90")
outcome.model(Y="ICU.admission", wt=hos.statin90$trim.wt, df=hos.statin90, study=statin.study, name="trim.hos.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt=hos.statin90$trim.wt, df=hos.statin90, study=statin.study, name="trim.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.statin90$trim.wt, df=hos.statin90, study=statin.study, name="trim.hos.statin90")

print_table(hos.statin90, wt=hos.statin90$trim.wt, df.study=statin.study, save_name="trim.hos.statin90")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized]results_hos.statin90.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.statin <- sum(hos.statin90$statin) / nrow(hos.statin90)
# wt : ATE
hos.statin90$st.iptw.wt <- ( p.statin/hos.statin90$ps )*hos.statin90$statin + ( (1-p.statin)/(1-hos.statin90$ps) )*( 1-hos.statin90$statin )

ps_summary(hos.statin90$statin, hos.statin90$ps, hos.statin90$st.iptw.wt, "stabilized.hos.statin90")

outcome.model(Y="outcome1", hos.statin90$st.iptw.wt, df=hos.statin90, study=statin.study, name="stabilized.hos.statin90") # -1.21

outcome.model(Y="Death", hos.statin90$st.iptw.wt, df=hos.statin90, study=statin.study, name="stabilized.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin90$st.iptw.wt, df=hos.statin90, study=statin.study, name="stabilized.hos.statin90")
outcome.model(Y="ICU.admission", hos.statin90$st.iptw.wt, df=hos.statin90, study=statin.study, name="stabilized.hos.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin90$st.iptw.wt, df=hos.statin90, study=statin.study, name="stabilized.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin90$st.iptw.wt, df=hos.statin90, study=statin.study, name="stabilized.hos.statin90")

print_table(hos.statin90, wt=hos.statin90$st.iptw.wt, df.study=statin.study, save_name="stabilized.hos.statin90")

sink()

#### IPTW : ATT ####

hos.statin90$iptw.att <- hos.statin90$ps/(1-hos.statin90$ps)
hos.statin90$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[Hos_IPTW_ATT]results_hos.statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(hos.statin90))
print(statin.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.statin90$statin, hos.statin90$ps, hos.statin90$iptw.att, "hos.statin90.att")
outcome.model(Y="outcome1", hos.statin90$iptw.att, df=hos.statin90, study=statin.study, name="hos.statin90.att") # 0.6249

outcome.model(Y="Death", hos.statin90$iptw.att, df=hos.statin90, study=statin.study, name="hos.statin90.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin90$iptw.att, df=hos.statin90, study=statin.study, name="hos.statin90.att")
outcome.model(Y="ICU.admission", hos.statin90$iptw.att, df=hos.statin90, study=statin.study, name="hos.statin90.att")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin90$iptw.att, df=hos.statin90, study=statin.study, name="hos.statin90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin90$iptw.att, df=hos.statin90, study=statin.study, name="hos.statin90.att")

print_table(hos.statin90, hos.statin90$iptw.att, df.study=statin.study, save_name="hos.statin90.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim_ATT]results_hos.statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(hos.statin90))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.statin90$trim.att <- hos.statin90$iptw.att
hos.statin90$trim.att[(hos.statin90$ps<ps.cut | hos.statin90$ps>(1-ps.cut))] <- 0
print(nrow(hos.statin90[hos.statin90$trim.att!=0,]))

ps_summary(hos.statin90$statin[hos.statin90$trim.att!=0], hos.statin90$ps[hos.statin90$trim.att!=0], hos.statin90$trim.att[hos.statin90$trim.att!=0], "trim.hos.statin90.att")
outcome.model(Y="outcome1", hos.statin90$trim.att, df=hos.statin90, study=statin.study, name="trim.hos.statin90.att") # 0.6802

outcome.model(Y="Death", hos.statin90$trim.att, df=hos.statin90, study=statin.study, name="trim.hos.statin90.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin90$trim.att, df=hos.statin90, study=statin.study, name="trim.hos.statin90.att")
outcome.model(Y="ICU.admission", hos.statin90$trim.att, df=hos.statin90, study=statin.study, name="trim.hos.statin90.att")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin90$trim.att, df=hos.statin90, study=statin.study, name="trim.hos.statin90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin90$trim.att, df=hos.statin90, study=statin.study, name="trim.hos.statin90.att")

print_table(hos.statin90, hos.statin90$trim.att, df.study=statin.study, save_name="trim.hos.statin90.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized_ATT]results_hos.statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(hos.statin90))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
hos.statin90$st.iptw.att <- ( (1-p.statin)/p.statin) * (hos.statin90$ps / (1-hos.statin90$ps) )  *  (1-hos.statin90$statin)
hos.statin90$st.iptw.att[ind.trt1] <- 1
ps_summary(hos.statin90$statin, hos.statin90$ps, hos.statin90$st.iptw.att, "stabilized.att.hos.statin90")

outcome.model(Y="outcome1", hos.statin90$st.iptw.att, df=hos.statin90, study=statin.study, name="stabilized.att.hos.statin90") # -1.21

outcome.model(Y="Death", hos.statin90$st.iptw.att, df=hos.statin90, study=statin.study, name="stabilized.att.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin90$st.iptw.att, df=hos.statin90, study=statin.study, name="stabilized.att.hos.statin90")
outcome.model(Y="ICU.admission", hos.statin90$st.iptw.att, df=hos.statin90, study=statin.study, name="stabilized.att.hos.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin90$st.iptw.att, df=hos.statin90, study=statin.study, name="stabilized.att.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin90$st.iptw.att, df=hos.statin90, study=statin.study, name="stabilized.att.hos.statin90")

print_table(hos.statin90, wt=hos.statin90$st.iptw.att, df.study=statin.study, save_name="stabilized.att.hos.statin90")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[Hos_Deciles]results_hos.statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
print(nrow(hos.statin90))
print(statin.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

hos.statin90$ps.dec <- cut(hos.statin90$ps, 
                           breaks=c(quantile(hos.statin90$ps, probs=seq(0,1,0.1))),
                           labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(hos.statin90$ps.dec)); cat("\n")
hos.statin90$dec_info <- paste0(hos.statin90$statin, ":", hos.statin90$ps.dec)

cat("\n"); cat(table(hos.statin90$dec_info)); cat("\n"); 
hos.statin90.dec <- data.frame(table(hos.statin90$ps.dec, hos.statin90$statin))
cat("\n"); print(hos.statin90.dec); cat("\n"); 

colnames(hos.statin90.dec) <- c("ps.dec", "statin", "n")
hos.statin90.dec$dec_info <- paste0(hos.statin90.dec$statin, ":", hos.statin90.dec$ps.dec)
hos.statin90.dec$n[hos.statin90.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.statin90.dec$dec.wt <- 1/hos.statin90.dec$n

if (is.null(hos.statin90$dec.wt)) {
  hos.statin90 <- merge(hos.statin90, hos.statin90.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(hos.statin90$statin, hos.statin90$ps, hos.statin90$dec.wt, "dec.hos.statin90")
outcome.model(Y="outcome1", hos.statin90$dec.wt, df=hos.statin90, study=statin.study, name="dec.hos.statin90") # -0.03765

outcome.model(Y="Death", hos.statin90$dec.wt, df=hos.statin90, study=statin.study, name="dec.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin90$dec.wt, df=hos.statin90, study=statin.study, name="dec.hos.statin90")
outcome.model(Y="ICU.admission", hos.statin90$dec.wt, df=hos.statin90, study=statin.study, name="dec.hos.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin90$dec.wt, df=hos.statin90, study=statin.study, name="dec.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin90$dec.wt, df=hos.statin90, study=statin.study, name="dec.hos.statin90")

print_table(hos.statin90, wt=hos.statin90$dec.wt, df.study=statin.study, save_name="dec.hos.statin90")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[Hos_Matching]results_hos.statin90.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = hos.statin90$ps[ind.trt0]
trt1.ps = hos.statin90$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.statin90$match.wt = rep(0,nrow(hos.statin90))
hos.statin90$match.wt[ind.trt1] = 1
hos.statin90$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(hos.statin90$statin, hos.statin90$ps, hos.statin90$match.wt, "match.hos.statin90")
outcome.model(Y="outcome1", hos.statin90$match.wt, df=hos.statin90, study=statin.study, name="match.hos.statin90") # 1.6468

outcome.model(Y="Death", hos.statin90$match.wt, df=hos.statin90, study=statin.study, name="match.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin90$match.wt, df=hos.statin90, study=statin.study, name="match.hos.statin90")
outcome.model(Y="ICU.admission", hos.statin90$match.wt, df=hos.statin90, study=statin.study, name="match.hos.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin90$match.wt, df=hos.statin90, study=statin.study, name="match.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin90$match.wt, df=hos.statin90, study=statin.study, name="match.hos.statin90")

print_table(hos.statin90, wt=hos.statin90$match.wt, df.study=statin.study, save_name="match.hos.statin90")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[Hos_Kernel]results_hos.statin90.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

hos.statin90$kernel.wt = rep(0,nrow(hos.statin90))
hos.statin90$kernel.wt[ind.trt0] = colSums(matchMat.k)
hos.statin90$kernel.wt[ind.trt1] = 1

ps_summary(hos.statin90$statin, hos.statin90$ps, hos.statin90$kernel.wt, "kernel.hos.statin90")
outcome.model(Y="outcome1", hos.statin90$kernel.wt, df=hos.statin90, study=statin.study, name="kernel.hos.statin90")

outcome.model(Y="Death", hos.statin90$kernel.wt, df=hos.statin90, study=statin.study, name="kernel.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.statin90$kernel.wt, df=hos.statin90, study=statin.study, name="kernel.hos.statin90")
outcome.model(Y="ICU.admission", hos.statin90$kernel.wt, df=hos.statin90, study=statin.study, name="kernel.hos.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", hos.statin90$kernel.wt, df=hos.statin90, study=statin.study, name="kernel.hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.statin90$kernel.wt, df=hos.statin90, study=statin.study, name="kernel.hos.statin90")

print_table(hos.statin90, wt=hos.statin90$kernel.wt, df.study=statin.study, save_name="kernel.hos.statin90")

sink()

###############################################
############  All immunosuppressant 120    ###############
###############################################

immunosuppressant120.con <- immunosuppressant120[,immunosuppressant.confounder]
immunosuppressant120.fit <- glm(immunosuppressant120$immunosuppressant ~ ., data = immunosuppressant120.con, family="binomial")

immunosuppressant120$ps <- immunosuppressant120.fit$fitted.values
ind.trt0 <- which(immunosuppressant120$immunosuppressant == 0)
ind.trt1 <- which(immunosuppressant120$immunosuppressant == 1)

immunosuppressant120$iptw.wt <- ( 1/immunosuppressant120$ps )*immunosuppressant120$immunosuppressant + ( 1/(1-immunosuppressant120$ps) )*( 1-immunosuppressant120$immunosuppressant )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW]results_immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(immunosuppressant120$immunosuppressant, immunosuppressant120$ps, immunosuppressant120$iptw.wt, "immunosuppressant120")
outcome.model(Y="outcome1", wt=immunosuppressant120$iptw.wt, df=immunosuppressant120, study=immunosuppressant.study, name="immunosuppressant120") # 0.6249

outcome.model(Y="Death", wt=immunosuppressant120$iptw.wt, df=immunosuppressant120, study=immunosuppressant.study, name="immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=immunosuppressant120$iptw.wt, df=immunosuppressant120, study=immunosuppressant.study, name="immunosuppressant120")
outcome.model(Y="ICU.admission", wt=immunosuppressant120$iptw.wt, df=immunosuppressant120, study=immunosuppressant.study, name="immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=immunosuppressant120$iptw.wt, df=immunosuppressant120, study=immunosuppressant.study, name="immunosuppressant120")

print_table(df.all=immunosuppressant120, wt=immunosuppressant120$iptw.wt, df.study=immunosuppressant.study, save_name="immunosuppressant120")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim]results_immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

immunosuppressant120$trim.wt <- immunosuppressant120$iptw.wt
immunosuppressant120$trim.wt[(immunosuppressant120$ps<ps.cut | immunosuppressant120$ps>(1-ps.cut))] <- 0
print(nrow(immunosuppressant120[immunosuppressant120$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(immunosuppressant120$immunosuppressant[immunosuppressant120$trim.wt!=0], immunosuppressant120$ps[immunosuppressant120$trim.wt!=0], immunosuppressant120$trim.wt[immunosuppressant120$trim.wt!=0], "trim.immunosuppressant120")
outcome.model(Y="outcome1", wt=immunosuppressant120$trim.wt, df=immunosuppressant120, study=immunosuppressant.study, name="trim.immunosuppressant120") # 0.6802

outcome.model(Y="Death", wt=immunosuppressant120$trim.wt, df=immunosuppressant120, study=immunosuppressant.study, name="trim.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=immunosuppressant120$trim.wt, df=immunosuppressant120, study=immunosuppressant.study, name="trim.immunosuppressant120")
outcome.model(Y="ICU.admission", wt=immunosuppressant120$trim.wt, df=immunosuppressant120, study=immunosuppressant.study, name="trim.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=immunosuppressant120$trim.wt, df=immunosuppressant120, study=immunosuppressant.study, name="trim.immunosuppressant120")

print_table(df.all=immunosuppressant120, wt=immunosuppressant120$trim.wt, df.study=immunosuppressant.study, save_name="trim.immunosuppressant120")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized]results_immunosuppressant120.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.immunosuppressant <- sum(immunosuppressant120$immunosuppressant) / nrow(immunosuppressant120)
# wt : ATE
immunosuppressant120$st.iptw.wt <- ( p.immunosuppressant/immunosuppressant120$ps )*immunosuppressant120$immunosuppressant + ( (1-p.immunosuppressant)/(1-immunosuppressant120$ps) )*( 1-immunosuppressant120$immunosuppressant )

ps_summary(immunosuppressant120$immunosuppressant, immunosuppressant120$ps, immunosuppressant120$st.iptw.wt, "stabilized.immunosuppressant120")

outcome.model(Y="outcome1", immunosuppressant120$st.iptw.wt, df=immunosuppressant120, study=immunosuppressant.study, name="stabilized.immunosuppressant120") # -1.21

outcome.model(Y="Death", immunosuppressant120$st.iptw.wt, df=immunosuppressant120, study=immunosuppressant.study, name="stabilized.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant120$st.iptw.wt, df=immunosuppressant120, study=immunosuppressant.study, name="stabilized.immunosuppressant120")
outcome.model(Y="ICU.admission", immunosuppressant120$st.iptw.wt, df=immunosuppressant120, study=immunosuppressant.study, name="stabilized.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant120$st.iptw.wt, df=immunosuppressant120, study=immunosuppressant.study, name="stabilized.immunosuppressant120")

print_table(df.all=immunosuppressant120, wt=immunosuppressant120$st.iptw.wt, df.study=immunosuppressant.study, save_name="stabilized.immunosuppressant120")

sink()

#### IPTW : ATT ####

immunosuppressant120$iptw.att <- immunosuppressant120$ps/(1-immunosuppressant120$ps)
immunosuppressant120$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[ALL_IPTW_ATT]results_immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(immunosuppressant120$immunosuppressant, immunosuppressant120$ps, immunosuppressant120$iptw.att, "immunosuppressant120.att")
outcome.model(Y="outcome1", immunosuppressant120$iptw.att, df=immunosuppressant120, study=immunosuppressant.study, name="immunosuppressant120.att") # 0.6249

outcome.model(Y="Death", immunosuppressant120$iptw.att, df=immunosuppressant120, study=immunosuppressant.study, name="immunosuppressant120.att")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant120$iptw.att, df=immunosuppressant120, study=immunosuppressant.study, name="immunosuppressant120.att")
outcome.model(Y="ICU.admission", immunosuppressant120$iptw.att, df=immunosuppressant120, study=immunosuppressant.study, name="immunosuppressant120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant120$iptw.att, df=immunosuppressant120, study=immunosuppressant.study, name="immunosuppressant120.att")

print_table(df.all=immunosuppressant120, immunosuppressant120$iptw.att, df.study=immunosuppressant.study, save_name="immunosuppressant120.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim_ATT]results_immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

immunosuppressant120$trim.att <- immunosuppressant120$iptw.att
immunosuppressant120$trim.att[(immunosuppressant120$ps<ps.cut | immunosuppressant120$ps>(1-ps.cut))] <- 0
print(nrow(immunosuppressant120[immunosuppressant120$trim.att!=0,]))

ps_summary(immunosuppressant120$immunosuppressant[immunosuppressant120$trim.att!=0], immunosuppressant120$ps[immunosuppressant120$trim.att!=0], immunosuppressant120$trim.att[immunosuppressant120$trim.att!=0], "trim.immunosuppressant120.att")
outcome.model(Y="outcome1", immunosuppressant120$trim.att, df=immunosuppressant120, study=immunosuppressant.study, name="trim.immunosuppressant120.att") # 0.6802

outcome.model(Y="Death", immunosuppressant120$trim.att, df=immunosuppressant120, study=immunosuppressant.study, name="trim.immunosuppressant120.att")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant120$trim.att, df=immunosuppressant120, study=immunosuppressant.study, name="trim.immunosuppressant120.att")
outcome.model(Y="ICU.admission", immunosuppressant120$trim.att, df=immunosuppressant120, study=immunosuppressant.study, name="trim.immunosuppressant120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant120$trim.att, df=immunosuppressant120, study=immunosuppressant.study, name="trim.immunosuppressant120.att")

print_table(df.all=immunosuppressant120, immunosuppressant120$trim.att, df.study=immunosuppressant.study, save_name="trim.immunosuppressant120.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized_ATT]results_immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
immunosuppressant120$st.iptw.att <- ( (1-p.immunosuppressant)/p.immunosuppressant) * (immunosuppressant120$ps / (1-immunosuppressant120$ps) )  *  (1-immunosuppressant120$immunosuppressant)
immunosuppressant120$st.iptw.att[ind.trt1] <- 1
ps_summary(immunosuppressant120$immunosuppressant, immunosuppressant120$ps, immunosuppressant120$st.iptw.att, "stabilized.att.immunosuppressant120")

outcome.model(Y="outcome1", immunosuppressant120$st.iptw.att, df=immunosuppressant120, study=immunosuppressant.study, name="stabilized.att.immunosuppressant120") # -1.21

outcome.model(Y="Death", immunosuppressant120$st.iptw.att, df=immunosuppressant120, study=immunosuppressant.study, name="stabilized.att.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant120$st.iptw.att, df=immunosuppressant120, study=immunosuppressant.study, name="stabilized.att.immunosuppressant120")
outcome.model(Y="ICU.admission", immunosuppressant120$st.iptw.att, df=immunosuppressant120, study=immunosuppressant.study, name="stabilized.att.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant120$st.iptw.att, df=immunosuppressant120, study=immunosuppressant.study, name="stabilized.att.immunosuppressant120")

print_table(df.all=immunosuppressant120, wt=immunosuppressant120$st.iptw.att, df.study=immunosuppressant.study, save_name="stabilized.att.immunosuppressant120")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[ALL_Deciles]results_immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

immunosuppressant120$ps.dec <- cut(immunosuppressant120$ps, 
                                   breaks=c(quantile(immunosuppressant120$ps, probs=seq(0,1,0.1))),
                                   labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(immunosuppressant120$ps.dec)); cat("\n")
immunosuppressant120$dec_info <- paste0(immunosuppressant120$immunosuppressant, ":", immunosuppressant120$ps.dec)

cat("\n"); cat(table(immunosuppressant120$dec_info)); cat("\n"); 
immunosuppressant120.dec <- data.frame(table(immunosuppressant120$ps.dec, immunosuppressant120$immunosuppressant))
cat("\n"); print(immunosuppressant120.dec); cat("\n"); 

colnames(immunosuppressant120.dec) <- c("ps.dec", "immunosuppressant", "n")
immunosuppressant120.dec$dec_info <- paste0(immunosuppressant120.dec$immunosuppressant, ":", immunosuppressant120.dec$ps.dec)
immunosuppressant120.dec$n[immunosuppressant120.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
immunosuppressant120.dec$dec.wt <- 1/immunosuppressant120.dec$n

if (is.null(immunosuppressant120$dec.wt)) {
  immunosuppressant120 <- merge(immunosuppressant120, immunosuppressant120.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(immunosuppressant120$immunosuppressant, immunosuppressant120$ps, immunosuppressant120$dec.wt, "dec.immunosuppressant120")
outcome.model(Y="outcome1", immunosuppressant120$dec.wt, df=immunosuppressant120, study=immunosuppressant.study, name="dec.immunosuppressant120") # -0.03765

outcome.model(Y="Death", immunosuppressant120$dec.wt, df=immunosuppressant120, study=immunosuppressant.study, name="dec.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant120$dec.wt, df=immunosuppressant120, study=immunosuppressant.study, name="dec.immunosuppressant120")
outcome.model(Y="ICU.admission", immunosuppressant120$dec.wt, df=immunosuppressant120, study=immunosuppressant.study, name="dec.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant120$dec.wt, df=immunosuppressant120, study=immunosuppressant.study, name="dec.immunosuppressant120")

print_table(df.all=immunosuppressant120, wt=immunosuppressant120$dec.wt, df.study=immunosuppressant.study, save_name="dec.immunosuppressant120")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[ALL_Matching]results_immunosuppressant120.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = immunosuppressant120$ps[ind.trt0]
trt1.ps = immunosuppressant120$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
immunosuppressant120$match.wt = rep(0,nrow(immunosuppressant120))
immunosuppressant120$match.wt[ind.trt1] = 1
immunosuppressant120$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(immunosuppressant120$immunosuppressant, immunosuppressant120$ps, immunosuppressant120$match.wt, "match.immunosuppressant120")
outcome.model(Y="outcome1", immunosuppressant120$match.wt, df=immunosuppressant120, study=immunosuppressant.study, name="match.immunosuppressant120") # 1.6468

outcome.model(Y="Death", immunosuppressant120$match.wt, df=immunosuppressant120, study=immunosuppressant.study, name="match.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant120$match.wt, df=immunosuppressant120, study=immunosuppressant.study, name="match.immunosuppressant120")
outcome.model(Y="ICU.admission", immunosuppressant120$match.wt, df=immunosuppressant120, study=immunosuppressant.study, name="match.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant120$match.wt, df=immunosuppressant120, study=immunosuppressant.study, name="match.immunosuppressant120")

print_table(df.all=immunosuppressant120, wt=immunosuppressant120$match.wt, df.study=immunosuppressant.study, save_name="match.immunosuppressant120")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[ALL_Kernel]results_immunosuppressant120.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

immunosuppressant120$kernel.wt = rep(0,nrow(immunosuppressant120))
immunosuppressant120$kernel.wt[ind.trt0] = colSums(matchMat.k)
immunosuppressant120$kernel.wt[ind.trt1] = 1

ps_summary(immunosuppressant120$immunosuppressant, immunosuppressant120$ps, immunosuppressant120$kernel.wt, "kernel.immunosuppressant120")
outcome.model(Y="outcome1", immunosuppressant120$kernel.wt, df=immunosuppressant120, study=immunosuppressant.study, name="kernel.immunosuppressant120")

outcome.model(Y="Death", immunosuppressant120$kernel.wt, df=immunosuppressant120, study=immunosuppressant.study, name="kernel.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant120$kernel.wt, df=immunosuppressant120, study=immunosuppressant.study, name="kernel.immunosuppressant120")
outcome.model(Y="ICU.admission", immunosuppressant120$kernel.wt, df=immunosuppressant120, study=immunosuppressant.study, name="kernel.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant120$kernel.wt, df=immunosuppressant120, study=immunosuppressant.study, name="kernel.immunosuppressant120")

print_table(df.all=immunosuppressant120, wt=immunosuppressant120$kernel.wt, df.study=immunosuppressant.study, save_name="kernel.immunosuppressant120")

sink()


###############################################
############  All immunosuppressant 90    ###############
###############################################

immunosuppressant90.con <- immunosuppressant90[,immunosuppressant.confounder]
immunosuppressant90.fit <- glm(immunosuppressant90$immunosuppressant ~ ., data = immunosuppressant90.con, family="binomial")

immunosuppressant90$ps <- immunosuppressant90.fit$fitted.values
ind.trt0 <- which(immunosuppressant90$immunosuppressant == 0)
ind.trt1 <- which(immunosuppressant90$immunosuppressant == 1)

immunosuppressant90$iptw.wt <- ( 1/immunosuppressant90$ps )*immunosuppressant90$immunosuppressant + ( 1/(1-immunosuppressant90$ps) )*( 1-immunosuppressant90$immunosuppressant )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW]results_immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(immunosuppressant90$immunosuppressant, immunosuppressant90$ps, immunosuppressant90$iptw.wt, "immunosuppressant90")
outcome.model(Y="outcome1", wt=immunosuppressant90$iptw.wt, df=immunosuppressant90, study=immunosuppressant.study, name="immunosuppressant90") # 0.6249

outcome.model(Y="Death", wt=immunosuppressant90$iptw.wt, df=immunosuppressant90, study=immunosuppressant.study, name="immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=immunosuppressant90$iptw.wt, df=immunosuppressant90, study=immunosuppressant.study, name="immunosuppressant90")
outcome.model(Y="ICU.admission", wt=immunosuppressant90$iptw.wt, df=immunosuppressant90, study=immunosuppressant.study, name="immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=immunosuppressant90$iptw.wt, df=immunosuppressant90, study=immunosuppressant.study, name="immunosuppressant90")

print_table(df.all=immunosuppressant90, wt=immunosuppressant90$iptw.wt, df.study=immunosuppressant.study, save_name="immunosuppressant90")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim]results_immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

immunosuppressant90$trim.wt <- immunosuppressant90$iptw.wt
immunosuppressant90$trim.wt[(immunosuppressant90$ps<ps.cut | immunosuppressant90$ps>(1-ps.cut))] <- 0
print(nrow(immunosuppressant90[immunosuppressant90$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(immunosuppressant90$immunosuppressant[immunosuppressant90$trim.wt!=0], immunosuppressant90$ps[immunosuppressant90$trim.wt!=0], immunosuppressant90$trim.wt[immunosuppressant90$trim.wt!=0], "trim.immunosuppressant90")
outcome.model(Y="outcome1", wt=immunosuppressant90$trim.wt, df=immunosuppressant90, study=immunosuppressant.study, name="trim.immunosuppressant90") # 0.6802

outcome.model(Y="Death", wt=immunosuppressant90$trim.wt, df=immunosuppressant90, study=immunosuppressant.study, name="trim.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=immunosuppressant90$trim.wt, df=immunosuppressant90, study=immunosuppressant.study, name="trim.immunosuppressant90")
outcome.model(Y="ICU.admission", wt=immunosuppressant90$trim.wt, df=immunosuppressant90, study=immunosuppressant.study, name="trim.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=immunosuppressant90$trim.wt, df=immunosuppressant90, study=immunosuppressant.study, name="trim.immunosuppressant90")

print_table(df.all=immunosuppressant90, wt=immunosuppressant90$trim.wt, df.study=immunosuppressant.study, save_name="trim.immunosuppressant90")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized]results_immunosuppressant90.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.immunosuppressant <- sum(immunosuppressant90$immunosuppressant) / nrow(immunosuppressant90)
# wt : ATE
immunosuppressant90$st.iptw.wt <- ( p.immunosuppressant/immunosuppressant90$ps )*immunosuppressant90$immunosuppressant + ( (1-p.immunosuppressant)/(1-immunosuppressant90$ps) )*( 1-immunosuppressant90$immunosuppressant )

ps_summary(immunosuppressant90$immunosuppressant, immunosuppressant90$ps, immunosuppressant90$st.iptw.wt, "stabilized.immunosuppressant90")

outcome.model(Y="outcome1", immunosuppressant90$st.iptw.wt, df=immunosuppressant90, study=immunosuppressant.study, name="stabilized.immunosuppressant90") # -1.21

outcome.model(Y="Death", immunosuppressant90$st.iptw.wt, df=immunosuppressant90, study=immunosuppressant.study, name="stabilized.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant90$st.iptw.wt, df=immunosuppressant90, study=immunosuppressant.study, name="stabilized.immunosuppressant90")
outcome.model(Y="ICU.admission", immunosuppressant90$st.iptw.wt, df=immunosuppressant90, study=immunosuppressant.study, name="stabilized.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant90$st.iptw.wt, df=immunosuppressant90, study=immunosuppressant.study, name="stabilized.immunosuppressant90")

print_table(df.all=immunosuppressant90, wt=immunosuppressant90$st.iptw.wt, df.study=immunosuppressant.study, save_name="stabilized.immunosuppressant90")

sink()

#### IPTW : ATT ####

immunosuppressant90$iptw.att <- immunosuppressant90$ps/(1-immunosuppressant90$ps)
immunosuppressant90$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[ALL_IPTW_ATT]results_immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(immunosuppressant90$immunosuppressant, immunosuppressant90$ps, immunosuppressant90$iptw.att, "immunosuppressant90.att")
outcome.model(Y="outcome1", immunosuppressant90$iptw.att, df=immunosuppressant90, study=immunosuppressant.study, name="immunosuppressant90.att") # 0.6249

outcome.model(Y="Death", immunosuppressant90$iptw.att, df=immunosuppressant90, study=immunosuppressant.study, name="immunosuppressant90.att")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant90$iptw.att, df=immunosuppressant90, study=immunosuppressant.study, name="immunosuppressant90.att")
outcome.model(Y="ICU.admission", immunosuppressant90$iptw.att, df=immunosuppressant90, study=immunosuppressant.study, name="immunosuppressant90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant90$iptw.att, df=immunosuppressant90, study=immunosuppressant.study, name="immunosuppressant90.att")

print_table(df.all=immunosuppressant90, immunosuppressant90$iptw.att, df.study=immunosuppressant.study, save_name="immunosuppressant90.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim_ATT]results_immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

immunosuppressant90$trim.att <- immunosuppressant90$iptw.att
immunosuppressant90$trim.att[(immunosuppressant90$ps<ps.cut | immunosuppressant90$ps>(1-ps.cut))] <- 0
print(nrow(immunosuppressant90[immunosuppressant90$trim.att!=0,]))

ps_summary(immunosuppressant90$immunosuppressant[immunosuppressant90$trim.att!=0], immunosuppressant90$ps[immunosuppressant90$trim.att!=0], immunosuppressant90$trim.att[immunosuppressant90$trim.att!=0], "trim.immunosuppressant90.att")
outcome.model(Y="outcome1", immunosuppressant90$trim.att, df=immunosuppressant90, study=immunosuppressant.study, name="trim.immunosuppressant90.att") # 0.6802

outcome.model(Y="Death", immunosuppressant90$trim.att, df=immunosuppressant90, study=immunosuppressant.study, name="trim.immunosuppressant90.att")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant90$trim.att, df=immunosuppressant90, study=immunosuppressant.study, name="trim.immunosuppressant90.att")
outcome.model(Y="ICU.admission", immunosuppressant90$trim.att, df=immunosuppressant90, study=immunosuppressant.study, name="trim.immunosuppressant90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant90$trim.att, df=immunosuppressant90, study=immunosuppressant.study, name="trim.immunosuppressant90.att")

print_table(df.all=immunosuppressant90, immunosuppressant90$trim.att, df.study=immunosuppressant.study, save_name="trim.immunosuppressant90.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized_ATT]results_immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
immunosuppressant90$st.iptw.att <- ( (1-p.immunosuppressant)/p.immunosuppressant) * (immunosuppressant90$ps / (1-immunosuppressant90$ps) )  *  (1-immunosuppressant90$immunosuppressant)
immunosuppressant90$st.iptw.att[ind.trt1] <- 1
ps_summary(immunosuppressant90$immunosuppressant, immunosuppressant90$ps, immunosuppressant90$st.iptw.att, "stabilized.att.immunosuppressant90")

outcome.model(Y="outcome1", immunosuppressant90$st.iptw.att, df=immunosuppressant90, study=immunosuppressant.study, name="stabilized.att.immunosuppressant90") # -1.21

outcome.model(Y="Death", immunosuppressant90$st.iptw.att, df=immunosuppressant90, study=immunosuppressant.study, name="stabilized.att.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant90$st.iptw.att, df=immunosuppressant90, study=immunosuppressant.study, name="stabilized.att.immunosuppressant90")
outcome.model(Y="ICU.admission", immunosuppressant90$st.iptw.att, df=immunosuppressant90, study=immunosuppressant.study, name="stabilized.att.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant90$st.iptw.att, df=immunosuppressant90, study=immunosuppressant.study, name="stabilized.att.immunosuppressant90")

print_table(df.all=immunosuppressant90, wt=immunosuppressant90$st.iptw.att, df.study=immunosuppressant.study, save_name="stabilized.att.immunosuppressant90")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[ALL_Deciles]results_immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

immunosuppressant90$ps.dec <- cut(immunosuppressant90$ps, 
                                  breaks=c(quantile(immunosuppressant90$ps, probs=seq(0,1,0.1))),
                                  labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(immunosuppressant90$ps.dec)); cat("\n")
immunosuppressant90$dec_info <- paste0(immunosuppressant90$immunosuppressant, ":", immunosuppressant90$ps.dec)

cat("\n"); cat(table(immunosuppressant90$dec_info)); cat("\n"); 
immunosuppressant90.dec <- data.frame(table(immunosuppressant90$ps.dec, immunosuppressant90$immunosuppressant))
cat("\n"); print(immunosuppressant90.dec); cat("\n"); 

colnames(immunosuppressant90.dec) <- c("ps.dec", "immunosuppressant", "n")
immunosuppressant90.dec$dec_info <- paste0(immunosuppressant90.dec$immunosuppressant, ":", immunosuppressant90.dec$ps.dec)
immunosuppressant90.dec$n[immunosuppressant90.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
immunosuppressant90.dec$dec.wt <- 1/immunosuppressant90.dec$n

if (is.null(immunosuppressant90$dec.wt)) {
  immunosuppressant90 <- merge(immunosuppressant90, immunosuppressant90.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(immunosuppressant90$immunosuppressant, immunosuppressant90$ps, immunosuppressant90$dec.wt, "dec.immunosuppressant90")
outcome.model(Y="outcome1", immunosuppressant90$dec.wt, df=immunosuppressant90, study=immunosuppressant.study, name="dec.immunosuppressant90") # -0.03765

outcome.model(Y="Death", immunosuppressant90$dec.wt, df=immunosuppressant90, study=immunosuppressant.study, name="dec.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant90$dec.wt, df=immunosuppressant90, study=immunosuppressant.study, name="dec.immunosuppressant90")
outcome.model(Y="ICU.admission", immunosuppressant90$dec.wt, df=immunosuppressant90, study=immunosuppressant.study, name="dec.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant90$dec.wt, df=immunosuppressant90, study=immunosuppressant.study, name="dec.immunosuppressant90")

print_table(df.all=immunosuppressant90, wt=immunosuppressant90$dec.wt, df.study=immunosuppressant.study, save_name="dec.immunosuppressant90")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[ALL_Matching]results_immunosuppressant90.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = immunosuppressant90$ps[ind.trt0]
trt1.ps = immunosuppressant90$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
immunosuppressant90$match.wt = rep(0,nrow(immunosuppressant90))
immunosuppressant90$match.wt[ind.trt1] = 1
immunosuppressant90$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(immunosuppressant90$immunosuppressant, immunosuppressant90$ps, immunosuppressant90$match.wt, "match.immunosuppressant90")
outcome.model(Y="outcome1", immunosuppressant90$match.wt, df=immunosuppressant90, study=immunosuppressant.study, name="match.immunosuppressant90") # 1.6468

outcome.model(Y="Death", immunosuppressant90$match.wt, df=immunosuppressant90, study=immunosuppressant.study, name="match.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant90$match.wt, df=immunosuppressant90, study=immunosuppressant.study, name="match.immunosuppressant90")
outcome.model(Y="ICU.admission", immunosuppressant90$match.wt, df=immunosuppressant90, study=immunosuppressant.study, name="match.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant90$match.wt, df=immunosuppressant90, study=immunosuppressant.study, name="match.immunosuppressant90")

print_table(df.all=immunosuppressant90, wt=immunosuppressant90$match.wt, df.study=immunosuppressant.study, save_name="match.immunosuppressant90")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[ALL_Kernel]results_immunosuppressant90.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

immunosuppressant90$kernel.wt = rep(0,nrow(immunosuppressant90))
immunosuppressant90$kernel.wt[ind.trt0] = colSums(matchMat.k)
immunosuppressant90$kernel.wt[ind.trt1] = 1

ps_summary(immunosuppressant90$immunosuppressant, immunosuppressant90$ps, immunosuppressant90$kernel.wt, "kernel.immunosuppressant90")
outcome.model(Y="outcome1", immunosuppressant90$kernel.wt, df=immunosuppressant90, study=immunosuppressant.study, name="kernel.immunosuppressant90")

outcome.model(Y="Death", immunosuppressant90$kernel.wt, df=immunosuppressant90, study=immunosuppressant.study, name="kernel.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", immunosuppressant90$kernel.wt, df=immunosuppressant90, study=immunosuppressant.study, name="kernel.immunosuppressant90")
outcome.model(Y="ICU.admission", immunosuppressant90$kernel.wt, df=immunosuppressant90, study=immunosuppressant.study, name="kernel.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", immunosuppressant90$kernel.wt, df=immunosuppressant90, study=immunosuppressant.study, name="kernel.immunosuppressant90")

print_table(df.all=immunosuppressant90, wt=immunosuppressant90$kernel.wt, df.study=immunosuppressant.study, save_name="kernel.immunosuppressant90")

sink()

###############################################
############  Hos immunosuppressant 120    ###############
###############################################

hos.immunosuppressant120.con <- hos.immunosuppressant120[,immunosuppressant.confounder]
hos.immunosuppressant120.fit <- glm(hos.immunosuppressant120$immunosuppressant ~ ., data = hos.immunosuppressant120.con, family="binomial")

hos.immunosuppressant120$ps <- hos.immunosuppressant120.fit$fitted.values
ind.trt0 <- which(hos.immunosuppressant120$immunosuppressant == 0)
ind.trt1 <- which(hos.immunosuppressant120$immunosuppressant == 1)

hos.immunosuppressant120$iptw.wt <- ( 1/hos.immunosuppressant120$ps )*hos.immunosuppressant120$immunosuppressant + ( 1/(1-hos.immunosuppressant120$ps) )*( 1-hos.immunosuppressant120$immunosuppressant )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW]results_hos.immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.immunosuppressant120$immunosuppressant, hos.immunosuppressant120$ps, hos.immunosuppressant120$iptw.wt, "hos.immunosuppressant120")
outcome.model(Y="outcome1", wt=hos.immunosuppressant120$iptw.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="hos.immunosuppressant120") # 0.6249

outcome.model(Y="Death", wt=hos.immunosuppressant120$iptw.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.immunosuppressant120$iptw.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="hos.immunosuppressant120")
outcome.model(Y="ICU.admission", wt=hos.immunosuppressant120$iptw.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.immunosuppressant120$iptw.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="hos.immunosuppressant120")

print_table(hos.immunosuppressant120, wt=hos.immunosuppressant120$iptw.wt, df.study=immunosuppressant.study, save_name="hos.immunosuppressant120")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim]results_hos.immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.immunosuppressant120$trim.wt <- hos.immunosuppressant120$iptw.wt
hos.immunosuppressant120$trim.wt[(hos.immunosuppressant120$ps<ps.cut | hos.immunosuppressant120$ps>(1-ps.cut))] <- 0
print(nrow(hos.immunosuppressant120[hos.immunosuppressant120$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(hos.immunosuppressant120$immunosuppressant[hos.immunosuppressant120$trim.wt!=0], hos.immunosuppressant120$ps[hos.immunosuppressant120$trim.wt!=0], hos.immunosuppressant120$trim.wt[hos.immunosuppressant120$trim.wt!=0], "trim.hos.immunosuppressant120")
outcome.model(Y="outcome1", wt=hos.immunosuppressant120$trim.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="trim.hos.immunosuppressant120") # 0.6802

outcome.model(Y="Death", wt=hos.immunosuppressant120$trim.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="trim.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.immunosuppressant120$trim.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="trim.hos.immunosuppressant120")
outcome.model(Y="ICU.admission", wt=hos.immunosuppressant120$trim.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="trim.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.immunosuppressant120$trim.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="trim.hos.immunosuppressant120")

print_table(hos.immunosuppressant120, wt=hos.immunosuppressant120$trim.wt, df.study=immunosuppressant.study, save_name="trim.hos.immunosuppressant120")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized]results_hos.immunosuppressant120.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.immunosuppressant <- sum(hos.immunosuppressant120$immunosuppressant) / nrow(hos.immunosuppressant120)
# wt : ATE
hos.immunosuppressant120$st.iptw.wt <- ( p.immunosuppressant/hos.immunosuppressant120$ps )*hos.immunosuppressant120$immunosuppressant + ( (1-p.immunosuppressant)/(1-hos.immunosuppressant120$ps) )*( 1-hos.immunosuppressant120$immunosuppressant )

ps_summary(hos.immunosuppressant120$immunosuppressant, hos.immunosuppressant120$ps, hos.immunosuppressant120$st.iptw.wt, "stabilized.hos.immunosuppressant120")

outcome.model(Y="outcome1", hos.immunosuppressant120$st.iptw.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="stabilized.hos.immunosuppressant120") # -1.21

outcome.model(Y="Death", hos.immunosuppressant120$st.iptw.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="stabilized.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant120$st.iptw.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="stabilized.hos.immunosuppressant120")
outcome.model(Y="ICU.admission", hos.immunosuppressant120$st.iptw.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="stabilized.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant120$st.iptw.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="stabilized.hos.immunosuppressant120")

print_table(hos.immunosuppressant120, wt=hos.immunosuppressant120$st.iptw.wt, df.study=immunosuppressant.study, save_name="stabilized.hos.immunosuppressant120")

sink()

#### IPTW : ATT ####

hos.immunosuppressant120$iptw.att <- hos.immunosuppressant120$ps/(1-hos.immunosuppressant120$ps)
hos.immunosuppressant120$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[Hos_IPTW_ATT]results_hos.immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.immunosuppressant120$immunosuppressant, hos.immunosuppressant120$ps, hos.immunosuppressant120$iptw.att, "hos.immunosuppressant120.att")
outcome.model(Y="outcome1", hos.immunosuppressant120$iptw.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="hos.immunosuppressant120.att") # 0.6249

outcome.model(Y="Death", hos.immunosuppressant120$iptw.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="hos.immunosuppressant120.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant120$iptw.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="hos.immunosuppressant120.att")
outcome.model(Y="ICU.admission", hos.immunosuppressant120$iptw.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="hos.immunosuppressant120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant120$iptw.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="hos.immunosuppressant120.att")

print_table(hos.immunosuppressant120, hos.immunosuppressant120$iptw.att, df.study=immunosuppressant.study, save_name="hos.immunosuppressant120.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim_ATT]results_hos.immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.immunosuppressant120$trim.att <- hos.immunosuppressant120$iptw.att
hos.immunosuppressant120$trim.att[(hos.immunosuppressant120$ps<ps.cut | hos.immunosuppressant120$ps>(1-ps.cut))] <- 0
print(nrow(hos.immunosuppressant120[hos.immunosuppressant120$trim.att!=0,]))

ps_summary(hos.immunosuppressant120$immunosuppressant[hos.immunosuppressant120$trim.att!=0], hos.immunosuppressant120$ps[hos.immunosuppressant120$trim.att!=0], hos.immunosuppressant120$trim.att[hos.immunosuppressant120$trim.att!=0], "trim.hos.immunosuppressant120.att")
outcome.model(Y="outcome1", hos.immunosuppressant120$trim.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="trim.hos.immunosuppressant120.att") # 0.6802

outcome.model(Y="Death", hos.immunosuppressant120$trim.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="trim.hos.immunosuppressant120.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant120$trim.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="trim.hos.immunosuppressant120.att")
outcome.model(Y="ICU.admission", hos.immunosuppressant120$trim.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="trim.hos.immunosuppressant120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant120$trim.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="trim.hos.immunosuppressant120.att")

print_table(hos.immunosuppressant120, hos.immunosuppressant120$trim.att, df.study=immunosuppressant.study, save_name="trim.hos.immunosuppressant120.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized_ATT]results_hos.immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
hos.immunosuppressant120$st.iptw.att <- ( (1-p.immunosuppressant)/p.immunosuppressant) * (hos.immunosuppressant120$ps / (1-hos.immunosuppressant120$ps) )  * (1- hos.immunosuppressant120$immunosuppressant)
hos.immunosuppressant120$st.iptw.att[ind.trt1] <- 1
ps_summary(hos.immunosuppressant120$immunosuppressant, hos.immunosuppressant120$ps, hos.immunosuppressant120$st.iptw.att, "stabilized.att.hos.immunosuppressant120")

outcome.model(Y="outcome1", hos.immunosuppressant120$st.iptw.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="stabilized.att.hos.immunosuppressant120") # -1.21

outcome.model(Y="Death", hos.immunosuppressant120$st.iptw.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="stabilized.att.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant120$st.iptw.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="stabilized.att.hos.immunosuppressant120")
outcome.model(Y="ICU.admission", hos.immunosuppressant120$st.iptw.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="stabilized.att.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant120$st.iptw.att, df=hos.immunosuppressant120, study=immunosuppressant.study, name="stabilized.att.hos.immunosuppressant120")

print_table(hos.immunosuppressant120, wt=hos.immunosuppressant120$st.iptw.att, df.study=immunosuppressant.study, save_name="stabilized.att.hos.immunosuppressant120")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[Hos_Deciles]results_hos.immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

hos.immunosuppressant120$ps.dec <- cut(hos.immunosuppressant120$ps, 
                                       breaks=c(quantile(hos.immunosuppressant120$ps, probs=seq(0,1,0.1))),
                                       labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(hos.immunosuppressant120$ps.dec)); cat("\n")
hos.immunosuppressant120$dec_info <- paste0(hos.immunosuppressant120$immunosuppressant, ":", hos.immunosuppressant120$ps.dec)

cat("\n"); cat(table(hos.immunosuppressant120$dec_info)); cat("\n"); 
hos.immunosuppressant120.dec <- data.frame(table(hos.immunosuppressant120$ps.dec, hos.immunosuppressant120$immunosuppressant))
cat("\n"); print(hos.immunosuppressant120.dec); cat("\n"); 

colnames(hos.immunosuppressant120.dec) <- c("ps.dec", "immunosuppressant", "n")
hos.immunosuppressant120.dec$dec_info <- paste0(hos.immunosuppressant120.dec$immunosuppressant, ":", hos.immunosuppressant120.dec$ps.dec)
hos.immunosuppressant120.dec$n[hos.immunosuppressant120.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.immunosuppressant120.dec$dec.wt <- 1/hos.immunosuppressant120.dec$n

if (is.null(hos.immunosuppressant120$dec.wt)) {
  hos.immunosuppressant120 <- merge(hos.immunosuppressant120, hos.immunosuppressant120.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(hos.immunosuppressant120$immunosuppressant, hos.immunosuppressant120$ps, hos.immunosuppressant120$dec.wt, "dec.hos.immunosuppressant120")
outcome.model(Y="outcome1", hos.immunosuppressant120$dec.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="dec.hos.immunosuppressant120") # -0.03765

outcome.model(Y="Death", hos.immunosuppressant120$dec.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="dec.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant120$dec.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="dec.hos.immunosuppressant120")
outcome.model(Y="ICU.admission", hos.immunosuppressant120$dec.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="dec.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant120$dec.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="dec.hos.immunosuppressant120")

print_table(hos.immunosuppressant120, wt=hos.immunosuppressant120$dec.wt, df.study=immunosuppressant.study, save_name="dec.hos.immunosuppressant120")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[Hos_Matching]results_hos.immunosuppressant120.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = hos.immunosuppressant120$ps[ind.trt0]
trt1.ps = hos.immunosuppressant120$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.immunosuppressant120$match.wt = rep(0,nrow(hos.immunosuppressant120))
hos.immunosuppressant120$match.wt[ind.trt1] = 1
hos.immunosuppressant120$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(hos.immunosuppressant120$immunosuppressant, hos.immunosuppressant120$ps, hos.immunosuppressant120$match.wt, "match.hos.immunosuppressant120")
outcome.model(Y="outcome1", hos.immunosuppressant120$match.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="match.hos.immunosuppressant120") # 1.6468

outcome.model(Y="Death", hos.immunosuppressant120$match.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="match.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant120$match.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="match.hos.immunosuppressant120")
outcome.model(Y="ICU.admission", hos.immunosuppressant120$match.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="match.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant120$match.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="match.hos.immunosuppressant120")

print_table(hos.immunosuppressant120, wt=hos.immunosuppressant120$match.wt, df.study=immunosuppressant.study, save_name="match.hos.immunosuppressant120")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[Hos_Kernel]results_hos.immunosuppressant120.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

hos.immunosuppressant120$kernel.wt = rep(0,nrow(hos.immunosuppressant120))
hos.immunosuppressant120$kernel.wt[ind.trt0] = colSums(matchMat.k)
hos.immunosuppressant120$kernel.wt[ind.trt1] = 1

ps_summary(hos.immunosuppressant120$immunosuppressant, hos.immunosuppressant120$ps, hos.immunosuppressant120$kernel.wt, "kernel.hos.immunosuppressant120")
outcome.model(Y="outcome1", hos.immunosuppressant120$kernel.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="kernel.hos.immunosuppressant120")

outcome.model(Y="Death", hos.immunosuppressant120$kernel.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="kernel.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant120$kernel.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="kernel.hos.immunosuppressant120")
outcome.model(Y="ICU.admission", hos.immunosuppressant120$kernel.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="kernel.hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant120$kernel.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="kernel.hos.immunosuppressant120")

print_table(hos.immunosuppressant120, wt=hos.immunosuppressant120$kernel.wt, df.study=immunosuppressant.study, save_name="kernel.hos.immunosuppressant120")

sink()


###############################################
############  Hos immunosuppressant 90    ###############
###############################################

hos.immunosuppressant90.con <- hos.immunosuppressant90[,immunosuppressant.confounder]
hos.immunosuppressant90.fit <- glm(hos.immunosuppressant90$immunosuppressant ~ ., data = hos.immunosuppressant90.con, family="binomial")

hos.immunosuppressant90$ps <- hos.immunosuppressant90.fit$fitted.values
ind.trt0 <- which(hos.immunosuppressant90$immunosuppressant == 0)
ind.trt1 <- which(hos.immunosuppressant90$immunosuppressant == 1)

hos.immunosuppressant90$iptw.wt <- ( 1/hos.immunosuppressant90$ps )*hos.immunosuppressant90$immunosuppressant + ( 1/(1-hos.immunosuppressant90$ps) )*( 1-hos.immunosuppressant90$immunosuppressant )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW]results_hos.immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.immunosuppressant90$immunosuppressant, hos.immunosuppressant90$ps, hos.immunosuppressant90$iptw.wt, "hos.immunosuppressant90")
outcome.model(Y="outcome1", wt=hos.immunosuppressant90$iptw.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="hos.immunosuppressant90") # 0.6249

outcome.model(Y="Death", wt=hos.immunosuppressant90$iptw.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.immunosuppressant90$iptw.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="hos.immunosuppressant90")
outcome.model(Y="ICU.admission", wt=hos.immunosuppressant90$iptw.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.immunosuppressant90$iptw.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="hos.immunosuppressant90")

print_table(hos.immunosuppressant90, wt=hos.immunosuppressant90$iptw.wt, df.study=immunosuppressant.study, save_name="hos.immunosuppressant90")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim]results_hos.immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.immunosuppressant90$trim.wt <- hos.immunosuppressant90$iptw.wt
hos.immunosuppressant90$trim.wt[(hos.immunosuppressant90$ps<ps.cut | hos.immunosuppressant90$ps>(1-ps.cut))] <- 0
print(nrow(hos.immunosuppressant90[hos.immunosuppressant90$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(hos.immunosuppressant90$immunosuppressant[hos.immunosuppressant90$trim.wt!=0], hos.immunosuppressant90$ps[hos.immunosuppressant90$trim.wt!=0], hos.immunosuppressant90$trim.wt[hos.immunosuppressant90$trim.wt!=0], "trim.hos.immunosuppressant90")
outcome.model(Y="outcome1", wt=hos.immunosuppressant90$trim.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="trim.hos.immunosuppressant90") # 0.6802

outcome.model(Y="Death", wt=hos.immunosuppressant90$trim.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="trim.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.immunosuppressant90$trim.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="trim.hos.immunosuppressant90")
outcome.model(Y="ICU.admission", wt=hos.immunosuppressant90$trim.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="trim.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.immunosuppressant90$trim.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="trim.hos.immunosuppressant90")

print_table(hos.immunosuppressant90, wt=hos.immunosuppressant90$trim.wt, df.study=immunosuppressant.study, save_name="trim.hos.immunosuppressant90")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized]results_hos.immunosuppressant90.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.immunosuppressant <- sum(hos.immunosuppressant90$immunosuppressant) / nrow(hos.immunosuppressant90)
# wt : ATE
hos.immunosuppressant90$st.iptw.wt <- ( p.immunosuppressant/hos.immunosuppressant90$ps )*hos.immunosuppressant90$immunosuppressant + ( (1-p.immunosuppressant)/(1-hos.immunosuppressant90$ps) )*( 1-hos.immunosuppressant90$immunosuppressant )

ps_summary(hos.immunosuppressant90$immunosuppressant, hos.immunosuppressant90$ps, hos.immunosuppressant90$st.iptw.wt, "stabilized.hos.immunosuppressant90")

outcome.model(Y="outcome1", hos.immunosuppressant90$st.iptw.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="stabilized.hos.immunosuppressant90") # -1.21

outcome.model(Y="Death", hos.immunosuppressant90$st.iptw.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="stabilized.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant90$st.iptw.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="stabilized.hos.immunosuppressant90")
outcome.model(Y="ICU.admission", hos.immunosuppressant90$st.iptw.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="stabilized.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant90$st.iptw.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="stabilized.hos.immunosuppressant90")

print_table(hos.immunosuppressant90, wt=hos.immunosuppressant90$st.iptw.wt, df.study=immunosuppressant.study, save_name="stabilized.hos.immunosuppressant90")

sink()

#### IPTW : ATT ####

hos.immunosuppressant90$iptw.att <- hos.immunosuppressant90$ps/(1-hos.immunosuppressant90$ps)
hos.immunosuppressant90$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[Hos_IPTW_ATT]results_hos.immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.immunosuppressant90$immunosuppressant, hos.immunosuppressant90$ps, hos.immunosuppressant90$iptw.att, "hos.immunosuppressant90.att")
outcome.model(Y="outcome1", hos.immunosuppressant90$iptw.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="hos.immunosuppressant90.att") # 0.6249

outcome.model(Y="Death", hos.immunosuppressant90$iptw.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="hos.immunosuppressant90.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant90$iptw.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="hos.immunosuppressant90.att")
outcome.model(Y="ICU.admission", hos.immunosuppressant90$iptw.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="hos.immunosuppressant90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant90$iptw.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="hos.immunosuppressant90.att")

print_table(hos.immunosuppressant90, hos.immunosuppressant90$iptw.att, df.study=immunosuppressant.study, save_name="hos.immunosuppressant90.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim_ATT]results_hos.immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.immunosuppressant90$trim.att <- hos.immunosuppressant90$iptw.att
hos.immunosuppressant90$trim.att[(hos.immunosuppressant90$ps<ps.cut | hos.immunosuppressant90$ps>(1-ps.cut))] <- 0
print(nrow(hos.immunosuppressant90[hos.immunosuppressant90$trim.att!=0,]))

ps_summary(hos.immunosuppressant90$immunosuppressant[hos.immunosuppressant90$trim.att!=0], hos.immunosuppressant90$ps[hos.immunosuppressant90$trim.att!=0], hos.immunosuppressant90$trim.att[hos.immunosuppressant90$trim.att!=0], "trim.hos.immunosuppressant90.att")
outcome.model(Y="outcome1", hos.immunosuppressant90$trim.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="trim.hos.immunosuppressant90.att") # 0.6802

outcome.model(Y="Death", hos.immunosuppressant90$trim.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="trim.hos.immunosuppressant90.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant90$trim.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="trim.hos.immunosuppressant90.att")
outcome.model(Y="ICU.admission", hos.immunosuppressant90$trim.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="trim.hos.immunosuppressant90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant90$trim.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="trim.hos.immunosuppressant90.att")

print_table(hos.immunosuppressant90, hos.immunosuppressant90$trim.att, df.study=immunosuppressant.study, save_name="trim.hos.immunosuppressant90.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized_ATT]results_hos.immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
hos.immunosuppressant90$st.iptw.att <- ( (1-p.immunosuppressant)/p.immunosuppressant) * (hos.immunosuppressant90$ps / (1-hos.immunosuppressant90$ps) )  *  (1-hos.immunosuppressant90$immunosuppressant)
hos.immunosuppressant90$st.iptw.att[ind.trt1] <- 1
ps_summary(hos.immunosuppressant90$immunosuppressant, hos.immunosuppressant90$ps, hos.immunosuppressant90$st.iptw.att, "stabilized.att.hos.immunosuppressant90")

outcome.model(Y="outcome1", hos.immunosuppressant90$st.iptw.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="stabilized.att.hos.immunosuppressant90") # -1.21

outcome.model(Y="Death", hos.immunosuppressant90$st.iptw.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="stabilized.att.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant90$st.iptw.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="stabilized.att.hos.immunosuppressant90")
outcome.model(Y="ICU.admission", hos.immunosuppressant90$st.iptw.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="stabilized.att.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant90$st.iptw.att, df=hos.immunosuppressant90, study=immunosuppressant.study, name="stabilized.att.hos.immunosuppressant90")

print_table(hos.immunosuppressant90, wt=hos.immunosuppressant90$st.iptw.att, df.study=immunosuppressant.study, save_name="stabilized.att.hos.immunosuppressant90")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[Hos_Deciles]results_hos.immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 90] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

hos.immunosuppressant90$ps.dec <- cut(hos.immunosuppressant90$ps, 
                                      breaks=c(quantile(hos.immunosuppressant90$ps, probs=seq(0,1,0.1))),
                                      labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(hos.immunosuppressant90$ps.dec)); cat("\n")
hos.immunosuppressant90$dec_info <- paste0(hos.immunosuppressant90$immunosuppressant, ":", hos.immunosuppressant90$ps.dec)

cat("\n"); cat(table(hos.immunosuppressant90$dec_info)); cat("\n"); 
hos.immunosuppressant90.dec <- data.frame(table(hos.immunosuppressant90$ps.dec, hos.immunosuppressant90$immunosuppressant))
cat("\n"); print(hos.immunosuppressant90.dec); cat("\n"); 

colnames(hos.immunosuppressant90.dec) <- c("ps.dec", "immunosuppressant", "n")
hos.immunosuppressant90.dec$dec_info <- paste0(hos.immunosuppressant90.dec$immunosuppressant, ":", hos.immunosuppressant90.dec$ps.dec)
hos.immunosuppressant90.dec$n[hos.immunosuppressant90.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.immunosuppressant90.dec$dec.wt <- 1/hos.immunosuppressant90.dec$n

if (is.null(hos.immunosuppressant90$dec.wt)) {
  hos.immunosuppressant90 <- merge(hos.immunosuppressant90, hos.immunosuppressant90.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(hos.immunosuppressant90$immunosuppressant, hos.immunosuppressant90$ps, hos.immunosuppressant90$dec.wt, "dec.hos.immunosuppressant90")
outcome.model(Y="outcome1", hos.immunosuppressant90$dec.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="dec.hos.immunosuppressant90") # -0.03765

outcome.model(Y="Death", hos.immunosuppressant90$dec.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="dec.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant90$dec.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="dec.hos.immunosuppressant90")
outcome.model(Y="ICU.admission", hos.immunosuppressant90$dec.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="dec.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant90$dec.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="dec.hos.immunosuppressant90")

print_table(hos.immunosuppressant90, wt=hos.immunosuppressant90$dec.wt, df.study=immunosuppressant.study, save_name="dec.hos.immunosuppressant90")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[Hos_Matching]results_hos.immunosuppressant90.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = hos.immunosuppressant90$ps[ind.trt0]
trt1.ps = hos.immunosuppressant90$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.immunosuppressant90$match.wt = rep(0,nrow(hos.immunosuppressant90))
hos.immunosuppressant90$match.wt[ind.trt1] = 1
hos.immunosuppressant90$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(hos.immunosuppressant90$immunosuppressant, hos.immunosuppressant90$ps, hos.immunosuppressant90$match.wt, "match.hos.immunosuppressant90")
outcome.model(Y="outcome1", hos.immunosuppressant90$match.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="match.hos.immunosuppressant90") # 1.6468

outcome.model(Y="Death", hos.immunosuppressant90$match.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="match.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant90$match.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="match.hos.immunosuppressant90")
outcome.model(Y="ICU.admission", hos.immunosuppressant90$match.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="match.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant90$match.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="match.hos.immunosuppressant90")

print_table(hos.immunosuppressant90, wt=hos.immunosuppressant90$match.wt, df.study=immunosuppressant.study, save_name="match.hos.immunosuppressant90")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[Hos_Kernel]results_hos.immunosuppressant90.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

hos.immunosuppressant90$kernel.wt = rep(0,nrow(hos.immunosuppressant90))
hos.immunosuppressant90$kernel.wt[ind.trt0] = colSums(matchMat.k)
hos.immunosuppressant90$kernel.wt[ind.trt1] = 1

ps_summary(hos.immunosuppressant90$immunosuppressant, hos.immunosuppressant90$ps, hos.immunosuppressant90$kernel.wt, "kernel.hos.immunosuppressant90")
outcome.model(Y="outcome1", hos.immunosuppressant90$kernel.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="kernel.hos.immunosuppressant90")

outcome.model(Y="Death", hos.immunosuppressant90$kernel.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="kernel.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.immunosuppressant90$kernel.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="kernel.hos.immunosuppressant90")
outcome.model(Y="ICU.admission", hos.immunosuppressant90$kernel.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="kernel.hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.immunosuppressant90$kernel.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="kernel.hos.immunosuppressant90")

print_table(hos.immunosuppressant90, wt=hos.immunosuppressant90$kernel.wt, df.study=immunosuppressant.study, save_name="kernel.hos.immunosuppressant90")

sink()


###############################################
############  All Glucocorticoid 120    ###############
###############################################

Glucocorticoid120.con <- Glucocorticoid120[,Glucocorticoid.confounder]
Glucocorticoid120.fit <- glm(Glucocorticoid120$Glucocorticoid ~ ., data = Glucocorticoid120.con, family="binomial")

Glucocorticoid120$ps <- Glucocorticoid120.fit$fitted.values
ind.trt0 <- which(Glucocorticoid120$Glucocorticoid == 0)
ind.trt1 <- which(Glucocorticoid120$Glucocorticoid == 1)

Glucocorticoid120$iptw.wt <- ( 1/Glucocorticoid120$ps )*Glucocorticoid120$Glucocorticoid + ( 1/(1-Glucocorticoid120$ps) )*( 1-Glucocorticoid120$Glucocorticoid )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW]results_Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(Glucocorticoid120$Glucocorticoid, Glucocorticoid120$ps, Glucocorticoid120$iptw.wt, "Glucocorticoid120")
outcome.model(Y="outcome1", wt=Glucocorticoid120$iptw.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="Glucocorticoid120") # 0.6249

outcome.model(Y="Death", wt=Glucocorticoid120$iptw.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=Glucocorticoid120$iptw.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="Glucocorticoid120")
outcome.model(Y="ICU.admission", wt=Glucocorticoid120$iptw.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=Glucocorticoid120$iptw.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="Glucocorticoid120")

print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120$iptw.wt, df.study=Glucocorticoid.study, save_name="Glucocorticoid120")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim]results_Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

Glucocorticoid120$trim.wt <- Glucocorticoid120$iptw.wt
Glucocorticoid120$trim.wt[(Glucocorticoid120$ps<ps.cut | Glucocorticoid120$ps>(1-ps.cut))] <- 0
print(nrow(Glucocorticoid120[Glucocorticoid120$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(Glucocorticoid120$Glucocorticoid[Glucocorticoid120$trim.wt!=0], Glucocorticoid120$ps[Glucocorticoid120$trim.wt!=0], Glucocorticoid120$trim.wt[Glucocorticoid120$trim.wt!=0], "trim.Glucocorticoid120")
outcome.model(Y="outcome1", wt=Glucocorticoid120$trim.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="trim.Glucocorticoid120") # 0.6802

outcome.model(Y="Death", wt=Glucocorticoid120$trim.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="trim.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=Glucocorticoid120$trim.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="trim.Glucocorticoid120")
outcome.model(Y="ICU.admission", wt=Glucocorticoid120$trim.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="trim.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=Glucocorticoid120$trim.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="trim.Glucocorticoid120")

print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120$trim.wt, df.study=Glucocorticoid.study, save_name="trim.Glucocorticoid120")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized]results_Glucocorticoid120.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.Glucocorticoid <- sum(Glucocorticoid120$Glucocorticoid) / nrow(Glucocorticoid120)
# wt : ATE
Glucocorticoid120$st.iptw.wt <- ( p.Glucocorticoid/Glucocorticoid120$ps )*Glucocorticoid120$Glucocorticoid + ( (1-p.Glucocorticoid)/(1-Glucocorticoid120$ps) )*( 1-Glucocorticoid120$Glucocorticoid )

ps_summary(Glucocorticoid120$Glucocorticoid, Glucocorticoid120$ps, Glucocorticoid120$st.iptw.wt, "stabilized.Glucocorticoid120")

outcome.model(Y="outcome1", Glucocorticoid120$st.iptw.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.Glucocorticoid120") # -1.21

outcome.model(Y="Death", Glucocorticoid120$st.iptw.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid120$st.iptw.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.Glucocorticoid120")
outcome.model(Y="ICU.admission", Glucocorticoid120$st.iptw.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid120$st.iptw.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.Glucocorticoid120")

print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120$st.iptw.wt, df.study=Glucocorticoid.study, save_name="stabilized.Glucocorticoid120")

sink()

#### IPTW : ATT ####

Glucocorticoid120$iptw.att <- Glucocorticoid120$ps/(1-Glucocorticoid120$ps)
Glucocorticoid120$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[ALL_IPTW_ATT]results_Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(Glucocorticoid120$Glucocorticoid, Glucocorticoid120$ps, Glucocorticoid120$iptw.att, "Glucocorticoid120.att")
outcome.model(Y="outcome1", Glucocorticoid120$iptw.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="Glucocorticoid120.att") # 0.6249

outcome.model(Y="Death", Glucocorticoid120$iptw.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="Glucocorticoid120.att")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid120$iptw.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="Glucocorticoid120.att")
outcome.model(Y="ICU.admission", Glucocorticoid120$iptw.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="Glucocorticoid120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid120$iptw.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="Glucocorticoid120.att")

print_table(df.all=Glucocorticoid120, Glucocorticoid120$iptw.att, df.study=Glucocorticoid.study, save_name="Glucocorticoid120.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim_ATT]results_Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

Glucocorticoid120$trim.att <- Glucocorticoid120$iptw.att
Glucocorticoid120$trim.att[(Glucocorticoid120$ps<ps.cut | Glucocorticoid120$ps>(1-ps.cut))] <- 0
print(nrow(Glucocorticoid120[Glucocorticoid120$trim.att!=0,]))

ps_summary(Glucocorticoid120$Glucocorticoid[Glucocorticoid120$trim.att!=0], Glucocorticoid120$ps[Glucocorticoid120$trim.att!=0], Glucocorticoid120$trim.att[Glucocorticoid120$trim.att!=0], "trim.Glucocorticoid120.att")
outcome.model(Y="outcome1", Glucocorticoid120$trim.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="trim.Glucocorticoid120.att") # 0.6802

outcome.model(Y="Death", Glucocorticoid120$trim.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="trim.Glucocorticoid120.att")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid120$trim.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="trim.Glucocorticoid120.att")
outcome.model(Y="ICU.admission", Glucocorticoid120$trim.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="trim.Glucocorticoid120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid120$trim.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="trim.Glucocorticoid120.att")

print_table(df.all=Glucocorticoid120, Glucocorticoid120$trim.att, df.study=Glucocorticoid.study, save_name="trim.Glucocorticoid120.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized_ATT]results_Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
Glucocorticoid120$st.iptw.att <- ( (1-p.Glucocorticoid)/p.Glucocorticoid) * (Glucocorticoid120$ps / (1-Glucocorticoid120$ps) )  *  (1-Glucocorticoid120$Glucocorticoid)
Glucocorticoid120$st.iptw.att[ind.trt1] <- 1
ps_summary(Glucocorticoid120$Glucocorticoid, Glucocorticoid120$ps, Glucocorticoid120$st.iptw.att, "stabilized.att.Glucocorticoid120")

outcome.model(Y="outcome1", Glucocorticoid120$st.iptw.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.att.Glucocorticoid120") # -1.21

outcome.model(Y="Death", Glucocorticoid120$st.iptw.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.att.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid120$st.iptw.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.att.Glucocorticoid120")
outcome.model(Y="ICU.admission", Glucocorticoid120$st.iptw.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.att.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid120$st.iptw.att, df=Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.att.Glucocorticoid120")

print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120$st.iptw.att, df.study=Glucocorticoid.study, save_name="stabilized.att.Glucocorticoid120")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[ALL_Deciles]results_Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

Glucocorticoid120$ps.dec <- cut(Glucocorticoid120$ps, 
                                breaks=c(quantile(Glucocorticoid120$ps, probs=seq(0,1,0.1))),
                                labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(Glucocorticoid120$ps.dec)); cat("\n")
Glucocorticoid120$dec_info <- paste0(Glucocorticoid120$Glucocorticoid, ":", Glucocorticoid120$ps.dec)

cat("\n"); cat(table(Glucocorticoid120$dec_info)); cat("\n"); 
Glucocorticoid120.dec <- data.frame(table(Glucocorticoid120$ps.dec, Glucocorticoid120$Glucocorticoid))
cat("\n"); print(Glucocorticoid120.dec); cat("\n"); 

colnames(Glucocorticoid120.dec) <- c("ps.dec", "Glucocorticoid", "n")
Glucocorticoid120.dec$dec_info <- paste0(Glucocorticoid120.dec$Glucocorticoid, ":", Glucocorticoid120.dec$ps.dec)
Glucocorticoid120.dec$n[Glucocorticoid120.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
Glucocorticoid120.dec$dec.wt <- 1/Glucocorticoid120.dec$n

if (is.null(Glucocorticoid120$dec.wt)) {
  Glucocorticoid120 <- merge(Glucocorticoid120, Glucocorticoid120.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(Glucocorticoid120$Glucocorticoid, Glucocorticoid120$ps, Glucocorticoid120$dec.wt, "dec.Glucocorticoid120")
outcome.model(Y="outcome1", Glucocorticoid120$dec.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="dec.Glucocorticoid120") # -0.03765

outcome.model(Y="Death", Glucocorticoid120$dec.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="dec.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid120$dec.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="dec.Glucocorticoid120")
outcome.model(Y="ICU.admission", Glucocorticoid120$dec.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="dec.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid120$dec.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="dec.Glucocorticoid120")

print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120$dec.wt, df.study=Glucocorticoid.study, save_name="dec.Glucocorticoid120")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[ALL_Matching]results_Glucocorticoid120.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = Glucocorticoid120$ps[ind.trt0]
trt1.ps = Glucocorticoid120$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
Glucocorticoid120$match.wt = rep(0,nrow(Glucocorticoid120))
Glucocorticoid120$match.wt[ind.trt1] = 1
Glucocorticoid120$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(Glucocorticoid120$Glucocorticoid, Glucocorticoid120$ps, Glucocorticoid120$match.wt, "match.Glucocorticoid120")
outcome.model(Y="outcome1", Glucocorticoid120$match.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="match.Glucocorticoid120") # 1.6468

outcome.model(Y="Death", Glucocorticoid120$match.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="match.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid120$match.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="match.Glucocorticoid120")
outcome.model(Y="ICU.admission", Glucocorticoid120$match.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="match.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid120$match.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="match.Glucocorticoid120")

print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120$match.wt, df.study=Glucocorticoid.study, save_name="match.Glucocorticoid120")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[ALL_Kernel]results_Glucocorticoid120.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

Glucocorticoid120$kernel.wt = rep(0,nrow(Glucocorticoid120))
Glucocorticoid120$kernel.wt[ind.trt0] = colSums(matchMat.k)
Glucocorticoid120$kernel.wt[ind.trt1] = 1

ps_summary(Glucocorticoid120$Glucocorticoid, Glucocorticoid120$ps, Glucocorticoid120$kernel.wt, "kernel.Glucocorticoid120")
outcome.model(Y="outcome1", Glucocorticoid120$kernel.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="kernel.Glucocorticoid120")

outcome.model(Y="Death", Glucocorticoid120$kernel.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="kernel.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid120$kernel.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="kernel.Glucocorticoid120")
outcome.model(Y="ICU.admission", Glucocorticoid120$kernel.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="kernel.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid120$kernel.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="kernel.Glucocorticoid120")

print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120$kernel.wt, df.study=Glucocorticoid.study, save_name="kernel.Glucocorticoid120")

sink()


###############################################
############  All Glucocorticoid 90    ###############
###############################################

Glucocorticoid90.con <- Glucocorticoid90[,Glucocorticoid.confounder]
Glucocorticoid90.fit <- glm(Glucocorticoid90$Glucocorticoid ~ ., data = Glucocorticoid90.con, family="binomial")

Glucocorticoid90$ps <- Glucocorticoid90.fit$fitted.values
ind.trt0 <- which(Glucocorticoid90$Glucocorticoid == 0)
ind.trt1 <- which(Glucocorticoid90$Glucocorticoid == 1)

Glucocorticoid90$iptw.wt <- ( 1/Glucocorticoid90$ps )*Glucocorticoid90$Glucocorticoid + ( 1/(1-Glucocorticoid90$ps) )*( 1-Glucocorticoid90$Glucocorticoid )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW]results_Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(Glucocorticoid90$Glucocorticoid, Glucocorticoid90$ps, Glucocorticoid90$iptw.wt, "Glucocorticoid90")
outcome.model(Y="outcome1", wt=Glucocorticoid90$iptw.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="Glucocorticoid90") # 0.6249

outcome.model(Y="Death", wt=Glucocorticoid90$iptw.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=Glucocorticoid90$iptw.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="Glucocorticoid90")
outcome.model(Y="ICU.admission", wt=Glucocorticoid90$iptw.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=Glucocorticoid90$iptw.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="Glucocorticoid90")

print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90$iptw.wt, df.study=Glucocorticoid.study, save_name="Glucocorticoid90")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim]results_Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

Glucocorticoid90$trim.wt <- Glucocorticoid90$iptw.wt
Glucocorticoid90$trim.wt[(Glucocorticoid90$ps<ps.cut | Glucocorticoid90$ps>(1-ps.cut))] <- 0
print(nrow(Glucocorticoid90[Glucocorticoid90$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(Glucocorticoid90$Glucocorticoid[Glucocorticoid90$trim.wt!=0], Glucocorticoid90$ps[Glucocorticoid90$trim.wt!=0], Glucocorticoid90$trim.wt[Glucocorticoid90$trim.wt!=0], "trim.Glucocorticoid90")
outcome.model(Y="outcome1", wt=Glucocorticoid90$trim.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="trim.Glucocorticoid90") # 0.6802

outcome.model(Y="Death", wt=Glucocorticoid90$trim.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="trim.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=Glucocorticoid90$trim.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="trim.Glucocorticoid90")
outcome.model(Y="ICU.admission", wt=Glucocorticoid90$trim.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="trim.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=Glucocorticoid90$trim.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="trim.Glucocorticoid90")

print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90$trim.wt, df.study=Glucocorticoid.study, save_name="trim.Glucocorticoid90")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized]results_Glucocorticoid90.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.Glucocorticoid <- sum(Glucocorticoid90$Glucocorticoid) / nrow(Glucocorticoid90)
# wt : ATE
Glucocorticoid90$st.iptw.wt <- ( p.Glucocorticoid/Glucocorticoid90$ps )*Glucocorticoid90$Glucocorticoid + ( (1-p.Glucocorticoid)/(1-Glucocorticoid90$ps) )*( 1-Glucocorticoid90$Glucocorticoid )

ps_summary(Glucocorticoid90$Glucocorticoid, Glucocorticoid90$ps, Glucocorticoid90$st.iptw.wt, "stabilized.Glucocorticoid90")

outcome.model(Y="outcome1", Glucocorticoid90$st.iptw.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.Glucocorticoid90") # -1.21

outcome.model(Y="Death", Glucocorticoid90$st.iptw.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid90$st.iptw.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.Glucocorticoid90")
outcome.model(Y="ICU.admission", Glucocorticoid90$st.iptw.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid90$st.iptw.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.Glucocorticoid90")

print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90$st.iptw.wt, df.study=Glucocorticoid.study, save_name="stabilized.Glucocorticoid90")

sink()

#### IPTW : ATT ####

Glucocorticoid90$iptw.att <- Glucocorticoid90$ps/(1-Glucocorticoid90$ps)
Glucocorticoid90$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[ALL_IPTW_ATT]results_Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(Glucocorticoid90$Glucocorticoid, Glucocorticoid90$ps, Glucocorticoid90$iptw.att, "Glucocorticoid90.att")
outcome.model(Y="outcome1", Glucocorticoid90$iptw.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="Glucocorticoid90.att") # 0.6249

outcome.model(Y="Death", Glucocorticoid90$iptw.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="Glucocorticoid90.att")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid90$iptw.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="Glucocorticoid90.att")
outcome.model(Y="ICU.admission", Glucocorticoid90$iptw.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="Glucocorticoid90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid90$iptw.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="Glucocorticoid90.att")

print_table(df.all=Glucocorticoid90, Glucocorticoid90$iptw.att, df.study=Glucocorticoid.study, save_name="Glucocorticoid90.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim_ATT]results_Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

Glucocorticoid90$trim.att <- Glucocorticoid90$iptw.att
Glucocorticoid90$trim.att[(Glucocorticoid90$ps<ps.cut | Glucocorticoid90$ps>(1-ps.cut))] <- 0
print(nrow(Glucocorticoid90[Glucocorticoid90$trim.att!=0,]))

ps_summary(Glucocorticoid90$Glucocorticoid[Glucocorticoid90$trim.att!=0], Glucocorticoid90$ps[Glucocorticoid90$trim.att!=0], Glucocorticoid90$trim.att[Glucocorticoid90$trim.att!=0], "trim.Glucocorticoid90.att")
outcome.model(Y="outcome1", Glucocorticoid90$trim.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="trim.Glucocorticoid90.att") # 0.6802

outcome.model(Y="Death", Glucocorticoid90$trim.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="trim.Glucocorticoid90.att")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid90$trim.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="trim.Glucocorticoid90.att")
outcome.model(Y="ICU.admission", Glucocorticoid90$trim.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="trim.Glucocorticoid90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid90$trim.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="trim.Glucocorticoid90.att")

print_table(df.all=Glucocorticoid90, Glucocorticoid90$trim.att, df.study=Glucocorticoid.study, save_name="trim.Glucocorticoid90.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized_ATT]results_Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
Glucocorticoid90$st.iptw.att <- ( (1-p.Glucocorticoid)/p.Glucocorticoid) * (Glucocorticoid90$ps / (1-Glucocorticoid90$ps) )  *  (1-Glucocorticoid90$Glucocorticoid)
Glucocorticoid90$st.iptw.att[ind.trt1] <- 1
ps_summary(Glucocorticoid90$Glucocorticoid, Glucocorticoid90$ps, Glucocorticoid90$st.iptw.att, "stabilized.att.Glucocorticoid90")

outcome.model(Y="outcome1", Glucocorticoid90$st.iptw.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.att.Glucocorticoid90") # -1.21

outcome.model(Y="Death", Glucocorticoid90$st.iptw.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.att.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid90$st.iptw.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.att.Glucocorticoid90")
outcome.model(Y="ICU.admission", Glucocorticoid90$st.iptw.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.att.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid90$st.iptw.att, df=Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.att.Glucocorticoid90")

print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90$st.iptw.att, df.study=Glucocorticoid.study, save_name="stabilized.att.Glucocorticoid90")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[ALL_Deciles]results_Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

Glucocorticoid90$ps.dec <- cut(Glucocorticoid90$ps, 
                               breaks=c(quantile(Glucocorticoid90$ps, probs=seq(0,1,0.1))),
                               labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(Glucocorticoid90$ps.dec)); cat("\n")
Glucocorticoid90$dec_info <- paste0(Glucocorticoid90$Glucocorticoid, ":", Glucocorticoid90$ps.dec)

cat("\n"); cat(table(Glucocorticoid90$dec_info)); cat("\n"); 
Glucocorticoid90.dec <- data.frame(table(Glucocorticoid90$ps.dec, Glucocorticoid90$Glucocorticoid))
cat("\n"); print(Glucocorticoid90.dec); cat("\n"); 

colnames(Glucocorticoid90.dec) <- c("ps.dec", "Glucocorticoid", "n")
Glucocorticoid90.dec$dec_info <- paste0(Glucocorticoid90.dec$Glucocorticoid, ":", Glucocorticoid90.dec$ps.dec)
Glucocorticoid90.dec$n[Glucocorticoid90.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
Glucocorticoid90.dec$dec.wt <- 1/Glucocorticoid90.dec$n

if (is.null(Glucocorticoid90$dec.wt)) {
  Glucocorticoid90 <- merge(Glucocorticoid90, Glucocorticoid90.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(Glucocorticoid90$Glucocorticoid, Glucocorticoid90$ps, Glucocorticoid90$dec.wt, "dec.Glucocorticoid90")
outcome.model(Y="outcome1", Glucocorticoid90$dec.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="dec.Glucocorticoid90") # -0.03765

outcome.model(Y="Death", Glucocorticoid90$dec.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="dec.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid90$dec.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="dec.Glucocorticoid90")
outcome.model(Y="ICU.admission", Glucocorticoid90$dec.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="dec.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid90$dec.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="dec.Glucocorticoid90")

print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90$dec.wt, df.study=Glucocorticoid.study, save_name="dec.Glucocorticoid90")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[ALL_Matching]results_Glucocorticoid90.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = Glucocorticoid90$ps[ind.trt0]
trt1.ps = Glucocorticoid90$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
Glucocorticoid90$match.wt = rep(0,nrow(Glucocorticoid90))
Glucocorticoid90$match.wt[ind.trt1] = 1
Glucocorticoid90$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(Glucocorticoid90$Glucocorticoid, Glucocorticoid90$ps, Glucocorticoid90$match.wt, "match.Glucocorticoid90")
outcome.model(Y="outcome1", Glucocorticoid90$match.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="match.Glucocorticoid90") # 1.6468

outcome.model(Y="Death", Glucocorticoid90$match.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="match.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid90$match.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="match.Glucocorticoid90")
outcome.model(Y="ICU.admission", Glucocorticoid90$match.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="match.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid90$match.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="match.Glucocorticoid90")

print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90$match.wt, df.study=Glucocorticoid.study, save_name="match.Glucocorticoid90")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[ALL_Kernel]results_Glucocorticoid90.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

Glucocorticoid90$kernel.wt = rep(0,nrow(Glucocorticoid90))
Glucocorticoid90$kernel.wt[ind.trt0] = colSums(matchMat.k)
Glucocorticoid90$kernel.wt[ind.trt1] = 1

ps_summary(Glucocorticoid90$Glucocorticoid, Glucocorticoid90$ps, Glucocorticoid90$kernel.wt, "kernel.Glucocorticoid90")
outcome.model(Y="outcome1", Glucocorticoid90$kernel.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="kernel.Glucocorticoid90")

outcome.model(Y="Death", Glucocorticoid90$kernel.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="kernel.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", Glucocorticoid90$kernel.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="kernel.Glucocorticoid90")
outcome.model(Y="ICU.admission", Glucocorticoid90$kernel.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="kernel.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Glucocorticoid90$kernel.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="kernel.Glucocorticoid90")

print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90$kernel.wt, df.study=Glucocorticoid.study, save_name="kernel.Glucocorticoid90")

sink()

###############################################
############  Hos Glucocorticoid 120    ###############
###############################################

hos.Glucocorticoid120.con <- hos.Glucocorticoid120[,Glucocorticoid.confounder]
hos.Glucocorticoid120.fit <- glm(hos.Glucocorticoid120$Glucocorticoid ~ ., data = hos.Glucocorticoid120.con, family="binomial")

hos.Glucocorticoid120$ps <- hos.Glucocorticoid120.fit$fitted.values
ind.trt0 <- which(hos.Glucocorticoid120$Glucocorticoid == 0)
ind.trt1 <- which(hos.Glucocorticoid120$Glucocorticoid == 1)

hos.Glucocorticoid120$iptw.wt <- ( 1/hos.Glucocorticoid120$ps )*hos.Glucocorticoid120$Glucocorticoid + ( 1/(1-hos.Glucocorticoid120$ps) )*( 1-hos.Glucocorticoid120$Glucocorticoid )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW]results_hos.Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.Glucocorticoid120$Glucocorticoid, hos.Glucocorticoid120$ps, hos.Glucocorticoid120$iptw.wt, "hos.Glucocorticoid120")
outcome.model(Y="outcome1", wt=hos.Glucocorticoid120$iptw.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="hos.Glucocorticoid120") # 0.6249

outcome.model(Y="Death", wt=hos.Glucocorticoid120$iptw.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.Glucocorticoid120$iptw.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="hos.Glucocorticoid120")
outcome.model(Y="ICU.admission", wt=hos.Glucocorticoid120$iptw.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.Glucocorticoid120$iptw.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="hos.Glucocorticoid120")

print_table(hos.Glucocorticoid120, wt=hos.Glucocorticoid120$iptw.wt, df.study=Glucocorticoid.study, save_name="hos.Glucocorticoid120")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim]results_hos.Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.Glucocorticoid120$trim.wt <- hos.Glucocorticoid120$iptw.wt
hos.Glucocorticoid120$trim.wt[(hos.Glucocorticoid120$ps<ps.cut | hos.Glucocorticoid120$ps>(1-ps.cut))] <- 0
print(nrow(hos.Glucocorticoid120[hos.Glucocorticoid120$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(hos.Glucocorticoid120$Glucocorticoid[hos.Glucocorticoid120$trim.wt!=0], hos.Glucocorticoid120$ps[hos.Glucocorticoid120$trim.wt!=0], hos.Glucocorticoid120$trim.wt[hos.Glucocorticoid120$trim.wt!=0], "trim.hos.Glucocorticoid120")
outcome.model(Y="outcome1", wt=hos.Glucocorticoid120$trim.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid120") # 0.6802

outcome.model(Y="Death", wt=hos.Glucocorticoid120$trim.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.Glucocorticoid120$trim.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid120")
outcome.model(Y="ICU.admission", wt=hos.Glucocorticoid120$trim.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.Glucocorticoid120$trim.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid120")

print_table(hos.Glucocorticoid120, wt=hos.Glucocorticoid120$trim.wt, df.study=Glucocorticoid.study, save_name="trim.hos.Glucocorticoid120")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized]results_hos.Glucocorticoid120.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.Glucocorticoid <- sum(hos.Glucocorticoid120$Glucocorticoid) / nrow(hos.Glucocorticoid120)
# wt : ATE
hos.Glucocorticoid120$st.iptw.wt <- ( p.Glucocorticoid/hos.Glucocorticoid120$ps )*hos.Glucocorticoid120$Glucocorticoid + ( (1-p.Glucocorticoid)/(1-hos.Glucocorticoid120$ps) )*( 1-hos.Glucocorticoid120$Glucocorticoid )

ps_summary(hos.Glucocorticoid120$Glucocorticoid, hos.Glucocorticoid120$ps, hos.Glucocorticoid120$st.iptw.wt, "stabilized.hos.Glucocorticoid120")

outcome.model(Y="outcome1", hos.Glucocorticoid120$st.iptw.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.hos.Glucocorticoid120") # -1.21

outcome.model(Y="Death", hos.Glucocorticoid120$st.iptw.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid120$st.iptw.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.hos.Glucocorticoid120")
outcome.model(Y="ICU.admission", hos.Glucocorticoid120$st.iptw.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid120$st.iptw.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.hos.Glucocorticoid120")

print_table(hos.Glucocorticoid120, wt=hos.Glucocorticoid120$st.iptw.wt, df.study=Glucocorticoid.study, save_name="stabilized.hos.Glucocorticoid120")

sink()

#### IPTW : ATT ####

hos.Glucocorticoid120$iptw.att <- hos.Glucocorticoid120$ps/(1-hos.Glucocorticoid120$ps)
hos.Glucocorticoid120$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[Hos_IPTW_ATT]results_hos.Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.Glucocorticoid120$Glucocorticoid, hos.Glucocorticoid120$ps, hos.Glucocorticoid120$iptw.att, "hos.Glucocorticoid120.att")
outcome.model(Y="outcome1", hos.Glucocorticoid120$iptw.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="hos.Glucocorticoid120.att") # 0.6249

outcome.model(Y="Death", hos.Glucocorticoid120$iptw.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="hos.Glucocorticoid120.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid120$iptw.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="hos.Glucocorticoid120.att")
outcome.model(Y="ICU.admission", hos.Glucocorticoid120$iptw.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="hos.Glucocorticoid120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid120$iptw.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="hos.Glucocorticoid120.att")

print_table(hos.Glucocorticoid120, hos.Glucocorticoid120$iptw.att, df.study=Glucocorticoid.study, save_name="hos.Glucocorticoid120.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim_ATT]results_hos.Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.Glucocorticoid120$trim.att <- hos.Glucocorticoid120$iptw.att
hos.Glucocorticoid120$trim.att[(hos.Glucocorticoid120$ps<ps.cut | hos.Glucocorticoid120$ps>(1-ps.cut))] <- 0
print(nrow(hos.Glucocorticoid120[hos.Glucocorticoid120$trim.att!=0,]))

ps_summary(hos.Glucocorticoid120$Glucocorticoid[hos.Glucocorticoid120$trim.att!=0], hos.Glucocorticoid120$ps[hos.Glucocorticoid120$trim.att!=0], hos.Glucocorticoid120$trim.att[hos.Glucocorticoid120$trim.att!=0], "trim.hos.Glucocorticoid120.att")
outcome.model(Y="outcome1", hos.Glucocorticoid120$trim.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid120.att") # 0.6802

outcome.model(Y="Death", hos.Glucocorticoid120$trim.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid120.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid120$trim.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid120.att")
outcome.model(Y="ICU.admission", hos.Glucocorticoid120$trim.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid120.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid120$trim.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid120.att")

print_table(hos.Glucocorticoid120, hos.Glucocorticoid120$trim.att, df.study=Glucocorticoid.study, save_name="trim.hos.Glucocorticoid120.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized_ATT]results_hos.Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
hos.Glucocorticoid120$st.iptw.att <- ( (1-p.Glucocorticoid)/p.Glucocorticoid) * (hos.Glucocorticoid120$ps / (1-hos.Glucocorticoid120$ps) )  *  (1-hos.Glucocorticoid120$Glucocorticoid)
hos.Glucocorticoid120$st.iptw.att[ind.trt1] <- 1
ps_summary(hos.Glucocorticoid120$Glucocorticoid, hos.Glucocorticoid120$ps, hos.Glucocorticoid120$st.iptw.att, "stabilized.att.hos.Glucocorticoid120")

outcome.model(Y="outcome1", hos.Glucocorticoid120$st.iptw.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.att.hos.Glucocorticoid120") # -1.21

outcome.model(Y="Death", hos.Glucocorticoid120$st.iptw.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.att.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid120$st.iptw.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.att.hos.Glucocorticoid120")
outcome.model(Y="ICU.admission", hos.Glucocorticoid120$st.iptw.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.att.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid120$st.iptw.att, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="stabilized.att.hos.Glucocorticoid120")

print_table(hos.Glucocorticoid120, wt=hos.Glucocorticoid120$st.iptw.att, df.study=Glucocorticoid.study, save_name="stabilized.att.hos.Glucocorticoid120")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[Hos_Deciles]results_hos.Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

hos.Glucocorticoid120$ps.dec <- cut(hos.Glucocorticoid120$ps, 
                                    breaks=c(quantile(hos.Glucocorticoid120$ps, probs=seq(0,1,0.1))),
                                    labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(hos.Glucocorticoid120$ps.dec)); cat("\n")
hos.Glucocorticoid120$dec_info <- paste0(hos.Glucocorticoid120$Glucocorticoid, ":", hos.Glucocorticoid120$ps.dec)

cat("\n"); cat(table(hos.Glucocorticoid120$dec_info)); cat("\n"); 
hos.Glucocorticoid120.dec <- data.frame(table(hos.Glucocorticoid120$ps.dec, hos.Glucocorticoid120$Glucocorticoid))
cat("\n"); print(hos.Glucocorticoid120.dec); cat("\n"); 

colnames(hos.Glucocorticoid120.dec) <- c("ps.dec", "Glucocorticoid", "n")
hos.Glucocorticoid120.dec$dec_info <- paste0(hos.Glucocorticoid120.dec$Glucocorticoid, ":", hos.Glucocorticoid120.dec$ps.dec)
hos.Glucocorticoid120.dec$n[hos.Glucocorticoid120.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.Glucocorticoid120.dec$dec.wt <- 1/hos.Glucocorticoid120.dec$n

if (is.null(hos.Glucocorticoid120$dec.wt)) {
  hos.Glucocorticoid120 <- merge(hos.Glucocorticoid120, hos.Glucocorticoid120.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(hos.Glucocorticoid120$Glucocorticoid, hos.Glucocorticoid120$ps, hos.Glucocorticoid120$dec.wt, "dec.hos.Glucocorticoid120")
outcome.model(Y="outcome1", hos.Glucocorticoid120$dec.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="dec.hos.Glucocorticoid120") # -0.03765

outcome.model(Y="Death", hos.Glucocorticoid120$dec.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="dec.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid120$dec.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="dec.hos.Glucocorticoid120")
outcome.model(Y="ICU.admission", hos.Glucocorticoid120$dec.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="dec.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid120$dec.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="dec.hos.Glucocorticoid120")

print_table(hos.Glucocorticoid120, wt=hos.Glucocorticoid120$dec.wt, df.study=Glucocorticoid.study, save_name="dec.hos.Glucocorticoid120")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[Hos_Matching]results_hos.Glucocorticoid120.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = hos.Glucocorticoid120$ps[ind.trt0]
trt1.ps = hos.Glucocorticoid120$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.Glucocorticoid120$match.wt = rep(0,nrow(hos.Glucocorticoid120))
hos.Glucocorticoid120$match.wt[ind.trt1] = 1
hos.Glucocorticoid120$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(hos.Glucocorticoid120$Glucocorticoid, hos.Glucocorticoid120$ps, hos.Glucocorticoid120$match.wt, "match.hos.Glucocorticoid120")
outcome.model(Y="outcome1", hos.Glucocorticoid120$match.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="match.hos.Glucocorticoid120") # 1.6468

outcome.model(Y="Death", hos.Glucocorticoid120$match.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="match.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid120$match.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="match.hos.Glucocorticoid120")
outcome.model(Y="ICU.admission", hos.Glucocorticoid120$match.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="match.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid120$match.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="match.hos.Glucocorticoid120")

print_table(hos.Glucocorticoid120, wt=hos.Glucocorticoid120$match.wt, df.study=Glucocorticoid.study, save_name="match.hos.Glucocorticoid120")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[Hos_Kernel]results_hos.Glucocorticoid120.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

hos.Glucocorticoid120$kernel.wt = rep(0,nrow(hos.Glucocorticoid120))
hos.Glucocorticoid120$kernel.wt[ind.trt0] = colSums(matchMat.k)
hos.Glucocorticoid120$kernel.wt[ind.trt1] = 1

ps_summary(hos.Glucocorticoid120$Glucocorticoid, hos.Glucocorticoid120$ps, hos.Glucocorticoid120$kernel.wt, "kernel.hos.Glucocorticoid120")
outcome.model(Y="outcome1", hos.Glucocorticoid120$kernel.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="kernel.hos.Glucocorticoid120")

outcome.model(Y="Death", hos.Glucocorticoid120$kernel.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="kernel.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid120$kernel.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="kernel.hos.Glucocorticoid120")
outcome.model(Y="ICU.admission", hos.Glucocorticoid120$kernel.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="kernel.hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid120$kernel.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="kernel.hos.Glucocorticoid120")

print_table(hos.Glucocorticoid120, wt=hos.Glucocorticoid120$kernel.wt, df.study=Glucocorticoid.study, save_name="kernel.hos.Glucocorticoid120")

sink()


###############################################
############  Hos Glucocorticoid 90    ###############
###############################################

hos.Glucocorticoid90.con <- hos.Glucocorticoid90[,Glucocorticoid.confounder]
hos.Glucocorticoid90.fit <- glm(hos.Glucocorticoid90$Glucocorticoid ~ ., data = hos.Glucocorticoid90.con, family="binomial")

hos.Glucocorticoid90$ps <- hos.Glucocorticoid90.fit$fitted.values
ind.trt0 <- which(hos.Glucocorticoid90$Glucocorticoid == 0)
ind.trt1 <- which(hos.Glucocorticoid90$Glucocorticoid == 1)

hos.Glucocorticoid90$iptw.wt <- ( 1/hos.Glucocorticoid90$ps )*hos.Glucocorticoid90$Glucocorticoid + ( 1/(1-hos.Glucocorticoid90$ps) )*( 1-hos.Glucocorticoid90$Glucocorticoid )

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW]results_hos.Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.Glucocorticoid90$Glucocorticoid, hos.Glucocorticoid90$ps, hos.Glucocorticoid90$iptw.wt, "hos.Glucocorticoid90")
outcome.model(Y="outcome1", wt=hos.Glucocorticoid90$iptw.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="hos.Glucocorticoid90") # 0.6249

outcome.model(Y="Death", wt=hos.Glucocorticoid90$iptw.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.Glucocorticoid90$iptw.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="hos.Glucocorticoid90")
outcome.model(Y="ICU.admission", wt=hos.Glucocorticoid90$iptw.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.Glucocorticoid90$iptw.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="hos.Glucocorticoid90")

print_table(hos.Glucocorticoid90, wt=hos.Glucocorticoid90$iptw.wt, df.study=Glucocorticoid.study, save_name="hos.Glucocorticoid90")
sink()

#### IPTW TRIMMING : ATE ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim]results_hos.Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.Glucocorticoid90$trim.wt <- hos.Glucocorticoid90$iptw.wt
hos.Glucocorticoid90$trim.wt[(hos.Glucocorticoid90$ps<ps.cut | hos.Glucocorticoid90$ps>(1-ps.cut))] <- 0
print(nrow(hos.Glucocorticoid90[hos.Glucocorticoid90$trim.wt!=0,]))

# ps_summary 결과 재확인 필요
ps_summary(hos.Glucocorticoid90$Glucocorticoid[hos.Glucocorticoid90$trim.wt!=0], hos.Glucocorticoid90$ps[hos.Glucocorticoid90$trim.wt!=0], hos.Glucocorticoid90$trim.wt[hos.Glucocorticoid90$trim.wt!=0], "trim.hos.Glucocorticoid90")
outcome.model(Y="outcome1", wt=hos.Glucocorticoid90$trim.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid90") # 0.6802

outcome.model(Y="Death", wt=hos.Glucocorticoid90$trim.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.Glucocorticoid90$trim.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid90")
outcome.model(Y="ICU.admission", wt=hos.Glucocorticoid90$trim.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.Glucocorticoid90$trim.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid90")

print_table(hos.Glucocorticoid90, wt=hos.Glucocorticoid90$trim.wt, df.study=Glucocorticoid.study, save_name="trim.hos.Glucocorticoid90")
sink()

#### Stabilized IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized]results_hos.Glucocorticoid90.txt")
cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.Glucocorticoid <- sum(hos.Glucocorticoid90$Glucocorticoid) / nrow(hos.Glucocorticoid90)
# wt : ATE
hos.Glucocorticoid90$st.iptw.wt <- ( p.Glucocorticoid/hos.Glucocorticoid90$ps )*hos.Glucocorticoid90$Glucocorticoid + ( (1-p.Glucocorticoid)/(1-hos.Glucocorticoid90$ps) )*( 1-hos.Glucocorticoid90$Glucocorticoid )

ps_summary(hos.Glucocorticoid90$Glucocorticoid, hos.Glucocorticoid90$ps, hos.Glucocorticoid90$st.iptw.wt, "stabilized.hos.Glucocorticoid90")

outcome.model(Y="outcome1", hos.Glucocorticoid90$st.iptw.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.hos.Glucocorticoid90") # -1.21

outcome.model(Y="Death", hos.Glucocorticoid90$st.iptw.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid90$st.iptw.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.hos.Glucocorticoid90")
outcome.model(Y="ICU.admission", hos.Glucocorticoid90$st.iptw.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid90$st.iptw.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.hos.Glucocorticoid90")

print_table(hos.Glucocorticoid90, wt=hos.Glucocorticoid90$st.iptw.wt, df.study=Glucocorticoid.study, save_name="stabilized.hos.Glucocorticoid90")

sink()

#### IPTW : ATT ####

hos.Glucocorticoid90$iptw.att <- hos.Glucocorticoid90$ps/(1-hos.Glucocorticoid90$ps)
hos.Glucocorticoid90$iptw.att[ind.trt1] <- 1

sink("./resultsCNLLS/model/[Hos_IPTW_ATT]results_hos.Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT\n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.Glucocorticoid90$Glucocorticoid, hos.Glucocorticoid90$ps, hos.Glucocorticoid90$iptw.att, "hos.Glucocorticoid90.att")
outcome.model(Y="outcome1", hos.Glucocorticoid90$iptw.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="hos.Glucocorticoid90.att") # 0.6249

outcome.model(Y="Death", hos.Glucocorticoid90$iptw.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="hos.Glucocorticoid90.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid90$iptw.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="hos.Glucocorticoid90.att")
outcome.model(Y="ICU.admission", hos.Glucocorticoid90$iptw.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="hos.Glucocorticoid90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid90$iptw.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="hos.Glucocorticoid90.att")

print_table(hos.Glucocorticoid90, hos.Glucocorticoid90$iptw.att, df.study=Glucocorticoid.study, save_name="hos.Glucocorticoid90.att")
sink()

#### IPTW TRIMMING : ATT ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim_ATT]results_hos.Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.Glucocorticoid90$trim.att <- hos.Glucocorticoid90$iptw.att
hos.Glucocorticoid90$trim.att[(hos.Glucocorticoid90$ps<ps.cut | hos.Glucocorticoid90$ps>(1-ps.cut))] <- 0
print(nrow(hos.Glucocorticoid90[hos.Glucocorticoid90$trim.att!=0,]))

ps_summary(hos.Glucocorticoid90$Glucocorticoid[hos.Glucocorticoid90$trim.att!=0], hos.Glucocorticoid90$ps[hos.Glucocorticoid90$trim.att!=0], hos.Glucocorticoid90$trim.att[hos.Glucocorticoid90$trim.att!=0], "trim.hos.Glucocorticoid90.att")
outcome.model(Y="outcome1", hos.Glucocorticoid90$trim.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid90.att") # 0.6802

outcome.model(Y="Death", hos.Glucocorticoid90$trim.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid90.att")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid90$trim.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid90.att")
outcome.model(Y="ICU.admission", hos.Glucocorticoid90$trim.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid90.att")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid90$trim.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="trim.hos.Glucocorticoid90.att")

print_table(hos.Glucocorticoid90, hos.Glucocorticoid90$trim.att, df.study=Glucocorticoid.study, save_name="trim.hos.Glucocorticoid90.att")
sink()

#### Stabilized IPTW : ATT ####
sink("./resultsCNLLS/model/[Hos_IPTW_stabilized_ATT]results_hos.Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW ATT : Stabilized \n") # -1.1
cat("------------------\n")
# wt2 : ATT 
hos.Glucocorticoid90$st.iptw.att <- ( (1-p.Glucocorticoid)/p.Glucocorticoid) * (hos.Glucocorticoid90$ps / (1-hos.Glucocorticoid90$ps) )  *  (1-hos.Glucocorticoid90$Glucocorticoid)
hos.Glucocorticoid90$st.iptw.att[ind.trt1] <- 1
ps_summary(hos.Glucocorticoid90$Glucocorticoid, hos.Glucocorticoid90$ps, hos.Glucocorticoid90$st.iptw.att, "stabilized.att.hos.Glucocorticoid90")

outcome.model(Y="outcome1", hos.Glucocorticoid90$st.iptw.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.att.hos.Glucocorticoid90") # -1.21

outcome.model(Y="Death", hos.Glucocorticoid90$st.iptw.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.att.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid90$st.iptw.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.att.hos.Glucocorticoid90")
outcome.model(Y="ICU.admission", hos.Glucocorticoid90$st.iptw.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.att.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid90$st.iptw.att, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="stabilized.att.hos.Glucocorticoid90")

print_table(hos.Glucocorticoid90, wt=hos.Glucocorticoid90$st.iptw.att, df.study=Glucocorticoid.study, save_name="stabilized.att.hos.Glucocorticoid90")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[Hos_Deciles]results_hos.Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 90] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

hos.Glucocorticoid90$ps.dec <- cut(hos.Glucocorticoid90$ps, 
                                   breaks=c(quantile(hos.Glucocorticoid90$ps, probs=seq(0,1,0.1))),
                                   labels=seq(1:10), include.lowest=TRUE)
cat("\n"); cat(table(hos.Glucocorticoid90$ps.dec)); cat("\n")
hos.Glucocorticoid90$dec_info <- paste0(hos.Glucocorticoid90$Glucocorticoid, ":", hos.Glucocorticoid90$ps.dec)

cat("\n"); cat(table(hos.Glucocorticoid90$dec_info)); cat("\n"); 
hos.Glucocorticoid90.dec <- data.frame(table(hos.Glucocorticoid90$ps.dec, hos.Glucocorticoid90$Glucocorticoid))
cat("\n"); print(hos.Glucocorticoid90.dec); cat("\n"); 

colnames(hos.Glucocorticoid90.dec) <- c("ps.dec", "Glucocorticoid", "n")
hos.Glucocorticoid90.dec$dec_info <- paste0(hos.Glucocorticoid90.dec$Glucocorticoid, ":", hos.Glucocorticoid90.dec$ps.dec)
hos.Glucocorticoid90.dec$n[hos.Glucocorticoid90.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.Glucocorticoid90.dec$dec.wt <- 1/hos.Glucocorticoid90.dec$n

if (is.null(hos.Glucocorticoid90$dec.wt)) {
  hos.Glucocorticoid90 <- merge(hos.Glucocorticoid90, hos.Glucocorticoid90.dec[,c("dec_info", "dec.wt")], by="dec_info")
} else {
  cat("\n(Warnings) dec.wt is not null\n")
}

ps_summary(hos.Glucocorticoid90$Glucocorticoid, hos.Glucocorticoid90$ps, hos.Glucocorticoid90$dec.wt, "dec.hos.Glucocorticoid90")
outcome.model(Y="outcome1", hos.Glucocorticoid90$dec.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="dec.hos.Glucocorticoid90") # -0.03765

outcome.model(Y="Death", hos.Glucocorticoid90$dec.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="dec.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid90$dec.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="dec.hos.Glucocorticoid90")
outcome.model(Y="ICU.admission", hos.Glucocorticoid90$dec.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="dec.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid90$dec.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="dec.hos.Glucocorticoid90")

print_table(hos.Glucocorticoid90, wt=hos.Glucocorticoid90$dec.wt, df.study=Glucocorticoid.study, save_name="dec.hos.Glucocorticoid90")

sink()

#### One to One Matching ####

sink("./resultsCNLLS/model/[Hos_Matching]results_hos.Glucocorticoid90.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
trt0.ps = hos.Glucocorticoid90$ps[ind.trt0]
trt1.ps = hos.Glucocorticoid90$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.Glucocorticoid90$match.wt = rep(0,nrow(hos.Glucocorticoid90))
hos.Glucocorticoid90$match.wt[ind.trt1] = 1
hos.Glucocorticoid90$match.wt[ind.trt0] = colSums(matchMat)

ps_summary(hos.Glucocorticoid90$Glucocorticoid, hos.Glucocorticoid90$ps, hos.Glucocorticoid90$match.wt, "match.hos.Glucocorticoid90")
outcome.model(Y="outcome1", hos.Glucocorticoid90$match.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="match.hos.Glucocorticoid90") # 1.6468

outcome.model(Y="Death", hos.Glucocorticoid90$match.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="match.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid90$match.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="match.hos.Glucocorticoid90")
outcome.model(Y="ICU.admission", hos.Glucocorticoid90$match.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="match.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid90$match.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="match.hos.Glucocorticoid90")

print_table(hos.Glucocorticoid90, wt=hos.Glucocorticoid90$match.wt, df.study=Glucocorticoid.study, save_name="match.hos.Glucocorticoid90")

sink()

#### Kernel Matching ####
sink("./resultsCNLLS/model/[Hos_Kernel]results_hos.Glucocorticoid90.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")
propDiffMat.k = propDiffMat/h  

rownames(propDiffMat.k) = ind.trt1
colnames(propDiffMat.k) = ind.trt0

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

hos.Glucocorticoid90$kernel.wt = rep(0,nrow(hos.Glucocorticoid90))
hos.Glucocorticoid90$kernel.wt[ind.trt0] = colSums(matchMat.k)
hos.Glucocorticoid90$kernel.wt[ind.trt1] = 1

ps_summary(hos.Glucocorticoid90$Glucocorticoid, hos.Glucocorticoid90$ps, hos.Glucocorticoid90$kernel.wt, "kernel.hos.Glucocorticoid90")
outcome.model(Y="outcome1", hos.Glucocorticoid90$kernel.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="kernel.hos.Glucocorticoid90")

outcome.model(Y="Death", hos.Glucocorticoid90$kernel.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="kernel.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Glucocorticoid90$kernel.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="kernel.hos.Glucocorticoid90")
outcome.model(Y="ICU.admission", hos.Glucocorticoid90$kernel.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="kernel.hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Glucocorticoid90$kernel.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="kernel.hos.Glucocorticoid90")

print_table(hos.Glucocorticoid90, wt=hos.Glucocorticoid90$kernel.wt, df.study=Glucocorticoid.study, save_name="kernel.hos.Glucocorticoid90")

sink()

###############################################
############  All Anticoagulants 120    ###############
###############################################

Anticoagulants120.con <- Anticoagulants120[,Anticoagulants.confounder]
Anticoagulants120.fit <- glm(Anticoagulants120$Anticoagulants ~ ., data = Anticoagulants120.con, family="binomial")

Anticoagulants120$ps <- Anticoagulants120.fit$fitted.values
ind.trt0 <- which(Anticoagulants120$Anticoagulants == 0)
ind.trt1 <- which(Anticoagulants120$Anticoagulants == 1)

Anticoagulants120$iptw.wt <- ( 1/Anticoagulants120$ps )*Anticoagulants120$Anticoagulants + ( 1/(1-Anticoagulants120$ps) )*( 1-Anticoagulants120$Anticoagulants )

Anticoagulants120$iptw.att <- Anticoagulants120$ps/(1-Anticoagulants120$ps)
Anticoagulants120$iptw.att[ind.trt1] <- 1

#### Bleeding and Transfusion ####
Anticoagulants.scr1 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr1 <- glm(Anticoagulants120[,"bleeding.transfusion"]~Anticoagulants120[,c], data=Anticoagulants120, family = "binomial")
  if ( length(summary(fit.scr1)$coef) == 4) {
    print("ERROR!!, exposure coefficeints is NA.")
    next
  }
  if (summary(fit.scr1)$coef[2,4] < 0.2){
    cat("\n", summary(fit.scr1)$coef[2,4], "\n")
    Anticoagulants.scr1 <- c(Anticoagulants.scr1, c)
  }
}
cat("\n [Confounder after p-value scr1eening(0.2)] \n")
print(Anticoagulants.study[Anticoagulants.study$var %in% Anticoagulants.scr1, c(2,1)])

scr1 <- Anticoagulants120[,Anticoagulants.scr1]

#### myocardial.stroke.TIA.Thromboembolism ####
Anticoagulants.scr2 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr2 <- glm(Anticoagulants120[,"myocardial.storke.TIA.Thromboembolism"]~Anticoagulants120[,c], data=Anticoagulants120, family = "binomial")
  if ( length(summary(fit.scr2)$coef) == 4) {
    print("ERROR!!, exposure coefficeints is NA.")
    next
  }
  if (summary(fit.scr2)$coef[2,4] < 0.2){
    cat("\n", summary(fit.scr1)$coef[2,4], "\n")
    Anticoagulants.scr2 <- c(Anticoagulants.scr2, c)
  }
}
cat("\n [Confounder after p-value scr2eening(0.2)] \n")
print(Anticoagulants.study[Anticoagulants.study$var %in% Anticoagulants.scr2, c(2,1)])

scr2 <- Anticoagulants120[,Anticoagulants.scr2]

# [1] ps model for screening variable
# (a) exposure ~ confounders
fit1 <- glm(Anticoagulants120$Anticoagulants ~ ., data = scr1, family="binomial")
Anticoagulants120$ps2 <- fit1$fitted.values

Anticoagulants120$iptw.wt2 <- ( 1/Anticoagulants120$ps2 )*Anticoagulants120$Anticoagulants + ( 1/(1-Anticoagulants120$ps2) )*( 1-Anticoagulants120$Anticoagulants )

Anticoagulants120$iptw.att2 <- Anticoagulants120$ps2/(1-Anticoagulants120$ps2)
Anticoagulants120$iptw.att2[ind.trt1] <- 1

fit1 <- glm(Anticoagulants120$Anticoagulants ~ ., data = scr2, family="binomial")
Anticoagulants120$ps3 <- fit1$fitted.values

Anticoagulants120$iptw.wt3 <- ( 1/Anticoagulants120$ps3 )*Anticoagulants120$Anticoagulants + ( 1/(1-Anticoagulants120$ps3) )*( 1-Anticoagulants120$Anticoagulants )

Anticoagulants120$iptw.att3 <- Anticoagulants120$ps3/(1-Anticoagulants120$ps3)
Anticoagulants120$iptw.att3[ind.trt1] <- 1

#### code ####

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW]results_Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps, Anticoagulants120$iptw.wt, "Anticoagulants120")
outcome.model(Y="outcome1", wt=Anticoagulants120$iptw.wt, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants120") # -17

outcome.model(Y="Death", wt=Anticoagulants120$iptw.wt, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=Anticoagulants120$iptw.wt, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants120")
outcome.model(Y="ICU.admission", wt=Anticoagulants120$iptw.wt, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=Anticoagulants120$iptw.wt, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps2, Anticoagulants120$iptw.wt2, "Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants120$iptw.wt2, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps3, Anticoagulants120$iptw.wt3, "Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants120$iptw.wt3, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants120")

print_table(df.all=Anticoagulants120, wt=Anticoagulants120$iptw.wt, df.study=Anticoagulants.study, save_name="Anticoagulants120")
sink()

#### IPTW Trimming : ATE ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim]results_Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

Anticoagulants120$trim.wt <- Anticoagulants120$iptw.wt
Anticoagulants120$trim.wt[(Anticoagulants120$ps<ps.cut | Anticoagulants120$ps>(1-ps.cut))] <- 0

Anticoagulants120$trim.wt2 <- Anticoagulants120$iptw.wt2
Anticoagulants120$trim.wt2[(Anticoagulants120$ps2<ps.cut | Anticoagulants120$ps2>(1-ps.cut))] <- 0

Anticoagulants120$trim.wt3 <- Anticoagulants120$iptw.wt3
Anticoagulants120$trim.wt3[(Anticoagulants120$ps3<ps.cut | Anticoagulants120$ps3>(1-ps.cut))] <- 0

print(nrow(Anticoagulants120[Anticoagulants120$trim.wt!=0,]))
print(nrow(Anticoagulants120[Anticoagulants120$trim.wt2!=0,]))
print(nrow(Anticoagulants120[Anticoagulants120$trim.wt3!=0,]))

ps_summary(Anticoagulants120$Anticoagulants[Anticoagulants120$trim.wt!=0], Anticoagulants120$ps[Anticoagulants120$trim.wt!=0], Anticoagulants120$trim.wt[Anticoagulants120$trim.wt!=0], "trim.Anticoagulants120")
outcome.model(Y="outcome1", wt=Anticoagulants120$trim.wt, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants120") # -17

outcome.model(Y="Death", wt=Anticoagulants120$trim.wt, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=Anticoagulants120$trim.wt, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants120")
outcome.model(Y="ICU.admission", wt=Anticoagulants120$trim.wt, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=Anticoagulants120$trim.wt, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps2, Anticoagulants120$trim.wt2, "trim.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants120$trim.wt2, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps3, Anticoagulants120$trim.wt3, "trim.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants120$trim.wt3, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants120")

print_table(df.all=Anticoagulants120, wt=Anticoagulants120$trim.wt, df.study=Anticoagulants.study, save_name="trim.Anticoagulants120")
sink()

#### IPTW Stabilized : ATE ####

sink("./resultsCNLLS/model/[ALL_IPTW_stabilized]results_Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.Anticoagulants <- sum(Anticoagulants120$Anticoagulants) / nrow(Anticoagulants120)
Anticoagulants120$st.iptw.wt1 <- ( p.Anticoagulants/Anticoagulants120$ps )*Anticoagulants120$Anticoagulants + ( (1-p.Anticoagulants)/(1-Anticoagulants120$ps) )*( 1-Anticoagulants120$Anticoagulants )
Anticoagulants120$st.iptw.wt2 <- ( p.Anticoagulants/Anticoagulants120$ps2 )*Anticoagulants120$Anticoagulants + ( (1-p.Anticoagulants)/(1-Anticoagulants120$ps2) )*( 1-Anticoagulants120$Anticoagulants )
Anticoagulants120$st.iptw.wt3 <- ( p.Anticoagulants/Anticoagulants120$ps3 )*Anticoagulants120$Anticoagulants + ( (1-p.Anticoagulants)/(1-Anticoagulants120$ps3) )*( 1-Anticoagulants120$Anticoagulants )

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps, Anticoagulants120$st.iptw.wt1, "stabilized.Anticoagulants120")

outcome.model(Y="outcome1", Anticoagulants120$st.iptw.wt1, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants120") # -15

outcome.model(Y="Death", Anticoagulants120$st.iptw.wt1, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants120$st.iptw.wt1, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants120")
outcome.model(Y="ICU.admission", Anticoagulants120$st.iptw.wt1, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants120$st.iptw.wt1, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps2, Anticoagulants120$st.iptw.wt2, "stabilized.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants120$st.iptw.wt2, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps3, Anticoagulants120$st.iptw.wt3, "stabilized.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants120$st.iptw.wt3, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants120")

print_table(df.all=Anticoagulants120, wt=Anticoagulants120$st.iptw.wt1, df.study=Anticoagulants.study, save_name="stabilized.Anticoagulants120")

sink()

#### IPTW : ATT ####

sink("./resultsCNLLS/model/[ALL_IPTW_ATT]results_Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps, Anticoagulants120$iptw.att, "Anticoagulants.att120")
outcome.model(Y="outcome1", Anticoagulants120$iptw.att, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants.att120") # -17

outcome.model(Y="Death", Anticoagulants120$iptw.att, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants120$iptw.att, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants.att120")
outcome.model(Y="ICU.admission", Anticoagulants120$iptw.att, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants120$iptw.att, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants.att120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps2, Anticoagulants120$iptw.att2, "Anticoagulants.att120")
outcome.model(Y="bleeding.transfusion", Anticoagulants120$iptw.att2, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants.att120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps3, Anticoagulants120$iptw.att3, "Anticoagulants.att120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", Anticoagulants120$iptw.att3, df=Anticoagulants120, study=Anticoagulants.study, name="Anticoagulants.att120")

print_table(df.all=Anticoagulants120, Anticoagulants120$iptw.att, df.study=Anticoagulants.study, save_name="Anticoagulants.att120")
sink()

#### IPTW Trimming : ATT ####
sink("./resultsCNLLS/model/[ALL_IPTW_trim_ATT]results_Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

Anticoagulants120$trim.att <- Anticoagulants120$iptw.att
Anticoagulants120$trim.att[(Anticoagulants120$ps<ps.cut | Anticoagulants120$ps>(1-ps.cut))] <- 0

Anticoagulants120$trim.att2 <- Anticoagulants120$iptw.att2
Anticoagulants120$trim.att2[(Anticoagulants120$ps2<ps.cut | Anticoagulants120$ps2>(1-ps.cut))] <- 0

Anticoagulants120$trim.att3 <- Anticoagulants120$iptw.att3
Anticoagulants120$trim.att3[(Anticoagulants120$ps3<ps.cut | Anticoagulants120$ps3>(1-ps.cut))] <- 0

print(nrow(Anticoagulants120[Anticoagulants120$trim.att!=0,]))
print(nrow(Anticoagulants120[Anticoagulants120$trim.att2!=0,]))
print(nrow(Anticoagulants120[Anticoagulants120$trim.att3!=0,]))

ps_summary(Anticoagulants120$Anticoagulants[Anticoagulants120$trim.att!=0], Anticoagulants120$ps[Anticoagulants120$trim.att!=0], Anticoagulants120$trim.att[Anticoagulants120$trim.att!=0], "trim.Anticoagulants.att120")
outcome.model(Y="outcome1", Anticoagulants120$trim.att, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants.att120") # -17

outcome.model(Y="Death", Anticoagulants120$trim.att, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants120$trim.att, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants.att120")
outcome.model(Y="ICU.admission", Anticoagulants120$trim.att, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants120$trim.att, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants.att120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps2, Anticoagulants120$trim.att2, "trim.Anticoagulants.att120")
outcome.model(Y="bleeding.transfusion", Anticoagulants120$trim.att2, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants.att120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps3, Anticoagulants120$trim.att3, "trim.Anticoagulants.att120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", Anticoagulants120$trim.att3, df=Anticoagulants120, study=Anticoagulants.study, name="trim.Anticoagulants.att120")

print_table(df.all=Anticoagulants120, Anticoagulants120$trim.att, df.study=Anticoagulants.study, save_name="trim.Anticoagulants.att120")
sink()

#### IPTW Stabilized : ATT ####
sink("./resultsCNLLS/model/[ALL_IPTW_stabilized_ATT]results_Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")

p.Anticoagulants <- sum(Anticoagulants120$Anticoagulants) / nrow(Anticoagulants120)

Anticoagulants120$st.iptw.att <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (Anticoagulants120$ps / (1-Anticoagulants120$ps) )  *  (1-Anticoagulants120$Anticoagulants)
Anticoagulants120$st.iptw.att[ind.trt1] <- 1

Anticoagulants120$st.iptw.att2 <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (Anticoagulants120$ps2 / (1-Anticoagulants120$ps2) )  *  (1-Anticoagulants120$Anticoagulants)
Anticoagulants120$st.iptw.att2[ind.trt1] <- 1

Anticoagulants120$st.iptw.att3 <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (Anticoagulants120$ps3 / (1-Anticoagulants120$ps3) )  *  (1-Anticoagulants120$Anticoagulants)
Anticoagulants120$st.iptw.att3[ind.trt1] <- 1

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps, Anticoagulants120$st.iptw.att, "stabilized.Anticoagulants.att120")

outcome.model(Y="outcome1", Anticoagulants120$st.iptw.att, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants.att120") # -15

outcome.model(Y="Death", Anticoagulants120$st.iptw.att, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants120$st.iptw.att, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants.att120")
outcome.model(Y="ICU.admission", Anticoagulants120$st.iptw.att, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants120$st.iptw.att, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants.att120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps2, Anticoagulants120$st.iptw.att2, "stabilized.Anticoagulants.att120")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants120$st.iptw.att2, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants.att120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps3, Anticoagulants120$st.iptw.att3, "stabilized.Anticoagulants.att120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants120$st.iptw.att3, df=Anticoagulants120, study=Anticoagulants.study, name="stabilized.Anticoagulants.att120")

print_table(df.all=Anticoagulants120, wt=Anticoagulants120$st.iptw.att, df.study=Anticoagulants.study, save_name="stabilized.Anticoagulants.att120")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[ALL_Deciles]results_Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

#(1)
Anticoagulants120$ps.dec <- cut(Anticoagulants120$ps, 
                                breaks=c(quantile(Anticoagulants120$ps, probs=seq(0,1,0.1))),
                                labels=seq(1:10), include.lowest=TRUE)
Anticoagulants120$dec_info <- paste0(Anticoagulants120$Anticoagulants, ":", Anticoagulants120$ps.dec)

Anticoagulants120.dec <- data.frame(table(Anticoagulants120$ps.dec, Anticoagulants120$Anticoagulants))
colnames(Anticoagulants120.dec) <- c("ps.dec", "Anticoagulants", "n")
Anticoagulants120.dec$dec_info <- paste0(Anticoagulants120.dec$Anticoagulants, ":", Anticoagulants120.dec$ps.dec)
Anticoagulants120.dec$n[Anticoagulants120.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
Anticoagulants120.dec$dec.wt <- 1/Anticoagulants120.dec$n

cat("\n (1) \n"); cat(Anticoagulants120.dec); cat("\n")
Anticoagulants120 <- merge(Anticoagulants120, Anticoagulants120.dec[,c("dec_info", "dec.wt")], by="dec_info")

#(2)
Anticoagulants120$ps.dec2 <- cut(Anticoagulants120$ps2, 
                                 breaks=c(quantile(Anticoagulants120$ps2, probs=seq(0,1,0.1))),
                                 labels=seq(1:10), include.lowest=TRUE)
Anticoagulants120$dec_info2 <- paste0(Anticoagulants120$Anticoagulants, ":", Anticoagulants120$ps.dec2)
Anticoagulants120.dec2 <- data.frame(table(Anticoagulants120$ps.dec2, Anticoagulants120$Anticoagulants))
colnames(Anticoagulants120.dec) <- c("ps.dec", "Anticoagulants", "n")
Anticoagulants120.dec$dec_info <- paste0(Anticoagulants120.dec2$Anticoagulants, ":", Anticoagulants120.dec2$ps.dec)
Anticoagulants120.dec2$n[Anticoagulants120.dec2$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
Anticoagulants120.dec2$dec.wt2 <- 1/Anticoagulants120.dec2$n

cat("\n (2) \n"); cat(Anticoagulants120.dec2); cat("\n")
Anticoagulants120 <- merge(Anticoagulants120, Anticoagulants120.dec2[,c("dec_info", "dec.wt2")], by="dec_info")

#(3)
Anticoagulants120$ps.dec3 <- cut(Anticoagulants120$ps3, 
                                 breaks=c(quantile(Anticoagulants120$ps3, probs=seq(0,1,0.1))),
                                 labels=seq(1:10), include.lowest=TRUE)
Anticoagulants120$dec_info3 <- paste0(Anticoagulants120$Anticoagulants, ":", Anticoagulants120$ps.dec3)
Anticoagulants120.dec3 <- data.frame(table(Anticoagulants120$ps.dec3, Anticoagulants120$Anticoagulants))
colnames(Anticoagulants120.dec) <- c("ps.dec", "Anticoagulants", "n")
Anticoagulants120.dec$dec_info <- paste0(Anticoagulants120.dec3$Anticoagulants, ":", Anticoagulants120.dec3$ps.dec)
Anticoagulants120.dec3$n[Anticoagulants120.dec2$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
Anticoagulants120.dec3$dec.wt3 <- 1/Anticoagulants120.dec3$n

cat("\n (3) \n"); cat(Anticoagulants120.dec3); cat("\n")
Anticoagulants120 <- merge(Anticoagulants120, Anticoagulants120.dec3[,c("dec_info", "dec.wt3")], by="dec_info")


ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps, Anticoagulants120$dec.wt, "dec.Anticoagulants120")
outcome.model(Y="outcome1", Anticoagulants120$dec.wt, df=Anticoagulants120, study=Anticoagulants.study, name="dec.Anticoagulants120") # -0.03765

outcome.model(Y="Death", Anticoagulants120$dec.wt, df=Anticoagulants120, study=Anticoagulants.study, name="dec.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants120$dec.wt, df=Anticoagulants120, study=Anticoagulants.study, name="dec.Anticoagulants120")
outcome.model(Y="ICU.admission", Anticoagulants120$dec.wt, df=Anticoagulants120, study=Anticoagulants.study, name="dec.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants120$dec.wt, df=Anticoagulants120, study=Anticoagulants.study, name="dec.Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps2, Anticoagulants120$dec.wt2, "dec.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants120$dec.wt2, df=Anticoagulants120, study=Anticoagulants.study, name="dec.Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps3, Anticoagulants120$dec.wt3, "dec.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants120$dec.wt3, df=Anticoagulants120, study=Anticoagulants.study, name="dec.Anticoagulants120")

print_table(df.all=Anticoagulants120, wt=Anticoagulants120$dec.wt, df.study=Anticoagulants.study, save_name="dec.Anticoagulants120")

sink()

#### One to One Matching ####
sink("./resultsCNLLS/model/[ALL_Matching]results_Anticoagulants120.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
# (1)
trt0.ps = Anticoagulants120$ps[ind.trt0]
trt1.ps = Anticoagulants120$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
Anticoagulants120$match.wt = rep(0,nrow(Anticoagulants120))
Anticoagulants120$match.wt[ind.trt1] = 1
Anticoagulants120$match.wt[ind.trt0] = colSums(matchMat)

# (2)
trt0.ps2 = Anticoagulants120$ps2[ind.trt0]
trt1.ps2 = Anticoagulants120$ps2[ind.trt1]

propDiffMat2 = outer(trt1.ps2, trt0.ps2, "-")
propDiffMat2 = abs(propDiffMat2)

# index 를 이름으로 붙여주기
rownames(propDiffMat2) = ind.trt1
colnames(propDiffMat2) = ind.trt0

matchMat2 = t( apply(propDiffMat2, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
Anticoagulants120$match.wt2 = rep(0,nrow(Anticoagulants120))
Anticoagulants120$match.wt2[ind.trt1] = 1
Anticoagulants120$match.wt2[ind.trt0] = colSums(matchMat2)

# (3)
trt0.ps3 = Anticoagulants120$ps3[ind.trt0]
trt1.ps3 = Anticoagulants120$ps3[ind.trt1]

propDiffMat3 = outer(trt1.ps3, trt0.ps3, "-")
propDiffMat3 = abs(propDiffMat3)

# index 를 이름으로 붙여주기
rownames(propDiffMat3) = ind.trt1
colnames(propDiffMat3) = ind.trt0

matchMat3 = t( apply(propDiffMat3, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
Anticoagulants120$match.wt3 = rep(0,nrow(Anticoagulants120))
Anticoagulants120$match.wt3[ind.trt1] = 1
Anticoagulants120$match.wt3[ind.trt0] = colSums(matchMat3)

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps, Anticoagulants120$match.wt, "match.Anticoagulants120")
outcome.model(Y="outcome1", Anticoagulants120$match.wt, df=Anticoagulants120, study=Anticoagulants.study, name="match.Anticoagulants120") # 1.6468

outcome.model(Y="Death", Anticoagulants120$match.wt, df=Anticoagulants120, study=Anticoagulants.study, name="match.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants120$match.wt, df=Anticoagulants120, study=Anticoagulants.study, name="match.Anticoagulants120")
outcome.model(Y="ICU.admission", Anticoagulants120$match.wt, df=Anticoagulants120, study=Anticoagulants.study, name="match.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants120$match.wt, df=Anticoagulants120, study=Anticoagulants.study, name="match.Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps2, Anticoagulants120$match.wt2, "match.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants120$match.wt2, df=Anticoagulants120, study=Anticoagulants.study, name="match.Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps3, Anticoagulants120$match.wt3, "match.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants120$match.wt3, df=Anticoagulants120, study=Anticoagulants.study, name="match.Anticoagulants120")

print_table(df.all=Anticoagulants120, wt=Anticoagulants120$match.wt, df.study=Anticoagulants.study, save_name="match.Anticoagulants120")

sink()

#### Kernel Matching ####

sink("./resultsCNLLS/model/[ALL_Kernel]results_Anticoagulants120.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")

#(1)
propDiffMat.k = propDiffMat/h  

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

Anticoagulants120$kernel.wt = rep(0,nrow(Anticoagulants120))
Anticoagulants120$kernel.wt[ind.trt0] = colSums(matchMat.k)
Anticoagulants120$kernel.wt[ind.trt1] = 1

#(2)
propDiffMat.k2 = propDiffMat2/h  

matchMat.k2 = exp(- propDiffMat.k2^2 / 2)
matchMat.k2 <- matchMat.k2 / rowSums(matchMat.k2)

Anticoagulants120$kernel.wt = rep(0,nrow(Anticoagulants120))
Anticoagulants120$kernel.wt[ind.trt0] = colSums(matchMat.k2)
Anticoagulants120$kernel.wt[ind.trt1] = 1

#(3)
propDiffMat.k3 = propDiffMat3/h  

matchMat.k3 = exp(- propDiffMat.k3^2 / 2)
matchMat.k3 <- matchMat.k3 / rowSums(matchMat.k3)

Anticoagulants120$kernel.wt = rep(0,nrow(Anticoagulants120))
Anticoagulants120$kernel.wt[ind.trt0] = colSums(matchMat.k3)
Anticoagulants120$kernel.wt[ind.trt1] = 1

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps, Anticoagulants120$kernel.wt, "kernel.Anticoagulants120")
outcome.model(Y="outcome1", Anticoagulants120$kernel.wt, df=Anticoagulants120, study=Anticoagulants.study, name="kernel.Anticoagulants120")

outcome.model(Y="Death", Anticoagulants120$kernel.wt, df=Anticoagulants120, study=Anticoagulants.study, name="kernel.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants120$kernel.wt, df=Anticoagulants120, study=Anticoagulants.study, name="kernel.Anticoagulants120")
outcome.model(Y="ICU.admission", Anticoagulants120$kernel.wt, df=Anticoagulants120, study=Anticoagulants.study, name="kernel.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants120$kernel.wt, df=Anticoagulants120, study=Anticoagulants.study, name="kernel.Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps2, Anticoagulants120$kernel.wt2, "kernel.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants120$kernel.wt2, df=Anticoagulants120, study=Anticoagulants.study, name="kernel.Anticoagulants120")

ps_summary(Anticoagulants120$Anticoagulants, Anticoagulants120$ps3, Anticoagulants120$kernel.wt3, "kernel.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants120$kernel.wt3, df=Anticoagulants120, study=Anticoagulants.study, name="kernel.Anticoagulants120")

print_table(df.all=Anticoagulants120, wt=Anticoagulants120$kernel.wt, df.study=Anticoagulants.study, save_name="kernel.Anticoagulants120")

sink()
###############################################
############  All Anticoagulants 90    ###############
###############################################

Anticoagulants90.con <- Anticoagulants90[,Anticoagulants.confounder]
Anticoagulants90.fit <- glm(Anticoagulants90$Anticoagulants ~ ., data = Anticoagulants90.con, family="binomial")

Anticoagulants90$ps <- Anticoagulants90.fit$fitted.values
ind.trt0 <- which(Anticoagulants90$Anticoagulants == 0)
ind.trt1 <- which(Anticoagulants90$Anticoagulants == 1)

Anticoagulants90$iptw.wt <- ( 1/Anticoagulants90$ps )*Anticoagulants90$Anticoagulants + ( 1/(1-Anticoagulants90$ps) )*( 1-Anticoagulants90$Anticoagulants )

Anticoagulants90$iptw.att <- Anticoagulants90$ps/(1-Anticoagulants90$ps)
Anticoagulants90$iptw.att[ind.trt1] <- 1

#### Bleeding and Transfusion ####
Anticoagulants.scr1 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr1 <- glm(Anticoagulants90[,"bleeding.transfusion"]~Anticoagulants90[,c], data=Anticoagulants90, family = "binomial")
  if ( length(summary(fit.scr1)$coef) == 4) {
    print("ERROR!!, exposure coefficeints is NA.")
    next
  }
  if (summary(fit.scr1)$coef[2,4] < 0.2){
    cat("\n", summary(fit.scr1)$coef[2,4], "\n")
    Anticoagulants.scr1 <- c(Anticoagulants.scr1, c)
  }
}
cat("\n [Confounder after p-value scr1eening(0.2)] \n")
print(Anticoagulants.study[Anticoagulants.study$var %in% Anticoagulants.scr1, c(2,1)])

scr1 <- Anticoagulants90[,Anticoagulants.scr1]

#### myocardial.stroke.TIA.Thromboembolism ####
Anticoagulants.scr2 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr2 <- glm(Anticoagulants90[,"myocardial.storke.TIA.Thromboembolism"]~Anticoagulants90[,c], data=Anticoagulants90, family = "binomial")
  if ( length(summary(fit.scr2)$coef) == 4) {
    print("ERROR!!, exposure coefficeints is NA.")
    next
  }
  if (summary(fit.scr2)$coef[2,4] < 0.2){
    cat("\n", summary(fit.scr1)$coef[2,4], "\n")
    Anticoagulants.scr2 <- c(Anticoagulants.scr2, c)
  }
}
cat("\n [Confounder after p-value scr2eening(0.2)] \n")
print(Anticoagulants.study[Anticoagulants.study$var %in% Anticoagulants.scr2, c(2,1)])

scr2 <- Anticoagulants90[,Anticoagulants.scr2]

# [1] ps model for screening variable
# (a) exposure ~ confounders
fit1 <- glm(Anticoagulants90$Anticoagulants ~ ., data = scr1, family="binomial")
Anticoagulants90$ps2 <- fit1$fitted.values

Anticoagulants90$iptw.wt2 <- ( 1/Anticoagulants90$ps2 )*Anticoagulants90$Anticoagulants + ( 1/(1-Anticoagulants90$ps2) )*( 1-Anticoagulants90$Anticoagulants )

Anticoagulants90$iptw.att2 <- Anticoagulants90$ps2/(1-Anticoagulants90$ps2)
Anticoagulants90$iptw.att2[ind.trt1] <- 1

fit1 <- glm(Anticoagulants90$Anticoagulants ~ ., data = scr2, family="binomial")
Anticoagulants90$ps3 <- fit1$fitted.values

Anticoagulants90$iptw.wt3 <- ( 1/Anticoagulants90$ps3 )*Anticoagulants90$Anticoagulants + ( 1/(1-Anticoagulants90$ps3) )*( 1-Anticoagulants90$Anticoagulants )

Anticoagulants90$iptw.att3 <- Anticoagulants90$ps3/(1-Anticoagulants90$ps3)
Anticoagulants90$iptw.att3[ind.trt1] <- 1

#### code ####

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[ALL_IPTW]results_Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps, Anticoagulants90$iptw.wt, "Anticoagulants90")
outcome.model(Y="outcome1", wt=Anticoagulants90$iptw.wt, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants90") # -17

outcome.model(Y="Death", wt=Anticoagulants90$iptw.wt, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=Anticoagulants90$iptw.wt, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants90")
outcome.model(Y="ICU.admission", wt=Anticoagulants90$iptw.wt, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=Anticoagulants90$iptw.wt, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants90")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps2, Anticoagulants90$iptw.wt2, "Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants90$iptw.wt2, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants90_2")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps3, Anticoagulants90$iptw.wt3, "Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants90$iptw.wt3, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants90_3")

print_table(df.all=Anticoagulants90, wt=Anticoagulants90$iptw.wt, df.study=Anticoagulants.study, save_name="Anticoagulants90")
sink()

#### IPTW Trimming : ATE ####

sink("./resultsCNLLS/model/[ALL_IPTW_trim]results_Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

Anticoagulants90$trim.wt <- Anticoagulants90$iptw.wt
Anticoagulants90$trim.wt[(Anticoagulants90$ps<ps.cut | Anticoagulants90$ps>(1-ps.cut))] <- 0

Anticoagulants90$trim.wt2 <- Anticoagulants90$iptw.wt2
Anticoagulants90$trim.wt2[(Anticoagulants90$ps2<ps.cut | Anticoagulants90$ps2>(1-ps.cut))] <- 0

Anticoagulants90$trim.wt3 <- Anticoagulants90$iptw.wt3
Anticoagulants90$trim.wt3[(Anticoagulants90$ps3<ps.cut | Anticoagulants90$ps3>(1-ps.cut))] <- 0

print(nrow(Anticoagulants90[Anticoagulants90$trim.wt!=0,]))
print(nrow(Anticoagulants90[Anticoagulants90$trim.wt2!=0,]))
print(nrow(Anticoagulants90[Anticoagulants90$trim.wt3!=0,]))

ps_summary(Anticoagulants90$Anticoagulants[Anticoagulants90$trim.wt!=0], Anticoagulants90$ps[Anticoagulants90$trim.wt!=0], Anticoagulants90$trim.wt[Anticoagulants90$trim.wt!=0], "trim.Anticoagulants90")
outcome.model(Y="outcome1", wt=Anticoagulants90$trim.wt, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants90") # -17

outcome.model(Y="Death", wt=Anticoagulants90$trim.wt, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=Anticoagulants90$trim.wt, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants90")
outcome.model(Y="ICU.admission", wt=Anticoagulants90$trim.wt, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=Anticoagulants90$trim.wt, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants90")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps2, Anticoagulants90$trim.wt2, "trim.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants90$trim.wt2, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants90_2")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps3, Anticoagulants90$trim.wt3, "trim.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants90$trim.wt3, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants90_3")

print_table(df.all=Anticoagulants90, wt=Anticoagulants90$trim.wt, df.study=Anticoagulants.study, save_name="trim.Anticoagulants90")
sink()

#### IPTW Stabilized : ATE ####

sink("./resultsCNLLS/model/[ALL_IPTW_stabilized]results_Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.Anticoagulants <- sum(Anticoagulants90$Anticoagulants) / nrow(Anticoagulants90)
Anticoagulants90$st.iptw.wt1 <- ( p.Anticoagulants/Anticoagulants90$ps )*Anticoagulants90$Anticoagulants + ( (1-p.Anticoagulants)/(1-Anticoagulants90$ps) )*( 1-Anticoagulants90$Anticoagulants )
Anticoagulants90$st.iptw.wt2 <- ( p.Anticoagulants/Anticoagulants90$ps2 )*Anticoagulants90$Anticoagulants + ( (1-p.Anticoagulants)/(1-Anticoagulants90$ps2) )*( 1-Anticoagulants90$Anticoagulants )
Anticoagulants90$st.iptw.wt3 <- ( p.Anticoagulants/Anticoagulants90$ps3 )*Anticoagulants90$Anticoagulants + ( (1-p.Anticoagulants)/(1-Anticoagulants90$ps3) )*( 1-Anticoagulants90$Anticoagulants )

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps, Anticoagulants90$st.iptw.wt1, "stabilized.Anticoagulants90")

outcome.model(Y="outcome1", Anticoagulants90$st.iptw.wt1, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants90") # -15

outcome.model(Y="Death", Anticoagulants90$st.iptw.wt1, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants90$st.iptw.wt1, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants90")
outcome.model(Y="ICU.admission", Anticoagulants90$st.iptw.wt1, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants90$st.iptw.wt1, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants90")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps2, Anticoagulants90$st.iptw.wt2, "stabilized.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants90$st.iptw.wt2, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants90_2")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps3, Anticoagulants90$st.iptw.wt3, "stabilized.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants90$st.iptw.wt3, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants90_3")

print_table(df.all=Anticoagulants90, wt=Anticoagulants90$st.iptw.wt1, df.study=Anticoagulants.study, save_name="stabilized.Anticoagulants90")

sink()

#### IPTW : ATT ####

sink("./resultsCNLLS/model/[ALL_IPTW_ATT]results_Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps, Anticoagulants90$iptw.att, "Anticoagulants.att90")
outcome.model(Y="outcome1", Anticoagulants90$iptw.att, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants.att90") # -17

outcome.model(Y="Death", Anticoagulants90$iptw.att, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants90$iptw.att, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants.att90")
outcome.model(Y="ICU.admission", Anticoagulants90$iptw.att, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants90$iptw.att, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants.att90")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps2, Anticoagulants90$iptw.att2, "Anticoagulants.att90_2")
outcome.model(Y="bleeding.transfusion", Anticoagulants90$iptw.att2, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants.att90_2")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps3, Anticoagulants90$iptw.att3, "Anticoagulants.att90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", Anticoagulants90$iptw.att3, df=Anticoagulants90, study=Anticoagulants.study, name="Anticoagulants.att90_3")

print_table(df.all=Anticoagulants90, Anticoagulants90$iptw.att, df.study=Anticoagulants.study, save_name="Anticoagulants.att90")
sink()

#### IPTW Trimming : ATT ####
sink("./resultsCNLLS/model/[ALL_IPTW_trim_ATT]results_Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

Anticoagulants90$trim.att <- Anticoagulants90$iptw.att
Anticoagulants90$trim.att[(Anticoagulants90$ps<ps.cut | Anticoagulants90$ps>(1-ps.cut))] <- 0

Anticoagulants90$trim.att2 <- Anticoagulants90$iptw.att2
Anticoagulants90$trim.att2[(Anticoagulants90$ps2<ps.cut | Anticoagulants90$ps2>(1-ps.cut))] <- 0

Anticoagulants90$trim.att3 <- Anticoagulants90$iptw.att3
Anticoagulants90$trim.att3[(Anticoagulants90$ps3<ps.cut | Anticoagulants90$ps3>(1-ps.cut))] <- 0

print(nrow(Anticoagulants90[Anticoagulants90$trim.att!=0,]))
print(nrow(Anticoagulants90[Anticoagulants90$trim.att2!=0,]))
print(nrow(Anticoagulants90[Anticoagulants90$trim.att3!=0,]))

ps_summary(Anticoagulants90$Anticoagulants[Anticoagulants90$trim.att!=0], Anticoagulants90$ps[Anticoagulants90$trim.att!=0], Anticoagulants90$trim.att[Anticoagulants90$trim.att!=0], "trim.Anticoagulants.att90")
outcome.model(Y="outcome1", Anticoagulants90$trim.att, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants.att90") # -17

outcome.model(Y="Death", Anticoagulants90$trim.att, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants90$trim.att, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants.att90")
outcome.model(Y="ICU.admission", Anticoagulants90$trim.att, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants90$trim.att, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants.att90")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps2, Anticoagulants90$trim.att2, "trim.Anticoagulants.att90_2")
outcome.model(Y="bleeding.transfusion", Anticoagulants90$trim.att2, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants.att90_2")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps3, Anticoagulants90$trim.att3, "trim.Anticoagulants.att90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", Anticoagulants90$trim.att3, df=Anticoagulants90, study=Anticoagulants.study, name="trim.Anticoagulants.att90_3")

print_table(df.all=Anticoagulants90, Anticoagulants90$trim.att, df.study=Anticoagulants.study, save_name="trim.Anticoagulants.att90")
sink()

#### IPTW Stabilized : ATT ####

sink("./resultsCNLLS/model/[ALL_IPTW_stabilized_ATT]results_Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")

p.Anticoagulants <- sum(Anticoagulants90$Anticoagulants) / nrow(Anticoagulants90)

Anticoagulants90$st.iptw.att <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (Anticoagulants90$ps / (1-Anticoagulants90$ps) )  *  (1-Anticoagulants90$Anticoagulants)
Anticoagulants90$st.iptw.att[ind.trt1] <- 1

Anticoagulants90$st.iptw.att2 <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (Anticoagulants90$ps2 / (1-Anticoagulants90$ps2) )  *  (1-Anticoagulants90$Anticoagulants)
Anticoagulants90$st.iptw.att2[ind.trt1] <- 1

Anticoagulants90$st.iptw.att3 <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (Anticoagulants90$ps3 / (1-Anticoagulants90$ps3) )  *  (1-Anticoagulants90$Anticoagulants)
Anticoagulants90$st.iptw.att3[ind.trt1] <- 1

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps, Anticoagulants90$st.iptw.att, "stabilized.Anticoagulants.att90")

outcome.model(Y="outcome1", Anticoagulants90$st.iptw.att, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants.att90") # -15

outcome.model(Y="Death", Anticoagulants90$st.iptw.att, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants90$st.iptw.att, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants.att90")
outcome.model(Y="ICU.admission", Anticoagulants90$st.iptw.att, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants90$st.iptw.att, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants.att90")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps2, Anticoagulants90$st.iptw.att2, "stabilized.Anticoagulants.att90_2")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants90$st.iptw.att2, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants.att90_2")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps3, Anticoagulants90$st.iptw.att3, "stabilized.Anticoagulants.att90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants90$st.iptw.att3, df=Anticoagulants90, study=Anticoagulants.study, name="stabilized.Anticoagulants.att90_3")

print_table(df.all=Anticoagulants90, wt=Anticoagulants90$st.iptw.att, df.study=Anticoagulants.study, save_name="stabilized.Anticoagulants.att90")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[ALL_Deciles]results_Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

#(1)
Anticoagulants90$ps.dec <- cut(Anticoagulants90$ps, 
                               breaks=c(quantile(Anticoagulants90$ps, probs=seq(0,1,0.1))),
                               labels=seq(1:10), include.lowest=TRUE)
Anticoagulants90$dec_info <- paste0(Anticoagulants90$Anticoagulants, ":", Anticoagulants90$ps.dec)

Anticoagulants90.dec <- data.frame(table(Anticoagulants90$ps.dec, Anticoagulants90$Anticoagulants))
colnames(Anticoagulants90.dec) <- c("ps.dec", "Anticoagulants", "n")
Anticoagulants90.dec$dec_info <- paste0(Anticoagulants90.dec$Anticoagulants, ":", Anticoagulants90.dec$ps.dec)
Anticoagulants90.dec$n[Anticoagulants90.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
Anticoagulants90.dec$dec.wt <- 1/Anticoagulants90.dec$n

cat("\n (1) \n"); cat(Anticoagulants90.dec); cat("\n")
Anticoagulants90 <- merge(Anticoagulants90, Anticoagulants90.dec[,c("dec_info", "dec.wt")], by="dec_info")

#(2)
Anticoagulants90$ps.dec2 <- cut(Anticoagulants90$ps2, 
                                breaks=c(quantile(Anticoagulants90$ps2, probs=seq(0,1,0.1))),
                                labels=seq(1:10), include.lowest=TRUE)
Anticoagulants90$dec_info2 <- paste0(Anticoagulants90$Anticoagulants, ":", Anticoagulants90$ps.dec2)
Anticoagulants90.dec2 <- data.frame(table(Anticoagulants90$ps.dec2, Anticoagulants90$Anticoagulants))
colnames(Anticoagulants90.dec) <- c("ps.dec", "Anticoagulants", "n")
Anticoagulants90.dec$dec_info <- paste0(Anticoagulants90.dec2$Anticoagulants, ":", Anticoagulants90.dec2$ps.dec)
Anticoagulants90.dec2$n[Anticoagulants90.dec2$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
Anticoagulants90.dec2$dec.wt2 <- 1/Anticoagulants90.dec2$n

cat("\n (2) \n"); cat(Anticoagulants90.dec2); cat("\n")
Anticoagulants90 <- merge(Anticoagulants90, Anticoagulants90.dec2[,c("dec_info", "dec.wt2")], by="dec_info")

#(3)
Anticoagulants90$ps.dec3 <- cut(Anticoagulants90$ps3, 
                                breaks=c(quantile(Anticoagulants90$ps3, probs=seq(0,1,0.1))),
                                labels=seq(1:10), include.lowest=TRUE)
Anticoagulants90$dec_info3 <- paste0(Anticoagulants90$Anticoagulants, ":", Anticoagulants90$ps.dec3)
Anticoagulants90.dec3 <- data.frame(table(Anticoagulants90$ps.dec3, Anticoagulants90$Anticoagulants))
colnames(Anticoagulants90.dec) <- c("ps.dec", "Anticoagulants", "n")
Anticoagulants90.dec$dec_info <- paste0(Anticoagulants90.dec3$Anticoagulants, ":", Anticoagulants90.dec3$ps.dec)
Anticoagulants90.dec3$n[Anticoagulants90.dec2$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
Anticoagulants90.dec3$dec.wt3 <- 1/Anticoagulants90.dec3$n

cat("\n (3) \n"); cat(Anticoagulants90.dec3); cat("\n")
Anticoagulants90 <- merge(Anticoagulants90, Anticoagulants90.dec3[,c("dec_info", "dec.wt3")], by="dec_info")


ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps, Anticoagulants90$dec.wt, "dec.Anticoagulants90")
outcome.model(Y="outcome1", Anticoagulants90$dec.wt, df=Anticoagulants90, study=Anticoagulants.study, name="dec.Anticoagulants90") # -0.03765

outcome.model(Y="Death", Anticoagulants90$dec.wt, df=Anticoagulants90, study=Anticoagulants.study, name="dec.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants90$dec.wt, df=Anticoagulants90, study=Anticoagulants.study, name="dec.Anticoagulants90")
outcome.model(Y="ICU.admission", Anticoagulants90$dec.wt, df=Anticoagulants90, study=Anticoagulants.study, name="dec.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants90$dec.wt, df=Anticoagulants90, study=Anticoagulants.study, name="dec.Anticoagulants90")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps2, Anticoagulants90$dec.wt2, "dec.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants90$dec.wt2, df=Anticoagulants90, study=Anticoagulants.study, name="dec.Anticoagulants90_2")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps3, Anticoagulants90$dec.wt3, "dec.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants90$dec.wt3, df=Anticoagulants90, study=Anticoagulants.study, name="dec.Anticoagulants90_3")

print_table(df.all=Anticoagulants90, wt=Anticoagulants90$dec.wt, df.study=Anticoagulants.study, save_name="dec.Anticoagulants90")

sink()

#### One to One Matching ####
sink("./resultsCNLLS/model/[ALL_Matching]results_Anticoagulants90.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
# (1)
trt0.ps = Anticoagulants90$ps[ind.trt0]
trt1.ps = Anticoagulants90$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
Anticoagulants90$match.wt = rep(0,nrow(Anticoagulants90))
Anticoagulants90$match.wt[ind.trt1] = 1
Anticoagulants90$match.wt[ind.trt0] = colSums(matchMat)

# (2)
trt0.ps2 = Anticoagulants90$ps2[ind.trt0]
trt1.ps2 = Anticoagulants90$ps2[ind.trt1]

propDiffMat2 = outer(trt1.ps2, trt0.ps2, "-")
propDiffMat2 = abs(propDiffMat2)

# index 를 이름으로 붙여주기
rownames(propDiffMat2) = ind.trt1
colnames(propDiffMat2) = ind.trt0

matchMat2 = t( apply(propDiffMat2, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
Anticoagulants90$match.wt2 = rep(0,nrow(Anticoagulants90))
Anticoagulants90$match.wt2[ind.trt1] = 1
Anticoagulants90$match.wt2[ind.trt0] = colSums(matchMat2)

# (3)
trt0.ps3 = Anticoagulants90$ps3[ind.trt0]
trt1.ps3 = Anticoagulants90$ps3[ind.trt1]

propDiffMat3 = outer(trt1.ps3, trt0.ps3, "-")
propDiffMat3 = abs(propDiffMat3)

# index 를 이름으로 붙여주기
rownames(propDiffMat3) = ind.trt1
colnames(propDiffMat3) = ind.trt0

matchMat3 = t( apply(propDiffMat3, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
Anticoagulants90$match.wt3 = rep(0,nrow(Anticoagulants90))
Anticoagulants90$match.wt3[ind.trt1] = 1
Anticoagulants90$match.wt3[ind.trt0] = colSums(matchMat3)

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps, Anticoagulants90$match.wt, "match.Anticoagulants90")
outcome.model(Y="outcome1", Anticoagulants90$match.wt, df=Anticoagulants90, study=Anticoagulants.study, name="match.Anticoagulants90") # 1.6468

outcome.model(Y="Death", Anticoagulants90$match.wt, df=Anticoagulants90, study=Anticoagulants.study, name="match.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants90$match.wt, df=Anticoagulants90, study=Anticoagulants.study, name="match.Anticoagulants90")
outcome.model(Y="ICU.admission", Anticoagulants90$match.wt, df=Anticoagulants90, study=Anticoagulants.study, name="match.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants90$match.wt, df=Anticoagulants90, study=Anticoagulants.study, name="match.Anticoagulants90")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps2, Anticoagulants90$match.wt2, "match.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants90$match.wt2, df=Anticoagulants90, study=Anticoagulants.study, name="match.Anticoagulants90_2")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps3, Anticoagulants90$match.wt3, "match.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants90$match.wt3, df=Anticoagulants90, study=Anticoagulants.study, name="match.Anticoagulants90_3")

print_table(df.all=Anticoagulants90, wt=Anticoagulants90$match.wt, df.study=Anticoagulants.study, save_name="match.Anticoagulants90")

sink()

#### Kernel Matching ####

sink("./resultsCNLLS/model/[ALL_Kernel]results_Anticoagulants90.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")

#(1)
propDiffMat.k = propDiffMat/h  

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

Anticoagulants90$kernel.wt = rep(0,nrow(Anticoagulants90))
Anticoagulants90$kernel.wt[ind.trt0] = colSums(matchMat.k)
Anticoagulants90$kernel.wt[ind.trt1] = 1

#(2)
propDiffMat.k2 = propDiffMat2/h  

matchMat.k2 = exp(- propDiffMat.k2^2 / 2)
matchMat.k2 <- matchMat.k2 / rowSums(matchMat.k2)

Anticoagulants90$kernel.wt = rep(0,nrow(Anticoagulants90))
Anticoagulants90$kernel.wt[ind.trt0] = colSums(matchMat.k2)
Anticoagulants90$kernel.wt[ind.trt1] = 1

#(3)
propDiffMat.k3 = propDiffMat3/h  

matchMat.k3 = exp(- propDiffMat.k3^2 / 2)
matchMat.k3 <- matchMat.k3 / rowSums(matchMat.k3)

Anticoagulants90$kernel.wt = rep(0,nrow(Anticoagulants90))
Anticoagulants90$kernel.wt[ind.trt0] = colSums(matchMat.k3)
Anticoagulants90$kernel.wt[ind.trt1] = 1

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps, Anticoagulants90$kernel.wt, "kernel.Anticoagulants90")
outcome.model(Y="outcome1", Anticoagulants90$kernel.wt, df=Anticoagulants90, study=Anticoagulants.study, name="kernel.Anticoagulants90")

outcome.model(Y="Death", Anticoagulants90$kernel.wt, df=Anticoagulants90, study=Anticoagulants.study, name="kernel.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", Anticoagulants90$kernel.wt, df=Anticoagulants90, study=Anticoagulants.study, name="kernel.Anticoagulants90")
outcome.model(Y="ICU.admission", Anticoagulants90$kernel.wt, df=Anticoagulants90, study=Anticoagulants.study, name="kernel.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", Anticoagulants90$kernel.wt, df=Anticoagulants90, study=Anticoagulants.study, name="kernel.Anticoagulants90")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps2, Anticoagulants90$kernel.wt2, "kernel.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=Anticoagulants90$kernel.wt2, df=Anticoagulants90, study=Anticoagulants.study, name="kernel.Anticoagulants90_2")

ps_summary(Anticoagulants90$Anticoagulants, Anticoagulants90$ps3, Anticoagulants90$kernel.wt3, "kernel.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=Anticoagulants90$kernel.wt3, df=Anticoagulants90, study=Anticoagulants.study, name="kernel.Anticoagulants90_3")

print_table(df.all=Anticoagulants90, wt=Anticoagulants90$kernel.wt, df.study=Anticoagulants.study, save_name="kernel.Anticoagulants90")

sink()

###############################################
############  Hos Anticoagulants 120    ###############
###############################################

hos.Anticoagulants120.con <- hos.Anticoagulants120[,Anticoagulants.confounder]
hos.Anticoagulants120.fit <- glm(hos.Anticoagulants120$Anticoagulants ~ ., data = hos.Anticoagulants120.con, family="binomial")

hos.Anticoagulants120$ps <- hos.Anticoagulants120.fit$fitted.values
ind.trt0 <- which(hos.Anticoagulants120$Anticoagulants == 0)
ind.trt1 <- which(hos.Anticoagulants120$Anticoagulants == 1)

hos.Anticoagulants120$iptw.wt <- ( 1/hos.Anticoagulants120$ps )*hos.Anticoagulants120$Anticoagulants + ( 1/(1-hos.Anticoagulants120$ps) )*( 1-hos.Anticoagulants120$Anticoagulants )

hos.Anticoagulants120$iptw.att <- hos.Anticoagulants120$ps/(1-hos.Anticoagulants120$ps)
hos.Anticoagulants120$iptw.att[ind.trt1] <- 1

#### Bleeding and Transfusion ####
Anticoagulants.scr1 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr1 <- glm(hos.Anticoagulants120[,"bleeding.transfusion"]~hos.Anticoagulants120[,c], data=hos.Anticoagulants120, family = "binomial")
  if ( length(summary(fit.scr1)$coef) == 4) {
    print("ERROR!!, exposure coefficeints is NA.")
    next
  }
  if (summary(fit.scr1)$coef[2,4] < 0.2){
    cat("\n", summary(fit.scr1)$coef[2,4], "\n")
    Anticoagulants.scr1 <- c(Anticoagulants.scr1, c)
  }
}
cat("\n [Confounder after p-value scr1eening(0.2)] \n")
print(Anticoagulants.study[Anticoagulants.study$var %in% Anticoagulants.scr1, c(2,1)])

scr1 <- hos.Anticoagulants120[,Anticoagulants.scr1]

#### myocardial.stroke.TIA.Thromboembolism ####
Anticoagulants.scr2 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr2 <- glm(hos.Anticoagulants120[,"myocardial.storke.TIA.Thromboembolism"]~hos.Anticoagulants120[,c], data=hos.Anticoagulants120, family = "binomial")
  if ( length(summary(fit.scr2)$coef) == 4) {
    print("ERROR!!, exposure coefficeints is NA.")
    next
  }
  if (summary(fit.scr2)$coef[2,4] < 0.2){
    cat("\n", summary(fit.scr1)$coef[2,4], "\n")
    Anticoagulants.scr2 <- c(Anticoagulants.scr2, c)
  }
}
cat("\n [Confounder after p-value scr2eening(0.2)] \n")
print(Anticoagulants.study[Anticoagulants.study$var %in% Anticoagulants.scr2, c(2,1)])

scr2 <- hos.Anticoagulants120[,Anticoagulants.scr2]

# [1] ps model for screening variable
# (a) exposure ~ confounders
fit1 <- glm(hos.Anticoagulants120$Anticoagulants ~ ., data = scr1, family="binomial")
hos.Anticoagulants120$ps2 <- fit1$fitted.values

hos.Anticoagulants120$iptw.wt2 <- ( 1/hos.Anticoagulants120$ps2 )*hos.Anticoagulants120$Anticoagulants + ( 1/(1-hos.Anticoagulants120$ps2) )*( 1-hos.Anticoagulants120$Anticoagulants )

hos.Anticoagulants120$iptw.att2 <- hos.Anticoagulants120$ps2/(1-hos.Anticoagulants120$ps2)
hos.Anticoagulants120$iptw.att2[ind.trt1] <- 1

fit1 <- glm(hos.Anticoagulants120$Anticoagulants ~ ., data = scr2, family="binomial")
hos.Anticoagulants120$ps3 <- fit1$fitted.values

hos.Anticoagulants120$iptw.wt3 <- ( 1/hos.Anticoagulants120$ps3 )*hos.Anticoagulants120$Anticoagulants + ( 1/(1-hos.Anticoagulants120$ps3) )*( 1-hos.Anticoagulants120$Anticoagulants )

hos.Anticoagulants120$iptw.att3 <- hos.Anticoagulants120$ps3/(1-hos.Anticoagulants120$ps3)
hos.Anticoagulants120$iptw.att3[ind.trt1] <- 1

#### code ####

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW]results_hos.Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps, hos.Anticoagulants120$iptw.wt, "hos.Anticoagulants120")
outcome.model(Y="outcome1", wt=hos.Anticoagulants120$iptw.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants120") # -17

outcome.model(Y="Death", wt=hos.Anticoagulants120$iptw.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.Anticoagulants120$iptw.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants120")
outcome.model(Y="ICU.admission", wt=hos.Anticoagulants120$iptw.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.Anticoagulants120$iptw.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps2, hos.Anticoagulants120$iptw.wt2, "hos.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants120$iptw.wt2, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps3, hos.Anticoagulants120$iptw.wt3, "hos.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants120$iptw.wt3, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants120")

print_table(hos.Anticoagulants120, wt=hos.Anticoagulants120$iptw.wt, df.study=Anticoagulants.study, save_name="hos.Anticoagulants120")
sink()

#### IPTW Trimming : ATE ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim]results_hos.Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.Anticoagulants120$trim.wt <- hos.Anticoagulants120$iptw.wt
hos.Anticoagulants120$trim.wt[(hos.Anticoagulants120$ps<ps.cut | hos.Anticoagulants120$ps>(1-ps.cut))] <- 0

hos.Anticoagulants120$trim.wt2 <- hos.Anticoagulants120$iptw.wt2
hos.Anticoagulants120$trim.wt2[(hos.Anticoagulants120$ps2<ps.cut | hos.Anticoagulants120$ps2>(1-ps.cut))] <- 0

hos.Anticoagulants120$trim.wt3 <- hos.Anticoagulants120$iptw.wt3
hos.Anticoagulants120$trim.wt3[(hos.Anticoagulants120$ps3<ps.cut | hos.Anticoagulants120$ps3>(1-ps.cut))] <- 0

print(nrow(hos.Anticoagulants120[hos.Anticoagulants120$trim.wt!=0,]))
print(nrow(hos.Anticoagulants120[hos.Anticoagulants120$trim.wt2!=0,]))
print(nrow(hos.Anticoagulants120[hos.Anticoagulants120$trim.wt3!=0,]))

ps_summary(hos.Anticoagulants120$Anticoagulants[hos.Anticoagulants120$trim.wt!=0], hos.Anticoagulants120$ps[hos.Anticoagulants120$trim.wt!=0], hos.Anticoagulants120$trim.wt[hos.Anticoagulants120$trim.wt!=0], "trim.hos.Anticoagulants120")
outcome.model(Y="outcome1", wt=hos.Anticoagulants120$trim.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants120") # -17

outcome.model(Y="Death", wt=hos.Anticoagulants120$trim.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.Anticoagulants120$trim.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants120")
outcome.model(Y="ICU.admission", wt=hos.Anticoagulants120$trim.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.Anticoagulants120$trim.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps2, hos.Anticoagulants120$trim.wt2, "trim.hos.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants120$trim.wt2, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps3, hos.Anticoagulants120$trim.wt3, "trim.hos.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants120$trim.wt3, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants120")

print_table(hos.Anticoagulants120, wt=hos.Anticoagulants120$trim.wt, df.study=Anticoagulants.study, save_name="trim.hos.Anticoagulants120")
sink()

#### IPTW Stabilized : ATE ####

sink("./resultsCNLLS/model/[Hos_IPTW_stabilized]results_hos.Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.Anticoagulants <- sum(hos.Anticoagulants120$Anticoagulants) / nrow(hos.Anticoagulants120)
hos.Anticoagulants120$st.iptw.wt1 <- ( p.Anticoagulants/hos.Anticoagulants120$ps )*hos.Anticoagulants120$Anticoagulants + ( (1-p.Anticoagulants)/(1-hos.Anticoagulants120$ps) )*( 1-hos.Anticoagulants120$Anticoagulants )
hos.Anticoagulants120$st.iptw.wt2 <- ( p.Anticoagulants/hos.Anticoagulants120$ps2 )*hos.Anticoagulants120$Anticoagulants + ( (1-p.Anticoagulants)/(1-hos.Anticoagulants120$ps2) )*( 1-hos.Anticoagulants120$Anticoagulants )
hos.Anticoagulants120$st.iptw.wt3 <- ( p.Anticoagulants/hos.Anticoagulants120$ps3 )*hos.Anticoagulants120$Anticoagulants + ( (1-p.Anticoagulants)/(1-hos.Anticoagulants120$ps3) )*( 1-hos.Anticoagulants120$Anticoagulants )

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps, hos.Anticoagulants120$st.iptw.wt1, "stabilized.hos.Anticoagulants120")

outcome.model(Y="outcome1", hos.Anticoagulants120$st.iptw.wt1, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants120") # -15

outcome.model(Y="Death", hos.Anticoagulants120$st.iptw.wt1, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants120$st.iptw.wt1, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants120")
outcome.model(Y="ICU.admission", hos.Anticoagulants120$st.iptw.wt1, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants120$st.iptw.wt1, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps2, hos.Anticoagulants120$st.iptw.wt2, "stabilized.hos.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants120$st.iptw.wt2, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps3, hos.Anticoagulants120$st.iptw.wt3, "stabilized.hos.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants120$st.iptw.wt3, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants120")

print_table(hos.Anticoagulants120, wt=hos.Anticoagulants120$st.iptw.wt1, df.study=Anticoagulants.study, save_name="stabilized.hos.Anticoagulants120")

sink()

#### IPTW : ATT ####

sink("./resultsCNLLS/model/[Hos_IPTW_ATT]results_hos.Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps, hos.Anticoagulants120$iptw.att, "hos.Anticoagulants.att120")
outcome.model(Y="outcome1", hos.Anticoagulants120$iptw.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants.att120") # -17

outcome.model(Y="Death", hos.Anticoagulants120$iptw.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants120$iptw.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants.att120")
outcome.model(Y="ICU.admission", hos.Anticoagulants120$iptw.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants120$iptw.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants.att120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps2, hos.Anticoagulants120$iptw.att2, "hos.Anticoagulants.att120")
outcome.model(Y="bleeding.transfusion", hos.Anticoagulants120$iptw.att2, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants.att120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps3, hos.Anticoagulants120$iptw.att3, "hos.Anticoagulants.att120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", hos.Anticoagulants120$iptw.att3, df=hos.Anticoagulants120, study=Anticoagulants.study, name="hos.Anticoagulants.att120")

print_table(hos.Anticoagulants120, hos.Anticoagulants120$iptw.att, df.study=Anticoagulants.study, save_name="hos.Anticoagulants.att120")
sink()

#### IPTW Trimming : ATT ####
sink("./resultsCNLLS/model/[Hos_IPTW_trim_ATT]results_hos.Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.Anticoagulants120$trim.att <- hos.Anticoagulants120$iptw.att
hos.Anticoagulants120$trim.att[(hos.Anticoagulants120$ps<ps.cut | hos.Anticoagulants120$ps>(1-ps.cut))] <- 0

hos.Anticoagulants120$trim.att2 <- hos.Anticoagulants120$iptw.att2
hos.Anticoagulants120$trim.att2[(hos.Anticoagulants120$ps2<ps.cut | hos.Anticoagulants120$ps2>(1-ps.cut))] <- 0

hos.Anticoagulants120$trim.att3 <- hos.Anticoagulants120$iptw.att3
hos.Anticoagulants120$trim.att3[(hos.Anticoagulants120$ps3<ps.cut | hos.Anticoagulants120$ps3>(1-ps.cut))] <- 0

print(nrow(hos.Anticoagulants120[hos.Anticoagulants120$trim.att!=0,]))
print(nrow(hos.Anticoagulants120[hos.Anticoagulants120$trim.att2!=0,]))
print(nrow(hos.Anticoagulants120[hos.Anticoagulants120$trim.att3!=0,]))

ps_summary(hos.Anticoagulants120$Anticoagulants[hos.Anticoagulants120$trim.att!=0], hos.Anticoagulants120$ps[hos.Anticoagulants120$trim.att!=0], hos.Anticoagulants120$trim.att[hos.Anticoagulants120$trim.att!=0], "trim.hos.Anticoagulants.att120")
outcome.model(Y="outcome1", hos.Anticoagulants120$trim.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att120") # -17

outcome.model(Y="Death", hos.Anticoagulants120$trim.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants120$trim.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att120")
outcome.model(Y="ICU.admission", hos.Anticoagulants120$trim.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants120$trim.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps2, hos.Anticoagulants120$trim.att2, "trim.hos.Anticoagulants.att120")
outcome.model(Y="bleeding.transfusion", hos.Anticoagulants120$trim.att2, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps3, hos.Anticoagulants120$trim.att3, "trim.hos.Anticoagulants.att120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", hos.Anticoagulants120$trim.att3, df=hos.Anticoagulants120, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att120")

print_table(hos.Anticoagulants120, hos.Anticoagulants120$trim.att, df.study=Anticoagulants.study, save_name="trim.hos.Anticoagulants.att120")
sink()

#### IPTW Stabilized : ATT ####

sink("./resultsCNLLS/model/[Hos_IPTW_stabilized_ATT]results_hos.Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")

p.Anticoagulants <- sum(hos.Anticoagulants120$Anticoagulants) / nrow(hos.Anticoagulants120)

hos.Anticoagulants120$st.iptw.att <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (hos.Anticoagulants120$ps / (1-hos.Anticoagulants120$ps) )  *  (1-hos.Anticoagulants120$Anticoagulants)
hos.Anticoagulants120$st.iptw.att[ind.trt1] <- 1

hos.Anticoagulants120$st.iptw.att2 <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (hos.Anticoagulants120$ps2 / (1-hos.Anticoagulants120$ps2) )  *  (1-hos.Anticoagulants120$Anticoagulants)
hos.Anticoagulants120$st.iptw.att2[ind.trt1] <- 1

hos.Anticoagulants120$st.iptw.att3 <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (hos.Anticoagulants120$ps3 / (1-hos.Anticoagulants120$ps3) )  *  (1-hos.Anticoagulants120$Anticoagulants)
hos.Anticoagulants120$st.iptw.att3[ind.trt1] <- 1

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps, hos.Anticoagulants120$st.iptw.att, "stabilized.hos.Anticoagulants.att120")

outcome.model(Y="outcome1", hos.Anticoagulants120$st.iptw.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att120") # -15

outcome.model(Y="Death", hos.Anticoagulants120$st.iptw.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants120$st.iptw.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att120")
outcome.model(Y="ICU.admission", hos.Anticoagulants120$st.iptw.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants120$st.iptw.att, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps2, hos.Anticoagulants120$st.iptw.att2, "stabilized.hos.Anticoagulants.att120")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants120$st.iptw.att2, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps3, hos.Anticoagulants120$st.iptw.att3, "stabilized.hos.Anticoagulants.att120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants120$st.iptw.att3, df=hos.Anticoagulants120, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att120")

print_table(hos.Anticoagulants120, wt=hos.Anticoagulants120$st.iptw.att, df.study=Anticoagulants.study, save_name="stabilized.hos.Anticoagulants.att120")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[Hos_Deciles]results_hos.Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

#(1)
hos.Anticoagulants120$ps.dec <- cut(hos.Anticoagulants120$ps, 
                                    breaks=c(quantile(hos.Anticoagulants120$ps, probs=seq(0,1,0.1))),
                                    labels=seq(1:10), include.lowest=TRUE)
hos.Anticoagulants120$dec_info <- paste0(hos.Anticoagulants120$Anticoagulants, ":", hos.Anticoagulants120$ps.dec)

hos.Anticoagulants120.dec <- data.frame(table(hos.Anticoagulants120$ps.dec, hos.Anticoagulants120$Anticoagulants))
colnames(hos.Anticoagulants120.dec) <- c("ps.dec", "Anticoagulants", "n")
hos.Anticoagulants120.dec$dec_info <- paste0(hos.Anticoagulants120.dec$Anticoagulants, ":", hos.Anticoagulants120.dec$ps.dec)
hos.Anticoagulants120.dec$n[hos.Anticoagulants120.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.Anticoagulants120.dec$dec.wt <- 1/hos.Anticoagulants120.dec$n

cat("\n (1) \n"); cat(hos.Anticoagulants120.dec); cat("\n")
hos.Anticoagulants120 <- merge(hos.Anticoagulants120, hos.Anticoagulants120.dec[,c("dec_info", "dec.wt")], by="dec_info")

#(2)
hos.Anticoagulants120$ps.dec2 <- cut(hos.Anticoagulants120$ps2, 
                                     breaks=c(quantile(hos.Anticoagulants120$ps2, probs=seq(0,1,0.1))),
                                     labels=seq(1:10), include.lowest=TRUE)
hos.Anticoagulants120$dec_info2 <- paste0(hos.Anticoagulants120$Anticoagulants, ":", hos.Anticoagulants120$ps.dec2)
hos.Anticoagulants120.dec2 <- data.frame(table(hos.Anticoagulants120$ps.dec2, hos.Anticoagulants120$Anticoagulants))
colnames(hos.Anticoagulants120.dec) <- c("ps.dec", "Anticoagulants", "n")
hos.Anticoagulants120.dec$dec_info <- paste0(hos.Anticoagulants120.dec2$Anticoagulants, ":", hos.Anticoagulants120.dec2$ps.dec)
hos.Anticoagulants120.dec2$n[hos.Anticoagulants120.dec2$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.Anticoagulants120.dec2$dec.wt2 <- 1/hos.Anticoagulants120.dec2$n

cat("\n (2) \n"); cat(hos.Anticoagulants120.dec2); cat("\n")
hos.Anticoagulants120 <- merge(hos.Anticoagulants120, hos.Anticoagulants120.dec2[,c("dec_info", "dec.wt2")], by="dec_info")

#(3)
hos.Anticoagulants120$ps.dec3 <- cut(hos.Anticoagulants120$ps3, 
                                     breaks=c(quantile(hos.Anticoagulants120$ps3, probs=seq(0,1,0.1))),
                                     labels=seq(1:10), include.lowest=TRUE)
hos.Anticoagulants120$dec_info3 <- paste0(hos.Anticoagulants120$Anticoagulants, ":", hos.Anticoagulants120$ps.dec3)
hos.Anticoagulants120.dec3 <- data.frame(table(hos.Anticoagulants120$ps.dec3, hos.Anticoagulants120$Anticoagulants))
colnames(hos.Anticoagulants120.dec) <- c("ps.dec", "Anticoagulants", "n")
hos.Anticoagulants120.dec$dec_info <- paste0(hos.Anticoagulants120.dec3$Anticoagulants, ":", hos.Anticoagulants120.dec3$ps.dec)
hos.Anticoagulants120.dec3$n[hos.Anticoagulants120.dec2$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.Anticoagulants120.dec3$dec.wt3 <- 1/hos.Anticoagulants120.dec3$n

cat("\n (3) \n"); cat(hos.Anticoagulants120.dec3); cat("\n")
hos.Anticoagulants120 <- merge(hos.Anticoagulants120, hos.Anticoagulants120.dec3[,c("dec_info", "dec.wt3")], by="dec_info")


ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps, hos.Anticoagulants120$dec.wt, "dec.hos.Anticoagulants120")
outcome.model(Y="outcome1", hos.Anticoagulants120$dec.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="dec.hos.Anticoagulants120") # -0.03765

outcome.model(Y="Death", hos.Anticoagulants120$dec.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="dec.hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants120$dec.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="dec.hos.Anticoagulants120")
outcome.model(Y="ICU.admission", hos.Anticoagulants120$dec.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="dec.hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants120$dec.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="dec.hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps2, hos.Anticoagulants120$dec.wt2, "dec.hos.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants120$dec.wt2, df=hos.Anticoagulants120, study=Anticoagulants.study, name="dec.hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps3, hos.Anticoagulants120$dec.wt3, "dec.hos.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants120$dec.wt3, df=hos.Anticoagulants120, study=Anticoagulants.study, name="dec.hos.Anticoagulants120")

print_table(hos.Anticoagulants120, wt=hos.Anticoagulants120$dec.wt, df.study=Anticoagulants.study, save_name="dec.hos.Anticoagulants120")

sink()

#### One to One Matching ####
sink("./resultsCNLLS/model/[Hos_Matching]results_hos.Anticoagulants120.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
# (1)
trt0.ps = hos.Anticoagulants120$ps[ind.trt0]
trt1.ps = hos.Anticoagulants120$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.Anticoagulants120$match.wt = rep(0,nrow(hos.Anticoagulants120))
hos.Anticoagulants120$match.wt[ind.trt1] = 1
hos.Anticoagulants120$match.wt[ind.trt0] = colSums(matchMat)

# (2)
trt0.ps2 = hos.Anticoagulants120$ps2[ind.trt0]
trt1.ps2 = hos.Anticoagulants120$ps2[ind.trt1]

propDiffMat2 = outer(trt1.ps2, trt0.ps2, "-")
propDiffMat2 = abs(propDiffMat2)

# index 를 이름으로 붙여주기
rownames(propDiffMat2) = ind.trt1
colnames(propDiffMat2) = ind.trt0

matchMat2 = t( apply(propDiffMat2, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.Anticoagulants120$match.wt2 = rep(0,nrow(hos.Anticoagulants120))
hos.Anticoagulants120$match.wt2[ind.trt1] = 1
hos.Anticoagulants120$match.wt2[ind.trt0] = colSums(matchMat2)

# (3)
trt0.ps3 = hos.Anticoagulants120$ps3[ind.trt0]
trt1.ps3 = hos.Anticoagulants120$ps3[ind.trt1]

propDiffMat3 = outer(trt1.ps3, trt0.ps3, "-")
propDiffMat3 = abs(propDiffMat3)

# index 를 이름으로 붙여주기
rownames(propDiffMat3) = ind.trt1
colnames(propDiffMat3) = ind.trt0

matchMat3 = t( apply(propDiffMat3, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.Anticoagulants120$match.wt3 = rep(0,nrow(hos.Anticoagulants120))
hos.Anticoagulants120$match.wt3[ind.trt1] = 1
hos.Anticoagulants120$match.wt3[ind.trt0] = colSums(matchMat3)

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps, hos.Anticoagulants120$match.wt, "match.hos.Anticoagulants120")
outcome.model(Y="outcome1", hos.Anticoagulants120$match.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="match.hos.Anticoagulants120") # 1.6468

outcome.model(Y="Death", hos.Anticoagulants120$match.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="match.hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants120$match.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="match.hos.Anticoagulants120")
outcome.model(Y="ICU.admission", hos.Anticoagulants120$match.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="match.hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants120$match.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="match.hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps2, hos.Anticoagulants120$match.wt2, "match.hos.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants120$match.wt2, df=hos.Anticoagulants120, study=Anticoagulants.study, name="match.hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps3, hos.Anticoagulants120$match.wt3, "match.hos.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants120$match.wt3, df=hos.Anticoagulants120, study=Anticoagulants.study, name="match.hos.Anticoagulants120")

print_table(hos.Anticoagulants120, wt=hos.Anticoagulants120$match.wt, df.study=Anticoagulants.study, save_name="match.hos.Anticoagulants120")

sink()

#### Kernel Matching ####

sink("./resultsCNLLS/model/[Hos_Kernel]results_hos.Anticoagulants120.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")

#(1)
propDiffMat.k = propDiffMat/h  

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

hos.Anticoagulants120$kernel.wt = rep(0,nrow(hos.Anticoagulants120))
hos.Anticoagulants120$kernel.wt[ind.trt0] = colSums(matchMat.k)
hos.Anticoagulants120$kernel.wt[ind.trt1] = 1

#(2)
propDiffMat.k2 = propDiffMat2/h  

matchMat.k2 = exp(- propDiffMat.k2^2 / 2)
matchMat.k2 <- matchMat.k2 / rowSums(matchMat.k2)

hos.Anticoagulants120$kernel.wt = rep(0,nrow(hos.Anticoagulants120))
hos.Anticoagulants120$kernel.wt[ind.trt0] = colSums(matchMat.k2)
hos.Anticoagulants120$kernel.wt[ind.trt1] = 1

#(3)
propDiffMat.k3 = propDiffMat3/h  

matchMat.k3 = exp(- propDiffMat.k3^2 / 2)
matchMat.k3 <- matchMat.k3 / rowSums(matchMat.k3)

hos.Anticoagulants120$kernel.wt = rep(0,nrow(hos.Anticoagulants120))
hos.Anticoagulants120$kernel.wt[ind.trt0] = colSums(matchMat.k3)
hos.Anticoagulants120$kernel.wt[ind.trt1] = 1

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps, hos.Anticoagulants120$kernel.wt, "kernel.hos.Anticoagulants120")
outcome.model(Y="outcome1", hos.Anticoagulants120$kernel.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="kernel.hos.Anticoagulants120")

outcome.model(Y="Death", hos.Anticoagulants120$kernel.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="kernel.hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants120$kernel.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="kernel.hos.Anticoagulants120")
outcome.model(Y="ICU.admission", hos.Anticoagulants120$kernel.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="kernel.hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants120$kernel.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="kernel.hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps2, hos.Anticoagulants120$kernel.wt2, "kernel.hos.Anticoagulants120")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants120$kernel.wt2, df=hos.Anticoagulants120, study=Anticoagulants.study, name="kernel.hos.Anticoagulants120")

ps_summary(hos.Anticoagulants120$Anticoagulants, hos.Anticoagulants120$ps3, hos.Anticoagulants120$kernel.wt3, "kernel.hos.Anticoagulants120")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants120$kernel.wt3, df=hos.Anticoagulants120, study=Anticoagulants.study, name="kernel.hos.Anticoagulants120")

print_table(hos.Anticoagulants120, wt=hos.Anticoagulants120$kernel.wt, df.study=Anticoagulants.study, save_name="kernel.hos.Anticoagulants120")

sink()
###############################################
############  Hos Anticoagulants 90    ###############
###############################################

hos.Anticoagulants90.con <- hos.Anticoagulants90[,Anticoagulants.confounder]
hos.Anticoagulants90.fit <- glm(hos.Anticoagulants90$Anticoagulants ~ ., data = hos.Anticoagulants90.con, family="binomial")

hos.Anticoagulants90$ps <- hos.Anticoagulants90.fit$fitted.values
ind.trt0 <- which(hos.Anticoagulants90$Anticoagulants == 0)
ind.trt1 <- which(hos.Anticoagulants90$Anticoagulants == 1)

hos.Anticoagulants90$iptw.wt <- ( 1/hos.Anticoagulants90$ps )*hos.Anticoagulants90$Anticoagulants + ( 1/(1-hos.Anticoagulants90$ps) )*( 1-hos.Anticoagulants90$Anticoagulants )

hos.Anticoagulants90$iptw.att <- hos.Anticoagulants90$ps/(1-hos.Anticoagulants90$ps)
hos.Anticoagulants90$iptw.att[ind.trt1] <- 1

#### Bleeding and Transfusion ####
Anticoagulants.scr1 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr1 <- glm(hos.Anticoagulants90[,"bleeding.transfusion"]~hos.Anticoagulants90[,c], data=hos.Anticoagulants90, family = "binomial")
  if ( length(summary(fit.scr1)$coef) == 4) {
    print("ERROR!!, exposure coefficeints is NA.")
    next
  }
  if (summary(fit.scr1)$coef[2,4] < 0.2){
    cat("\n", summary(fit.scr1)$coef[2,4], "\n")
    Anticoagulants.scr1 <- c(Anticoagulants.scr1, c)
  }
}
cat("\n [Confounder after p-value scr1eening(0.2)] \n")
print(Anticoagulants.study[Anticoagulants.study$var %in% Anticoagulants.scr1, c(2,1)])

scr1 <- hos.Anticoagulants90[,Anticoagulants.scr1]

#### myocardial.stroke.TIA.Thromboembolism ####
Anticoagulants.scr2 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr2 <- glm(hos.Anticoagulants90[,"myocardial.storke.TIA.Thromboembolism"]~hos.Anticoagulants90[,c], data=hos.Anticoagulants90, family = "binomial")
  if ( length(summary(fit.scr2)$coef) == 4) {
    print("ERROR!!, exposure coefficeints is NA.")
    next
  }
  if (summary(fit.scr2)$coef[2,4] < 0.2){
    cat("\n", summary(fit.scr1)$coef[2,4], "\n")
    Anticoagulants.scr2 <- c(Anticoagulants.scr2, c)
  }
}
cat("\n [Confounder after p-value scr2eening(0.2)] \n")
print(Anticoagulants.study[Anticoagulants.study$var %in% Anticoagulants.scr2, c(2,1)])

scr2 <- hos.Anticoagulants90[,Anticoagulants.scr2]

# [1] ps model for screening variable
# (a) exposure ~ confounders
fit1 <- glm(hos.Anticoagulants90$Anticoagulants ~ ., data = scr1, family="binomial")
hos.Anticoagulants90$ps2 <- fit1$fitted.values

hos.Anticoagulants90$iptw.wt2 <- ( 1/hos.Anticoagulants90$ps2 )*hos.Anticoagulants90$Anticoagulants + ( 1/(1-hos.Anticoagulants90$ps2) )*( 1-hos.Anticoagulants90$Anticoagulants )

hos.Anticoagulants90$iptw.att2 <- hos.Anticoagulants90$ps2/(1-hos.Anticoagulants90$ps2)
hos.Anticoagulants90$iptw.att2[ind.trt1] <- 1

fit1 <- glm(hos.Anticoagulants90$Anticoagulants ~ ., data = scr2, family="binomial")
hos.Anticoagulants90$ps3 <- fit1$fitted.values

hos.Anticoagulants90$iptw.wt3 <- ( 1/hos.Anticoagulants90$ps3 )*hos.Anticoagulants90$Anticoagulants + ( 1/(1-hos.Anticoagulants90$ps3) )*( 1-hos.Anticoagulants90$Anticoagulants )

hos.Anticoagulants90$iptw.att3 <- hos.Anticoagulants90$ps3/(1-hos.Anticoagulants90$ps3)
hos.Anticoagulants90$iptw.att3[ind.trt1] <- 1

#### code ####

#### IPTW : ATE ####
sink("./resultsCNLLS/model/[Hos_IPTW]results_hos.Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps, hos.Anticoagulants90$iptw.wt, "hos.Anticoagulants90")
outcome.model(Y="outcome1", wt=hos.Anticoagulants90$iptw.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants90") # -17

outcome.model(Y="Death", wt=hos.Anticoagulants90$iptw.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.Anticoagulants90$iptw.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants90")
outcome.model(Y="ICU.admission", wt=hos.Anticoagulants90$iptw.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.Anticoagulants90$iptw.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants90")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps2, hos.Anticoagulants90$iptw.wt2, "hos.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants90$iptw.wt2, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants90_2")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps3, hos.Anticoagulants90$iptw.wt3, "hos.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants90$iptw.wt3, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants90_3")

print_table(hos.Anticoagulants90, wt=hos.Anticoagulants90$iptw.wt, df.study=Anticoagulants.study, save_name="hos.Anticoagulants90")
sink()

#### IPTW Trimming : ATE ####

sink("./resultsCNLLS/model/[Hos_IPTW_trim]results_hos.Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.Anticoagulants90$trim.wt <- hos.Anticoagulants90$iptw.wt
hos.Anticoagulants90$trim.wt[(hos.Anticoagulants90$ps<ps.cut | hos.Anticoagulants90$ps>(1-ps.cut))] <- 0

hos.Anticoagulants90$trim.wt2 <- hos.Anticoagulants90$iptw.wt2
hos.Anticoagulants90$trim.wt2[(hos.Anticoagulants90$ps2<ps.cut | hos.Anticoagulants90$ps2>(1-ps.cut))] <- 0

hos.Anticoagulants90$trim.wt3 <- hos.Anticoagulants90$iptw.wt3
hos.Anticoagulants90$trim.wt3[(hos.Anticoagulants90$ps3<ps.cut | hos.Anticoagulants90$ps3>(1-ps.cut))] <- 0

print(nrow(hos.Anticoagulants90[hos.Anticoagulants90$trim.wt!=0,]))
print(nrow(hos.Anticoagulants90[hos.Anticoagulants90$trim.wt2!=0,]))
print(nrow(hos.Anticoagulants90[hos.Anticoagulants90$trim.wt3!=0,]))

ps_summary(hos.Anticoagulants90$Anticoagulants[hos.Anticoagulants90$trim.wt!=0], hos.Anticoagulants90$ps[hos.Anticoagulants90$trim.wt!=0], hos.Anticoagulants90$trim.wt[hos.Anticoagulants90$trim.wt!=0], "trim.hos.Anticoagulants90")
outcome.model(Y="outcome1", wt=hos.Anticoagulants90$trim.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants90") # -17

outcome.model(Y="Death", wt=hos.Anticoagulants90$trim.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.Anticoagulants90$trim.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants90")
outcome.model(Y="ICU.admission", wt=hos.Anticoagulants90$trim.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.Anticoagulants90$trim.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants90")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps2, hos.Anticoagulants90$trim.wt2, "trim.hos.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants90$trim.wt2, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants90_2")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps3, hos.Anticoagulants90$trim.wt3, "trim.hos.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants90$trim.wt3, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants90_3")

print_table(hos.Anticoagulants90, wt=hos.Anticoagulants90$trim.wt, df.study=Anticoagulants.study, save_name="trim.hos.Anticoagulants90")
sink()

#### IPTW Stabilized : ATE ####

sink("./resultsCNLLS/model/[Hos_IPTW_stabilized]results_hos.Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized \n") # -1.1
cat("------------------\n")
p.Anticoagulants <- sum(hos.Anticoagulants90$Anticoagulants) / nrow(hos.Anticoagulants90)
hos.Anticoagulants90$st.iptw.wt1 <- ( p.Anticoagulants/hos.Anticoagulants90$ps )*hos.Anticoagulants90$Anticoagulants + ( (1-p.Anticoagulants)/(1-hos.Anticoagulants90$ps) )*( 1-hos.Anticoagulants90$Anticoagulants )
hos.Anticoagulants90$st.iptw.wt2 <- ( p.Anticoagulants/hos.Anticoagulants90$ps2 )*hos.Anticoagulants90$Anticoagulants + ( (1-p.Anticoagulants)/(1-hos.Anticoagulants90$ps2) )*( 1-hos.Anticoagulants90$Anticoagulants )
hos.Anticoagulants90$st.iptw.wt3 <- ( p.Anticoagulants/hos.Anticoagulants90$ps3 )*hos.Anticoagulants90$Anticoagulants + ( (1-p.Anticoagulants)/(1-hos.Anticoagulants90$ps3) )*( 1-hos.Anticoagulants90$Anticoagulants )

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps, hos.Anticoagulants90$st.iptw.wt1, "stabilized.hos.Anticoagulants90")

outcome.model(Y="outcome1", hos.Anticoagulants90$st.iptw.wt1, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants90") # -15

outcome.model(Y="Death", hos.Anticoagulants90$st.iptw.wt1, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants90$st.iptw.wt1, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants90")
outcome.model(Y="ICU.admission", hos.Anticoagulants90$st.iptw.wt1, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants90$st.iptw.wt1, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants90")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps2, hos.Anticoagulants90$st.iptw.wt2, "stabilized.hos.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants90$st.iptw.wt2, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants90_2")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps3, hos.Anticoagulants90$st.iptw.wt3, "stabilized.hos.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants90$st.iptw.wt3, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants90_3")

print_table(hos.Anticoagulants90, wt=hos.Anticoagulants90$st.iptw.wt1, df.study=Anticoagulants.study, save_name="stabilized.hos.Anticoagulants90")

sink()

#### IPTW : ATT ####

sink("./resultsCNLLS/model/[Hos_IPTW_ATT]results_hos.Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("------------------\n")
cat(" [1] IPTW : ATT \n") # -1.1
cat("------------------\n")
cat(" - propensity score dist\n\n")
ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps, hos.Anticoagulants90$iptw.att, "hos.Anticoagulants.att90")
outcome.model(Y="outcome1", hos.Anticoagulants90$iptw.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants.att90") # -17

outcome.model(Y="Death", hos.Anticoagulants90$iptw.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants90$iptw.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants.att90")
outcome.model(Y="ICU.admission", hos.Anticoagulants90$iptw.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants90$iptw.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants.att90")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps2, hos.Anticoagulants90$iptw.att2, "hos.Anticoagulants.att90_2")
outcome.model(Y="bleeding.transfusion", hos.Anticoagulants90$iptw.att2, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants.att90_2")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps3, hos.Anticoagulants90$iptw.att3, "hos.Anticoagulants.att90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", hos.Anticoagulants90$iptw.att3, df=hos.Anticoagulants90, study=Anticoagulants.study, name="hos.Anticoagulants.att90_3")

print_table(hos.Anticoagulants90, hos.Anticoagulants90$iptw.att, df.study=Anticoagulants.study, save_name="hos.Anticoagulants.att90")
sink()

#### IPTW Trimming : ATT ####
sink("./resultsCNLLS/model/[Hos_IPTW_trim_ATT]results_hos.Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [2] IPTW ATT : with TRIMMING \n") # 0.32
cat("------------------\n")

hos.Anticoagulants90$trim.att <- hos.Anticoagulants90$iptw.att
hos.Anticoagulants90$trim.att[(hos.Anticoagulants90$ps<ps.cut | hos.Anticoagulants90$ps>(1-ps.cut))] <- 0

hos.Anticoagulants90$trim.att2 <- hos.Anticoagulants90$iptw.att2
hos.Anticoagulants90$trim.att2[(hos.Anticoagulants90$ps2<ps.cut | hos.Anticoagulants90$ps2>(1-ps.cut))] <- 0

hos.Anticoagulants90$trim.att3 <- hos.Anticoagulants90$iptw.att3
hos.Anticoagulants90$trim.att3[(hos.Anticoagulants90$ps3<ps.cut | hos.Anticoagulants90$ps3>(1-ps.cut))] <- 0

print(nrow(hos.Anticoagulants90[hos.Anticoagulants90$trim.att!=0,]))
print(nrow(hos.Anticoagulants90[hos.Anticoagulants90$trim.att2!=0,]))
print(nrow(hos.Anticoagulants90[hos.Anticoagulants90$trim.att3!=0,]))

ps_summary(hos.Anticoagulants90$Anticoagulants[hos.Anticoagulants90$trim.att!=0], hos.Anticoagulants90$ps[hos.Anticoagulants90$trim.att!=0], hos.Anticoagulants90$trim.att[hos.Anticoagulants90$trim.att!=0], "trim.hos.Anticoagulants.att90")
outcome.model(Y="outcome1", hos.Anticoagulants90$trim.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att90") # -17

outcome.model(Y="Death", hos.Anticoagulants90$trim.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants90$trim.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att90")
outcome.model(Y="ICU.admission", hos.Anticoagulants90$trim.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants90$trim.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att90")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps2, hos.Anticoagulants90$trim.att2, "trim.hos.Anticoagulants.att90_2")
outcome.model(Y="bleeding.transfusion", hos.Anticoagulants90$trim.att2, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att90_2")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps3, hos.Anticoagulants90$trim.att3, "trim.hos.Anticoagulants.att90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", hos.Anticoagulants90$trim.att3, df=hos.Anticoagulants90, study=Anticoagulants.study, name="trim.hos.Anticoagulants.att90_3")

print_table(hos.Anticoagulants90, hos.Anticoagulants90$trim.att, df.study=Anticoagulants.study, save_name="trim.hos.Anticoagulants.att90")
sink()

#### IPTW Stabilized : ATT ####

sink("./resultsCNLLS/model/[Hos_IPTW_stabilized_ATT]results_hos.Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [3] IPTW : Stabilized ATT \n") # -1.1
cat("------------------\n")

p.Anticoagulants <- sum(hos.Anticoagulants90$Anticoagulants) / nrow(hos.Anticoagulants90)

hos.Anticoagulants90$st.iptw.att <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (hos.Anticoagulants90$ps / (1-hos.Anticoagulants90$ps) )  *  (1-hos.Anticoagulants90$Anticoagulants)
hos.Anticoagulants90$st.iptw.att[ind.trt1] <- 1

hos.Anticoagulants90$st.iptw.att2 <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (hos.Anticoagulants90$ps2 / (1-hos.Anticoagulants90$ps2) )  *  (1-hos.Anticoagulants90$Anticoagulants)
hos.Anticoagulants90$st.iptw.att2[ind.trt1] <- 1

hos.Anticoagulants90$st.iptw.att3 <- ( (1-p.Anticoagulants)/p.Anticoagulants) * (hos.Anticoagulants90$ps3 / (1-hos.Anticoagulants90$ps3) )  *  (1-hos.Anticoagulants90$Anticoagulants)
hos.Anticoagulants90$st.iptw.att3[ind.trt1] <- 1

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps, hos.Anticoagulants90$st.iptw.att, "stabilized.hos.Anticoagulants.att90")

outcome.model(Y="outcome1", hos.Anticoagulants90$st.iptw.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att90") # -15

outcome.model(Y="Death", hos.Anticoagulants90$st.iptw.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants90$st.iptw.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att90")
outcome.model(Y="ICU.admission", hos.Anticoagulants90$st.iptw.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants90$st.iptw.att, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att90")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps2, hos.Anticoagulants90$st.iptw.att2, "stabilized.hos.Anticoagulants.att90_2")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants90$st.iptw.att2, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att90_2")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps3, hos.Anticoagulants90$st.iptw.att3, "stabilized.hos.Anticoagulants.att90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants90$st.iptw.att3, df=hos.Anticoagulants90, study=Anticoagulants.study, name="stabilized.hos.Anticoagulants.att90_3")

print_table(hos.Anticoagulants90, wt=hos.Anticoagulants90$st.iptw.att, df.study=Anticoagulants.study, save_name="stabilized.hos.Anticoagulants.att90")

sink()

#### Deciles Matching ####
sink("./resultsCNLLS/model/[Hos_Deciles]results_hos.Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 90] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])

cat("\n\n------------------\n")
cat(" [4] Matching : Deciles \n") # -0.072
cat("------------------\n")

#(1)
hos.Anticoagulants90$ps.dec <- cut(hos.Anticoagulants90$ps, 
                                   breaks=c(quantile(hos.Anticoagulants90$ps, probs=seq(0,1,0.1))),
                                   labels=seq(1:10), include.lowest=TRUE)
hos.Anticoagulants90$dec_info <- paste0(hos.Anticoagulants90$Anticoagulants, ":", hos.Anticoagulants90$ps.dec)

hos.Anticoagulants90.dec <- data.frame(table(hos.Anticoagulants90$ps.dec, hos.Anticoagulants90$Anticoagulants))
colnames(hos.Anticoagulants90.dec) <- c("ps.dec", "Anticoagulants", "n")
hos.Anticoagulants90.dec$dec_info <- paste0(hos.Anticoagulants90.dec$Anticoagulants, ":", hos.Anticoagulants90.dec$ps.dec)
hos.Anticoagulants90.dec$n[hos.Anticoagulants90.dec$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.Anticoagulants90.dec$dec.wt <- 1/hos.Anticoagulants90.dec$n

cat("\n (1) \n"); cat(hos.Anticoagulants90.dec); cat("\n")
hos.Anticoagulants90 <- merge(hos.Anticoagulants90, hos.Anticoagulants90.dec[,c("dec_info", "dec.wt")], by="dec_info")

#(2)
hos.Anticoagulants90$ps.dec2 <- cut(hos.Anticoagulants90$ps2, 
                                    breaks=c(quantile(hos.Anticoagulants90$ps2, probs=seq(0,1,0.1))),
                                    labels=seq(1:10), include.lowest=TRUE)
hos.Anticoagulants90$dec_info2 <- paste0(hos.Anticoagulants90$Anticoagulants, ":", hos.Anticoagulants90$ps.dec2)
hos.Anticoagulants90.dec2 <- data.frame(table(hos.Anticoagulants90$ps.dec2, hos.Anticoagulants90$Anticoagulants))
colnames(hos.Anticoagulants90.dec) <- c("ps.dec", "Anticoagulants", "n")
hos.Anticoagulants90.dec$dec_info <- paste0(hos.Anticoagulants90.dec2$Anticoagulants, ":", hos.Anticoagulants90.dec2$ps.dec)
hos.Anticoagulants90.dec2$n[hos.Anticoagulants90.dec2$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.Anticoagulants90.dec2$dec.wt2 <- 1/hos.Anticoagulants90.dec2$n

cat("\n (2) \n"); cat(hos.Anticoagulants90.dec2); cat("\n")
hos.Anticoagulants90 <- merge(hos.Anticoagulants90, hos.Anticoagulants90.dec2[,c("dec_info", "dec.wt2")], by="dec_info")

#(3)
hos.Anticoagulants90$ps.dec3 <- cut(hos.Anticoagulants90$ps3, 
                                    breaks=c(quantile(hos.Anticoagulants90$ps3, probs=seq(0,1,0.1))),
                                    labels=seq(1:10), include.lowest=TRUE)
hos.Anticoagulants90$dec_info3 <- paste0(hos.Anticoagulants90$Anticoagulants, ":", hos.Anticoagulants90$ps.dec3)
hos.Anticoagulants90.dec3 <- data.frame(table(hos.Anticoagulants90$ps.dec3, hos.Anticoagulants90$Anticoagulants))
colnames(hos.Anticoagulants90.dec) <- c("ps.dec", "Anticoagulants", "n")
hos.Anticoagulants90.dec$dec_info <- paste0(hos.Anticoagulants90.dec3$Anticoagulants, ":", hos.Anticoagulants90.dec3$ps.dec)
hos.Anticoagulants90.dec3$n[hos.Anticoagulants90.dec2$n==0] <- 0.0001 # 혹시 모를 에러 막기 위해 매칭에 사용되진 않음.
hos.Anticoagulants90.dec3$dec.wt3 <- 1/hos.Anticoagulants90.dec3$n

cat("\n (3) \n"); cat(hos.Anticoagulants90.dec3); cat("\n")
hos.Anticoagulants90 <- merge(hos.Anticoagulants90, hos.Anticoagulants90.dec3[,c("dec_info", "dec.wt3")], by="dec_info")


ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps, hos.Anticoagulants90$dec.wt, "dec.hos.Anticoagulants90")
outcome.model(Y="outcome1", hos.Anticoagulants90$dec.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="dec.hos.Anticoagulants90") # -0.03765

outcome.model(Y="Death", hos.Anticoagulants90$dec.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="dec.hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants90$dec.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="dec.hos.Anticoagulants90")
outcome.model(Y="ICU.admission", hos.Anticoagulants90$dec.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="dec.hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants90$dec.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="dec.hos.Anticoagulants90")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps2, hos.Anticoagulants90$dec.wt2, "dec.hos.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants90$dec.wt2, df=hos.Anticoagulants90, study=Anticoagulants.study, name="dec.hos.Anticoagulants90_2")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps3, hos.Anticoagulants90$dec.wt3, "dec.hos.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants90$dec.wt3, df=hos.Anticoagulants90, study=Anticoagulants.study, name="dec.hos.Anticoagulants90_3")

print_table(hos.Anticoagulants90, wt=hos.Anticoagulants90$dec.wt, df.study=Anticoagulants.study, save_name="dec.hos.Anticoagulants90")

sink()

#### One to One Matching ####
sink("./resultsCNLLS/model/[Hos_Matching]results_hos.Anticoagulants90.txt")
cat("\n\n------------------\n")
cat(" [5] Matching : 1:1 \n") # 2.188
cat("------------------\n")
# exposure/control groups 정의
# (1)
trt0.ps = hos.Anticoagulants90$ps[ind.trt0]
trt1.ps = hos.Anticoagulants90$ps[ind.trt1]

propDiffMat = outer(trt1.ps, trt0.ps, "-")
propDiffMat = abs(propDiffMat)

# index 를 이름으로 붙여주기
rownames(propDiffMat) = ind.trt1
colnames(propDiffMat) = ind.trt0

matchMat = t( apply(propDiffMat, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.Anticoagulants90$match.wt = rep(0,nrow(hos.Anticoagulants90))
hos.Anticoagulants90$match.wt[ind.trt1] = 1
hos.Anticoagulants90$match.wt[ind.trt0] = colSums(matchMat)

# (2)
trt0.ps2 = hos.Anticoagulants90$ps2[ind.trt0]
trt1.ps2 = hos.Anticoagulants90$ps2[ind.trt1]

propDiffMat2 = outer(trt1.ps2, trt0.ps2, "-")
propDiffMat2 = abs(propDiffMat2)

# index 를 이름으로 붙여주기
rownames(propDiffMat2) = ind.trt1
colnames(propDiffMat2) = ind.trt0

matchMat2 = t( apply(propDiffMat2, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.Anticoagulants90$match.wt2 = rep(0,nrow(hos.Anticoagulants90))
hos.Anticoagulants90$match.wt2[ind.trt1] = 1
hos.Anticoagulants90$match.wt2[ind.trt0] = colSums(matchMat2)

# (3)
trt0.ps3 = hos.Anticoagulants90$ps3[ind.trt0]
trt1.ps3 = hos.Anticoagulants90$ps3[ind.trt1]

propDiffMat3 = outer(trt1.ps3, trt0.ps3, "-")
propDiffMat3 = abs(propDiffMat3)

# index 를 이름으로 붙여주기
rownames(propDiffMat3) = ind.trt1
colnames(propDiffMat3) = ind.trt0

matchMat3 = t( apply(propDiffMat3, 1, function(t) { a = rep(0,length(t)); a[t == min(t)][1] = 1; return(a) } ) )

# matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
# exposure group은 모두 1로 주면 된다.
hos.Anticoagulants90$match.wt3 = rep(0,nrow(hos.Anticoagulants90))
hos.Anticoagulants90$match.wt3[ind.trt1] = 1
hos.Anticoagulants90$match.wt3[ind.trt0] = colSums(matchMat3)

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps, hos.Anticoagulants90$match.wt, "match.hos.Anticoagulants90")
outcome.model(Y="outcome1", hos.Anticoagulants90$match.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="match.hos.Anticoagulants90") # 1.6468

outcome.model(Y="Death", hos.Anticoagulants90$match.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="match.hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants90$match.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="match.hos.Anticoagulants90")
outcome.model(Y="ICU.admission", hos.Anticoagulants90$match.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="match.hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants90$match.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="match.hos.Anticoagulants90")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps2, hos.Anticoagulants90$match.wt2, "match.hos.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants90$match.wt2, df=hos.Anticoagulants90, study=Anticoagulants.study, name="match.hos.Anticoagulants90_2")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps3, hos.Anticoagulants90$match.wt3, "match.hos.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants90$match.wt3, df=hos.Anticoagulants90, study=Anticoagulants.study, name="match.hos.Anticoagulants90_3")

print_table(hos.Anticoagulants90, wt=hos.Anticoagulants90$match.wt, df.study=Anticoagulants.study, save_name="match.hos.Anticoagulants90")

sink()

#### Kernel Matching ####

sink("./resultsCNLLS/model/[Hos_Kernel]results_hos.Anticoagulants90.txt")

cat("\n\n------------------\n")
cat(" [6] Matching : Kernel \n")
cat("------------------\n")

#(1)
propDiffMat.k = propDiffMat/h  

matchMat.k = exp(- propDiffMat.k^2 / 2)
matchMat.k <- matchMat.k / rowSums(matchMat.k)

hos.Anticoagulants90$kernel.wt = rep(0,nrow(hos.Anticoagulants90))
hos.Anticoagulants90$kernel.wt[ind.trt0] = colSums(matchMat.k)
hos.Anticoagulants90$kernel.wt[ind.trt1] = 1

#(2)
propDiffMat.k2 = propDiffMat2/h  

matchMat.k2 = exp(- propDiffMat.k2^2 / 2)
matchMat.k2 <- matchMat.k2 / rowSums(matchMat.k2)

hos.Anticoagulants90$kernel.wt = rep(0,nrow(hos.Anticoagulants90))
hos.Anticoagulants90$kernel.wt[ind.trt0] = colSums(matchMat.k2)
hos.Anticoagulants90$kernel.wt[ind.trt1] = 1

#(3)
propDiffMat.k3 = propDiffMat3/h  

matchMat.k3 = exp(- propDiffMat.k3^2 / 2)
matchMat.k3 <- matchMat.k3 / rowSums(matchMat.k3)

hos.Anticoagulants90$kernel.wt = rep(0,nrow(hos.Anticoagulants90))
hos.Anticoagulants90$kernel.wt[ind.trt0] = colSums(matchMat.k3)
hos.Anticoagulants90$kernel.wt[ind.trt1] = 1

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps, hos.Anticoagulants90$kernel.wt, "kernel.hos.Anticoagulants90")
outcome.model(Y="outcome1", hos.Anticoagulants90$kernel.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="kernel.hos.Anticoagulants90")

outcome.model(Y="Death", hos.Anticoagulants90$kernel.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="kernel.hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", hos.Anticoagulants90$kernel.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="kernel.hos.Anticoagulants90")
outcome.model(Y="ICU.admission", hos.Anticoagulants90$kernel.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="kernel.hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", hos.Anticoagulants90$kernel.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="kernel.hos.Anticoagulants90")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps2, hos.Anticoagulants90$kernel.wt2, "kernel.hos.Anticoagulants90_2")
outcome.model(Y="bleeding.transfusion", wt=hos.Anticoagulants90$kernel.wt2, df=hos.Anticoagulants90, study=Anticoagulants.study, name="kernel.hos.Anticoagulants90_2")

ps_summary(hos.Anticoagulants90$Anticoagulants, hos.Anticoagulants90$ps3, hos.Anticoagulants90$kernel.wt3, "kernel.hos.Anticoagulants90_3")
outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt=hos.Anticoagulants90$kernel.wt3, df=hos.Anticoagulants90, study=Anticoagulants.study, name="kernel.hos.Anticoagulants90_3")

print_table(hos.Anticoagulants90, wt=hos.Anticoagulants90$kernel.wt, df.study=Anticoagulants.study, save_name="kernel.hos.Anticoagulants90")

sink()
