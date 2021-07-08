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


co19_t200DT <- data.table(co19_t200)

targetJID <- intersect(unique(co19_t200DT[CONFIRM =="Y",]$JID), unique(co19_t200DT[PAT_AGE >= 18, ]$JID))
age40JID <- unique(co19_t200DT[PAT_AGE >= 40, ]$JID)

# 배제상병 제외
excluded_t400 <- unique(data.table(co19_t400)[SICK_TY_CD == '3', "MID"])
excluded_twjhe400 <- unique(data.table(co19_twjhe400)[SICK_TY_CD == '3', "MID"])

# -------------------------------------------------- #
co19_t200_trans_dn <- co19_t200[(co19_t200$JID %in% targetJID) & !(co19_t200$MID %in% excluded_t400), c("JID", "MID", "INSUP_TP_CD", "SEX_TP_CD", "RECU_FR_DD", "PAT_AGE", "MAIN_SICK", "SUB_SICK", "DEATH", "CONFIRM")]
co19_t200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_t200_trans_dn$RECU_FR_DD), "%Y%m%d")

co19_t300_trans_dn <- co19_t300[co19_t300$JID %in% targetJID,c("JID", "MID", "DIV_CD")]
co19_t400_trans_dn <- co19_t400[(co19_t400$JID %in% targetJID) & !(co19_t400$MID %in% excluded_t400),c("JID", "MID", "SICK_CD")]
co19_t530_trans_dn <- co19_t530[co19_t530$JID %in% targetJID,c("JID", "MID", "DIV_CD", "GNL_CD")]

co19_twjhe200_trans_dn <- co19_twjhe200[(co19_twjhe200$JID %in% targetJID) & !(co19_twjhe200$MID %in% excluded_twjhe400), c("JID", "MID", "SEX_TP_CD", "RECU_FR_DD", "PAT_AGE", "MAIN_SICK", "SUB_SICK", "RECU_FR_DD")]
co19_twjhe200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_twjhe200_trans_dn$RECU_FR_DD), "%Y%m%d")

co19_twjhe300_trans_dn <- co19_twjhe300[co19_twjhe300$JID %in% targetJID,c("JID", "MID", "DIV_CD", "GNL_CD")]
co19_twjhe400_trans_dn <- co19_twjhe400[(co19_twjhe400$JID %in% targetJID) & !(co19_twjhe400$MID %in% excluded_twjhe400),c("JID", "MID", "SICK_CD")]
co19_twjhe530_trans_dn <- co19_twjhe530[co19_twjhe530$JID %in% targetJID,c("JID", "MID", "DIV_CD", "GNL_CD")]

# External files (code_book)
codebook_path <- "./"
codebook <- "code-books-201020.xlsx"

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
# "antiplatelet/anticoagulant study" -> "antiplatelet study", "anticoagulant study"
code_ICD10_df <- data.frame(code_ICD10[,c("uniqueID", "name", "diagnosis", "code", "codeType", "searchFor", "statin study", "antiplatelet study", "anticoagulant study", "immune suppressant study")])
code_ATC_df <- data.frame(code_ATC[,c("uniqueID", "name", "use", "code", "codeType", "searchFor", "statin study", "antiplatelet study", "anticoagulant study", "immune suppressant study")])
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
patient_dt <- as.data.table(co19_t200_trans_dn)[, .(first_covid = min(RECU_FR_DD)), by = .(JID, INSUP_TP_CD, SEX_TP_CD, PAT_AGE)]
patient_info <- as.data.frame(patient_dt)

# (def) AGE 
patient_info$AGE <- ifelse(patient_info$PAT_AGE<=64, 0, 1)

# 열 삭제
# (def) SEX_TP_CD 1:male, 2:female, 9:etc
patient_info <- patient_info[!(patient_info$SEX_TP_CD == 9),]
patient_info$GENDER <- patient_info$SEX_TP_CD - 1

# (def) 건강보험 : medical insurance, 의료급여&보훈 : medical aid
patient_info <- patient_info[(patient_info$INSUP_TP_CD == 4 | patient_info$INSUP_TP_CD == 5 | patient_info$INSUP_TP_CD == 7),]
patient_info$MedInfo <- ifelse(patient_info$INSUP_TP_CD == 4, 1, 0)

patient_info <- as.data.frame(patient_info[,c("JID", "first_covid", "PAT_AGE", "AGE", "GENDER", "MedInfo")])

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

cat(" covid_outcome R data save \n")

# [3] comorbidties ICD10 (sheet2)
como_info_statin90 <- gen_dummies(code_ICD10_df,  "statin.study",90)
como_info_statin90 <- como_info_statin90[como_info_statin90$JID %in% age40JID, ]

como_info_anti90 <- gen_dummies(code_ICD10_df, "antiplatelet.study",90)

como_info_immune90 <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 90)

como_info_statin120 <- gen_dummies(code_ICD10_df, "statin.study",120)
como_info_statin120 <- como_info_statin120[como_info_statin120$JID %in% age40JID, ]

como_info_anti120 <- gen_dummies(code_ICD10_df, "antiplatelet.study",120)

como_info_immune120 <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 120)

cat(" como_info R data save \n")

# -- codewise frequency comorbidties -- #
cat("\n -3- comorbidties ICD10 (sheet2)  \n")
save_freq("como_info_statin90")
save_freq("como_info_statin120")

save_freq("como_info_anti90")
save_freq("como_info_anti120")

save_freq("como_info_immune90")
save_freq("como_info_immune120")

# [4] medications ATC code (sheet3)
# [code book(code_ATC)의 code column] = [ATC_mapping_df의 ATC코드 column] 
# [ATC_mapping_df의 주성분코드] = [T530의 GNL_CD]
# [ATC_mapping_df의 제품코드]  = T530의 DIV_CD  (T300의 DIV_CD도 일부 존재)
ATC_med_statin90 <- gen_dummies(code_ATC_df, "statin.study",90)
ATC_med_statin90 <- ATC_med_statin90[ATC_med_statin90$JID %in% age40JID, ]

ATC_med_antipl90 <- gen_dummies(code_ATC_df, "antiplatelet.study",90)
ATC_med_antico90 <- gen_dummies(code_ATC_df, "anticoagulant.study",90)

ATC_med_immune90 <- gen_dummies(code_ATC_df, "immune.suppressant.study", 90)

ATC_med_statin120 <- gen_dummies(code_ATC_df, "statin.study",120)
ATC_med_statin120 <- ATC_med_statin120[ATC_med_statin120$JID %in% age40JID, ]

ATC_med_antipl120 <- gen_dummies(code_ATC_df, "antiplatelet.study",120)
ATC_med_antico120 <- gen_dummies(code_ATC_df, "anticoagulant.study",120)

ATC_med_immune120 <- gen_dummies(code_ATC_df, "immune.suppressant.study", 120)

cat(" ATC_med R data save \n")

cat(" -4- medication ATC (sheet3) \n")
df_name <- "ATC_med"
save_freq("ATC_med_statin90")
save_freq("ATC_med_statin120")

save_freq("ATC_med_antipl90")
save_freq("ATC_med_antipl120")

save_freq("ATC_med_antico90")
save_freq("ATC_med_antico120")

save_freq("ATC_med_immune90")
save_freq("ATC_med_immune120")

cat(" codewise data (finish) \n")


# [2] COVID19 code (sheet5)
#covid_diag <- gen_dummies(codeBook=code_covid_df)

# -- codewise frequency covid19
#cat(" -4- covid (sheet5) \n")
#save_freq("covid_diag")

#######################
#sink("./resultsCNLLS/frequency/Confirm_vs_code.txt")
#for (j in (ncol(patient_info)+2):ncol(covid_diag)) {
#  cat(sprintf(" → Confirm by HIRA (Y) versus code [%s]\n", colnames(covid_diag)[j]))
#  print(table("Y" = covid_diag[ ,"ID1001.Y"], "code" = covid_diag[ ,j]))
#  cat("\n")
#}
#sink()
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
  
  cat(sprintf(" (2) [save] %s marginal association \n", study))
  write.csv(round(asso.mat,6), sprintf("./resultsCNLLS/asso_%s.csv", study) )
  
  return (data.frame(cbind(como_df[ ,excl.cols],x,y)))
}

# como_df = como_info_statin90; ATC_df = ATC_med_statin90; study = "statin90"

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

# [3-1] antiplatelets 90
print(" <-------------- anti 90 --------------> \n")
antipl90 <- make_df(como_df = como_info_anti90,
                    ATC_df = ATC_med_antipl90,
                    outcome_df=covid_outcomes,
                    study = "anti90")

# [4-1] antiplatelets 120
print(" <-------------- anti 120 --------------> \n")
antipl120 <- make_df(como_df = como_info_anti120,
                     ATC_df = ATC_med_antipl120,
                     outcome_df=covid_outcomes,
                     study = "anti120")

# [3-2] anticoagulants 90
print(" <-------------- anti 90 --------------> \n")
antico90 <- make_df(como_df = como_info_anti90, 
                    ATC_df = ATC_med_antico90,
                    outcome_df=covid_outcomes,
                    study = "anti90")

# [4-2] anticoagulants 120
print(" <-------------- anti 120 --------------> \n")
antico120 <- make_df(como_df = como_info_anti120,
                     ATC_df = ATC_med_antico120,
                     outcome_df=covid_outcomes,
                     study = "anti120")

# [5-1] immune 90
print(" <-------------- immune 90 --------------> \n")
immune90 <- make_df(como_df = como_info_immune90,
                    ATC_df = ATC_med_immune90,
                    outcome_df=covid_outcomes,
                    study = "immune90")

# [6-1] immune 120
print(" <-------------- immune 120 --------------> \n")
immune120 <- make_df(como_df = como_info_immune120,
                     ATC_df = ATC_med_immune120,
                     outcome_df=covid_outcomes,
                     study = "immune120")

# [5-1] glucocorticoid 90
print(" <-------------- glucocorticoid 90 --------------> \n")
immune90 <- make_df(como_df = como_info_immune90,
                    ATC_df = ATC_med_immune90,
                    outcome_df=covid_outcomes,
                    study = "immune90")

# [6-1] glucocorticoid 120
print(" <-------------- glucocorticoid 120 --------------> \n")
immune120 <- make_df(como_df = como_info_immune120,
                     ATC_df = ATC_med_immune120,
                     outcome_df=covid_outcomes,
                     study = "immune120")

statin90$Lipid.lowering.agents.excluding.statin <- statin90$Lipid.lowering.agents.including.statin - statin90$Lipid.lowering.agents.including.statin * statin90$statin2
statin120$Lipid.lowering.agents.excluding.statin <- statin120$Lipid.lowering.agents.including.statin - statin120$Lipid.lowering.agents.including.statin * statin120$statin2

statin90$first_covid <- NULL
statin120$first_covid <- NULL

antipl90$first_covid <- NULL
antipl120$first_covid <- NULL
antipl90$statin <- NULL
antipl120$statin <- NULL

antico90$first_covid <- NULL
antico120$first_covid <- NULL
antico90$statin <- NULL
antico120$statin <- NULL

immune90$first_covid <- NULL
immune120$first_covid <- NULL
immune90$statin <- NULL
immune120$statin <- NULL

####################################################################
#####################        Modeling        #######################
####################################################################  
# statin variable
statin.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$statin.study == "exposure.and.outcome.related",
                                            c("name", "statin.study")])$name,
                       unique(code_ATC_df[code_ATC_df$statin.study == "exposure.and.outcome.related",
                                          c("name", "statin.study")])$name)

statin.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$statin.study == "outcome.related",
                                         c("name", "statin.study")])$name,
                    unique(code_ATC_df[code_ATC_df$statin.study == "outcome.related",
                                       c("name", "statin.study")])$name)

statin.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", statin.Ex.Out.Rel), gsub("[-,/ ]", ".", statin.Out.Rel), "Lipid.lowering.agents.excluding.statin")
statin.confounder <- setdiff(statin.confounder, "statin2")

statin.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA")
statin.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA")

statin90$outcome1 <- ifelse(rowSums(statin90[,statin.outcome1])>0,1,0)
statin120$outcome1 <-  ifelse(rowSums(statin120[,statin.outcome1])>0,1,0)

#  “TIA”, “Stroke”, “Coronary artery disease ”, “Atherosclerosis”, “Peripheral vascular disease”
statin90$TIA.Stroke.Coronary.Atherosclerosis.Peripheral <- 
  ifelse(statin90$TIA + statin90$Stroke + statin90$Coronary.artery.disease + statin90$Peripheral.vascular.disease>0,1,0)

statin120$TIA.Stroke.Coronary.Atherosclerosis.Peripheral <- 
  ifelse(statin120$TIA + statin120$Stroke + statin120$Coronary.artery.disease + statin120$Peripheral.vascular.disease>0,1,0)

statin.Emodifier <- c("AGE", "GENDER", "Hyperlipidemia", "Hypertension", "Diabetes.mellitus", "TIA.Stroke.Coronary.Atherosclerosis.Peripheral")

statin.exposure <- "statin"

statin.study <- rbind(data.frame(var = statin.exposure, type = "Exposure"),
                      data.frame(var = statin.confounder, type = rep("Confounder", length(statin.confounder))),
                      data.frame(var = statin.outcome1, type = rep("Outcome1", length(statin.outcome1))),
                      data.frame(var = statin.outcome2, type = rep("Outcome2", length(statin.outcome2))), 
                      data.frame(var = statin.Emodifier, type = rep("Effect.Modifier",length(statin.Emodifier))))

# antiplatelets variable
antipl.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$antiplatelet.study == "exposure.and.outcome.related",
                                          c("name", "antiplatelet.study")])$name,
                     unique(code_ATC_df[code_ATC_df$antiplatelet.study == "exposure.and.outcome.related",
                                        c("name", "antiplatelet.study")])$name)

antipl.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$antiplatelet.study == "outcome.related",
                                       c("name", "antiplatelet.study")])$name,
                  unique(code_ATC_df[code_ATC_df$antiplatelet.study == "outcome.related",
                                     c("name", "antiplatelet.study")])$name)

antipl.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", antipl.Ex.Out.Rel), gsub("[-,/ ]", ".", antipl.Out.Rel))
antipl.confounder <- setdiff(antipl.confounder, "statin")

antipl.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA", "Thromboembolism", 
                   "ischemic.stroke.and.TIA", "TIA.transient.ischemic.attack.", "Stroke", "hemorragic.stroke")

# - outcome1 - 
# "myocardial infarction,  ischemic stroke and TIA", "Thromboembolism"
# "bleeding", "transfusion"
antipl90$myocardial.storke.TIA.Thromboembolism <- ifelse(antipl90$myocardial.infarction.ischemic.stroke.and.TIA + antipl90$Thromboembolism>0,1,0)
antipl90$bleeding.transfusion <- ifelse(antipl90$bleeding + antipl90$transfusion>0,1,0)

antipl120$myocardial.storke.TIA.Thromboembolism <- ifelse(antipl120$myocardial.infarction.ischemic.stroke.and.TIA + antipl120$Thromboembolism>0,1,0)
antipl120$bleeding.transfusion <- ifelse(antipl120$bleeding + antipl120$transfusion>0,1,0)

antipl.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.storke.TIA.Thromboembolism", "bleeding.transfusion")

antipl90$outcome1 <- ifelse(rowSums(antipl90[,antipl.outcome1])>0,1,0)
antipl120$outcome1 <- ifelse(rowSums(antipl120[,antipl.outcome1])>0,1,0)

#  “TIA”, “Stroke”, “Coronary artery disease ”, “Peripheral vascular disease”
#  “Atrial Fibrillation”, “Thromboembolism” 
antipl90$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(antipl90$TIA + antipl90$Stroke + antipl90$Coronary.artery.disease + antipl90$Peripheral.vascular.disease>0,1,0)
antipl90$Atrial.Thromboembolism <- ifelse(antipl90$Atrial.fibrillation + antipl90$Thromboembolism>0,1,0)

antipl120$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(antipl120$TIA + antipl120$Stroke + antipl120$Coronary.artery.disease + antipl120$Peripheral.vascular.disease>0,1,0)
antipl120$Atrial.Thromboembolism <- ifelse(antipl120$Atrial.fibrillation + antipl120$Thromboembolism>0,1,0)

antipl.Emodifier <- c("AGE", "GENDER", "Atrial.Thromboembolism", "TIA.Stroke.Coronary.Peripheral")
antipl.exposure <- "Antiplatelets"

antipl.study <- rbind(data.frame(var = antipl.exposure, type = "Exposure"),
                    data.frame(var = antipl.confounder, type = rep("Confounder", length(antipl.confounder))),
                    data.frame(var = antipl.outcome1, type = rep("Outcome1", length(antipl.outcome1))),
                    data.frame(var = antipl.outcome2, type = rep("Outcome2", length(antipl.outcome2))), 
                    data.frame(var = antipl.Emodifier, type = rep("Effect.Modifier",length(antipl.Emodifier))))

# anticoagulants variable
antico.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$anticoagulant.study == "exposure.and.outcome.related",
                                          c("name", "anticoagulant.study")])$name,
                     unique(code_ATC_df[code_ATC_df$anticoagulant.study == "exposure.and.outcome.related",
                                        c("name", "anticoagulant.study")])$name)

antico.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$anticoagulant.study == "outcome.related",
                                       c("name", "anticoagulant.study")])$name,
                  unique(code_ATC_df[code_ATC_df$anticoagulant.study == "outcome.related",
                                     c("name", "anticoagulant.study")])$name)

antico.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", antico.Ex.Out.Rel), gsub("[-,/ ]", ".", antico.Out.Rel))
antico.confounder <- setdiff(antico.confounder, "statin")

antico.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA", "Thromboembolism", 
                   "ischemic.stroke.and.TIA", "TIA.transient.ischemic.attack.", "Stroke", "hemorragic.stroke")

# - outcome1 - 
# "myocardial infarction,  ischemic stroke and TIA", "Thromboembolism"
# "bleeding", "transfusion"
antico90$myocardial.storke.TIA.Thromboembolism <- ifelse(antico90$myocardial.infarction.ischemic.stroke.and.TIA + antico90$Thromboembolism>0,1,0)
antico90$bleeding.transfusion <- ifelse(antico90$bleeding + antico90$transfusion>0,1,0)

antico120$myocardial.storke.TIA.Thromboembolism <- ifelse(antico120$myocardial.infarction.ischemic.stroke.and.TIA + antico120$Thromboembolism>0,1,0)
antico120$bleeding.transfusion <- ifelse(antico120$bleeding + antico120$transfusion>0,1,0)

antico.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.storke.TIA.Thromboembolism", "bleeding.transfusion")

antico90$outcome1 <- ifelse(rowSums(antico90[,antico.outcome1])>0,1,0)
antico120$outcome1 <- ifelse(rowSums(antico120[,antico.outcome1])>0,1,0)

#  “TIA”, “Stroke”, “Coronary artery disease ”, “Peripheral vascular disease”
#  “Atrial Fibrillation”, “Thromboembolism” 
antico90$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(antico90$TIA + antico90$Stroke + antico90$Coronary.artery.disease + antico90$Peripheral.vascular.disease>0,1,0)
antico90$Atrial.Thromboembolism <- ifelse(antico90$Atrial.fibrillation + antico90$Thromboembolism>0,1,0)

antico120$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(antico120$TIA + antico120$Stroke + antico120$Coronary.artery.disease + antico120$Peripheral.vascular.disease>0,1,0)
antico120$Atrial.Thromboembolism <- ifelse(antico120$Atrial.fibrillation + antico120$Thromboembolism>0,1,0)

antico.Emodifier <- c("AGE", "GENDER", "Atrial.Thromboembolism", "TIA.Stroke.Coronary.Peripheral")
antico.exposure <- "Anticoagulants"

antico.study <- rbind(data.frame(var = antico.exposure, type = "Exposure"),
                    data.frame(var = antico.confounder, type = rep("Confounder", length(antico.confounder))),
                    data.frame(var = antico.outcome1, type = rep("Outcome1", length(antico.outcome1))),
                    data.frame(var = antico.outcome2, type = rep("Outcome2", length(antico.outcome2))), 
                    data.frame(var = antico.Emodifier, type = rep("Effect.Modifier",length(antico.Emodifier))))


# immune variable
immune.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$immune.suppressant.study == "exposure.and.outcome.related",
                                            c("name", "immune.suppressant.study")])$name,
                       unique(code_ATC_df[code_ATC_df$immune.suppressant.study == "exposure.and.outcome.related",
                                          c("name", "immune.suppressant.study")])$name)
immune.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$immune.suppressant.study == "outcome.related",
                                         c("name", "immune.suppressant.study")])$name,
                    unique(code_ATC_df[code_ATC_df$immune.suppressant.study == "outcome.related",
                                       c("name", "immune.suppressant.study")])$name)

immune.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", immune.Ex.Out.Rel), gsub("[-,/ ]", ".", immune.Out.Rel))
immune.confounder <- setdiff(immune.confounder, c("statin", "Antineolastic"))

immune.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission")
immune.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission")
immune90$outcome1 <- ifelse(rowSums(immune90[,immune.outcome1])>0,1,0)
immune120$outcome1 <- ifelse(rowSums(immune120[,immune.outcome1])>0,1,0)

immune.Emodifier <- c("AGE", "GENDER", "immuned.deficiency.including.Cacner.HIV", "autoimmune.disease")


immune90$immuned.deficiency.including.Cacner.HIV <- ifelse(immune90$immuned.deficiency.including.Cacner.HIV + immune90$Antineoplastic, 1, 0)
immune120$immuned.deficiency.including.Cacner.HIV <- ifelse(immune120$immuned.deficiency.including.Cacner.HIV + immune120$Antineoplastic, 1, 0)


immune.exposure <- "immunosuppressant"

immune.study <- rbind(data.frame(var = immune.exposure, type = "Exposure"),
                      data.frame(var = immune.confounder, type = rep("Confounder", length(immune.confounder))),
                      data.frame(var = immune.outcome1, type = rep("Outcome1", length(immune.outcome1))),
                      data.frame(var = immune.outcome2, type = rep("Outcome2", length(immune.outcome2))), 
                      data.frame(var = immune.Emodifier, type = rep("Effect.Modifier",length(immune.Emodifier))))

# table 1,2. print #
print_table <- function(df.all, df.study){
  agebreaks <- c(0, 30, 40, 50, 60, 70, 80, 90, 500)
  agelabels <- c("30-", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")
  
  setDT(df.all)[, AGE2 := cut(PAT_AGE,
                              breaks = agebreaks,
                              right = FALSE,
                              labels = agelabels)]
  df.all <- data.frame(df.all)
  
  df.all$GENDER2 <- ifelse(df.all$GENDER==1, "Female", "Male")
  df.all$MedInfo2 <- ifelse(df.all$MedInfo==1, "National Health Insurance", "Medical Aid")
  
  exposure <- as.character( df.study[df.study$type=="Exposure","var"] )
  
  df.User <- df.all[df.all[,exposure]==1,]
  df.NonUser <- df.all[!df.all[,exposure]==1,]
  
  N <- data.frame(Index="N", "all"=nrow(df.all), User=nrow(df.User), NonUser=nrow(df.NonUser))
  
  # ---------- table 1 ---------- #
  # 1. Age
  
  mean <- data.frame(Index="mean", all=mean(df.all$PAT_AGE), User=mean(df.User$PAT_AGE), NonUser=mean(df.NonUser$PAT_AGE))
  sd <- data.frame(Index="sd", all=sd(df.all$PAT_AGE), User=sd(df.User$PAT_AGE), NonUser=sd(df.NonUser$PAT_AGE))
  
  allAge <- data.frame(table(df.all$AGE2)); colnames(allAge) <- c("Index", "all")
  UserAge <- data.frame(table(df.User$AGE2)); colnames(UserAge) <- c("Index", "User")
  NonUserAge <- data.frame(table(df.NonUser$AGE2)); colnames(NonUserAge) <- c("Index", "NonUser")
  
  AgeTable <- merge(allAge, UserAge, by="Index", all=TRUE)
  AgeTable <- merge(AgeTable, NonUserAge, by="Index", all=TRUE)
  AgeTable <- rbind(mean, sd, AgeTable)
  
  # 2. Sex
  SexTable <- data.frame("Index"=c("Male", "Female"),
                         "all"=c(nrow(df.all[df.all$MedInfo2=="Male",]), nrow(df.all[!df.all$MedInfo2=="Female", ])),
                         "User"=c(nrow(df.User[df.User$MedInfo2=="Male",]), nrow(df.User[!df.User$MedInfo2=="Female", ])),
                         "NonUser"=c(nrow(df.NonUser[df.NonUser$MedInfo2=="Male",]), nrow(df.NonUser[!df.NonUser$MedInfo2=="Female", ])))
  
  # 3. MedInfo
  MedTable <- data.frame("Index"=c("Medical Insurance", "Medical Aid"),
                         "all"=c(nrow(df.all[df.all$MedInfo2=="National Health Insurance",]), nrow(df.all[!df.all$MedInfo2=="Medical Aid", ])),
                         "User"=c(nrow(df.User[df.User$MedInfo2=="National Health Insurance",]), nrow(df.User[!df.User$MedInfo2=="Medical Aid", ])),
                         "NonUser"=c(nrow(df.NonUser[df.NonUser$MedInfo2=="National Health Insurance",]), nrow(df.NonUser[!df.NonUser$MedInfo2=="Medical Aid", ])))
  
  confounder <- as.character( df.study[df.study$type=="Confounder","var"] )
  
  # 4. Comorbidities
  como_names <- names(df.all)[6:28]
  
  comoTable <- NULL;
  for (name in como_names) {
    comoTmp <- data.frame("Index"=name, "all"=nrow( df.all[df.all[,name],] ), 
                          "User"=nrow( df.User[df.User[,name],] ), "NonUser"=nrow( df.NonUser[df.NonUser[,name],] ) )
    comoTable <- rbind(comoTable, comoTmp)
  }
  
  # 5. ATC medications
  ATC_names <- names(df.all)[29:44]
  ATCTable <- NULL;
  for (name in ATC_names) {
    ATCTmp <- data.frame("Index"=name, "all"=nrow( df.all[df.all[,name],] ), 
                         "User"=nrow( df.User[df.User[,name],] ), "NonUser"=nrow( df.NonUser[df.NonUser[,name],] ) )
    ATCTable <- rbind(ATCTable, ATCTmp)
  }

  t1 <- rbind(N, AgeTable, SexTable, MedTable,
              comoTable[comoTable$Index %in% confounder, ], 
              ATCTable[ATCTable$Index %in% confounder, ])
  
  # ---------- table 2 ---------- #
  # 6. Risk of adverse clinical outcomes associated with Exposure ~ .
  # (1) # of patients (2) # of events (3) Event Rates (%)
  NonUserO <- data.frame( Index="Primary Outcome",
                          N_Patients=nrow(df.NonUser),
                          N_Events=nrow(df.NonUser[df.NonUser$outcome1,]),
                          EventRates=nrow(df.NonUser[df.NonUser$outcome1,])/nrow(df.NonUser) )
  UserO <- data.frame( Index="Primary Outcome",
                       N_Patients=nrow(df.User),
                       N_Events=nrow(df.User[df.User$outcome1,]),
                       EventRates=nrow(df.User[df.User$outcome1,])/nrow(df.User) )
  
  Primary <- data.frame(Index=c("NonUser-Primary", "User-Primary"),
                        N_Patients=c(nrow(df.NonUser), nrow(df.User)),
                        N_Events=c(nrow(df.NonUser[df.NonUser$outcome1,]),nrow(df.User[df.User$outcome1,]) ),
                        EventRates=c(nrow(df.NonUser[df.NonUser$outcome1,])/nrow(df.NonUser),
                                    nrow(df.User[df.User$outcome1,])/nrow(df.User) ) )
  
  outcomes2 <- as.character( df.study[df.study$type=="Outcome2","var"] )
  Secondary <- NULL
  for (outcome2 in outcomes2){
    ind <- paste0(c("NonUser-", "User-"), outcome2)
    secondaryTmp <- data.frame(Index=ind,
                               N_Patients=c(nrow(df.NonUser), nrow(df.User)),
                               N_Events=c(nrow(df.NonUser[df.NonUser[,outcome2],]),nrow(df.User[df.User[,outcome2],]) ),
                               EventRates=c(nrow(df.NonUser[df.NonUser[,outcome2],])/nrow(df.NonUser),
                                            nrow(df.User[df.User[,outcome2],])/nrow(df.User) )  )
    Secondary <- rbind(Secondary, secondaryTmp)
  }
  t2 <- Secondary
  filename1 <- paste0("./resultsCNLLS/table/[Table1]", exposure, ".csv")
  filename2 <- paste0("./resultsCNLLS/table/[Table2]", exposure, ".csv")
  
  write.csv(t1, filename1)
  write.csv(t2, filename2)
}

print_table(statin90, statin.study) # 관측 수 적어서 error 발생
print_table(statin120, statin.study) # 관측 수 적어서 error 발생
print_table(antipl90, antipl.study)
print_table(antipl120, antipl.study)
print_table(antico90, antico.study)
print_table(antico120, antico.study)
print_table(immune90, immune.study)
print_table(immune120, immune.study)

CI.modeling <- function(df, study, name){
  save_path <- sprintf("./resultsCNLLS/model/coef_%s/", name)
  dir.create(save_path, showWarnings=FALSE)
  
  print(study[,c(2,1)])
  study.exposure <- as.character(study[study$type == "Exposure","var"])
  study.confounder <- as.character(study[study$type == "Confounder","var"])
  study.Effect.Modifier <- as.character(study[study$type == "Effect.Modifier","var"])
  study.second <- as.character(study[study$type == "Outcome2","var"])
  
  Exposure <- df[,study.exposure]
  Y1 <- df[,"outcome1"]
  
  Confounder <- df[,study.confounder]
  X <- df[,c(study.exposure, "AGE", "GENDER", "MedInfo", "Hypertension", "Diabetes.mellitus")]
  
  cat("=============================================\n")
  cat("=============================================\n")
  cat("            primary outcome\n\n")
  cat("=============================================\n")
  cat("=============================================\n")
  
  # [0-1] outcome1 ~ exposure (NAIVE)
  fit0.1 <- glm(Y1 ~ Exposure, family="binomial")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [0-1] Naive Primary Outcome Model \n\n")
  print(summary(fit0.1))
   
  Coef_0.1 <- data.frame("coef" = exp(summary(fit0.1)$coef[,1]), # 계수  
                         "s.e" = summary(fit0.1)$coef[,2], # 표준 오차
                         "lower.ci" = exp(summary(fit0.1)$coef[,1] - 1.96 * summary(fit0.1)$coef[,2]), # CI
                         "upper.ci" = exp(summary(fit0.1)$coef[,1] + 1.96 * summary(fit0.1)$coef[,2]))
  write.csv(Coef_0.1, paste0(save_path, "primary_Naive.csv"))
  
  # [0-2] Naive Multivariate model  outcome1 ~ exposure + confounder : wt (X)
  fit0.2 <- glm(Y1~., data=X, family="binomial")
  pred0.2 <- predict(fit0.2, type="response")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [0-2] Naive Multivariate Primary Outcome Model \n\n")
  print(summary(fit0.2))
  
  Coef_0.2 <- data.frame("coef" = exp(summary(fit0.2)$coef[,1]), # 계수  
                         "s.e" = summary(fit0.2)$coef[,2], # 표준 오차
                         "lower.ci" = exp(summary(fit0.2)$coef[,1] - 1.96 * summary(fit0.2)$coef[,2]), # CI
                         "upper.ci" = exp(summary(fit0.2)$coef[,1] + 1.96 * summary(fit0.2)$coef[,2]))
  write.csv(Coef_0.2, paste0(save_path, "primary_MultiNaive.csv"))
  
  # [1] propensity score
  #   (a) exposure ~ confounders
  fit1a<- glm(Exposure ~ ., data = Confounder, family="binomial")
  df$ps1a <- predict(fit1a, type="response")
  df$wt1a <- df$ps1a*df[,study.exposure] + (1-df$ps1a)*(1-df[,study.exposure])
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [1] Propensity Score Model (a)\n\n")
  cat(" - propensity score dist\n\n")
  print(summary(df$ps1a))
  cat("\n\n")
  cat(" - weight dist\n\n")
  print(summary(df$wt1a))
  cat("\n\n")
  cat(" - 1/weight dist\n\n")
  print(summary(1/df$wt1a))
  cat("\n\n")
  
  cat(" - fit1a model summary \n\n")
  print(summary(fit1a))
  cat("\n\n")
  
  Coef_1a <- data.frame("coef" = exp(summary(fit1a)$coef[,1]), # 계수  
                         "s.e" = summary(fit1a)$coef[,2], # 표준 오차
                         "lower.ci" = exp(summary(fit1a)$coef[,1] - 1.96 * summary(fit1a)$coef[,2]), # CI
                         "upper.ci" = exp(summary(fit1a)$coef[,1] + 1.96 * summary(fit1a)$coef[,2]))
  write.csv(Coef_1a, paste0(save_path, "primary_PS.csv"))
  
  # [2] outcome model (main)
  #   (a) outcome1 ~ exposure (wt 1a)
  fit2a <- glm(Y1~Exposure, weight = 1/df$wt1a, family="binomial")
  pred2a <- predict(fit2a, type="response")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [2] Primary Outcome Model (a)\n\n")
  print(summary(fit2a))
  cat("\n\n")
  
  Coef_2a <- data.frame("coef" = exp(summary(fit2a)$coef[,1]), # 계수  
                         "s.e" = summary(fit2a)$coef[,2], # 표준 오차
                         "lower.ci" = exp(summary(fit2a)$coef[,1] - 1.96 * summary(fit2a)$coef[,2]), # CI
                         "upper.ci" = exp(summary(fit2a)$coef[,1] + 1.96 * summary(fit2a)$coef[,2]))
  write.csv(Coef_2a, paste0(save_path, "primary_IPTW.csv"))
  
  # [3] outcome ~ exposure + effect modifier 
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  
  for (v in study.Effect.Modifier){
    cat(sprintf(" [3] Effect Modifier Primary Outcome Model : %s \n\n", v))
    EffectModifier <- df[,c(study.exposure,v)]
    EffectModifier[,"A*V"] <- Exposure * df[,v]
    
    # outcome1 ~ exposure + v + exposure*v (wt1a)
    fit3_v <- glm(Y1~., data=EffectModifier, weight = 1/df$wt1a, family="binomial")
    print(summary(fit3_v))
    Coef_3_v <- data.frame("coef" = exp(summary(fit3_v)$coef[,1]), # 계수  
                           "s.e" = summary(fit3_v)$coef[,2], # 표준 오차
                           "lower.ci" = exp(summary(fit3_v)$coef[,1] - 1.96 * summary(fit3_v)$coef[,2]), # CI
                           "upper.ci" = exp(summary(fit3_v)$coef[,1] + 1.96 * summary(fit3_v)$coef[,2]))
    
    E.mod1 <- sprintf("primary_EffectModifier_%s.csv", v)
    E.mod2 <- sprintf("Cov_primary_EffectModifier_%s.csv", v)
    write.csv(Coef_3_v, paste0(save_path, E.mod1))
    write.csv(vcov(fit3_v), paste0(save_path, E.mod2))
    
    cat("\n\n")
  }

  cat("=============================================\n")
  cat("=============================================\n")
  cat("            secondary outcome\n\n")
  cat("=============================================\n")
  cat("=============================================\n")
  
  for (outcome2 in study.second) {
    cat("*** "); cat(outcome2);cat(" ***\n\n")
    
    # study confounder 초기화
    study.confounder <- as.character(study[study$type == "Confounder","var"])
    study.scr <- as.character(study[study$type == "Confounder","var"])
    
    Y2 <- df[,outcome2]
    
    #MI, TIA, stroke, or thromboembolism - 생기는 사람이 소수임
    #Bleeding(except skin bleeding) or transfusion - primary에 포함X
    if (outcome2 %in% c("myocardial.storke.TIA.Thromboembolism", "bleeding.transfusion")){ 
      cat("============================================================\n")
      cat("============================================================\n")
      
      
      for (c in study.confounder[4:length(study.confounder)]){
        study.scr <- c("AGE", "GENDER", "MedInfo")
        # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
        fit.scr <- glm(df[,outcome2]~df[,c], data=df, family = "binomial")
        if ( length(summary(fit.scr)$coef) == 4) {
          print("ERROR!!, exposure coefficeints is NA.")
          next
        }
        if (summary(fit.scr)$coef[2,4] < 0.2){
          study.scr <- c(study.scr, c)
        }
      }
      cat("\n [Confounder after p-value Screening(0.2)] \n")
      print(study[study$var %in% study.scr, c(2,1)])
      
      Screening <- df[,study.scr]
      
      # [1] ps model for screening variable
      # (a) exposure ~ confounders
      fit1a<- glm(Exposure ~ ., data = Screening, family="binomial")
      df$ps1a <- predict(fit1a, type="response")
      df$wt1a <- df$ps1a*df[,study.exposure] + (1-df$ps1a)*(1-df[,study.exposure])
      
      cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
      cat(" [1] Propensity Score Model - new (a)\n\n")
      cat(" - propensity score dist\n\n")
      print(summary(df$ps1a))
      cat("\n\n")
      cat(" - weight dist\n\n")
      print(summary(df$wt1a))
      cat("\n\n")
      cat(" - 1/weight dist\n\n")
      print(summary(1/df$wt1a))
      cat("\n\n")
      cat(" - model summary \n\n")
      print(summary(fit1a))
      cat("\n\n")
      
      secondary_PS <- sprintf("Secondary_PS_%s.csv", outcome2)
      Coef_1a <- data.frame("coef" = exp(summary(fit1a)$coef[,1]), # 계수  
                             "s.e" = summary(fit1a)$coef[,2], # 표준 오차
                             "lower.ci" = exp(summary(fit1a)$coef[,1] - 1.96 * summary(fit1a)$coef[,2]), # CI
                             "upper.ci" = exp(summary(fit1a)$coef[,1] + 1.96 * summary(fit1a)$coef[,2]))
      write.csv(Coef_1a, paste0(save_path, secondary_PS))
    }
    
    # [0-1] Naive model  outcome2 ~ exposure : wt (X)
    fit0.1 <- glm(Y2~Exposure, family="binomial")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [0-1] Naive Secondary Outcome Model \n\n")
    print(summary(fit0.1))
    
    secondary_Naive <- sprintf("Secondary_Naive_%s.csv", outcome2)
    Coef_0.1 <- data.frame("coef" = exp(summary(fit0.1)$coef[,1]), # 계수  
                           "s.e" = summary(fit0.1)$coef[,2], # 표준 오차
                           "lower.ci" = exp(summary(fit0.1)$coef[,1] - 1.96 * summary(fit0.1)$coef[,2]), # CI
                           "upper.ci" = exp(summary(fit0.1)$coef[,1] + 1.96 * summary(fit0.1)$coef[,2]))
    write.csv(Coef_0.1, paste0(save_path, secondary_Naive))
    
    # [0-2] Naive model  outcome2 ~ exposure + confounder : wt (X)
    fit0.2 <- glm(Y2~., data=X, family="binomial")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [0-2] Naive Multivariate Secondary Outcome Model \n\n")
    print(summary(fit0.2))
    
    secondary_MultiNaive <- sprintf("Secondary_MultiNaive_%s.csv", outcome2)
    
    Coef_0.2 <- data.frame("coef" = exp(summary(fit0.2)$coef[,1]), # 계수  
                           "s.e" = summary(fit0.2)$coef[,2], # 표준 오차
                           "lower.ci" = exp(summary(fit0.2)$coef[,1] - 1.96 * summary(fit0.2)$coef[,2]), # CI
                           "upper.ci" = exp(summary(fit0.2)$coef[,1] + 1.96 * summary(fit0.2)$coef[,2]))
    write.csv(Coef_0.1, paste0(save_path, secondary_MultiNaive))
    
    # [2] outcome model (main)
    #   (a) outcome2 ~ exposure (wt1a)
    fit2a <- glm(Y2~Exposure, weight = 1/df$wt1a, family="binomial")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [2-a] Secondary Outcome Model\n\n")
    print(summary(fit2a))
    cat("\n\n")
    
    secondary_IPTW <- sprintf("Secondary_IPTW_%s.csv", outcome2)
    
    Coef_2a <- data.frame("coef" = exp(summary(fit2a)$coef[,1]), # 계수  
                           "s.e" = summary(fit2a)$coef[,2], # 표준 오차
                           "lower.ci" = exp(summary(fit2a)$coef[,1] - 1.96 * summary(fit2a)$coef[,2]), # CI
                           "upper.ci" = exp(summary(fit2a)$coef[,1] + 1.96 * summary(fit2a)$coef[,2]))
    write.csv(Coef_2a, paste0(save_path, secondary_IPTW))
    
  }
}

print("check point - CI modeling -")
##########
# statin 90
sink("./resultsCNLLS/model/results_statin90.txt")
cat("\n <--------- [statin 90] Result (primary) ---------> \n")
CI.modeling(df = data.frame(statin90), study=statin.study[order(statin.study$type, statin.study$var),], name="statin90")
sink()

# statin 120
sink("./resultsCNLLS/model/results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
CI.modeling(df = data.frame(statin120), study=statin.study[order(statin.study$type, statin.study$var),], name="statin120")
sink()

# antiplatelets 90
sink("./resultsCNLLS/model/results_antipl90.txt")
cat("\n <--------- [antipl 90] Result (primary) ---------> \n")
CI.modeling(df = data.frame(antipl90), study=antipl.study[order(antipl.study$type, antipl.study$var),], name="antipl90")
sink()

# antiplatelets 120
sink("./resultsCNLLS/model/results_antipl120.txt")
cat("\n <--------- [antipl 120] Result (primary) ---------> \n")
CI.modeling(df = data.frame(antipl120), study=antipl.study[order(antipl.study$type, antipl.study$var),], name="antipl120")
sink()

# anticoagulants 90
sink("./resultsCNLLS/model/results_antico90.txt")
cat("\n <--------- [anti 90] Result (primary) ---------> \n")
CI.modeling(df = data.frame(antico90), study=antico.study[order(antico.study$type, antico.study$var),], name="antico90")
sink()

# anticoagulants 120
sink("./resultsCNLLS/model/results_antico120.txt")
cat("\n <--------- [antico 120] Result (primary) ---------> \n")
CI.modeling(df = data.frame(antico120), study=antico.study[order(antico.study$type, antico.study$var),], name="antico120")
sink()

# immune 90
sink("./resultsCNLLS/model/results_immune90.txt")
cat("\n <--------- [immune 90] Result (primary) ---------> \n")
CI.modeling(df = data.frame(immune90), study=immune.study[order(immune.study$type, immune.study$var),], name="immune90")
sink()

# immune 120
sink("./resultsCNLLS/model/results_immune120.txt")
cat("\n <--------- [immune 120] Result (primary) ---------> \n")
CI.modeling(df = data.frame(immune120), study=immune.study[order(immune.study$type, immune.study$var),], name="immune120")
sink()
