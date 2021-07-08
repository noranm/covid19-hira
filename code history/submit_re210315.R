#######################################
########## [1] Preparation ############
#######################################
# external data:
# 1. code-books-210202.xlsx / code-books-210202-2.xlsx
# 2. ATCcode_mapping.xlsx

# library:
library(readxl)
library(dplyr)
library(data.table)

# save path:
dir.create("./resultsCNLLS/", showWarnings=FALSE)
dir.create("./resultsCNLLS/frequency", showWarnings=FALSE)
dir.create("./resultsCNLLS/frequency2", showWarnings=FALSE)
dir.create("./resultsCNLLS/frequency3", showWarnings=FALSE)
dir.create("./resultsCNLLS/model", showWarnings=FALSE)
dir.create("./resultsCNLLS/ps", showWarnings=FALSE)
dir.create("./resultsCNLLS/ps/[Check]distribution", showWarnings=FALSE)
dir.create("./resultsCNLLS/table", showWarnings=FALSE)

#raw data (PLEASE REPLACE sample_schema.xlsx WITH THE RAW DATA)

# raw data's type (co19_t200 ~ co19_twjhe530) : data.frame
# *** data.frame() function ***
#raw data (PLEASE REPLACE sample_schema.xlsx WITH THE RAW DATA)
data_path <- "./sample_schema_sanity.xlsx"

co19_t200 = data.frame(read_excel(data_path, sheet=4))
co19_t300 = data.frame(read_excel(data_path, sheet=5))
co19_t400 = data.frame(read_excel(data_path, sheet=6))
co19_t530 = data.frame(read_excel(data_path, sheet=7))

# Medical use history data
co19_twjhe200 = data.frame(read_excel(data_path, sheet=8))
co19_twjhe300 = data.frame(read_excel(data_path, sheet=9))
co19_twjhe400 = data.frame(read_excel(data_path, sheet=10))
co19_twjhe530 = data.frame(read_excel(data_path, sheet=11))

# process log1
sink("./resultsCNLLS/p-log1.txt")

testJID <- unique(co19_t200$JID)
cat(sprintf("\n - test patient: %d\n", length(testJID)))

####################
# study population #
####################
co19_t200$AGE_cal <- 2020-as.numeric(substr(co19_t200$PAT_BTH, 1, 4))
# "---JID" objects must be "vector"
under40JID <- unique(co19_t200[(co19_t200$AGE_cal < 40),]$JID)
cat(sprintf("\n - under 40 patient: %d\n", length(under40JID)))

under20JID <- unique(co19_t200[(co19_t200$AGE_cal < 20),]$JID)
cat(sprintf("\n - under 20 patient: %d\n", length(under20JID)))

confirmedJID <- unique(co19_t200[(co19_t200$CONFIRM=="Y"), ]$JID)
cat(sprintf("\n - confirmed patient: %d\n", length(confirmedJID)))

targetJID <- unique(co19_t200[(co19_t200$CONFIRM=="Y") & (co19_t200$AGE_cal > 39), ]$JID)
targetJID <- setdiff(targetJID, NA)
cat(sprintf("\n - target patient: %d\n", length(targetJID)))

cancer_jid1 <- unique(co19_twjhe200[co19_twjhe200$PRCL_SYM_TP_CD %in% c("V027", "V193", "V194"), ]$JID)
cancer_jid2 <- unique(co19_t200[co19_t200$PRCL_SYM_TP_CD %in% c("V027", "V193", "V194"), ]$JID)
cancer_jid <- unique(c(cancer_jid1, cancer_jid2))

print(length(cancer_jid))

# *** select variables :
co19_t200_trans_dn <- co19_t200[(co19_t200$JID %in% targetJID), c("JID", "MID", "INSUP_TP_CD", "SEX_TP_CD", "RECU_FR_DD", "FOM_TP_CD", "VST_DDCNT", "PAT_BTH", "AGE_cal", "MAIN_SICK", "SUB_SICK", "PRCL_SYM_TP_CD", "DEATH", "CONFIRM")]
co19_t200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_t200_trans_dn$RECU_FR_DD), "%Y%m%d")

co19_t300_trans_dn <- co19_t300[co19_t300$JID %in% targetJID, c("JID", "MID", "DIV_CD", "GNL_CD")]
co19_t400_trans_dn <- co19_t400[co19_t400$JID %in% targetJID, c("JID", "MID", "SICK_CD")]
co19_t530_trans_dn <- co19_t530[co19_t530$JID %in% targetJID, c("JID", "MID", "DIV_CD", "GNL_CD")]

co19_twjhe200_trans_dn <- co19_twjhe200[co19_twjhe200$JID %in% targetJID, c("JID", "MID", "SEX_TP_CD", "RECU_FR_DD", "MAIN_SICK", "SUB_SICK", "PRCL_SYM_TP_CD")]
co19_twjhe200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_twjhe200_trans_dn$RECU_FR_DD), "%Y%m%d")

co19_twjhe300_trans_dn <- co19_twjhe300[co19_twjhe300$JID %in% targetJID,c("JID", "MID", "DIV_CD", "GNL_CD")]
co19_twjhe400_trans_dn <- co19_twjhe400[co19_twjhe400$JID %in% targetJID,c("JID", "MID", "SICK_CD")]
co19_twjhe530_trans_dn <- co19_twjhe530[co19_twjhe530$JID %in% targetJID,c("JID", "MID", "DIV_CD", "GNL_CD")]

cat("dataset check\n")
print(dim(co19_t200)); print(dim(co19_t200_trans_dn)); print(colnames(co19_t200_trans_dn))
print(dim(co19_t300)); print(dim(co19_t300_trans_dn)); print(colnames(co19_t300_trans_dn))
print(dim(co19_t400)); print(dim(co19_t400_trans_dn)); print(colnames(co19_t400_trans_dn))
print(dim(co19_t530)); print(dim(co19_t530_trans_dn)); print(colnames(co19_t530_trans_dn))

print(dim(co19_twjhe200)); print(dim(co19_twjhe200_trans_dn)); print(colnames(co19_twjhe200_trans_dn))
print(dim(co19_twjhe300)); print(dim(co19_twjhe300_trans_dn)); print(colnames(co19_twjhe300_trans_dn))
print(dim(co19_twjhe400)); print(dim(co19_twjhe400_trans_dn)); print(colnames(co19_twjhe400_trans_dn))
print(dim(co19_twjhe530)); print(dim(co19_twjhe530_trans_dn)); print(colnames(co19_twjhe530_trans_dn))

FOM_TP_CD2 <- as.character(co19_t200_trans_dn$FOM_TP_CD)
VST_DDCNT2 <- as.numeric(co19_t200_trans_dn$VST_DDCNT)

print(typeof(FOM_TP_CD2))
print(typeof(VST_DDCNT2))

# [HR]
# 021, 071, 21, 71 all check
hospitalMID <- c(co19_t200_trans_dn[(FOM_TP_CD2 %in% c("21", "71", "021", "071")), ]$MID)
hospitalJID <- unique(co19_t200_trans_dn[(co19_t200_trans_dn$MID %in% hospitalMID), ]$JID)

# length value >= 1
print(length(hospitalMID))
print(length(hospitalJID))

cat("\n-> Hospitalized Patients\n")
print(table(co19_t200_trans_dn$FOM_TP_CD))
print(summary(co19_t200_trans_dn$FOM_TP_CD))

print(summary(co19_t200_trans_dn$VST_DDCNT))
print(typeof(co19_t200_trans_dn$VST_DDCNT))

cat("\n-> Patients with Cancer\n")
print(table(co19_twjhe200_trans_dn$PRCL_SYM_TP_CD))
print(table(co19_t200_trans_dn$PRCL_SYM_TP_CD))

sink() 

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

#####################################
########## EXTERNAL DATA ############
#####################################

# External files (code_book)
codebook_path <- "./"
codebook <- "code-books-210218.xlsx"
codebook2 <- "code-books-210218-2.xlsx"

code_ICD10 <- read_excel(file.path(codebook_path, codebook), sheet = 2)
code_ATC <- read_excel(file.path(codebook_path, codebook), sheet = 3)
code_ATC2 <- read_excel(file.path(codebook_path, codebook2), sheet = 3)
code_ATC3 <- read_excel(file.path(codebook_path, codebook2), sheet = 6)
code_outcomes <- read_excel(file.path(codebook_path, codebook), sheet = 4)
code_covid <- read_excel(file.path(codebook_path, codebook), sheet = 5)
ATC_mapping <- read_excel(file.path(codebook_path, "ATCcode_mapping.xlsx"))

code_ICD10_df <- data.frame(code_ICD10[,c("uniqueID", "name", "diagnosis", "code", "codeType", "searchFor", "statin study", "antiplatelet anticoagulant study", "immune suppressant study")])
code_ATC_df <- data.frame(code_ATC[,c("uniqueID", "name", "use", "code", "codeType", "searchFor", "statin study", "antiplatelet anticoagulant study", "immune suppressant study")])
code_ATC_df2 <- data.frame(code_ATC2[,c("uniqueID", "name", "use", "code", "codeType", "searchFor", "statin study", "antiplatelet anticoagulant study", "immune suppressant study")])
code_ATC_df3 <- data.frame(code_ATC3[,c("uniqueID", "name", "use", "code", "codeType", "searchFor")])
code_outcomes_df <- data.frame(code_outcomes[,c("uniqueID",  "name", "code", "codeType", "searchFor")])
code_covid_df <- data.frame(code_covid[,c("uniqueID", "name", "code", "codeType", "searchFor")])

code_ICD10_df$uniqueCode <- paste0("ID", code_ICD10_df$uniqueID, ".", code_ICD10_df$code)
code_ATC_df$uniqueCode <- paste0("ID", code_ATC_df$uniqueID, ".", code_ATC_df$code)
code_ATC_df2$uniqueCode <- paste0("ID", code_ATC_df2$uniqueID, ".", code_ATC_df2$code)
code_ATC_df3$uniqueCode <- paste0("ID", code_ATC_df3$uniqueID, ".", code_ATC_df3$code)
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

##############################################
########## THE END OF IMPORT DATA ############
##############################################
# process log2
sink("./resultsCNLLS/p-log2.txt")
# summarise by JID 
co19_T200DT <- data.table(co19_t200_trans_dn)

patient_dt <- co19_T200DT[, .(first_visit=min(RECU_FR_DD), last_visit=max(RECU_FR_DD)), by=.(JID, SEX_TP_CD, AGE_cal, INSUP_TP_CD)]
patient_info <- as.data.frame(patient_dt)
print(dim(patient_dt)); print(colnames(patient_dt))

cat(sprintf("\n patient_info row : %d\n", nrow(patient_info)))

cat(sprintf("\nlength - test JID : %d\n", length(testJID) ))
cat(sprintf("\nlength - STATIN target JID : %d\n", length(targetJID) ))
cat(sprintf("\nlength - ANTI target JID : %d\n", length(targetJID) ))
cat(sprintf("\nlength - IMMUNE target JID : %d\n", length(targetJID) ))

cat(sprintf("\nlength - STATIN hospital JID : %d\n", length(hospitalJID) ))
cat(sprintf("\nlength - ANTI hospital JID : %d\n", length(hospitalJID) ))
cat(sprintf("\nlength - IMMUNE hospital JID : %d\n", length(hospitalJID) ))

cat("\n nrow(patient_info) \n")
print(nrow(patient_info))
patient_info$AGE <- ifelse(patient_info$AGE_cal <= 64, 0, 1)
print(table(patient_info$AGE))

# 열 삭제
# (def) SEX_TP_CD 1:male, 2:female, 9:etc
patient_info <- patient_info[!(patient_info$SEX_TP_CD == 9),]
print(nrow(patient_info))

patient_info$GENDER <- patient_info$SEX_TP_CD - 1
print(table(patient_info$GENDER))

# (def) 건강보험 : medical insurance, 의료급여&보훈 : medical aid
patient_info <- patient_info[(patient_info$INSUP_TP_CD == 4 | patient_info$INSUP_TP_CD == 5 | patient_info$INSUP_TP_CD == 7),]
patient_info$MedInfo <- ifelse(patient_info$INSUP_TP_CD == 4, 0, 1)
print(nrow(patient_info))
print(table(patient_info$MedInfo))

patient_info <- as.data.frame(patient_info[,c("JID", "first_visit", "last_visit", "AGE_cal", "AGE", "GENDER", "MedInfo")])
print(dim(patient_info)); print(colnames(patient_info))

cat("\nAll statin anti and immuno\n")
cat("Age table \n")
print(table(patient_info$AGE))
cat("Gender table \n")
print(table(patient_info$GENDER))
cat("Medical Info table \n")
print(table(patient_info$MedInfo))

cat("\nHospital statin, anti and immuno\n")
cat("Age table \n")
print(table(patient_info[patient_info$JID %in% hospitalJID, ]$AGE))
cat("Gender table \n")
print(table(patient_info[patient_info$JID %in% hospitalJID, ]$GENDER))
cat("Medical Info table \n")
print(table(patient_info[patient_info$JID %in% hospitalJID, ]$MedInfo))
sink() 

# flow chart N #
sink("./resultsCNLLS/flow-chart.txt")
cat(sprintf("\n TEST : %d\n", length(testJID) ))

cat(sprintf("\n CONFIRMED : %d\n", length(confirmedJID) ))
cat(sprintf("\n CONFIRMED & under 40 ages : %d\n", length(intersect(under40JID, confirmedJID) )) )
cat(sprintf("\n CONFIRMED & under 20 ages : %d\n", length(intersect(under40JID, confirmedJID) )) )

sink() 

####################################################################
#####################      Function Def      #######################
####################################################################
# process function
verbose.period = 200
gen_dummies <- function(codeBook, study = NULL, Bdays = 0, After=FALSE) {
  time1 = Sys.time()
  df_rule <- NULL
  print("[START] > Bdays for loop; jid in patient_info$JID \n")
  if (After) {
    for (jid in patient_info$JID) {
      verbose = (jid %% verbose.period == 1)
      
      df_temp <- co19_twjhe200_trans_dn[which(co19_twjhe200_trans_dn$JID == jid),]
      
      if(verbose) cat(sprintf(" [V] nrow(df_temp) = %d \n", nrow(df_temp)))
      if(!nrow(df_temp)) next
      
      first <- patient_info2[patient_info2$JID == jid,]$first_visit
      exposure_rule <- df_temp$RECU_FR_DD >= first 
      
      if (unique(codeBook$codeType) == "ATC") {
        # ATC Confounder period : Bdays
        confounder_rule <-  (df_temp$RECU_FR_DD < first) & (df_temp$RECU_FR_DD >= first - Bdays)
      } else{
        # Como Confounder period : 3 years
        confounder_rule <-  (df_temp$RECU_FR_DD < first) & (df_temp$RECU_FR_DD >= first - 1096)
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
    final <- data.frame(patient_info2)
  } else{
    if (Bdays!=0) {
      for (jid in patient_info$JID) {
        verbose = (jid %% verbose.period == 1)
        
        df_temp <- co19_twjhe200_trans_dn[which(co19_twjhe200_trans_dn$JID == jid),]
        
        if(verbose) cat(sprintf(" [V] nrow(df_temp) = %d \n", nrow(df_temp)))
        if(!nrow(df_temp)) next
        
        first <- patient_info[patient_info$JID == jid,]$first_visit
        exposure_rule <- (df_temp$RECU_FR_DD <= first) & (df_temp$RECU_FR_DD >= first - Bdays)
        
        if (study=="statin.study"){
          if (unique(codeBook$codeType) == "ATC") {
            # (statin) ATC Confounder period : 360
            confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays) & (df_temp$RECU_FR_DD >= first - (Bdays+360))
          } else{
            # (statin) 상병은 -3y
            confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays) & (df_temp$RECU_FR_DD >= first - 1096)
          }
        } else {
          if (unique(codeBook$codeType) == "ATC") {
            # ATC Confounder period : 120
            confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays) & (df_temp$RECU_FR_DD >= first - (Bdays+120) )
          } else{
            # 상병은 -3y
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
  }
  
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
        searchJID <- df[ifelse(toupper(df[,var_name]) %in% mapped_var1, TRUE, FALSE),]$JID
        
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
  cat("\n [Total Time] : ", time2-time1); cat("\n")
  
  return (data.frame(final))
}
save_freq <- function(df_name) {
  cat(" <----------------------------> \n")
  df <- get(sprintf("%s",df_name))
  cat(sprintf("  (Dim) # of JID = %d, code = %d \n", dim(df)[1], dim(df)[2]))
  print(colnames(df))
  code_freq <- colSums(df[,-excl.cols])
  code_freq_per <- round(code_freq/nrow(df[,-excl.cols]),6)
  
  cat(sprintf(" [save] %s frequency \n", df_name))
  write.csv(cbind(code_freq, code_freq_per), sprintf("./resultsCNLLS/frequency/%s_frequency.csv", df_name), row.names=T)
  cat("\n")
}
frequency3 <- function(df, study.exposure){
  #study.exposure <- as.character(study[study$type == "Exposure","var"])
  #study.exposure <- as.character(study.exposure)
  for (EachName in colnames(df)[3:ncol(df)]) {
    if( EachName %in% c("last_visit", "first_visit", "AGE_cal") ) {
      next
    }
    cat(sprintf(" --- %s --- \n",EachName))
    print(table("Exposure"=df[,study.exposure], df[,EachName]))
    cat("\n\n")
  }
}

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
  
  z <- cbind(x,y)
  name_freq <- colSums(z)
  name_freq_per <- round(colMeans(z),6)
  #print(colMeans(x))
  write.csv(cbind(name_freq, name_freq_per), sprintf("./resultsCNLLS/frequency2/[Before]name_freq_%s.csv", study))
  cat(sprintf(" (1) [save] %s codewise frequency \n", study))
  
  como_df_agg2 = t( apply(como_df2, 1, function(x) tapply(x, code_ICD10_df$diagnosis, vec_or) ) )
  como_df_agg2 <- data.frame(como_df_agg2)
  
  ATC_df_agg2 = t( apply(ATC_df2, 1, function(x) tapply(x, code_ATC_df_tmp$use, vec_or) ) )
  ATC_df_agg2 <- data.frame(ATC_df_agg2)
  
  x2 <- cbind(como_df_agg2, ATC_df_agg2)
  name_freq <- colSums(x2)
  name_freq_per <- round(colMeans(x2),6)
  write.csv(cbind(name_freq, name_freq_per), sprintf("./resultsCNLLS/frequency3/[Before]use_diag_freq_%s.csv", study))
  cat(sprintf(" (2) [save] %s use-diag frequency \n", study))
  
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
  write.csv(round(asso.mat,6), sprintf("./resultsCNLLS/frequency2/[Before]asso_%s.csv", study) )
  cat(sprintf(" (3) [save] %s marginal association \n", study))
  
  return (data.frame(cbind(como_df[ ,excl.cols],x,y)))
}
make_df2 <- function(como_df, ATC_df, outcome_df, study) {
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
  code_ATC_df_tmp = code_ATC_df2[code_ATC_df2$uniqueCode %in% colnames(ATC_df2), ]
  
  if (ncol(ATC_df2) != nrow(code_ATC_df_tmp)) stop("[2] Code names do not match!!!")
  
  ATC_df_agg = t( apply(ATC_df2, 1, function(x) tapply(x, code_ATC_df_tmp$name, vec_or) ) )
  ATC_df_agg <- data.frame(ATC_df_agg)
  
  # covid_outcomes_agg
  if (ncol(outcome_df[ ,-excl.cols]) != nrow(code_outcomes_df)) stop("Code names do not match!!!")
  
  outcomes_df_agg = t( apply(outcome_df[ ,-excl.cols], 1, function(x) tapply(x, code_outcomes_df$name, vec_or) ) )
  outcomes_df_agg <- data.frame(outcomes_df_agg)
  
  x <- cbind(como_df_agg, ATC_df_agg)
  y <- outcomes_df_agg
  
  z <- cbind(x,y)
  name_freq <- colSums(z)
  name_freq_per <- round(colMeans(z),6)
  #print(colMeans(x))
  write.csv(cbind(name_freq, name_freq_per), sprintf("./resultsCNLLS/frequency2/[After]name_freq_%s.csv", study))
  cat(sprintf(" (1) [save] %s codewise frequency \n", study))
  
  como_df_agg2 = t( apply(como_df2, 1, function(x) tapply(x, code_ICD10_df$diagnosis, vec_or) ) )
  como_df_agg2 <- data.frame(como_df_agg2)
  
  ATC_df_agg2 = t( apply(ATC_df2, 1, function(x) tapply(x, code_ATC_df_tmp$use, vec_or) ) )
  ATC_df_agg2 <- data.frame(ATC_df_agg2)
  
  x2 <- cbind(como_df_agg2, ATC_df_agg2)
  name_freq <- colSums(x2)
  name_freq_per <- round(colMeans(x2),6)
  write.csv(cbind(name_freq, name_freq_per), sprintf("./resultsCNLLS/frequency3/[After]use_diag_freq_%s.csv", study))
  cat(sprintf(" (2) [save] %s use-diag frequency \n", study))
  
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
  write.csv(round(asso.mat,6), sprintf("./resultsCNLLS/frequency2/[After]asso_%s.csv", study) )
  cat(sprintf(" (3) [save] %s marginal association \n", study))
  
  return (data.frame(cbind(como_df[ ,excl.cols],x,y)))
}

# modeling function
ps.weight <- function(exposure, name, ps, h=0.06, ps.cut=0.01){
  ind.trt0 <- which(exposure == 0)
  ind.trt1 <- which(exposure == 1)
  
  # PS summary
  cat("\nPropensity Score Summary\n\n")
  NonUser <- ps[ind.trt0]
  User <- ps[ind.trt1]
  
  cat(sprintf("- NonUser \n (<ps.cut) : %d\n (>(1-ps.cut)) : %d\n", 
              sum(NonUser<ps.cut), sum(NonUser>(1-ps.cut)) )) 
  print(summary(NonUser)); cat("\n")
  
  cat(sprintf("- User \n (<ps.cut) : %d\n (>(1-ps.cut)) : %d\n", 
              sum(User<ps.cut), sum(User > (1-ps.cut)) ))
  print(summary(User)); cat("\n")
  
  propensity_name0 <- sprintf("./resultsCNLLS/ps/[%s][propensity]_NonUser.csv", name)
  propensity_name1 <- sprintf("./resultsCNLLS/ps/[%s][propensity]_User.csv", name)
  
  NonUserFreq <- data.frame(table(round(NonUser,4)))
  UserFreq <- data.frame(table(round(User,4)))
  
  write.csv(NonUserFreq, propensity_name0)
  write.csv(UserFreq, propensity_name1)
  
  # initialize to 1 (?)
  iptw.wt <- rep(1, length(exposure))
  trim.wt <- rep(1, length(exposure))
  st.iptw.wt <- rep(1, length(exposure))
  
  iptw.att <- rep(1, length(exposure))
  trim.att <- rep(1, length(exposure))
  st.iptw.att <- rep(1, length(exposure))
  
  dec.wt <- rep(1, length(exposure))
  match.wt <- rep(1, length(exposure))
  kernel.wt <- rep(1, length(exposure))
  
  final <- cbind(exposure, ps)
  wt.check <- function(wt) {
    
    # (1) 10,000 이상은 절단
    wt[wt>10000] <- 10000
    
    # (2) 0.00001 이하는 0으로 취급
    wt[wt<0.00001] <- 0
    
    # (3) all same wt 에러 발생
    rule1 <- ( length(unique(wt)) == 1)
    if (rule1) stop(e)
    
    # (4) all zero wt 에러 발생
    rule2 <- (sum(wt) == 0)
    if (rule2) stop(e)
    
    return ( c(wt) )
  }
  ########## [ ATE ] ##########
  # iptw weight
  cat("\n - iptw - \n")
  tryCatch( {
    
    iptw.wt <- ( 1/ps )*exposure + ( 1/(1-ps) )*( 1-exposure )
    
    cat("\n\n")
    cat(" - iptw.wt dist\n\n")
    print(summary(iptw.wt[ind.trt0]))
    print(summary(iptw.wt[ind.trt1]))
    cat("\n\n") 
    
    iptw.wt <- wt.check(iptw.wt)
    final <- cbind(final, iptw.wt)
    
  },
  error = function(e) print("error"),
  finally = function(f) print("success") )
  
  # iptw weight with trimming
  cat("\n - iptw with trimming - \n")
  tryCatch( {
    
    trim.wt <- iptw.wt
    trim.wt[ps<ps.cut | ps>(1-ps.cut)] <- 0
    
    cat("\n\n")
    cat(" - trim.wt dist\n\n")
    print(sum(trim.wt==0))
    
    print(summary(trim.wt[ind.trt0]))
    print(summary(trim.wt[ind.trt1]))
    cat("\n\n")
    
    trim.wt <- wt.check(trim.wt)
    
    if (sum(trim.wt==0)!=length(trim.wt)){
      final <- cbind(final, trim.wt)
    } else {
      cat(" - ALL weights with trimming are 0.\n")
    }
  },
  error = function(e) print("error"),
  finally = function(f) print("success") )
  
  # stabilized iptw
  cat("\n - Stabilized iptw- \n")
  tryCatch( {
    
    p.exposure <- sum(exposure) / length(exposure)
    st.iptw.wt <- ( p.exposure/ps )*exposure + ( (1-p.exposure)/(1-ps) )*( 1-exposure )
    
    cat("\n\n")
    cat(" - st.iptw.wt dist\n\n")
    print(summary(st.iptw.wt[ind.trt0]))
    print(summary(st.iptw.wt[ind.trt1]))
    cat("\n\n")
    
    st.iptw.wt <- wt.check(st.iptw.wt)
    
    final <- cbind(final, st.iptw.wt)
    
  },
  error = function(e) print("error"),
  finally = function(f) print("success") )
  
  
  ########## [ ATT ] ########## 
  # standardized mortality ratio weighting == iptw att
  cat("\n - iptw ATT - \n")
  tryCatch( {
    
    iptw.att <- ps/(1-ps)
    iptw.att[ind.trt1] <- 1
    
    cat("\n\n")
    cat(" - iptw.att dist\n\n")
    print(summary(iptw.att[ind.trt0]))
    print(summary(iptw.att[ind.trt1]))
    cat("\n\n")
    
    iptw.att <- wt.check(iptw.att)
    
    final <- cbind(final, iptw.att)
    
  },
  error = function(e) print("error"),
  finally = function(f) print("success") )
  
  # standardized mortality ratio weighting with trimming
  cat("\n - iptw ATT with trimming - \n")
  tryCatch( {
    
    trim.att <- iptw.att
    trim.att[ps<ps.cut | ps>(1-ps.cut)] <- 0
    
    cat("\n\n")
    cat(" - trim.att dist\n\n")
    print(summary(trim.att[ind.trt0]))
    print(summary(trim.att[ind.trt1]))
    cat("\n\n")
    
    trim.att <- wt.check(trim.att)
    
    if (sum(trim.att==0)!=length(trim.att)){
      final <- cbind(final, trim.att)
    } else {
      cat(" - ALL weights with trimming are 0.\n")
    }
  },
  error = function(e) print("error"),
  finally = function(f) print("success") )
  
  # stabilized iptw ATT
  cat("\n - stabilized iptw ATT - \n")
  tryCatch( {
    
    st.iptw.att <- ( (1-p.exposure)/p.exposure ) * ( ps / (1-ps) ) * (1-exposure)
    st.iptw.att[ind.trt1] <- 1
    
    cat("\n\n")
    cat(" - st.iptw.att dist\n\n")
    print(summary(st.iptw.att[ind.trt0]))
    print(summary(st.iptw.att[ind.trt1]))
    cat("\n\n")
    
    st.iptw.att <- wt.check(st.iptw.att)
    
    final <- cbind(final, st.iptw.att)
    
  },
  error = function(e) print("error"),
  finally = function(f) print("success") )
  
  # [Matching]
  # Deciles
  ####################
  # error check code #
  ####################
  cat("\n - Deciles ATT - \n")
  tryCatch( {
    ps.dec <- cut(ps,
                  breaks=c(quantile(ps, probs=seq(0,1,0.1))),
                  labels=seq(1:10), include.lowest=TRUE)
    
    cat("\n"); print(table(ps.dec)); cat("\n")
    cat("\n"); print(table(ps.dec, exposure)); cat("\n")
    
    dec_info <- paste0(exposure, "-", ps.dec)
    dec <- data.frame( cbind( dec_info, 
                              id=1:length(exposure) ) )
    cat("\n"); print(table(dec$dec_info)); cat("\n");
    
    Ntotal <- length(exposure)
    Nexposed <- sum(exposure)
    Nreference <- Ntotal - Nexposed
    
    dec_fr <- summary(ps.dec)
    dec_ex <- data.frame(table("strata"=ps.dec, exposure),
                         "Nstratum"=c(dec_fr, dec_fr))
    cat("\n"); print(dec_ex); cat("\n"); 
    dec_ex$dec_info <- paste0(dec_ex$exposure, "-", dec_ex$strata)
    
    dec_ex$Ngroup <- 0
    dec_ex[dec_ex$exposure==0, ]$Ngroup <- Nreference
    dec_ex[dec_ex$exposure==1, ]$Ngroup <- Nexposed
    
    dec_ex$dec.wt <- ( dec_ex$Ngroup / Ntotal) * ( dec_ex$Nstratum / dec_ex$Freq)
    
    dec_ex$exposure <- NULL; dec_ex$ps.dec<-NULL;
    # merge 하면 순서가 섞임.
    dec_info2 <- merge(dec, dec_ex, by="dec_info")
    dec_info2$id <- as.numeric(dec_info2$id)
    dec.wt <- dec_info2[order(dec_info2$id),]$dec.wt
    print(table(dec.wt))
    
    cat("\n\n")
    cat(" - dec.wt dist\n\n")
    print(summary(dec.wt[ind.trt0]))
    print(summary(dec.wt[ind.trt1]))
    cat("\n\n")
    
    dec.wt <- wt.check(dec.wt)
    
    final <- cbind(final, dec.wt)
  },
  error = function(e) print("error"),
  finally = function(f) print("success") )
  
  # One to One Matching
  cat("\n - Matching ATT - \n")
  trt0.ps = ps[ind.trt0]
  trt1.ps = ps[ind.trt1]
  
  propDiffMat = outer(trt1.ps, trt0.ps, "-")
  propDiffMat = abs(propDiffMat)
  
  tryCatch( {
    # index 를 이름으로 붙여주기
    rownames(propDiffMat) = ind.trt1
    colnames(propDiffMat) = ind.trt0
    
    matchMat = t( apply(propDiffMat, 1, 
                        function(t) { a = rep(0,length(t)); a[order(t) <= 5] = 1/5; return(a) } ) )
    
    # matchMat을 column-sum하면, control group의 individual별로 weight가 나온다.
    # exposure group은 모두 1로 주면 된다.
    match.wt[ind.trt1] = 1
    match.wt[ind.trt0] = colSums(matchMat)
    cat("\n check & matching weight \n")
    print(table(match.wt)); print(table(exposure));
    
    cat("\n\n")
    cat(" - match.wt dist\n\n")
    print(summary(match.wt[exposure==0]))
    print(summary(match.wt[exposure==1]))
    cat("\n\n")
    
    match.wt <- wt.check(match.wt)
    
    final <- cbind(final, match.wt)
    
  },
  error = function(e) print("error"),
  finally = function(f) print("success") )
  
  # Kernel
  cat("\n - Kernel Matching ATT - \n")
  tryCatch( {
    
    propDiffMat.k = propDiffMat/h  
    
    rownames(propDiffMat.k) = ind.trt1
    colnames(propDiffMat.k) = ind.trt0
    
    matchMat.k = exp(- propDiffMat.k^2 / 2)
    matchMat.k <- matchMat.k / rowSums(matchMat.k)
    
    kernel.wt[ind.trt0] = colSums(matchMat.k)
    kernel.wt[ind.trt1] = 1
    cat("\n check & matching weight \n")
    print(table(round(kernel.wt,4))); print(table(exposure));
    
    cat("\n\n")
    cat(" - kernel.wt dist\n\n")
    print(summary(kernel.wt[exposure==0]))
    print(summary(kernel.wt[exposure==1]))
    cat("\n\n")
    
    kernel.wt <- wt.check(kernel.wt)
    
    final <- cbind(final, kernel.wt)
    
  },
  error = function(e) print("error"),
  finally = function(f) print("success") )
  
  final <- data.frame(final)
  
  # WT save
  finalNonUser <- final[final$exposure==0, ]
  finalUser <- final[final$exposure==1, ]
  
  wt_name0 <- sprintf("./resultsCNLLS/ps/[%s][ALL_WEIGHT]_NonUser.csv", name)
  wt_name1 <- sprintf("./resultsCNLLS/ps/[%s][ALL_WEIGHT]_User.csv", name)
  
  write.csv(round(finalNonUser,4), wt_name0)
  write.csv(round(finalUser,4), wt_name1)
  
  return ( final )
}
outcome.model <- function(Y="outcome1", wt_df, df, study, name){
  save_path <- sprintf("./resultsCNLLS/model/%s_coef/", name)
  dir.create(save_path, showWarnings=FALSE)
  cat("\n\n ================================== \n\n")
  cat(name); cat("\n"); cat(Y)
  cat("\n"); cat(colnames(wt_df))
  cat("\n\n ================================== \n\n")
  
  study.exposure <- as.character(study[study$type == "Exposure","var"])
  study.confounder <- as.character(study[study$type == "Confounder","var"])
  study.Effect.Modifier <- as.character(study[study$type == "Effect.Modifier", "var"])
  
  Exposure <- df[,study.exposure]
  Confounder <- df[,study.confounder]
  Outcome <- df[,as.character(Y)]
  X <- df[,c(study.exposure, "AGE", "GENDER", "MedInfo", "Hypertension", "Diabetes.mellitus")]
  
  # ---------- table 2 ---------- #
  # 6. Risk of adverse clinical outcomes associated with Exposure ~ .
  # (1) # of patients (2) # of events (3) Event Rates (%)
  cat(sprintf("\n %s TABLE \n\n", Y))
  df.User <- df[Exposure==1,]
  df.NonUser <- df[Exposure==0,]
  
  OutcomeTable <- data.frame(Index=c("NonUser", "User"),
                             N_Patients=c(nrow(df.NonUser), nrow(df.User)),
                             N_Events=c(sum(df.NonUser[, Y]),sum(df.User[, Y])),
                             EventRates=c(sum(df.NonUser[, Y])/nrow(df.NonUser), sum(df.User[, Y])/nrow(df.User) ) )
  print(OutcomeTable); cat("\n")
  outcome_path <- paste0(save_path, Y, "_Incidence.csv")
  write.csv(OutcomeTable, outcome_path)
  
  if (!is.null(wt_df$trim.wt)) {
    df.trim <- df[wt_df$trim.wt!=0,]
    Exposure.trim <- Exposure[wt_df$trim.wt!=0]
    df.User2 <- df.trim[Exposure.trim==1,]
    df.NonUser2 <- df.trim[Exposure.trim==0,]
    print(sum(wt_df$trim.wt==0))
    OutcomeTable.T <- data.frame(Index=c("NonUser", "User"),
                                 N_Patients=c(nrow(df.NonUser2), nrow(df.User2)),
                                 N_Events=c(sum(df.NonUser2[, Y]),sum(df.User2[, Y])),
                                 EventRates=c(sum(df.NonUser2[, Y])/nrow(df.NonUser2), sum(df.User2[, Y])/nrow(df.User2) ) )
    print(OutcomeTable.T); cat("\n")
    outcome_path.T <- paste0(save_path, Y, "_Incidence_TRIMMING.wt.csv")
    write.csv(OutcomeTable.T, outcome_path.T)
  }
  
  # [0-1] outcome ~ exposure (NAIVE)
  fit0.1 <- glm(Outcome ~ Exposure, family="binomial")
  
  cat(" [0-1] Naive Primary Outcome Model \n\n")
  print(summary(fit0.1))
  
  
  Coef_0.1 <- data.frame("coef" = exp(summary(fit0.1)$coef[,1]), # 계수  
                         "s.e" = summary(fit0.1)$coef[,2], # 표준 오차
                         "lower.ci" = exp(summary(fit0.1)$coef[,1] - 1.96 * summary(fit0.1)$coef[,2]), # CI
                         "upper.ci" = exp(summary(fit0.1)$coef[,1] + 1.96 * summary(fit0.1)$coef[,2]),
                         "p-value" = summary(fit0.1)$coef[,4])
  print(Coef_0.1)
  
  path0.1 <- paste0(save_path, sprintf("[0-1]%s_Naive.csv", Y))
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
  print(Coef_0.2)
  
  path0.2 <- paste0(save_path, sprintf("[0-2]%s_MultiNaive.csv", Y))
  write.csv(Coef_0.2, path0.2)
  
  wt_names <- c("iptw.wt", "trim.wt", "st.iptw.wt", 
                "iptw.att", "trim.att", "st.iptw.att",
                "dec.wt", "match.wt", "kernel.wt")
  
  # [1] Outcome adjustment model
  Adjust <- data.frame(A=Exposure, PS=wt_df$ps)
  fit1 <- glm(Outcome~., data=Adjust, family="binomial")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [1] Outcome adjustment model \n\n")
  print(summary(fit1))
  
  Coef_1 <- data.frame("coef" = exp(summary(fit1)$coef[,1]), # 계수  
                       "s.e" = summary(fit1)$coef[,2], # 표준 오차
                       "lower.ci" = exp(summary(fit1)$coef[,1] - 1.96 * summary(fit1)$coef[,2]), # CI
                       "upper.ci" = exp(summary(fit1)$coef[,1] + 1.96 * summary(fit1)$coef[,2]),
                       "p-value" = summary(fit1)$coef[,4])
  print(Coef_1)
  
  for (wt in wt_names){
    # [2] outcome model (main)
    #   (a) outcome1 ~ exposure (wt 1a)
    #if (wt2) : 없으면 next 추가필요 
    if (!(wt %in% colnames(wt_df))) {
      cat(sprintf("\n %s : generate X\n",wt))
      next
    }
    wt2 <- wt_df[,wt]
    
    fit2a <- glm(Outcome~Exposure, weight = wt2, family="binomial")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [2] Primary Outcome Model (a) "); cat(wt); cat("\n\n")
    print(summary(fit2a)); cat("\n\n"); print(vcov(fit2a)); cat("\n\n")
    Coef_2a <- data.frame("coef" = exp(summary(fit2a)$coef[,1]), # 계수  
                          "s.e" = summary(fit2a)$coef[,2], # 표준 오차
                          "lower.ci" = exp(summary(fit2a)$coef[,1] - 1.96 * summary(fit2a)$coef[,2]), # CI
                          "upper.ci" = exp(summary(fit2a)$coef[,1] + 1.96 * summary(fit2a)$coef[,2]),
                          "p-value" = summary(fit2a)$coef[,4])
    print(Coef_2a)
    
    tryCatch({
      cat("\n (*) GEE variance \n\n"); 
      Xmat <- cbind(1,Exposure)
      beta <- fit2a$coef
      m.vec <- exp(Xmat %*% beta)/( 1+exp(Xmat %*% beta) )
      s0 <- t(Xmat) %*% diag(c(m.vec*(1-m.vec)*wt2)) %*% Xmat
      s1 <- t(Xmat) %*% diag(c((Outcome-m.vec)*wt2)^2) %*% Xmat
      s <- solve(s0) %*% s1 %*% solve(s0)
      print(s); cat("\n\n");
      gee.se <- c(sqrt(s[1,1]), sqrt(s[2,2]))
      
      Coef_2a.gee <- data.frame("coef" = exp(summary(fit2a)$coef[,1]), # 계수  
                                "s.e" = gee.se, # 표준 오차
                                "lower.ci" = exp(summary(fit2a)$coef[,1] - 1.96 * gee.se), # CI
                                "upper.ci" = exp(summary(fit2a)$coef[,1] + 1.96 * gee.se),
                                "p-value" = (1-pnorm(abs(beta/gee.se)))*2) # p-norm check.
      
      cat("\n") ; print(Coef_2a.gee); cat("\n")
      path2a <- paste0(save_path, sprintf("[2][%s]%s_main.csv", wt, Y))
      write.csv(Coef_2a.gee, path2a)
    },
    warning=function(w) cat(" : warnings \n"),
    error=function(e) cat(" : Failed\n"),
    finally=function(f) cat(" : Succeed\n"))
  }
  
  OutcomeTable2 <- NULL;
  for (v in study.Effect.Modifier){
    cat(sprintf("\n\n [3] Effect Modifier Primary Outcome Model : %s \n\n", v))
    
    df.UserO <- df.User[df.User[,v]==1,]
    df.UserX <- df.User[df.User[,v]==0,]
    
    df.NonUserO <- df.NonUser[df.NonUser[,v]==1,]
    df.NonUserX <- df.NonUser[df.NonUser[,v]==0,]
    
    tmpTable <- data.frame(Index = c(paste0(v,"-X"), paste0(v,"-O")),
                           N_patients_User=c(nrow(df.UserX), nrow(df.UserO)),
                           N_patients_NonUser=c(nrow(df.NonUserX), nrow(df.NonUserO)),
                           N_events_User=c(sum(df.UserX[,Y]), sum(df.UserO[,Y])),
                           N_events_NonUser=c(sum(df.NonUserX[,Y]), sum(df.NonUserO[,Y])),
                           N_Incidence_User=c(sum(df.UserX[, Y])/nrow(df.UserX), sum(df.UserO[, Y])/nrow(df.UserO) ),
                           N_Incidence_NonUser=c(sum(df.NonUserX[, Y])/nrow(df.NonUserX), sum(df.NonUserO[, Y])/nrow(df.NonUserO)))
    
    OutcomeTable2 <- rbind(OutcomeTable2, tmpTable)
    
    print(table("v"=df.User[,v], "Y"=df.User[,Y]))
    print(table("v"=df.NonUser[,v], "Y"=df.NonUser[,Y]))
    print(tmpTable)
    
    EffectModifier <- df[,c(study.exposure, v)]
    EffectModifier$AV <- Exposure * df[,v]
    
    ########## REFIT Separately ##########
    study.sub.confounder <- setdiff(study.confounder, v)
    cat("\nSubgroup analysis Confounder : \n")
    print(study.sub.confounder)
    sub.Confounder <- df[,study.sub.confounder]
    
    sub.fit <- glm(Exposure~., data=sub.Confounder, family="binomial")
    sub.ps <- sub.fit$fitted.values
    sub.name <- sprintf("[%s][Subgroup-%s]", name, v)
    
    sub.wt <- ps.weight(exposure=Exposure, name=sub.name, ps=sub.fit$fitted.values)
    sub_wt_names <- intersect(colnames(sub.wt), wt_names)
    
    cat("\n"); print(sub_wt_names); cat("\n")
    ######################################
    for (wt in sub_wt_names){
      cat("\n\n"); cat(wt); cat("\n\n")
      #if (wt2) : 없으면 next 추가필요 
      
      wt2 <- sub.wt[,wt]
      # outcome1 ~ exposure + v + exposure*v (iptw.wt)
      fit3_v <- glm(Outcome~., data=EffectModifier, weight=wt2, family="binomial")
      print(summary(fit3_v)); cat("\n\n"); print(vcov(fit3_v)); cat("\n\n")
      
      
      cat("\n (*) GEE variance \n\n"); 
      tryCatch({
        beta <- fit3_v$coef
        beta <- beta[!is.na(beta)]; p <- length(names(beta))
        Xmat <- as.matrix(cbind(1, EffectModifier[, names(beta)[2:p] ]))
        
        m.vec <- exp(Xmat %*% beta)/( 1+exp(Xmat %*% beta) )
        
        s0 <- t(Xmat) %*% diag(c(m.vec*(1-m.vec)*wt2)) %*% Xmat
        s1 <- t(Xmat) %*% diag(c((Outcome-m.vec)*wt2)^2) %*% Xmat
        s <- solve(s0) %*% s1 %*% solve(s0)
        print(s); cat("\n\n");
        gee.se <- c(sqrt(s[1,1]))
        for (i in 2:p){
          gee.se <- c(gee.se, sqrt(s[i,i])) 
        }
        
        Coef_3_v <- data.frame("coef" = exp(summary(fit3_v)$coef[,1]), # 계수  
                               "s.e" = gee.se, # 표준 오차
                               "lower.ci" = exp(summary(fit3_v)$coef[,1] - 1.96 * gee.se), # CI
                               "upper.ci" = exp(summary(fit3_v)$coef[,1] + 1.96 * gee.se),
                               "p-value" = (1-pnorm(abs(beta/gee.se)))*2)
        
        print(Coef_3_v)
        E.mod1 <- sprintf("[3-1][%s]%s_EffectModifier_%s.csv", wt, Y, v)
        E.mod2 <- sprintf("[3-2][%s]Cov_%s_EffectModifier_%s.csv", wt, Y,v)
        
        write.csv(Coef_3_v, paste0(save_path, E.mod1))
        write.csv(s, paste0(save_path, E.mod2))
        
        if (p == 4){
          exp.b0 <- Coef_3_v[1,1]; var.b0 <- s[1,1]; se.b0 <- sqrt(s[1,1])
          exp.b1 <- Coef_3_v[2,1]; var.b1 <- s[2,2]; se.b1 <- sqrt(s[2,2])
          exp.b2 <- Coef_3_v[3,1]; var.b2 <- s[3,3]; se.b2 <- sqrt(s[3,3])
          exp.b3 <- Coef_3_v[4,1]; var.b3 <- s[4,4]; se.b3 <- sqrt(s[4,4])
          
          cat(sprintf("(v=0) y=%.2f+%.2fX\n", (exp.b0), (exp.b1)))
          cat(sprintf("(v=1) y=%.2f+%.2fX\n", (exp.b0*exp.b2), (exp.b1*exp.b3)))
          cov.b1.b3 <- s[2,4]
          
          Em.exp <- exp.b1 * exp.b3
          Em.se <- sqrt( var.b1 + var.b3 + 2*cov.b1.b3 )
          
          dTmp <- data.frame("exp.coef"=c(exp.b1, Em.exp),
                             "s.e"=c(se.b1, Em.se),
                             "lower.ci"=c(exp.b1/exp(1.96*se.b1), Em.exp/exp(1.96*Em.se)),
                             "upper.ci"=c(exp.b1*exp(1.96*se.b1), Em.exp*exp(1.96*Em.se)))
          print(dTmp)
          
        } else {
          next
        }
      },
      error=function(e) cat("\n : Failed\n"),
      finally=function(f) cat("\n : Succeed\n"))
      
    }
  }
  outcome_path2 <- paste0(save_path, Y, "_Subgroup_Incidence.csv")
  print(OutcomeTable2)
  write.csv(OutcomeTable2, outcome_path2)
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" - * - * - * - * - * - * - * - * - * - * - \n")
}

# print function
cal.aSD <- function(g1.m, g2.m){
  g1.var <- g1.m*(1-g1.m)
  g2.var <- g2.m*(1-g2.m)
  return( abs(g1.m - g2.m) / sqrt( ( g1.var + g2.var )/2 ) )
}
# df.all = statin120; wt ; df.study=statin.study; save_name="statin120" 
print_table <- function(df.all, wt, df.study, save_name){
  if (is.null(wt)) {
    cat("This weight was not defined")
    return (0)
  }
  cat("\n\n -- PRINT : "); cat(save_name); cat("\n\n")
  agebreaks <- c(-5, 30, 40, 50, 60, 70, 80, 90, 500)
  agelabels <- c("30-", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90+")
  
  df.all$AGE2 = cut(df.all$AGE_cal, breaks = agebreaks,
                    right = FALSE, labels = agelabels)
  
  df.all <- data.frame(df.all)
  
  # 이름 받아오기 # 
  exposure <- as.character( df.study[df.study$type=="Exposure","var"] )
  confounder <- as.character( df.study[df.study$type=="Confounder","var"] )
  outcome2 <- as.character( df.study[df.study$type=="Outcome2","var"] )
  #details <- setdiff(colnames(df.all), c(df.study$var,
  #                                       "JID", "AGE_cal", "AGE2", "outcome1", "re.outcome1",
  #                                       "Fracture", "statin2"))
  
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
  
  cha.t <- rbind(N, mean, sd, AgeTable1, AgeTable2, SexTable, MedTable, conTable)
  filename.cha <- paste0("./resultsCNLLS/table/[Table1]characteristics_", save_name, ".csv")
  write.csv(cha.t, filename.cha)
  
  # 5. outcomes
  y_names <- names(df.all)
  y_names2 <- y_names[y_names %in% outcome2]
  
  yTable <- NULL;
  for (name in y_names2) {
    conTmp <- data.frame("Index"=name, 
                         "all"=sum(df.all[,name]), 
                         "User"=sum(df.User[,name]), 
                         "NonUser"=sum(df.NonUser[,name]), 
                         "AfterIPTW.User"=sum(df.User[,name] * User.wt), 
                         "AfterIPTW.NonUser"=sum(df.NonUser[,name] * NonUser.wt) )
    
    yTable <- rbind(yTable, conTmp)
  }
  print(yTable)
  filename.y <- paste0("./resultsCNLLS/table/[Table]weighted_Y_", save_name, ".csv")
  write.csv(yTable, filename.y)
  
  # 6. weighted detail
  
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
  
  conTable2 <- cbind(conTable,
                     "BeforeASD"=con.aSD1, "AfterASD"=con.aSD2)
  filename.con <- paste0("./resultsCNLLS/table/[Table1]confounder_", save_name, ".csv")
  write.csv(conTable2, filename.con)
  
  # ---------- table 2 ---------- #
  # 6. Risk of adverse clinical outcomes associated with Exposure ~ .
  # (1) # of patients (2) # of events (3) Event Rates (%)
  Primary <- data.frame(Index="outcome1",
                        N_Patients_all=nrow(df.all),
                        N_Patients_NonUser=nrow(df.NonUser),
                        N_Patients_User=nrow(df.User),
                        
                        N_Events_NonUser=sum(df.NonUser$outcome1),
                        N_Events_User=sum(df.User$outcome1),
                        
                        Incidence_NonUser=sum(df.NonUser$outcome1)/nrow(df.NonUser),
                        Incidence_User=sum(df.User$outcome1)/nrow(df.User) )
  
  outcomes2 <- as.character( df.study[df.study$type=="Outcome2","var"] )
  
  Secondary <- NULL
  for (outcome2 in outcomes2){
    ind <- paste0(c("NonUser-", "User-"), outcome2)
    secondaryTmp <- data.frame(Index=outcome2,
                               N_Patients_all=nrow(df.all),
                               N_Patients_NonUser=nrow(df.NonUser),
                               N_Patients_User=nrow(df.User),
                               
                               N_Events_NonUser=sum(df.NonUser[,outcome2]),
                               N_Events_User=sum(df.User[,outcome2]),
                               
                               Incidence_NonUser=sum(df.NonUser[,outcome2])/nrow(df.NonUser),
                               Incidence_User=sum(df.User[,outcome2])/nrow(df.User) )
    
    Secondary <- rbind(Secondary, secondaryTmp)
  }
  figure3 <- rbind(Primary, Secondary)
  print(figure3)
  filename_figure3 <- paste0("./resultsCNLLS/table/[Figure3]Incidence_", save_name, ".csv")
  write.csv(figure3, filename_figure3)
}

dist_check <- function(df.all, wt.df, study, save_name){
  wt.df <- data.frame(wt.df)
  study.exposure <- as.character(study[study$type == "Exposure","var"])
  study.confounder <- as.character(study[study$type == "Confounder","var"])
  study.Effect.Modifier <- as.character(study[study$type == "Effect.Modifier", "var"])
  study.outcome2 <- as.character(study[study$type == "Outcome2","var"])
  
  exposure <- df.all[, study.exposure]
  ind.trt0 <- which(exposure == 0); ind.trt1 <- which(exposure == 1)
  df.all <- cbind(df.all, wt.df)
  
  df.NonUser <- df.all[ind.trt0,]
  df.User <- df.all[ind.trt1,]
  
  dist0 <- NULL; dist1 <- NULL
  for (o in c("outcome1", study.outcome2)) {
    t0 <- as.matrix(table("wt"=round(df.NonUser$ps,4), df.NonUser[, o]))
    t1 <- as.matrix(table("wt"=round(df.User$ps,4), df.User[, o]))
    
    dist0 <- cbind(dist0, t0)
    dist1 <- cbind(dist1, t1)
    
  }
  save_path0 <- paste0(sprintf("./resultsCNLLS/ps/[Check]distribution/%s_NonUser", save_name))
  save_path1 <- paste0(sprintf("./resultsCNLLS/ps/[Check]distribution/%s_User", save_name))
  
  write.csv(dist0, save_path0)
  write.csv(dist1, save_path1)
}

####################################################################
#####################       data Before      #######################
####################################################################
# process log 3
sink("./resultsCNLLS/p-log3.txt")

excl.cols = (1:ncol(patient_info))
print(dim(patient_info)); print(colnames(patient_info))

# [1] outcomes (sheet4)
# outcomes code book 에 중복된 셀 존재
cat(" -1- outcomes (sheet4) \n")
covid_outcomes <- gen_dummies(code_outcomes_df)

# -- codewise frequency outcomes 
save_freq("covid_outcomes")

# [3] comorbidties ICD10 (sheet2)
como_info_statin90 <- gen_dummies(code_ICD10_df, "statin.study", 240) #90 -> 240
como_info_Anticoagulants90 <- gen_dummies(code_ICD10_df, "antiplatelet.anticoagulant.study",90)
como_info_immunosuppressant90 <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 90)

como_info_statin120 <- gen_dummies(code_ICD10_df, "statin.study", 360) #120 -> 360
como_info_Anticoagulants120 <- gen_dummies(code_ICD10_df, "antiplatelet.anticoagulant.study",120)
como_info_immunosuppressant120 <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 120)

# -- codewise frequency comorbidities -- #
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
ATC_med_Anticoagulants90 <- gen_dummies(code_ATC_df, "antiplatelet.anticoagulant.study",90)
ATC_med_immunosuppressant90 <- gen_dummies(code_ATC_df, "immune.suppressant.study", 90)

ATC_med_statin120 <- gen_dummies(code_ATC_df, "statin.study", 360)
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

# [2] COVID19 code (sheet5)
covid_diag <- gen_dummies(codeBook=code_covid_df)

# -- codewise frequency covid19
cat(" -4- covid (sheet5) \n")
save_freq("covid_diag")

for (j in (ncol(patient_info)+2):ncol(covid_diag)) {
  cat(sprintf(" → Confirm by HIRA (Y) versus code [%s]\n", colnames(covid_diag)[j]))
  print(table("Y" = covid_diag[ ,"ID1001.Y"], "code" = covid_diag[ ,j]))
  cat("\n")
}

cat(" codewise data (finish) \n")
# covid_diag_agg
colnames(covid_diag)[colnames(covid_diag) == "Y"] = "Confirm"
sink() 

# final dataset (--Before--)
# [1] statin 90
print(" <-------------- statin 90 --------------> \n")
statin90 <- make_df(como_df = como_info_statin90, ATC_df = ATC_med_statin90,
                    outcome_df=covid_outcomes, study = "statin90")

# [2] statin 120
print(" <-------------- statin 120 --------------> \n")
statin120 <- make_df(como_df = como_info_statin120, ATC_df = ATC_med_statin120,
                     outcome_df=covid_outcomes, study = "statin120")

# [3] antiplatelet.anticoagulant.study 90
print(" <-------------- anti 90 --------------> \n")
Anticoagulants90 <- make_df(como_df = como_info_Anticoagulants90, ATC_df = ATC_med_Anticoagulants90,
                            outcome_df=covid_outcomes, study = "Anticoagulants90")

# [4] antiplatelet.anticoagulant.study 120
print(" <-------------- anti 120 --------------> \n")
Anticoagulants120 <- make_df(como_df = como_info_Anticoagulants120, ATC_df = ATC_med_Anticoagulants120,
                             outcome_df=covid_outcomes, study = "Anticoagulants120")

# [5] immune 90
print(" <-------------- immune 90 --------------> \n")
immunosuppressant90 <- make_df(como_df = como_info_immunosuppressant90, ATC_df = ATC_med_immunosuppressant90,
                               outcome_df=covid_outcomes, study = "immunosuppressant90")
# [6] immune 120
print(" <-------------- immune 120 --------------> \n")
immunosuppressant120 <- make_df(como_df = como_info_immunosuppressant120, ATC_df = ATC_med_immunosuppressant120,
                                outcome_df=covid_outcomes, study = "immunosuppressant120")

# dataset check
sink("./resultsCNLLS/d-check1.txt")
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

statin90$V_cancer <- 0 ; statin90[statin90$JID %in% cancer_jid,]$V_cancer <- 1
statin120$V_cancer <- 0 ; statin120[statin120$JID %in% cancer_jid,]$V_cancer <- 1

# malignancy
statin90$Malignancy <- statin90$Malignancy * statin90$V_cancer
statin120$Malignancy <- statin120$Malignancy * statin120$V_cancer

statin90$V_cancer <- NULL
statin120$V_cancer <- NULL

statin90$first_visit <- NULL
statin120$first_visit <- NULL
statin90$last_visit <- NULL
statin120$last_visit <- NULL

statin.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA")
statin.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA", "Use.of.mechanical.ventilation.ICU.admission")

statin90$outcome1 <- ifelse(rowSums(statin90[,statin.outcome1])>0,1,0)
statin120$outcome1 <-  ifelse(rowSums(statin120[,statin.outcome1])>0,1,0)

statin.re.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission")

statin90$re.outcome1 <- ifelse(rowSums(statin90[,statin.re.outcome1])>0,1,0)
statin120$re.outcome1 <-  ifelse(rowSums(statin120[,statin.re.outcome1])>0,1,0)

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
                      data.frame(var = statin.confounder, type = rep("Confounder", length(statin.confounder)) ),
                      data.frame(var = statin.outcome1, type = rep("Outcome1", length(statin.outcome1)) ),
                      data.frame(var = statin.re.outcome1, type = "Re.Outcome1"),
                      data.frame(var = statin.outcome2, type = rep("Outcome2", length(statin.outcome2)) ), 
                      data.frame(var = statin.Emodifier, type = rep("Effect.Modifier",length(statin.Emodifier)) ) )

#### anticoagulants.antiplatelets ####
Anticoagulants90$first_visit <- NULL
Anticoagulants120$first_visit <- NULL
Anticoagulants90$last_visit <- NULL
Anticoagulants120$last_visit <- NULL

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
                               unique(code_ATC_df[code_ATC_df$antiplatelet.anticoagulant.study == "exposure.and.outcome.related",
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

Anticoagulants.re.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission")

Anticoagulants90$re.outcome1 <- ifelse(rowSums(Anticoagulants90[,Anticoagulants.re.outcome1])>0,1,0)
Anticoagulants120$re.outcome1 <- ifelse(rowSums(Anticoagulants120[,Anticoagulants.re.outcome1])>0,1,0)

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
                              data.frame(var = Anticoagulants.re.outcome1, type = "Re.Outcome1"),
                              data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                              data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier", length(Anticoagulants.Emodifier))))

#### immunosuppressant ####
immunosuppressant90$first_visit <- NULL
immunosuppressant120$first_visit <- NULL

immunosuppressant90$last_visit <- NULL
immunosuppressant120$last_visit <- NULL

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

immunosuppressant90$autoimmune.disease <- immunosuppressant90$autoimmune.disease * immunosuppressant90$immunosuppressant2
immunosuppressant120$autoimmune.disease <- immunosuppressant120$autoimmune.disease * immunosuppressant120$immunosuppressant2

immuno_status <- c("ID2374.B20", "ID2375.B21", "ID2376.B22", "ID2377.B23", "ID2378.B24",
                   "ID2296.z94", "ID2297.T86",
                   "ID2285.K50", "ID2286.K51", "ID2287.K52")

immunosuppressant90$immuno.status <- as.numeric(rowSums(como_info_immunosuppressant120[, immuno_status]) > 0)
immunosuppressant120$immuno.status <- as.numeric(rowSums(como_info_immunosuppressant120[, immuno_status]) > 0)

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

# Glucocorticoid 90
immunosuppressant90$Glucocorticoid <- ATC_med_immunosuppressant90$ID3110.H02
immunosuppressant120$Glucocorticoid <- ATC_med_immunosuppressant120$ID3110.H02
## Glucocorticoid
Glucocorticoid90 <- immunosuppressant90[( immunosuppressant90$immunosuppressant == 0) | ( immunosuppressant90$Glucocorticoid == 1 ),]
Glucocorticoid120 <- immunosuppressant120[( immunosuppressant120$immunosuppressant == 0) | ( immunosuppressant120$Glucocorticoid == 1 ),]

Glucocorticoid.confounder <- immunosuppressant.confounder
Glucocorticoid.study <- rbind(data.frame(var = "Glucocorticoid", type = "Exposure"),
                              data.frame(var = immunosuppressant.confounder, type = rep("Confounder", length(immunosuppressant.confounder))),
                              data.frame(var = immunosuppressant.outcome1, type = rep("Outcome1", length(immunosuppressant.outcome1))),
                              data.frame(var = immunosuppressant.outcome2, type = rep("Outcome2", length(immunosuppressant.outcome2))), 
                              data.frame(var = immunosuppressant.Emodifier, type = rep("Effect.Modifier",length(immunosuppressant.Emodifier))))

# hospital defined.
hos.statin90 <- statin90[statin90$JID %in% hospitalJID,]
hos.statin120 <- statin120[statin120$JID %in% hospitalJID,]
hos.Anticoagulants90 <- Anticoagulants90[Anticoagulants90$JID %in% hospitalJID,]
hos.Anticoagulants120 <- Anticoagulants120[Anticoagulants120$JID %in% hospitalJID,]
hos.immunosuppressant90 <- immunosuppressant90[immunosuppressant90$JID %in% hospitalJID,]
hos.immunosuppressant120 <- immunosuppressant120[immunosuppressant120$JID %in% hospitalJID,]

hos.Glucocorticoid90 <- hos.immunosuppressant90[( hos.immunosuppressant90$immunosuppressant == 0) | ( hos.immunosuppressant90$Glucocorticoid == 1 ),]
hos.Glucocorticoid120 <- hos.immunosuppressant120[( hos.immunosuppressant120$immunosuppressant == 0) | ( hos.immunosuppressant120$Glucocorticoid == 1 ),]

sink("./resultsCNLLS/d-check2.txt")
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

sink("./resultsCNLLS/d-check3.txt")
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
frequency3(Glucocorticoid90, "Glucocorticoid")
cat("\n\n ** Glucocorticoid 120 ** \n\n")
frequency3(Glucocorticoid120, "Glucocorticoid")
cat("\n\n ** hos.Glucocorticoid 90 ** \n\n")
frequency3(hos.Glucocorticoid90, "Glucocorticoid")
cat("\n\n ** hos.Glucocorticoid 120 ** \n\n")
frequency3(hos.Glucocorticoid120, "Glucocorticoid")
sink() 

############################################
############   Before statin  ##############
############################################
statin120.con <- statin120[,statin.confounder]
statin120.fit <- glm(statin120$statin ~ ., data = statin120.con, family="binomial")

sink("./resultsCNLLS/model/[Before][ALL]results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin120))
print(statin.study[,c(2,1)])
summary(statin120.fit)
statin120.wt <- ps.weight(ps=statin120.fit$fitted.values, exposure=statin120$statin, name="[Before][ALL]statin120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=statin120.wt, df=statin120, study=statin.study, name="[Before][ALL]statin120") # 0.6249

outcome.model(Y="Death", wt_df=statin120.wt, df=statin120, study=statin.study, name="[Before][ALL]statin120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=statin120.wt, df=statin120, study=statin.study, name="[Before][ALL]statin120")
outcome.model(Y="ICU.admission", wt_df=statin120.wt, df=statin120, study=statin.study, name="[Before][ALL]statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt_df=statin120.wt, df=statin120, study=statin.study, name="[Before][ALL]statin120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=statin120.wt, df=statin120, study=statin.study, name="[Before][ALL]statin120")

outcome.model(Y="re.outcome1", wt_df=statin120.wt, df=statin120, study=statin.study, name="[Before][ALL]statin120") # 0.6249

print_table(df.all=statin120, wt=statin120.wt$iptw.wt, df.study=statin.study, save_name="[Before][ALL]iptw.statin120")
print_table(df.all=statin120, wt=statin120.wt$trim.wt, df.study=statin.study, save_name="[Before][ALL]trim.statin120")
print_table(df.all=statin120, wt=statin120.wt$st.iptw.wt, df.study=statin.study, save_name="[Before][ALL]st.iptw.statin120")
print_table(df.all=statin120, wt=statin120.wt$iptw.att, df.study=statin.study, save_name="[Before][ALL]iptw.att.statin120")
print_table(df.all=statin120, wt=statin120.wt$trim.att, df.study=statin.study, save_name="[Before][ALL]trim.att.statin120")
print_table(df.all=statin120, wt=statin120.wt$st.iptw.att, df.study=statin.study, save_name="[Before][ALL]st.iptw.att.statin120")
print_table(df.all=statin120, wt=statin120.wt$dec.wt, df.study=statin.study, save_name="[Before][ALL]dec.statin120")
print_table(df.all=statin120, wt=statin120.wt$match.wt, df.study=statin.study, save_name="[Before][ALL]match.statin120")
print_table(df.all=statin120, wt=statin120.wt$kernel.wt, df.study=statin.study, save_name="[Before][ALL]kernel.statin120")

sink() 

hos.statin120.con <- hos.statin120[,statin.confounder]
hos.statin120.fit <- glm(hos.statin120$statin ~ ., data = hos.statin120.con, family="binomial")

sink("./resultsCNLLS/model/[Before][HOS]results_hos.statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(hos.statin120))
print(statin.study[,c(2,1)])
summary(hos.statin120.fit)
hos.statin120.wt <- ps.weight(ps=hos.statin120.fit$fitted.values, exposure=hos.statin120$statin, name="[Before][HOS]hos.statin120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.statin120.wt, df=hos.statin120, study=statin.study, name="[Before][HOS]hos.statin120") # 0.6249

outcome.model(Y="Death", wt_df=hos.statin120.wt, df=hos.statin120, study=statin.study, name="[Before][HOS]hos.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=hos.statin120.wt, df=hos.statin120, study=statin.study, name="[Before][HOS]hos.statin120")
outcome.model(Y="ICU.admission", wt_df=hos.statin120.wt, df=hos.statin120, study=statin.study, name="[Before][HOS]hos.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt_df=hos.statin120.wt, df=hos.statin120, study=statin.study, name="[Before][HOS]hos.statin120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=hos.statin120.wt, df=hos.statin120, study=statin.study, name="[Before][HOS]hos.statin120")

outcome.model(Y="re.outcome1", wt_df=hos.statin120.wt, df=hos.statin120, study=statin.study, name="[Before][HOS]hos.statin120") # 0.6249

print_table(df.all=hos.statin120, wt=hos.statin120.wt$iptw.wt, df.study=statin.study, save_name="[Before][HOS]iptw.hos.statin120")
print_table(df.all=hos.statin120, wt=hos.statin120.wt$trim.wt, df.study=statin.study, save_name="[Before][HOS]trim.hos.statin120")
print_table(df.all=hos.statin120, wt=hos.statin120.wt$st.iptw.wt, df.study=statin.study, save_name="[Before][HOS]st.iptw.hos.statin120")
print_table(df.all=hos.statin120, wt=hos.statin120.wt$iptw.att, df.study=statin.study, save_name="[Before][HOS]iptw.att.hos.statin120")
print_table(df.all=hos.statin120, wt=hos.statin120.wt$trim.att, df.study=statin.study, save_name="[Before][HOS]trim.att.hos.statin120")
print_table(df.all=hos.statin120, wt=hos.statin120.wt$st.iptw.att, df.study=statin.study, save_name="[Before][HOS]st.iptw.att.hos.statin120")
print_table(df.all=hos.statin120, wt=hos.statin120.wt$dec.wt, df.study=statin.study, save_name="[Before][HOS]dec.hos.statin120")
print_table(df.all=hos.statin120, wt=hos.statin120.wt$match.wt, df.study=statin.study, save_name="[Before][HOS]match.hos.statin120")
print_table(df.all=hos.statin120, wt=hos.statin120.wt$kernel.wt, df.study=statin.study, save_name="[Before][HOS]kernel.hos.statin120")
sink() 

statin90.con <- statin90[,statin.confounder]
statin90.fit <- glm(statin90$statin ~ ., data = statin90.con, family="binomial")

sink("./resultsCNLLS/model/[Before][ALL]results_statin90.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(statin90))
print(statin.study[,c(2,1)])
summary(statin90.fit)
statin90.wt <- ps.weight(ps=statin90.fit$fitted.values, exposure=statin90$statin, name="[Before][ALL]statin90")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=statin90.wt, df=statin90, study=statin.study, name="[Before][ALL]statin90") # 0.6249

outcome.model(Y="Death", wt_df=statin90.wt, df=statin90, study=statin.study, name="[Before][ALL]statin90")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=statin90.wt, df=statin90, study=statin.study, name="[Before][ALL]statin90")
outcome.model(Y="ICU.admission", wt_df=statin90.wt, df=statin90, study=statin.study, name="[Before][ALL]statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt_df=statin90.wt, df=statin90, study=statin.study, name="[Before][ALL]statin90")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=statin90.wt, df=statin90, study=statin.study, name="[Before][ALL]statin90")

outcome.model(Y="re.outcome1", wt_df=statin90.wt, df=statin90, study=statin.study, name="[Before][ALL]statin90") # 0.6249

print_table(df.all=statin90, wt=statin90.wt$iptw.wt, df.study=statin.study, save_name="[Before][ALL]iptw.statin90")
print_table(df.all=statin90, wt=statin90.wt$trim.wt, df.study=statin.study, save_name="[Before][ALL]trim.statin90")
print_table(df.all=statin90, wt=statin90.wt$st.iptw.wt, df.study=statin.study, save_name="[Before][ALL]st.iptw.statin90")
print_table(df.all=statin90, wt=statin90.wt$iptw.att, df.study=statin.study, save_name="[Before][ALL]iptw.att.statin90")
print_table(df.all=statin90, wt=statin90.wt$trim.att, df.study=statin.study, save_name="[Before][ALL]trim.att.statin90")
print_table(df.all=statin90, wt=statin90.wt$st.iptw.att, df.study=statin.study, save_name="[Before][ALL]st.iptw.att.statin90")
print_table(df.all=statin90, wt=statin90.wt$dec.wt, df.study=statin.study, save_name="[Before][ALL]dec.statin90")
print_table(df.all=statin90, wt=statin90.wt$match.wt, df.study=statin.study, save_name="[Before][ALL]match.statin90")
print_table(df.all=statin90, wt=statin90.wt$kernel.wt, df.study=statin.study, save_name="[Before][ALL]kernel.statin90")
sink() 

hos.statin90.con <- hos.statin90[,statin.confounder]
hos.statin90.fit <- glm(hos.statin90$statin ~ ., data = hos.statin90.con, family="binomial")

sink("./resultsCNLLS/model/[Before][HOS]results_hos.statin90.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(hos.statin90))
print(statin.study[,c(2,1)])
summary(hos.statin90.fit)
hos.statin90.wt <- ps.weight(ps=hos.statin90.fit$fitted.values, exposure=hos.statin90$statin, name="[Before][HOS]hos.statin90")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.statin90.wt, df=hos.statin90, study=statin.study, name="[Before][HOS]hos.statin90") # 0.6249

outcome.model(Y="Death", wt_df=hos.statin90.wt, df=hos.statin90, study=statin.study, name="[Before][HOS]hos.statin90")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=hos.statin90.wt, df=hos.statin90, study=statin.study, name="[Before][HOS]hos.statin90")
outcome.model(Y="ICU.admission", wt_df=hos.statin90.wt, df=hos.statin90, study=statin.study, name="[Before][HOS]hos.statin90")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt_df=hos.statin90.wt, df=hos.statin90, study=statin.study, name="[Before][HOS]hos.statin90")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=hos.statin90.wt, df=hos.statin90, study=statin.study, name="[Before][HOS]hos.statin90")

outcome.model(Y="re.outcome1", wt_df=hos.statin90.wt, df=hos.statin90, study=statin.study, name="[Before][HOS]hos.statin90") # 0.6249

print_table(df.all=hos.statin90, wt=hos.statin90.wt$iptw.wt, df.study=statin.study, save_name="[Before][HOS]iptw.hos.statin90")
print_table(df.all=hos.statin90, wt=hos.statin90.wt$trim.wt, df.study=statin.study, save_name="[Before][HOS]trim.hos.statin90")
print_table(df.all=hos.statin90, wt=hos.statin90.wt$st.iptw.wt, df.study=statin.study, save_name="[Before][HOS]st.iptw.hos.statin90")
print_table(df.all=hos.statin90, wt=hos.statin90.wt$iptw.att, df.study=statin.study, save_name="[Before][HOS]iptw.att.hos.statin90")
print_table(df.all=hos.statin90, wt=hos.statin90.wt$trim.att, df.study=statin.study, save_name="[Before][HOS]trim.att.hos.statin90")
print_table(df.all=hos.statin90, wt=hos.statin90.wt$st.iptw.att, df.study=statin.study, save_name="[Before][HOS]st.iptw.att.hos.statin90")
print_table(df.all=hos.statin90, wt=hos.statin90.wt$dec.wt, df.study=statin.study, save_name="[Before][HOS]dec.hos.statin90")
print_table(df.all=hos.statin90, wt=hos.statin90.wt$match.wt, df.study=statin.study, save_name="[Before][HOS]match.hos.statin90")
print_table(df.all=hos.statin90, wt=hos.statin90.wt$kernel.wt, df.study=statin.study, save_name="[Before][HOS]kernel.hos.statin90")
sink() 


############################################
############   Before immunosuppressant  ##############
############################################
immunosuppressant120.con <- immunosuppressant120[,immunosuppressant.confounder]
immunosuppressant120.fit <- glm(immunosuppressant120$immunosuppressant ~ ., data = immunosuppressant120.con, family="binomial")

sink("./resultsCNLLS/model/[Before][ALL]results_immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])
summary(immunosuppressant120.fit)
immunosuppressant120.wt <- ps.weight(ps=immunosuppressant120.fit$fitted.values, exposure=immunosuppressant120$immunosuppressant, name="[Before][ALL]immunosuppressant120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=immunosuppressant120.wt, df=immunosuppressant120, study=immunosuppressant.study, name="[Before][ALL]immunosuppressant120") # 0.6249

outcome.model(Y="Death", wt_df=immunosuppressant120.wt, df=immunosuppressant120, study=immunosuppressant.study, name="[Before][ALL]immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=immunosuppressant120.wt, df=immunosuppressant120, study=immunosuppressant.study, name="[Before][ALL]immunosuppressant120")
outcome.model(Y="ICU.admission", wt_df=immunosuppressant120.wt, df=immunosuppressant120, study=immunosuppressant.study, name="[Before][ALL]immunosuppressant120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=immunosuppressant120.wt, df=immunosuppressant120, study=immunosuppressant.study, name="[Before][ALL]immunosuppressant120")

print_table(df.all=immunosuppressant120, wt=immunosuppressant120.wt$iptw.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]iptw.immunosuppressant120")
print_table(df.all=immunosuppressant120, wt=immunosuppressant120.wt$trim.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]trim.immunosuppressant120")
print_table(df.all=immunosuppressant120, wt=immunosuppressant120.wt$st.iptw.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]st.iptw.immunosuppressant120")
print_table(df.all=immunosuppressant120, wt=immunosuppressant120.wt$iptw.att, df.study=immunosuppressant.study, save_name="[Before][ALL]iptw.att.immunosuppressant120")
print_table(df.all=immunosuppressant120, wt=immunosuppressant120.wt$trim.att, df.study=immunosuppressant.study, save_name="[Before][ALL]trim.att.immunosuppressant120")
print_table(df.all=immunosuppressant120, wt=immunosuppressant120.wt$st.iptw.att, df.study=immunosuppressant.study, save_name="[Before][ALL]st.iptw.att.immunosuppressant120")
print_table(df.all=immunosuppressant120, wt=immunosuppressant120.wt$dec.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]dec.immunosuppressant120")
print_table(df.all=immunosuppressant120, wt=immunosuppressant120.wt$match.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]match.immunosuppressant120")
print_table(df.all=immunosuppressant120, wt=immunosuppressant120.wt$kernel.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]kernel.immunosuppressant120")
sink() 

hos.immunosuppressant120.con <- hos.immunosuppressant120[,immunosuppressant.confounder]
hos.immunosuppressant120.fit <- glm(hos.immunosuppressant120$immunosuppressant ~ ., data = hos.immunosuppressant120.con, family="binomial")

sink("./resultsCNLLS/model/[Before][HOS]results_hos.immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])
summary(hos.immunosuppressant120.fit)
hos.immunosuppressant120.wt <- ps.weight(ps=hos.immunosuppressant120.fit$fitted.values, exposure=hos.immunosuppressant120$immunosuppressant, name="[Before][HOS]hos.immunosuppressant120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.immunosuppressant120.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="[Before][HOS]hos.immunosuppressant120") # 0.6249

outcome.model(Y="Death", wt_df=hos.immunosuppressant120.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="[Before][HOS]hos.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=hos.immunosuppressant120.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="[Before][HOS]hos.immunosuppressant120")
outcome.model(Y="ICU.admission", wt_df=hos.immunosuppressant120.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="[Before][HOS]hos.immunosuppressant120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=hos.immunosuppressant120.wt, df=hos.immunosuppressant120, study=immunosuppressant.study, name="[Before][HOS]hos.immunosuppressant120")

print_table(df.all=hos.immunosuppressant120, wt=hos.immunosuppressant120.wt$iptw.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]iptw.hos.immunosuppressant120")
print_table(df.all=hos.immunosuppressant120, wt=hos.immunosuppressant120.wt$trim.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]trim.hos.immunosuppressant120")
print_table(df.all=hos.immunosuppressant120, wt=hos.immunosuppressant120.wt$st.iptw.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]st.iptw.hos.immunosuppressant120")
print_table(df.all=hos.immunosuppressant120, wt=hos.immunosuppressant120.wt$iptw.att, df.study=immunosuppressant.study, save_name="[Before][HOS]iptw.att.hos.immunosuppressant120")
print_table(df.all=hos.immunosuppressant120, wt=hos.immunosuppressant120.wt$trim.att, df.study=immunosuppressant.study, save_name="[Before][HOS]trim.att.hos.immunosuppressant120")
print_table(df.all=hos.immunosuppressant120, wt=hos.immunosuppressant120.wt$st.iptw.att, df.study=immunosuppressant.study, save_name="[Before][HOS]st.iptw.att.hos.immunosuppressant120")
print_table(df.all=hos.immunosuppressant120, wt=hos.immunosuppressant120.wt$dec.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]dec.hos.immunosuppressant120")
print_table(df.all=hos.immunosuppressant120, wt=hos.immunosuppressant120.wt$match.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]match.hos.immunosuppressant120")
print_table(df.all=hos.immunosuppressant120, wt=hos.immunosuppressant120.wt$kernel.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]kernel.hos.immunosuppressant120")
sink() 

immunosuppressant90.con <- immunosuppressant90[,immunosuppressant.confounder]
immunosuppressant90.fit <- glm(immunosuppressant90$immunosuppressant ~ ., data = immunosuppressant90.con, family="binomial")

sink("./resultsCNLLS/model/[Before][ALL]results_immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])
summary(immunosuppressant90.fit)
immunosuppressant90.wt <- ps.weight(ps=immunosuppressant90.fit$fitted.values, exposure=immunosuppressant90$immunosuppressant, name="[Before][ALL]immunosuppressant90")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=immunosuppressant90.wt, df=immunosuppressant90, study=immunosuppressant.study, name="[Before][ALL]immunosuppressant90") # 0.6249

outcome.model(Y="Death", wt_df=immunosuppressant90.wt, df=immunosuppressant90, study=immunosuppressant.study, name="[Before][ALL]immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=immunosuppressant90.wt, df=immunosuppressant90, study=immunosuppressant.study, name="[Before][ALL]immunosuppressant90")
outcome.model(Y="ICU.admission", wt_df=immunosuppressant90.wt, df=immunosuppressant90, study=immunosuppressant.study, name="[Before][ALL]immunosuppressant90")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=immunosuppressant90.wt, df=immunosuppressant90, study=immunosuppressant.study, name="[Before][ALL]immunosuppressant90")

print_table(df.all=immunosuppressant90, wt=immunosuppressant90.wt$iptw.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]iptw.immunosuppressant90")
print_table(df.all=immunosuppressant90, wt=immunosuppressant90.wt$trim.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]trim.immunosuppressant90")
print_table(df.all=immunosuppressant90, wt=immunosuppressant90.wt$st.iptw.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]st.iptw.immunosuppressant90")
print_table(df.all=immunosuppressant90, wt=immunosuppressant90.wt$iptw.att, df.study=immunosuppressant.study, save_name="[Before][ALL]iptw.att.immunosuppressant90")
print_table(df.all=immunosuppressant90, wt=immunosuppressant90.wt$trim.att, df.study=immunosuppressant.study, save_name="[Before][ALL]trim.att.immunosuppressant90")
print_table(df.all=immunosuppressant90, wt=immunosuppressant90.wt$st.iptw.att, df.study=immunosuppressant.study, save_name="[Before][ALL]st.iptw.att.immunosuppressant90")
print_table(df.all=immunosuppressant90, wt=immunosuppressant90.wt$dec.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]dec.immunosuppressant90")
print_table(df.all=immunosuppressant90, wt=immunosuppressant90.wt$match.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]match.immunosuppressant90")
print_table(df.all=immunosuppressant90, wt=immunosuppressant90.wt$kernel.wt, df.study=immunosuppressant.study, save_name="[Before][ALL]kernel.immunosuppressant90")
sink() 

hos.immunosuppressant90.con <- hos.immunosuppressant90[,immunosuppressant.confounder]
hos.immunosuppressant90.fit <- glm(hos.immunosuppressant90$immunosuppressant ~ ., data = hos.immunosuppressant90.con, family="binomial")

sink("./resultsCNLLS/model/[Before][HOS]results_hos.immunosuppressant90.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(hos.immunosuppressant90))
print(immunosuppressant.study[,c(2,1)])
summary(hos.immunosuppressant90.fit)
hos.immunosuppressant90.wt <- ps.weight(ps=hos.immunosuppressant90.fit$fitted.values, exposure=hos.immunosuppressant90$immunosuppressant, name="[Before][HOS]hos.immunosuppressant90")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.immunosuppressant90.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="[Before][HOS]hos.immunosuppressant90") # 0.6249

outcome.model(Y="Death", wt_df=hos.immunosuppressant90.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="[Before][HOS]hos.immunosuppressant90")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=hos.immunosuppressant90.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="[Before][HOS]hos.immunosuppressant90")
outcome.model(Y="ICU.admission", wt_df=hos.immunosuppressant90.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="[Before][HOS]hos.immunosuppressant90")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=hos.immunosuppressant90.wt, df=hos.immunosuppressant90, study=immunosuppressant.study, name="[Before][HOS]hos.immunosuppressant90")

print_table(df.all=hos.immunosuppressant90, wt=hos.immunosuppressant90.wt$iptw.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]iptw.hos.immunosuppressant90")
print_table(df.all=hos.immunosuppressant90, wt=hos.immunosuppressant90.wt$trim.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]trim.hos.immunosuppressant90")
print_table(df.all=hos.immunosuppressant90, wt=hos.immunosuppressant90.wt$st.iptw.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]st.iptw.hos.immunosuppressant90")
print_table(df.all=hos.immunosuppressant90, wt=hos.immunosuppressant90.wt$iptw.att, df.study=immunosuppressant.study, save_name="[Before][HOS]iptw.att.hos.immunosuppressant90")
print_table(df.all=hos.immunosuppressant90, wt=hos.immunosuppressant90.wt$trim.att, df.study=immunosuppressant.study, save_name="[Before][HOS]trim.att.hos.immunosuppressant90")
print_table(df.all=hos.immunosuppressant90, wt=hos.immunosuppressant90.wt$st.iptw.att, df.study=immunosuppressant.study, save_name="[Before][HOS]st.iptw.att.hos.immunosuppressant90")
print_table(df.all=hos.immunosuppressant90, wt=hos.immunosuppressant90.wt$dec.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]dec.hos.immunosuppressant90")
print_table(df.all=hos.immunosuppressant90, wt=hos.immunosuppressant90.wt$match.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]match.hos.immunosuppressant90")
print_table(df.all=hos.immunosuppressant90, wt=hos.immunosuppressant90.wt$kernel.wt, df.study=immunosuppressant.study, save_name="[Before][HOS]kernel.hos.immunosuppressant90")
sink() 


############################################
############   Before Glucocorticoid  ##############
############################################
Glucocorticoid120.con <- Glucocorticoid120[,Glucocorticoid.confounder]
Glucocorticoid120.fit <- glm(Glucocorticoid120$Glucocorticoid ~ ., data = Glucocorticoid120.con, family="binomial")

sink("./resultsCNLLS/model/[Before][ALL]results_Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])
summary(Glucocorticoid120.fit)
Glucocorticoid120.wt <- ps.weight(ps=Glucocorticoid120.fit$fitted.values, exposure=Glucocorticoid120$Glucocorticoid, name="[Before][ALL]Glucocorticoid120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=Glucocorticoid120.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="[Before][ALL]Glucocorticoid120") # 0.6249

outcome.model(Y="Death", wt_df=Glucocorticoid120.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="[Before][ALL]Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=Glucocorticoid120.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="[Before][ALL]Glucocorticoid120")
outcome.model(Y="ICU.admission", wt_df=Glucocorticoid120.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="[Before][ALL]Glucocorticoid120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=Glucocorticoid120.wt, df=Glucocorticoid120, study=Glucocorticoid.study, name="[Before][ALL]Glucocorticoid120")

print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120.wt$iptw.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]iptw.Glucocorticoid120")
print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120.wt$trim.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]trim.Glucocorticoid120")
print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120.wt$st.iptw.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]st.iptw.Glucocorticoid120")
print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120.wt$iptw.att, df.study=Glucocorticoid.study, save_name="[Before][ALL]iptw.att.Glucocorticoid120")
print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120.wt$trim.att, df.study=Glucocorticoid.study, save_name="[Before][ALL]trim.att.Glucocorticoid120")
print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120.wt$st.iptw.att, df.study=Glucocorticoid.study, save_name="[Before][ALL]st.iptw.att.Glucocorticoid120")
print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120.wt$dec.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]dec.Glucocorticoid120")
print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120.wt$match.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]match.Glucocorticoid120")
print_table(df.all=Glucocorticoid120, wt=Glucocorticoid120.wt$kernel.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]kernel.Glucocorticoid120")
sink() 

hos.Glucocorticoid120.con <- hos.Glucocorticoid120[,Glucocorticoid.confounder]
hos.Glucocorticoid120.fit <- glm(hos.Glucocorticoid120$Glucocorticoid ~ ., data = hos.Glucocorticoid120.con, family="binomial")

sink("./resultsCNLLS/model/[Before][HOS]results_hos.Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])
summary(hos.Glucocorticoid120.fit)
hos.Glucocorticoid120.wt <- ps.weight(ps=hos.Glucocorticoid120.fit$fitted.values, exposure=hos.Glucocorticoid120$Glucocorticoid, name="[Before][HOS]hos.Glucocorticoid120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.Glucocorticoid120.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="[Before][HOS]hos.Glucocorticoid120") # 0.6249

outcome.model(Y="Death", wt_df=hos.Glucocorticoid120.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="[Before][HOS]hos.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=hos.Glucocorticoid120.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="[Before][HOS]hos.Glucocorticoid120")
outcome.model(Y="ICU.admission", wt_df=hos.Glucocorticoid120.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="[Before][HOS]hos.Glucocorticoid120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=hos.Glucocorticoid120.wt, df=hos.Glucocorticoid120, study=Glucocorticoid.study, name="[Before][HOS]hos.Glucocorticoid120")

print_table(df.all=hos.Glucocorticoid120, wt=hos.Glucocorticoid120.wt$iptw.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]iptw.hos.Glucocorticoid120")
print_table(df.all=hos.Glucocorticoid120, wt=hos.Glucocorticoid120.wt$trim.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]trim.hos.Glucocorticoid120")
print_table(df.all=hos.Glucocorticoid120, wt=hos.Glucocorticoid120.wt$st.iptw.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]st.iptw.hos.Glucocorticoid120")
print_table(df.all=hos.Glucocorticoid120, wt=hos.Glucocorticoid120.wt$iptw.att, df.study=Glucocorticoid.study, save_name="[Before][HOS]iptw.att.hos.Glucocorticoid120")
print_table(df.all=hos.Glucocorticoid120, wt=hos.Glucocorticoid120.wt$trim.att, df.study=Glucocorticoid.study, save_name="[Before][HOS]trim.att.hos.Glucocorticoid120")
print_table(df.all=hos.Glucocorticoid120, wt=hos.Glucocorticoid120.wt$st.iptw.att, df.study=Glucocorticoid.study, save_name="[Before][HOS]st.iptw.att.hos.Glucocorticoid120")
print_table(df.all=hos.Glucocorticoid120, wt=hos.Glucocorticoid120.wt$dec.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]dec.hos.Glucocorticoid120")
print_table(df.all=hos.Glucocorticoid120, wt=hos.Glucocorticoid120.wt$match.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]match.hos.Glucocorticoid120")
print_table(df.all=hos.Glucocorticoid120, wt=hos.Glucocorticoid120.wt$kernel.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]kernel.hos.Glucocorticoid120")
sink() 

Glucocorticoid90.con <- Glucocorticoid90[,Glucocorticoid.confounder]
Glucocorticoid90.fit <- glm(Glucocorticoid90$Glucocorticoid ~ ., data = Glucocorticoid90.con, family="binomial")

sink("./resultsCNLLS/model/[Before][ALL]results_Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])
summary(Glucocorticoid90.fit)
Glucocorticoid90.wt <- ps.weight(ps=Glucocorticoid90.fit$fitted.values, exposure=Glucocorticoid90$Glucocorticoid, name="[Before][ALL]Glucocorticoid90")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=Glucocorticoid90.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="[Before][ALL]Glucocorticoid90") # 0.6249

outcome.model(Y="Death", wt_df=Glucocorticoid90.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="[Before][ALL]Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=Glucocorticoid90.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="[Before][ALL]Glucocorticoid90")
outcome.model(Y="ICU.admission", wt_df=Glucocorticoid90.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="[Before][ALL]Glucocorticoid90")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=Glucocorticoid90.wt, df=Glucocorticoid90, study=Glucocorticoid.study, name="[Before][ALL]Glucocorticoid90")

print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90.wt$iptw.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]iptw.Glucocorticoid90")
print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90.wt$trim.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]trim.Glucocorticoid90")
print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90.wt$st.iptw.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]st.iptw.Glucocorticoid90")
print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90.wt$iptw.att, df.study=Glucocorticoid.study, save_name="[Before][ALL]iptw.att.Glucocorticoid90")
print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90.wt$trim.att, df.study=Glucocorticoid.study, save_name="[Before][ALL]trim.att.Glucocorticoid90")
print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90.wt$st.iptw.att, df.study=Glucocorticoid.study, save_name="[Before][ALL]st.iptw.att.Glucocorticoid90")
print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90.wt$dec.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]dec.Glucocorticoid90")
print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90.wt$match.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]match.Glucocorticoid90")
print_table(df.all=Glucocorticoid90, wt=Glucocorticoid90.wt$kernel.wt, df.study=Glucocorticoid.study, save_name="[Before][ALL]kernel.Glucocorticoid90")
sink() 

hos.Glucocorticoid90.con <- hos.Glucocorticoid90[,Glucocorticoid.confounder]
hos.Glucocorticoid90.fit <- glm(hos.Glucocorticoid90$Glucocorticoid ~ ., data = hos.Glucocorticoid90.con, family="binomial")

sink("./resultsCNLLS/model/[Before][HOS]results_hos.Glucocorticoid90.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(hos.Glucocorticoid90))
print(Glucocorticoid.study[,c(2,1)])
summary(hos.Glucocorticoid90.fit)
hos.Glucocorticoid90.wt <- ps.weight(ps=hos.Glucocorticoid90.fit$fitted.values, exposure=hos.Glucocorticoid90$Glucocorticoid, name="[Before][HOS]hos.Glucocorticoid90")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.Glucocorticoid90.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="[Before][HOS]hos.Glucocorticoid90") # 0.6249

outcome.model(Y="Death", wt_df=hos.Glucocorticoid90.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="[Before][HOS]hos.Glucocorticoid90")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=hos.Glucocorticoid90.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="[Before][HOS]hos.Glucocorticoid90")
outcome.model(Y="ICU.admission", wt_df=hos.Glucocorticoid90.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="[Before][HOS]hos.Glucocorticoid90")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=hos.Glucocorticoid90.wt, df=hos.Glucocorticoid90, study=Glucocorticoid.study, name="[Before][HOS]hos.Glucocorticoid90")

print_table(df.all=hos.Glucocorticoid90, wt=hos.Glucocorticoid90.wt$iptw.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]iptw.hos.Glucocorticoid90")
print_table(df.all=hos.Glucocorticoid90, wt=hos.Glucocorticoid90.wt$trim.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]trim.hos.Glucocorticoid90")
print_table(df.all=hos.Glucocorticoid90, wt=hos.Glucocorticoid90.wt$st.iptw.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]st.iptw.hos.Glucocorticoid90")
print_table(df.all=hos.Glucocorticoid90, wt=hos.Glucocorticoid90.wt$iptw.att, df.study=Glucocorticoid.study, save_name="[Before][HOS]iptw.att.hos.Glucocorticoid90")
print_table(df.all=hos.Glucocorticoid90, wt=hos.Glucocorticoid90.wt$trim.att, df.study=Glucocorticoid.study, save_name="[Before][HOS]trim.att.hos.Glucocorticoid90")
print_table(df.all=hos.Glucocorticoid90, wt=hos.Glucocorticoid90.wt$st.iptw.att, df.study=Glucocorticoid.study, save_name="[Before][HOS]st.iptw.att.hos.Glucocorticoid90")
print_table(df.all=hos.Glucocorticoid90, wt=hos.Glucocorticoid90.wt$dec.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]dec.hos.Glucocorticoid90")
print_table(df.all=hos.Glucocorticoid90, wt=hos.Glucocorticoid90.wt$match.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]match.hos.Glucocorticoid90")
print_table(df.all=hos.Glucocorticoid90, wt=hos.Glucocorticoid90.wt$kernel.wt, df.study=Glucocorticoid.study, save_name="[Before][HOS]kernel.hos.Glucocorticoid90")
sink() 


###############################################
#############    Before Anticoagulants    ################
###############################################
Anticoagulants120.con <- Anticoagulants120[,Anticoagulants.confounder]
Anticoagulants120.fit <- glm(Anticoagulants120$Anticoagulants ~ ., data = Anticoagulants120.con, family="binomial")

sink("./resultsCNLLS/model/[Before][ALL]results_Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])
summary(Anticoagulants120.fit)

Anticoagulants120.wt <- ps.weight(ps=Anticoagulants120.fit$fitted.values, exposure=Anticoagulants120$Anticoagulants, name="[Before][ALL]Anticoagulants120")

cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=Anticoagulants120.wt, df=Anticoagulants120, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants120") # 0.6249

outcome.model(Y="Death", wt=Anticoagulants120.wt, df=Anticoagulants120, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=Anticoagulants120.wt, df=Anticoagulants120, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants120")
outcome.model(Y="ICU.admission", wt=Anticoagulants120.wt, df=Anticoagulants120, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=Anticoagulants120.wt, df=Anticoagulants120, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants120")

outcome.model(Y="re.outcome1", wt_df=Anticoagulants120.wt, df=Anticoagulants120, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants120") # 0.6249

cat("\n\n -- PRINT -- \n\n")
print_table(df.all=Anticoagulants120, wt=Anticoagulants120.wt$iptw.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]iptw.Anticoagulants120")
print_table(df.all=Anticoagulants120, wt=Anticoagulants120.wt$trim.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]trim.Anticoagulants120")
print_table(df.all=Anticoagulants120, wt=Anticoagulants120.wt$st.iptw.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]st.iptw.Anticoagulants120")
print_table(df.all=Anticoagulants120, wt=Anticoagulants120.wt$iptw.att, df.study=Anticoagulants.study, save_name="[Before][ALL]iptw.att.Anticoagulants120")
print_table(df.all=Anticoagulants120, wt=Anticoagulants120.wt$trim.att, df.study=Anticoagulants.study, save_name="[Before][ALL]trim.att.Anticoagulants120")
print_table(df.all=Anticoagulants120, wt=Anticoagulants120.wt$st.iptw.att, df.study=Anticoagulants.study, save_name="[Before][ALL]st.iptw.attAnticoagulants120")
print_table(df.all=Anticoagulants120, wt=Anticoagulants120.wt$dec.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]dec.Anticoagulants120")
print_table(df.all=Anticoagulants120, wt=Anticoagulants120.wt$match.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]match.Anticoagulants120")
print_table(df.all=Anticoagulants120, wt=Anticoagulants120.wt$kernel.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]kernel.Anticoagulants120")

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
Anticoagulants120.fit1 <- glm(Anticoagulants120$Anticoagulants ~ ., data = scr1, family="binomial")

Anticoagulants120.wt1 <- ps.weight(ps=Anticoagulants120.fit1$fitted.values,
                                   exposure=Anticoagulants120$Anticoagulants,
                                   name="[Before][ALL]Anticoagulants120_1")

Anticoagulants.study1 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr1, type = rep("Confounder", length(Anticoagulants.scr1))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))

outcome.model(Y="bleeding.transfusion", wt_df=Anticoagulants120.wt1, df=Anticoagulants120, study=Anticoagulants.study1, name="[Before][ALL]Anticoagulants120_1")


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

Anticoagulants120.fit2 <- glm(Anticoagulants120$Anticoagulants ~ ., data = scr1, family="binomial")

Anticoagulants120.wt2 <- ps.weight(ps=Anticoagulants120.fit2$fitted.values, exposure=Anticoagulants120$Anticoagulants, name="[Before][ALL]Anticoagulants120_2")

Anticoagulants.study2 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr2, type = rep("Confounder", length(Anticoagulants.scr2))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))


outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt_df=Anticoagulants120.wt2, df=Anticoagulants120, study=Anticoagulants.study2, name="[Before][ALL]Anticoagulants120_2")
sink() 

hos.Anticoagulants120.con <- hos.Anticoagulants120[,Anticoagulants.confounder]
hos.Anticoagulants120.fit <- glm(hos.Anticoagulants120$Anticoagulants ~ ., data = hos.Anticoagulants120.con, family="binomial")

sink("./resultsCNLLS/model/[Before][HOS]results_hos.Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])
summary(hos.Anticoagulants120.fit)

# deciles, trimming... 
hos.Anticoagulants120.wt <- ps.weight(ps=hos.Anticoagulants120.fit$fitted.values, exposure=hos.Anticoagulants120$Anticoagulants, name="[Before][HOS]hos.Anticoagulants120")

cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.Anticoagulants120.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants120") # 0.6249

outcome.model(Y="Death", wt=hos.Anticoagulants120.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.Anticoagulants120.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants120")
outcome.model(Y="ICU.admission", wt=hos.Anticoagulants120.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.Anticoagulants120.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants120")

outcome.model(Y="re.outcome1", wt_df=hos.Anticoagulants120.wt, df=hos.Anticoagulants120, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants120") # 0.6249

cat("\n\n -- PRINT -- \n\n")
print_table(df.all=hos.Anticoagulants120, wt=hos.Anticoagulants120.wt$iptw.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]iptw.hos.Anticoagulants120")
print_table(df.all=hos.Anticoagulants120, wt=hos.Anticoagulants120.wt$trim.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]trim.hos.Anticoagulants120")
print_table(df.all=hos.Anticoagulants120, wt=hos.Anticoagulants120.wt$st.iptw.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]st.iptw.hos.Anticoagulants120")
print_table(df.all=hos.Anticoagulants120, wt=hos.Anticoagulants120.wt$iptw.att, df.study=Anticoagulants.study, save_name="[Before][HOS]iptw.att.hos.Anticoagulants120")
print_table(df.all=hos.Anticoagulants120, wt=hos.Anticoagulants120.wt$trim.att, df.study=Anticoagulants.study, save_name="[Before][HOS]trim.att.hos.Anticoagulants120")
print_table(df.all=hos.Anticoagulants120, wt=hos.Anticoagulants120.wt$st.iptw.att, df.study=Anticoagulants.study, save_name="[Before][HOS]st.iptw.atthos.Anticoagulants120")
print_table(df.all=hos.Anticoagulants120, wt=hos.Anticoagulants120.wt$dec.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]dec.hos.Anticoagulants120")
print_table(df.all=hos.Anticoagulants120, wt=hos.Anticoagulants120.wt$match.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]match.hos.Anticoagulants120")
print_table(df.all=hos.Anticoagulants120, wt=hos.Anticoagulants120.wt$kernel.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]kernel.hos.Anticoagulants120")

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
hos.Anticoagulants120.fit1 <- glm(hos.Anticoagulants120$Anticoagulants ~ ., data = scr1, family="binomial")

hos.Anticoagulants120.wt1 <- ps.weight(ps=hos.Anticoagulants120.fit1$fitted.values,
                                       exposure=hos.Anticoagulants120$Anticoagulants,
                                       name="[Before][HOS]hos.Anticoagulants120_1")

Anticoagulants.study1 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr1, type = rep("Confounder", length(Anticoagulants.scr1))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))

outcome.model(Y="bleeding.transfusion", wt_df=hos.Anticoagulants120.wt1, df=hos.Anticoagulants120, study=Anticoagulants.study1, name="[Before][HOS]hos.Anticoagulants120_1")


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

hos.Anticoagulants120.fit2 <- glm(hos.Anticoagulants120$Anticoagulants ~ ., data = scr1, family="binomial")

hos.Anticoagulants120.wt2 <- ps.weight(ps=hos.Anticoagulants120.fit2$fitted.values, exposure=hos.Anticoagulants120$Anticoagulants, name="[Before][HOS]hos.Anticoagulants120_2")

Anticoagulants.study2 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr2, type = rep("Confounder", length(Anticoagulants.scr2))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))


outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt_df=hos.Anticoagulants120.wt2, df=hos.Anticoagulants120, study=Anticoagulants.study2, name="[Before][HOS]hos.Anticoagulants120_2")
sink() 

Anticoagulants90.con <- Anticoagulants90[,Anticoagulants.confounder]
Anticoagulants90.fit <- glm(Anticoagulants90$Anticoagulants ~ ., data = Anticoagulants90.con, family="binomial")

sink("./resultsCNLLS/model/[Before][ALL]results_Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])
summary(Anticoagulants90.fit)

# deciles, trimming... 
# --> error (count 수가 너무 적어서 sample data가 에러 발생)
# --> (계획) error 발생해도 진행되도록.
Anticoagulants90.wt <- ps.weight(ps=Anticoagulants90.fit$fitted.values, exposure=Anticoagulants90$Anticoagulants, name="[Before][ALL]Anticoagulants90")

cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=Anticoagulants90.wt, df=Anticoagulants90, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants90") # 0.6249

outcome.model(Y="Death", wt=Anticoagulants90.wt, df=Anticoagulants90, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=Anticoagulants90.wt, df=Anticoagulants90, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants90")
outcome.model(Y="ICU.admission", wt=Anticoagulants90.wt, df=Anticoagulants90, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=Anticoagulants90.wt, df=Anticoagulants90, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants90")

outcome.model(Y="re.outcome1", wt_df=Anticoagulants90.wt, df=Anticoagulants90, study=Anticoagulants.study, name="[Before][ALL]Anticoagulants90") # 0.6249

cat("\n\n -- PRINT -- \n\n")
print_table(df.all=Anticoagulants90, wt=Anticoagulants90.wt$iptw.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]iptw.Anticoagulants90")
print_table(df.all=Anticoagulants90, wt=Anticoagulants90.wt$trim.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]trim.Anticoagulants90")
print_table(df.all=Anticoagulants90, wt=Anticoagulants90.wt$st.iptw.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]st.iptw.Anticoagulants90")
print_table(df.all=Anticoagulants90, wt=Anticoagulants90.wt$iptw.att, df.study=Anticoagulants.study, save_name="[Before][ALL]iptw.att.Anticoagulants90")
print_table(df.all=Anticoagulants90, wt=Anticoagulants90.wt$trim.att, df.study=Anticoagulants.study, save_name="[Before][ALL]trim.att.Anticoagulants90")
print_table(df.all=Anticoagulants90, wt=Anticoagulants90.wt$st.iptw.att, df.study=Anticoagulants.study, save_name="[Before][ALL]st.iptw.attAnticoagulants90")
print_table(df.all=Anticoagulants90, wt=Anticoagulants90.wt$dec.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]dec.Anticoagulants90")
print_table(df.all=Anticoagulants90, wt=Anticoagulants90.wt$match.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]match.Anticoagulants90")
print_table(df.all=Anticoagulants90, wt=Anticoagulants90.wt$kernel.wt, df.study=Anticoagulants.study, save_name="[Before][ALL]kernel.Anticoagulants90")

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
Anticoagulants90.fit1 <- glm(Anticoagulants90$Anticoagulants ~ ., data = scr1, family="binomial")

Anticoagulants90.wt1 <- ps.weight(ps=Anticoagulants90.fit1$fitted.values,
                                  exposure=Anticoagulants90$Anticoagulants,
                                  name="[Before][ALL]Anticoagulants90_1")

Anticoagulants.study1 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr1, type = rep("Confounder", length(Anticoagulants.scr1))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))

outcome.model(Y="bleeding.transfusion", wt_df=Anticoagulants90.wt1, df=Anticoagulants90, study=Anticoagulants.study1, name="[Before][ALL]Anticoagulants90_1")


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

Anticoagulants90.fit2 <- glm(Anticoagulants90$Anticoagulants ~ ., data = scr1, family="binomial")

Anticoagulants90.wt2 <- ps.weight(ps=Anticoagulants90.fit2$fitted.values, exposure=Anticoagulants90$Anticoagulants, name="[Before][ALL]Anticoagulants90_2")

Anticoagulants.study2 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr2, type = rep("Confounder", length(Anticoagulants.scr2))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))


outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt_df=Anticoagulants90.wt2, df=Anticoagulants90, study=Anticoagulants.study2, name="[Before][ALL]Anticoagulants90_2")
sink() 

hos.Anticoagulants90.con <- hos.Anticoagulants90[,Anticoagulants.confounder]
hos.Anticoagulants90.fit <- glm(hos.Anticoagulants90$Anticoagulants ~ ., data = hos.Anticoagulants90.con, family="binomial")

sink("./resultsCNLLS/model/[Before][HOS]results_hos.Anticoagulants90.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(hos.Anticoagulants90))
print(Anticoagulants.study[,c(2,1)])
summary(hos.Anticoagulants90.fit)

# deciles, trimming... 
# --> error (count 수가 너무 적어서 sample data가 에러 발생)
# --> (계획) error 발생해도 진행되도록.
hos.Anticoagulants90.wt <- ps.weight(ps=hos.Anticoagulants90.fit$fitted.values, exposure=hos.Anticoagulants90$Anticoagulants, name="[Before][HOS]hos.Anticoagulants90")

cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.Anticoagulants90.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants90") # 0.6249

outcome.model(Y="Death", wt=hos.Anticoagulants90.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.Anticoagulants90.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants90")
outcome.model(Y="ICU.admission", wt=hos.Anticoagulants90.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants90")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.Anticoagulants90.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants90")

outcome.model(Y="re.outcome1", wt_df=hos.Anticoagulants90.wt, df=hos.Anticoagulants90, study=Anticoagulants.study, name="[Before][HOS]hos.Anticoagulants90") # 0.6249

cat("\n\n -- PRINT -- \n\n")
print_table(df.all=hos.Anticoagulants90, wt=hos.Anticoagulants90.wt$iptw.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]iptw.hos.Anticoagulants90")
print_table(df.all=hos.Anticoagulants90, wt=hos.Anticoagulants90.wt$trim.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]trim.hos.Anticoagulants90")
print_table(df.all=hos.Anticoagulants90, wt=hos.Anticoagulants90.wt$st.iptw.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]st.iptw.hos.Anticoagulants90")
print_table(df.all=hos.Anticoagulants90, wt=hos.Anticoagulants90.wt$iptw.att, df.study=Anticoagulants.study, save_name="[Before][HOS]iptw.att.hos.Anticoagulants90")
print_table(df.all=hos.Anticoagulants90, wt=hos.Anticoagulants90.wt$trim.att, df.study=Anticoagulants.study, save_name="[Before][HOS]trim.att.hos.Anticoagulants90")
print_table(df.all=hos.Anticoagulants90, wt=hos.Anticoagulants90.wt$st.iptw.att, df.study=Anticoagulants.study, save_name="[Before][HOS]st.iptw.atthos.Anticoagulants90")
print_table(df.all=hos.Anticoagulants90, wt=hos.Anticoagulants90.wt$dec.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]dec.hos.Anticoagulants90")
print_table(df.all=hos.Anticoagulants90, wt=hos.Anticoagulants90.wt$match.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]match.hos.Anticoagulants90")
print_table(df.all=hos.Anticoagulants90, wt=hos.Anticoagulants90.wt$kernel.wt, df.study=Anticoagulants.study, save_name="[Before][HOS]kernel.hos.Anticoagulants90")

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
hos.Anticoagulants90.fit1 <- glm(hos.Anticoagulants90$Anticoagulants ~ ., data = scr1, family="binomial")

hos.Anticoagulants90.wt1 <- ps.weight(ps=hos.Anticoagulants90.fit1$fitted.values,
                                      exposure=hos.Anticoagulants90$Anticoagulants,
                                      name="[Before][HOS]hos.Anticoagulants90_1")

Anticoagulants.study1 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr1, type = rep("Confounder", length(Anticoagulants.scr1))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))

outcome.model(Y="bleeding.transfusion", wt_df=hos.Anticoagulants90.wt1, df=hos.Anticoagulants90, study=Anticoagulants.study1, name="[Before][HOS]hos.Anticoagulants90_1")


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

hos.Anticoagulants90.fit2 <- glm(hos.Anticoagulants90$Anticoagulants ~ ., data = scr1, family="binomial")

hos.Anticoagulants90.wt2 <- ps.weight(ps=hos.Anticoagulants90.fit2$fitted.values, exposure=hos.Anticoagulants90$Anticoagulants, name="[Before][HOS]hos.Anticoagulants90_2")

Anticoagulants.study2 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr2, type = rep("Confounder", length(Anticoagulants.scr2))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))

outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt_df=hos.Anticoagulants90.wt2, df=hos.Anticoagulants90, study=Anticoagulants.study2, name="[Before][HOS]hos.Anticoagulants90_2")
sink()

####################################################################
#####################      data After       ########################
####################################################################
patient_info2 <- as.data.frame(patient_dt)

cat("\n nrow(patient_info2) \n")
print(nrow(patient_info2))
patient_info2$AGE <- ifelse(patient_info2$AGE_cal<=64, 0, 1)
print(table(patient_info2$AGE))

# 열 삭제
# (def) SEX_TP_CD 1:male, 2:female, 9:etc
patient_info2 <- patient_info2[!(patient_info2$SEX_TP_CD == 9),]
print(nrow(patient_info2))

patient_info2$GENDER <- patient_info2$SEX_TP_CD - 1
print(table(patient_info2$GENDER))

# (def) 건강보험 : medical insurance, 의료급여&보훈 : medical aid
patient_info2 <- patient_info2[(patient_info2$INSUP_TP_CD == 4 | patient_info2$INSUP_TP_CD == 5 | patient_info2$INSUP_TP_CD == 7),]
patient_info2$MedInfo <- ifelse(patient_info2$INSUP_TP_CD == 4, 0, 1)
print(nrow(patient_info2))
print(table(patient_info2$MedInfo))

patient_info2 <- as.data.frame(patient_info2[,c("JID", "first_visit", "last_visit", "AGE_cal", "AGE", "GENDER", "MedInfo")])

date_info <- merge(co19_t200_trans_dn[,c("JID", "MID", "RECU_FR_DD")], patient_info2[,c("JID", "first_visit", "last_visit")], by="JID", all.x=TRUE)

rule1 <- (date_info$first_visit <= date_info$RECU_FR_DD) & (date_info$RECU_FR_DD <= date_info$last)
rule_outcome <- date_info[rule1,]$MID

name_unique <- unique(code_outcomes_df$name)
final <- patient_info2[,c("JID", "first_visit", "last_visit")]
for (i in name_unique) {
  if (i=="Death") next
  code_info <- data.frame("code_val" = code_outcomes_df[code_outcomes_df$name == i, "code"],
                          "uniqueCode" = code_outcomes_df[code_outcomes_df$name == i,"uniqueCode"],
                          "code_len" = nchar(code_outcomes_df[code_outcomes_df$name == i, "code"]),
                          "reg" = paste0("^", code_outcomes_df[code_outcomes_df$name == i, "code"]))
  
  #,"exclusive.or" = code_outcomes_df[code_outcomes_df$name == i, "exclusive.or"])
  #ex.or.code <- code_info[code_info$exclusive.or == 1, ]
  name <- gsub("[-, /]", ".", i)
  search.reg <- paste(code_info$reg, collapse ="|")
  
  sF_split1 <- strsplit(code_outcomes_df[code_outcomes_df$name == i,"searchFor"], "; ")[[1]]
  print("[START] >> sF_split1 while loop;\n")
  j = 1; searchMIDs <- c()
  while(!is.na(sF_split1[j])){
    sF_split2 <- strsplit(sF_split1[j], "/")
    
    df_name <- sF_split2[[1]][1]
    var_name <- sF_split2[[1]][2]
    df_all <- data.frame(get(df_name))
    #--------- 기간 걸러내기 (twjhe 200을 위해 필요) ---------#
    if (sum(rule1)) {
      df <- df_all[df_all$MID %in% rule_outcome,]
    } else {
      df <- df_all
    }
    #----------------------------------#
    cat(sprintf("[%d] before : %d, after : %d\n",j, nrow(df_all), nrow(df)))
    df_all <- NULL
    
    searchMID <- df[grep(search.reg, df[,var_name], ignore.case = TRUE),]$MID
    searchMIDs <- unique(c(searchMIDs, searchMID))
    
    j <- j+1
    
  }
  if (!length(searchMIDs)) {
    final[,name] <- final$last_visit
    next()
  }
  searchDT <- data.table(co19_t200_trans_dn[co19_t200_trans_dn$MID %in% searchMIDs,])[,.(first_outcome=min(RECU_FR_DD)), by=.(JID)]
  searchDF <- data.frame(searchDT)
  colnames(searchDF)[colnames(searchDF)=="first_outcome"] <- name
  final <- merge(final, searchDF[,c("JID", name)], all.x=TRUE, by="JID")
  print(name)
  final[is.na(final[,name]),name] <- final[is.na(final[,name]),]$last_visit
}

# 이름 바꿔둘까 고민해야 할 듯.
colnames(final) <-gsub("[-,/ ()]", ".", colnames(final))

after.exposure <- function(drug, rule_mid, data){
  code_info <- data.frame("code_val" = code_ATC_df3[code_ATC_df3$name == drug, "code"],
                          "uniqueCode" = code_ATC_df3[code_ATC_df3$name == drug,"uniqueCode"],
                          "code_len" = nchar(code_ATC_df3[code_ATC_df3$name == drug, "code"]),
                          "reg" = paste0("^", code_ATC_df3[code_ATC_df3$name == drug, "code"]))
  
  search.reg <- paste(code_info$reg, collapse ="|")
  abs <- grep(search.reg, ATC_mapping_df$ATC, ignore.case = TRUE)
  mapped_var <- toupper(ATC_mapping_df[abs, "GNL_CD"])
  
  sF_split1 <- strsplit(code_ATC_df3[code_ATC_df3$name == drug,"searchFor"], "; ")[[1]]
  print("[START] >> sF_split1 while loop;\n")
  j = 1; searchJIDs <- c()
  while(!is.na(sF_split1[j])){
    sF_split2 <- strsplit(sF_split1[j], "/")
    
    df_name <- sF_split2[[1]][1]
    var_name <- sF_split2[[1]][2]
    df_all <- data.frame(get(df_name))
    
    if (length(rule_mid)) {
      df <- df_all[df_all$MID %in% rule_mid,]
    } else{
      df <- df_all
    }
    #----------------------------------#
    cat(sprintf("[%d] before : %d, after : %d\n",j, nrow(df_all), nrow(df)))
    df_all <- NULL
    searchJID <- df[df[,var_name] %in% mapped_var, "JID"]
    searchJIDs <- unique(c(searchJIDs, searchJID))
    j <- j+1
    
  }
  data$new <- 0
  if (length(searchJIDs)!=0) {
    data[data$JID %in% searchJIDs, "new"] <- 1
  }
  return (data$new)
}

final$Death <- patient_info2$last_visit
# (1) statin
statin_outcomes <- final[,c("JID", statin.outcome1)]
statin_outcomes$outcome1 <- apply(statin_outcomes[,-1],1,min)

date_info_statin <- merge(date_info, statin_outcomes[,c("JID", "outcome1")], all.x=TRUE, by="JID")
rule_statin <- (date_info_statin$first_visit <= date_info_statin$RECU_FR_DD) & (date_info_statin$RECU_FR_DD <= date_info_statin$outcome1)
rule_statin2 <- date_info_statin[rule_statin,]$MID

statin_outcomes$statin <- after.exposure(drug="statin", 
                                          rule_mid=rule_statin2, 
                                          data=statin_outcomes)

# (2) Anticoagulants
Anticoagulants_outcomes <- final[,c("JID", Anticoagulants.outcome1)]
Anticoagulants_outcomes$outcome1 <- apply(Anticoagulants_outcomes[,-1],1,min)

date_info_Anticoagulants <- merge(date_info, Anticoagulants_outcomes[,c("JID", "outcome1")], all.x=TRUE, by="JID")
rule_Anticoagulants <- (date_info_Anticoagulants$first_visit <= date_info_Anticoagulants$RECU_FR_DD) & (date_info_Anticoagulants$RECU_FR_DD <= date_info_Anticoagulants$outcome1)
rule_Anticoagulants2 <- date_info_Anticoagulants[rule_Anticoagulants,]$MID

Anticoagulants_outcomes$Anticoagulants <- after.exposure(drug="Anticoagulants", rule_mid=rule_Anticoagulants2, data=Anticoagulants_outcomes)

# (3) Immunosuppressant
immunosuppressant_outcomes <- final[,c("JID", immunosuppressant.outcome1)]
immunosuppressant_outcomes$outcome1 <- apply(immunosuppressant_outcomes[,-1],1,min)

date_info_Immunosuppressant <- merge(date_info, immunosuppressant_outcomes[,c("JID", "outcome1")], all.x=TRUE, by="JID")
rule_immunosuppressant <- (date_info_Immunosuppressant$first_visit <= date_info_Immunosuppressant$RECU_FR_DD) & (date_info_Immunosuppressant$RECU_FR_DD <= date_info_Immunosuppressant$outcome1)
rule_immunosuppressant2 <- date_info_Immunosuppressant[rule_immunosuppressant,]$MID

immunosuppressant_outcomes$immunosuppressant <- after.exposure(drug="immunosuppressant", rule_mid=rule_immunosuppressant2, data=immunosuppressant_outcomes)

print(dim(patient_info2)); print(colnames(patient_info2))

# [3] comorbidties ICD10 (sheet2)
como_after_statin120 <- gen_dummies(code_ICD10_df, "statin.study", 120, After=TRUE)
como_after_Anticoagulants120 <- gen_dummies(code_ICD10_df, "antiplatelet.anticoagulant.study", 120, After=TRUE)
como_after_immunosuppressant120 <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 120, After=TRUE)

# -- codewise frequency comorbidities -- #
cat("\n -3- comorbidties ICD10 (sheet2)  \n")
save_freq("como_after_statin120")
save_freq("como_after_Anticoagulants120")
save_freq("como_after_immunosuppressant120")

# [4] medications ATC code (sheet3)
# [code book(code_ATC)의 code column] = [ATC_mapping_df의 ATC코드 column] 
# [ATC_mapping_df의 주성분코드] = [T530의 GNL_CD]
# [ATC_mapping_df의 제품코드]  = T530의 DIV_CD  (T300의 DIV_CD도 일부 존재)
ATC_after_statin120 <- gen_dummies(code_ATC_df2, "statin.study", 120, After=TRUE)
ATC_after_Anticoagulants120 <- gen_dummies(code_ATC_df2, "antiplatelet.anticoagulant.study",120, After=TRUE)
ATC_after_immunosuppressant120 <- gen_dummies(code_ATC_df2, "immune.suppressant.study", 120, After=TRUE)

cat(" ATC_after R data save \n")
cat(" -4- medication ATC (sheet3) \n")
save_freq("ATC_after_statin120")
save_freq("ATC_after_Anticoagulants120")
save_freq("ATC_after_immunosuppressant120")

A.statin120 <- make_df2(como_df = como_after_statin120, ATC_df = ATC_after_statin120,
                        outcome_df=covid_outcomes, study = "A.statin120")
A.Anticoagulants120 <- make_df2(como_df = como_after_Anticoagulants120, ATC_df = ATC_after_Anticoagulants120,
                                outcome_df=covid_outcomes, study = "A.Anticoagulants120")
A.immunosuppressant120 <- make_df2(como_df = como_after_immunosuppressant120, ATC_df = ATC_after_immunosuppressant120,
                                   outcome_df=covid_outcomes, study = "A.immunosuppressant120")

A.statin120$statin <- statin_outcomes$statin
A.Anticoagulants120$Anticoagulants <- Anticoagulants_outcomes$Anticoagulants
A.immunosuppressant120$immunosuppressant <- immunosuppressant_outcomes$immunosuppressant

#### Modeling 순서대로 하자 ####
#### Statin ####
# Lipid Lowering agents excluding statin 에서 including statin 은 왜 exposure.itself? check.
A.statin120$Lipid.lowering.agents.excluding.statin <- A.statin120$Lipid.lowering.agents.including.statin - A.statin120$Lipid.lowering.agents.including.statin * A.statin120$statin2
A.statin120$V_cancer <- 0 ; A.statin120[A.statin120$JID %in% cancer_jid,]$V_cancer <- 1

# malignancy
A.statin120$Malignancy <- A.statin120$Malignancy * A.statin120$V_cancer
A.statin120$V_cancer <- NULL

A.statin120$first_visit <- NULL
A.statin120$last_visit <- NULL

A.statin120$outcome1 <-  ifelse(rowSums(A.statin120[,statin.outcome1])>0,1,0)
A.statin120$re.outcome1 <-  ifelse(rowSums(A.statin120[,statin.re.outcome1])>0,1,0)
A.statin120$Use.of.mechanical.ventilation.ICU.admission <- as.numeric(A.statin120$Use.of.mechanical.ventilation + A.statin120$ICU.admission > 0)

A.statin120$Hypertension <- A.statin120$Hypertension * A.statin120$anitihypertensive.use
A.statin120$arrhythmias.or.antiarrhythmias.use <- as.numeric(A.statin120$anti.arrhythmias.use+A.statin120$Other.arrhythmias>0)
A.statin120$autoimmune.disease <- A.statin120$autoimmune.disease * A.statin120$immunosuppressant

# “TIA”, “Stroke”, “Coronary artery disease ”, “Atherosclerosis”, “Peripheral vascular disease”
A.statin120$TIA.Stroke.Coronary.Atherosclerosis.Peripheral <- 
  ifelse(A.statin120$stroke.or.TIA.history + A.statin120$Coronary.artery.disease + A.statin120$Peripheral.vascular.disease>0,1,0)

#### anticoagulants.antiplatelets ####
A.Anticoagulants120$first_visit <- NULL
A.Anticoagulants120$last_visit <- NULL

# malignancy
A.Anticoagulants120$V_cancer <- 0
A.Anticoagulants120[A.Anticoagulants120$JID %in% cancer_jid,]$V_cancer <- 1
A.Anticoagulants120$Malignancy <- A.Anticoagulants120$Malignancy * A.Anticoagulants120$V_cancer
A.Anticoagulants120$V_cancer <- NULL

A.Anticoagulants120$Hypertension <- A.Anticoagulants120$Hypertension * A.Anticoagulants120$anitihypertensive.use
A.Anticoagulants120$arrhythmias.or.antiarrhythmias.use <- as.numeric(A.Anticoagulants120$anti.arrhythmias.use+A.Anticoagulants120$Other.arrhythmias>0)
A.Anticoagulants120$autoimmune.disease <- A.Anticoagulants120$autoimmune.disease * A.Anticoagulants120$immunosuppressant

# - outcome1 - 
A.Anticoagulants120$Use.of.mechanical.ventilation.ICU.admission <- as.numeric(A.Anticoagulants120$Use.of.mechanical.ventilation + A.Anticoagulants120$ICU.admission > 0)

# "myocardial infarction, ischemic stroke and TIA", "Thromboembolism"
# "bleeding", "transfusion"
A.Anticoagulants120$myocardial.storke.TIA.Thromboembolism <- ifelse(A.Anticoagulants120$myocardial.infarction + A.Anticoagulants120$Stroke + A.Anticoagulants120$TIA.transient.ischemic.attack. + A.Anticoagulants120$Thromboembolism>0,1,0)
A.Anticoagulants120$bleeding.transfusion <- ifelse(A.Anticoagulants120$bleeding + A.Anticoagulants120$transfusion>0,1,0)

A.Anticoagulants120$outcome1 <- ifelse(rowSums(A.Anticoagulants120[,Anticoagulants.outcome1])>0,1,0)
A.Anticoagulants120$re.outcome1 <- ifelse(rowSums(A.Anticoagulants120[,Anticoagulants.re.outcome1])>0,1,0)

# “TIA”, “Stroke”, “Coronary artery disease ”, “Peripheral vascular disease”
# “Atrial Fibrillation”, “Thromboembolism” 
A.Anticoagulants120$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(A.Anticoagulants120$stroke.or.TIA.history + A.Anticoagulants120$Coronary.artery.disease + A.Anticoagulants120$Peripheral.vascular.disease>0,1,0)
A.Anticoagulants120$Atrial.Thromboembolism <- ifelse(A.Anticoagulants120$Atrial.fibrillation + A.Anticoagulants120$Thromboembolism>0,1,0)

#### immunosuppressant ####
A.immunosuppressant120$first_visit <- NULL
A.immunosuppressant120$last_visit <- NULL

A.immunosuppressant120$statin <- NULL

A.immunosuppressant120$V_cancer <- 0
A.immunosuppressant120[A.immunosuppressant120$JID %in% cancer_jid,]$V_cancer <- 1
A.immunosuppressant120$Malignancy <- A.immunosuppressant120$Malignancy * A.immunosuppressant120$V_cancer
A.immunosuppressant120$V_cancer <- NULL 

A.immunosuppressant120$autoimmune.disease <- A.immunosuppressant120$autoimmune.disease * A.immunosuppressant120$immunosuppressant2

A.immunosuppressant120$immuno.status <- as.numeric(rowSums(como_after_immunosuppressant120[, immuno_status]) > 0)

A.immunosuppressant120$immuned.deficiency.including.Cacner.HIV <- as.numeric(A.immunosuppressant120$HIV + A.immunosuppressant120$autoimmune.disease + A.immunosuppressant120$Antineoplastic + A.immunosuppressant120$Malignancy > 1)
A.immunosuppressant120$Use.of.mechanical.ventilation.ICU.admission <- as.numeric(A.immunosuppressant120$Use.of.mechanical.ventilation + A.immunosuppressant120$ICU.admission > 0)
A.immunosuppressant120$Hypertension <- A.immunosuppressant120$Hypertension * A.immunosuppressant120$anitihypertensive.use
A.immunosuppressant120$arrhythmias.or.antiarrhythmias.use <- as.numeric(A.immunosuppressant120$anti.arrhythmias.use+A.immunosuppressant120$Other.arrhythmias>0)

A.immunosuppressant120$outcome1 <- ifelse(rowSums(A.immunosuppressant120[,immunosuppressant.outcome1])>0,1,0)

# Glucocorticoid
A.immunosuppressant120$Glucocorticoid <- ATC_after_immunosuppressant120$ID3110.H02
A.Glucocorticoid120 <- A.immunosuppressant120[( A.immunosuppressant120$immunosuppressant == 0) | ( A.immunosuppressant120$Glucocorticoid == 1 ),]

# hospital defined.
hos.A.statin120 <- A.statin120[A.statin120$JID %in% hospitalJID,]
hos.A.Anticoagulants120 <- A.Anticoagulants120[A.Anticoagulants120$JID %in% hospitalJID,]
hos.A.immunosuppressant120 <- A.immunosuppressant120[A.immunosuppressant120$JID %in% hospitalJID,]
hos.A.Glucocorticoid120 <- hos.A.immunosuppressant120[( hos.A.immunosuppressant120$immunosuppressant == 0) | ( hos.A.immunosuppressant120$Glucocorticoid == 1 ),]

# dataset check
sink("./resultsCNLLS/after-d-check2.txt")
cat("\n\n ** STATIN 120 ** \n\n")
table("Before"=statin120$statin, "After"=A.statin120$statin)
frequency3(A.statin120, "statin")
table("Before"=hos.statin120$statin, "After"=hos.A.statin120$statin)
frequency3(hos.A.statin120, "statin")

cat("\n\n ** Anticoagulants 120 ** \n\n")
table("Before"=Anticoagulants120$Anticoagulants, "After"=A.Anticoagulants120$Anticoagulants)
frequency3(A.Anticoagulants120, "Anticoagulants")
table("Before"=hos.Anticoagulants120$Anticoagulants, "After"=hos.A.Anticoagulants120$Anticoagulants)
frequency3(hos.A.Anticoagulants120, "Anticoagulants")

cat("\n\n ** Immunosuppressant 120 ** \n\n")
table("Before"=immunosuppressant120$immunosuppressant, "After"=A.immunosuppressant120$immunosuppressant)
frequency3(A.immunosuppressant120, "immunosuppressant")
table("Before"=hos.immunosuppressant120$immunosuppressant, "After"=hos.A.immunosuppressant120$immunosuppressant)
frequency3(hos.A.immunosuppressant120, "immunosuppressant")

cat("\n\n ** Glucocorticoid 120 ** \n\n")
table("Before"=Glucocorticoid120$Glucocorticoid, "After"=A.Glucocorticoid120$Glucocorticoid)
frequency3(A.Glucocorticoid120, "Glucocorticoid")
table("Before"=hos.Glucocorticoid120$Glucocorticoid, "After"=hos.A.Glucocorticoid120$Glucocorticoid)
frequency3(hos.A.Glucocorticoid120, "Glucocorticoid")
sink() 

############################################
############   After statin  ###############
############################################
A.statin120.con <- A.statin120[,statin.confounder]
A.statin120.fit <- glm(A.statin120$statin ~ ., data = A.statin120.con, family="binomial")

sink("./resultsCNLLS/model/[After][ALL]results_A.statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(A.statin120))
print(statin.study[,c(2,1)])
summary(A.statin120.fit)
A.statin120.wt <- ps.weight(ps=A.statin120.fit$fitted.values, exposure=A.statin120$statin, name="[After][ALL]A.statin120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=A.statin120.wt, df=A.statin120, study=statin.study, name="[After][ALL]A.statin120") # 0.6249

outcome.model(Y="Death", wt_df=A.statin120.wt, df=A.statin120, study=statin.study, name="[After][ALL]A.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=A.statin120.wt, df=A.statin120, study=statin.study, name="[After][ALL]A.statin120")
outcome.model(Y="ICU.admission", wt_df=A.statin120.wt, df=A.statin120, study=statin.study, name="[After][ALL]A.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt_df=A.statin120.wt, df=A.statin120, study=statin.study, name="[After][ALL]A.statin120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=A.statin120.wt, df=A.statin120, study=statin.study, name="[After][ALL]A.statin120")

outcome.model(Y="re.outcome1", wt_df=A.statin120.wt, df=A.statin120, study=statin.study, name="[After][ALL]A.statin120") # 0.6249

print_table(df.all=A.statin120, wt=A.statin120.wt$iptw.wt, df.study=statin.study, save_name="[After][ALL]iptw.A.statin120")
print_table(df.all=A.statin120, wt=A.statin120.wt$trim.wt, df.study=statin.study, save_name="[After][ALL]trim.A.statin120")
print_table(df.all=A.statin120, wt=A.statin120.wt$st.iptw.wt, df.study=statin.study, save_name="[After][ALL]st.iptw.A.statin120")
print_table(df.all=A.statin120, wt=A.statin120.wt$iptw.att, df.study=statin.study, save_name="[After][ALL]iptw.att.A.statin120")
print_table(df.all=A.statin120, wt=A.statin120.wt$trim.att, df.study=statin.study, save_name="[After][ALL]trim.att.A.statin120")
print_table(df.all=A.statin120, wt=A.statin120.wt$st.iptw.att, df.study=statin.study, save_name="[After][ALL]st.iptw.att.A.statin120")
print_table(df.all=A.statin120, wt=A.statin120.wt$dec.wt, df.study=statin.study, save_name="[After][ALL]dec.A.statin120")
print_table(df.all=A.statin120, wt=A.statin120.wt$match.wt, df.study=statin.study, save_name="[After][ALL]match.A.statin120")
print_table(df.all=A.statin120, wt=A.statin120.wt$kernel.wt, df.study=statin.study, save_name="[After][ALL]kernel.A.statin120")
sink() 

hos.A.statin120.con <- hos.A.statin120[,statin.confounder]
hos.A.statin120.fit <- glm(hos.A.statin120$statin ~ ., data = hos.A.statin120.con, family="binomial")

sink("./resultsCNLLS/model/[After][HOS]results_hos.A.statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
print(nrow(hos.A.statin120))
print(statin.study[,c(2,1)])
summary(hos.A.statin120.fit)
hos.A.statin120.wt <- ps.weight(ps=hos.A.statin120.fit$fitted.values, exposure=hos.A.statin120$statin, name="[After][HOS]hos.A.statin120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.A.statin120.wt, df=hos.A.statin120, study=statin.study, name="[After][HOS]hos.A.statin120") # 0.6249

outcome.model(Y="Death", wt_df=hos.A.statin120.wt, df=hos.A.statin120, study=statin.study, name="[After][HOS]hos.A.statin120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=hos.A.statin120.wt, df=hos.A.statin120, study=statin.study, name="[After][HOS]hos.A.statin120")
outcome.model(Y="ICU.admission", wt_df=hos.A.statin120.wt, df=hos.A.statin120, study=statin.study, name="[After][HOS]hos.A.statin120")
outcome.model(Y="myocardial.infarction.ischemic.stroke.and.TIA", wt_df=hos.A.statin120.wt, df=hos.A.statin120, study=statin.study, name="[After][HOS]hos.A.statin120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=hos.A.statin120.wt, df=hos.A.statin120, study=statin.study, name="[After][HOS]hos.A.statin120")

outcome.model(Y="re.outcome1", wt_df=hos.A.statin120.wt, df=hos.A.statin120, study=statin.study, name="[After][HOS]hos.A.statin120") # 0.6249

print_table(df.all=hos.A.statin120, wt=hos.A.statin120.wt$iptw.wt, df.study=statin.study, save_name="[After][HOS]iptw.hos.A.statin120")
print_table(df.all=hos.A.statin120, wt=hos.A.statin120.wt$trim.wt, df.study=statin.study, save_name="[After][HOS]trim.hos.A.statin120")
print_table(df.all=hos.A.statin120, wt=hos.A.statin120.wt$st.iptw.wt, df.study=statin.study, save_name="[After][HOS]st.iptw.hos.A.statin120")
print_table(df.all=hos.A.statin120, wt=hos.A.statin120.wt$iptw.att, df.study=statin.study, save_name="[After][HOS]iptw.att.hos.A.statin120")
print_table(df.all=hos.A.statin120, wt=hos.A.statin120.wt$trim.att, df.study=statin.study, save_name="[After][HOS]trim.att.hos.A.statin120")
print_table(df.all=hos.A.statin120, wt=hos.A.statin120.wt$st.iptw.att, df.study=statin.study, save_name="[After][HOS]st.iptw.att.hos.A.statin120")
print_table(df.all=hos.A.statin120, wt=hos.A.statin120.wt$dec.wt, df.study=statin.study, save_name="[After][HOS]dec.hos.A.statin120")
print_table(df.all=hos.A.statin120, wt=hos.A.statin120.wt$match.wt, df.study=statin.study, save_name="[After][HOS]match.hos.A.statin120")
print_table(df.all=hos.A.statin120, wt=hos.A.statin120.wt$kernel.wt, df.study=statin.study, save_name="[After][HOS]kernel.hos.A.statin120")
sink() 

############################################
############   After immunosuppressant  ##############
############################################
A.immunosuppressant120.con <- A.immunosuppressant120[,immunosuppressant.confounder]
A.immunosuppressant120.fit <- glm(A.immunosuppressant120$immunosuppressant ~ ., data = A.immunosuppressant120.con, family="binomial")

sink("./resultsCNLLS/model/[After][ALL]results_A.immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(A.immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])
summary(A.immunosuppressant120.fit)
A.immunosuppressant120.wt <- ps.weight(ps=A.immunosuppressant120.fit$fitted.values, exposure=A.immunosuppressant120$immunosuppressant, name="[After][ALL]A.immunosuppressant120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=A.immunosuppressant120.wt, df=A.immunosuppressant120, study=immunosuppressant.study, name="[After][ALL]A.immunosuppressant120") # 0.6249

outcome.model(Y="Death", wt_df=A.immunosuppressant120.wt, df=A.immunosuppressant120, study=immunosuppressant.study, name="[After][ALL]A.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=A.immunosuppressant120.wt, df=A.immunosuppressant120, study=immunosuppressant.study, name="[After][ALL]A.immunosuppressant120")
outcome.model(Y="ICU.admission", wt_df=A.immunosuppressant120.wt, df=A.immunosuppressant120, study=immunosuppressant.study, name="[After][ALL]A.immunosuppressant120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=A.immunosuppressant120.wt, df=A.immunosuppressant120, study=immunosuppressant.study, name="[After][ALL]A.immunosuppressant120")

print_table(df.all=A.immunosuppressant120, wt=A.immunosuppressant120.wt$iptw.wt, df.study=immunosuppressant.study, save_name="[After][ALL]iptw.A.immunosuppressant120")
print_table(df.all=A.immunosuppressant120, wt=A.immunosuppressant120.wt$trim.wt, df.study=immunosuppressant.study, save_name="[After][ALL]trim.A.immunosuppressant120")
print_table(df.all=A.immunosuppressant120, wt=A.immunosuppressant120.wt$st.iptw.wt, df.study=immunosuppressant.study, save_name="[After][ALL]st.iptw.A.immunosuppressant120")
print_table(df.all=A.immunosuppressant120, wt=A.immunosuppressant120.wt$iptw.att, df.study=immunosuppressant.study, save_name="[After][ALL]iptw.att.A.immunosuppressant120")
print_table(df.all=A.immunosuppressant120, wt=A.immunosuppressant120.wt$trim.att, df.study=immunosuppressant.study, save_name="[After][ALL]trim.att.A.immunosuppressant120")
print_table(df.all=A.immunosuppressant120, wt=A.immunosuppressant120.wt$st.iptw.att, df.study=immunosuppressant.study, save_name="[After][ALL]st.iptw.att.A.immunosuppressant120")
print_table(df.all=A.immunosuppressant120, wt=A.immunosuppressant120.wt$dec.wt, df.study=immunosuppressant.study, save_name="[After][ALL]dec.A.immunosuppressant120")
print_table(df.all=A.immunosuppressant120, wt=A.immunosuppressant120.wt$match.wt, df.study=immunosuppressant.study, save_name="[After][ALL]match.A.immunosuppressant120")
print_table(df.all=A.immunosuppressant120, wt=A.immunosuppressant120.wt$kernel.wt, df.study=immunosuppressant.study, save_name="[After][ALL]kernel.A.immunosuppressant120")
sink() 

hos.A.immunosuppressant120.con <- hos.A.immunosuppressant120[,immunosuppressant.confounder]
hos.A.immunosuppressant120.fit <- glm(hos.A.immunosuppressant120$immunosuppressant ~ ., data = hos.A.immunosuppressant120.con, family="binomial")

sink("./resultsCNLLS/model/[After][HOS]results_hos.A.immunosuppressant120.txt")
cat("\n <--------- [immunosuppressant 120] Result (primary) ---------> \n")
print(nrow(hos.A.immunosuppressant120))
print(immunosuppressant.study[,c(2,1)])
summary(hos.A.immunosuppressant120.fit)
hos.A.immunosuppressant120.wt <- ps.weight(ps=hos.A.immunosuppressant120.fit$fitted.values, exposure=hos.A.immunosuppressant120$immunosuppressant, name="[After][HOS]hos.A.immunosuppressant120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.A.immunosuppressant120.wt, df=hos.A.immunosuppressant120, study=immunosuppressant.study, name="[After][HOS]hos.A.immunosuppressant120") # 0.6249

outcome.model(Y="Death", wt_df=hos.A.immunosuppressant120.wt, df=hos.A.immunosuppressant120, study=immunosuppressant.study, name="[After][HOS]hos.A.immunosuppressant120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=hos.A.immunosuppressant120.wt, df=hos.A.immunosuppressant120, study=immunosuppressant.study, name="[After][HOS]hos.A.immunosuppressant120")
outcome.model(Y="ICU.admission", wt_df=hos.A.immunosuppressant120.wt, df=hos.A.immunosuppressant120, study=immunosuppressant.study, name="[After][HOS]hos.A.immunosuppressant120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=hos.A.immunosuppressant120.wt, df=hos.A.immunosuppressant120, study=immunosuppressant.study, name="[After][HOS]hos.A.immunosuppressant120")

print_table(df.all=hos.A.immunosuppressant120, wt=hos.A.immunosuppressant120.wt$iptw.wt, df.study=immunosuppressant.study, save_name="[After][HOS]iptw.hos.A.immunosuppressant120")
print_table(df.all=hos.A.immunosuppressant120, wt=hos.A.immunosuppressant120.wt$trim.wt, df.study=immunosuppressant.study, save_name="[After][HOS]trim.hos.A.immunosuppressant120")
print_table(df.all=hos.A.immunosuppressant120, wt=hos.A.immunosuppressant120.wt$st.iptw.wt, df.study=immunosuppressant.study, save_name="[After][HOS]st.iptw.hos.A.immunosuppressant120")
print_table(df.all=hos.A.immunosuppressant120, wt=hos.A.immunosuppressant120.wt$iptw.att, df.study=immunosuppressant.study, save_name="[After][HOS]iptw.att.hos.A.immunosuppressant120")
print_table(df.all=hos.A.immunosuppressant120, wt=hos.A.immunosuppressant120.wt$trim.att, df.study=immunosuppressant.study, save_name="[After][HOS]trim.att.hos.A.immunosuppressant120")
print_table(df.all=hos.A.immunosuppressant120, wt=hos.A.immunosuppressant120.wt$st.iptw.att, df.study=immunosuppressant.study, save_name="[After][HOS]st.iptw.att.hos.A.immunosuppressant120")
print_table(df.all=hos.A.immunosuppressant120, wt=hos.A.immunosuppressant120.wt$dec.wt, df.study=immunosuppressant.study, save_name="[After][HOS]dec.hos.A.immunosuppressant120")
print_table(df.all=hos.A.immunosuppressant120, wt=hos.A.immunosuppressant120.wt$match.wt, df.study=immunosuppressant.study, save_name="[After][HOS]match.hos.A.immunosuppressant120")
print_table(df.all=hos.A.immunosuppressant120, wt=hos.A.immunosuppressant120.wt$kernel.wt, df.study=immunosuppressant.study, save_name="[After][HOS]kernel.hos.A.immunosuppressant120")
sink() 

############################################
############   After Glucocorticoid  ##############
############################################
A.Glucocorticoid120.con <- A.Glucocorticoid120[,Glucocorticoid.confounder]
A.Glucocorticoid120.fit <- glm(A.Glucocorticoid120$Glucocorticoid ~ ., data = A.Glucocorticoid120.con, family="binomial")

sink("./resultsCNLLS/model/[After][ALL]results_A.Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(A.Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])
summary(A.Glucocorticoid120.fit)
A.Glucocorticoid120.wt <- ps.weight(ps=A.Glucocorticoid120.fit$fitted.values, exposure=A.Glucocorticoid120$Glucocorticoid, name="[After][ALL]A.Glucocorticoid120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=A.Glucocorticoid120.wt, df=A.Glucocorticoid120, study=Glucocorticoid.study, name="[After][ALL]A.Glucocorticoid120") # 0.6249

outcome.model(Y="Death", wt_df=A.Glucocorticoid120.wt, df=A.Glucocorticoid120, study=Glucocorticoid.study, name="[After][ALL]A.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=A.Glucocorticoid120.wt, df=A.Glucocorticoid120, study=Glucocorticoid.study, name="[After][ALL]A.Glucocorticoid120")
outcome.model(Y="ICU.admission", wt_df=A.Glucocorticoid120.wt, df=A.Glucocorticoid120, study=Glucocorticoid.study, name="[After][ALL]A.Glucocorticoid120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=A.Glucocorticoid120.wt, df=A.Glucocorticoid120, study=Glucocorticoid.study, name="[After][ALL]A.Glucocorticoid120")

print_table(df.all=A.Glucocorticoid120, wt=A.Glucocorticoid120.wt$iptw.wt, df.study=Glucocorticoid.study, save_name="[After][ALL]iptw.A.Glucocorticoid120")
print_table(df.all=A.Glucocorticoid120, wt=A.Glucocorticoid120.wt$trim.wt, df.study=Glucocorticoid.study, save_name="[After][ALL]trim.A.Glucocorticoid120")
print_table(df.all=A.Glucocorticoid120, wt=A.Glucocorticoid120.wt$st.iptw.wt, df.study=Glucocorticoid.study, save_name="[After][ALL]st.iptw.A.Glucocorticoid120")
print_table(df.all=A.Glucocorticoid120, wt=A.Glucocorticoid120.wt$iptw.att, df.study=Glucocorticoid.study, save_name="[After][ALL]iptw.att.A.Glucocorticoid120")
print_table(df.all=A.Glucocorticoid120, wt=A.Glucocorticoid120.wt$trim.att, df.study=Glucocorticoid.study, save_name="[After][ALL]trim.att.A.Glucocorticoid120")
print_table(df.all=A.Glucocorticoid120, wt=A.Glucocorticoid120.wt$st.iptw.att, df.study=Glucocorticoid.study, save_name="[After][ALL]st.iptw.att.A.Glucocorticoid120")
print_table(df.all=A.Glucocorticoid120, wt=A.Glucocorticoid120.wt$dec.wt, df.study=Glucocorticoid.study, save_name="[After][ALL]dec.A.Glucocorticoid120")
print_table(df.all=A.Glucocorticoid120, wt=A.Glucocorticoid120.wt$match.wt, df.study=Glucocorticoid.study, save_name="[After][ALL]match.A.Glucocorticoid120")
print_table(df.all=A.Glucocorticoid120, wt=A.Glucocorticoid120.wt$kernel.wt, df.study=Glucocorticoid.study, save_name="[After][ALL]kernel.A.Glucocorticoid120")
sink() 

hos.A.Glucocorticoid120.con <- hos.A.Glucocorticoid120[,Glucocorticoid.confounder]
hos.A.Glucocorticoid120.fit <- glm(hos.A.Glucocorticoid120$Glucocorticoid ~ ., data = hos.A.Glucocorticoid120.con, family="binomial")

sink("./resultsCNLLS/model/[After][HOS]results_hos.A.Glucocorticoid120.txt")
cat("\n <--------- [Glucocorticoid 120] Result (primary) ---------> \n")
print(nrow(hos.A.Glucocorticoid120))
print(Glucocorticoid.study[,c(2,1)])
summary(hos.A.Glucocorticoid120.fit)
hos.A.Glucocorticoid120.wt <- ps.weight(ps=hos.A.Glucocorticoid120.fit$fitted.values, exposure=hos.A.Glucocorticoid120$Glucocorticoid, name="[After][HOS]hos.A.Glucocorticoid120")
cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.A.Glucocorticoid120.wt, df=hos.A.Glucocorticoid120, study=Glucocorticoid.study, name="[After][HOS]hos.A.Glucocorticoid120") # 0.6249

outcome.model(Y="Death", wt_df=hos.A.Glucocorticoid120.wt, df=hos.A.Glucocorticoid120, study=Glucocorticoid.study, name="[After][HOS]hos.A.Glucocorticoid120")
outcome.model(Y="Use.of.mechanical.ventilation", wt_df=hos.A.Glucocorticoid120.wt, df=hos.A.Glucocorticoid120, study=Glucocorticoid.study, name="[After][HOS]hos.A.Glucocorticoid120")
outcome.model(Y="ICU.admission", wt_df=hos.A.Glucocorticoid120.wt, df=hos.A.Glucocorticoid120, study=Glucocorticoid.study, name="[After][HOS]hos.A.Glucocorticoid120")

outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt_df=hos.A.Glucocorticoid120.wt, df=hos.A.Glucocorticoid120, study=Glucocorticoid.study, name="[After][HOS]hos.A.Glucocorticoid120")

print_table(df.all=hos.A.Glucocorticoid120, wt=hos.A.Glucocorticoid120.wt$iptw.wt, df.study=Glucocorticoid.study, save_name="[After][HOS]iptw.hos.A.Glucocorticoid120")
print_table(df.all=hos.A.Glucocorticoid120, wt=hos.A.Glucocorticoid120.wt$trim.wt, df.study=Glucocorticoid.study, save_name="[After][HOS]trim.hos.A.Glucocorticoid120")
print_table(df.all=hos.A.Glucocorticoid120, wt=hos.A.Glucocorticoid120.wt$st.iptw.wt, df.study=Glucocorticoid.study, save_name="[After][HOS]st.iptw.hos.A.Glucocorticoid120")
print_table(df.all=hos.A.Glucocorticoid120, wt=hos.A.Glucocorticoid120.wt$iptw.att, df.study=Glucocorticoid.study, save_name="[After][HOS]iptw.att.hos.A.Glucocorticoid120")
print_table(df.all=hos.A.Glucocorticoid120, wt=hos.A.Glucocorticoid120.wt$trim.att, df.study=Glucocorticoid.study, save_name="[After][HOS]trim.att.hos.A.Glucocorticoid120")
print_table(df.all=hos.A.Glucocorticoid120, wt=hos.A.Glucocorticoid120.wt$st.iptw.att, df.study=Glucocorticoid.study, save_name="[After][HOS]st.iptw.att.hos.A.Glucocorticoid120")
print_table(df.all=hos.A.Glucocorticoid120, wt=hos.A.Glucocorticoid120.wt$dec.wt, df.study=Glucocorticoid.study, save_name="[After][HOS]dec.hos.A.Glucocorticoid120")
print_table(df.all=hos.A.Glucocorticoid120, wt=hos.A.Glucocorticoid120.wt$match.wt, df.study=Glucocorticoid.study, save_name="[After][HOS]match.hos.A.Glucocorticoid120")
print_table(df.all=hos.A.Glucocorticoid120, wt=hos.A.Glucocorticoid120.wt$kernel.wt, df.study=Glucocorticoid.study, save_name="[After][HOS]kernel.hos.A.Glucocorticoid120")
sink() 


###############################################
#############    After Anticoagulants    ################
###############################################
A.Anticoagulants120.con <- A.Anticoagulants120[,Anticoagulants.confounder]
A.Anticoagulants120.fit <- glm(A.Anticoagulants120$Anticoagulants ~ ., data = A.Anticoagulants120.con, family="binomial")

sink("./resultsCNLLS/model/[After][ALL]results_A.Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(A.Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])
summary(A.Anticoagulants120.fit)

A.Anticoagulants120.wt <- ps.weight(ps=A.Anticoagulants120.fit$fitted.values, exposure=A.Anticoagulants120$Anticoagulants, name="[After][ALL]A.Anticoagulants120")

cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=A.Anticoagulants120.wt, df=A.Anticoagulants120, study=Anticoagulants.study, name="[After][ALL]A.Anticoagulants120") # 0.6249

outcome.model(Y="Death", wt=A.Anticoagulants120.wt, df=A.Anticoagulants120, study=Anticoagulants.study, name="[After][ALL]A.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=A.Anticoagulants120.wt, df=A.Anticoagulants120, study=Anticoagulants.study, name="[After][ALL]A.Anticoagulants120")
outcome.model(Y="ICU.admission", wt=A.Anticoagulants120.wt, df=A.Anticoagulants120, study=Anticoagulants.study, name="[After][ALL]A.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=A.Anticoagulants120.wt, df=A.Anticoagulants120, study=Anticoagulants.study, name="[After][ALL]A.Anticoagulants120")

outcome.model(Y="re.outcome1", wt_df=A.Anticoagulants120.wt, df=A.Anticoagulants120, study=Anticoagulants.study, name="[After][ALL]A.Anticoagulants120") # 0.6249

cat("\n\n -- PRINT -- \n\n")
print_table(df.all=A.Anticoagulants120, wt=A.Anticoagulants120.wt$iptw.wt, df.study=Anticoagulants.study, save_name="[After][ALL]iptw.A.Anticoagulants120")
print_table(df.all=A.Anticoagulants120, wt=A.Anticoagulants120.wt$trim.wt, df.study=Anticoagulants.study, save_name="[After][ALL]trim.A.Anticoagulants120")
print_table(df.all=A.Anticoagulants120, wt=A.Anticoagulants120.wt$st.iptw.wt, df.study=Anticoagulants.study, save_name="[After][ALL]st.iptw.A.Anticoagulants120")
print_table(df.all=A.Anticoagulants120, wt=A.Anticoagulants120.wt$iptw.att, df.study=Anticoagulants.study, save_name="[After][ALL]iptw.att.A.Anticoagulants120")
print_table(df.all=A.Anticoagulants120, wt=A.Anticoagulants120.wt$trim.att, df.study=Anticoagulants.study, save_name="[After][ALL]trim.att.A.Anticoagulants120")
print_table(df.all=A.Anticoagulants120, wt=A.Anticoagulants120.wt$st.iptw.att, df.study=Anticoagulants.study, save_name="[After][ALL]st.iptw.attA.Anticoagulants120")
print_table(df.all=A.Anticoagulants120, wt=A.Anticoagulants120.wt$dec.wt, df.study=Anticoagulants.study, save_name="[After][ALL]dec.A.Anticoagulants120")
print_table(df.all=A.Anticoagulants120, wt=A.Anticoagulants120.wt$match.wt, df.study=Anticoagulants.study, save_name="[After][ALL]match.A.Anticoagulants120")
print_table(df.all=A.Anticoagulants120, wt=A.Anticoagulants120.wt$kernel.wt, df.study=Anticoagulants.study, save_name="[After][ALL]kernel.A.Anticoagulants120")

#### Bleeding and Transfusion ####
Anticoagulants.scr1 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr1 <- glm(A.Anticoagulants120[,"bleeding.transfusion"]~A.Anticoagulants120[,c], data=A.Anticoagulants120, family = "binomial")
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

scr1 <- A.Anticoagulants120[,Anticoagulants.scr1]
A.Anticoagulants120.fit1 <- glm(A.Anticoagulants120$Anticoagulants ~ ., data = scr1, family="binomial")

A.Anticoagulants120.wt1 <- ps.weight(ps=A.Anticoagulants120.fit1$fitted.values,
                                     exposure=A.Anticoagulants120$Anticoagulants,
                                     name="[After][ALL]A.Anticoagulants120_1")

Anticoagulants.study1 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr1, type = rep("Confounder", length(Anticoagulants.scr1))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))

outcome.model(Y="bleeding.transfusion", wt_df=A.Anticoagulants120.wt1, df=A.Anticoagulants120, study=Anticoagulants.study1, name="[After][ALL]A.Anticoagulants120_1")


#### myocardial.stroke.TIA.Thromboembolism ####
Anticoagulants.scr2 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr2 <- glm(A.Anticoagulants120[,"myocardial.storke.TIA.Thromboembolism"]~A.Anticoagulants120[,c], data=A.Anticoagulants120, family = "binomial")
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
scr2 <- A.Anticoagulants120[,Anticoagulants.scr2]

A.Anticoagulants120.fit2 <- glm(A.Anticoagulants120$Anticoagulants ~ ., data = scr1, family="binomial")

A.Anticoagulants120.wt2 <- ps.weight(ps=A.Anticoagulants120.fit2$fitted.values, exposure=A.Anticoagulants120$Anticoagulants, name="[After][ALL]A.Anticoagulants120")

Anticoagulants.study2 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr2, type = rep("Confounder", length(Anticoagulants.scr2))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))


outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt_df=A.Anticoagulants120.wt2, df=A.Anticoagulants120, study=Anticoagulants.study2, name="[After][ALL]A.Anticoagulants120_2")
sink()

hos.A.Anticoagulants120.con <- hos.A.Anticoagulants120[,Anticoagulants.confounder]
hos.A.Anticoagulants120.fit <- glm(hos.A.Anticoagulants120$Anticoagulants ~ ., data = hos.A.Anticoagulants120.con, family="binomial")

sink("./resultsCNLLS/model/[After][HOS]results_hos.A.Anticoagulants120.txt")
cat("\n <--------- [Anticoagulants 120] Result (primary) ---------> \n")
print(nrow(hos.A.Anticoagulants120))
print(Anticoagulants.study[,c(2,1)])
summary(hos.A.Anticoagulants120.fit)

# deciles, trimming... 
# --> error (count 수가 너무 적어서 sample data가 에러 발생)
# --> (계획) error 발생해도 진행되도록.
hos.A.Anticoagulants120.wt <- ps.weight(ps=hos.A.Anticoagulants120.fit$fitted.values, exposure=hos.A.Anticoagulants120$Anticoagulants, name="[After][HOS]hos.A.Anticoagulants120")

cat("\n")
cat(" Results \n") # -1.1
cat("\n")

outcome.model(Y="outcome1", wt_df=hos.A.Anticoagulants120.wt, df=hos.A.Anticoagulants120, study=Anticoagulants.study, name="[After][HOS]hos.A.Anticoagulants120") # 0.6249

outcome.model(Y="Death", wt=hos.A.Anticoagulants120.wt, df=hos.A.Anticoagulants120, study=Anticoagulants.study, name="[After][HOS]hos.A.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation", wt=hos.A.Anticoagulants120.wt, df=hos.A.Anticoagulants120, study=Anticoagulants.study, name="[After][HOS]hos.A.Anticoagulants120")
outcome.model(Y="ICU.admission", wt=hos.A.Anticoagulants120.wt, df=hos.A.Anticoagulants120, study=Anticoagulants.study, name="[After][HOS]hos.A.Anticoagulants120")
outcome.model(Y="Use.of.mechanical.ventilation.ICU.admission", wt=hos.A.Anticoagulants120.wt, df=hos.A.Anticoagulants120, study=Anticoagulants.study, name="[After][HOS]hos.A.Anticoagulants120")

outcome.model(Y="re.outcome1", wt_df=hos.A.Anticoagulants120.wt, df=hos.A.Anticoagulants120, study=Anticoagulants.study, name="[After][HOS]hos.A.Anticoagulants120") # 0.6249

cat("\n\n -- PRINT -- \n\n")
print_table(df.all=hos.A.Anticoagulants120, wt=hos.A.Anticoagulants120.wt$iptw.wt, df.study=Anticoagulants.study, save_name="[After][HOS]iptw.hos.A.Anticoagulants120")
print_table(df.all=hos.A.Anticoagulants120, wt=hos.A.Anticoagulants120.wt$trim.wt, df.study=Anticoagulants.study, save_name="[After][HOS]trim.hos.A.Anticoagulants120")
print_table(df.all=hos.A.Anticoagulants120, wt=hos.A.Anticoagulants120.wt$st.iptw.wt, df.study=Anticoagulants.study, save_name="[After][HOS]st.iptw.hos.A.Anticoagulants120")
print_table(df.all=hos.A.Anticoagulants120, wt=hos.A.Anticoagulants120.wt$iptw.att, df.study=Anticoagulants.study, save_name="[After][HOS]iptw.att.hos.A.Anticoagulants120")
print_table(df.all=hos.A.Anticoagulants120, wt=hos.A.Anticoagulants120.wt$trim.att, df.study=Anticoagulants.study, save_name="[After][HOS]trim.att.hos.A.Anticoagulants120")
print_table(df.all=hos.A.Anticoagulants120, wt=hos.A.Anticoagulants120.wt$st.iptw.att, df.study=Anticoagulants.study, save_name="[After][HOS]st.iptw.atthos.A.Anticoagulants120")
print_table(df.all=hos.A.Anticoagulants120, wt=hos.A.Anticoagulants120.wt$dec.wt, df.study=Anticoagulants.study, save_name="[After][HOS]dec.hos.A.Anticoagulants120")
print_table(df.all=hos.A.Anticoagulants120, wt=hos.A.Anticoagulants120.wt$match.wt, df.study=Anticoagulants.study, save_name="[After][HOS]match.hos.A.Anticoagulants120")
print_table(df.all=hos.A.Anticoagulants120, wt=hos.A.Anticoagulants120.wt$kernel.wt, df.study=Anticoagulants.study, save_name="[After][HOS]kernel.hos.A.Anticoagulants120")

#### Bleeding and Transfusion ####
Anticoagulants.scr1 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr1 <- glm(hos.A.Anticoagulants120[,"bleeding.transfusion"]~hos.A.Anticoagulants120[,c], data=hos.A.Anticoagulants120, family = "binomial")
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

scr1 <- hos.A.Anticoagulants120[,Anticoagulants.scr1]
hos.A.Anticoagulants120.fit1 <- glm(hos.A.Anticoagulants120$Anticoagulants ~ ., data = scr1, family="binomial")

hos.A.Anticoagulants120.wt1 <- ps.weight(ps=hos.A.Anticoagulants120.fit1$fitted.values,
                                         exposure=hos.A.Anticoagulants120$Anticoagulants,
                                         name="[After][HOS]hos.A.Anticoagulants120_1")

Anticoagulants.study1 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr1, type = rep("Confounder", length(Anticoagulants.scr1))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))

outcome.model(Y="bleeding.transfusion", wt_df=hos.A.Anticoagulants120.wt1, df=hos.A.Anticoagulants120, study=Anticoagulants.study1, name="[After][HOS]hos.A.Anticoagulants120_1")


#### myocardial.stroke.TIA.Thromboembolism ####
Anticoagulants.scr2 <- c("AGE", "GENDER", "MedInfo")
for (c in Anticoagulants.confounder[4:length(Anticoagulants.confounder)]){
  # outcome2 ~ confounder 중 p-value>0.2 인 confounder 찾기.
  cat("\n", c, "\n")
  fit.scr2 <- glm(hos.A.Anticoagulants120[,"myocardial.storke.TIA.Thromboembolism"]~hos.A.Anticoagulants120[,c], data=hos.A.Anticoagulants120, family = "binomial")
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
scr2 <- hos.A.Anticoagulants120[,Anticoagulants.scr2]

hos.A.Anticoagulants120.fit2 <- glm(hos.A.Anticoagulants120$Anticoagulants ~ ., data = scr1, family="binomial")

hos.A.Anticoagulants120.wt2 <- ps.weight(ps=hos.A.Anticoagulants120.fit2$fitted.values, exposure=hos.A.Anticoagulants120$Anticoagulants, name="[After][HOS]hos.A.Anticoagulants120_2")

Anticoagulants.study2 <- rbind(data.frame(var = Anticoagulants.exposure, type = "Exposure"),
                               data.frame(var = Anticoagulants.scr2, type = rep("Confounder", length(Anticoagulants.scr2))),
                               data.frame(var = Anticoagulants.outcome1, type = rep("Outcome1", length(Anticoagulants.outcome1))),
                               data.frame(var = Anticoagulants.outcome2, type = rep("Outcome2", length(Anticoagulants.outcome2))), 
                               data.frame(var = Anticoagulants.Emodifier, type = rep("Effect.Modifier",length(Anticoagulants.Emodifier))))


outcome.model(Y="myocardial.storke.TIA.Thromboembolism", wt_df=hos.A.Anticoagulants120.wt2, df=hos.A.Anticoagulants120, study=Anticoagulants.study2, name="[After][HOS]hos.A.Anticoagulants120_2")
sink() 

dist_check(df.all=statin120, wt.df=statin120.wt, study=statin.study, save_name="[Before][ALL]statin120")
dist_check(df.all=hos.statin120, wt.df=hos.statin120.wt, study=statin.study, save_name="[Before][HOS]hos.statin120")
dist_check(df.all=Anticoagulants120, wt.df=Anticoagulants120.wt, study=Anticoagulants.study, save_name="[Before][ALL]Anticoagulants120")
dist_check(df.all=hos.Anticoagulants120, wt.df=hos.Anticoagulants120.wt, study=Anticoagulants.study, save_name="[Before][HOS]hos.Anticoagulants120")
dist_check(df.all=immunosuppressant120, wt.df=immunosuppressant120.wt, study=immunosuppressant.study, save_name="[Before][ALL]immunosuppressant120")
dist_check(df.all=hos.immunosuppressant120, wt.df=hos.immunosuppressant120.wt, study=immunosuppressant.study, save_name="[Before][HOS]hos.immunosuppressant120")
dist_check(df.all=Glucocorticoid120, wt.df=Glucocorticoid120.wt, study=Glucocorticoid.study, save_name="[Before][ALL]Glucocorticoid120")
dist_check(df.all=hos.Glucocorticoid120, wt.df=hos.Glucocorticoid120.wt, study=Glucocorticoid.study, save_name="[Before][HOS]hos.Glucocorticoid120")

dist_check(df.all=A.statin120, wt.df=A.statin120.wt, study=statin.study, save_name="[After][ALL]statin120")
dist_check(df.all=hos.A.statin120, wt.df=hos.A.statin120.wt, study=statin.study, save_name="[After][HOS]hos.statin120")
dist_check(df.all=A.Anticoagulants120, wt.df=A.Anticoagulants120.wt, study=Anticoagulants.study, save_name="[After][ALL]Anticoagulants120")
dist_check(df.all=hos.A.Anticoagulants120, wt.df=hos.A.Anticoagulants120.wt, study=Anticoagulants.study, save_name="[After][HOS]hos.Anticoagulants120")
dist_check(df.all=A.immunosuppressant120, wt.df=A.immunosuppressant120.wt, study=immunosuppressant.study, save_name="[After][ALL]immunosuppressant120")
dist_check(df.all=hos.A.immunosuppressant120, wt.df=hos.A.immunosuppressant120.wt, study=immunosuppressant.study, save_name="[After][HOS]hos.immunosuppressant120")
dist_check(df.all=A.Glucocorticoid120, wt.df=A.Glucocorticoid120.wt, study=Glucocorticoid.study, save_name="[After][ALL]Glucocorticoid120")
dist_check(df.all=hos.A.Glucocorticoid120, wt.df=hos.A.Glucocorticoid120.wt, study=Glucocorticoid.study, save_name="[After][HOS]hos.Glucocorticoid120")
