####################################################################
######      IMPORT RAW DATA AND EXTERNAL DATA      #################
####################################################################
# external data:
# 1. code-books-200721.xlsx
# 2. ATCcode_mapping.xlsx

library(readxl)
# library(dplyr)
library(data.table)

dir.create("./resultsCNLLS/", showWarnings=FALSE)
options(warn=-1)

# raw data (PLEASE REPLACE sample_schema.xlsx WITH THE RAW DATA)
data_path <- "./sample_schema.xlsx"
co19_t200 = data.frame(read_excel(data_path, sheet=4))
co19_t300 = data.frame(read_excel(data_path, sheet=5))
co19_t400 = data.frame(read_excel(data_path, sheet=6))
co19_t530 = data.frame(read_excel(data_path, sheet=7))

co19_t200[is.na(co19_t200$CONFIRM), "CONFIRM"] <- "N"
covid_jid <- unique(co19_t200[co19_t200$CONFIRM == "Y",]$JID)

# Medical use history data
co19_twjhe200 = data.frame(read_excel(data_path, sheet=8))
co19_twjhe300 = data.frame(read_excel(data_path, sheet=9))
co19_twjhe400 = data.frame(read_excel(data_path, sheet=10))
co19_twjhe530 = data.frame(read_excel(data_path, sheet=11))

# -------------------------------------------------- #
co19_t200_trans_dn <- co19_t200[co19_t200$JID %in% covid_jid, c("JID", "MID", "INSUP_TP_CD", "SEX_TP_CD", "RECU_FR_DD", "PAT_BTH" , "MAIN_SICK", "SUB_SICK", "DEATH", "CONFIRM")]
co19_t200_trans_dn$PAT_BTH <- as.Date(as.character(co19_t200_trans_dn$PAT_BTH), "%Y%m%d")
co19_t200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_t200_trans_dn$RECU_FR_DD), "%Y%m%d")

co19_t300_trans_dn <- co19_t300[co19_t200$JID %in% covid_jid, c("JID", "MID", "DIV_CD")]
co19_t400_trans_dn <- co19_t400[co19_t200$JID %in% covid_jid, c("JID", "MID", "SICK_CD")]
co19_t530_trans_dn <- co19_t530[co19_t200$JID %in% covid_jid, c("JID", "MID", "DIV_CD", "GNL_CD")]

co19_twjhe200_trans_dn <- co19_twjhe200[co19_t200$JID %in% covid_jid, c("JID", "MID", "SEX_TP_CD", "RECU_FR_DD", "PAT_AGE", "MAIN_SICK", "SUB_SICK", "RECU_FR_DD")]
co19_twjhe200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_twjhe200_trans_dn$RECU_FR_DD), "%Y%m%d")

co19_twjhe300_trans_dn <- co19_twjhe300[co19_t200$JID %in% covid_jid, c("JID", "MID", "DIV_CD")]
co19_twjhe400_trans_dn <- co19_twjhe400[co19_t200$JID %in% covid_jid, c("JID", "MID", "SICK_CD")]
co19_twjhe530_trans_dn <- co19_twjhe530[co19_t200$JID %in% covid_jid, c("JID", "MID", "DIV_CD", "GNL_CD")]

# External files (code_book)
codebook_path <- "./"
codebook <- "code-books-200723.xlsx"

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

####################################################################
##########     THE END OF IMPORT EXTERNAL DATA     #################
####################################################################

code_ICD10_df <- data.frame(code_ICD10[,c("uniqueID", "name", "code", "codeType", "searchFor", "statin study", "antiplatelet/anticoagulant study", "immune suppressant study")])
code_ATC_df <- data.frame(code_ATC[,c("uniqueID", "name","code", "codeType", "searchFor", "statin study", "antiplatelet/anticoagulant study", "immune suppressant study")])
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

####################################################################
#####################      Function Def      #######################
####################################################################
verbose.period = 50
gen_dummies <- function(codeBook, study = NULL, Bdays = 0){
  print("[START] > Bdays for loop; jid in patient_info$JID \n")
  time1 = Sys.time()
  df_rule <- NULL
  if (Bdays!=0) {
    
  }
  final <- data.frame(patient_info)
  print("[END] > Bdays for loop; jid in patient_info$JID\n")
  print("[START] > codeBook for loop; i in 1:norw(codeBook)\n")
  
  for (i in 1:nrow(codeBook)) {
    verbose = (i %% verbose.period == 1)
    
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
        dummy2 <- NULL
        
        dummy2_temp <- data.frame("JID" = df$JID, "var" = ifelse(toupper(df[,var_name]) %in% mapped_var, 1, 0))
        dummy2_temp[is.na(dummy2_temp[,"var"]),"var"] <- 0
        dummy2 <- as.data.table(dummy2_temp)[, .(var_sum = sum(var)), by=JID]
        
      } else { #--------- (else -1-) codeBook$codeType != "ATC" ---------#
        dummy_temp <- data.frame("JID" = df$JID, "var" = ifelse(toupper(substr(df[,var_name],1,code_len)) == toupper(code_val),1,0))
        dummy_temp[is.na(dummy_temp[,"var"]),"var"] <- 0
        dummy2 <- as.data.table(dummy_temp)[, .(var_sum = sum(var)), by=JID]
      }
      
      
      if (j == 1) {
        dummy <- dummy2
      } else { 
        dummy$var_sum <- dummy$var_sum + dummy2$var_sum
      }
      j = j+1
      
    } # while loop exit
    if (verbose) print("[END] >> sF_split1 while loop;\n")
    
    if(is.null(dummy)) next
    
    final_temp <- data.frame("JID" = dummy$JID, "var" = ifelse(dummy$var_sum>0,1,0))
    
    colnames(final_temp) <- c("JID", uniqueCode)
    final <- merge(final, final_temp, by=c("JID"), all=TRUE)
    
  }
  print("[END] > codeBook for loop; i in 1:norw(codeBook)\n")
  # [!] 추가함
  final[is.na(final)] <- 0
  
  time2 = Sys.time()
  cat("\n [Total Time] : ", time2-time1)
  
  return (data.frame(final))
}

gen_dummies <- function(patient_df, codeBook, study = NULL, Bdays = 0){
  time1 = Sys.time()
  df_rule <- NULL
  print("[START] > Bdays for loop; jid in patient_df$JID \n")
  
  for (jid in patient_df$JID) {
    df_temp <- co19_twjhe200_trans_dn[which(co19_twjhe200_trans_dn$JID == jid),]
      
    cat(sprintf(" [V] nrow(df_temp) = %d \n", nrow(df_temp)))
    if(!nrow(df_temp)) next
      
    first <- patient_df[patient_df$JID == jid,]$first_covid
    exposure_rule <- (df_temp$RECU_FR_DD <= first) & (df_temp$RECU_FR_DD >= first - Bdays)
    confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays)
    
    # rule 추가가능 : 탐색 기준
    df_rule_temp <- data.frame("JID" = jid,
                               "MID" = df_temp$MID,
                               "EXPOSURE.itself" = exposure_rule,
                               "exposure.and.outcome.related" = confounder_rule,
                               "outcome.related" = confounder_rule,
                               "x" = confounder_rule,
                               "x.exposure.only" = confounder_rule,
                               "all" = rep(TRUE, length(confounder_rule)))
        
    df_rule <- rbind(df_rule, df_rule_temp)
    df_temp <- NULL
    # print(colSums(df_rule[df_rule$JID ==jid, c(3,4)]))
  }
  final <- data.frame(patient_df)
  print("[END] > Bdays for loop; jid in patient_df$JID\n")
  print("[START] > codeBook for loop; i in 1:norw(codeBook)\n")
  name_unique <- unique(codeBook$name)
  
  for (i in name_unique) {
    code_info <- data.frame("code_val" = codeBook[codeBook$name == i, "code"],
                            "uniqueCode" = codeBook[codeBook$name == i,"uniqueCode"],
                            "code_len" = nchar(codeBook[codeBook$name == i, "code"]),
                            "reg" = paste0("^", codeBook[codeBook$name == i, "code"]))
    
    #,"exclusive.or" = codeBook[codeBook$name == i, "exclusive.or"])
    #ex.or.code <- code_info[code_info$exclusive.or == 1, ]
    name <- gsub("[-, /]", ".", i)
    if (nrow(code_covid_df) == nrow(codeBook)) {
      name <- code_info$code_val
    }
    final[,name] <- 0
    search.reg <- paste(code_info$reg, collapse ="|")
    
    verbose <- (nrow(code_info)%%verbose.period<=10) 
    cat("\n - * - * - * - * - * - * - \n")
    print(i)
    if (verbose) cat("\n - * - * - * - * - * - * - \n")
    if (verbose) print(code_info)
    
    if (!is.null(study)) {
      r.v <- unique(codeBook[codeBook$name == i, study])
      if (verbose) cat(sprintf(" - %s in %s.\n\n", r.v, study))
      
      # "x", "x.exposure.only" 없애는 코드
      #if (r.v %in% c("outcome.related", "x", "x.exposure.only")) {
      #  next
      #}
      
    } else {
      r.v <- NULL
    }
    
    if (sum(codeBook[codeBook$name == i,"codeType"] == "ATC")) {
      rule <- grep(search.reg, ATC_mapping_df$ATC, ignore.case = TRUE)
      # if exclusive or 에 대한 rule2 만들기
      
      #--------- (if -2-) "ATC" mapping X ---------#
      if (!length(rule)){  
        cat(" ATC code %s : mapping X\n")
        print(code_info$code)
        next
      }
    }
    
    sF_split1 <- strsplit(codeBook[codeBook$name == i,"searchFor"], "; ")[[1]]
    if (verbose) print("[START] >> sF_split1 while loop;\n")
    j = 1; dummy <- NULL
    while(!is.na(sF_split1[[1]][j])){
      sF_split2 <- strsplit(sF_split1[[1]][j], "/")
      
      df_name <- sF_split2[[1]][1]
      var_name <- sF_split2[[1]][2]
      df_all <- data.frame(get(df_name))
      
      #--------- df_rule 이 존재 : Bdays 설정 ---------#
      if (is.null(r.v)){
        df <- df_all
        # outcome이면 t200에서 특정 시점을 기준으로 자를 필요 없이, 다 가져와야 함.
      } else {
        if (r.v %in% names(df_rule)){ # names(df_rule) : JID, MID, EXPOSURE.itself, exposure.and.outcome.related
          df <- df_all[df_all$MID %in% df_rule[df_rule[,r.v],"MID"], ]
        }
        else{
          df <- df_all
        }
      }
      #------------------------------------------------#
      
      if (verbose) cat(sprintf("[%d] before : %d, after : %d\n",j, nrow(df_all), nrow(df)))
      df_all <- NULL
      #--------- (if -1-) codeBook$codeType == "ATC" ---------#
      
      if (codeBook[codeBook$name == i,"codeType"] == "ATC") {
        mapped_var <- toupper(ATC_mapping_df[rule, var_name])
        searchJID <- df[df[,var_name] %in% mapped_var,"JID"]
      } else { #--------- (else -1-) codeBook$codeType != "ATC" ---------#
        searchJID <- df[grep(search.reg, df[,var_name], ignore.case = TRUE),]$JID
      }
      j <- j+1
      if (!length(searchJID)) {
        next()
      }
      final[final$JID %in% searchJID, name] <- 1
      
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
  dir.create("./resultsCNLLS/frequency", showWarnings=FALSE)
  
  cat(" <----------------------------> \n")
  df <- get(sprintf("%s",df_name))
  cat(sprintf("  (Dim) # of JID = %d, code = %d \n", dim(df)[1], dim(df)[2]))
  
  code_freq <- colSums(df[,-excl.cols])
  code_freq_per <- round(code_freq/nrow(df[,-excl.cols]),6)

  
  cat(sprintf(" (1) [save] %s frequency \n", df_name))
  write.csv(code_freq, sprintf("./resultsCNLLS/frequency/%s_frequency.csv", df_name), row.names=T)
  
  cat(sprintf(" (2) [save] %s frequency (per) \n", df_name))
  write.csv(code_freq_per, sprintf("./resultsCNLLS/frequency/%s_frequency_per.csv", df_name), row.names=T)
  cat("\n")
}

primary.CI <- function(df1, study){
  print(study[,c(2,1)])
  study.exposure <- study[study$type == "Exposure","var"]
  study.confounder <- study[study$type == "Confounder","var"]
  study.Effect.Modifier <- study[study$type == "Effect.Modifier","var"]
  
  # [0-1] Naive model  outcome ~ exposure : wt (X)
  fit0.1 <- glm(df1[,"outcome1"]~df1[,study.exposure], family="binomial")
  pred0.1 <- predict(fit0.1, type="response")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [0-1] Naive Outcome Model \n\n")
  print(summary(fit0.1))
  
  # [0-2] Naive model  outcome ~ exposure + confounder : wt (X)
  fit0.2 <- glm(df1[,"outcome1"]~., data=df1[,c(study.exposure, study.confounder)], family="binomial")
  pred0.2 <- predict(fit0.2, type="response")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [0-2] Naive Multivariate Outcome Model \n\n")
  print(summary(fit0.2))
  
  # [1] propensity score
  #   (a) treatment ~ confounders
  fit1a<- glm(df1[,study.exposure] ~ ., data = df1[,study.confounder], family="binomial")
  df1$ps1a <- predict(fit1a, type="response")
  df1$wt1a <- df1$ps1a*df1[,study.exposure] + (1-df1$ps1a)*(1-df1[,study.exposure])
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [1] Propensity Score Model (a)\n\n")
  cat(" - propensity score dist\n\n")
  print(summary(df1$ps1a))
  cat("\n\n")
  cat(" - weight dist\n\n")
  print(summary(df1$wt1a))
  cat("\n\n")
  cat(" - fit1a model summary \n\n")
  print(summary(fit1a))
  cat("\n\n")
  
  # [2] outcome model (main)
  #   (a) outcome ~ exposure - ps 1a
  fit2a <- glm(df1[,"outcome1"]~df1[,study.exposure], weight = 1/df1$wt1a, family="binomial")
  pred2a <- predict(fit2a, type="response")
  
  cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
  cat(" [2] Outcome Model (a)\n\n")
  print(summary(fit2a))
  cat("\n\n")
  
  # [3] outcome ~ exposure + effect modifier 
  for (v in study.Effect.Modifier){
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(sprintf(" [3] Effect Modifier Model : %s \n\n", v))
    df1[,paste0(study.exposure,"*",v)] <- df1[,study.exposure] * df1[,v]
    fit3_v <- glm(df1[,"outcome1"]~., data=df1[,c(study.exposure, v, paste0(study.exposure,"*",v))], weight = 1/df1$wt1a, family="binomial")
    pred3_v <-  predict(fit3_v, type="response")
    
    print(summary(fit3_v))
    cat("\n\n")
  }

}

secondary.CI <- function(df1, study){
  study.exposure <- study[study$type == "Exposure","var"]
  study.confounder <- study[study$type == "Confounder","var"]
  study.Effect.Modifier <- study[study$type == "Effect.Modifier","var"]
  study.second <- study[study$type == "Outcome2","var"]
  
  print(study[,c(2,1)])
  
  m <- 1
  for (outcome2 in study.second) {
    study.scr <- c("AGE", "GENDER", "MedInfo")
    cat("============================================================\n")
    cat("============================================================\n")
    cat(sprintf("           %d. ", m));cat(outcome2);cat("\n\n")
    
    m <- m+1
    for (confounder in study.confounder[4:length(study.confounder)]){
      fit.scr <- glm(df1[,outcome2]~df1[,confounder], family = "binomial")
      if ( length(summary(fit.scr)$coef) == 4) {
        print("ERROR!!, exposure coefficeints is NA.")
        next
      }
      if (summary(fit.scr)$coef[2,4] < 0.2){
        study.scr <- c(study.scr, confounder)
      }
    }
    cat("\n [Confounder after p-value Screening(0.2)] \n")
    print(study[study$var %in% study.scr,c(2,1)])
    
    # [0-1] Naive model  outcome ~ exposure : wt (X)
    fit0.1 <- glm(df1[,"outcome1"]~df1[,study.exposure], family="binomial")
    pred0.1 <- predict(fit0.1, type="response")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [0-1] Naive Outcome Model \n\n")
    print(summary(fit0.1))
    
    # [0-2] Naive model  outcome ~ exposure + confounder : wt (X)
    fit0.2 <- glm(df1[,"outcome1"]~., data=df1[,c(study.exposure, study.scr)], family="binomial")
    pred0.2 <- predict(fit0.2, type="response")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [0-2] Naive Multivariate Outcome Model \n\n")
    print(summary(fit0.2))
    
    # [1] ps model for screening variable
    # (a) treatment ~ confounders(screening)
    fit1a<- glm(df1[,study.exposure] ~ ., data = df1[,study.scr], family="binomial")
    df1$ps1a <- predict(fit1a, type="response")
    df1$wt1a <- df1$ps1a*df1[,study.exposure] + (1-df1$ps1a)*(1-df1[,study.exposure])
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [1] Propensity Score Model (a)\n\n")
    cat(" - propensity score dist\n\n")
    print(summary(df1$ps1a))
    cat("\n\n")
    cat(" - weight dist\n\n")
    print(summary(df1$wt1a))
    cat("\n\n")
    cat(" - model summary \n\n")
    print(summary(fit1a))
    cat("\n\n")
    
    #   (b) treatment ~ effect modifier union confounders (think. 1a = 1b) 
    fit1b <- glm(df1[,study.exposure] ~ ., data = df1[,unique(c(study.Effect.Modifier, study.scr))], family="binomial")
    df1$ps1b <- predict(fit1b, type="response")
    df1$wt1b <- df1$ps1b*df1[,study.exposure] + (1-df1$ps1b)*(1-df1[,study.exposure])
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - * - *\n")
    cat(" [1] Propensity Score Model (b)\n\n")
    print(summary(df1$ps1b))
    print(summary(df1$wt1b))
    print(summary(fit1b))
    
    # [2] outcome model (main)
    #   (a) outcome ~ exposure - ps 1a
    fit2a <- glm(df1[,"outcome1"]~df1[,study.exposure], weight = 1/df1$wt1a, family="binomial")
    pred2a <- predict(fit2a, type="response")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
    cat(" [2-a] Outcome Model\n\n")
    print(summary(fit2a))
    cat("\n\n")
    
    #   (b) outcome ~ exposure - ps 1b
    fit2b <- glm(df1[,"outcome1"]~df1[,study.exposure], weight = 1/df1$wt1b, family="binomial")
    pred2b <- predict(fit2b, type="response")
    
    cat("\n - * - * - * - * - * - * - * - * - * - * - * - *\n")
    cat(" [2-b] Outcome Model\n\n")
    print(summary(fit2b))
    
    # [3] outcome ~ exposure + effect modifier 
    for ( v in study.Effect.Modifier){
      cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
      cat(sprintf(" [3-a] Effect Modifier Model : %s \n\n", v))
      df1[,paste0(study.exposure,"*",v)] <- df1[,study.exposure] * df1[,v]
      fit3_v <- glm(df1[,"outcome1"]~., data=df1[,c(study.exposure, v, paste0(study.exposure,"*",v))], weight = 1/df1$wt1a, family="binomial")
      pred3_v <- predict(fit3_v, type="response")
      print(summary(fit3_v))
      cat("\n\n")
    }
    
    for ( v in study.Effect.Modifier){
      cat("\n - * - * - * - * - * - * - * - * - * - * - \n")
      cat(sprintf(" [3-b] Effect Modifier Model : %s \n\n", v))
      df1[,paste0(study.exposure,"*",v)] <- df1[,study.exposure] * df1[,v]
      fit3_v <- glm(df1[,"outcome1"]~., data=df1[,c(study.exposure, v, paste0(study.exposure,"*",v))], weight = 1/df1$wt1a, family="binomial")
      pred3_v <- predict(fit3_v, type="response")
      print(summary(fit3_v))
      cat("\n\n")
    }
  }
}

for_experiment <- function(df, n=100) {
  random <- data.frame("JID" = 100:(100+n-1), ifelse(matrix(runif(n*(ncol(df)-1)), nrow=n, ncol=ncol(df)-1)>0.8,1,0))
  colnames(random) <- colnames(df)
  return (rbind(df, random))
}

####################################################################
#####################      patient info      #######################
####################################################################

# co19_t200_trans_dn
patient_dt <- as.data.table(co19_t200_trans_dn)[, .(first_covid = min(RECU_FR_DD)), by = .(JID, INSUP_TP_CD, SEX_TP_CD, PAT_BTH)]
patient_info <- as.data.frame(patient_dt)

# (def) AGE 
patient_info$AGE = as.numeric(substr(patient_info$first_covid,1,4)) - as.numeric(substr(patient_info$PAT_BTH,1,4))
patient_info$PAT_BTH <- NULL
patient_info2 <- patient_info

patient_info$AGE2 <- ifelse(patient_info$AGE<=64, 0, 1)
#patient_info <- rbind(data.frame("JID" = 99,
#                                 "INSUP_TP_CD" = 3,
#                                 "SEX_TP_CD" = 2,
#                                 "first_covid" = as.Date("2022-01-30"),
#                                 "AGE" = 20),
#                      patient_info)


# 열 삭제
# (def) SEX_TP_CD 1:male, 2:female, 9:etc
patient_info <- patient_info[!(patient_info$SEX_TP_CD == 9),]
patient_info$GENDER <- patient_info$SEX_TP_CD - 1

# (def) 건강보험 : medical insurance, 의료급여&보훈 : medical aid
patient_info <- patient_info[(patient_info$INSUP_TP_CD == 4 | patient_info$INSUP_TP_CD == 5 | patient_info$INSUP_TP_CD == 7),]
patient_info$MedInfo <- ifelse(patient_info$INSUP_TP_CD == 4, 1, 0)

patient_info$SEX_TP_CD <- NULL
patient_info$INSUP_TP_CD <- NULL

# patient_info2
patient_info2$INSUP_TP_CD <- as.factor(patient_info2$INSUP_TP_CD) 
patient_info2$SEX_TP_CD <- as.factor(patient_info2$SEX_TP_CD) 
    
agebreaks2 <- c(0,19,40,65, 500)
agelabels2 <- c("0-18", "19-39","40-64","65+")

setDT(patient_info2)[, AGE := cut(AGE,
                                  breaks = agebreaks2,
                                  right = FALSE, 
                                  labels = agelabels2)]

## print summary
sink("./resultsCNLLS/patient.txt")
# ---------------------------------- statistics of patient 
cat("\n =========== patient (COVID) summary =========== \n\n")
cat(" <-- # of JID --> \n")
cat(sprintf(" - raw data : %d\n - after drop : %d \n\n", nrow(patient_info2), nrow(patient_info)))

cat(" <-- raw data summary --> \n")
summary(patient_info2[,c("INSUP_TP_CD", "AGE", "SEX_TP_CD")])
cat("\n [INSUP_TP_CD] 4: 건강보험 5: 의료급여 7:보훈\n")
cat(" [SEX_TP_CD] 1: 남 2: 여 9: 기타\n\n")
sink()

####################################################################
#####################       data frame       #######################
####################################################################
age18_jid <- unique(patient_info[patient_info$AGE >= 18,]$JID)
age40_jid <- unique(patient_info[patient_info$AGE >= 40,]$JID)

patient_info <- as.data.frame(patient_info[patient_info$JID %in% age18_jid,]) # only covid-19 & adult

patient_info$AGE <- patient_info$AGE2
patient_info$AGE2 <- NULL
excl.cols = (1:ncol(patient_info))

for (jid in patient_info$JID) {
  df_temp <- co19_twjhe200_trans_dn[which(co19_twjhe200_trans_dn$JID == jid),]
  
  cat(sprintf(" [V] nrow(df_temp) = %d \n", nrow(df_temp)))
  if(!nrow(df_temp)) next
  
  first <- patient_info[patient_info$JID == jid,]$first_covid
  exposure_rule <- (df_temp$RECU_FR_DD <= first) & (df_temp$RECU_FR_DD >= first - Bdays)
  confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays)
  
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

# [1] outcomes (sheet4)
# outcomes code book 에 중복된 셀 존재
covid_outcomes <- gen_dummies(patient_info, code_outcomes_df)

# [2] COVID19 code (sheet5)
covid_diag <- gen_dummies(patient_info, code_covid_df)

# [3] comorbidties ICD10 (sheet2)
como_info_statin30 <- gen_dummies(code_ICD10_df, "statin.study",30)
como_info_anti30 <- gen_dummies(patient_info, code_ICD10_df, "antiplatelet.anticoagulant.study",30)
como_info_immune30 <- gen_dummies(patient_info, code_ICD10_df, "immune.suppressant.study", 30)

como_info_statin120 <- gen_dummies(patient_info[patient_info$JID %in%age40_jid,], code_ICD10_df, "statin.study",120)
como_info_anti120 <- gen_dummies(patient_info, code_ICD10_df, "antiplatelet.anticoagulant.study",120)
como_info_immune120 <- gen_dummies(patient_info, code_ICD10_df, "immune.suppressant.study", 120)

# [4] medications ATC code (sheet3)
# [code book(code_ATC)의 code column] = [ATC_mapping_df의 ATC코드 column] 
# [ATC_mapping_df의 주성분코드] = [T530의 GNL_CD]
# [ATC_mapping_df의 제품코드]  = T530의 DIV_CD  (T300의 DIV_CD도 일부 존재)
ATC_med_statin30 <- gen_dummies(patient_info[patient_info$JID %in% age40_jid,], code_ATC_df, "statin.study",30)
ATC_med_anti30 <- gen_dummies(patient_info, code_ATC_df, "antiplatelet.anticoagulant.study",30)
ATC_med_immune30 <- gen_dummies(patient_info, code_ATC_df, "immune.suppressant.study", 30)

ATC_med_statin120 <- gen_dummies(patient_info[patient_info$JID %in% age40_jid,], code_ATC_df, "statin.study",120)
ATC_med_anti120 <- gen_dummies(patient_info, code_ATC_df, "antiplatelet.anticoagulant.study",120)
ATC_med_immune120 <- gen_dummies(patient_info, code_ATC_df, "immune.suppressant.study", 120)

dir.create("./resultsCNLLS/Rdata", showWarnings=FALSE)
save(covid_outcomes,
     covid_diag,
     como_info_statin30, como_info_statin120, como_info_anti30, como_info_anti120, como_info_immune30, como_info_immune120,
     ATC_med_statin30, ATC_med_statin120, ATC_med_anti30, ATC_med_anti120, ATC_med_immune30, ATC_med_immune120,
     file = "./resultsCNLLS/Rdata/data_eachCodeBook.Rdata")

Out.Diag <- cbind(covid_outcomes, covid_diag[,-excl.cols])

statin30.exp <- cbind(como_info_statin30, ATC_med_statin30[,-excl.cols])
statin120.exp <- cbind(como_info_statin120, ATC_med_statin120[,-excl.cols])
statin30.exp$Lipid.lowering.agents.excluding.statin <- statin30.exp$Lipid.lowering.agents.including.statin - statin30.exp$Lipid.lowering.agents.including.statin * statin30.exp$statin
statin120.exp$Lipid.lowering.agents.excluding.statin <- statin120.exp$Lipid.lowering.agents.including.statin - statin120.exp$Lipid.lowering.agents.including.statin * statin120.exp$statin

anti30.exp <- cbind(como_info_anti30, ATC_med_anti30[,-excl.cols])
anti120.exp <- cbind(como_info_anti120, ATC_med_anti120[,-excl.cols])

immune30.exp <- cbind(como_info_immune30, ATC_med_immune30[,-excl.cols])
immune120.exp <- cbind(como_info_immune120, ATC_med_immune120[,-excl.cols])


# print(" <-------------- statin --------------> \n")
statin30 <- cbind(Out.Diag[Out.Diag$JID %in% age40_jid,], statin30.exp[,-excl.cols])
statin120 <- cbind(Out.Diag[Out.Diag$JID %in% age40_jid,], statin120.exp[,-excl.cols])

statin30$first_covid <- NULL
statin120$first_covid <- NULL

print(" <-------------- anti --------------> \n")
anti30 <- cbind(Out.Diag, anti30.exp[,-excl.cols])
anti120 <- cbind(Out.Diag, anti120.exp[,-excl.cols])
anti30$first_covid <- NULL
anti120$first_covid <- NULL
anti30$statin <- NULL
anti120$statin <- NULL

print(" <-------------- immune --------------> \n")
immune30 <- cbind(Out.Diag, immune30.exp[,-excl.cols])
immune120 <- cbind(Out.Diag, immune120.exp[,-excl.cols])
immune30$first_covid <- NULL
immune120$first_covid <- NULL
immune30$statin <- NULL
immune120$statin <- NULL

save(statin30, statin120,
     anti30, anti120,
     immune30, immune120,
     file = "./resultsCNLLS/Rdata/data_eachStudy.Rdata")

save_freq("statin30")
save_freq("statin120")

save_freq("anti30")
save_freq("anti120")

save_freq("immune30")
save_freq("immune120")

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

statin.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", statin.Ex.Out.Rel), gsub("[-,/ ]", ".", statin.Out.Rel))
statin.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA")
statin.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA", "Y")

statin30$outcome1 <- ifelse(rowSums(statin30[,statin.outcome1])>0,1,0)
statin120$outcome1 <-  ifelse(rowSums(statin120[,statin.outcome1])>0,1,0)

#  “TIA”, “Stroke”, “Coronary artery disease ”, “Atherosclerosis”, “Peripheral vascular disease”
statin30$TIA.Stroke.Coronary.Atherosclerosis.Peripheral <- 
  ifelse(statin30$TIA + statin30$Stroke + statin30$Coronary.artery.disease + statin30$Atherosclerosis + statin30$Peripheral.vascular.disease>0,1,0)

statin120$TIA.Stroke.Coronary.Atherosclerosis.Peripheral <- 
  ifelse(statin120$TIA + statin120$Stroke + statin120$Coronary.artery.disease + statin120$Atherosclerosis + statin120$Peripheral.vascular.disease>0,1,0)

statin.Emodifier <- c("AGE", "GENDER", "Hyperlipidemia", "Hypertension", "Diabetes.mellitus", "TIA.Stroke.Coronary.Atherosclerosis.Peripheral")

#statin.exposure <- "Lipid.lowering.agents.excluding.statin"
statin.exposure <- "statin"
statin.confounder = c(statin.confounder, "Lipid.lowering.agents.excluding.statin")

statin.study <- rbind(data.frame(var = statin.exposure, type = "Exposure"),
                      data.frame(var = statin.confounder, type = rep("Confounder", length(statin.confounder))),
                      data.frame(var = statin.outcome1, type = rep("Outcome1", length(statin.outcome1))),
                      data.frame(var = statin.outcome2, type = rep("Outcome2", length(statin.outcome2))), 
                      data.frame(var = statin.Emodifier, type = rep("Effect.Modifier",length(statin.Emodifier))))

# anti variable
anti.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$antiplatelet.anticoagulant.study == "exposure.and.outcome.related",
                                          c("name", "antiplatelet.anticoagulant.study")])$name,
                     unique(code_ATC_df[code_ATC_df$antiplatelet.anticoagulant.study == "exposure.and.outcome.related",
                                        c("name", "antiplatelet.anticoagulant.study")])$name)

anti.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$antiplatelet.anticoagulant.study == "outcome.related",
                                       c("name", "antiplatelet.anticoagulant.study")])$name,
                  unique(code_ATC_df[code_ATC_df$antiplatelet.anticoagulant.study == "outcome.related",
                                     c("name", "antiplatelet.anticoagulant.study")])$name)

anti.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", anti.Ex.Out.Rel), gsub("[-,/ ]", ".", anti.Out.Rel))[-54]

anti.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.infarction.ischemic.stroke.and.TIA", "Thromboembolism", 
                   "ischemic.stroke.and.TIA", "TIA.transient.ischemic.attack.", "Stroke", "hemorragic.stroke")

# - outcome1 - 
# "myocardial infarction,  ischemic stroke and TIA", "Thromboembolism"
# "bleeding", "transfusion"
anti30$myocardial.storke.TIA.Thromboembolism <- ifelse(anti30$myocardial.infarction.ischemic.stroke.and.TIA + anti30$Thromboembolism>0,1,0)
anti30$bleeding.transfusion <- ifelse(anti30$bleeding + anti30$transfusion>0,1,0)

anti120$myocardial.storke.TIA.Thromboembolism <- ifelse(anti120$myocardial.infarction.ischemic.stroke.and.TIA + anti120$Thromboembolism>0,1,0)
anti120$bleeding.transfusion <- ifelse(anti120$bleeding + anti120$transfusion>0,1,0)

anti.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission", "myocardial.storke.TIA.Thromboembolism", "bleeding.transfusion")

anti30$outcome1 <- ifelse(rowSums(anti30[,anti.outcome1])>0,1,0)
anti120$outcome1 <- ifelse(rowSums(anti120[,anti.outcome1])>0,1,0)

#  “TIA”, “Stroke”, “Coronary artery disease ”, “Peripheral vascular disease”
#  “Atrial Fibrillation”, “Thromboembolism” 
anti30$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(anti30$TIA + anti30$Stroke + anti30$Coronary.artery.disease + anti30$Peripheral.vascular.disease>0,1,0)
anti30$Atrial.Thromboembolism <- ifelse(anti30$Atrial.fibrillation + anti30$Thromboembolism>0,1,0)

anti120$TIA.Stroke.Coronary.Peripheral <- 
  ifelse(anti120$TIA + anti120$Stroke + anti120$Coronary.artery.disease + anti120$Peripheral.vascular.disease>0,1,0)
anti120$Atrial.Thromboembolism <- ifelse(anti120$Atrial.fibrillation + anti120$Thromboembolism>0,1,0)

anti.Emodifier <- c("AGE", "GENDER", "Atrial.Thromboembolism", "TIA.Stroke.Coronary.Peripheral")
anti.exposure <- "Anticoagulants.antiplatelets"

anti.study <- rbind(data.frame(var = anti.exposure, type = "Exposure"),
                    data.frame(var = anti.confounder, type = rep("Confounder", length(anti.confounder))),
                    data.frame(var = anti.outcome1, type = rep("Outcome1", length(anti.outcome1))),
                    data.frame(var = anti.outcome2, type = rep("Outcome2", length(anti.outcome2))), 
                    data.frame(var = anti.Emodifier, type = rep("Effect.Modifier",length(anti.Emodifier))))

# immune variable
immune.Ex.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$immune.suppressant.study == "exposure.and.outcome.related",
                                            c("name", "immune.suppressant.study")])$name,
                       unique(code_ATC_df[code_ATC_df$immune.suppressant.study == "exposure.and.outcome.related",
                                          c("name", "immune.suppressant.study")])$name)
immune.Out.Rel <- c(unique(code_ICD10_df[code_ICD10_df$immune.suppressant.study == "outcome.related",
                                         c("name", "immune.suppressant.study")])$name,
                    unique(code_ATC_df[code_ATC_df$immune.suppressant.study == "outcome.related",
                                       c("name", "immune.suppressant.study")])$name)

immune.confounder <- c("AGE", "GENDER", "MedInfo", gsub("[-,/ ]", ".", immune.Ex.Out.Rel), gsub("[-,/ ]", ".", immune.Out.Rel))[-54]

immune.outcome1 <- c("Death", "Use.of.mechanical.ventilation", "ICU.admission")
immune.outcome2 <-  c("Death", "Use.of.mechanical.ventilation", "ICU.admission")
immune30$outcome1 <- ifelse(rowSums(immune30[,immune.outcome1])>0,1,0)
immune120$outcome1 <- ifelse(rowSums(immune120[,immune.outcome1])>0,1,0)

immune.Emodifier <- c("AGE", "GENDER", "Malignancy", "autoimmune.disease")

immune.exposure <- "immunosuppressant"

immune.study <- rbind(data.frame(var = immune.exposure, type = "Exposure"),
                      data.frame(var = immune.confounder, type = rep("Confounder", length(immune.confounder))),
                      data.frame(var = immune.outcome1, type = rep("Outcome1", length(immune.outcome1))),
                      data.frame(var = immune.outcome2, type = rep("Outcome2", length(immune.outcome2))), 
                      data.frame(var = immune.Emodifier, type = rep("Effect.Modifier",length(immune.Emodifier))))

#statin30_2 <- for_experiment(statin30, n=1000)
#statin120_2 <- for_experiment(statin120, n=1000)

#anti30_2 <- for_experiment(anti30, n=1000)
#anti120_2 <- for_experiment(anti120, n=1000)

#immune30_2 <- for_experiment(immune30, n=1000)
#immune120_2 <- for_experiment(immune120, n=1000)

####################################################################
#####################          print         #######################
#################################################################### 
# statin 30
sink("./resultsCNLLS/results_statin30.txt")
cat("\n <--------- [statin 30] Result (primary) ---------> \n")
primary.CI(df1 = statin30, study=statin.study[order(statin.study$type, statin.study$var),])
sink()

sink("./resultsCNLLS/results2_statin30.txt")
cat("\n <--------- [statin 30] Result (second) ---------> \n")
secondary.CI(df1 = statin30, study=statin.study[order(statin.study$type, statin.study$var),])
sink()

# statin 120
sink("./resultsCNLLS/results_statin120.txt")
cat("\n <--------- [statin 120] Result (primary) ---------> \n")
primary.CI(df1 = statin120, study=statin.study[order(statin.study$type, statin.study$var),])
sink()

sink("./resultsCNLLS/results2_statin120.txt")
cat("\n <--------- [statin 120] Result (second) ---------> \n")
secondary.CI(df1 = statin120, study=statin.study[order(statin.study$type, statin.study$var),])
sink()

# anti 30
sink("./resultsCNLLS/results_anti30.txt")
cat("\n <--------- [anti 30] Result (primary) ---------> \n")
primary.CI(df1 = anti30, study=anti.study[order(anti.study$type, anti.study$var),])
sink()

sink("./resultsCNLLS/results2_anti30.txt")
cat("\n <--------- [anti 30] Result (second) ---------> \n")
secondary.CI(df1 = anti30, study=anti.study[order(anti.study$type, anti.study$var),])
sink()

# anti 120
sink("./resultsCNLLS/results_anti120.txt")
cat("\n <--------- [anti 120] Result (primary) ---------> \n")
primary.CI(df1 = anti120, study=anti.study[order(anti.study$type, anti.study$var),])
sink()

sink("./resultsCNLLS/results2_anti120.txt")
cat("\n <--------- [anti 120] Result (second) ---------> \n")
secondary.CI(df1 = anti120, study=anti.study[order(anti.study$type, anti.study$var),])
sink()

# immune 30
sink("./resultsCNLLS/results_immune30.txt")
cat("\n <--------- [immune 30] Result (primary) ---------> \n")
primary.CI(df1 = immune30, study=immune.study[order(immune.study$type, immune.study$var),])
sink()

sink("./resultsCNLLS/results2_immune30.txt")
cat("\n <--------- [immune 30] Result (second) ---------> \n")
secondary.CI(df1 = immune30, study=immune.study[order(immune.study$type, immune.study$var),])
sink()

# immune 120
sink("./resultsCNLLS/results_immune120.txt")
cat("\n <--------- [immune 120] Result (primary) ---------> \n")
primary.CI(df1 = immune120, study=immune.study[order(immune.study$type, immune.study$var),])
sink()

sink("./resultsCNLLS/results2_immune120.txt")
cat("\n <--------- [immune 120] Result (second) ---------> \n")
secondary.CI(df1 = immune120, study=immune.study[order(immune.study$type, immune.study$var),])
sink()

