#########################################################
########## IMPORT RAW DATA AND EXTERNAL DATA ############
#########################################################
# external data:
# 1. code-books-200605.xlsx
# 2. ATCcode_mapping.xlsx

library(readxl)
library(dplyr)

options(warn=-1)

# raw data (PLEASE REPLACE sample_schema.xlsx WITH THE RAW DATA)
data_path <- "data/sample_schema.xlsx"
co19_t200 = read_excel(data_path, sheet=4)
co19_t300 = read_excel(data_path, sheet=5)
co19_t400 = read_excel(data_path, sheet=6)
co19_t530 = read_excel(data_path, sheet=7)

# Medical use history data
co19_twjhe200 = read_excel(data_path, sheet=8)
co19_twjhe300 = read_excel(data_path, sheet=9)
co19_twjhe400 = read_excel(data_path, sheet=10)
co19_twjhe530 = read_excel(data_path, sheet=11)


# MID, JID 추가됨
# t200: MAIN_SICK, SUB_SICK
# t300: DIV_CD, GNL_CD
# t400: SICK_CD
# t530: DIV_CD, GNL_CD
co19_t200_trans_dn <- co19_t200[,c("JID", "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK", "DEATH", "CONFIRM")]
co19_t200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_t200$RECU_FR_DD), "%Y%m%d")
co19_t300_trans_dn <- co19_t300[,c("JID", "MID", "DIV_CD")]
co19_t400_trans_dn <- co19_t400[,c("JID", "MID", "SICK_CD")]
co19_t530_trans_dn <- co19_t530[,c("JID", "MID", "DIV_CD", "GNL_CD")]

co19_twjhe200_trans_dn <- co19_twjhe200[,c("JID", "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK", "RECU_FR_DD")]
co19_twjhe200_trans_dn$RECU_FR_DD <- as.Date(as.character(co19_twjhe200$RECU_FR_DD), "%Y%m%d")
co19_twjhe300_trans_dn <- co19_twjhe300[,c("JID", "MID", "DIV_CD")]
co19_twjhe400_trans_dn <- co19_twjhe400[,c("JID", "MID", "SICK_CD")]
co19_twjhe530_trans_dn <- co19_twjhe530[,c("JID", "MID", "DIV_CD", "GNL_CD")]

# External files (code_book)
codebook_path <- "codebook"
codebook <- "code-books-200611.xlsx"

code_ICD10 <- read_excel(file.path(codebook_path, codebook), sheet = 2)
code_ATC <- read_excel(file.path(codebook_path, codebook), sheet = 3)
code_outcomes <- read_excel(file.path(codebook_path, codebook), sheet = 4)
code_covid <- read_excel(file.path(codebook_path, codebook), sheet = 5)
ATC_mapping <- read_excel(file.path(codebook_path, "ATCcode_mapping.xlsx"))

#######################################################
########## THE END OF IMPORT EXTERNAL DATA ############
#######################################################
code_ICD10_df <- data.frame(code_ICD10[,c("uniqueID", "code", "codeType", "searchFor", "statin study", "antiplatelet/anticoagulant study", "immune suppressant study")])
names(code_ICD10_df)
code_ATC_df <- data.frame(code_ATC[,c("uniqueID", "code", "codeType", "searchFor", "statin study", "antiplatelet/anticoagulant study", "immune suppressant study")])
names(code_ATC_df)
code_outcomes_df <- data.frame(code_outcomes[,c("uniqueID", "code", "codeType", "searchFor")])
code_covid_df <- data.frame(code_covid[,c("uniqueID", "code", "codeType", "searchFor")])
#colnames(code_covid_df)

ATC_mapping_df <- data.frame(ATC_mapping)[,c("주성분코드", "제품코드", "ATC코드")]
colnames(ATC_mapping_df) <- c("GNL_CD", "DIV_CD", "ATC")

# return each JID's the first Diagnosis of covid19
# co19_t200_trans_dn
first <- co19_t200_trans_dn
FirstCovid19 <- first %>% group_by(JID) %>% summarize(first_covid = min(RECU_FR_DD)) %>% data.frame()

i=1
codeBook <- code_ATC_df
j=1
Bdays=30
gen_dummies <- function(codeBook, study = NULL, Bdays = 0){
  if (Bdays!=0) {
    df_rule <- NULL
    for (jid in FirstCovid19$JID) {
      df_temp <- co19_twjhe200_trans_dn[which(co19_twjhe200_trans_dn$JID %in% jid),]
      
      exposure_rule <- (df_temp$RECU_FR_DD < FirstCovid19[FirstCovid19$JID == jid, "first_covid"]) & (df_temp$RECU_FR_DD > FirstCovid19[FirstCovid19$JID == jid, "first_covid"] - Bdays)
      confounder_rule <-  (df_temp$RECU_FR_DD < FirstCovid19[FirstCovid19$JID == jid, "first_covid"] - Bdays)
      
      df_rule_temp <- data.frame("JID" = jid,
                                 "MID" = df_temp$MID,
                                 "EXPOSURE.itself" = ifelse(exposure_rule, 1, 0),
                                 "exposure.and.outcome.related" = ifelse(confounder_rule, 1, 0))
      df_rule <- rbind(df_rule, df_rule_temp)
      # print(colSums(df_rule[,c(3,4)]))
    }
  }
  final <- NULL  
  
  for (i in 1:nrow(codeBook)) {
    code_val <- codeBook[i,"code"]
    code_len <- nchar(codeBook[i,"code"])
    
    if (is.null(study) != 1) {
      r.v <- codeBook[i, study]
      # print(r.v)
    }
    
    sF_split1 <- strsplit(codeBook[i,"searchFor"], "; ")
    cat(sprintf("Unique ID : %d. [%s]", codeBook[i,"uniqueID"], code_val))
    print(Sys.time())
    
    j = 1; dummy <- NULL
    while(!is.na(sF_split1[[1]][j])){
      sF_split2 <- strsplit(sF_split1[[1]][j], "/")
      
      df_name <- sF_split2[[1]][1]
      var_name <- sF_split2[[1]][2]
      df_all <- data.frame(get(df_name)) # df 초기화, cbind() 불가
      
      
      if (is.null(df_rule)){
        df <- df_all
      } else {
        if (r.v %in% names(df_rule)){
          df <- df_all[which(df_all$MID %in% df_rule[df_rule[,r.v]==1,"MID"]), ]
          print(nrow(df_all))
          
        }
        else{
          df <- df_all
        }
      }
      #names(df_all)
      #names(df)
      print(nrow(df))
      
      if (codeBook[i,"codeType"] == "ATC") {
        rule = (toupper(substr(ATC_mapping_df[ ,"ATC"],1,code_len)) == toupper(code_val))
        #print(sum(rule))
        
        if (sum(rule)==0){
          cat(" ATC code", i,". [", code_val,"] mapping X ")
          print(" ")
          j = j+1 #while 문 사용해서 +1
          next
        } else {
          mapped_var <- ATC_mapping_df[rule, var_name]
          dummy2 <- NULL
          #each_var = mapped_var
          for (each_var in mapped_var) {
            dummy2_temp <- data.frame("JID" = df$JID, "var" = ifelse(df[,var_name] == each_var, 1, 0))
            dummy2_temp[is.na(dummy2_temp[,"var"]),"var"] <- 0
            dummy2_temp2 <- dummy2_temp %>% group_by(JID) %>% summarise(var = sum(dummy2_temp[,"var"])) %>% data.frame()
            
            colnames(dummy2_temp2) <- c("JID", each_var)
            # dummy2_temp : each_var 에 대한 정보 저장
            if (is.null((dummy2))) {
              dummy2 <- rbind(dummy2, dummy2_temp2)
            } else { 
              dummy2 <- merge(dummy2, dummy2_temp2, by=c("JID"), all=TRUE)
            }
          }
          # dummy_temp2 : searchFor 하나에 대한 정보 저장 (dummy2 의 ifelse(rowSums>-1,1,0) )
          #print("dummy2 생성")
          dummy2$zero <- 0 # mapped_value 가 한 개일 경우 rowSums 이 작동하지 않기 때문에 생성
          dummy_temp2 <- data.frame("JID" = dummy2$JID, "var" = ifelse(rowSums(dummy2[,-1])>0,1,0))
        }
      } else {
        dummy_temp <- data.frame("JID" = df$JID, "var" = ifelse(toupper(substr(df[,var_name],1,code_len)) == toupper(code_val),1,0))
        dummy_temp[is.na(dummy_temp[,"var"]),"var"] <- 0
        dummy_temp2 <- dummy_temp %>% group_by(JID) %>% summarise(var = sum(dummy_temp[,"var"])) %>% data.frame()
      }
      if (j == 1) {
        # [?] 왜 rbind 만 되는지
        dummy <- rbind(dummy, dummy_temp2)
      } else { 
        dummy <- merge(dummy, dummy_temp2, by=c("JID"), all=TRUE)
        #print(nrow(dummy))
      }
      j = j+1
    }
    
    #head(final)
    #print(nrow(final))
    if(is.null(dummy)) next
    #print("final_temp 생성")
    dummy$zero <- 0
    final_temp <- data.frame("JID" = dummy$JID, "var" = ifelse(rowSums(dummy[,-1])>0,1,0))
    colnames(final_temp) <- c("JID", code_val)
    if (i==1) {
      final <- rbind(final, final_temp)
    } else { 
      final <- merge(final, final_temp, by=c("JID"), all=TRUE)
    }
  }
  return (final)
}

time = Sys.time()
# [1] comorbidties ICD10 (sheet2)
phx_info30_statin <- gen_dummies(code_ICD10_df, "statin.study",30)
phx_info30_anti <- gen_dummies(code_ICD10_df, "antiplatelet.anticoagulant.study",30)
phx_info30_immune <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 30)

colSums(phx_info30_statin[,-1])
colSums(phx_info30_anti[,-1])
colSums(phx_info30_immune[,-1])

phx_info120_statin <- gen_dummies(code_ICD10_df, "statin.study",120)
phx_info120_anti <- gen_dummies(code_ICD10_df, "antiplatelet.anticoagulant.study",120)
phx_info120_immune <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 120)

colSums(phx_info120_statin[,-1])
colSums(phx_info120_anti[,-1])
colSums(phx_info120_immune[,-1])

sum(phx_info30_statin != phx_info120_statin)
sum(phx_info30_anti != phx_info120_anti)
sum(phx_info30_immune != phx_info120_immune)

time1 = Sys.time()

# [2] outcomes (sheet4)
# [?] outcomes code book 에 중복된 셀 존재
covid_sick <- gen_dummies(code_outcomes_df)
time2 = Sys.time()

# [3] medications ATC code (sheet2)
# [code book(code_ATC)의 code column] = [ATC_mapping_df의 ATC코드 column] 
# [ATC_mapping_df의 주성분코드] = [T530의 GNL_CD]
# [ATC_mapping_df의 제품코드]  = T530의 DIV_CD  (T300의 DIV_CD도 일부 존재)
ATC_med30_statin <- gen_dummies(code_ATC_df, "statin.study",30)
ATC_med30_anti <- gen_dummies(code_ATC_df, "antiplatelet.anticoagulant.study",30)
ATC_med30_immune <- gen_dummies(code_ATC_df, "immune.suppressant.study", 30)

colSums(ATC_med30_statin[,-1])
colSums(ATC_med30_anti[,-1])
colSums(ATC_med30_immune[,-1])

ATC_med120_statin <- gen_dummies(code_ATC_df, "statin.study",120)
ATC_med120_anti <- gen_dummies(code_ATC_df, "antiplatelet.anticoagulant.study",120)
ATC_med120_immune <- gen_dummies(code_ATC_df, "immune.suppressant.study", 120)

colSums(phx_info120_statin[,-1])
colSums(phx_info120_anti[,-1])
colSums(phx_info120_immune[,-1])

time3 = Sys.time()

sum(ATC_med30_statin != ATC_med120_statin)
sum(ATC_med30_anti != ATC_med120_anti)
sum(ATC_med30_immune != ATC_med120_immune)

# [4] COVID19 code (sheet5)
covid_diag <- gen_dummies(code_covid_df)
time4 = Sys.time()