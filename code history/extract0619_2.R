#########################################################
########## IMPORT RAW DATA AND EXTERNAL DATA ############
#########################################################
# external data:
# 1. code-books-200605.xlsx
# 2. ATCcode_mapping.xlsx

library(readxl)
# library(dplyr)
library(data.table)
verbose.period = 2  # insert 2 for sample and 1000000 for the real
dir.create("./resultsCNLLS/", showWarnings=FALSE)
options(warn=-1)

# raw data (PLEASE REPLACE sample_schema.xlsx WITH THE RAW DATA)
data_path <- "./sample_schema.xlsx"
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
# t200: MAIN_SICK, SUB_SICK, INSUP_TP_CD, SEX_TP_CD, PAT_BTH
# t300: DIV_CD, GNL_CD
# t400: SICK_CD
# t530: DIV_CD, GNL_CD

co19_t200_trans_dn <- co19_t200[,c("JID", "MID", "INSUP_TP_CD", "SEX_TP_CD", "PAT_BTH" , "MAIN_SICK", "SUB_SICK", "DEATH", "CONFIRM")]
co19_t200_trans_dn$PAT_BTH <- as.Date(as.character(co19_t200$PAT_BTH), "%Y%m%d")
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
codebook_path <- "./"
codebook <- "code-books-200611.xlsx"

code_ICD10 <- read_excel(file.path(codebook_path, codebook), sheet = 2)
code_ATC <- read_excel(file.path(codebook_path, codebook), sheet = 3)
code_outcomes <- read_excel(file.path(codebook_path, codebook), sheet = 4)
code_covid <- read_excel(file.path(codebook_path, codebook), sheet = 5)
ATC_mapping <- read_excel(file.path(codebook_path, "ATCcode_mapping.xlsx"))

#######################################################
########## THE END OF IMPORT EXTERNAL DATA ############
#######################################################
code_ICD10_df <- data.frame(code_ICD10[,c("uniqueID", "name", "code", "codeType", "searchFor", "statin study", "antiplatelet/anticoagulant study", "immune suppressant study")])
names(code_ICD10_df)
code_ATC_df <- data.frame(code_ATC[,c("uniqueID", "name","code", "codeType", "searchFor", "statin study", "antiplatelet/anticoagulant study", "immune suppressant study")])
names(code_ATC_df)
code_outcomes_df <- data.frame(code_outcomes[,c("uniqueID",  "name", "code", "codeType", "searchFor")])
code_covid_df <- data.frame(code_covid[,c("uniqueID", "name", "code", "codeType", "searchFor")])
#colnames(code_covid_df)

ATC_mapping_df <- data.frame(ATC_mapping)[,c("주성분코드", "제품코드", "ATC코드")]
colnames(ATC_mapping_df) <- c("GNL_CD", "DIV_CD", "ATC")

# return each JID's the first Diagnosis of covid19
# co19_t200_trans_dn
first_dt <- as.data.table(co19_t200_trans_dn)
names(first_dt)
#mtcars_dt[, .(n=.N, mileage=mean(mpg) %>% round(2)), by=gear]
FirstCovid19 <- first_dt[, .(first_covid = min(RECU_FR_DD)), by = .(JID, INSUP_TP_CD, SEX_TP_CD, PAT_BTH)]
# (def) AGE 
FirstCovid19$AGE = floor( as.numeric(as.Date("2020-01-01") - FirstCovid19$PAT_BTH) / 365 )
FirstCovid19$PAT_BTH = NULL

# i=285
# codeBook <- code_ATC_df
# jid=23
# Bdays=30
gen_dummies <- function(codeBook, study = NULL, Bdays = 0){
  df_rule <- NULL
  if (Bdays!=0) {
    for (jid in FirstCovid19$JID) {
      df_temp <- co19_twjhe200_trans_dn[which(co19_twjhe200_trans_dn$JID == jid),]
      
      first <- FirstCovid19[FirstCovid19$JID == jid, "first_covid"]
      exposure_rule <- (df_temp$RECU_FR_DD <= first) & (df_temp$RECU_FR_DD >= first - Bdays)
      confounder_rule <-  (df_temp$RECU_FR_DD < first - Bdays)

      # rule 추가가능
      df_rule_temp <- data.frame("JID" = jid,
                                 "MID" = df_temp$MID,
                                 "EXPOSURE.itself" = ifelse(exposure_rule, 1, 0),
                                 "exposure.and.outcome.related" = ifelse(confounder_rule, 1, 0),
                                 "outcome.related" = ifelse(confounder_rule, 1, 0),
                                 "x" = ifelse(confounder_rule, 1, 0),
                                 "x.exposure.only" = ifelse(confounder_rule, 1, 0))
      df_rule <- rbind(df_rule, df_rule_temp)
      print(colSums(df_rule[df_rule$JID ==jid, c(3,4)]))
    }
  }
  final <- data.frame(FirstCovid19)
  for (i in 1:nrow(codeBook)) {
    verbose = (i %% verbose.period == 1)
    if (verbose) cat(sprintf("\n[%d]\n",i)) 
    
    code_val <- codeBook[i,"code"]
    code_len <- nchar(codeBook[i,"code"])
    
    if (!is.null(study)) {
      r.v <- codeBook[i, study]
      if (verbose) print(r.v)
    }
    
    sF_split1 <- strsplit(codeBook[i,"searchFor"], "; ")
    if (verbose) cat(sprintf("Unique ID : %d. [%s]", codeBook[i,"uniqueID"], code_val))
    if (verbose) print(Sys.time())
    
    j = 1; dummy <- NULL
    while(!is.na(sF_split1[[1]][j])){
      sF_split2 <- strsplit(sF_split1[[1]][j], "/")
      
      df_name <- sF_split2[[1]][1]
      var_name <- sF_split2[[1]][2]
      df_all <- data.frame(get(df_name)) # df 초기화, cbind() 불가
      
      if (is.null(df_rule)){
        df <- df_all 
        # outcome이면 t200에서 특정 시점을 기준으로 자를 필요 없이, 다 가져와야 함.
      } else {
        if (r.v %in% names(df_rule)){ # names(df_rule) : JID, MID, EXPOSURE.itself, exposure.and.outcome.related
          df <- df_all[df_all$MID %in% df_rule[df_rule[,r.v]==1,"MID"], ]
          # df에는 검색대상이 좁혀짐. (r.v가 )
        }
        else{
          df <- df_all
        }
      }
      if (verbose) cat(sprintf("[%d] before : %d, after : %d\n",j, nrow(df_all), nrow(df)))
      if (codeBook[i,"codeType"] == "ATC") {
        rule = (toupper(substr(ATC_mapping_df[ ,"ATC"],1,code_len)) == toupper(code_val))
        if (sum(rule)==0){
          if (verbose) cat(" ATC code", i,". [", code_val,"] mapping X ")
          j = j+1 #while 문 사용해서 +1
          next
        } else {
          mapped_var <- ATC_mapping_df[rule, var_name]
          dummy2 <- NULL
          for (each_var in mapped_var) {
            dummy2_temp <- data.frame("JID" = df$JID, "var" = ifelse(df[,var_name] == each_var, 1, 0))
            dummy2_temp[is.na(dummy2_temp[,"var"]),"var"] <- 0
            dummy2_tempDT <- as.data.table(dummy2_temp)
            dummy2_tempDT2 <- dummy2_tempDT[, .(var_sum = sum(var)), by=JID]
            if (is.null(dummy2)) {
              dummy2 <- dummy2_tempDT2
            }
            else {
              dummy2$var_sum <- dummy2$var_sum + dummy2_tempDT2$var_sum
            }
          }
          # dummy_temp2 : searchFor 하나에 대한 정보 저장 (dummy2 의 ifelse(rowSums>-1,1,0) )
          dummy_temp2 <- data.frame("JID" = dummy2$JID, "var" = ifelse(dummy2$var_sum>0,1,0))
        }
      } else {
        dummy_temp <- data.frame("JID" = df$JID, "var" = ifelse(toupper(substr(df[,var_name],1,code_len)) == toupper(code_val),1,0))
        dummy_temp[is.na(dummy_temp[,"var"]),"var"] <- 0
        dummy_tempDT <- as.data.table(dummy_temp)
        dummy2 <- dummy_tempDT[, .(var_sum = sum(var)), by=JID]
      }
      if (j == 1) {
        dummy <- dummy2
      } else { 
        dummy$var_sum <- dummy$var_sum + dummy2$var_sum
      }
      j = j+1
    }
    
    if(is.null(dummy)) next
    
    final_temp <- data.frame("JID" = dummy$JID, "var" = ifelse(dummy$var_sum>0,1,0))
    
    colnames(final_temp) <- c("JID", code_val)
    final <- merge(final, final_temp, by=c("JID"), all=TRUE)
  }
  # [?] 추가함
  final[is.na(final)] <- 0
  return (final)
}

time = Sys.time()
# [1] comorbidties ICD10 (sheet2)
phx_info30_statin <- gen_dummies(code_ICD10_df, "statin.study",30)
phx_info30_anti <- gen_dummies(code_ICD10_df, "antiplatelet.anticoagulant.study",30)
phx_info30_immune <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 30)

#colSums(phx_info30_statin[,-1])
#colSums(phx_info30_anti[,-1])
#colSums(phx_info30_immune[,-1])

phx_info120_statin <- gen_dummies(code_ICD10_df, "statin.study",120)
phx_info120_anti <- gen_dummies(code_ICD10_df, "antiplatelet.anticoagulant.study",120)
phx_info120_immune <- gen_dummies(code_ICD10_df, "immune.suppressant.study", 120)

#colSums(phx_info120_statin[,-1])
#colSums(phx_info120_anti[,-1])
#colSums(phx_info120_immune[,-1])

# check
sum(phx_info30_statin != phx_info120_statin)
sum(phx_info30_anti != phx_info120_anti)
sum(phx_info30_immune != phx_info120_immune)
sum(phx_info30_statin != phx_info120_anti)
sum(phx_info30_anti != phx_info120_immune)
sum(phx_info30_immune != phx_info120_statin)

time1 = Sys.time()

# [2] outcomes (sheet4)
# outcomes code book 에 중복된 셀 존재
covid_outcomes <- gen_dummies(code_outcomes_df)
time2 = Sys.time()

# [3] medications ATC code (sheet2)
# [code book(code_ATC)의 code column] = [ATC_mapping_df의 ATC코드 column] 
# [ATC_mapping_df의 주성분코드] = [T530의 GNL_CD]
# [ATC_mapping_df의 제품코드]  = T530의 DIV_CD  (T300의 DIV_CD도 일부 존재)
ATC_med30_statin <- gen_dummies(code_ATC_df, "statin.study",30)
ATC_med30_anti <- gen_dummies(code_ATC_df, "antiplatelet.anticoagulant.study",30)
ATC_med30_immune <- gen_dummies(code_ATC_df, "immune.suppressant.study", 30)

#colSums(ATC_med30_statin[,-1])
#colSums(ATC_med30_anti[,-1])
#colSums(ATC_med30_immune[,-1])

ATC_med120_statin <- gen_dummies(code_ATC_df, "statin.study",120)
ATC_med120_anti <- gen_dummies(code_ATC_df, "antiplatelet.anticoagulant.study",120)
ATC_med120_immune <- gen_dummies(code_ATC_df, "immune.suppressant.study", 120)

#colSums(phx_info120_statin[,-1])
#colSums(phx_info120_anti[,-1])
#colSums(phx_info120_immune[,-1])

time3 = Sys.time()

sum(ATC_med30_statin != ATC_med120_statin)
sum(ATC_med30_anti != ATC_med120_anti)
sum(ATC_med30_immune != ATC_med120_immune)
sum(ATC_med30_statin != ATC_med120_anti)
sum(ATC_med30_anti != ATC_med120_immune)
sum(ATC_med30_immune != ATC_med120_statin)

# [4] COVID19 code (sheet5)
covid_diag <- gen_dummies(code_covid_df)
time4 = Sys.time()

cat("<---------------------------------------------------------->\n")
cat("<------------------------ RESULTS ------------------------->\n")
cat("<---------------------------------------------------------->\n")


excl.cols = (1:ncol(FirstCovid19))

# covid_diag_agg
covid_diag_agg = covid_diag[ ,-excl.cols]
colnames(covid_diag_agg)[colnames(covid_diag_agg) == "Y"] = "Confirm"

# covid_outcomes_agg

if (ncol(covid_outcomes[ ,-excl.cols]) != nrow(code_outcomes_df)) stop("Code names do not match!!!")
vec_or = function(x) {as.numeric(sum(x) > 0)}
covid_outcomes_agg = t( apply(covid_outcomes[ ,-excl.cols], 1, function(x) tapply(x, code_outcomes_df$name, vec_or) ) )

phx_df <- phx_info30_statin; ATC_df <-  ATC_med30_statin
# 각 스터디에 대해서
marginal_asso <- function(phx_df, ATC_df, study = "statin30") {
  # phx_info
  phx_df2 = phx_df[ ,-excl.cols]
  #head(phx_df2)
  if (ncol(phx_df2) != nrow(code_ICD10_df)) stop("Code names do not match!!!")
  #print("ok")
  vec_or = function(x) { as.numeric(sum(x) > 0) }
  
  phx_df_agg = t( apply(phx_df2, 1, function(x) tapply(x, code_ICD10_df$name, vec_or) ) )
  phx_df_agg[ ,"Fracture"] =
    phx_df_agg[ ,"Fracture"] - (phx_df_agg[ ,"Fracture"] * phx_df2[ ,"S025"])
  
  # ATC_med
  ATC_df2 = ATC_df[ ,-excl.cols]
  code_ATC_df_tmp = code_ATC_df[code_ATC_df$code %in% colnames(ATC_df2), ]
  
  if (ncol(ATC_df2) != nrow(code_ATC_df_tmp)) stop("Code names do not match!!!")
  vec_or = function(x) {as.numeric(sum(x) > 0)}
  
  #print("ok")
  
  ATC_df_agg = t( apply(ATC_df2, 1, function(x) tapply(x, code_ATC_df_tmp$name, vec_or) ) )
  ATC_df_agg[ ,"Lipid lowering agents including statin"] =
    ATC_df_agg[ ,"Lipid lowering agents including statin"] - 
    (ATC_df_agg[ ,"Lipid lowering agents including statin"] * ATC_df_agg[ ,"statin"]) # [?] phx 랑 다름
  
  x <- cbind(covid_diag_agg, phx_df_agg, ATC_df_agg)
  y <- covid_outcomes_agg
  
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
  
  write.csv(colMeans(x), sprintf("./resultsCNLLS/colmeans_x_%s.csv", study), col.names=T, row.names=T)
  write.csv(colMeans(y), sprintf("./resultsCNLLS/colmeans_y_%s.csv", study), col.names=T, row.names=T)
  write.csv(asso.mat, sprintf("./resultsCNLLS/asso_%s.csv", study), col.names=T, row.names=T )
  
  return (asso.mat)
}

# [1] statin 30
print(" <-------------- statin 30 --------------> ")
asso.mat_statin30 <- marginal_asso(phx_df = phx_info30_statin, 
                                   ATC_df = ATC_med30_statin,
                                   study = "statin30")

# [2] statin 120
print(" <-------------- statin 120 --------------> ")
asso.mat_statin120 <- marginal_asso(phx_df = phx_info120_statin, 
                                    ATC_df = ATC_med120_statin,
                                    study = "statin120")

# [3] anti 30
print(" <-------------- anti 30 --------------> ")
asso.mat_anti30 <- marginal_asso(phx_df = phx_info30_anti, 
                                 ATC_df = ATC_med30_anti,
                                 study = "anti30")

# [4] anti 120
print(" <-------------- anti 120 --------------> ")
asso.mat_anti120 <- marginal_asso(phx_df = phx_info120_anti, 
                                  ATC_df = ATC_med120_anti,
                                  study = "anti120")

# [5] immune 30
print(" <-------------- immune 30 --------------> ")
asso.mat_immune30 <- marginal_asso(phx_df = phx_info30_immune, 
                                   ATC_df = ATC_med30_immune,
                                   study = "immune30")

# [6] immune 120
print(" <-------------- immune 120 --------------> ")
asso.mat_immune120 <- marginal_asso(phx_df = phx_info120_immune, 
                                    ATC_df = ATC_med120_immune,
                                    study = "immune120")
