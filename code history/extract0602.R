
# load raw data (sample data for now)
# load code book 2_JL.xlsx
# load ATC mapping data
# define an empty new data frame to save cleaned data

####### for each row of all worksheets (excluding `general`) in the codebook, ###
####### define a new column of size N by 1  #####################################
# N : sample size
# code each cell as 1 if the value in `Code` exists at his/her `searchFor`, 0 otherwise

# caution 1: uniqueID 251번은 우선 스킵.
# caution 2: if codeType is ATC, it needs one more lookup from the ATC mapping data


########################## END of the for loop ##################################



# merge several columns by `or` opeartions

# final check
#print(colSums(df))

# Corona claim data
data_path <- "./data/sample_schema.xlsx"
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
co19_t200_trans_dn <- co19_t200[,c("JID", "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK")]
co19_t300_trans_dn <- co19_t300[,c("JID", "MID", "DIV_CD")]
co19_t400_trans_dn <- co19_t400[,c("JID", "MID", "SICK_CD")]
co19_t530_trans_dn <- co19_t530[,c("JID", "MID", "DIV_CD", "GNL_CD")]

co19_twjhe200_trans_dn <- co19_twjhe200[,c("JID", "MID", "SEX_TP_CD", "PAT_AGE", "MAIN_SICK", "SUB_SICK")]
co19_twjhe300_trans_dn <- co19_twjhe300[,c("JID", "MID", "DIV_CD")]
co19_twjhe400_trans_dn <- co19_twjhe400[,c("JID", "MID", "SICK_CD")]
co19_twjhe530_trans_dn <- co19_twjhe530[,c("JID", "MID", "DIV_CD", "GNL_CD")]

# External files (code_book)
codebook_path <- "./codebook"
codebook <- "code books 2_JL_YC.xlsx"

code_ICD10 <- read_excel(file.path(codebook_path, codebook), sheet = 2)
code_ATC <- read_excel(file.path(codebook_path, codebook), sheet = 3)
code_outcomes <- read_excel(file.path(codebook_path, codebook), sheet = 4)
ATC_mapping <- read_excel(file.path(codebook_path, "약제급여목록표(20년 4월) ATC코드 매핑.xlsx"))
# code_covid <- read_excel("./data/code books 2_JL_YC.xlsx", sheet = 5)

head(code_ICD10)
head(code_ATC)
head(code_outcomes)

code_ICD10_df <- data.frame(code_ICD10)
code_ATC_df <- data.frame(code_ATC)
code_outcomes_df <- data.frame(code_outcomes)
ATC_mapping_df <- data.frame(ATC_mapping)[,c("주성분코드", "제품코드", "ATC코드")]
colnames(ATC_mapping_df) <- c("GNL_CD", "DIV_CD", "ATC")
str(ATC_mapping_df)

#####################################################
######################################### code_ICD10 
#####################################################
#codeBook <- code_outcomes_df
#i=6
head(codeBook)
#codeBook <- code_ATC_df

gen_mapping <- function(codeBook){
  final <- NULL
  for (i in 1:nrow(codeBook)) {
    print(i)
    code_val <- codeBook[i,"code"]
    code_len <- nchar(codeBook[i,"code"])
    sF_split1 <- strsplit(codeBook[i,"searchFor"], "; ")
    
    j = 1; dummy <- NULL
    while(!is.na(sF_split1[[1]][j])){
      sF_split2 <- strsplit(sF_split1[[1]][j], "/")
      
      df_name <- sF_split2[[1]][1]
      var_name <- sF_split2[[1]][2]
      df <- data.frame(get(df_name))# df 초기화됨 -> cbind() 불가
      
      dummy_temp <- data.frame("JID" = df$JID, "var" = ifelse(substr(df[,var_name],1,code_len) == code_val,1,0))
      dummy_temp[is.na(dummy_temp[,"var"]),"var"] <- 0
      dummy_temp2 <- dummy_temp %>% group_by(JID) %>% summarise(var = sum(dummy_temp[,"var"])) %>% data.frame()
  
      if (j == 1) {
        # [?] 왜 rbind 만 되는지
        dummy <- rbind(dummy, dummy_temp2)
      } else { 
        dummy <- merge(dummy, dummy_temp2, by=c("JID"), all=TRUE)
        #print(nrow(dummy))
      }
      j = j+1
    }
    #head(dummy)
    final_temp <- data.frame("JID" = dummy$JID, "var" = ifelse(rowSums(dummy[,-1])>0,1,0))
    colnames(final_temp) <- c("JID", code_val)
    if (i==1) {
      final <- rbind(final, final_temp)
    } else { 
      final <- merge(final, final_temp, by=c("JID"), all=TRUE)
    }
    #head(final)
    #print(nrow(final))
  }
  return (final)
}

phx_info <- gen_mapping(code_ICD10_df)
str(phx_info)
phx_info

# [?] outcomes code book 에 중복된 셀 존재
covid_sick <- gen_mapping(code_outcomes_df)
str(covid_sick)
covid_med

# [code book(code_ATC)의 code column] = [ATC_mapping_df의 ATC코드 column] 
# [ATC_mapping_df의 주성분코드] = [T530의 GNL_CD]
# [ATC_mapping_df의 제품코드]  = T530의 DIV_CD  (T300의 DIV_CD도 일부 존재)


# DIV_CD 를 ATC 로,
#co19_t530_trans_dn - GNL_CD, DIV_CD
co19_t530_trans_dn <- merge(co19_t530_trans_dn, ATC_mapping_df, by = c("GNL_CD","DIV_CD"))

t530_DIV_CD <- co19_t530_trans_dn$DIV_CD
t530_GNL_CD <- co19_t530_trans_dn$GNL_CD

co19_t530_trans_dn$DIV_CD <- co19_t530_trans_dn$ATC
co19_t530_trans_dn$GNL_CD <- co19_t530_trans_dn$ATC
co19_t530_trans_dn <- co19_t530_trans_dn[,c("JID","GNL_CD","DIV_CD")]
head(co19_t530_trans_dn)

#co19_t300_trans_dn - DIV_CD
t300_merge <- merge(co19_t300_trans_dn, ATC_mapping_df[,c("DIV_CD","ATC")], by = "DIV_CD",all=TRUE)

t300_merge[t300_merge$DIV_CD == "M1007192",]

head(t300_merge[!is.na(t300_merge$JID),])
var_name
ATC_mapping_df[which("645302132" %in% ATC_mapping_df[,var_name]),]

t300_DIV_CD <- co19_t300_trans_dn$DIV_CD
co19_t300_trans_dn$DIV_CD <- co19_t300_trans_dn$ATC
co19_t300_trans_dn <- co19_t300_trans_dn[,c("JID", "DIV_CD")]
head(co19_t300_trans_dn)

# code_ATC mapping
covid_med <- gen_mapping(code_ATC_df)
str(covid_med)

# Check, code_ATC_df, ATC 와 ATC_mapping_df, ATC 매칭 체크

code_ATC_df2 <- codeATC_df


disease_covid = disease_covid[which(disease_covid$MID %in% demographic_covid$MID),]


