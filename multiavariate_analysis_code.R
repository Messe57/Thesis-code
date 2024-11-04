###TESI OMEGA3C CORRELAZIONE####
#####importazione librerie#####
rm(list=ls())
graphics.off()
library(settings)
library('readxl')
library('Matrix')
library('corrplot')
library('forcats')
library('lattice')
library('ggplot2')
library('lme4')
library('lubridate')
library('nlmeU')
library('dplyr')
library('tidyr')
library('psych')
library('naniar')
library('sjPlot')
library('tidyverse')
library('janitor')
library('rlang')
library('ggthemes')
library('gt')
library('gtExtras')
library('webshot2')
library('magrittr')
library('plot.matrix')
library('car')
library('nlme')
# library('insight')

#####lettura dataset#####
#"C:\Users\filip\Politecnico_tesi\DB_Anonimizzato_20240520 - Copia.xlsx"
df <- read_excel("C:/Users/filip/Politecnico_tesi/DB_Anonimizzato_20240520 - Copia.xlsx", 
                 na = '0') %>%
  as_tibble() %>% 
  clean_names()


#####visualizzazione struttura df#####
str(df)
summary(df)
nomi_colonne <- colnames(df)
for (col in nomi_colonne) {
  numero_di_na <- sum(is.na(df[[col]]))
  print(paste(col, numero_di_na))
}
#####rimozione colonne con NA##### 
df_senzaNA <- df[, colSums(!is.na(df)) > 0]
#rimanenti 69 variabili#

head(df_senzaNA)
nomi_colonne <- colnames(df_senzaNA)
summary(df_senzaNA)

df_clean <- df_senzaNA%>%
  mutate(across(everything(), ~ sub(".* - ", "", .)))

####rimozione delle righe che non hanno ne NPS ne CES####
df_clean$nps <- as.numeric(df_clean$nps)
df_clean$ces <- as.numeric(df_clean$ces)
df_clean <- df_clean[!is.na(df_clean$nps) | !is.na(df_clean$ces), ]
# df_clean <- df_clean[!is.na(df_clean$ces),]
# table(df_clean$ces, df_clean$easy_support)

####rimozione righe con tutti NA####
df_clean <- df_clean[rowSums(is.na(df_clean)) != ncol(df_clean), ]

#####trasformazione del tipo della variabile#####
df_clean$local_response_date <- ymd(df_clean$local_response_date)
df_clean$banca_mcf_xav_cod <- as.factor(df_clean$banca_mcf_xav_cod)
df_clean$browser <- as.factor(df_clean$browser)
df_clean$sistema_operativo <- as.factor(df_clean$sistema_operativo)
df_clean$segmento_des_comm <- as.factor(df_clean$segmento_des_comm)
df_clean$nps_segment <- as.factor(df_clean$nps_segment)
df_clean$satisfaction <- as.numeric(df_clean$satisfaction)
df_clean$gestore <- as.factor(df_clean$gestore)
df_clean$survey <- as.factor(df_clean$survey)
df_clean$operazione <- as.factor(df_clean$operazione)
df_clean$banca_mcf_prodotto_code <- as.factor(df_clean$banca_mcf_prodotto_code)
df_clean$regione_des_ana <- as.factor(df_clean$regione_des_ana)
df_clean$area <- as.factor(df_clean$area)
df_clean$direzione <- as.factor(df_clean$direzione)
df_clean$filiale <- as.factor(df_clean$filiale)
df_clean$email <- as.factor(df_clean$email)
df_clean$customer_segment <- as.factor(df_clean$customer_segment)
df_clean$attivita_eco_des_ana <- as.factor(df_clean$attivita_eco_des_ana)
df_clean$sesso_code_ana <- as.factor(df_clean$sesso_code_ana)
df_clean$fascia_eta_code_ana <- as.factor(df_clean$fascia_eta_code_ana)
df_clean$fascia_anzianita_code_ana <- as.factor(df_clean$fascia_eta_code_ana)
df_clean$anzianita_anni_num <- as.numeric(df_clean$anzianita_anni_num)
df_clean$xav_profilo_postazione <- as.factor(df_clean$xav_profilo_postazione)
df_clean$fascia_fatturato_ana <- as.factor(df_clean$fascia_fatturato_ana)
df_clean$banca_mcf_xav_des <- as.factor(df_clean$banca_mcf_xav_des)
df_clean$vdbank_flg_ana <- as.factor(df_clean$vdbank_flg_ana)
df_clean$xntweb_flg_ana <- as.factor(df_clean$xntweb_flg_ana)
df_clean$cliente_con_mutuo_flg_ana <- as.factor(df_clean$cliente_con_mutuo_flg_ana)
df_clean$cliente_investitore_flg_ana <- as.factor(df_clean$cliente_investitore_flg_ana)
df_clean$cliente_solo_cc_flg_ana <- as.factor(df_clean$cliente_solo_cc_flg_ana)
df_clean$in_bonis_flg_comm <- as.factor(df_clean$in_bonis_flg_comm)
df_clean$multi_flg_comm <- as.factor(df_clean$multi_flg_comm)
df_clean$risk_rating_comm <- as.factor(df_clean$risk_rating_comm)
df_clean$cs_abi_num_comm <- as.numeric(df_clean$cs_abi_num_comm)
df_clean$fascia_tr_code_comm <- as.factor(df_clean$fascia_tr_code_comm)
df_clean$fascia_utilizzo_online_comm <- as.factor(df_clean$fascia_utilizzo_online_comm)
df_clean$nps_factors_survey_canale <- as.factor(df_clean$nps_factors_survey_canale)
df_clean$nps_factors_survey_canale_other <- as.factor(df_clean$nps_factors_survey_canale_other)
df_clean$ces_factors_transfer_intercept <- as.factor(df_clean$ces_factors_transfer_intercept)
df_clean$ces_factors_other <- as.factor(df_clean$ces_factors_other)
df_clean$app_knowledge <- as.factor(df_clean$app_knowledge)
df_clean$app_used <- as.factor(df_clean$app_used)
df_clean$xa_app_not_used_reason <- as.factor(df_clean$xa_app_not_used_reason)
df_clean$xa_app_not_used_reason_other <- as.factor(df_clean$xa_app_not_used_reason_other)
df_clean$xv_va_app_not_used_reason <- as.factor(df_clean$xv_va_app_not_used_reason)
df_clean$nps_factors_survey_filiale <- as.factor(df_clean$nps_factors_survey_filiale)
df_clean$reason_for_score_nps_filiale <- as.factor(df_clean$reason_for_score_nps_filiale)
df_clean$need_satisfied <- as.factor(df_clean$need_satisfied)
df_clean$easy_support <- as.numeric(df_clean$easy_support)
df_clean$service_satisfaction <- as.numeric(df_clean$service_satisfaction)
df_clean$reason_for_satisfaction <- as.factor(df_clean$reason_for_satisfaction)
df_clean$reason_for_score_comment <- as.factor(df_clean$reason_for_score_comment)
df_clean$other_issue_type <- as.factor(df_clean$other_issue_type)
df_clean$expenses_icon <- as.factor(df_clean$expenses_icon)
df_clean$expenses_icon_used <- as.factor(df_clean$expenses_icon_used)
df_clean$consulted_features <- as.factor(df_clean$consulted_features)
df_clean$interested_in_functionality <- as.factor(df_clean$interested_in_functionality)
df_clean$ces_reason_for_score <- as.factor(df_clean$ces_reason_for_score)
df_clean$feature_not_used <- as.factor(df_clean$feature_not_used_other)
df_clean$not_entered_in_expenses_section <- as.factor(df_clean$not_entered_in_expenses_section)
df_clean$not_entered_in_expenses_section_other <- as.factor(df_clean$not_entered_in_expenses_section_other)
df_clean$suggestions_to_improve <- as.factor(df_clean$suggestions_to_improve)
df_clean$reason_for_dissatisfaction <- as.factor(df_clean$reason_for_dissatisfaction)
df_clean$feature_not_used_other <- as.factor(df_clean$feature_not_used_other)
df_clean$disservices_description <- as.factor(df_clean$disservices_description)


# df_clean <- df_clean %>%
#   mutate_all(~ ifelse(is.na(.), 0, .))

# convert_factor_to_binary <- function(col) {
#   if (is.factor(col) && length(levels(col)) == 2) {
#     return(ifelse(col == levels(col)[1], 1, 0))
#   }
#   return(col)
# }
# 
# df_binary <- as.data.frame(lapply(df_clean, convert_factor_to_binary))
summary(df_clean)
str(df_clean)

# df_clean <- df_clean[df_clean$anzianita_anni_num < 100, ]
df_clean$target <- ifelse(is.na(df_clean$nps), df_clean$ces, df_clean$nps)
df_clean$target.f <- as.factor(df_clean$target)
df_clean$target_bin <- ifelse(df_clean$target > 8, 1, 0)
df_clean$target_bin.f <- as.factor(df_clean$target_bin)
table(df_clean$target_bin)
table(df_clean$survey)

# #filling NA in binary cols with 0
# df_clean$email <- as.numeric(df_clean$email)
# df_clean$multi_flg_comm <- as.numeric(df_clean$multi_flg_comm)
# df_clean$reason_for_satisfaction <- as.numeric(df_clean$reason_for_satisfaction)
# df_clean$suggestions_to_improve <- as.numeric(df_clean$suggestions_to_improve)
# df_clean$reason_for_dissatisfaction <- as.numeric(df_clean$reason_for_dissatisfaction)
# df_clean$banca_mcf_xav_des <- as.numeric(df_clean$banca_mcf_xav_des)
# df_clean$nps_factors_survey_canale_other <- as.numeric(df_clean$nps_factors_survey_canale_other)
# df_clean$ces_factors_other <- as.numeric(df_clean$ces_factors_other)
# df_clean$xa_app_not_used_reason_other <- as.numeric(df_clean$xa_app_not_used_reason_other)
# df_clean$reason_for_score_nps_filiale <- as.numeric(df_clean$reason_for_score_nps_filiale)
# df_clean$disservices_description <- as.numeric(df_clean$disservices_description)
# df_clean$reason_for_score_comment <- as.numeric(df_clean$reason_for_score_comment)
# df_clean$other_issue_type <- as.numeric(df_clean$other_issue_type)
# df_clean$ces_reason_for_score <- as.numeric(df_clean$ces_reason_for_score)
# df_clean$feature_not_used <- as.numeric(df_clean$feature_not_used)
# df_clean$feature_not_used_other <- as.numeric(df_clean$feature_not_used_other)
# df_clean$not_entered_in_expenses_section_other <- as.numeric(df_clean$not_entered_in_expenses_section_other)
# #sostituisco gli NA con 0 solo nelle colonne specifiche
# df_clean[, c("multi_flg_comm", 
#              "reason_for_satisfaction",
#              "suggestions_to_improve", 
#              "reason_for_dissatisfaction",
#              "email",
#              "banca_mcf_xav_des",
#              "nps_factors_survey_canale_other",
#              "ces_factors_other",
#              "xa_app_not_used_reason_other",
#              "reason_for_score_nps_filiale",
#              "disservices_description",
#              "reason_for_score_comment",
#              "other_issue_type", 
#              "ces_reason_for_score",
#              "feature_not_used", 
#              "feature_not_used_other", 
#              "not_entered_in_expenses_section_other")][is.na(df_clean[, c("multi_flg_comm",
#                                                                           "reason_for_satisfaction",
#                                                                           "suggestions_to_improve", 
#                                                                           "reason_for_dissatisfaction", 
#                                                                           "email",
#                                                                           "banca_mcf_xav_des",
#                                                                           "nps_factors_survey_canale_other",
#                                                                           "ces_factors_other",
#                                                                           "xa_app_not_used_reason_other",
#                                                                           "reason_for_score_nps_filiale",
#                                                                           "disservices_description",
#                                                                           "reason_for_score_comment",
#                                                                           "other_issue_type",
#                                                                           "ces_reason_for_score",
#                                                                           "feature_not_used",
#                                                                           "feature_not_used_other",
#                                                                           "not_entered_in_expenses_section_other")])] <- 0
# df_clean$multi_flg_comm <- as.factor(df_clean$multi_flg_comm)
# df_clean$reason_for_satisfaction <- as.factor(df_clean$reason_for_satisfaction)
# df_clean$suggestions_to_improve <- as.factor(df_clean$suggestions_to_improve)
# df_clean$reason_for_dissatisfaction <- as.factor(df_clean$reason_for_dissatisfaction)
# df_clean$email <- as.factor(df_clean$email)
# df_clean$banca_mcf_xav_des <- as.factor(df_clean$banca_mcf_xav_des)
# df_clean$nps_factors_survey_canale_other <- as.factor(df_clean$nps_factors_survey_canale_other)
# df_clean$ces_factors_other <- as.factor(df_clean$ces_factors_other)
# df_clean$xa_app_not_used_reason_other <- as.factor(df_clean$xa_app_not_used_reason_other)
# df_clean$reason_for_score_nps_filiale <- as.factor(df_clean$reason_for_score_nps_filiale)
# df_clean$disservices_description <- as.factor(df_clean$disservices_description)
# df_clean$reason_for_score_comment <- as.factor(df_clean$reason_for_score_comment)
# df_clean$other_issue_type <- as.factor(df_clean$other_issue_type)
# df_clean$ces_reason_for_score <- as.factor(df_clean$ces_reason_for_score)
# df_clean$feature_not_used <- as.factor(df_clean$feature_not_used)
# df_clean$feature_not_used_other <- as.factor(df_clean$feature_not_used_other)
# df_clean$not_entered_in_expenses_section_other <- as.factor(df_clean$not_entered_in_expenses_section_other)

#####rimozione duplicati######
# df_clean <- unique(df_clean)


####separazione del dataset nelle varie survey####
survey_1  = df_clean[df_clean$survey == 1,] 
survey_2 = df_clean[df_clean$survey == 2,] 
survey_3 = df_clean[df_clean$survey == 3,] 
survey_4 = df_clean[df_clean$survey == 4,] 
survey_16 = df_clean[df_clean$survey == 16,] 
survey_18 = df_clean[df_clean$survey == 18,] 
survey_52 = df_clean[df_clean$survey == 52,]

####rimozione delle colonne con tutti NA per le varie survey####
survey_1<- survey_1[, colSums(!is.na(survey_1)) > 0]
survey_2<- survey_2[, colSums(!is.na(survey_2)) > 0]
survey_3<- survey_3[, colSums(!is.na(survey_3)) > 0]
survey_4<- survey_4[, colSums(!is.na(survey_18)) > 0]
survey_16<- survey_16[, colSums(!is.na(survey_16)) > 0]
survey_18<- survey_18[, colSums(!is.na(survey_18)) > 0]
survey_52<- survey_52[, colSums(!is.na(survey_52)) > 0]

####check for missing values ####
df_clean %>% sapply(function(x) sum(is.na(x)))
survey_1 %>% sapply(function(x) sum(is.na(x)))
survey_2 %>% sapply(function(x) sum(is.na(x)))
survey_3 %>% sapply(function(x) sum(is.na(x)))
survey_4 %>% sapply(function(x) sum(is.na(x)))
survey_16 %>% sapply(function(x) sum(is.na(x)))
survey_18 %>% sapply(function(x) sum(is.na(x)))
survey_52 %>% sapply(function(x) sum(is.na(x)))

####check for duplicates ####
df_clean %>% duplicated() %>% sum()
survey_1 %>% duplicated() %>% sum()
survey_2 %>% duplicated() %>% sum()
survey_3 %>% duplicated() %>% sum()
survey_4 %>% duplicated() %>% sum()
survey_16 %>% duplicated() %>% sum()
survey_18 %>% duplicated() %>% sum()
survey_52 %>% duplicated() %>% sum()

##no duplicated rows

skimr::skim(df_clean)


####pre exploratory data analysis general ####
cols_tot <- names(df_clean)
df_clean %>% 
  select(all_of(cols_tot)) %>% 
  map(function(x) n_distinct(x, na.rm = TRUE))


#checking duplicated columns#
names(df_clean)

ser <- df_clean$service_satisfaction
sat <- df_clean$satisfaction
diff <- sum(ser != sat)
# diff --> nessun valore differente

table(df_clean$segmento_des_comm, df_clean$customer_segment)
#variabili riscalate tra loro, troppo collineari

table(df_clean$fascia_eta_code_ana, df_clean$fascia_anzianita_code_ana)
#variabili identiche

ftable_result <- ftable(df_clean$fascia_eta_code_ana, df_clean$fascia_anzianita_code_ana)
print(ftable_result)

library(gmodels)
CrossTable(df_clean$fascia_eta_code_ana, df_clean$fascia_anzianita_code_ana,
           prop.chisq = FALSE,  # Disable chi-squared contributions
           prop.t = FALSE,       # Show proportions of table
           prop.r = FALSE,       # Show row proportions
           prop.c = FALSE)       # Show column proportions

CrossTable(df_clean$target, df_clean$operazione,
           prop.chisq = FALSE,  # Disable chi-squared contributions
           prop.t = FALSE,       # Show proportions of table
           prop.r = FALSE,       # Show row proportions
           prop.c = FALSE)       # Show column proportions



(unique(df_clean$filiale)) #1365 livelli
(unique(df_clean$gestore)) #3960 livelli

#####exploratory analysis generale######
names(df_clean)
summary(df_clean$ces) 
table(df_clean$ces) 
hist(df_clean$ces)
ces_distribution <- as.data.frame(table(df_clean$ces))
names(ces_distribution) <- c("ces", "Count") 
ces_distribution$ces <- as.numeric(as.character(ces_distribution$ces))
ces_distribution <- ces_distribution[ces_distribution$ces > 0,]
# Calcola la proporzione per ogni valore del ces 
ces_distribution$Proportion <- ces_distribution$Count / sum(ces_distribution$Count) 
daily_mean_ces <- df_clean %>%
  group_by(local_response_date) %>%
  summarise(mean_ces = mean(ces, na.rm = TRUE))
ggplot(df_clean, aes(y = ces)) +   
  geom_boxplot(fill = "lightgreen") +   
  labs(title = "Boxplot of Daily Mean ces", y = "Daily Mean ces") +   
  theme_minimal()
###escludendo i valori NA, più del 58% delle osservazioni ha valore 9/10
###circa il 30%  si trova tra 7/8 e i restanti più sotto

summary(df_clean$satisfaction) 
table(df_clean$satisfaction) ## 28350 NA
hist(df_clean$satisfaction) 
boxplot(df_clean$satisfaction) 
satisfaction_distribution <- as.data.frame(table(df_clean$satisfaction))
names(satisfaction_distribution) <- c("satisfaction", "Count") 
satisfaction_distribution$satisfaction <- as.numeric(as.character(satisfaction_distribution$satisfaction))
satisfaction_distribution <- satisfaction_distribution[satisfaction_distribution$satisfaction > 0,]
# Calcola la proporzione per ogni valore del satisfaction 
satisfaction_distribution$Proportion <- satisfaction_distribution$Count / sum(satisfaction_distribution$Count) 
daily_mean_satisfaction <- df_clean %>%   
  group_by(local_response_date) %>%   
  summarise(mean_satisfaction = mean(satisfaction, na.rm = TRUE)) 
ggplot(df_clean, aes(y = satisfaction)) +   
  geom_boxplot(fill = "lightgreen") +   
  labs(title = "Boxplot of Daily Mean Satisfaction", y = "Daily Mean Satisfaction") +   
  theme_minimal()
###60% 9/10, 23% 7/8 e i restanti al di sotto

summary(df_clean$browser) ##18813 NA
table(df_clean$browser) 
browser_distribution <- as.data.frame(table(df_clean$browser)) 
names(browser_distribution) <- c("browser", "Count") 
# Calcola la proporzione per ogni categoria del browser 
browser_distribution$Proportion <- browser_distribution$Count / sum(browser_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del browser 
ggplot(browser_distribution, aes(x = browser, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  
  labs(title = "Normalized Distribution of Browser Categories", x = "Browser", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi tutte le survey si riferiscono a 5 browsers (1,2,3,18,5)

summary(df_clean$gestore) ##18917 NA
table(df_clean$gestore) 
gestore_distribution <- as.data.frame(table(df_clean$gestore)) 
names(gestore_distribution) <- c("gestore", "Count") 
# Calcola la proporzione per ogni categoria del gestore 
gestore_distribution$Proportion <- gestore_distribution$Count / sum(gestore_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(gestore_distribution, aes(x = gestore, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  
  labs(title = "Normalized Distribution of gestore Categories", x = "gestore", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###troppi livelli per poter identificare una distribuzione o dare un significato


table(df_clean$anzianita_anni_num) 
summary(df_clean$anzianita_anni_num) ##51821 NA
years_distribution <- as.data.frame(table(df_clean$anzianita_anni_num)) 
names(years_distribution) <- c("Years", "Count") 
# Calcola la proporzione per ogni categoria del browser 
years_distribution$Proportion <- years_distribution$Count / sum(years_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del browser 
ggplot(years_distribution, aes(x = Years, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of years Categories", x = "Years", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
### 220 outlier oltre i 100 --> nessun valore tra i 69 e i 123/1218

table(df_clean$area) 
summary(df_clean$area) 
area_distribution <- as.data.frame(table(df_clean$area)) 
names(area_distribution) <- c("area", "Count") 
# Calcola la proporzione per ogni categoria del area 
area_distribution$Proportion <- area_distribution$Count / sum(area_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(area_distribution, aes(x = area, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Area Categories", x = "Area", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###20% dei record si riferiscono ad area 1, 16% è NA, gli altri si dividono abbastanza equamente

table(df_clean$operazione) 
summary(df_clean$operazione) ##118981 NA
operazione_distribution <- as.data.frame(table(df_clean$operazione)) 
names(operazione_distribution) <- c("operazione", "Count") 
# Calcola la proporzione per ogni categoria della operazione
operazione_distribution$operazione <- as.numeric(as.character(operazione_distribution$operazione))
operazione_distribution <- operazione_distribution[order(operazione_distribution$operazione), ]
# Istogramma della distribuzione normalizzata delle categorie dell' operazione 
operazione_distribution$Proportion <- operazione_distribution$Count/sum(operazione_distribution$Count)
ggplot(operazione_distribution, aes(x = operazione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Operazione Categories", x = "Operazione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###le operazioni da 1 a 10 sono le più eseguite, a parte 7/8 che sono inesistenti

table(df_clean$regione_des_ana) 
summary(df_clean$regione_des_ana) 
regione_des_ana_distribution <- as.data.frame(table(df_clean$regione_des_ana)) 
names(regione_des_ana_distribution) <- c("regione", "Count") 
regione_des_ana_distribution$regione <- as.numeric(as.character(regione_des_ana_distribution$regione))
regione_des_ana_distribution <- regione_des_ana_distribution[order(regione_des_ana_distribution$regione),]
# Calcola la proporzione per ogni categoria della Regione
regione_des_ana_distribution$Proportion <- regione_des_ana_distribution$Count / sum(regione_des_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(regione_des_ana_distribution, aes(x = regione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Regione Categories", x = "regione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###metà dei record fanno riferimento a Regione 1 (Lombardia?)

table(df_clean$sistema_operativo) 
summary(df_clean$sistema_operativo) 
sistema_operativo_distribution <- as.data.frame(table(df_clean$sistema_operativo)) 
names(sistema_operativo_distribution) <- c("sistema_operativo", "Count") 
sistema_operativo_distribution$sistema_operativo <- as.numeric(as.character(sistema_operativo_distribution$sistema_operativo))
sistema_operativo_distribution <- sistema_operativo_distribution[order(sistema_operativo_distribution$sistema_operativo),]
# Calcola la proporzione per ogni categoria del sistema_operativo 
sistema_operativo_distribution$Proportion <- sistema_operativo_distribution$Count / sum(sistema_operativo_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sistema_operativo_distribution, aes(x = sistema_operativo, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sistema_operativo Categories", x = "sistema_operativo", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi tutti i record fanno riferimento ai primi 8 livelli

table(df_clean$direzione) 
summary(df_clean$direzione) ##circa 18000 NA
direzione_distribution <- as.data.frame(table(df_clean$direzione)) 
names(direzione_distribution) <- c("direzione", "Count") 
direzione_distribution$direzione <- as.numeric(as.character(direzione_distribution$direzione))
direzione_distribution <- direzione_distribution[order(direzione_distribution$direzione),]
# Calcola la proporzione per ogni categoria del direzione 
direzione_distribution$Proportion <- direzione_distribution$Count / sum(direzione_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del direzione 
ggplot(direzione_distribution, aes(x = direzione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of direzione Categories", x = "direzione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###distribuzione omogenea nei primi 9 livelli con picchi per 1 e 18, i livelli 10/11 irrisori

table(df_clean$filiale) 
summary(df_clean$filiale) ###circa 18000 NA
filiale_distribution <- as.data.frame(table(df_clean$filiale)) 
names(filiale_distribution) <- c("filiale", "Count") 
# Calcola la proporzione per ogni categoria del filiale 
filiale_distribution$Proportion <- filiale_distribution$Count / sum(filiale_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(filiale_distribution, aes(x = filiale, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of filiale Categories", x = "filiale", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###15% delle osservazioni concentrate nel livello 1, 1368 filiali?

table(df_clean$email) ###circa 18000 NA

table(df_clean$customer_segment) #11 LIVELLI
summary(df_clean$customer_segment) #18000 NA
customer_segment_distribution <- as.data.frame(table(df_clean$customer_segment)) 
names(customer_segment_distribution) <- c("customer_segment", "Count") 
# Calcola la proporzione per ogni categoria del customer_segment 
customer_segment_distribution$Proportion <- customer_segment_distribution$Count / sum(customer_segment_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(customer_segment_distribution, aes(x = customer_segment, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of customer_segment Categories", x = "customer_segment", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###le osservazioni si distribuiscono tra il segmento 1 (67%) e 2 (22%)

table(df_clean$attivita_eco_des_ana) 
summary(df_clean$attivita_eco_des_ana) ##CIRCA 18000 NA 
attivita_eco_des_ana_distribution <- as.data.frame(table(df_clean$attivita_eco_des_ana)) 
names(attivita_eco_des_ana_distribution) <- c("attivita_eco_des_ana", "Count") 
# Calcola la proporzione per ogni categoria del attivita_eco_des_ana 
attivita_eco_des_ana_distribution$Proportion <- attivita_eco_des_ana_distribution$Count / sum(attivita_eco_des_ana_distribution$Count)
attivita_eco_des_ana_distribution <- attivita_eco_des_ana_distribution[order(-attivita_eco_des_ana_distribution$Proportion), ]
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(attivita_eco_des_ana_distribution, aes(x = attivita_eco_des_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of attivita_eco_des_ana Categories", x = "attivita_eco_des_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###180% di osservazioni si dividono tra 1,18, 287,291,296 poi tutti gli altri con molti valori singoli

table(df_clean$sesso_code_ana) 
summary(df_clean$sesso_code_ana) ##CIRCA 6000 NA
sesso_code_ana_distribution <- as.data.frame(table(df_clean$sesso_code_ana)) 
names(sesso_code_ana_distribution) <- c("sesso_code_ana", "Count") 
# Calcola la proporzione per ogni categoria del sesso_code_ana 
sesso_code_ana_distribution$Proportion <- sesso_code_ana_distribution$Count / sum(sesso_code_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sesso_code_ana_distribution, aes(x = sesso_code_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sesso_code_ana Categories", x = "sesso_code_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###63% di livello 1, 36% di livello 2--> quale è maschio/femmina?

table(df_clean$fascia_eta_code_ana) 
summary(df_clean$fascia_eta_code_ana) ##CIRCA 6000 NA
fascia_eta_code_ana_distribution <- as.data.frame(table(df_clean$fascia_eta_code_ana)) 
names(fascia_eta_code_ana_distribution) <- c("fascia_eta_code_ana", "Count") 
# Calcola la proporzione per ogni categoria del fascia_eta_code_ana 
fascia_eta_code_ana_distribution$Proportion <- fascia_eta_code_ana_distribution$Count / sum(fascia_eta_code_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(fascia_eta_code_ana_distribution, aes(x = fascia_eta_code_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of fascia_eta_code_ana Categories", x = "fascia_eta_code_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###più dell'90% delle osservazioni si trova nei primi 5 segmenti

table(df_clean$xav_profilo_postazione) 
summary(df_clean$xav_profilo_postazione) ##CIRCA 281823 NA
xav_profilo_postazione_distribution <- as.data.frame(table(df_clean$xav_profilo_postazione)) 
names(xav_profilo_postazione_distribution) <- c("xav_profilo_postazione", "Count") 
# Calcola la proporzione per ogni categoria del xav_profilo_postazione 
xav_profilo_postazione_distribution$Proportion <- xav_profilo_postazione_distribution$Count / sum(xav_profilo_postazione_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(xav_profilo_postazione_distribution, aes(x = xav_profilo_postazione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of xav_profilo_postazione Categories", x = "xav_profilo_postazione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###questa variabile presenta NA per tutti i valori per NPS, però presenta
### circa 3800 valori: 57% livello 3, 26% livello 1, 118% livello 2 e 2% livello 18


table(df_clean$vdbank_flg_ana)
summary(df_clean$vdbank_flg_ana)
###18000 NA, 67% valori 2, 33% valori 1

table(df_clean$xntweb_flg_ana)
summary(df_clean$xntweb_flg_ana)
###10000 NA, 90% valori 1

table(df_clean$cliente_con_mutuo_flg_ana)
summary(df_clean$cliente_con_mutuo_flg_ana)
###2500 NA, 82% livello 2, 18% 1

table(df_clean$cliente_investitore_flg_ana)
summary(df_clean$cliente_investitore_flg_ana)
###pochi NA, 67% di livello 2, 33 di livello 1

table(df_clean$cliente_solo_cc_flg_ana)
summary(df_clean$cliente_solo_cc_flg_ana)
###pochi NA, quasi tutte osservazioni di livello 2

table(df_clean$in_bonis_flg_comm)
summary(df_clean$in_bonis_flg_comm)
###18000 NA, 518 di valore 2, gli altri 1


table(df_clean$multi_flg_comm)
summary(df_clean$multi_flg_comm)
###17000 NA, gli altri 50/50

table(df_clean$risk_rating_comm)
summary(df_clean$risk_rating_comm) ##circa 27000 NA
risk_rating_comm_distribution <- as.data.frame(table(df_clean$risk_rating_comm)) 
names(risk_rating_comm_distribution) <- c("risk_rating_comm", "Count") 
# Calcola la proporzione per ogni categoria del risk_rating_comm 
risk_rating_comm_distribution$Proportion <- risk_rating_comm_distribution$Count / sum(risk_rating_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(risk_rating_comm_distribution, aes(x = risk_rating_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of risk_rating_comm Categories", x = "risk_rating_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###93% delle osservazioni presentano valori tra 1 e 7

table(df_clean$cs_abi_num_comm)
summary(df_clean$cs_abi_num_comm) ##15830 NA
cs_abi_num_comm_distribution <- as.data.frame(table(df_clean$cs_abi_num_comm)) 
names(cs_abi_num_comm_distribution) <- c("cs_abi_num_comm", "Count") 
# Calcola la proporzione per ogni categoria del cs_abi_num_comm 
cs_abi_num_comm_distribution$Proportion <- cs_abi_num_comm_distribution$Count / sum(cs_abi_num_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(cs_abi_num_comm_distribution, aes(x = cs_abi_num_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of cs_abi_num_comm Categories", x = "cs_abi_num_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###distribuzione abbastanza regolare, vale indagare su questi valori

table(df_clean$fascia_tr_code_comm)
summary(df_clean$fascia_tr_code_comm) ##252018 NA
fascia_tr_code_comm_distribution <- as.data.frame(table(df_clean$fascia_tr_code_comm)) 
names(fascia_tr_code_comm_distribution) <- c("fascia_tr_code_comm", "Count") 
# Calcola la proporzione per ogni categoria del fascia_tr_code_comm 
fascia_tr_code_comm_distribution$Proportion <- fascia_tr_code_comm_distribution$Count / sum(fascia_tr_code_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(fascia_tr_code_comm_distribution, aes(x = fascia_tr_code_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of fascia_tr_code_comm Categories", x = "fascia_tr_code_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###gran parte NA, i pochi valori si distribuiscono tra i livelli 1 e 2 --> a cosa corrispondono?

table(df_clean$fascia_utilizzo_online_comm)
summary(df_clean$fascia_utilizzo_online_comm)
###95% di livello 1, poi circa 3000 NA, i pochi rimanenti su 2 e 3

table(df_clean$need_satisfied)
summary(df_clean$need_satisfied) ##28350 NA
need_satisfied_distribution <- as.data.frame(table(df_clean$need_satisfied)) 
names(need_satisfied_distribution) <- c("need_satisfied", "Count") 
# Calcola la proporzione per ogni categoria del need_satisfied 
need_satisfied_distribution$Proportion <- need_satisfied_distribution$Count / sum(need_satisfied_distribution$Count)
###83% di livello 1, il resto 2

table(df_clean$easy_support)
summary(df_clean$easy_support) ##28350 NA
easy_support_distribution <- as.data.frame(table(df_clean$easy_support)) 
names(easy_support_distribution) <- c("easy_support", "Count") 
# Calcola la proporzione per ogni categoria del easy_support 
easy_support_distribution$Proportion <- easy_support_distribution$Count / sum(easy_support_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(easy_support_distribution, aes(x = easy_support, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of easy_support Categories", x = "easy_support", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi il 50% in 9/10, 30% in 7/8, il restante 20% nei voti minori


table(df_clean$reason_for_satisfaction)
summary(df_clean$reason_for_satisfaction) 
###circa 30000 gli NA

table(df_clean$suggestions_to_improve)
summary(df_clean$suggestions_to_improve) 
### solo 116 hanno suggerito miglioramenti

table(df_clean$reason_for_dissatisfaction)
summary(df_clean$reason_for_dissatisfaction) 
###31840 NA, solo 446 hanno espresso motivazioni

####exploratory analysis survey 1 ####
cols_1 <- names(survey_1)
survey_1 %>% 
  select(all_of(cols_1)) %>% 
  map(function(x) n_distinct(x, na.rm = TRUE))

names(survey_1)
dim(survey_1) #3936 righe e 39 variabili

summary(survey_1$ces)
table(survey_1$ces) 
ces_distribution <- as.data.frame(table(survey_1$ces)) 
names(ces_distribution) <- c("ces", "Count") 
# Calcola la proporzione per ogni categoria del ces 
ces_distribution$Proportion <- ces_distribution$Count / sum(ces_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del ces 
ggplot(ces_distribution, aes(x = ces, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  
  labs(title = "Normalized Distribution of ces Categories", x = "ces", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###70% delle survey hanno score maggiore di 7

summary(survey_1$satisfaction) 
table(survey_1$satisfaction) 
hist(survey_1$satisfaction) 
boxplot(survey_1$satisfaction) 
daily_mean_satisfaction <- survey_1 %>%   
  group_by(local_response_date) %>%   
  summarise(mean_satisfaction = mean(satisfaction, na.rm = TRUE)) 
ggplot(survey_1, aes(y = satisfaction)) +   
  geom_boxplot(fill = "lightgreen") +   
  labs(title = "Boxplot of Daily Mean Satisfaction", y = "Daily Mean Satisfaction") +   
  theme_minimal()
###75% delle survey hanno score maggiore di 8

summary(survey_1$browser)
table(survey_1$browser) 
browser_distribution <- as.data.frame(table(survey_1$browser)) 
names(browser_distribution) <- c("browser", "Count") 
# Calcola la proporzione per ogni categoria del browser 
browser_distribution$Proportion <- browser_distribution$Count / sum(browser_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del browser 
ggplot(browser_distribution, aes(x = browser, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  
  labs(title = "Normalized Distribution of Browser Categories", x = "Browser", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi tutte le survey si riferiscono a 18 browsers (1,2,3,5)


table(survey_1$anzianita_anni_num) 
summary(survey_1$anzianita_anni_num) 
years_distribution <- as.data.frame(table(survey_1$anzianita_anni_num)) 
names(years_distribution) <- c("Years", "Count") 
# Calcola la proporzione per ogni categoria del browser 
years_distribution$Proportion <- years_distribution$Count / sum(years_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del browser 
ggplot(years_distribution, aes(x = Years, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of years Categories", x = "Years", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###75% di clienti con meno di 13 anni, qualche outlier oltre i 100 e 77 NA

table(survey_1$area) 
summary(survey_1$area) 
area_distribution <- as.data.frame(table(survey_1$area)) 
names(area_distribution) <- c("area", "Count") 
# Calcola la proporzione per ogni categoria del area 
area_distribution$Proportion <- area_distribution$Count / sum(area_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(area_distribution, aes(x = area, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Area Categories", x = "Area", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi tutti i record si riferiscono ad area 1

table(survey_1$operazione) ####tutti operazione 1

table(survey_1$regione_des_ana) 
summary(survey_1$regione_des_ana) 
regione_des_ana_distribution <- as.data.frame(table(survey_1$regione_des_ana)) 
names(regione_des_ana_distribution) <- c("Regione", "Count") 
# Calcola la proporzione per ogni categoria della Regione
regione_des_ana_distribution$Proportion <- regione_des_ana_distribution$Count / sum(regione_des_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(regione_des_ana_distribution, aes(x = Regione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Regione Categories", x = "Regione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi tutti i record fanno riferimento a Regione 1 (Lombardia?)

table(survey_1$sistema_operativo) 
summary(survey_1$sistema_operativo) 
sistema_operativo_distribution <- as.data.frame(table(survey_1$sistema_operativo)) 
names(sistema_operativo_distribution) <- c("sistema_operativo", "Count") 
# Calcola la proporzione per ogni categoria del sistema_operativo 
sistema_operativo_distribution$Proportion <- sistema_operativo_distribution$Count / sum(sistema_operativo_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sistema_operativo_distribution, aes(x = sistema_operativo, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sistema_operativo Categories", x = "sistema_operativo", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi tutti i record fanno riferimento ai livelli 1,3,18,8

table(survey_1$direzione) 
summary(survey_1$direzione) 
direzione_distribution <- as.data.frame(table(survey_1$direzione)) 
names(direzione_distribution) <- c("direzione", "Count") 
# Calcola la proporzione per ogni categoria del direzione 
direzione_distribution$Proportion <- direzione_distribution$Count / sum(direzione_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(direzione_distribution, aes(x = direzione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of direzione Categories", x = "direzione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###11 livelli, quasi tutte concentrati sul livello 1 

table(survey_1$filiale) 
summary(survey_1$filiale) 
filiale_distribution <- as.data.frame(table(survey_1$filiale)) 
names(filiale_distribution) <- c("filiale", "Count") 
# Calcola la proporzione per ogni categoria del filiale 
filiale_distribution$Proportion <- filiale_distribution$Count / sum(filiale_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(filiale_distribution, aes(x = filiale, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of filiale Categories", x = "filiale", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###72% delle osservazioni concentrate nel livello 1, poi in 8 e 152, poi un paio di record per filiale

table(survey_1$email) 
###tutti i record presentano il valore email cliente

table(survey_1$customer_segment) 
summary(survey_1$customer_segment) 
customer_segment_distribution <- as.data.frame(table(survey_1$customer_segment)) 
names(customer_segment_distribution) <- c("customer_segment", "Count") 
# Calcola la proporzione per ogni categoria del customer_segment 
customer_segment_distribution$Proportion <- customer_segment_distribution$Count / sum(customer_segment_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(customer_segment_distribution, aes(x = customer_segment, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of customer_segment Categories", x = "customer_segment", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###le osservazioni si distribuiscono tra il segmento 1 (82%) e 2 (18%)

table(survey_1$attivita_eco_des_ana) 
summary(survey_1$attivita_eco_des_ana) 
attivita_eco_des_ana_distribution <- as.data.frame(table(survey_1$attivita_eco_des_ana)) 
names(attivita_eco_des_ana_distribution) <- c("attivita_eco_des_ana", "Count") 
# Calcola la proporzione per ogni categoria del attivita_eco_des_ana 
attivita_eco_des_ana_distribution$Proportion <- attivita_eco_des_ana_distribution$Count / sum(attivita_eco_des_ana_distribution$Count)
attivita_eco_des_ana_distribution <- attivita_eco_des_ana_distribution[order(-attivita_eco_des_ana_distribution$Proportion), ]
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(attivita_eco_des_ana_distribution, aes(x = attivita_eco_des_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of attivita_eco_des_ana Categories", x = "attivita_eco_des_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###20% di osservazioni per livello 1, 518% per il livello 18, 7% per 82 e poi tutti gli altri

table(survey_1$sesso_code_ana) 
summary(survey_1$sesso_code_ana) 
sesso_code_ana_distribution <- as.data.frame(table(survey_1$sesso_code_ana)) 
names(sesso_code_ana_distribution) <- c("sesso_code_ana", "Count") 
# Calcola la proporzione per ogni categoria del sesso_code_ana 
sesso_code_ana_distribution$Proportion <- sesso_code_ana_distribution$Count / sum(sesso_code_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sesso_code_ana_distribution, aes(x = sesso_code_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sesso_code_ana Categories", x = "sesso_code_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###63% di livello 1, 36% di livello 2--> quale è maschio/femmina?

table(survey_1$fascia_eta_code_ana) 
summary(survey_1$fascia_eta_code_ana) 
fascia_eta_code_ana_distribution <- as.data.frame(table(survey_1$fascia_eta_code_ana)) 
names(fascia_eta_code_ana_distribution) <- c("fascia_eta_code_ana", "Count") 
# Calcola la proporzione per ogni categoria del fascia_eta_code_ana 
fascia_eta_code_ana_distribution$Proportion <- fascia_eta_code_ana_distribution$Count / sum(fascia_eta_code_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(fascia_eta_code_ana_distribution, aes(x = fascia_eta_code_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of fascia_eta_code_ana Categories", x = "fascia_eta_code_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###più dell'80% delle osservazioni si trova nei primi 5 segmenti

table(survey_1$vdbank_flg_ana) 
###31823 osservazioni di livello 1, le altri mancanti

table(survey_1$xntweb_flg_ana)
###577 osservazioni di livello 1, le altri mancanti

table(survey_1$cliente_con_mutuo_flg_ana)
###3595 osservazioni di livello 2, le altri con livello 1--> si/no

table(survey_1$cliente_investitore_flg_ana)
###3070 osservazioni di livello 2, 865 di livello 1 --> si/no

table(survey_1$cliente_solo_cc_flg_ana)
###91 osservazioni livello 1, le altre di livello 2 --> si/no

table(survey_1$in_bonis_flg_comm)
###tutti valori 1 --> azienda con posizione finanziaria positiva


table(survey_1$multi_flg_comm)
###1883 osservazioni di valore 1, le altre mancanti

table(survey_1$risk_rating_comm)
summary(survey_1$risk_rating_comm) #3700 NA
risk_rating_comm_distribution <- as.data.frame(table(survey_1$risk_rating_comm)) 
names(risk_rating_comm_distribution) <- c("risk_rating_comm", "Count") 
# Calcola la proporzione per ogni categoria del risk_rating_comm 
risk_rating_comm_distribution$Proportion <- risk_rating_comm_distribution$Count / sum(risk_rating_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(risk_rating_comm_distribution, aes(x = risk_rating_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of risk_rating_comm Categories", x = "risk_rating_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###le osservazioni presentano valori tra 1 e 7 --> basso rischio?

table(survey_1$cs_abi_num_comm)
summary(survey_1$cs_abi_num_comm) 
cs_abi_num_comm_distribution <- as.data.frame(table(survey_1$cs_abi_num_comm)) 
names(cs_abi_num_comm_distribution) <- c("cs_abi_num_comm", "Count") 
# Calcola la proporzione per ogni categoria del cs_abi_num_comm 
cs_abi_num_comm_distribution$Proportion <- cs_abi_num_comm_distribution$Count / sum(cs_abi_num_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(cs_abi_num_comm_distribution, aes(x = cs_abi_num_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of cs_abi_num_comm Categories", x = "cs_abi_num_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###distribuzione su 10 valori asimmetrica verso sinistra

table(survey_1$fascia_tr_code_comm)
summary(survey_1$fascia_tr_code_comm) #3701 NA
fascia_tr_code_comm_distribution <- as.data.frame(table(survey_1$fascia_tr_code_comm)) 
names(fascia_tr_code_comm_distribution) <- c("fascia_tr_code_comm", "Count") 
# Calcola la proporzione per ogni categoria del fascia_tr_code_comm 
fascia_tr_code_comm_distribution$Proportion <- fascia_tr_code_comm_distribution$Count / sum(fascia_tr_code_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(fascia_tr_code_comm_distribution, aes(x = fascia_tr_code_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of fascia_tr_code_comm Categories", x = "fascia_tr_code_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###gran parte NA, i pochi valori si distribuiscono tra i livelli 1 e 2 --> a cosa corrispondono?

table(survey_1$fascia_utilizzo_online_comm)
###circa l'86% di livello 1, poi circa 500 NA, i pochi rimanenti su 2 e 3

table(survey_1$need_satisfied)
summary(survey_1$need_satisfied) 
need_satisfied_distribution <- as.data.frame(table(survey_1$need_satisfied)) 
names(need_satisfied_distribution) <- c("need_satisfied", "Count") 
# Calcola la proporzione per ogni categoria del need_satisfied 
need_satisfied_distribution$Proportion <- need_satisfied_distribution$Count / sum(need_satisfied_distribution$Count)
###85% di livello 1 e i restanti di livello 2 --> si/no

table(survey_1$easy_support)
summary(survey_1$easy_support) 
easy_support_distribution <- as.data.frame(table(survey_1$easy_support)) 
names(easy_support_distribution) <- c("easy_support", "Count") 
# Calcola la proporzione per ogni categoria del easy_support 
easy_support_distribution$Proportion <- easy_support_distribution$Count / sum(easy_support_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(easy_support_distribution, aes(x = easy_support, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of easy_support Categories", x = "easy_support", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi il 50% in 9/10, 30% in 7/8, il restante 20% nei voti minori

table(survey_1$service_satisfaction)
summary(survey_1$service_satisfaction) 
service_satisfaction_distribution <- as.data.frame(table(survey_1$service_satisfaction)) 
names(service_satisfaction_distribution) <- c("service_satisfaction", "Count") 
# Calcola la proporzione per ogni categoria del service_satisfaction 
service_satisfaction_distribution$Proportion <- service_satisfaction_distribution$Count / sum(service_satisfaction_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(service_satisfaction_distribution, aes(x = service_satisfaction, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of service_satisfaction Categories", x = "service_satisfaction", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###60% 9/10, 23% da 7/8, i restanti al di sotto

table(survey_1$reason_for_satisfaction)
summary(survey_1$reason_for_satisfaction) 
###circa 50/50 con gli NA

table(survey_1$suggestions_to_improve)
summary(survey_1$suggestions_to_improve) 
### solo 116 hanno suggerito miglioramenti

table(survey_1$reason_for_dissatisfaction)
summary(survey_1$reason_for_dissatisfaction) 
### solo 18186 hanno fornito motivazioni per l'insoddifazione


####exploratory analysis survey 2 ####

cols_2 <- names(survey_2)
survey_2 %>% 
  select(all_of(cols_2)) %>% 
  map(function(x) n_distinct(x, na.rm = TRUE))

names(survey_2)
dim(survey_2) #3863 righe, 318 variabili

summary(survey_2$ces)
table(survey_2$ces) 
ces_distribution <- as.data.frame(table(survey_2$ces)) 
names(ces_distribution) <- c("ces", "Count") 
# Calcola la proporzione per ogni categoria del ces 
ces_distribution$Proportion <- ces_distribution$Count / sum(ces_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del ces 
ggplot(ces_distribution, aes(x = ces, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  
  labs(title = "Normalized Distribution of ces Categories", x = "ces", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
#quasi tutti passive o promoters

summary(survey_2$satisfaction) 
table(survey_2$satisfaction) 
###all zeros 

summary(survey_2$browser)
table(survey_2$browser) 
browser_distribution <- as.data.frame(table(survey_2$browser)) 
names(browser_distribution) <- c("browser", "Count") 
# Calcola la proporzione per ogni categoria del browser 
browser_distribution$Proportion <- browser_distribution$Count / sum(browser_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del browser 
ggplot(browser_distribution, aes(x = browser, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  
  labs(title = "Normalized Distribution of Browser Categories", x = "Browser", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi tutte i record si riferiscono a 18 browsers (1 - 53%,2 - 21%,3 - 12%,18 - 11%)


table(survey_2$anzianita_anni_num) 
summary(survey_2$anzianita_anni_num) 
years_distribution <- as.data.frame(table(survey_2$anzianita_anni_num)) 
names(years_distribution) <- c("Years", "Count") 
# Calcola la proporzione per ogni categoria del browser 
years_distribution$Proportion <- years_distribution$Count / sum(years_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del browser 
ggplot(years_distribution, aes(x = Years, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of years Categories", x = "Years", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###distribuzione a tratti omogenea con picco a 11 anni

table(survey_2$area) 
summary(survey_2$area) ##3709 NA
area_distribution <- as.data.frame(table(survey_2$area)) 
names(area_distribution) <- c("area", "Count") 
area_distribution$area <- as.numeric(as.character(area_distribution$area))
area_distribution <- area_distribution[order(area_distribution$area),]
# Calcola la proporzione per ogni categoria del area 
area_distribution$Proportion <- area_distribution$Count / sum(area_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(area_distribution, aes(x = area, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Area Categories", x = "Area", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###distribuzione eterogenea, valore maggiore area 2 con 7%

names(survey_2)
table(survey_2$app_knowledge) ####67% 1, 33% 2
summary(survey_2$app_knowledge)

table(survey_2$regione_des_ana) 
summary(survey_2$regione_des_ana) #3709 NA
regione_des_ana_distribution <- as.data.frame(table(survey_2$regione_des_ana)) 
names(regione_des_ana_distribution) <- c("regione", "Count") 
regione_des_ana_distribution$regione <- as.numeric(as.character(regione_des_ana_distribution$regione))
regione_des_ana_distribution <- regione_des_ana_distribution[order(regione_des_ana_distribution$regione),]
# Calcola la proporzione per ogni categoria della Regione
regione_des_ana_distribution$Proportion <- regione_des_ana_distribution$Count / sum(regione_des_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(regione_des_ana_distribution, aes(x = regione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Regione Categories", x = "Regione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###più del 180% dei record fanno riferimento a Regione 1, poi 2,3,6,8

table(survey_2$sistema_operativo) 
summary(survey_2$sistema_operativo) 
sistema_operativo_distribution <- as.data.frame(table(survey_2$sistema_operativo)) 
names(sistema_operativo_distribution) <- c("sistema_operativo", "Count") 
# Calcola la proporzione per ogni categoria del sistema_operativo 
sistema_operativo_distribution$Proportion <- sistema_operativo_distribution$Count / sum(sistema_operativo_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sistema_operativo_distribution, aes(x = sistema_operativo, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sistema_operativo Categories", x = "sistema_operativo", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###1818% livello 1, 22% livello 7, 20% livello 5, basse percentuali per 2,18,6; gli altri minime

table(survey_2$direzione) 
summary(survey_2$direzione) #3709 NA
direzione_distribution <- as.data.frame(table(survey_2$direzione)) 
names(direzione_distribution) <- c("direzione", "Count") 
# Calcola la proporzione per ogni categoria del direzione 
direzione_distribution$Proportion <- direzione_distribution$Count / sum(direzione_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(direzione_distribution, aes(x = direzione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of direzione Categories", x = "direzione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi tutti NA, i pochi si distribuiscono nei livelli compresi tra 2 e 9

table(survey_2$filiale) 
summary(survey_2$filiale) #3709 NA
filiale_distribution <- as.data.frame(table(survey_2$filiale)) 
names(filiale_distribution) <- c("filiale", "Count") 
# Calcola la proporzione per ogni categoria del filiale 
filiale_distribution$Proportion <- filiale_distribution$Count / sum(filiale_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(filiale_distribution, aes(x = filiale, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of filiale Categories", x = "filiale", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###pochissimi record per filiale

table(survey_2$email) 
summary(survey_2$email)
###153 email Cliente, 3710 NA 

table(survey_2$customer_segment) 
summary(survey_2$customer_segment) #3709 NA
customer_segment_distribution <- as.data.frame(table(survey_2$customer_segment)) 
names(customer_segment_distribution) <- c("customer_segment", "Count") 
# Calcola la proporzione per ogni categoria del customer_segment 
customer_segment_distribution$Proportion <- customer_segment_distribution$Count / sum(customer_segment_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(customer_segment_distribution, aes(x = customer_segment, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of customer_segment Categories", x = "customer_segment", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###188% livello 1, 15% livello 2, 26% livello 3, 5% livello 18 e 18% livello 10

table(survey_2$xav_profilo_postazione) 
summary(survey_2$xav_profilo_postazione) #nessun NA
xav_profilo_postazione_distribution <- as.data.frame(table(survey_2$xav_profilo_postazione)) 
names(xav_profilo_postazione_distribution) <- c("xav_profilo_postazione", "Count") 
# Calcola la proporzione per ogni categoria del xav_profilo_postazione 
xav_profilo_postazione_distribution$Proportion <- xav_profilo_postazione_distribution$Count / sum(xav_profilo_postazione_distribution$Count)
xav_profilo_postazione_distribution <- xav_profilo_postazione_distribution[order(-xav_profilo_postazione_distribution$Proportion), ]
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(xav_profilo_postazione_distribution, aes(x = xav_profilo_postazione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of xav_profilo_postazione Categories", x = "xav_profilo_postazione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###57% di osservazioni per livello 3, 2% per il livello 18, 118% per 2 e 26% per livello 1


table(survey_2$banca_mcf_xav_des) 
summary(survey_2$banca_mcf_xav_des) #9 NA, gli altri 38518 sono Ragione Sociale Cliente


table(survey_2$ces_factors_transfer_intercept) 
summary(survey_2$ces_factors_transfer_intercept) #3780 NA
ces_factors_transfer_intercept_distribution <- as.data.frame(table(survey_2$ces_factors_transfer_intercept)) 
names(ces_factors_transfer_intercept_distribution) <- c("ces_factors_transfer_intercept", "Count") 
# Calcola la proporzione per ogni categoria del ces_factors_transfer_intercept 
ces_factors_transfer_intercept_distribution$Proportion <- ces_factors_transfer_intercept_distribution$Count / sum(ces_factors_transfer_intercept_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(ces_factors_transfer_intercept_distribution, aes(x = ces_factors_transfer_intercept, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of ces_factors_transfer_intercept Categories", x = "ces_factors_transfer_intercept", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###38% livello 5, 18% livello 18, 8% livello 6, gli altri percentuali basse

table(survey_2$xa_app_not_used_reason) 
summary(survey_2$xa_app_not_used_reason) #2385 NA
xa_app_not_used_reason_distribution <- as.data.frame(table(survey_2$xa_app_not_used_reason)) 
names(xa_app_not_used_reason_distribution) <- c("xa_app_not_used_reason", "Count") 
# Calcola la proporzione per ogni categoria del xa_app_not_used_reason 
xa_app_not_used_reason_distribution$Proportion <- xa_app_not_used_reason_distribution$Count / sum(xa_app_not_used_reason_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(xa_app_not_used_reason_distribution, aes(x = xa_app_not_used_reason, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of xa_app_not_used_reason Categories", x = "xa_app_not_used_reason", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###55% livello 1, 37% livello 2, pochi record per i livelli 3,18,5


table(survey_2$xa_app_not_used_reason_other)
summary(survey_2$xa_app_not_used_reason_other)
###180 osservazioni con other, 3823 NA

table(survey_2$cliente_con_mutuo_flg_ana)
summary(survey_2$cliente_con_mutuo_flg_ana)
###1616 livello 1, 2239 livello 2, 8 NA

table(survey_2$cliente_investitore_flg_ana)
summary(survey_2$cliente_investitore_flg_ana)
###solo livello 2 e 8 NA

table(survey_2$cliente_solo_cc_flg_ana)
summary(survey_2$cliente_solo_cc_flg_ana)
###solo livello 2 e 8 NA

table(survey_2$in_bonis_flg_comm)
summary(survey_2$in_bonis_flg_comm)
###1189 livello 1, 5 di livello 2 e 3709 NA


table(survey_2$app_used)
summary(survey_2$app_used)
###1331 osservazioni di valore 1, 11878 livello 2

table(survey_2$risk_rating_comm)
summary(survey_2$risk_rating_comm) #3875 NA
risk_rating_comm_distribution <- as.data.frame(table(survey_2$risk_rating_comm)) 
names(risk_rating_comm_distribution) <- c("risk_rating_comm", "Count") 
# Calcola la proporzione per ogni categoria del risk_rating_comm 
risk_rating_comm_distribution$Proportion <- risk_rating_comm_distribution$Count / sum(risk_rating_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(risk_rating_comm_distribution, aes(x = risk_rating_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of risk_rating_comm Categories", x = "risk_rating_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###distribuzione abbastanza omogenea su 11 livelli con meno valori verso i livelli più alti

table(survey_2$fascia_tr_code_comm)
summary(survey_2$fascia_tr_code_comm) #3525 NA
###161 livello 1, 161 livello 2 e 16 livello 3

table(survey_2$fascia_utilizzo_online_comm)
summary(survey_2$fascia_utilizzo_online_comm)
### 29918 livello 1, 686 livello 2, 11818 livello 3 e 39 NA

table(survey_2$easy_support)
summary(survey_2$easy_support) 
### tutti NA --> non viene eliminata dai precedenti filtri perché assumono valore 0

table(survey_2$service_satisfaction)
summary(survey_2$service_satisfaction) 
### tutti NA 

#### exploratory analysis survey 3 ####
cols_3 <- names(survey_3)
survey_3 %>% 
  select(all_of(cols_3)) %>% 
  map(function(x) n_distinct(x, na.rm = TRUE))

names(survey_3)
dim(survey_3) #13369 righe, 181 variabili
summary(survey_3$ces) 
table(survey_3$ces) 
### no CES per survey_3

summary(survey_3$nps) 
table(survey_3$nps) ## 859 NA
hist(survey_3$nps) 
boxplot(survey_3$nps) 
nps_distribution <- as.data.frame(table(survey_3$nps))
names(nps_distribution) <- c("nps", "Count") 
nps_distribution$nps <- as.numeric(as.character(nps_distribution$nps))
nps_distribution <- nps_distribution[nps_distribution$nps > 0,]
# Calcola la proporzione per ogni valore del nps 
nps_distribution$Proportion <- nps_distribution$Count / sum(nps_distribution$Count) 
daily_mean_nps <- survey_3 %>%   
  group_by(local_response_date) %>%   
  summarise(mean_nps = mean(nps, na.rm = TRUE)) 
ggplot(survey_3, aes(y = nps)) +   
  geom_boxplot(fill = "lightgreen") +   
  labs(title = "Boxplot of Daily Mean nps", y = "Daily Mean nps") +   
  theme_minimal()
###62% voto 9/10, 29% voto 7/8 e i restanti 6 o sotto

summary(survey_3$satisfaction) 
table(survey_3$satisfaction) ## 28350 NA
###tutti NA

summary(survey_3$browser) ##3 NA
table(survey_3$browser) 
browser_distribution <- as.data.frame(table(survey_3$browser)) 
names(browser_distribution) <- c("browser", "Count") 
# Calcola la proporzione per ogni categoria del browser 
browser_distribution$Proportion <- browser_distribution$Count / sum(browser_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del browser 
ggplot(browser_distribution, aes(x = browser, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +  
  labs(title = "Normalized Distribution of Browser Categories", x = "Browser", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###22% livello 1, 25% livello 2, 2% livello 3, 188% livello 5


table(survey_3$anzianita_anni_num) 
summary(survey_3$anzianita_anni_num) ##1088 NA
years_distribution <- as.data.frame(table(survey_3$anzianita_anni_num)) 
names(years_distribution) <- c("Years", "Count") 
# Calcola la proporzione per ogni categoria del browser 
years_distribution$Proportion <- years_distribution$Count / sum(years_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del browser 
ggplot(years_distribution, aes(x = Years, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of years Categories", x = "Years", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###83 outlier oltre i 100 --> nessun valore tra i 69 e i 123/1218

table(survey_3$area) 
summary(survey_3$area) 
area_distribution <- as.data.frame(table(survey_3$area)) 
names(area_distribution) <- c("area", "Count") 
# Calcola la proporzione per ogni categoria del area 
area_distribution$Proportion <- area_distribution$Count / sum(area_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(area_distribution, aes(x = area, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Area Categories", x = "Area", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###distribuzione varia su tutti i valori tranne agli estremi

table(survey_3$operazione) 
summary(survey_3$operazione) 
operazione_distribution <- as.data.frame(table(survey_3$operazione)) 
names(operazione_distribution) <- c("operazione", "Count") 
# Calcola la proporzione per ogni categoria della operazione
operazione_distribution$operazione <- as.numeric(as.character(operazione_distribution$operazione))
operazione_distribution <- operazione_distribution[order(operazione_distribution$operazione), ]
# Istogramma della distribuzione normalizzata delle categorie dell' operazione 
operazione_distribution$Proportion <- operazione_distribution$Count/sum(operazione_distribution$Count)
ggplot(operazione_distribution, aes(x = operazione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Operazione Categories", x = "Operazione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###operazioni da 2 a 10 sono le più frequenti (7/8 non presenti)

table(survey_3$regione_des_ana) 
summary(survey_3$regione_des_ana) 
regione_des_ana_distribution <- as.data.frame(table(survey_3$regione_des_ana)) 
names(regione_des_ana_distribution) <- c("regione", "Count") 
regione_des_ana_distribution$regione <- as.numeric(as.character(regione_des_ana_distribution$regione))
regione_des_ana_distribution <- regione_des_ana_distribution[order(regione_des_ana_distribution$regione),]
# Calcola la proporzione per ogni categoria della Regione
regione_des_ana_distribution$Proportion <- regione_des_ana_distribution$Count / sum(regione_des_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(regione_des_ana_distribution, aes(x = regione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Regione Categories", x = "regione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###la regione 1 è sempre la più frequente, 2,3,6 e 8 a seguire; da 10 in poi poche osservazioni

table(survey_3$sistema_operativo) 
summary(survey_3$sistema_operativo) #3 NA
sistema_operativo_distribution <- as.data.frame(table(survey_3$sistema_operativo)) 
names(sistema_operativo_distribution) <- c("sistema_operativo", "Count") 
sistema_operativo_distribution$sistema_operativo <- as.numeric(as.character(sistema_operativo_distribution$sistema_operativo))
sistema_operativo_distribution <- sistema_operativo_distribution[order(sistema_operativo_distribution$sistema_operativo),]
# Calcola la proporzione per ogni categoria del sistema_operativo 
sistema_operativo_distribution$Proportion <- sistema_operativo_distribution$Count / sum(sistema_operativo_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sistema_operativo_distribution, aes(x = sistema_operativo, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sistema_operativo Categories", x = "sistema_operativo", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###23% livello 1, 23 % livello 3, 188% livello 18, 3% livello 8

table(survey_3$direzione) 
summary(survey_3$direzione) # no NA
direzione_distribution <- as.data.frame(table(survey_3$direzione)) 
names(direzione_distribution) <- c("direzione", "Count") 
direzione_distribution$direzione <- as.numeric(as.character(direzione_distribution$direzione))
direzione_distribution <- direzione_distribution[order(direzione_distribution$direzione),]
# Calcola la proporzione per ogni categoria del direzione 
direzione_distribution$Proportion <- direzione_distribution$Count / sum(direzione_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del direzione 
ggplot(direzione_distribution, aes(x = direzione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of direzione Categories", x = "direzione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###distribuzione omogenea nei livelli da 2 a 9 con picco per 18, i livelli 1/10/11 nulli

table(survey_3$filiale) 
summary(survey_3$filiale) #other 10617
filiale_distribution <- as.data.frame(table(survey_3$filiale)) 
names(filiale_distribution) <- c("filiale", "Count") 
# Calcola la proporzione per ogni categoria del filiale 
filiale_distribution$Proportion <- filiale_distribution$Count / sum(filiale_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(filiale_distribution, aes(x = filiale, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of filiale Categories", x = "filiale", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
### tantissimi valori differenti

table(survey_3$email) ###ci sono tutti i 13369 valori
summary(survey_3$email)


table(survey_3$customer_segment) #11 LIVELLI
summary(survey_3$customer_segment) #no NA
customer_segment_distribution <- as.data.frame(table(survey_3$customer_segment)) 
names(customer_segment_distribution) <- c("customer_segment", "Count") 
# Calcola la proporzione per ogni categoria del customer_segment 
customer_segment_distribution$Proportion <- customer_segment_distribution$Count / sum(customer_segment_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(customer_segment_distribution, aes(x = customer_segment, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of customer_segment Categories", x = "customer_segment", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###63% livello 1, 23% livello 2, 6% livello 3, 5% livello 18, 1% livello 5

table(survey_3$attivita_eco_des_ana) 
summary(survey_3$attivita_eco_des_ana) ##93 NA 
attivita_eco_des_ana_distribution <- as.data.frame(table(survey_3$attivita_eco_des_ana)) 
names(attivita_eco_des_ana_distribution) <- c("attivita_eco_des_ana", "Count") 
# Calcola la proporzione per ogni categoria del attivita_eco_des_ana 
attivita_eco_des_ana_distribution$Proportion <- attivita_eco_des_ana_distribution$Count / sum(attivita_eco_des_ana_distribution$Count)
attivita_eco_des_ana_distribution <- attivita_eco_des_ana_distribution[order(-attivita_eco_des_ana_distribution$Proportion), ]
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(attivita_eco_des_ana_distribution, aes(x = attivita_eco_des_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of attivita_eco_des_ana Categories", x = "attivita_eco_des_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###20% di osservazioni per livello 18 e 18% livello 2, le altre si distribuiscono infinitesimalmente

table(survey_3$sesso_code_ana) 
summary(survey_3$sesso_code_ana) ##1706 NA
sesso_code_ana_distribution <- as.data.frame(table(survey_3$sesso_code_ana)) 
names(sesso_code_ana_distribution) <- c("sesso_code_ana", "Count") 
# Calcola la proporzione per ogni categoria del sesso_code_ana 
sesso_code_ana_distribution$Proportion <- sesso_code_ana_distribution$Count / sum(sesso_code_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sesso_code_ana_distribution, aes(x = sesso_code_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sesso_code_ana Categories", x = "sesso_code_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi 50/50 tra maschi e femmine

table(survey_3$fascia_eta_code_ana) 
summary(survey_3$fascia_eta_code_ana) #1706 NA
fascia_eta_code_ana_distribution <- as.data.frame(table(survey_3$fascia_eta_code_ana)) 
names(fascia_eta_code_ana_distribution) <- c("fascia_eta_code_ana", "Count") 
# Calcola la proporzione per ogni categoria del fascia_eta_code_ana 
fascia_eta_code_ana_distribution$Proportion <- fascia_eta_code_ana_distribution$Count / sum(fascia_eta_code_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(fascia_eta_code_ana_distribution, aes(x = fascia_eta_code_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of fascia_eta_code_ana Categories", x = "fascia_eta_code_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###tanti valori per le prime 3 fasce, alcuni per le fasce da 18 a 6 e pochi per 7 e 8

table(survey_3$vdbank_flg_ana)
summary(survey_3$vdbank_flg_ana)
###non presente

table(survey_3$xntweb_flg_ana)
summary(survey_3$xntweb_flg_ana)
###2908 NA, solo valori 1

table(survey_3$cliente_con_mutuo_flg_ana)
summary(survey_3$cliente_con_mutuo_flg_ana)
###2178 NA, 87% livello 2, 13% 1

table(survey_3$cliente_investitore_flg_ana)
summary(survey_3$cliente_investitore_flg_ana)
###506 NA, 67% di livello 2, 33% di livello 1

table(survey_3$cliente_solo_cc_flg_ana)
summary(survey_3$cliente_solo_cc_flg_ana)
###506 NA, quasi tutte osservazioni di livello 2, solo 172 di livello 1

table(survey_3$in_bonis_flg_comm)
summary(survey_3$in_bonis_flg_comm)
###13369 di valore 1

table(survey_3$multi_flg_comm)
summary(survey_3$multi_flg_comm)
###11167 NA, tutti valori 1

table(survey_3$risk_rating_comm)
summary(survey_3$risk_rating_comm) ##circa 27000 NA
risk_rating_comm_distribution <- as.data.frame(table(survey_3$risk_rating_comm)) 
names(risk_rating_comm_distribution) <- c("risk_rating_comm", "Count") 
# Calcola la proporzione per ogni categoria del risk_rating_comm 
risk_rating_comm_distribution$Proportion <- risk_rating_comm_distribution$Count / sum(risk_rating_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(risk_rating_comm_distribution, aes(x = risk_rating_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of risk_rating_comm Categories", x = "risk_rating_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###100% delle osservazioni presentano valori tra 1 e 7

table(survey_3$cs_abi_num_comm)
summary(survey_3$cs_abi_num_comm) ##8188 NA
cs_abi_num_comm_distribution <- as.data.frame(table(survey_3$cs_abi_num_comm)) 
names(cs_abi_num_comm_distribution) <- c("cs_abi_num_comm", "Count") 
# Calcola la proporzione per ogni categoria del cs_abi_num_comm 
cs_abi_num_comm_distribution$Proportion <- cs_abi_num_comm_distribution$Count / sum(cs_abi_num_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(cs_abi_num_comm_distribution, aes(x = cs_abi_num_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of cs_abi_num_comm Categories", x = "cs_abi_num_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###distribuzione abbastanza regolare, vale indagare su questi valori

table(survey_3$fascia_tr_code_comm)
summary(survey_3$fascia_tr_code_comm) ##81180 NA
fascia_tr_code_comm_distribution <- as.data.frame(table(survey_3$fascia_tr_code_comm)) 
names(fascia_tr_code_comm_distribution) <- c("fascia_tr_code_comm", "Count") 
# Calcola la proporzione per ogni categoria del fascia_tr_code_comm 
fascia_tr_code_comm_distribution$Proportion <- fascia_tr_code_comm_distribution$Count / sum(fascia_tr_code_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(fascia_tr_code_comm_distribution, aes(x = fascia_tr_code_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of fascia_tr_code_comm Categories", x = "fascia_tr_code_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###218185 valori 1, 25182 valori 2, 2182 valori 3

table(survey_3$fascia_utilizzo_online_comm)
summary(survey_3$fascia_utilizzo_online_comm)
###95% di livello 1, poi 1696 NA, i pochi rimanenti su 2 e 3

table(survey_3$need_satisfied)
summary(survey_3$need_satisfied)
###non presente per questa survey

table(survey_3$easy_support)
summary(survey_3$easy_support) 
###non presente per questa survey

names(survey_3)
table(survey_3$segmento_des_comm)
summary(survey_3$segmento_des_comm)
segmento_des_comm_distribution <- as.data.frame(table(survey_3$segmento_des_comm)) 
names(segmento_des_comm_distribution) <- c("segmento_des_comm", "Count") 
# Calcola la proporzione per ogni categoria del segmento_des_comm 
segmento_des_comm_distribution$Proportion <- segmento_des_comm_distribution$Count / sum(segmento_des_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(segmento_des_comm_distribution, aes(x = segmento_des_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of segmento_des_comm Categories", x = "segmento_des_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###oltre 60% per livello 1, no 2, poi 3, 18,5,6 con alcune osservazioni

table(survey_3$banca_mcf_prodotto_code)
###diverso per ogni record

table(survey_3$nps_factors_survey_filiale)
summary(survey_3$nps_factors_survey_filiale) #628 NA
nps_factors_survey_filiale_distribution <- as.data.frame(table(survey_3$nps_factors_survey_filiale)) 
names(nps_factors_survey_filiale_distribution) <- c("nps_factors_survey_filiale", "Count") 
# Calcola la proporzione per ogni categoria del nps_factors_survey_filiale 
nps_factors_survey_filiale_distribution$Proportion <- nps_factors_survey_filiale_distribution$Count / sum(nps_factors_survey_filiale_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(nps_factors_survey_filiale_distribution, aes(x = nps_factors_survey_filiale, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of nps_factors_survey_filiale Categories", x = "nps_factors_survey_filiale", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###tanti valori, ma i primi 10 livelli sono i valori più frequenti con picco massimo in 2

table(survey_3$reason_for_score_nps_filiale)
summary(survey_3$reason_for_score_nps_filiale) 
###8221 valori, 51188 NA

table(survey_3$disservices_description)
summary(survey_3$disservices_description)
### 1292 valori, 12077 NA

#### exploratory analysis survey 4 ####
cols_4 <- names(survey_4)
survey_4 %>% 
  select(all_of(cols_4)) %>% 
  map(function(x) n_distinct(x, na.rm = TRUE))

names(survey_4)
dim(survey_4) ##appena 1302 righe, 43 variabili

summary(survey_4$ces) 
table(survey_4$ces) ##1175 NA  e 2 zero trasformati in NA(?)
hist(survey_4$ces) 
boxplot(survey_4$ces) 
ces_distribution <- as.data.frame(table(survey_4$ces))
names(ces_distribution) <- c("ces", "Count") 
ces_distribution$ces <- as.numeric(as.character(ces_distribution$ces))
ces_distribution <- ces_distribution[ces_distribution$ces > 0,]
# Calcola la proporzione per ogni valore del ces 
ces_distribution$Proportion <- ces_distribution$Count / sum(ces_distribution$Count) 
daily_mean_ces <- survey_4 %>%   
  group_by(local_response_date) %>%   
  summarise(mean_ces = mean(ces, na.rm = TRUE)) 
ggplot(survey_4, aes(y = ces)) +   
  geom_boxplot(fill = "lightgreen") +   
  labs(title = "Boxplot of Daily Mean ces", y = "Daily Mean ces") +   
  theme_minimal()
###78 tra 9/10, 35 tra 7/8 e 12 dal 6 in giù

summary(survey_4$nps) 
table(survey_4$nps) ## 4 NA
hist(survey_4$nps) 
boxplot(survey_4$nps) 
nps_distribution <- as.data.frame(table(survey_4$nps))
names(nps_distribution) <- c("nps", "Count") 
nps_distribution$nps <- as.numeric(as.character(nps_distribution$nps))
nps_distribution <- nps_distribution[nps_distribution$nps > 0,]
# Calcola la proporzione per ogni valore del nps 
nps_distribution$Proportion <- nps_distribution$Count / sum(nps_distribution$Count) 
daily_mean_nps <- survey_4 %>%   
  group_by(local_response_date) %>%   
  summarise(mean_nps = mean(nps, na.rm = TRUE)) 
ggplot(survey_4, aes(y = nps)) +   
  geom_boxplot(fill = "lightgreen") +   
  labs(title = "Boxplot of Daily Mean nps", y = "Daily Mean nps") +   
  theme_minimal()
###69% voto 9/10, 25% voto 7/8 e i restanti 6 o sotto

summary(survey_4$satisfaction) 
table(survey_4$satisfaction)
###tutti NA

table(survey_4$anzianita_anni_num) 
summary(survey_4$anzianita_anni_num) ##1088 NA
years_distribution <- as.data.frame(table(survey_4$anzianita_anni_num)) 
names(years_distribution) <- c("Years", "Count") 
# Calcola la proporzione per ogni categoria del browser 
years_distribution$Proportion <- years_distribution$Count / sum(years_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del browser 
ggplot(years_distribution, aes(x = Years, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of years Categories", x = "Years", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###12 outlier oltre i 100 --> la curva di distribuzione cresce tra i 7 e 11 anni per poi scendere

table(survey_4$area) 
summary(survey_4$area) 
area_distribution <- as.data.frame(table(survey_4$area)) #47 NA 
names(area_distribution) <- c("area", "Count") 
# Calcola la proporzione per ogni categoria del area 
area_distribution$Proportion <- area_distribution$Count / sum(area_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(area_distribution, aes(x = area, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Area Categories", x = "Area", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###distribuzione varia su tutti i valori tranne agli estremi 45,46,47

table(survey_4$regione_des_ana) 
summary(survey_4$regione_des_ana) #47 NA
regione_des_ana_distribution <- as.data.frame(table(survey_4$regione_des_ana)) 
names(regione_des_ana_distribution) <- c("regione", "Count") 
regione_des_ana_distribution$regione <- as.numeric(as.character(regione_des_ana_distribution$regione))
regione_des_ana_distribution <- regione_des_ana_distribution[order(regione_des_ana_distribution$regione),]
# Calcola la proporzione per ogni categoria della Regione
regione_des_ana_distribution$Proportion <- regione_des_ana_distribution$Count / sum(regione_des_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(regione_des_ana_distribution, aes(x = regione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Regione Categories", x = "regione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###la regione 1 è sempre la più frequente, 2,3,6 e 8 a seguire; da 10 in poi poche osservazioni

table(survey_4$sistema_operativo) 
summary(survey_4$sistema_operativo) #0 NA
sistema_operativo_distribution <- as.data.frame(table(survey_4$sistema_operativo)) 
names(sistema_operativo_distribution) <- c("sistema_operativo", "Count") 
sistema_operativo_distribution$sistema_operativo <- as.numeric(as.character(sistema_operativo_distribution$sistema_operativo))
sistema_operativo_distribution <- sistema_operativo_distribution[order(sistema_operativo_distribution$sistema_operativo),]
# Calcola la proporzione per ogni categoria del sistema_operativo 
sistema_operativo_distribution$Proportion <- sistema_operativo_distribution$Count / sum(sistema_operativo_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sistema_operativo_distribution, aes(x = sistema_operativo, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sistema_operativo Categories", x = "sistema_operativo", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###tutte osservazioni per livello 4

table(survey_4$direzione) 
summary(survey_4$direzione) # 47 NA
direzione_distribution <- as.data.frame(table(survey_4$direzione)) 
names(direzione_distribution) <- c("direzione", "Count") 
direzione_distribution$direzione <- as.numeric(as.character(direzione_distribution$direzione))
direzione_distribution <- direzione_distribution[order(direzione_distribution$direzione),]
# Calcola la proporzione per ogni categoria del direzione 
direzione_distribution$Proportion <- direzione_distribution$Count / sum(direzione_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del direzione 
ggplot(direzione_distribution, aes(x = direzione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of direzione Categories", x = "direzione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###distribuzione omogenea nei livelli da 2 a 9 con picco per 4, i livelli 1/10/11 nulli

table(survey_4$filiale) 
summary(survey_4$filiale) #other 871, 47 NA
filiale_distribution <- as.data.frame(table(survey_4$filiale)) 
names(filiale_distribution) <- c("filiale", "Count") 
# Calcola la proporzione per ogni categoria del filiale 
filiale_distribution$Proportion <- filiale_distribution$Count / sum(filiale_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(filiale_distribution, aes(x = filiale, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of filiale Categories", x = "filiale", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
### tantissimi valori differenti, tante osservazioni per 534

table(survey_4$email) 
summary(survey_4$email)
###ci sono 1251 valori, 51 NA

table(survey_4$customer_segment) #11 LIVELLI
summary(survey_4$customer_segment) #47 NA
customer_segment_distribution <- as.data.frame(table(survey_4$customer_segment)) 
names(customer_segment_distribution) <- c("customer_segment", "Count") 
# Calcola la proporzione per ogni categoria del customer_segment 
customer_segment_distribution$Proportion <- customer_segment_distribution$Count / sum(customer_segment_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(customer_segment_distribution, aes(x = customer_segment, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of customer_segment Categories", x = "customer_segment", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###79% livello 1, 9% livello 2, 10% livello 6,gli altri percentuali molto basse

table(survey_4$attivita_eco_des_ana) 
summary(survey_4$attivita_eco_des_ana) ##48 NA 
attivita_eco_des_ana_distribution <- as.data.frame(table(survey_4$attivita_eco_des_ana)) 
names(attivita_eco_des_ana_distribution) <- c("attivita_eco_des_ana", "Count") 
# Calcola la proporzione per ogni categoria del attivita_eco_des_ana 
attivita_eco_des_ana_distribution$Proportion <- attivita_eco_des_ana_distribution$Count / sum(attivita_eco_des_ana_distribution$Count)
attivita_eco_des_ana_distribution <- attivita_eco_des_ana_distribution[order(-attivita_eco_des_ana_distribution$Proportion), ]
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(attivita_eco_des_ana_distribution, aes(x = attivita_eco_des_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of attivita_eco_des_ana Categories", x = "attivita_eco_des_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###tantissimi valori differenti, picco per il valore 287

table(survey_4$sesso_code_ana) 
summary(survey_4$sesso_code_ana) ##47 NA
sesso_code_ana_distribution <- as.data.frame(table(survey_4$sesso_code_ana)) 
names(sesso_code_ana_distribution) <- c("sesso_code_ana", "Count") 
# Calcola la proporzione per ogni categoria del sesso_code_ana 
sesso_code_ana_distribution$Proportion <- sesso_code_ana_distribution$Count / sum(sesso_code_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sesso_code_ana_distribution, aes(x = sesso_code_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sesso_code_ana Categories", x = "sesso_code_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###quasi 50/50 tra maschi e femmine

table(survey_4$fascia_eta_code_ana) 
summary(survey_4$fascia_eta_code_ana) #47 NA
fascia_eta_code_ana_distribution <- as.data.frame(table(survey_4$fascia_eta_code_ana)) 
names(fascia_eta_code_ana_distribution) <- c("fascia_eta_code_ana", "Count") 
# Calcola la proporzione per ogni categoria del fascia_eta_code_ana 
fascia_eta_code_ana_distribution$Proportion <- fascia_eta_code_ana_distribution$Count / sum(fascia_eta_code_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(fascia_eta_code_ana_distribution, aes(x = fascia_eta_code_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of fascia_eta_code_ana Categories", x = "fascia_eta_code_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###tanti valori per la prima fascia e la quinta, alcuni per le fasce 3,4,6,8 e pochi per 2 e 7

table(survey_4$vdbank_flg_ana)
summary(survey_4$vdbank_flg_ana)
###7 valori 1, 1248 valori 2 e 47 NA

table(survey_4$xntweb_flg_ana)
summary(survey_4$xntweb_flg_ana)
###47 NA,1213 valori 1, 42 valori 2

table(survey_4$cliente_con_mutuo_flg_ana)
summary(survey_4$cliente_con_mutuo_flg_ana)
###47 NA, 992 livello 2, 263 valori 1

table(survey_4$cliente_investitore_flg_ana)
summary(survey_4$cliente_investitore_flg_ana)
###47 NA, 903 livello 2, 352 livello 1

table(survey_4$cliente_solo_cc_flg_ana)
summary(survey_4$cliente_solo_cc_flg_ana)
###47 NA, 1252 di livello 2, 3 di livello 1

table(survey_4$in_bonis_flg_comm)
summary(survey_4$in_bonis_flg_comm)
###47 NA, 1249 di livello 1, 6 di livello 2

table(survey_4$multi_flg_comm)
summary(survey_4$multi_flg_comm)
###47 NA, 307 valori 1, 948 valori 2

table(survey_4$risk_rating_comm)
summary(survey_4$risk_rating_comm) ##1004 NA
risk_rating_comm_distribution <- as.data.frame(table(survey_4$risk_rating_comm)) 
names(risk_rating_comm_distribution) <- c("risk_rating_comm", "Count") 
# Calcola la proporzione per ogni categoria del risk_rating_comm 
risk_rating_comm_distribution$Proportion <- risk_rating_comm_distribution$Count / sum(risk_rating_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(risk_rating_comm_distribution, aes(x = risk_rating_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of risk_rating_comm Categories", x = "risk_rating_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###pchi valori per i livelli 8,9, 10, 11, 12, per i primi 7 invece abbiamo più osservazioni con il picco in 2


table(survey_4$fascia_tr_code_comm)
summary(survey_4$fascia_tr_code_comm) ##1145 NA
fascia_tr_code_comm_distribution <- as.data.frame(table(survey_4$fascia_tr_code_comm)) 
names(fascia_tr_code_comm_distribution) <- c("fascia_tr_code_comm", "Count") 
# Calcola la proporzione per ogni categoria del fascia_tr_code_comm 
fascia_tr_code_comm_distribution$Proportion <- fascia_tr_code_comm_distribution$Count / sum(fascia_tr_code_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(fascia_tr_code_comm_distribution, aes(x = fascia_tr_code_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of fascia_tr_code_comm Categories", x = "fascia_tr_code_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###57 valori 1, 97 valori 2, 3 valori 3

table(survey_4$fascia_utilizzo_online_comm)
summary(survey_4$fascia_utilizzo_online_comm)
###1058 di livello 1, 174 valori 2, 10 valori 3, 60 NA,

table(survey_4$easy_support)
summary(survey_4$easy_support) 
###non presente per questa survey


table(survey_4$segmento_des_comm)
summary(survey_4$segmento_des_comm)
segmento_des_comm_distribution <- as.data.frame(table(survey_4$segmento_des_comm)) 
names(segmento_des_comm_distribution) <- c("segmento_des_comm", "Count") 
# Calcola la proporzione per ogni categoria del segmento_des_comm 
segmento_des_comm_distribution$Proportion <- segmento_des_comm_distribution$Count / sum(segmento_des_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(segmento_des_comm_distribution, aes(x = segmento_des_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of segmento_des_comm Categories", x = "segmento_des_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###oltre 76% per livello 1, 3% livello 2, poi 10% per livello 3 e 7

names(survey_4)
table(survey_4$expenses_icon)
summary(survey_4$expenses_icon)
###74 valori 1, 584 valori 2

table(survey_4$expenses_icon_used)
summary(survey_4$expenses_icon_used) 
###338 valori 1, 127 valori 2, 253 valori 3, 548 NA

table(survey_4$consulted_features)
summary(survey_4$consulted_features)
consulted_features_distribution <- as.data.frame(table(survey_4$consulted_features)) 
names(consulted_features_distribution) <- c("consulted_features", "Count") 
consulted_features_distribution$consulted_features <- as.numeric(as.character(consulted_features_distribution$consulted_features))
consulted_features_distribution <- consulted_features_distribution[order(consulted_features_distribution$consulted_features),]
# Calcola la proporzione per ogni categoria del consulted_features 
consulted_features_distribution$Proportion <- consulted_features_distribution$Count / sum(consulted_features_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(consulted_features_distribution, aes(x = consulted_features, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of consulted_features Categories", x = "consulted_features", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
### 1175 NA, tanti valori per i primi 15 livelli, poi pohe osservazioni

names(survey_4)
table(survey_4$interested_in_functionality)
summary(survey_4$interested_in_functionality)
interested_in_functionality_distribution <- as.data.frame(table(survey_4$interested_in_functionality)) 
names(interested_in_functionality_distribution) <- c("interested_in_functionality", "Count") 
interested_in_functionality_distribution$interested_in_functionality <- as.numeric(as.character(interested_in_functionality_distribution$interested_in_functionality))
interested_in_functionality_distribution <- interested_in_functionality_distribution[order(interested_in_functionality_distribution$interested_in_functionality),]
# Calcola la proporzione per ogni categoria del interested_in_functionality 
interested_in_functionality_distribution$Proportion <- interested_in_functionality_distribution$Count / sum(interested_in_functionality_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(interested_in_functionality_distribution, aes(x = interested_in_functionality, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of interested_in_functionality Categories", x = "interested_in_functionality", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 45, hjust = 1))
###1175 NA, 9 livelli, 40% livello 2, i primi 5 valori sono più frequenti

table(survey_4$ces_reason_for_score)
summary(survey_4$ces_reason_for_score)
###1270 NA, 32 valori

table(survey_4$feature_not_used)
summary(survey_4$feature_not_used)
###1295 NA, 7 other

table(survey_4$feature_not_used_other)
summary(survey_4$feature_not_used_other)
###1295 NA, 7 other

table(survey_4$not_entered_in_expenses_section)
summary(survey_4$not_entered_in_expenses_section)
###1049 NA, 40 valori di livello 1, 54 di livello 2, pochi valori per 3,4,5,6,8

table(survey_4$not_entered_in_expenses_section_other)
summary(survey_4$not_entered_in_expenses_section_other)
###3 other, 1299 NA

####analisi esploratoria survey 16####
cols_16 <- names(survey_16)
survey_16 %>% 
  select(all_of(cols_16)) %>% 
  map(function(x) n_distinct(x, na.rm = TRUE))

names(survey_16)
dim(survey_16) ##appena 2074 righe, 41 variabili

summary(survey_16$ces) 
table(survey_16$ces) ##1175 NA  e 2 zero trasformati in NA(?)
hist(survey_16$ces) 
boxplot(survey_16$ces) 
ces_distribution <- as.data.frame(table(survey_16$ces))
names(ces_distribution) <- c("ces", "Count") 
ces_distribution$ces <- as.numeric(as.character(ces_distribution$ces))
ces_distribution <- ces_distribution[ces_distribution$ces > 0,]
# Calcola la proporzione per ogni valore del ces 
ces_distribution$Proportion <- ces_distribution$Count / sum(ces_distribution$Count) 
daily_mean_ces <- survey_16 %>%   
  group_by(local_response_date) %>%   
  summarise(mean_ces = mean(ces, na.rm = TRUE)) 
ggplot(survey_16, aes(y = ces)) +   
  geom_boxplot(fill = "lightgreen") +   
  labs(title = "Boxplot of Daily Mean ces", y = "Daily Mean ces") +   
  theme_minimal()
###78 tra 9/10, 35 tra 7/8 e 12 dal 6 in giù

summary(survey_16$nps) 
table(survey_16$nps) ## 18 NA
hist(survey_16$nps) 
boxplot(survey_16$nps) 
nps_distribution <- as.data.frame(table(survey_16$nps))
names(nps_distribution) <- c("nps", "Count") 
nps_distribution$nps <- as.numeric(as.character(nps_distribution$nps))
nps_distribution <- nps_distribution[nps_distribution$nps > 0,]
# Calcola la proporzione per ogni valore del nps 
nps_distribution$Proportion <- nps_distribution$Count / sum(nps_distribution$Count) 
daily_mean_nps <- survey_16 %>%   
  group_by(local_response_date) %>%   
  summarise(mean_nps = mean(nps, na.rm = TRUE)) 
ggplot(survey_16, aes(y = nps)) +   
  geom_boxplot(fill = "lightgreen") +   
  labs(title = "Boxplot of Daily Mean nps", y = "Daily Mean nps") +   
  theme_minimal()
###51% voto 9/10, 38% voto 7/8 e i restanti 6 o sotto

table(survey_16$anzianita_anni_num) 
summary(survey_16$anzianita_anni_num) ##253 NA
years_distribution <- as.data.frame(table(survey_16$anzianita_anni_num)) 
names(years_distribution) <- c("Years", "Count") 
# Calcola la proporzione per ogni categoria del browser 
years_distribution$Proportion <- years_distribution$Count / sum(years_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del browser 
ggplot(years_distribution, aes(x = Years, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of years Categories", x = "Years", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###7 outlier oltre i 100 --> la curva di distribuzione cresce tra i 7 e 11 anni per poi scendere

table(survey_16$area) 
summary(survey_16$area) 
area_distribution <- as.data.frame(table(survey_16$area)) #2818 NA 
names(area_distribution) <- c("area", "Count") 
# Calcola la proporzione per ogni categoria del area 
area_distribution$Proportion <- area_distribution$Count / sum(area_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(area_distribution, aes(x = area, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Area Categories", x = "Area", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi tuti i record appartengono all'area 1

table(survey_16$regione_des_ana) 
summary(survey_16$regione_des_ana) #152 NA
regione_des_ana_distribution <- as.data.frame(table(survey_16$regione_des_ana)) 
names(regione_des_ana_distribution) <- c("regione", "Count") 
regione_des_ana_distribution$regione <- as.numeric(as.character(regione_des_ana_distribution$regione))
regione_des_ana_distribution <- regione_des_ana_distribution[order(regione_des_ana_distribution$regione),]
# Calcola la proporzione per ogni categoria della Regione
regione_des_ana_distribution$Proportion <- regione_des_ana_distribution$Count / sum(regione_des_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(regione_des_ana_distribution, aes(x = regione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of Regione Categories", x = "regione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###quasi tuti i record appartengono alla regione 1

table(survey_16$sistema_operativo) 
summary(survey_16$sistema_operativo) #0 NA
sistema_operativo_distribution <- as.data.frame(table(survey_16$sistema_operativo)) 
names(sistema_operativo_distribution) <- c("sistema_operativo", "Count") 
sistema_operativo_distribution$sistema_operativo <- as.numeric(as.character(sistema_operativo_distribution$sistema_operativo))
sistema_operativo_distribution <- sistema_operativo_distribution[order(sistema_operativo_distribution$sistema_operativo),]
# Calcola la proporzione per ogni categoria del sistema_operativo 
sistema_operativo_distribution$Proportion <- sistema_operativo_distribution$Count / sum(sistema_operativo_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sistema_operativo_distribution, aes(x = sistema_operativo, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sistema_operativo Categories", x = "sistema_operativo", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###livelli 1,2,18,5,6,7,10,11 i più frequenti

table(survey_16$direzione) 
summary(survey_16$direzione) # 152 NA
direzione_distribution <- as.data.frame(table(survey_16$direzione)) 
names(direzione_distribution) <- c("direzione", "Count") 
direzione_distribution$direzione <- as.numeric(as.character(direzione_distribution$direzione))
direzione_distribution <- direzione_distribution[order(direzione_distribution$direzione),]
# Calcola la proporzione per ogni categoria del direzione 
direzione_distribution$Proportion <- direzione_distribution$Count / sum(direzione_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del direzione 
ggplot(direzione_distribution, aes(x = direzione, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of direzione Categories", x = "direzione", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###praticamente solo livello 1

table(survey_16$filiale) 
summary(survey_16$filiale) #other 53, 152 NA
filiale_distribution <- as.data.frame(table(survey_16$filiale)) 
names(filiale_distribution) <- c("filiale", "Count") 
# Calcola la proporzione per ogni categoria del filiale 
filiale_distribution$Proportion <- filiale_distribution$Count / sum(filiale_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(filiale_distribution, aes(x = filiale, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of filiale Categories", x = "filiale", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
### tantissimi valori differenti, tante osservazioni per 1

table(survey_16$email) 
summary(survey_16$email)
###ci sono 1916 valori, 158 NA

table(survey_16$customer_segment) #9 LIVELLI
summary(survey_16$customer_segment) #152 NA
customer_segment_distribution <- as.data.frame(table(survey_16$customer_segment)) 
names(customer_segment_distribution) <- c("customer_segment", "Count") 
# Calcola la proporzione per ogni categoria del customer_segment 
customer_segment_distribution$Proportion <- customer_segment_distribution$Count / sum(customer_segment_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(customer_segment_distribution, aes(x = customer_segment, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of customer_segment Categories", x = "customer_segment", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###77% livello 1, 21% livello 2,gli altri percentuali molto basse

table(survey_16$attivita_eco_des_ana) 
summary(survey_16$attivita_eco_des_ana) ##157 NA 
attivita_eco_des_ana_distribution <- as.data.frame(table(survey_16$attivita_eco_des_ana)) 
names(attivita_eco_des_ana_distribution) <- c("attivita_eco_des_ana", "Count") 
# Calcola la proporzione per ogni categoria del attivita_eco_des_ana 
attivita_eco_des_ana_distribution$Proportion <- attivita_eco_des_ana_distribution$Count / sum(attivita_eco_des_ana_distribution$Count)
attivita_eco_des_ana_distribution <- attivita_eco_des_ana_distribution[order(-attivita_eco_des_ana_distribution$Proportion), ]
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(attivita_eco_des_ana_distribution, aes(x = attivita_eco_des_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of attivita_eco_des_ana Categories", x = "attivita_eco_des_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###tantissimi valori differenti, livelli 293,1, 287,82 i più frequenti

table(survey_16$sesso_code_ana) 
summary(survey_16$sesso_code_ana) ##152 NA
sesso_code_ana_distribution <- as.data.frame(table(survey_16$sesso_code_ana)) 
names(sesso_code_ana_distribution) <- c("sesso_code_ana", "Count") 
# Calcola la proporzione per ogni categoria del sesso_code_ana 
sesso_code_ana_distribution$Proportion <- sesso_code_ana_distribution$Count / sum(sesso_code_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(sesso_code_ana_distribution, aes(x = sesso_code_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of sesso_code_ana Categories", x = "sesso_code_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###70/30 tra livello 1 e 2

table(survey_16$fascia_eta_code_ana) #8 livelli
summary(survey_16$fascia_eta_code_ana) #182 NA
fascia_eta_code_ana_distribution <- as.data.frame(table(survey_16$fascia_eta_code_ana)) 
names(fascia_eta_code_ana_distribution) <- c("fascia_eta_code_ana", "Count") 
# Calcola la proporzione per ogni categoria del fascia_eta_code_ana 
fascia_eta_code_ana_distribution$Proportion <- fascia_eta_code_ana_distribution$Count / sum(fascia_eta_code_ana_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(fascia_eta_code_ana_distribution, aes(x = fascia_eta_code_ana, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of fascia_eta_code_ana Categories", x = "fascia_eta_code_ana", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###tanti valori per 1,3,5,8

table(survey_16$vdbank_flg_ana)
summary(survey_16$vdbank_flg_ana)
###1310 valori 1, 612 valori 2 e 152 NA

table(survey_16$xntweb_flg_ana)
summary(survey_16$xntweb_flg_ana)
###152 NA,1180 valori 1, 1782 valori 2

table(survey_16$cliente_con_mutuo_flg_ana)
summary(survey_16$cliente_con_mutuo_flg_ana)
###152 NA, 1728 livello 2, 1918 valori 1

table(survey_16$cliente_investitore_flg_ana)
summary(survey_16$cliente_investitore_flg_ana)
###152 NA, 1336 livello 2, 586 livello 1

table(survey_16$cliente_solo_cc_flg_ana)
summary(survey_16$cliente_solo_cc_flg_ana)
###152 NA, 1890 di livello 2, 32 di livello 1

table(survey_16$in_bonis_flg_comm)
summary(survey_16$in_bonis_flg_comm)
###152 NA, 1918 di livello 1, 18 di livello 2

table(survey_16$multi_flg_comm)
summary(survey_16$multi_flg_comm)
###152 NA, 896 valori 1, 1026 valori 2

table(survey_16$risk_rating_comm)#12livelli
summary(survey_16$risk_rating_comm) ##1918 NA
risk_rating_comm_distribution <- as.data.frame(table(survey_16$risk_rating_comm)) 
names(risk_rating_comm_distribution) <- c("risk_rating_comm", "Count") 
# Calcola la proporzione per ogni categoria del risk_rating_comm 
risk_rating_comm_distribution$Proportion <- risk_rating_comm_distribution$Count / sum(risk_rating_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(risk_rating_comm_distribution, aes(x = risk_rating_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of risk_rating_comm Categories", x = "risk_rating_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###pchi valori per i livelli 9, 11, 12, per i primi 7 invece abbiamo più osservazioni con il picco in 2

table(survey_16$fascia_tr_code_comm) #3 livelli
summary(survey_16$fascia_tr_code_comm) ##2061 NA
fascia_tr_code_comm_distribution <- as.data.frame(table(survey_16$fascia_tr_code_comm)) 
names(fascia_tr_code_comm_distribution) <- c("fascia_tr_code_comm", "Count") 
# Calcola la proporzione per ogni categoria del fascia_tr_code_comm 
fascia_tr_code_comm_distribution$Proportion <- fascia_tr_code_comm_distribution$Count / sum(fascia_tr_code_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(fascia_tr_code_comm_distribution, aes(x = fascia_tr_code_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of fascia_tr_code_comm Categories", x = "fascia_tr_code_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###5 valori 1, 7 valori 2, 1 valori 3

table(survey_16$fascia_utilizzo_online_comm)
summary(survey_16$fascia_utilizzo_online_comm)
###1719 di livello 1, 25 valori 2, 3 valori 3, 327 NA

table(survey_16$segmento_des_comm) #118 livelli
summary(survey_16$segmento_des_comm) # 0 NA
segmento_des_comm_distribution <- as.data.frame(table(survey_16$segmento_des_comm)) 
names(segmento_des_comm_distribution) <- c("segmento_des_comm", "Count") 
# Calcola la proporzione per ogni categoria del segmento_des_comm 
segmento_des_comm_distribution$Proportion <- segmento_des_comm_distribution$Count / sum(segmento_des_comm_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(segmento_des_comm_distribution, aes(x = segmento_des_comm, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of segmento_des_comm Categories", x = "segmento_des_comm", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
###oltre 71% per livello 1, 7% livello 2, 20% per livello 3

names(survey_16)
table(survey_16$consulted_features)
summary(survey_16$consulted_features)
consulted_features_distribution <- as.data.frame(table(survey_16$consulted_features)) 
names(consulted_features_distribution) <- c("consulted_features", "Count") 
consulted_features_distribution$consulted_features <- as.numeric(as.character(consulted_features_distribution$consulted_features))
consulted_features_distribution <- consulted_features_distribution[order(consulted_features_distribution$consulted_features),]
# Calcola la proporzione per ogni categoria del consulted_features 
consulted_features_distribution$Proportion <- consulted_features_distribution$Count / sum(consulted_features_distribution$Count) 
# Istogramma della distribuzione normalizzata delle categorie del gestore 
ggplot(consulted_features_distribution, aes(x = consulted_features, y = Proportion)) +   
  geom_bar(stat = "identity", fill = "lightblue", color = "black") +   
  labs(title = "Normalized Distribution of consulted_features Categories", x = "consulted_features", y = "Proportion") +   
  theme_minimal() +   theme(axis.text.x = element_text(angle = 185, hjust = 1))
### 1175 NA, tanti valori per i primi 15 livelli, poi pohe osservazioni

names(survey_16)
table(survey_16$app_knowledge)
summary(survey_16$app_knowledge)
# 1    2      NA's 
# 2009   63    2 

table(survey_16$reason_for_score_comment)
summary(survey_16$reason_for_score_comment)
###1607 NA, 1867 reasons

table(survey_16$other_issue_type)
summary(survey_16$other_issue_type)
###20618 NA, 10 other

table(survey_16$app_used)
summary(survey_16$app_used)
###65 NA, 1815 valori di livello 1, 1918 di livello 2

table(survey_16$not_entered_in_expenses_section_other)
summary(survey_16$not_entered_in_expenses_section_other)
###3 other, 1299 NA



####analisi esploratoria bivariata generale####
str(df_clean)
# categorical_vars <- names(df_clean)[sapply(df_clean, is.factor)]

ggplot(df_clean, aes_string(x = "survey", y = "target")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra Survey e target")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Survey", y = "Target") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(df_clean, aes_string(x = "browser", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra Browser e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Browser", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#i primi 9 livelli presentano valori molto simili per mediana e variabilità, 
#tranne il livello 9 che ha una mediana più bassa e ha una minore variabilità
#dal livello 10 in poi variabilità e mediana sono più eterogenei a causa dei pochi valori

ggplot(df_clean, aes_string(x = "sistema_operativo", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra sistema_operativo e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "sistema_operativo", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i primi 11 livelli presentano valori molto simili per mediana e variabilità, 
#tranne il livello 6 che ha una mediana vicino al 10 e il livello 9 che ha una minore variabilità
#dal livello 12 in poi variabilità e mediana sono più eterogenei a causa dei pochi valori

ggplot(df_clean, aes_string(x = "segmento_des_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra segmento_des_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "segmento_des_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i primi 5 livelli hanno mediana e variabilità pressochè identica, dai livelli 6 a 9 abbiamo
#comportamenti irregolari e diversi, mentre da 10 a 118 non abbiamo il boxplot

ggplot(df_clean, aes_string(x = "nps_segment", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra nps_segment e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "nps_segment", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i promoter presentano una mediana pari a 10, i passive attorno a 8, i detractor a 5 
#ma con una maggiore variabilità rispetto agli altri due

ggplot(df_clean, aes_string(x = "gestore", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra gestore e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "gestore", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#plot non riuscito a causa dell'eccessivo numero di livelli

ggplot(df_clean, aes_string(x = "survey", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra survey e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "survey", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#le survey 3,16,52 presentano valori di mediana (sopra 8.75) e variabilità simili
#le survey 18 e 18 hanno una mediana pari a 10, la variabilità della 18 è simile 
#alle 3 descritte precedentemente, mentre la 18 ha 75% dei valori al di sopra del 8.75
#le survey 1 e 2 coerentemente non presentano il boxplot 

ggplot(df_clean, aes_string(x = "operazione", y = "target")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra operazione e Target")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "operazione", y = "Target") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#l'alto numero di livelli rende difficile trarre conclusioni, ma per la maggior parte
# troviamo una variabilità compresa tra 5 e 10 con mediana superiore a 8.75, 
#qualche livello presenta una minore variabilità e una mediana attorno ai 10

ggplot(df_clean, aes_string(x = "banca_mcf_prodotto_code", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra banca_mcf_prodotto_code e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "banca_mcf_prodotto_code", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#grafico non computabile a causa dei numerosi livelli

ggplot(df_clean, aes_string(x = "regione_des_ana", y = "target")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra regione_des_ana e Target")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "regione_des_ana", y = "Target") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#menzione per le regioni 9,13,118, 17,18,19 che hanno una mediana pari a 10
#la regione 20 è l'unica con mediana al di sotto di 8.75,
#la variabilità è abbastanza omogenea, a parte per 10, 118, 17, 18, 19, 20
#probabilmente a causa del ridotto numero di valori presenti

ggplot(df_clean, aes_string(x = "area", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra area e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "area", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#quasi tutti i livelli presentano la stessa variabilità e mediana sopra a 8.75
#i livelli 13, 15, 27, 182, 1818 hanno una mediana pari a 10,
# mentre i livelli 1818 e 186 hanno una variabilità più ristretta


ggplot(df_clean, aes_string(x = "direzione", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra direzione e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "direzione", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#la variabilità è la stessa per tutti i livelli, a parte l'11 dove è ristretta
#attorno ai valori 9 e 10. Qui la mediana è quindi superiore a 8.75 e cosi anche 
# nel livello 7, mentre per gli altri si assesta sempre intorno a questa soglia


ggplot(df_clean, aes_string(x = "filiale", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra filiale e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "filiale", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#troppi valori differenti per trarre delle considerazioni

ggplot(df_clean, aes_string(x = "email", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra email e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "email", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i valori con o senza l'email presentano una stessa variabilità e mediana
#nei confronti del target NPS, possiamo assumere questa variabile poco significativa


ggplot(df_clean, aes_string(x = "customer_segment", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra customer_segment e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "customer_segment", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#il livello 10 non presenta il boxplot, i livelli 5,6,9,11 hanno una variabilità
#ristretta; i livelli 9 e 11 hanno una mediana più bassa rispetto agli altri
#segmenti, mentre il segmento 6 ha una mediana pari a 10

ggplot(df_clean, aes_string(x = "attivita_eco_des_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra attivita_eco_des_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "attivita_eco_des_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#eccessivo numero di valori per dedurre qualcosa

ggplot(df_clean, aes_string(x = "sesso_code_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra sesso_code_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "sesso_code_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabile poco significativa perchè entrambi livelli e gli NA hanno
#la stessa mediana e variabilità

ggplot(df_clean, aes_string(x = "fascia_eta_code_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_eta_code_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_eta_code_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#tutti i livelli presentano stessa variabilità e mediana, poca significatività

ggplot(df_clean, aes_string(x = "fascia_anzianita_code_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_anzianita_code_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_anzianita_code_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#tutti gli 8 livelli e gli NA hanno stessa mediana e variabilità

ggplot(df_clean, aes_string(x = "xav_profilo_postazione", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xav_profilo_postazione e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xav_profilo_postazione", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#nessun boxplot per i 18 valori, solo NA

ggplot(df_clean, aes_string(x = "fascia_fatturato_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_fatturato_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_fatturato_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i livelli 2 e 5 presentano minore variabilità e una mediana leggermente più alta

ggplot(df_clean, aes_string(x = "banca_mcf_xav_des", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra banca_mcf_xav_des e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "banca_mcf_xav_des", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#no boxplot per ragione sociale cliente 

ggplot(df_clean, aes_string(x = "vdbank_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra vdbank_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "vdbank_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per entrambi i livelli 
#e anche per gli NA

ggplot(df_clean, aes_string(x = "xntweb_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xntweb_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xntweb_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per entrambi i livelli 
#e anche per gli NA

ggplot(df_clean, aes_string(x = "cliente_con_mutuo_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_con_mutuo_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_con_mutuo_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per entrambi i livelli 
#e anche per gli NA

ggplot(df_clean, aes_string(x = "cliente_investitore_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_investitore_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_investitore_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità identica però gli NA hanno una mediana pari a 10,
#mentre i livelli 1 e 2 sono leggermente superiori a 8.75

ggplot(df_clean, aes_string(x = "cliente_solo_cc_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_solo_cc_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_solo_cc_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità identica però gli NA hanno una mediana pari a 10,
#mentre i livelli 1 e 2 sono leggermente superiori a 8.75

ggplot(df_clean, aes_string(x = "in_bonis_flg_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra in_bonis_flg_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "in_bonis_flg_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana identica, ma variabilità ridotta per il livello 2

ggplot(df_clean, aes_string(x = "multi_flg_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra multi_flg_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "multi_flg_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per entrambi i livelli 
#e anche per gli NA

ggplot(df_clean, aes_string(x = "risk_rating_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra risk_rating_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "risk_rating_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i livelli 6,9,10,11,12 hanno una mediana pari a 10
#i livelli 9, 10, 11, 12 hanno una variabilità più ridotta
#gli NA hanno variabilità e mediana simile agli altri

ggplot(df_clean, aes_string(x = "fascia_tr_code_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_tr_code_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_tr_code_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 con una mediana attorno a 8 e una variabilità fino a 5
#livello 2 con una mediana pari a 10 e variabilità ristretta
#livello 3 con una mediana attorno a 8, ma ampia variabilità
#NA con mediana superiore a 8.75 e variabilità come livello 1

ggplot(df_clean, aes_string(x = "fascia_utilizzo_online_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_utilizzo_online_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_utilizzo_online_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# NA e livelli hanno la stessa variabilità, però il livello 2
# ha una mediana pari a 10

ggplot(df_clean, aes_string(x = "nps_factors_survey_canale", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra nps_factors_survey_canale e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "nps_factors_survey_canale", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#troppi valori e boxplot per trarre conclusioni

ggplot(df_clean, aes_string(x = "nps_factors_survey_canale_other", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra nps_factors_survey_canale_other e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "nps_factors_survey_canale_other", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pressochè identica, ma il livello Other ha una variabilità 
#maggiore rispetto agli NA

ggplot(df_clean, aes_string(x = "app_knowledge", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra app_knowledge e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "app_knowledge", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#stessa variabilità per i livelli e gli NA, ma il livello 2 ha una
#variabilità di circa 8 inferiore a livello 1 e NA dove supera 8.75

ggplot(df_clean, aes_string(x = "app_used", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra app_used e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "app_used", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per livelli e NA

ggplot(df_clean, aes_string(x = "xv_va_app_not_used_reason", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xv_va_app_not_used_reason e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xv_va_app_not_used_reason", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana (8.75) e variabilità identica nei livelli 1,2,3 e gli NA 
#livello 18 ha una variabilità più ampia e una mediana più bassa

ggplot(df_clean, aes_string(x = "xa_app_not_used_reason", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xa_app_not_used_reason e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xa_app_not_used_reason", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#nessun valore per gli NPS, solo NA

ggplot(df_clean, aes_string(x = "xa_app_not_used_reason_other", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xa_app_not_used_reason_other e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xa_app_not_used_reason_other", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#solo NA, nessun valore NPS per Other

ggplot(df_clean, aes_string(x = "nps_factors_survey_filiale", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra nps_factors_survey_filiale e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "nps_factors_survey_filiale", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#troppi valori per dedurre qualcosa

ggplot(df_clean, aes_string(x = "reason_for_score_nps_filiale", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra reason_for_score_nps_filiale e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "reason_for_score_nps_filiale", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#NPS filiale e NA con identica variabilità e mediana

ggplot(df_clean, aes_string(x = "disservices_description", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra disservices_description e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "disservices_description", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#il commento ha una mediana più bassa e una variabilità maggiore
#la mediana del commento ha valore 8, mentre gli NA superiore a 8.75

ggplot(df_clean, aes_string(x = "need_satisfied", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra need_satisfied e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "need_satisfied", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#no boxplot per i valori

ggplot(df_clean, aes_string(x = "reason_for_satisfaction", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra reason_for_satisfaction e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "reason_for_satisfaction", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#no boxplot per i valori

ggplot(df_clean, aes_string(x = "suggestions_to_improve", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra suggestions_to_improve e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "suggestions_to_improve", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#no boxplot per i valori

ggplot(df_clean, aes_string(x = "reason_for_dissatisfaction", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra reason_for_dissatisfaction e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "reason_for_dissatisfaction", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#no boxplot per i valori

ggplot(df_clean, aes_string(x = "reason_for_score_comment", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra reason_for_score_comment e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "reason_for_score_comment", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#il commento ha una mediana pari a 10, mentre la variabilità 
# è la stessa degli NA

ggplot(df_clean, aes_string(x = "other_issue_type", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra other_issue_type e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "other_issue_type", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#il commento ha una mediana pari a 8,quindi più bassa del 8.75
#degli NA, mentre la variabilità è maggiore

ggplot(df_clean, aes_string(x = "expenses_icon", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra expenses_icon e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "expenses_icon", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 ha una mediana pari a 10 e una variabilità stretta
#livello 2 e NA hanno la stessa mediana a 8.75 e variabilità identica

ggplot(df_clean, aes_string(x = "expenses_icon_used", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra expenses_icon_used e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "expenses_icon_used", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i livelli 1,2,3 hanno mediana pari a 10 e variabilità ridotta sopra ai 8.75,
#mentre NA hanno la solita variabilità e mediana 8.75

ggplot(df_clean, aes_string(x = "consulted_features", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra consulted_features e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "consulted_features", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#tanti livelli con mediana pari a 10 e variabilità ridotta
#qualche outlier sotto i 7,5, altrimenti tutti superiori
#NA con variabilità fino a 5 e mediana 8.75

ggplot(df_clean, aes_string(x = "interested_in_functionality", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra interested_in_functionality e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "interested_in_functionality", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabili interessante: livello 6 e 8 hanno una mediana molto bassa
#gli altri livelli hanno una mediana superiore a 8
#variabilità ampia per 6,7 e gli NA

ggplot(df_clean, aes_string(x = "ces_reason_for_score", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra ces_reason_for_score e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "ces_reason_for_score", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pari a 10 e variabilità ridotta per reason
#NA mediana pari a 8.75 e variabilità ampia

ggplot(df_clean, aes_string(x = "feature_not_used", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra feature_not_used e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "feature_not_used", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Other con mediana pari a 10 e variabilità pressochè nulla,
#gli NA hanno la solita variabilità tra 5 e 10 con mediana 8.75

ggplot(df_clean, aes_string(x = "feature_not_used_other", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra feature_not_used_other e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "feature_not_used_other", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Other con mediana pari a 10 e variabilità pressochè nulla,
#gli NA hanno la solita variabilità tra 5 e 10 con mediana 8.75

ggplot(df_clean, aes_string(x = "not_entered_in_expenses_section", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra not_entered_in_expenses_section e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "not_entered_in_expenses_section", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 e 18 con mediana pari a 10 e variabilità ristretta superiore a 8
#livello 2,3 e NA con variabilità tra 5 e 10 e mediana pari a 9
#livelli 5 e 6 con variabilità molto stretta, mediana di poco inferiore a 8.75 
#per il livello 5, mentre per 6 è di poco inferiore 10
#livello 8 con mediana pari a 8 e variabilità abbastanza ristretta

ggplot(df_clean, aes_string(x = "not_entered_in_expenses_section_other", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra not_entered_in_expenses_section_other e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "not_entered_in_expenses_section_other", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#other con mediana pari a 10 e variabilità superiore a 9
#NA con mediana pari a 9 e variabilità fino a 5 


####analisi esploratoria bivariata survey 1####
str(survey_1)
skimr::skim(survey_1)
categorical_vars <- names(survey_1)[sapply(survey_1, is.factor)]

ggplot(survey_1, aes_string(x = "browser", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra Browser e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Browser", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#i livelli 10 e 16 hanno una variabilità nulla e mediana rispettivamente 5 e 9
#i livelli 1,2,3,5 hanno ampia variabilità con mediana pari a 8 tranne il 2 che ha 9

ggplot(survey_1, aes_string(x = "sistema_operativo", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra sistema_operativo e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "sistema_operativo", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli 1,2,3,18,8,11,12,13,118,20
#i livelli 3,8,11,20 presentano mediana pari a 9, liv. 1 -> 8, liv.2 -> 7.5
#mentre i livelli 12-13 pari a 7 e il 118 pari a 10
#la variabilità è ampia per i livelli 1,2,3,18,8,11,118, mentre i livelli
#12,13 è ristretta e per 20 è nulla

ggplot(survey_1, aes_string(x = "segmento_des_comm", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra segmento_des_comm e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "segmento_des_comm", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 e 3 con ampia variabilità e mediana pari a 8

names(survey_1)
ggplot(survey_1, aes_string(x = "satisfaction", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra satisfaction e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "satisfaction", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#solo satisfaction con un valore con ampia variabilità e mediana 8

ggplot(survey_1, aes_string(x = "operazione", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra operazione e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "operazione", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#solo operazione 1 con ampia variabilità e mediana 8

ggplot(survey_1, aes_string(x = "regione_des_ana", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra regione_des_ana e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "regione_des_ana", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#menzione per le regioni 13,18,19,20 che hanno una mediana pari a 10
#le regioni 118 e 16 hanno una mediana pari rispettivamente a 3 e 5
#la variabilità è abbastanza ampia per i primi 9 livelli probabilmente per un maggiore
#numero di osservazioni, mentre i livelli 12,13,118,16,18,19,20 hanno variabilità minima o nulla
#probabilmente a causa del ridotto numero di valori presenti

ggplot(survey_1, aes_string(x = "area", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra area e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "area", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#boxplots molto diversi per ogni livello, non si posso individuare pattern regolari

ggplot(survey_1, aes_string(x = "direzione", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra direzione e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "direzione", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli da 1 a 9
#la variabilità è la stessa per tutti i livelli
#la mediana è uguale o superiore a 8, nei livelli 2,7,9 è pari a 9

ggplot(survey_1, aes_string(x = "filiale", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra filiale e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "filiale", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#troppi valori differenti per trarre delle considerazioni

ggplot(survey_1, aes_string(x = "email", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra email e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "email", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pari a 8 e ampia variabilità

ggplot(survey_1, aes_string(x = "customer_segment", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra customer_segment e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "customer_segment", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 e 2 
#mediana pari a 8 per entrambi e variabilità leggermente più ampia per l'1

ggplot(survey_1, aes_string(x = "attivita_eco_des_ana", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra attivita_eco_des_ana e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "attivita_eco_des_ana", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#eccessivo numero di valori per dedurre qualcosa

ggplot(survey_1, aes_string(x = "sesso_code_ana", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra sesso_code_ana e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "sesso_code_ana", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 con mediana pari a 9, mediana livello 2 pari a 8
#variabilità ampia per entrambi

ggplot(survey_1, aes_string(x = "fascia_eta_code_ana", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_eta_code_ana e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_eta_code_ana", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#tutti i livelli presentano stessa variabilità
#la mediana è uguale a 8 a parte per i livelli 3 e 6 che è pari a 9

ggplot(survey_1, aes_string(x = "fascia_anzianita_code_ana", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_anzianita_code_ana e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_anzianita_code_ana", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#tutti i livelli presentano stessa variabilità
#la mediana è uguale a 8 a parte per i livelli 3 e 6 che è pari a 9

ggplot(survey_1, aes_string(x = "vdbank_flg_ana", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra vdbank_flg_ana e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "vdbank_flg_ana", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana identica per il livello 1 e gli NA 

ggplot(survey_1, aes_string(x = "xntweb_flg_ana", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xntweb_flg_ana e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xntweb_flg_ana", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana identica per il livello 1 e gli NA 

ggplot(survey_1, aes_string(x = "cliente_con_mutuo_flg_ana", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_con_mutuo_flg_ana e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_con_mutuo_flg_ana", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per entrambi i livelli 
#per gli NA mediana pari a 10 e variabilità nulla

ggplot(survey_1, aes_string(x = "cliente_investitore_flg_ana", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_investitore_flg_ana e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_investitore_flg_ana", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per entrambi i livelli 
#per gli NA mediana pari a 10 e variabilità nulla

ggplot(survey_1, aes_string(x = "cliente_solo_cc_flg_ana", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_solo_cc_flg_ana e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_solo_cc_flg_ana", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pressochè identica per entrambi i livelli 
#la variabilità del livello 1 copre tutta la scala, ma anche quella del 2 è ampia
#per gli NA mediana pari a 10 e variabilità nulla

ggplot(survey_1, aes_string(x = "in_bonis_flg_comm", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra in_bonis_flg_comm e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "in_bonis_flg_comm", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#solo livello 1 con mediana pari a 8 e variabilità da 3 a 10

ggplot(survey_1, aes_string(x = "multi_flg_comm", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra multi_flg_comm e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "multi_flg_comm", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana identica per il livello 1 e gli NA 

ggplot(survey_1, aes_string(x = "risk_rating_comm", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra risk_rating_comm e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "risk_rating_comm", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli da 1 a 7
#i livelli 1,2,3,18,7 e NA hanno una mediana pari a 8, mentre 5 e 6 pari a 9
#i livelli 1,5,6,7 hanno una variabilità più ridotta
#gli NA hanno variabilità e mediana simile agli altri (2,3,18)

ggplot(survey_1, aes_string(x = "fascia_tr_code_comm", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_tr_code_comm e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_tr_code_comm", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 con una mediana attorno a 8 e una variabilità fino a 5
#livello 2 con una mediana pari a 10 e variabilità ristretta
#livello 3 con una mediana attorno a 8, ma ampia variabilità
#NA con mediana superiore a 8.75 e variabilità come livello 1

ggplot(survey_1, aes_string(x = "fascia_utilizzo_online_comm", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_utilizzo_online_comm e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_utilizzo_online_comm", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# NA e livelli hanno la stessa variabilità, però il livello 2
# ha una mediana pari a 10

ggplot(survey_1, aes_string(x = "ces_factors_survey_canale", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra ces_factors_survey_canale e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "ces_factors_survey_canale", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#troppi valori e boxplot per trarre conclusioni

ggplot(survey_1, aes_string(x = "ces_factors_survey_canale_other", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra ces_factors_survey_canale_other e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "ces_factors_survey_canale_other", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pressochè identica, ma il livello Other ha una variabilità 
#maggiore rispetto agli NA

ggplot(survey_1, aes_string(x = "app_knowledge", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra app_knowledge e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "app_knowledge", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#stessa variabilità per i livelli e gli NA, ma il livello 2 ha una
#variabilità di circa 8 inferiore a livello 1 e NA dove supera 8.75

ggplot(survey_1, aes_string(x = "app_used", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra app_used e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "app_used", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per livelli e NA

ggplot(survey_1, aes_string(x = "xv_va_app_not_used_reason", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xv_va_app_not_used_reason e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xv_va_app_not_used_reason", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana (8.75) e variabilità identica nei livelli 1,2,3 e gli NA 
#livello 18 ha una variabilità più ampia e una mediana più bassa

ggplot(survey_1, aes_string(x = "xa_app_not_used_reason", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xa_app_not_used_reason e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xa_app_not_used_reason", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#nessun valore per gli ces, solo NA

ggplot(survey_1, aes_string(x = "xa_app_not_used_reason_other", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xa_app_not_used_reason_other e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xa_app_not_used_reason_other", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#solo NA, nessun valore ces per Other

ggplot(survey_1, aes_string(x = "ces_factors_survey_filiale", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra ces_factors_survey_filiale e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "ces_factors_survey_filiale", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#troppi valori per dedurre qualcosa

ggplot(survey_1, aes_string(x = "reason_for_score_ces_filiale", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra reason_for_score_ces_filiale e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "reason_for_score_ces_filiale", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#ces filiale e NA con identica variabilità e mediana

ggplot(survey_1, aes_string(x = "disservices_description", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra disservices_description e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "disservices_description", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#il commento ha una mediana più bassa e una variabilità maggiore
#la mediana del commento ha valore 8, mentre gli NA superiore a 8.75

ggplot(survey_1, aes_string(x = "need_satisfied", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra need_satisfied e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "need_satisfied", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#no boxplot per i valori

ggplot(survey_1, aes_string(x = "reason_for_satisfaction", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra reason_for_satisfaction e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "reason_for_satisfaction", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#no boxplot per i valori

ggplot(survey_1, aes_string(x = "suggestions_to_improve", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra suggestions_to_improve e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "suggestions_to_improve", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#no boxplot per i valori

ggplot(survey_1, aes_string(x = "reason_for_dissatisfaction", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra reason_for_dissatisfaction e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "reason_for_dissatisfaction", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#no boxplot per i valori

ggplot(survey_1, aes_string(x = "reason_for_score_comment", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra reason_for_score_comment e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "reason_for_score_comment", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#il commento ha una mediana pari a 10, mentre la variabilità 
# è la stessa degli NA

ggplot(survey_1, aes_string(x = "other_issue_type", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra other_issue_type e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "other_issue_type", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#il commento ha una mediana pari a 8,quindi più bassa del 8.75
#degli NA, mentre la variabilità è maggiore

ggplot(survey_1, aes_string(x = "expenses_icon", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra expenses_icon e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "expenses_icon", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 ha una mediana pari a 10 e una variabilità stretta
#livello 2 e NA hanno la stessa mediana a 8.75 e variabilità identica

ggplot(survey_1, aes_string(x = "expenses_icon_used", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra expenses_icon_used e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "expenses_icon_used", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i livelli 1,2,3 hanno mediana pari a 10 e variabilità ridotta sopra ai 8.75,
#mentre NA hanno la solita variabilità e mediana 8.75

ggplot(survey_1, aes_string(x = "consulted_features", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra consulted_features e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "consulted_features", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#tanti livelli con mediana pari a 10 e variabilità ridotta
#qualche outlier sotto i 7,5, altrimenti tutti superiori
#NA con variabilità fino a 5 e mediana 8.75

ggplot(survey_1, aes_string(x = "interested_in_functionality", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra interested_in_functionality e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "interested_in_functionality", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabili interessante: livello 6 e 8 hanno una mediana molto bassa
#gli altri livelli hanno una mediana superiore a 8
#variabilità ampia per 6,7 e gli NA

ggplot(survey_1, aes_string(x = "ces_reason_for_score", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra ces_reason_for_score e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "ces_reason_for_score", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pari a 10 e variabilità ridotta per reason
#NA mediana pari a 8.75 e variabilità ampia

ggplot(survey_1, aes_string(x = "feature_not_used", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra feature_not_used e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "feature_not_used", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Other con mediana pari a 10 e variabilità pressochè nulla,
#gli NA hanno la solita variabilità tra 5 e 10 con mediana 8.75

ggplot(survey_1, aes_string(x = "feature_not_used_other", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra feature_not_used_other e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "feature_not_used_other", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Other con mediana pari a 10 e variabilità pressochè nulla,
#gli NA hanno la solita variabilità tra 5 e 10 con mediana 8.75

ggplot(survey_1, aes_string(x = "not_entered_in_expenses_section", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra not_entered_in_expenses_section e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "not_entered_in_expenses_section", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 e 18 con mediana pari a 10 e variabilità ristretta superiore a 8
#livello 2,3 e NA con variabilità tra 5 e 10 e mediana pari a 9
#livelli 5 e 6 con variabilità molto stretta, mediana di poco inferiore a 8.75 
#per il livello 5, mentre per 6 è di poco inferiore 10
#livello 8 con mediana pari a 8 e variabilità abbastanza ristretta

ggplot(survey_1, aes_string(x = "not_entered_in_expenses_section_other", y = "ces")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra not_entered_in_expenses_section_other e ces")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "not_entered_in_expenses_section_other", y = "ces") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#other con mediana pari a 10 e variabilità superiore a 9
#NA con mediana pari a 9 e variabilità fino a 5 




####analisi esploratoria bivariata survey 3####
skimr::skim(survey_3)
categorical_vars_3 <- names(survey_3)[sapply(survey_3, is.factor)]

ggplot(survey_3, aes_string(x = "browser", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra Browser e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Browser", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli 1,2,3,5,10
#i livelli 1,2,3,5 presentano valori molto simili per mediana e variabilità, 
#il livello 10 che ha una mediana più bassa e ha una variabilità più ampia
# gli NA hanno mediana pari a 10 e variabilità stretta superiore a 9

ggplot(survey_3, aes_string(x = "sistema_operativo", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra sistema_operativo e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "sistema_operativo", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i primi 11 livelli presentano valori molto simili per mediana e variabilità, 
#tranne il livello 6 che ha una mediana vicino al 10 e il livello 9 che ha una minore variabilità
#dal livello 12 in poi variabilità e mediana sono più eterogenei a causa dei pochi valori

ggplot(survey_3, aes_string(x = "segmento_des_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra segmento_des_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "segmento_des_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli 1,3,18,5,6
#i livelli hanno mediana pressochè identica, nel livello 6 abbiamo 
#una variabilità più ristretta 

ggplot(survey_3, aes_string(x = "nps_segment", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra nps_segment e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "nps_segment", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i promoter presentano una mediana pari a 10, i passive attorno a 8, i detractor a 5 
#con una coerente maggiore variabilità rispetto agli altri due

ggplot(survey_3, aes_string(x = "gestore", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra gestore e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "gestore", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#plot non riuscito a causa dell'eccessivo numero di livelli

ggplot(survey_3, aes_string(x = "operazione", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra operazione e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "operazione", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#l'alto numero di livelli rende difficile trarre conclusioni, ma per la maggior parte
# troviamo una variabilità compresa tra 5 e 10 con mediana superiore a 8.75, 
#qualche livello presenta una minore variabilità e una mediana attorno ai 10


ggplot(survey_3, aes_string(x = "regione_des_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra regione_des_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "regione_des_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#menzione per le regioni 9,13,118, 17,18,19 che hanno una mediana pari a 10
#la regione 20 è l'unica con mediana pari a 7,
#la variabilità è abbastanza omogenea, a parte per 118, 17, 18, 19, 20
#probabilmente a causa del ridotto numero di valori presenti

ggplot(survey_3, aes_string(x = "area", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra area e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "area", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#quasi tutti i livelli presentano la stessa variabilità e mediana pari a 9
#i livelli 27, 182, 187 hanno una mediana pari a 10,
#tutti i livelli hanno una stessa variabilità

ggplot(survey_3, aes_string(x = "direzione", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra direzione e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "direzione", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli da 2 a 9
#la variabilità è la stessa per tutti i livelli la mediana è pari a 9 
#per il livello 7 la mediana è pari a 10

ggplot(survey_3, aes_string(x = "filiale", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra filiale e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "filiale", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#troppi valori differenti per trarre delle considerazioni

ggplot(survey_3, aes_string(x = "email", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra email e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "email", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pari a 9 e variabilità tra 5 e 10, tutti i record hanno questo valore

ggplot(survey_3, aes_string(x = "customer_segment", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra customer_segment e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "customer_segment", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli 1,2,3,18,5
#i primi 18 livelli hanno identica mediana pari a 9 e variabilità 
#livello 5 con stessa mediana ma variabilità più ristretta

ggplot(survey_3, aes_string(x = "attivita_eco_des_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra attivita_eco_des_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "attivita_eco_des_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#eccessivo numero di valori per dedurre qualcosa

ggplot(survey_3, aes_string(x = "sesso_code_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra sesso_code_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "sesso_code_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabile poco significativa perchè entrambi livelli e gli NA hanno
#la stessa mediana e variabilità pari a 9 e variabilità tra 5 and 10

ggplot(survey_3, aes_string(x = "fascia_eta_code_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_eta_code_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_eta_code_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli da 1 a 7
#tutti i livelli presentano stessa variabilità e mediana, poca significatività

ggplot(survey_3, aes_string(x = "fascia_anzianita_code_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_anzianita_code_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_anzianita_code_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i 7 livelli e gli NA hanno stessa mediana e variabilità

ggplot(survey_3, aes_string(x = "fascia_fatturato_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_fatturato_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_fatturato_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i livelli 2 e 5 presentano minore variabilità 
#i livelli 1,3,18 e NA hanno la stessa variabilità
#tutti i livelli hanno mediana pari a 9

ggplot(survey_3, aes_string(x = "xntweb_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xntweb_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xntweb_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per entrambi il livello 1 e anche per gli NA

ggplot(survey_3, aes_string(x = "cliente_con_mutuo_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_con_mutuo_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_con_mutuo_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per entrambi i livelli 
#e anche per gli NA

ggplot(survey_3, aes_string(x = "cliente_investitore_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_investitore_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_investitore_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità identica per i livelli 1 e 2 e mediana pari a 9
#gli NA hanno una mediana pari a 10 e variabilità ridotta

ggplot(survey_3, aes_string(x = "cliente_solo_cc_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_solo_cc_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_solo_cc_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità identica per i livelli 1 e 2 e mediana pari a 9
#gli NA hanno una mediana pari a 10 e variabilità ridotta

ggplot(survey_3, aes_string(x = "in_bonis_flg_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra in_bonis_flg_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "in_bonis_flg_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pari a 9 e variabilità tra 5 e 10

ggplot(survey_3, aes_string(x = "multi_flg_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra multi_flg_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "multi_flg_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per il livello 1 e per gli NA
#con mediana pari a 9 e variabilità tra 5 e 10

ggplot(survey_3, aes_string(x = "risk_rating_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra risk_rating_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "risk_rating_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli da 1 a 7
#tutti con mediana 9 e variabilità tra 5 e 10
#gli NA hanno variabilità e mediana simile agli altri

ggplot(survey_3, aes_string(x = "fascia_tr_code_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_tr_code_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_tr_code_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 con una mediana attorno a 8 e una variabilità fino a 5
#livello 2 con una mediana pari a 10 e variabilità ristretta
#livello 3 con una mediana attorno a 8, ma ampia variabilità
#NA con mediana pari a 9 e variabilità come livello 1

ggplot(survey_3, aes_string(x = "fascia_utilizzo_online_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_utilizzo_online_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_utilizzo_online_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#NA e i livelli fino al 3 hanno la stessa variabilità, però il livello 2
#ha una mediana pari a 10

ggplot(survey_3, aes_string(x = "nps_factors_survey_filiale", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra nps_factors_survey_filiale e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "nps_factors_survey_filiale", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#troppi valori per dedurre qualcosa

ggplot(survey_3, aes_string(x = "reason_for_score_nps_filiale", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra reason_for_score_nps_filiale e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "reason_for_score_nps_filiale", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#NPS filiale e NA con identica variabilità e mediana pari a 9

ggplot(survey_3, aes_string(x = "disservices_description", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra disservices_description e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "disservices_description", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#il commento ha una mediana più bassa e una variabilità maggiore
#la mediana del commento ha valore 8, mentre gli NA superiore a 9

ggplot(survey_3, aes_string(x = "easy_support", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra easy_support e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "easy_support", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pari a 9 e variabilità fino a 5 

ggplot(survey_3, aes_string(x = "service_satisfaction", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra service_satisfaction e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "service_satisfaction", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pari a 9 e variabilità fino a 5 

####analisi esploratoria bivariata survey 4 ####
str(survey_4)
categorical_vars_4 <- names(survey_4)[sapply(survey_4, is.factor)]
names(survey_4)

ggplot(survey_4, aes_string(x = "sistema_operativo", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra sistema_operativo e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "sistema_operativo", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#unico livello 4 con mediana pari a 10 e variabilità fino a 5

ggplot(survey_4, aes_string(x = "segmento_des_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra segmento_des_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "segmento_des_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli 1,2,3,4,5,7,9
#livelli 1, 5,7,9 hanno mediana pari a 10 e variabilità ridotta per 3,4,5,7,9
#mediana pari a 9 per 2,3 

ggplot(survey_4, aes_string(x = "nps_segment", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra nps_segment e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "nps_segment", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i promoter presentano una mediana pari a 10, i passive attorno a 8, i detractor a 5 
#ma con una maggiore variabilità rispetto agli altri due coerentemente

names(survey_4)

ggplot(survey_4, aes_string(x = "regione_des_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra regione_des_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "regione_des_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#menzione per le regioni 9,13,15, 17,4,19,20 che hanno una mediana pari a 10
#le regioni 10 e 16  hanno mediana al di sotto di 8.75,
#la variabilità è abbastanza omogenea, a parte per 4,6,9,14, 17, 4, 19, 20
#probabilmente a causa del ridotto numero di valori presenti

ggplot(survey_4, aes_string(x = "area", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra area e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "area", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#quasi tutti i livelli presentano la stessa variabilità e mediana tra 9 e 10

ggplot(survey_4, aes_string(x = "direzione", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra direzione e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "direzione", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli da 2 a 10
#la variabilità è la stessa per tutti i livelli, a parte per 9 e 10 dove è ristretta
#attorno ai valori 8 e 10. Qui la mediana è sempre superiore a 8
#i livelli 2,3,5,7,9 hanno mediana pari a 10

ggplot(survey_4, aes_string(x = "filiale", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra filiale e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "filiale", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#troppi valori differenti per trarre delle considerazioni

ggplot(survey_4, aes_string(x = "email", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra email e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "email", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i valori con o senza l'email presentano una stessa variabilità 
#la mediana per le osservazioni con la mail è 10, per gli NA è 9

ggplot(survey_4, aes_string(x = "customer_segment", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra customer_segment e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "customer_segment", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livelli 1,2,3,4,6,7,8
#i livelli 2,3,4,6,8 hanno una variabilità ristretta;
#il livello 1 e NA hanno una variabilità più ampia
#i livelli 1,4,6,8 hanno una mediana pari a 10
#i livelli 2,3,7 e NA hanno una mediana attorno a 9

ggplot(survey_4, aes_string(x = "attivita_eco_des_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra attivita_eco_des_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "attivita_eco_des_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#eccessivo numero di valori per dedurre qualcosa

ggplot(survey_4, aes_string(x = "sesso_code_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra sesso_code_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "sesso_code_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# livelli e gli NA hanno la stessa variabilità, ma il livello ha mediana 10
#mentre il livello 1 e NA è pari a 9

ggplot(survey_4, aes_string(x = "fascia_eta_code_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_eta_code_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_eta_code_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i livelli 1,2,3,7 presentano una variabilità più ristretta, 
# mediana pari a 9 tranne per i livelli 1,3,5 pari a 10

ggplot(survey_4, aes_string(x = "fascia_anzianita_code_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_anzianita_code_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_anzianita_code_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i livelli 1,2,3,7 presentano una variabilità più ristretta, 
# mediana pari a 9 tranne per i livelli 1,3,5 pari a 10

ggplot(survey_4, aes_string(x = "vdbank_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra vdbank_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "vdbank_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana identica per entrambi i livelli, mentre livello 2 ha una variabilità maggiore
#gli NA hanno la stessa variabilità pari al livello 2 con una mediana pari a 9

ggplot(survey_4, aes_string(x = "xntweb_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra xntweb_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "xntweb_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per livello 2 e NA 
#livello 1 ha la stessa variabilità degli altri, ma mediana pari a 10

ggplot(survey_4, aes_string(x = "cliente_con_mutuo_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_con_mutuo_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_con_mutuo_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità e mediana pressochè identica per entrambi i livelli 
#gli NA hanno stessa variabilità ma mediana pari a 9

ggplot(survey_4, aes_string(x = "cliente_investitore_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_investitore_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_investitore_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità identica, ma gli NA e il livello 1 hanno una mediana pari a 9,
#mentre livello 2 ha mediana pari a 10

ggplot(survey_4, aes_string(x = "cliente_solo_cc_flg_ana", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra cliente_solo_cc_flg_ana e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "cliente_solo_cc_flg_ana", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i livelli hanno mediana pari a 10, mentre NA pari a 9
#livello 1 ha una variabilità più ristretta

ggplot(survey_4, aes_string(x = "in_bonis_flg_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra in_bonis_flg_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "in_bonis_flg_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana identica pari a 10, ma variabilità ridotta per il livello 2
#NA con variabilità ampia e mediana pari a 9

ggplot(survey_4, aes_string(x = "multi_flg_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra multi_flg_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "multi_flg_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabilità identica per entrambi i livellie per gli NA
#mediana pari a 10 per 2, mentre uguale a 9 per NA e 1

ggplot(survey_4, aes_string(x = "risk_rating_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra risk_rating_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "risk_rating_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i livelli 3,5,6,9,10,11,12 hanno una mediana pari a 10
#i livelli 1,4,5,6,8, 10, 11, 12 hanno una variabilità più ridotta

ggplot(survey_4, aes_string(x = "fascia_tr_code_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_tr_code_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_tr_code_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 con una mediana attorno a 9 e una variabilità tra 7 e 10
#livello 2 con una mediana pari a 10 e variabilità ristretta
#livello 3 con una mediana attorno a 9.5 e variabilità stretta
#NA con mediana pari a 10 e variabilità come ampia

ggplot(survey_4, aes_string(x = "fascia_utilizzo_online_comm", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra fascia_utilizzo_online_comm e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "fascia_utilizzo_online_comm", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
# NA e livelli 1,2 hanno la stessa variabilità, però il livello 2
# ha una mediana pari a 10 cosi come il 3 che ha pure una variabilità ridotta

ggplot(survey_4, aes_string(x = "reason_for_score_comment", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra reason_for_score_comment e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "reason_for_score_comment", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#il commento ha una mediana pari a 10 e variabilità stretta
# NA con mediana pari a 9 e variabilità ampia

ggplot(survey_4, aes_string(x = "expenses_icon", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra expenses_icon e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "expenses_icon", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 ha una mediana pari a 10
#livello 2  mediana a 8.75
#variabilità identica

ggplot(survey_4, aes_string(x = "expenses_icon_used", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra expenses_icon_used e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "expenses_icon_used", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#i livelli 1,2 hanno mediana pari a 10
#mentre il livello 3 e  NA hanno la  mediana 9
#variabilità minore per il livello 2

ggplot(survey_4, aes_string(x = "consulted_features", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra consulted_features e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "consulted_features", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#tanti livelli con mediana pari a 10 e variabilità ridotta
#qualche outlier sotto i 7,5, altrimenti tutti superiori
#NA con variabilità fino a 5 e mediana 9

ggplot(survey_4, aes_string(x = "interested_in_functionality", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra interested_in_functionality e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "interested_in_functionality", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#variabili interessante: livello 6 ha una mediana molto bassa
#gli altri livelli hanno una mediana superiore a e variabilità stretta

ggplot(survey_4, aes_string(x = "ces_reason_for_score", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra ces_reason_for_score e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "ces_reason_for_score", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#mediana pari a 10 e variabilità ridotta per reason
#NA mediana pari a 9 e variabilità ampia

ggplot(survey_4, aes_string(x = "feature_not_used", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra feature_not_used e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "feature_not_used", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Other con mediana pari a 10 e variabilità pressochè nulla,
#gli NA hanno la solita variabilità tra 5 e 10 con mediana 10

ggplot(survey_4, aes_string(x = "feature_not_used_other", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra feature_not_used_other e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "feature_not_used_other", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Other con mediana pari a 10 e variabilità pressochè nulla,
#gli NA hanno la solita variabilità tra 5 e 10 con mediana 10

ggplot(survey_4, aes_string(x = "not_entered_in_expenses_section", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra not_entered_in_expenses_section e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "not_entered_in_expenses_section", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#livello 1 e NA con mediana pari a 10 
#livello 2,3 e 4 con variabilità tra 4 e 10 e mediana pari a 9
#livelli 5 e 6 con variabilità nulla, mediana pari a 9
#livello 8 con mediana pari a 6 e variabilità nulla

ggplot(survey_4, aes_string(x = "not_entered_in_expenses_section_other", y = "nps")) +
  geom_boxplot(na.rm = TRUE) +
  ggtitle(paste("Relazione tra not_entered_in_expenses_section_other e NPS")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "not_entered_in_expenses_section_other", y = "NPS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#other con mediana pari a 9 e variabilità ampia
#NA con mediana pari a 10 e variabilità fino a 5 



#####p-value per variabile gender#####

lm_df_clean <- lm(target~sesso_code_ana, data = df_clean)
summary(lm_df_clean)

lm_survey_1 <- lm(target~sesso_code_ana, data = survey_1)
summary(lm_survey_1)

#sesso_code_ana non presente nelle survey 2#

lm_survey_3 <- lm(target~sesso_code_ana, data = survey_3)
summary(lm_survey_3)

lm_survey_4 <- lm(target~sesso_code_ana, data = survey_4)
summary(lm_survey_4)

lm_survey_16 <- lm(target~sesso_code_ana, data = survey_16)
summary(lm_survey_16)

lm_survey_18 <- lm(target~sesso_code_ana, data = survey_18)
summary(lm_survey_18)

lm_survey_52 <- lm(target~sesso_code_ana, data = survey_52)
summary(lm_survey_52)

#####p-value per la variabile direzione#####

lm_df_clean <- lm(target~direzione, data = df_clean)
summary(lm_df_clean) #significativa

lm_survey_1 <- lm(target~direzione, data = survey_1)
summary(lm_survey_1)

lm_survey_2 <- lm(target~direzione, data = survey_2)
summary(lm_survey_2)

lm_survey_3 <- lm(target~direzione, data = survey_3)
summary(lm_survey_3)

lm_survey_4 <- lm(target~direzione, data = survey_4)
summary(lm_survey_4)

lm_survey_16 <- lm(target~direzione, data = survey_16)
summary(lm_survey_16)

lm_survey_18 <- lm(target~direzione, data = survey_18)
summary(lm_survey_18)

lm_survey_52 <- lm(target~direzione, data = survey_52)
summary(lm_survey_52)


#####p-value per la variabile regione#####

lm_df_clean <- lm(target~regione_des_ana, data = df_clean)
summary(lm_df_clean) #significativa

lm_survey_1 <- lm(target~regione_des_ana, data = survey_1)
summary(lm_survey_1)

lm_survey_2 <- lm(target~regione_des_ana, data = survey_2)
summary(lm_survey_2)

lm_survey_3 <- lm(target~regione_des_ana, data = survey_3)
summary(lm_survey_3) #significativa

lm_survey_4 <- lm(target~regione_des_ana, data = survey_4)
summary(lm_survey_4)

lm_survey_16 <- lm(target~regione_des_ana, data = survey_16)
summary(lm_survey_16)

lm_survey_18 <- lm(target~regione_des_ana, data = survey_18)
summary(lm_survey_18) #significativa

lm_survey_52 <- lm(target~regione_des_ana, data = survey_52)
summary(lm_survey_52) #significativa


table(df_clean$satisfaction,df_clean$easy_support)
lm1 <- lm(easy_support ~ satisfaction, data = df_clean)
summary(lm1) #estremamente collineari

# table(df_clean$ces, df_clean$need_satisfied)
# lm1.1 <- lm(ces ~ need_satisfied, data = df_clean)
# summary(lm1.1)

table(df_clean$direzione, df_clean$regione_des_ana)
table(df_clean$xntweb_flg_ana, df_clean$vdbank_flg_ana)
table(df_clean$browser, df_clean$sistema_operativo)
table(df_clean$cliente_investitore_flg_ana, df_clean$cliente_con_mutuo_flg_ana)


#####risk_rating_comm NA#####
sum(is.na(df_clean$risk_rating_comm)) #83%
sum(is.na(survey_1$risk_rating_comm)) #94%
sum(is.na(survey_2$risk_rating_comm)) #98%
sum(is.na(survey_3$risk_rating_comm)) #80%
sum(is.na(survey_4$risk_rating_comm)) #77%
sum(is.na(survey_16$risk_rating_comm)) #92%
sum(is.na(survey_18$risk_rating_comm)) #72%
sum(is.na(survey_52$risk_rating_comm)) #78%
#troppi NA per essere incluse nell'analisi


#####regressioni lineari#####
#####implementazione modello random effect generale#####
library(nlme)
library(nlmeU)

colSums(is.na(df_clean))
table(df_clean$target, df_clean$multi_flg_comm)

df_clean$multi_flg_comm <- as.numeric(df_clean$multi_flg_comm)
df_clean$reason_for_satisfaction <- as.numeric(df_clean$reason_for_satisfaction)
df_clean$suggestions_to_improve <- as.numeric(df_clean$suggestions_to_improve)
df_clean$reason_for_dissatisfaction <- as.numeric(df_clean$reason_for_dissatisfaction)
df_clean$vdbank_flg_ana <- as.numeric(df_clean$vdbank_flg_ana)
df_clean$xntweb_flg_ana <- as.numeric(df_clean$xntweb_flg_ana)
df_clean$email <- as.numeric(df_clean$email)
df_clean[, c("multi_flg_comm", "reason_for_satisfaction","suggestions_to_improve", "reason_for_dissatisfaction",
             "vdbank_flg_ana","xntweb_flg_ana","email"
)][is.na(df_clean[, c("multi_flg_comm","reason_for_satisfaction", "suggestions_to_improve", 
                      "reason_for_dissatisfaction","vdbank_flg_ana","xntweb_flg_ana","email")])] <- 0

df_clean$multi_flg_comm <- as.factor(df_clean$multi_flg_comm)
df_clean$reason_for_satisfaction <- as.factor(df_clean$reason_for_satisfaction)
df_clean$suggestions_to_improve <- as.factor(df_clean$suggestions_to_improve)
df_clean$reason_for_dissatisfaction <- as.factor(df_clean$reason_for_dissatisfaction)
df_clean$vdbank_flg_ana <- as.factor(df_clean$vdbank_flg_ana)
df_clean$xntweb_flg_ana <- as.factor(df_clean$xntweb_flg_ana) 
df_clean$email <- as.factor(df_clean$email)
colSums(is.na(df_clean))
table(df_clean$target, df_clean$nps_factors_survey_filiale)

lm0.0_form <- formula(target ~ browser + regione_des_ana + + segmento_des_comm + satisfaction
                      + fascia_eta_code_ana + xntweb_flg_ana+ multi_flg_comm) 
lm0.0 <- lm(lm0.0_form, data = df_clean)
summary(lm0.0)
AIC(lm0.0)
alias(lm0.0)
vif(lm0.0) #no multicollinearità

# residualPlot(lm0.0)
# shapiro.test(residuals(lm0.0)) #p-value < 0.05
# qqnorm(resid(lm0.0)) 
# qqline(resid(lm0.0), col='red', lwd=2)
# #non c'è normalità nei residui

lm0.0_form <- formula(target ~ browser + direzione + segmento_des_comm + satisfaction
                      + fascia_eta_code_ana + xntweb_flg_ana + multi_flg_comm) #regione_des_ana
lm0.0_re <- lme(lm0.0_form, random = ~1|regione_des_ana,data = df_clean, na.action = na.exclude)
summary(lm0.0_re)
#calcolo della percentuale di varianza spiegata dall'effetto random
printCoefmat(summary(lm0.0_re)$tTable, has.Pvalue = TRUE, P.values = TRUE)
intervals(lm0.0_re, which = 'fixed')
print(vc <- VarCorr(lm0.0_re), comp = c("Variance", "Std.Dev."))
VarCorr(lm0.0_re)
var_eps = as.numeric(vc[2,1])
var_eps
sd_eps <- summary(lm0.0_re)$sigma
sd_eps
var_b = as.numeric(vc[1,1])
var_b
PVRE <- var_b/(var_b+var_eps)
PVRE 
#PVRE al 0.8% per regione 
#PVRE al 0.8% per area



#####implementazione modello random effect survey 1#####
library(nlme)
library(nlmeU)

survey_1_names <- names(survey_1)
colSums(is.na(survey_1))

#features non significative
# attivita_eco_des_ana, sistema_operativo, browser, direzione, customer_segment
#segmento_des_comm, anzianita_anni_num, cliente_con_mutuo_flg_ana, cliente_investitore_flg_ana
#cliente_solo_cc_flg_ana, risk_rating_comm, fascia_utilizzo_online_comm


#note: 
# fascia_anzianita_code_ana, fascia_eta_code_ana: fascia 18 significativa
# easy_support identica alla variabile target

survey_1$multi_flg_comm <- as.numeric(survey_1$multi_flg_comm)
survey_1$reason_for_satisfaction <- as.numeric(survey_1$reason_for_satisfaction)
survey_1$suggestions_to_improve <- as.numeric(survey_1$suggestions_to_improve)
survey_1$reason_for_dissatisfaction <- as.numeric(survey_1$reason_for_dissatisfaction)
survey_1$vdbank_flg_ana <- as.numeric(survey_1$vdbank_flg_ana)
survey_1$xntweb_flg_ana <- as.numeric(survey_1$xntweb_flg_ana)

survey_1[, c("multi_flg_comm",
             "reason_for_satisfaction",
             "suggestions_to_improve",
             "reason_for_dissatisfaction",
            "vdbank_flg_ana",
            "xntweb_flg_ana"
            )][is.na(survey_1[, c("multi_flg_comm",
                                  "reason_for_satisfaction",
                                  "suggestions_to_improve",
                                  "reason_for_dissatisfaction",
                                  "vdbank_flg_ana",
                                  "xntweb_flg_ana"
                                  )])] <- 0

survey_1$multi_flg_comm <- as.factor(survey_1$multi_flg_comm)
survey_1$reason_for_satisfaction <- as.factor(survey_1$reason_for_satisfaction)
survey_1$suggestions_to_improve <- as.factor(survey_1$suggestions_to_improve)
survey_1$reason_for_dissatisfaction <- as.factor(survey_1$reason_for_dissatisfaction)
survey_1$vdbank_flg_ana <- as.factor(survey_1$vdbank_flg_ana)
survey_1$xntweb_flg_ana <- as.factor(survey_1$xntweb_flg_ana)

table(survey_1$target, survey_1$need_satisfied)

lm1.0_form <- formula(target ~ satisfaction + sesso_code_ana
                      + cs_abi_num_comm + need_satisfied
                      + suggestions_to_improve)


lm1.0 <- lm(lm1.0_form, data = survey_1)
summary(lm1.0)
alias(lm1.0)
vif(lm1.0) #no multicollinearità

# table(survey_1$need_satisfied, survey_1$reason_for_dissatisfaction)

# residualPlot(lm1.0)
# shapiro.test(residuals(lm1.0)) #p-value < 0.05
# qqnorm(resid(lm1.0)) 
# qqline(resid(lm1.0), col='red', lwd=2)
# #non c'è normalità nei residui

colSums(is.na(survey_1))

# lm1.0_re <- lme(lm1.0_form,
#            random = ~1|regione_des_ana, # area 
#            data = survey_1,
#            na.action = na.exclude)
# summary(lm1.0_re)
# #calcolo della percentuale di varianza spiegata dall'effetto random
# printCoefmat(summary(lm1.0_re)$tTable, has.Pvalue = TRUE, P.values = TRUE)
# intervals(lm1.0_re, which = 'fixed')
# 
# print(vc <- VarCorr(lm1.0_re), comp = c("Variance", "Std.Dev."))
# VarCorr(lm1.0_re)
# var_eps = as.numeric(vc[2,1])
# var_eps
# 
# sd_eps <- summary(lm1.0_re)$sigma
# sd_eps
# 
# var_b = as.numeric(vc[1,1])
# var_b
# 
# PVRE <- var_b/(var_b+var_eps)
# PVRE 
# #PVRE regione e area troppo bassi





#####implementazione modello random effect survey 2#####
library(nlme)
library(nlmeU)
library(sjPlot)

survey_2_names <- names(survey_2)
colSums(is.na(survey_2))

#features non significative
#sistema_operativo,direzione, filiale, email, area, cliente_solo_cc_flg_ana
#anzianita_anni_num, in_bonis_flg_comm, risk_rating_comm
#fascia_utilizzo_online_comm, ces_factors_other, xa_app_not_used_reason_other,
#easy_support, service_satisfaction, app_knowledge, cliente_con_mutuo_flg_ana,
#segmento_des_comm, banca_mcf_xav_des, cliente_investitore_flg_ana


#app_used collineare con app_knowledge

survey_2$email <- as.numeric(survey_2$email)
survey_2$app_used <- as.numeric(survey_2$app_used)
survey_2$banca_mcf_xav_des <- as.numeric(survey_2$banca_mcf_xav_des)
survey_2[, c("email",
             "app_used", 
             "banca_mcf_xav_des")][is.na(survey_2[, c("email",
                                                      "app_used",
                                                      "banca_mcf_xav_des")])] <- 0
survey_2$email <- as.factor(survey_2$email)
survey_2$app_used <- as.factor(survey_2$app_used)
survey_2$banca_mcf_xav_des <- as.factor(survey_2$banca_mcf_xav_des)


table(survey_2$target, survey_2$direzione)

lm2.0_form <- formula(target ~ browser + segmento_des_comm + cliente_con_mutuo_flg_ana
                       + app_used + xav_profilo_postazione) #
lm2.0 <- lm(lm2.0_form, data = survey_2) #+ app_used
summary(lm2.0)
alias(lm2.0)
vif(lm2.0) #no multicollinearità

residualPlot(lm2.0)
shapiro.test(residuals(lm2.0)) #p-value < 0.05
qqnorm(resid(lm2.0)) 
qqline(resid(lm2.0), col='red', lwd=2)
#non c'è normalità nei residui

#R squared molto basso, il modello spiega pochissima varianza
survey_2_clean <- survey_2[!is.na(survey_2$cliente_con_mutuo_flg_ana), ]
lm2.1_form <- formula(target ~ browser + segmento_des_comm + cliente_con_mutuo_flg_ana
                       + app_used) #+ app_knowledge esclusa per runnare il modello 
# lm2.1_re <- lme(lm2.1_form,
#                 random = ~1|area, #regione_des_ana
#                 data = survey_2_clean,
#                 na.action = na.exclude)
# summary(lm2.1_re)
# 
# printCoefmat(summary(lm2.1_re)$tTable, has.Pvalue = TRUE, P.values = TRUE)
# intervals(lm2.1_re)
# 
# print(vc <- VarCorr(lm2.1_re), comp = c("Variance", "Std.Dev."))
# VarCorr(lm2.1_re)
# var_eps = as.numeric(vc[2,1])
# var_eps
# 
# sd_eps <- summary(lm2.1_re)$sigma
# sd_eps
# 
# var_b = as.numeric(vc[1,1])
# var_b
# 
# PVRE <- var_b/(var_b+var_eps)
# PVRE #PVRE per la variabile regione troppo basso 
# #PVRE per Area quasi 2%

# library(sjPlot)
# library(dplyr)
# library(ggplot2)
# library(nlmeU)
# library(corrplot)
# library(nlme)
# library(lattice)
# library(plot.matrix)
# library(lme4)
# library(insight)
# library(Matrix)
# re = ranef(lm2.1_re)
# dat = data.frame(x= row.names(re),y=re[,attr(re,'effectName')])
# dotplot(reorder(x,y)~y,data=dat)
# plot(ranef(lm2.1_re))



#####implementazione modello random effect survey 3#####
library(nlme)
library(nlmeU)
library(car)

survey_3_names <- names(survey_3)
colSums(is.na(survey_3))

survey_3$fascia_fatturato_ana <- as.numeric(survey_3$fascia_fatturato_ana)
survey_3$disservices_description <- as.numeric(survey_3$disservices_description)
survey_3$reason_for_score_nps_filiale <- as.numeric(survey_3$reason_for_score_nps_filiale)
survey_3[, c("fascia_fatturato_ana", 
             "reason_for_score_nps_filiale",
             "disservices_description")][is.na(survey_3[, c("fascia_fatturato_ana",
                                                            "reason_for_score_nps_filiale",
                                                            "disservices_description")])] <- 0
survey_3$fascia_fatturato_ana <- as.factor(survey_3$fascia_fatturato_ana)
survey_3$disservices_description <- as.factor(survey_3$disservices_description)
survey_3$reason_for_score_nps_filiale <- as.factor(survey_3$reason_for_score_nps_filiale)
# table(survey_3$nps_factors_survey_filiale,survey_3$reason_for_score_nps_filiale)
#features non significative
#satisfaction, gestore, banca_mcf_prodotto_code, direzione, anzianita_anni_num,
#fascia_fatturato_ana,  cliente_investitore_flg_ana, easy_support
#cliente_solo_cc_flg_ana, risk_rating_comm, area, fascia_utilizzo_online_comm,
#email, in_bonis_flg_comm, xntweb_flg_ana, multi_flg_comm, reason_for_score_nps_filiale

#note: 
#     fascia_anzianita_code_ana troppo correlata con fascia eta code
#     sistema_operativo collineare con browser
#     area ha valori significativi. ma causa problemi di collinearità
#     fascia_tr_code_comm significativa ma elimina troppe osservazioni rendendo altre variabili insignificanti

table(survey_3$in_bonis_flg_comm)
colSums(is.na(survey_3))

lm3.0_form <- formula(target ~ browser + segmento_des_comm
                      + operazione + regione_des_ana + fascia_eta_code_ana
                      + cliente_con_mutuo_flg_ana 
                      + cs_abi_num_comm +  
                      + disservices_description
                      + nps_factors_survey_filiale) #

lm3.0 <- lm(lm3.0_form, data = survey_3)
summary(lm3.0)
alias(lm3.0)
vif(lm3.0) #no multicollinearità

# residualPlot(lm3.0)
# shapiro.test(residuals(lm3.0)) #p-value < 0.05
# qqnorm(resid(lm3.0)) 
# qqline(resid(lm3.0), col='red', lwd=2)
# #non c'è normalità nei residui

survey_3 <- survey_3[!is.na(survey_3$cliente_con_mutuo_flg_ana), ]
survey_3 <- survey_3[!is.na(survey_3$cs_abi_num_comm), ]
survey_3 <- survey_3[!is.na(survey_3$nps_factors_survey_filiale), ]
survey_3 <- survey_3[!is.na(survey_3$fascia_eta_code_ana), ]
survey_3 <- survey_3[!is.na(survey_3$operazione), ]
survey_3 <- survey_3[!is.na(survey_3$reason_for_score_nps_filiale), ]
survey_3 <- survey_3[!is.na(survey_3$regione_des_ana), ]
survey_3 <- survey_3[!is.na(survey_3$sistema_operativo), ]
survey_3 <- survey_3[!is.na(survey_3$target), ]
colSums(is.na(survey_3))

lm3.0_form <- formula(target ~ browser + segmento_des_comm
                      + operazione + fascia_eta_code_ana
                      + cliente_con_mutuo_flg_ana + reason_for_score_nps_filiale
                      + cs_abi_num_comm + nps_factors_survey_filiale 
                      + disservices_description)

table(survey_3$regione_des_ana)
lm3.0_re <- lme(lm3.0_form,
                random = ~1|regione_des_ana, #  area
                data = survey_3)

summary(lm3.0_re)

printCoefmat(summary(lm3.0_re)$tTable, has.Pvalue = TRUE, P.values = TRUE)
intervals(lm3.0_re)

print(vc <- VarCorr(lm3.0_re), comp = c("Variance", "Std.Dev."))
VarCorr(lm3.0_re)
var_eps = as.numeric(vc[2,1])
var_eps

sd_eps <- summary(lm3.0_re)$sigma
sd_eps

var_b = as.numeric(vc[1,1])
var_b

PVRE <- var_b/(var_b+var_eps)
PVRE 
#PVRE 0.14% per regione 
#PVRE 0.04% per area 


#####implementazione modello random effect survey 4#####
library(nlme)
library(nlmeU)
library(car)

survey_4_names <- names(survey_4)
colSums(is.na(survey_4))

survey_4$email <- as.numeric(survey_4$email)
survey_4$reason_for_score_comment <- as.numeric(survey_4$reason_for_score_comment)
survey_4[, c("email",
             "reason_for_score_comment")][is.na(survey_4[, c("email",
                                                             "reason_for_score_comment")])] <- 0
survey_4$email <- as.factor(survey_4$email)
survey_4$reason_for_score_comment <- as.factor(survey_4$reason_for_score_comment)

#features non significative
# area, direzione, filiale, email, attivita_eco_des_ana, anzianita_anni_num
#vdbank_flg_ana, cliente_con_mutuo_flg_ana, cliente_solo_cc_flg_ana,
#in_bonis_flg_comm, risk_rating_comm, fascia_tr_code_comm,
#fascia_utilizzo_online_comm, easy_support, service_satisfaction,
#reason_for_score_comment, expenses_icon, expenses_icon_used
#sistema_operativo,satisfaction

#note: 
#troppi NA: not_entered_in_expenses_section_other,feature_not_used_other, 
# not_entered_in_expenses_section, feature_not_used, ces_reason_for_score
#interested_in_functionality, consulted_features, reason_for_score_comment
#expenses_icon_used, fascia_tr_code_comm, risk_rating_comm

#customer segment e segmento_des_comm troppo collineari
#fascia_anzianita_code_ana e fascia_eta_code_ana identiche/troppo collineari

colSums(is.na(survey_4))
table(survey_4$customer_segment, survey_4$segmento_des_comm)


lm4.0_form <- formula(target ~ regione_des_ana  + segmento_des_comm 
                      + sesso_code_ana + fascia_eta_code_ana
                      + xntweb_flg_ana + cliente_investitore_flg_ana
                      + multi_flg_comm )
lm4.0 <- lm(lm4.0_form, data = survey_4)
summary(lm4.0)
alias(lm4.0)
vif(lm4.0) #no multicollinearità

# residualPlot(lm4.0)
# shapiro.test(residuals(lm4.0)) #p-value < 0.05
# qqnorm(resid(lm4.0)) 
# qqline(resid(lm4.0), col='red', lwd=2)

colSums(is.na(survey_4))
survey_4 <- survey_4[!is.na(survey_4$segmento_des_comm), ]
survey_4 <- survey_4[!is.na(survey_4$fascia_eta_code_ana), ]
survey_4 <- survey_4[!is.na(survey_4$cliente_investitore_flg_ana), ]
survey_4 <- survey_4[!is.na(survey_4$multi_flg_comm), ]
survey_4 <- survey_4[!is.na(survey_4$xntweb_flg_ana), ]
survey_4 <- survey_4[!is.na(survey_4$regione_des_ana), ]
survey_4 <- survey_4[!is.na(survey_4$sesso_code_ana), ]
survey_4 <- survey_4[!is.na(survey_4$target), ]
colSums(is.na(survey_4))

lm4.0_form <- formula(target ~ segmento_des_comm 
                      + sesso_code_ana + fascia_eta_code_ana
                      + xntweb_flg_ana + cliente_investitore_flg_ana
                      + multi_flg_comm)

table(survey_4$regione_des_ana)
lm4.0_re <- lme(lm4.0_form,
                random = ~1|regione_des_ana, # area 
                data = survey_4)

summary(lm4.0_re)

printCoefmat(summary(lm4.0_re)$tTable, has.Pvalue = TRUE, P.values = TRUE)
intervals(lm4.0_re)

print(vc <- VarCorr(lm4.0_re), comp = c("Variance", "Std.Dev."))
VarCorr(lm4.0_re)
var_eps = as.numeric(vc[2,1])
var_eps

sd_eps <- summary(lm4.0_re)$sigma
sd_eps

var_b = as.numeric(vc[1,1])
var_b

PVRE <- var_b/(var_b+var_eps)
PVRE #PVRE per la variabile regione e area troppo bassi


#####implementazione modello random effect survey 16#####
library(nlme)
library(nlmeU)
library(car)

survey_16_names <- names(survey_16)
colSums(is.na(survey_16))

survey_16$email <- as.numeric(survey_16$email)
survey_16$reason_for_score_comment <- as.numeric(survey_16$reason_for_score_comment)
survey_16[, c("email",
             "reason_for_score_comment")][is.na(survey_16[, c("email",
                                                             "reason_for_score_comment")])] <- 0
survey_16$email <- as.factor(survey_16$email)
survey_16$reason_for_score_comment <- as.factor(survey_16$reason_for_score_comment)

#features non significative
# browser, sistema_operativo, regione_des_ana, area, direzione,
# filiale, attivita_eco_des_ana, sesso_code_ana, anzianita_anni_num,
# vdbank_flg_ana, xntweb_flg_ana, cliente_con_mutuo_flg_ana,
#cliente_investitore_flg_ana, cliente_solo_cc_flg_ana,
#in_bonis_flg_comm, segmento_des_comm, multi_flg_comm,
#fascia_utilizzo_online_comm, reason_for_score_comment
#app_knowledge

#note: 
#troppi NA: fascia_tr_code_comm, risk_rating_comm, nps_factors_survey_canale_other
#           other_issue_type, email
#nps_segment collineare con la variabile target
#customer_segment collineare con la variabile segmento_des_comm
#fascia_eta_code_ana e fascia.anzianita_code_ana collineari
#nps_factors_survey_canale annulla la significatività di app_knowledge
#fascia_eta_code_ana ha alcune fasce signiicative ma ha un VIF superiore a 5
# se inserito nel modello con nps_factors_survey_canale

# da analizzare: 
#xv_va_app_not_used_reason

          
colSums(is.na(survey_16))
table(survey_16$xv_va_app_not_used_reason)
table(survey_16$app_used, survey_16$app_knowledge)
unique(survey_16$nps_factors_survey_canale)

lm16.0_form <- formula(target ~ app_used + 
                         nps_factors_survey_canale)

lm16.0 <- lm(lm16.0_form, data = survey_16)
summary(lm16.0)
alias(lm16.0)
vif(lm16.0) #no multicollinearità

# x11()
# residualPlot(lm16.0)
# shapiro.test(resid(lm16.0))
# qqnorm(resid(lm16.0))
# qqline(resid(lm16.0), col = 'red', lwd =2)
# #nessun modello random effect perchè le variabili regione e area non sono significative



#####implementazione modello random effect survey 18#####
library(nlme)
library(nlmeU)
library(car)

survey_18_names <- names(survey_18)
str(survey_18)

survey_18$email <- as.numeric(survey_18$email)
survey_18$reason_for_score_comment <- as.numeric(survey_18$reason_for_score_comment)
survey_18[, c("email",
             "reason_for_score_comment")][is.na(survey_18[, c("email",
                                                             "reason_for_score_comment")])] <- 0
survey_18$email <- as.factor(survey_18$email)
survey_18$reason_for_score_comment <- as.factor(survey_18$reason_for_score_comment)

#features non significative
# area, direzione, filiale, customer_segment, attivita_eco_des_ana,
#anzianita_anni_num, vdbank_flg_ana, xntweb_flg_ana, cliente_con_mutuo_flg_ana
#cliente_investitore_flg_ana, cliente_solo_cc_flg_ana, in_bonis_flg_comm

#note: 
# troppi NA : not_entered_in_expenses_section_other, feature_not_used_other
# not_entered_in_expenses_section, ces_reason_for_score, feature_not_used,
# consulted_features, interested_in_functionality, expenses_icon_used, expenses_icon
#risk_rating_comm, fascia_tr_code_comm, reason_for_score_comment

colSums(is.na(survey_18))
table(survey_18$reason_for_score_comment, survey_18$target)


lm18.0_form <- formula(target ~ segmento_des_comm + regione_des_ana
                       + sesso_code_ana + fascia_eta_code_ana 
                       + multi_flg_comm + fascia_utilizzo_online_comm)
lm18.0 <- lm(lm18.0_form, data = survey_18)
summary(lm18.0)
alias(lm18.0)
vif(lm18.0) #no multicollinearità

# residualPlot(lm18.0)
# shapiro.test(resid(lm18.0))
# qqnorm(resid(lm18.0))
# qqline(resid(lm18.0), col = 'red', lwd = 2)

colSums(is.na(survey_18))
survey_18 <- survey_18[!is.na(survey_18$segmento_des_comm), ]
survey_18 <- survey_18[!is.na(survey_18$fascia_eta_code_ana), ]
survey_18 <- survey_18[!is.na(survey_18$fascia_utilizzo_online_comm), ]
survey_18 <- survey_18[!is.na(survey_18$multi_flg_comm), ]
survey_18 <- survey_18[!is.na(survey_18$regione_des_ana), ]
survey_18 <- survey_18[!is.na(survey_18$sesso_code_ana), ]
survey_18 <- survey_18[!is.na(survey_18$target), ]
colSums(is.na(survey_18))

lm18.0_form <- formula(target ~ segmento_des_comm 
                       + sesso_code_ana + fascia_eta_code_ana 
                       + multi_flg_comm + fascia_utilizzo_online_comm)

table(survey_18$regione_des_ana)
lm18.0_re <- lme(lm18.0_form,
                random = ~1|area, # regione_des_ana
                data = survey_18)

summary(lm18.0_re)

printCoefmat(summary(lm18.0_re)$tTable, has.Pvalue = TRUE, P.values = TRUE)
intervals(lm18.0_re)

print(vc <- VarCorr(lm18.0_re), comp = c("Variance", "Std.Dev."))
VarCorr(lm18.0_re)
var_eps = as.numeric(vc[2,1])
var_eps

sd_eps <- summary(lm18.0_re)$sigma
sd_eps

var_b = as.numeric(vc[1,1])
var_b

PVRE <- var_b/(var_b+var_eps)
PVRE 
#PVRE 0.4% per regione 
#PVRE 0.1% per area 



#####implementazione modello random effect survey 52#####
library(nlme)
library(nlmeU)
library(car)

survey_52_names <- names(survey_52)
str(survey_52)

survey_52$email <- as.numeric(survey_52$email)
survey_52$reason_for_score_comment <- as.numeric(survey_52$reason_for_score_comment)
survey_52[, c("email",
              "reason_for_score_comment")][is.na(survey_52[, c("email",
                                                               "reason_for_score_comment")])] <- 0
survey_52$email <- as.factor(survey_52$email)
survey_52$reason_for_score_comment <- as.factor(survey_52$reason_for_score_comment)

#features non significative
#browser, area, direzione, filiale, attivita_eco_des_ana, sesso_code_ana,
#vdbank_flg_ana, xntweb_flg_ana, cliente_con_mutuo_flg_ana, anzianita_anni_num
#cliente_investitore_flg_ana, cliente_solo_cc_flg_ana, in_bonis_flg_comm,
#fascia_utilizzo_online_comm, xv_va_app_not_used_reason, reason_for_score_comment

#note: 
# troppi NA : fascia_tr_code_comm, nps_factors_survey_canale_other, 
#xv_va_app_not_used_reason, other_issue_type, risk_rating_comm

colSums(is.na(survey_52))
table(survey_52$sistema_operativo, survey_52$target)


lm52.0_form <- formula(target ~ sistema_operativo + regione_des_ana
                       +segmento_des_comm+ email + fascia_eta_code_ana
                       + app_knowledge )

#nps_factors_survey_canale
lm52.0 <- lm(lm52.0_form, data = survey_52)
summary(lm52.0)
alias(lm52.0)
vif(lm52.0) #multicollinearità per nps_factors_survey_canale

# residualPlot(lm52.0)
# shapiro.test(residuals(lm52.0)) #p-value <0.05
# qqnorm(resid(lm52.0))
# qqline(resid(lm52.0), col = 'red', lwd = 2) #no normalità

colSums(is.na(survey_52))
survey_52 <- survey_52[!is.na(survey_52$segmento_des_comm), ]
survey_52 <- survey_52[!is.na(survey_52$sistema_operativo), ]
survey_52 <- survey_52[!is.na(survey_52$fascia_eta_code_ana), ]
survey_52 <- survey_52[!is.na(survey_52$anzianita_anni_num), ]
survey_52 <- survey_52[!is.na(survey_52$email), ]
survey_52 <- survey_52[!is.na(survey_52$app_knowledge), ]
survey_52 <- survey_52[!is.na(survey_52$regione_des_ana), ]
survey_52 <- survey_52[!is.na(survey_52$nps_factors_survey_canale), ]
survey_52 <- survey_52[!is.na(survey_52$target), ]
colSums(is.na(survey_52))

lm52.0_form <- formula(target ~ sistema_operativo + 
                       +segmento_des_comm + email + fascia_eta_code_ana
                       + anzianita_anni_num + 
                        + app_knowledge + nps_factors_survey_canale)

table(survey_52$regione_des_ana)
lm52.0_re <- lme(lm52.0_form,
                 random = ~1|regione_des_ana, #area 
                 data = survey_52)

summary(lm52.0_re)

printCoefmat(summary(lm52.0_re)$tTable, has.Pvalue = TRUE, P.values = TRUE)
intervals(lm52.0_re)

print(vc <- VarCorr(lm52.0_re), comp = c("Variance", "Std.Dev."))
VarCorr(lm52.0_re)
var_eps = as.numeric(vc[2,1])
var_eps

sd_eps <- summary(lm52.0_re)$sigma
sd_eps

var_b = as.numeric(vc[1,1])
var_b

PVRE <- var_b/(var_b+var_eps)
PVRE 
#PVRE per la variabile regione troppo basso
#PVRE 0.2% per area









#####regressioni logistiche#####
#####regressione logistica generale######

df_clean$multi_flg_comm <- as.numeric(df_clean$multi_flg_comm)
df_clean$reason_for_satisfaction <- as.numeric(df_clean$reason_for_satisfaction)
df_clean$suggestions_to_improve <- as.numeric(df_clean$suggestions_to_improve)
df_clean$reason_for_dissatisfaction <- as.numeric(df_clean$reason_for_dissatisfaction)
df_clean$vdbank_flg_ana <- as.numeric(df_clean$vdbank_flg_ana)
df_clean$xntweb_flg_ana <- as.numeric(df_clean$xntweb_flg_ana)
df_clean$email <- as.numeric(df_clean$email)
df_clean[, c("multi_flg_comm", "reason_for_satisfaction","suggestions_to_improve", "reason_for_dissatisfaction", "vdbank_flg_ana",
             "xntweb_flg_ana", "email" 
             )][is.na(df_clean[, c("multi_flg_comm","reason_for_satisfaction", "suggestions_to_improve",  "reason_for_dissatisfaction",
              "vdbank_flg_ana","xntweb_flg_ana","email")])] <- 0


df_clean$multi_flg_comm <- as.factor(df_clean$multi_flg_comm)
df_clean$reason_for_satisfaction <- as.factor(df_clean$reason_for_satisfaction)
df_clean$suggestions_to_improve <- as.factor(df_clean$suggestions_to_improve)
df_clean$reason_for_dissatisfaction <- as.factor(df_clean$reason_for_dissatisfaction)
df_clean$vdbank_flg_ana <- as.factor(df_clean$vdbank_flg_ana)
df_clean$xntweb_flg_ana <- as.factor(df_clean$xntweb_flg_ana) 
df_clean$email <- as.factor(df_clean$email)

names(df_clean)
colSums(is.na(df_clean))
table(df_clean$target_bin.f, df_clean$in_bonis_flg_comm)

lg0.0_form <- formula(target_bin.f ~  sistema_operativo + operazione + segmento_des_comm  + regione_des_ana
                                     + fascia_eta_code_ana + cliente_con_mutuo_flg_ana) 

lg0.0 <- glm(lg0.0_form,  data = df_clean, family = 'binomial')
summary(lg0.0)
alias(lg0.0)
vif(lg0.0) 
predizioni <- predict(lg0.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

df_clean_non_NA <- df_clean[complete.cases(df_clean[, c("segmento_des_comm","sistema_operativo",  "regione_des_ana", "operazione",
                                                         "fascia_eta_code_ana","cliente_con_mutuo_flg_ana")]), ]
confusion_matrix <- table(Predicted = previsioni_binarie, Actual = df_clean_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision) #0.59
print(accuracy) #0.58

help("plot")
library(pROC)
roc_curve <- roc(df_clean_non_NA$target_bin.f, predizioni, levels = c(0, 1), direction = "<")
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
plot.roc(roc_curve, lwd = 2, main = "Receiver Operating Characteristic (ROC) Curve",
     xlim = c(0,1), ylim = c(0,1), add=FALSE, reuse.auc=TRUE,
     axes=TRUE, legacy.axes=FALSE)
# abline(a = 0, b = 1, col = "navy", lty = 2, lwd = 2)
legend("topright", legend = paste("AUC =", round(auc_value, 2)),lwd = 2)

#variabili non significative:
#satisfaction, gestore, email, area, direzione,sesso_code_ana
#vdbank_flg_ana, xntweb_flg_ana, cliente_investitore_flg_ana
#cliente_solo_cc_flg_ana, in_bonis_flg_comm, multi_flg_comm
#browser collineare con sistema operativo

#note: operazione ha molte variabili significative e il modello ha un AIC minore,
    # ma anche tanti NA, quindi quando inserita nel modello riduce l'accuratezza delle previsioni di un 3%


#####regressione logistica survey_1######
survey_1$multi_flg_comm <- as.numeric(survey_1$multi_flg_comm)
survey_1$reason_for_satisfaction <- as.numeric(survey_1$reason_for_satisfaction)
survey_1$suggestions_to_improve <- as.numeric(survey_1$suggestions_to_improve)
survey_1$reason_for_dissatisfaction <- as.numeric(survey_1$reason_for_dissatisfaction)
survey_1$vdbank_flg_ana <- as.numeric(survey_1$vdbank_flg_ana)
survey_1$xntweb_flg_ana <- as.numeric(survey_1$xntweb_flg_ana)

survey_1[, c("multi_flg_comm",
             "reason_for_satisfaction",
             "suggestions_to_improve",
             "reason_for_dissatisfaction",
             "vdbank_flg_ana",
             "xntweb_flg_ana"
)][is.na(survey_1[, c("multi_flg_comm",
                      "reason_for_satisfaction",
                      "suggestions_to_improve",
                      "reason_for_dissatisfaction",
                      "vdbank_flg_ana",
                      "xntweb_flg_ana"
)])] <- 0

survey_1$multi_flg_comm <- as.factor(survey_1$multi_flg_comm)
survey_1$reason_for_satisfaction <- as.factor(survey_1$reason_for_satisfaction)
survey_1$suggestions_to_improve <- as.factor(survey_1$suggestions_to_improve)
survey_1$reason_for_dissatisfaction <- as.factor(survey_1$reason_for_dissatisfaction)
survey_1$vdbank_flg_ana <- as.factor(survey_1$vdbank_flg_ana)
survey_1$xntweb_flg_ana <- as.factor(survey_1$xntweb_flg_ana)
names(survey_1)
colSums(is.na(survey_1))
table(survey_1$service_satisfaction, survey_1$satisfaction)
lg1.0_form <- formula(target_bin.f ~  satisfaction + sesso_code_ana )

lg1.0 <- glm(lg1.0_form, 
             data = survey_1, 
             family = 'binomial')

summary(lg1.0)
alias(lg1.0)
vif(lg1.0) 

predizioni <- predict(lg1.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

survey_1_non_NA <- survey_1[complete.cases(survey_1[, c("satisfaction", 
                                                        "sesso_code_ana")]), ]

confusion_matrix <- table(Predicted = previsioni_binarie, Actual = survey_1_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision) #0.77
print(accuracy) # 0.84

library(pROC)
roc_curve <- roc(survey_1_non_NA$target_bin.f, predizioni, levels = c(0, 1), direction = "<")
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
plot(roc_curve, lwd = 2, main = "Receiver Operating Characteristic (ROC) Curve",
     xlab = "False Negative Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensibility)",
     add=FALSE, reuse.auc=TRUE, axes=TRUE, legacy.axes=FALSE)
# abline(a = 0, b = 1, col = "navy", lty = 2, lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), lwd = 2)


#variabili non significative:
# browser, , operazione, segmento_des_comm
#regione_des_ana, area, direzione, email
#customer_segment, vdbank_flg_ana, cliente_solo_cc_flg_ana
#cliente_con_mutuo_flg_ana, cliente_investitore_flg_ana
#in_bonis_flg_comm, cs_abi_num_comm, fascia_utilizzo_online_comm

#####regressione logistica survey_2######
names(survey_2)
colSums(is.na(survey_2))
table(survey_2$target_bin.f)

survey_2$email <- as.numeric(survey_2$email)
survey_2$app_used <- as.numeric(survey_2$app_used)
survey_2$banca_mcf_xav_des <- as.numeric(survey_2$banca_mcf_xav_des)
survey_2[, c("email",
             "app_used", 
             "banca_mcf_xav_des")][is.na(survey_2[, c("email",
                                                      "app_used",
                                                      "banca_mcf_xav_des")])] <- 0
survey_2$email <- as.factor(survey_2$email)
survey_2$app_used <- as.factor(survey_2$app_used)
survey_2$banca_mcf_xav_des <- as.factor(survey_2$banca_mcf_xav_des)

minor_class <- subset(survey_2, target_bin.f == "0")
major_class <- subset(survey_2, target_bin.f == "1")
minor_class_oversampled <- minor_class[sample(nrow(minor_class), size = nrow(major_class), replace = TRUE), ]
data_balanced <- rbind(minor_class_oversampled, major_class)

table(survey_2$target_bin.f, survey_2$easy_support)
lg2.0_form <- formula(target_bin.f ~ segmento_des_comm + browser
                                    + xav_profilo_postazione 
                                    + cliente_con_mutuo_flg_ana
                                    + app_used ) #

#variabili non significative: 
#banca_mcf_xav_des, cliente_investitore_flg_ana
#, in_bonis_flg_comm, sistema_operativo, cliente_solo_cc_flg_ana
#fascia_utilizzo_online_comm, app_knowledge, easy_support

lg2.0 <- glm(lg2.0_form, 
             data = data_balanced, 
             family = 'binomial')

summary(lg2.0)
alias(lg2.0)
vif(lg2.0) 

predizioni <- predict(lg2.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

data_balanced <- data_balanced[complete.cases(data_balanced[, c("segmento_des_comm",
                                                        "xav_profilo_postazione", 
                                                        "cliente_con_mutuo_flg_ana", 
                                                        "app_used")]), ]
table(data_balanced$target_bin.f)
confusion_matrix <- table(Predicted = previsioni_binarie, Actual = data_balanced$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations 
print(precision) #0.55
print(accuracy) #0.55


#####regressione logistica survey_3######
names(survey_3)
colSums(is.na(survey_3))

survey_3$fascia_fatturato_ana <- as.numeric(survey_3$fascia_fatturato_ana)
survey_3$disservices_description <- as.numeric(survey_3$disservices_description)
survey_3$reason_for_score_nps_filiale <- as.numeric(survey_3$reason_for_score_nps_filiale)
survey_3[, c("fascia_fatturato_ana", 
             "reason_for_score_nps_filiale",
             "disservices_description")][is.na(survey_3[, c("fascia_fatturato_ana",
                                                            "reason_for_score_nps_filiale",
                                                            "disservices_description")])] <- 0
survey_3$fascia_fatturato_ana <- as.factor(survey_3$fascia_fatturato_ana)
survey_3$disservices_description <- as.factor(survey_3$disservices_description)
survey_3$reason_for_score_nps_filiale <- as.factor(survey_3$reason_for_score_nps_filiale)

table(survey_3$target_bin.f, survey_3$reason_for_score_nps_filiale)
lg0.0_form <- formula(target_bin.f ~ browser + segmento_des_comm
                                    + operazione + regione_des_ana
                                    + fascia_eta_code_ana
                                    + cliente_con_mutuo_flg_ana
                                    + cs_abi_num_comm
                                    + fascia_utilizzo_online_comm
                                    + nps_factors_survey_filiale
                                    + reason_for_score_nps_filiale
                                    )

#fascia_tr_code_comm sarebbe molto significativa, ma elimina il 60% delle osservazioni

lg0.0 <- glm(lg0.0_form, 
             data = survey_3, 
             family = 'binomial')

summary(lg0.0)
alias(lg0.0)
vif(lg0.0) #no multicollinearità

predizioni <- predict(lg0.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

survey_3_non_NA <- survey_3[complete.cases(survey_3[, c("browser",
                                                        "segmento_des_comm", 
                                                        "operazione", 
                                                        "regione_des_ana",
                                                        "fascia_eta_code_ana",
                                                        "cliente_con_mutuo_flg_ana",
                                                        "cs_abi_num_comm",
                                                        "fascia_utilizzo_online_comm",
                                                        "nps_factors_survey_filiale",
                                                        "reason_for_score_nps_filiale")]), ]

confusion_matrix <- table(Predicted = previsioni_binarie, Actual = survey_3_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision)
print(accuracy) #0.65

#variabili non significative: 
#sistema_operativo, area, email, sesso_code_ana
#xntweb_flg_ana, cliente_investitore_flg_ana
#cliente_solo_cc_flg_ana, in_bonis_flg_comm, direzione 

#####regressione logistica survey_4#####

names(survey_4)
colSums(is.na(survey_4))

survey_4$email <- as.numeric(survey_4$email)
survey_4$reason_for_score_comment <- as.numeric(survey_4$reason_for_score_comment)
survey_4[, c("email",
             "reason_for_score_comment")][is.na(survey_4[, c("email",
                                                             "reason_for_score_comment")])] <- 0
survey_4$email <- as.factor(survey_4$email)
survey_4$reason_for_score_comment <- as.factor(survey_4$reason_for_score_comment)

table(survey_4$target_bin.f)
table(survey_4$target_bin.f, survey_4$expenses_icon_used)
lg4.0_form <- formula(target_bin.f ~ segmento_des_comm
                                    + area + fascia_eta_code_ana
                                    + expenses_icon)

lg4.0 <- glm(lg4.0_form, 
             data = survey_4, 
             family = 'binomial')

summary(lg4.0)
alias(lg4.0)
vif(lg4.0) #no multicollinearità

predizioni <- predict(lg4.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

survey_4_non_NA <- survey_4[complete.cases(survey_4[, c("segmento_des_comm",
                                                        "area", 
                                                        "expenses_icon", 
                                                        "regione_des_ana",
                                                        "fascia_eta_code_ana")]), ]

confusion_matrix <- table(Predicted = previsioni_binarie, Actual = survey_4_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision) #0.72
print(accuracy) #0.71

#roc curve
library(pROC)
roc_curve <- roc(survey_4_non_NA$target_bin.f, predizioni, levels = c(0, 1), direction = "<")
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
plot(roc_curve, lwd = 2, main = "Receiver Operating Characteristic (ROC) Curve",
     xlab = "False Negative Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensibility)",
         add=FALSE, reuse.auc=TRUE, axes=TRUE, legacy.axes=FALSE)
# abline(a = 0, b = 1, col = "navy", lty = 2, lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), lwd = 2)


#variabili non significative: 
#sistema_operativo, regione_des_ana, direzione, email
#sesso_code_ana, vdbank_flg_ana, xntweb_flg_ana, 
#cliente_con_mutuo_flg_ana, cliente_investitore_flg_ana
#cliente_solo_cc_flg_ana, in_bonis_flg_comm
#multi_flg_comm, fascia_utilizzo_online_comm

#####regressione logistica survey_16#####

names(survey_16)
colSums(is.na(survey_16))

survey_16$email <- as.numeric(survey_16$email)
survey_16$reason_for_score_comment <- as.numeric(survey_16$reason_for_score_comment)
survey_16[, c("email",
              "reason_for_score_comment")][is.na(survey_16[, c("email",
                                                               "reason_for_score_comment")])] <- 0
survey_16$email <- as.factor(survey_16$email)
survey_16$reason_for_score_comment <- as.factor(survey_16$reason_for_score_comment)

table(survey_16$target_bin.f)
table(survey_16$target_bin.f, survey_16$app_used)
lg0.0_form <- formula(target_bin.f ~ sistema_operativo + fascia_eta_code_ana)

lg0.0 <- glm(lg0.0_form, 
             data = survey_16, 
             family = 'binomial')

summary(lg0.0)
alias(lg0.0)
vif(lg0.0) #no multicollinearità

predizioni <- predict(lg0.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

survey_16_non_NA <- survey_16[complete.cases(survey_16[, c("sistema_operativo",
                                                        "fascia_eta_code_ana" )]), ]

confusion_matrix <- table(Predicted = previsioni_binarie, Actual = survey_16_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision) #0.60
print(accuracy) #0.60

#variabili non significative: 
#browser, segmento_des_comm, regione_des_ana
#area, direzione, email, customer_segment
#sesso_code_ana, vdbank_flg_ana, xntweb_flg_ana
#cliente_con_mutuo_flg_ana, cliente_investitore_flg_ana
#cliente_solo_cc_flg_ana, in_bonis_flg_comm, multi_flg_comm
#fascia_utilizzo_online_comm, app_knowledge, 
#app_used, nps_factors_survey_canale

#####regressione logistica survey_18#####
names(survey_18)
colSums(is.na(survey_18))
survey_18$email <- as.numeric(survey_18$email)
survey_18$reason_for_score_comment <- as.numeric(survey_18$reason_for_score_comment)
survey_18[, c("email",
              "reason_for_score_comment")][is.na(survey_18[, c("email",
                                                               "reason_for_score_comment")])] <- 0
survey_18$email <- as.factor(survey_18$email)
survey_18$reason_for_score_comment <- as.factor(survey_18$reason_for_score_comment)

table(survey_18$target_bin.f) 
# minor_class <- subset(survey_18, target_bin.f == "0")
# major_class <- subset(survey_18, target_bin.f == "1")
# minor_class_oversampled <- minor_class[sample(nrow(minor_class), size = nrow(major_class), replace = TRUE), ]
# data_balanced <- rbind(minor_class_oversampled, major_class)
table(survey_18$target_bin.f, survey_18$expenses_icon)
lg18.0_form <- formula(target_bin.f ~ segmento_des_comm + regione_des_ana
                                    + sesso_code_ana + fascia_eta_code_ana
                                    + reason_for_score_comment)

lg18.0 <- glm(lg18.0_form, 
             data = survey_18, 
             family = 'binomial')

summary(lg18.0)
alias(lg18.0)
vif(lg18.0) #no multicollinearità

predizioni <- predict(lg18.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

survey_18_non_NA <- survey_18[complete.cases(survey_18[, c("segmento_des_comm",
                                                           "fascia_eta_code_ana",
                                                           "regione_des_ana", 
                                                           "sesso_code_ana",
                                                           "reason_for_score_comment"
                                                           )]), ]

confusion_matrix <- table(Predicted = previsioni_binarie, Actual = survey_18_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision) #0.75
print(accuracy) #0.75

library(pROC)
roc_curve <- roc(survey_18_non_NA$target_bin.f, predizioni, levels = c(0, 1), direction = "<")
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
plot(roc_curve, lwd = 2, main = "Receiver Operating Characteristic (ROC) Curve",
     xlab = "False Negative Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensibility)",
     add=FALSE, reuse.auc=TRUE, axes=TRUE, legacy.axes=FALSE)
# abline(a = 0, b = 1, col = "navy", lty = 2, lwd = 2)
legend("bottomright", legend = paste("AUC =", round(auc_value, 2)), lwd = 2)

#variabili non significative: 
#sistema_operativo, area, direzione, email
#vdbank_flg_ana, cliente_con_mutuo_flg_ana
#cliente_investitore_flg_ana, cliente_solo_cc_flg_ana
#in_bonis_flg_comm, multi_flg_comm
#fascia_utilizzo_online_comm, easy_support

#####regressione logistica survey_52#####
survey_52$email <- as.numeric(survey_52$email)
survey_52$xv_va_app_not_used_reason <- as.numeric(survey_52$xv_va_app_not_used_reason)
survey_52$reason_for_score_comment <- as.numeric(survey_52$reason_for_score_comment)
survey_52[, c("email",
              "xv_va_app_not_used_reason",
              "reason_for_score_comment")][is.na(survey_52[, c("email",
                                                               "xv_va_app_not_used_reason",
                                                               "reason_for_score_comment")])] <- 0
survey_52$email <- as.factor(survey_52$email)
survey_52$reason_for_score_comment <- as.factor(survey_52$reason_for_score_comment)
survey_52$xv_va_app_not_used_reason <- as.factor(survey_52$xv_va_app_not_used_reason)

names(survey_52)
colSums(is.na(survey_52))

table(survey_52$target_bin.f, survey_52$app_used)
lg52.0_form <- formula(target_bin.f ~ browser + xv_va_app_not_used_reason
                                     + segmento_des_comm
                                     + regione_des_ana + fascia_eta_code_ana
                                     + app_knowledge )

lg52.0 <- glm(lg52.0_form, 
             data = survey_52, 
             family = 'binomial')

summary(lg52.0)
alias(lg52.0)
vif(lg52.0) #no multicollinearità

#sistema_operativo collineare con browser

predizioni <- predict(lg52.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

survey_52_non_NA <- survey_52[complete.cases(survey_52[, c("segmento_des_comm",
                                                           "fascia_eta_code_ana",
                                                           "regione_des_ana", 
                                                           "xv_va_app_not_used_reason",
                                                           "browser",
                                                           "app_knowledge"
)]), ]

confusion_matrix <- table(Predicted = previsioni_binarie, Actual = survey_52_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision) #0.62
print(accuracy) #0.61

#variabili non significative: 
# area, direzione,email, cliente_con_mutuo_flg_ana
#sesso_code_ana, vdbank_flg_ana, cliente_solo_cc_flg_ana
#xntweb_flg_ana, cliente_investitore_flg_ana
#in_bonis_flg_comm,multi_flg_comm, fascia_utilizzo_online_comm
#nps_factors_survey_canale, app_used





######implementazione modelli rf #####
#####random forest generale#####
library(randomForest)
colSums(is.na(df_clean))
table(df_clean$target, df_clean$operazione)

set.seed(123)
rf_model <- randomForest(target ~ segmento_des_comm + operazione + cs_abi_num_comm +  fascia_eta_code_ana,
                         data = df_clean,importance = TRUE, ntree = 300, na.action = na.exclude)
importance(rf_model)
varImpPlot(rf_model)

#var_explained: 3.29%
#MSE minima: 6.49

print(rf_model)


#####random forest survey 1#####
library(randomForest)
colSums(is.na(survey_1))
table(survey_1$target, survey_1$cliente_con_mutuo_flg_ana)
                   
# Modello Random Forest
set.seed(123)
rf_model <- randomForest(target ~ satisfaction + cs_abi_num_comm  
                         + anzianita_anni_num + cliente_solo_cc_flg_ana
                         + suggestions_to_improve 
                         + vdbank_flg_ana + sesso_code_ana + segmento_des_comm
                         + reason_for_satisfaction + cliente_investitore_flg_ana,
                         data = survey_1,
                         importance = TRUE,
                         ntree = 300,
                         na.action = na.exclude)

#variabili non importanti
# browser,need_satisfied, operazione, area, direzione
#email, fascia_eta_code_ana, fascia_anzianita_code_ana
#sistema_operativo, easy_support,in_bonis_flg_comm
#cliente_con_mutuo_flg_ana  

# Visualizzazione dell'importanza delle variabili
importance(rf_model)
varImpPlot(rf_model)

#var_explained: 61.96%
#MSE minima: 2.23

# Risultati
print(rf_model)


#####random forest survey 2#####
library(randomForest)
colSums(is.na(survey_2))
table(survey_2$target, survey_2$satisfaction)

# Modello Random Forest
set.seed(123)
rf_model <- randomForest(target ~ segmento_des_comm 
                         + cliente_con_mutuo_flg_ana 
                         + app_used + xav_profilo_postazione, 
                         data = survey_2,
                         importance = TRUE,
                         ntree = 300,
                         na.action = na.exclude)

#variabili non importanti
#banca_mcf_xav_des, cliente_investitore_flg_ana
#cliente_solo_cc_flg_ana, fascia_utilizzo_online_comm
#app_knowledge,satisfaction,browser 

# Visualizzazione dell'importanza delle variabili
importance(rf_model)
varImpPlot(rf_model)

#var_explained: 0.6%
#MSE minima: 1.35

# Risultati
print(rf_model)
#modello pessimo

#####random forest survey 3#####
library(randomForest)
colSums(is.na(survey_3))
table(survey_3$target, survey_3$reason_for_score_nps_filiale)

survey_3$fascia_fatturato_ana <- as.numeric(survey_3$fascia_fatturato_ana)
survey_3$disservices_description <- as.numeric(survey_3$disservices_description)
survey_3$reason_for_score_nps_filiale <- as.numeric(survey_3$reason_for_score_nps_filiale)
survey_3[, c("fascia_fatturato_ana", 
             "reason_for_score_nps_filiale",
             "disservices_description")][is.na(survey_3[, c("fascia_fatturato_ana",
                                                            "reason_for_score_nps_filiale",
                                                            "disservices_description")])] <- 0
survey_3$fascia_fatturato_ana <- as.factor(survey_3$fascia_fatturato_ana)
survey_3$disservices_description <- as.factor(survey_3$disservices_description)
survey_3$reason_for_score_nps_filiale <- as.factor(survey_3$reason_for_score_nps_filiale)

# Modello Random Forest
set.seed(123)
rf_model <- randomForest(target ~  browser + segmento_des_comm
                         + operazione + fascia_eta_code_ana
                         + cliente_con_mutuo_flg_ana + reason_for_score_nps_filiale
                         + cs_abi_num_comm  
                         + disservices_description, 
                         data = survey_3,
                         importance = TRUE,
                         ntree = 300,
                         na.action = na.exclude)


CrossTable(survey_3$target, survey_3$reason_for_score_nps_filiale,
           prop.chisq = FALSE,  
           prop.t = FALSE,       
           prop.r = FALSE,       
           prop.c = FALSE)       

#variabili non importanti
#direzione,area, email, fascia_eta_code_ana, sistema_operativo
#xntweb_flg_ana, in_bonis_flg_comm, fascia_utilizzo_online_comm
# anzianita_anni_num, easy_support, service_satisfaction
# cliente_solo_cc_flg_ana

# note: nps_factors_survey_filiale non analizzabile perchè > 53 valori
# Errore in randomForest.default(m, y, ...) : Can not handle categorical predictors with more than 53 categories.

# Visualizzazione dell'importanza delle variabili
importance(rf_model)
varImpPlot(rf_model)

#var_explained: 15.14%
#MSE minima: 5.92

# Risultati
print(rf_model)
#le migliori variabili sono le stesse della regressione lineare


#####random forest survey 4#####
library(randomForest)
colSums(is.na(survey_4))
table(survey_4$target, survey_4$segmento_des_comm)

# Modello Random Forest
set.seed(123)
rf_model <- randomForest(target ~  regione_des_ana  + segmento_des_comm
                         + sesso_code_ana + fascia_eta_code_ana
                         + cliente_investitore_flg_ana, 
                         data = survey_4,
                         importance = TRUE,
                         ntree = 300,
                         na.action = na.exclude)

#variabili non significative
#area, direzione, filiale, fascia_anzianita_code_ana
#xntweb_flg_ana, email, multi_flg_comm, vdbank_flg_ana
#cliente_con_mutuo_flg_ana, cliente_solo_cc_flg_ana
#in_bonis_flg_comm, fascia_utilizzo_online_comm
# easy_support, service_satisfaction, expenses_icon
# expenses_icon_used,reason_for_score_comment

# Visualizzazione dell'importanza delle variabili
importance(rf_model)
varImpPlot(rf_model)

#var_explained_max: 2.58%
#MSE minima: 1.91

# Risultati
print(rf_model)

#modello non molto utile

#####random forest survey 16#####
library(randomForest)
colSums(is.na(survey_16))
table(survey_16$target, survey_16$multi_flg_comm)

# Modello Random Forest
set.seed(123)
rf_model <- randomForest(target ~ segmento_des_comm 
                         + vdbank_flg_ana,  
                         data = survey_16,
                         importance = TRUE,
                         ntree = 300,
                         na.action = na.exclude)

#variabili non significative
#satisfaction, sesso_code_ana, email, direzione, app_used 
#regione_des_ana,area, fascia_anzianita_code_ana, anzianita_anni_num
#xntweb_flg_ana, cliente_con_mutuo_flg_ana, cliente_solo_cc_flg_ana
#in_bonis_flg_comm, risk_rating_comm, fascia_utilizzo_online_comm
#app_knowledge, multi_flg_comm, fascia_eta_code_ana, cliente_investitore_flg_ana
#sistema_operativo,browser
#
# nps_factors_survey_canale -> Errore in randomForest.default(m, y, ...) : 
# Can not handle categorical predictors with more than 53 categories.

# Visualizzazione dell'importanza delle variabili
importance(rf_model)
varImpPlot(rf_model)
print(rf_model)

#var_explained_max: 0.03%
#MSE minima: 2.77

#modello inutile

#####random forest survey 18#####

library(randomForest)
colSums(is.na(survey_18))
table(survey_18$target, survey_18$cliente_investitore_flg_ana)

survey_18$email <- as.numeric(survey_18$email)
survey_18$reason_for_score_comment <- as.numeric(survey_18$reason_for_score_comment)
survey_18[, c("email",
              "reason_for_score_comment")][is.na(survey_18[, c("email",
                                                               "reason_for_score_comment")])] <- 0
survey_18$email <- as.factor(survey_18$email)
survey_18$reason_for_score_comment <- as.factor(survey_18$reason_for_score_comment)

# Modello Random Forest
set.seed(123)
rf_model <- randomForest(target ~ segmento_des_comm 
                         + sesso_code_ana + fascia_eta_code_ana
                         + reason_for_score_comment,  
                         data = survey_18,
                         importance = TRUE,
                         ntree = 300,
                         na.action = na.exclude)

#variabili non significative
#area, direzione, email, cliente_solo_cc_flg_ana
#vdbank_flg_ana, in_bonis_flg_comm, fascia_utilizzo_online_comm
#multi_flg_comm, fascia_utilizzo_online_comm, sistema_operativo 
#cliente_investitore_flg_ana, regione_des_ana,cliente_con_mutuo_flg_ana
#reason_for_score_comment

# Visualizzazione dell'importanza delle variabili
importance(rf_model)
varImpPlot(rf_model)
print(rf_model)

#var_explained_max: 2.02%
#MSE minima: 1.80

#####random forest survey 52#####
library(randomForest)
colSums(is.na(survey_52))
table(survey_52$target, survey_52$reason_for_score_comment)

survey_52$email <- as.numeric(survey_52$email)
survey_52$reason_for_score_comment <- as.numeric(survey_52$reason_for_score_comment)
survey_52[, c("email",
              "reason_for_score_comment")][is.na(survey_52[, c("email",
                                                               "reason_for_score_comment")])] <- 0
survey_52$email <- as.factor(survey_52$email)
survey_52$reason_for_score_comment <- as.factor(survey_52$reason_for_score_comment)


set.seed(123)
rf_model <- randomForest(target ~ sistema_operativo + regione_des_ana
                         + segmento_des_comm + fascia_eta_code_ana
                         + cliente_investitore_flg_ana
                         + app_knowledge + email
                         + reason_for_score_comment,  
                         data = survey_52,
                         importance = TRUE,
                         ntree = 300,
                         na.action = na.exclude)


#variabili non significative
#vdbank_flg_ana, xntweb_flg_ana, cliente_solo_cc_flg_ana
# in_bonis_flg_comm, fascia_utilizzo_online_comm, area
#direzione, multi_flg_comm, browser, anzianita_anni_num
#cliente_con_mutuo_flg_ana

#nps_factors_survey_canale -> Errore in randomForest.default(m, y, ...) : 
# Can not handle categorical predictors with more than 53 categories

# Visualizzazione dell'importanza delle variabili
importance(rf_model)
varImpPlot(rf_model)
print(rf_model)

#var_explained_max: 1.04%
#MSE minima: 2.26

# sistema_operativo + regione_des_ana
# +segmento_des_comm+ email + fascia_eta_code_ana
# +cliente_investitore_flg_ana
# + app_knowledge
# + reason_for_score_comment








#####regressioni logistiche aggregate#####
digital_surveys <- df_clean[df_clean$survey %in% c(2, 4, 16, 18, 52), ]
dem_surveys <- df_clean[df_clean$survey %in% c(1,3), ]

#####regressione logistica digital#####
digital_surveys<- digital_surveys[, colSums(!is.na(digital_surveys)) > 0]
names(digital_surveys)
colSums(is.na(digital_surveys))


digital_surveys$email <- as.numeric(digital_surveys$email)
digital_surveys$banca_mcf_xav_des <- as.numeric(digital_surveys$banca_mcf_xav_des)
digital_surveys[, c("email", "banca_mcf_xav_des"
)][is.na(digital_surveys[, c("email", "banca_mcf_xav_des")])] <- 0
digital_surveys$email <- as.factor(digital_surveys$email)
digital_surveys$banca_mcf_xav_des <- as.factor(digital_surveys$banca_mcf_xav_des)

table(digital_surveys$target_bin.f, digital_surveys$fascia_utilizzo_online_comm)

lg0.0_form <- formula(target_bin.f ~ sistema_operativo
                                      + segmento_des_comm + direzione
                                      + banca_mcf_xav_des + in_bonis_flg_comm
                                      + fascia_utilizzo_online_comm)#
lg0.0 <- glm(lg0.0_form, 
             data = digital_surveys, 
             family = 'binomial')

summary(lg0.0)
alias(lg0.0) # regione_des_ana collineare con direzione
vif(lg0.0) 

predizioni <- predict(lg0.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

ds_non_NA <- digital_surveys[complete.cases(digital_surveys[, c("segmento_des_comm","sistema_operativo", 
                                                         "direzione",
                                                        "banca_mcf_xav_des", "in_bonis_flg_comm", 
                                                        "fascia_utilizzo_online_comm")]), ]

confusion_matrix <- table(Predicted = previsioni_binarie, Actual = ds_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision) #0.66
print(accuracy) #0.66

#variabili non significative:
#browser, area, email, sesso_code_ana
#xav_profilo_postazione, vdbank_flg_ana
#xntweb_flg_ana, cliente_con_mutuo_flg_ana
#cliente_investitore_flg_ana, cliente_solo_cc_flg_ana
#multi_flg_comm, regione_des_ana
#####regressione logistica dem#####

dem_surveys<- dem_surveys[, colSums(!is.na(dem_surveys)) > 0]
names(dem_surveys)
colSums(is.na(dem_surveys))

dem_surveys$email <- as.numeric(dem_surveys$email)
dem_surveys[, c("email")][is.na(dem_surveys[, c("email" )])] <- 0
dem_surveys$email <- as.factor(dem_surveys$email)

table(dem_surveys$target_bin.f, dem_surveys$nps_factors_survey_filiale)

lg0.0_form <- formula(target_bin.f ~ browser + segmento_des_comm
                                      + operazione
                                      + regione_des_ana + sesso_code_ana
                                      + fascia_eta_code_ana + cliente_con_mutuo_flg_ana
                                      + cs_abi_num_comm)
##nps_factors_survey_filiale migliora il modello ma elimina circa il 25% 
#delle osservazioni riducendo a cui sommando gli altri NA arrivano a circa il 40%

lg0.0 <- glm(lg0.0_form, 
             data = dem_surveys, 
             family = 'binomial')

summary(lg0.0)
alias(lg0.0) 
vif(lg0.0) #operazione e satisfaction collineari

predizioni <- predict(lg0.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

ds_non_NA <- dem_surveys[complete.cases(dem_surveys[, c("segmento_des_comm","browser", 
                                                                "operazione",
                                                                "regione_des_ana", "sesso_code_ana", 
                                                                "fascia_eta_code_ana", 
                                                        "cliente_con_mutuo_flg_ana", 
                                                        "cs_abi_num_comm")]), ]

confusion_matrix <- table(Predicted = previsioni_binarie, Actual = ds_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision) #0.59
print(accuracy) #0.57

#variabili non significative:
#sistema_operativo, gestore, area, xntweb_flg_ana,satisfaction
#direzione, fascia_fatturato_ana, vdbank_flg_ana
#cliente_investitore_flg_ana, cliente_solo_cc_flg_ana
#in_bonis_flg_comm, multi_flg_comm, risk_rating_comm
#fascia_utilizzo_online_comm, reason_for_score_nps_filiale



#####regressione logistica non relazionali#####
norel <- df_clean[df_clean$survey %in% c(1, 2, 3), ]
rel <- df_clean[df_clean$survey %in% c(4, 16,18, 52), ]
#####regressione logistica norel######

norel<- norel[, colSums(!is.na(norel)) > 0]
names(norel)
colSums(is.na(norel))

norel$fascia_fatturato_ana <- as.numeric(norel$fascia_fatturato_ana)
norel$email <- as.numeric(norel$email)
norel$banca_mcf_xav_des <- as.numeric(norel$banca_mcf_xav_des)
norel[, c("email", "banca_mcf_xav_des", "fascia_fatturato_ana"
)][is.na(norel[, c("email", "banca_mcf_xav_des", "fascia_fatturato_ana")])] <- 0
norel$email <- as.factor(norel$email)
norel$banca_mcf_xav_des <- as.factor(norel$banca_mcf_xav_des)
norel$fascia_fatturato_ana <- as.factor(norel$fascia_fatturato_ana)

table(norel$target_bin.f, norel$easy_support)

lg0.0_form <- formula(target_bin.f ~ sistema_operativo + operazione
                                    + segmento_des_comm
                                    + fascia_eta_code_ana
                                    + cliente_con_mutuo_flg_ana
                                    + cs_abi_num_comm
                                    + nps_factors_survey_filiale
                                    )
#nps_factors_survey_filiale dimezza le osservazioni disponibili, ma migliora notevolmente il modello
lg0.0 <- glm(lg0.0_form, 
             data = norel, 
             family = 'binomial')

summary(lg0.0)
alias(lg0.0) #sistema_operativo collineare con browser
vif(lg0.0) #no multicollinearità

predizioni <- predict(lg0.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

norel_non_NA <- norel[complete.cases(norel[, c("segmento_des_comm","sistema_operativo", 
                                            "operazione","fascia_eta_code_ana", "cliente_con_mutuo_flg_ana", 
                                            "cs_abi_num_comm", "nps_factors_survey_filiale")]), ]

confusion_matrix <- table(Predicted = previsioni_binarie, Actual = norel_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision) #0.66
print(accuracy) #0.66

#variabili non significative:
# browser, satisfaction, gestore
#regione_des_ana, area, email, xav_profilo_postazione
#fascia_fatturato_ana, banca_mcf_xav_des
#vdbank_flg_ana, xntweb_flg_ana, cliente_investitore_flg_ana
#cliente_solo_cc_flg_ana, in_bonis_flg_comm
#multi_flg_comm, risk_rating_comm, fascia_tr_code_comm
#sesso_code_ana, fascia_utilizzo_online_comm, direzione
#ces_factors_transfer_intercept, ces_factors_other
#app_knowledge, app_used, xa_app_not_used_reason
#xa_app_not_used_reason_other, reason_for_score_nps_filiale
#disservices_description, need_satisfied
#####regressione logistica rel######

rel<- rel[, colSums(!is.na(rel)) > 0]
names(rel)
colSums(is.na(rel))

rel$email <- as.numeric(rel$email)
rel$reason_for_score_comment <- as.numeric(rel$reason_for_score_comment)
rel[, c("email", "reason_for_score_comment")][is.na(rel[, c("email", "reason_for_score_comment")])] <- 0
rel$email <- as.factor(rel$email)
rel$reason_for_score_comment <- as.factor(rel$reason_for_score_comment)
table(rel$target_bin.f, rel$reason_for_score_comment)

lg0.0_form <- formula(target_bin.f ~ browser + segmento_des_comm
                              + regione_des_ana + fascia_eta_code_ana
                              + app_knowledge + reason_for_score_comment
) #

lg0.0 <- glm(lg0.0_form, 
             data = rel, 
             family = 'binomial')

summary(lg0.0)

alias(lg0.0) 
#browser collineare con sistema_operativo
#direzione collineare con regione_des_ana
vif(lg0.0) #no multicollinearità

predizioni <- predict(lg0.0, type = "response")
previsioni_binarie <- ifelse(predizioni > 0.5, 1, 0)

rel_non_NA <- rel[complete.cases(rel[, c("browser","app_knowledge", 
                                          "segmento_des_comm","fascia_eta_code_ana", 
                                          "app_knowledge", "reason_for_score_comment")]), ]

confusion_matrix <- table(Predicted = previsioni_binarie, Actual = rel_non_NA$target_bin.f)
print(confusion_matrix)
true_positives <- confusion_matrix[2, 2]
false_positives <- confusion_matrix[2, 1]
true_negatives <- confusion_matrix[1, 1]
total_observations <- sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
accuracy <- (true_positives + true_negatives) / total_observations
print(precision) #0.60
print(accuracy) #0.59

#variabili non significative:
#sistema_operativo, satisfaction, area, direzione, email
#sesso_code_ana, vdbank_flg_ana, xntweb_flg_ana
#cliente_con_mutuo_flg_ana, cliente_investitore_flg_ana
#cliente_solo_cc_flg_ana, in_bonis_flg_comm
#multi_flg_comm, risk_rating_comm, fascia_tr_code_comm
#fascia_utilizzo_online_comm, nps_factors_survey_canale
#nps_factors_survey_canale_other, app_used, xv_va_app_not_used_reason
#easy_support, service_satisfaction