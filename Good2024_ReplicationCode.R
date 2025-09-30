## Replication File 1 of 2
## Uses prepped data PADD_V1.csv
## "Power Over Presence: 
## Women’s Representation in Comprehensive Peace Negotiations and Gender Provision Outcomes"
## Elizabeth Good, Northwestern University
## Created: 14/08/2022
## Updated: 01/05/2024

##Analyses performed in R version 2023.06.2+561

#Load libraries
library(tidymodels)
library(corrplot)
library(dplyr)
library(skimr)
library(viridis)
library(stargazer)
library(mitools)
library(mice)
library(miceadds)
library(sandwich)
library(texreg)
library(moderndive)
library(VIM)
library(gridExtra)
library(ggpubr)
library(sjPlot)
library(imputeTS)
library(car)
library(margins)


df <- read.csv("PADD_Agreement Level.csv")

##############################################################################
##############################################################################
## Analyses for Main Paper
##############################################################################
##############################################################################


####
## Table 1
####

sum(df$FemDel_Bin == 1 & df$GeWom == 1, na.rm = TRUE)
sum(df$FemDel_Bin == 1 & df$GeWom == 0, na.rm = TRUE)
sum(df$FemDel_Bin == 0 & df$GeWom == 1, na.rm = TRUE)
sum(df$FemDel_Bin == 0 & df$GeWom == 0, na.rm = TRUE)


####
## Table 2
####

###Converting svac into numeric###
#state Department
df$state_prev_avg <- as.numeric(as.character(df$state_prev_avg))

#amnesty international
df$ai_prev_avg <- as.numeric(as.character(df$ai_prev_avg))

#human rights watch
df$hrw_prev_avg <- as.numeric(as.character(df$hrw_prev_avg))

#female_combatants_exs
df$female_combatants_exs <- as.numeric(as.character(df$female_combatants_exs))

##create dataframe with relevant variables for imputation
df_imp <- df[, 1:63]

###Removing agreement that negatively reference women (Adadda Agreement, Ethiopia Somalia Ogaden Puntland, 2007)
exclude_row <- 91

# Create a new dataframe excluding rows where the specified variable has the excluded value
df_ex <- df_imp[-exclude_row, ]

###duplicate original data###
df_original <- df_ex

###IMPUTATION###
init = mice(df_ex, maxit=0) 
meth = init$method
predM = init$predictorMatrix

#collinearity check
mice:::find.collinear(df_ex)

#Avoid imputing variables based on relevance, missingness, and collinearity
predM[, c("FemDel_P", "AgtId", "Con", "Dat", "Stage", "StageSub", "FemNeg_P", "FemMed_P", "Log_N", "FemLog_N", "FemLog_P", "Adv_N", "FemAdv_N", "FemAdv_P", "WomCom_N", "WomCom_P", "FemWomCom_N", "FemWomCom_P", "Unkn_N", "FemUnkn_N", "FemUnkn_P", "WomGrpRep_N", "WomGrpRep_P", "FemWomGrp_P")] = 0

meth[c("FemDel_P", "AgtId", "Con", "Dat", "Stage", "StageSub", "FemNeg_P", "FemMed_P", "Log_N", "FemLog_N", "FemLog_P", "Adv_N", "FemAdv_N", "FemAdv_P", "WomCom_N", "WomCom_P", "FemWomCom_N", "FemWomCom_P", "Unkn_N", "FemUnkn_N", "FemUnkn_P", "WomGrpRep_N", "WomGrpRep_P", "FemWomGrp_P")] = ""

#Run multiple (m=5) imputation
set.seed(103)
imputed <- mice(df_ex, method=meth, predictorMatrix=predM, m=5)

###model 1 - non-imputed data, no controls
original <- lm(GeWom ~ FemDel_P, data = df_original)

###model 2 - imputed, international involvement 
modelFit_II <- with(imputed, lm(GeWom ~ FemDel_P + UNSCR + ImUN + ImOth + NAP))

#Visualizing pooled data with original - international involvement 
datlist <- miceadds::mids2datlist(imputed)
model_pooled_II <- with(datlist, lm(GeWom ~ FemDel_P + UNSCR +Press_UNSC + ImUN + ImOth + NAP))
results <- pool(model_pooled_II)

bs_II <- lapply(model_pooled_II, coef)
vs_II <- lapply(model_pooled_II, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_II, vs_II))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_II <- summary(pool_mi(bs_II, vs_II))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_II), 
    coef = model1_pool_II$results, 
    se = model1_pool_II$se, 
    pvalues = model1_pool_II$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_II <- extract.df(model_pooled_II) 

###Model 3 - gender inequality
#Imputed - pooling 
modelFit_GI <- with(imputed, lm(GeWom ~ FemDel_P + GDI + SEP_Fem + TeenPreg))

#visualizing gender inequality - pooled data
datlist <- miceadds::mids2datlist(imputed)
model_pooled_GI <- with(datlist, lm(GeWom ~ FemDel_P + GDI + SEP_Fem + TeenPreg))
results <- pool(model_pooled_GI)

bs_GI <- lapply(model_pooled_GI, coef)
vs_GI <- lapply(model_pooled_GI, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_GI, vs_GI))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_GI <- summary(pool_mi(bs_GI, vs_GI))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_GI), 
    coef = model1_pool_GI$results, 
    se = model1_pool_GI$se, 
    pvalues = model1_pool_GI$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_GI <- extract.df(model_pooled_GI)

###Model 4 - women in politics
#Imputed - pooling
modelFit_WomPol <- with(imputed,lm(GeWom ~ FemDel_P + PolInt_Cmb + WomParl))

#visualizing women's interest in politics - pooled data
datlist <- miceadds::mids2datlist(imputed)
model_pooled_WP <- with(datlist, lm(GeWom ~ FemDel_P + PolInt_Cmb + WomParl))
results <- pool(model_pooled_WP)

bs_WP <- lapply(model_pooled_WP, coef)
vs_WP <- lapply(model_pooled_WP, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_WP, vs_WP))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_WP <- summary(pool_mi(bs_WP, vs_WP))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_WP), 
    coef = model1_pool_WP$results, 
    se = model1_pool_WP$se, 
    pvalues = model1_pool_WP$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_WP <- extract.df(model_pooled_WP)

###Model 5 - women in war
datlist <- miceadds::mids2datlist(imputed)
model_pooled_WW <- with(datlist, lm(GeWom ~ FemDel_P + state_prev_avg + female_combatants_exs))
results <- pool(model_pooled_WW)

bs_WW <- lapply(model_pooled_WW, coef)
vs_WW <- lapply(model_pooled_WW, FUN = function(x){vcovCL(x)})
summary(pool_mi(bs_WW, vs_WW))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_WW <- summary(pool_mi(bs_WW, vs_WW))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_WW), 
    coef = model1_pool_WW$results, 
    se = model1_pool_WW$se, 
    pvalues = model1_pool_WW$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_WW <- extract.df(model_pooled_WW)

###Model 6
datlist <- miceadds::mids2datlist(imputed)
model_pooled_KS <- with(datlist, lm(GeWom ~ FemDel_P + GDI + WomParl + SEP_Fem + TeenPreg + NYT_p + UNSCR + Press_UNSC + ImUN + ImOth + NAP + PolInt_Cmb + JobEql_Cmb + LeadPol_Cmb + state_prev_avg + female_combatants_exs))
results <- pool(model_pooled_KS)

bs_KS <- lapply(model_pooled_KS, coef)
vs_KS <- lapply(model_pooled_KS, FUN = function(x){vcovCL(x)}) 
summary(pool_mi(bs_KS, vs_KS))

extract.df <- function(tt, cl = NULL) {
  require(sandwich)
  require(mitools)
  require(texreg)
  m2 <- length(tt) 
  betas <- lapply(tt, coef)
  vars <- lapply(tt, FUN = function(x){vcovCL(x)})
  
  model1_pool_KS <- summary(pool_mi(bs_KS, vs_KS))
  
  R2 <- mean(sapply(1:m2, function(x) summary(tt[[x]])$r.squared))
  
  ns <- nobs(tt[[1]])
  
  tr <- createTexreg(
    coef.names = row.names(model1_pool_KS), 
    coef = model1_pool_KS$results, 
    se = model1_pool_KS$se, 
    pvalues = model1_pool_KS$p,
    gof.names = c("R^2", "Num. obs."), 
    gof = c(R2, ns),
    gof.decimal = c(T,F)
  )
} 

output_pool_KS <- extract.df(model_pooled_KS)


###Vizualizing Table 2
screenreg(list(original, output_pool_II, output_pool_GI, output_pool_WP, output_pool_WW, output_pool_KS), 
          custom.coef.names = c('(Intercept)', 'Women Delegates (%)', 'UNSC Resolutions', 'UNSC Press', 'UN Signatory', 'Third Party Signatory', 'National Action Plan', 'Gender Development Index', 'Secondary Education for Women', 'Teen Pregnancy', 'Womens Political Interest', 'Women in Parliament', 'Sexual Violence in Conflict', 'Women Active Combatants', 'New York Times Publications', 'Perception of Employment for Women', 'Perception of Women in Leadership'), custom.model.names = c('Original(1)', 'Imputed(2)', 'Imputed(3)', 'Imputed(4)', 'Imputed(5)', 'Imputed(6)'),
          digits = 3)


####
## Table 3
####

###equality
lm(EqGen ~ FemDel_Bin, data = df) %>%
  summary()
###protection of civilians
lm(ProtCiv ~ FemDel_Bin, data = df) %>%
  summary()
###freedom of movement
lm(CprFmov ~ FemDel_Bin, data = df) %>%
  summary()
###voting rights
lm(CprVote ~ FemDel_Bin, data = df) %>%
  summary()
###eductaion 
lm(SerEdu ~ FemDel_Bin, data = df) %>%
  summary()
###property
lm(SerProp ~ FemDel_Bin, data = df) %>%
  summary()
###work
lm(SerWork ~ FemDel_Bin, data = df) %>%
  summary()
###health
lm(SerHeal ~ FemDel_Bin, data = df) %>%
  summary()
###access to water
lm(Wat ~ FemDel_Bin, data = df) %>%
  summary()
###justice sector reform
lm(JusCr ~ FemDel_Bin, data = df) %>%
  summary()
###assistance to victims
lm(TjVic ~ FemDel_Bin, data = df) %>%
  summary()
###reparations
lm(TjRep ~ FemDel_Bin, data = df) %>%
  summary()
###reconciliation measures
lm(TjNR ~ FemDel_Bin, data = df) %>%
  summary()
###provisions for children
lm(GCh ~ FemDel_Bin, data = df) %>%
  summary()
###provisions for men and boys
lm(GeMe ~ FemDel_Bin, data = df) %>%
  summary()
###provisions for families
lm(GeFa ~ FemDel_Bin, data = df) %>%
  summary()
###inclusion of gender-neutral language 
lm(GeMeNu ~ FemDel_Bin, data = df) %>%
  summary()


####
## Table 4
####

#OLS regression between women in positions of power and provisions for women
sig_og <- lm(GeWom ~ FemSig_P, data = df)
neg_og <- lm(GeWom ~ FemNeg_P, data = df)
med_og <- lm(GeWom ~ FemMed_P, data = df)
ob_og <- lm(GeWom ~ FemOb_P, data = df)

#Visualize Table 4 
stargazer(sig_og, neg_og, med_og, ob_og, type="text",
          covariate.labels = c("Women Signatories (%)", "Women Negotiators (%)", "Women Mediators (%)", "Women Observers (%)"),
          dep.var.caption = "",
          dep.var.labels = "Provisions for Women",
          single.row = TRUE)


####
## Table 5
####
#create binary variables
df$FemSig_Bin <- ifelse(df$FemSig_N > 0, 1, 0)
df$FemNeg_Bin <- ifelse(df$FemNeg_N > 0, 1, 0)
df$FemMed_Bin <- ifelse(df$FemMed_N > 0, 1, 0)
df$FemOb_Bin <- ifelse(df$FemOb_N > 0, 1, 0)

#GLM regerssion - binary measurement of women in positions of power and provisions for women
sig_b_glm <- glm(GeWom ~ FemSig_Bin, data = df, family = 'binomial')
neg_b_glm <- glm(GeWom ~ FemNeg_Bin, data = df, family = 'binomial')
med_b_glm <- glm(GeWom ~ FemMed_Bin, data = df, family = 'binomial')
ob_b_glm <- glm(GeWom ~ FemOb_Bin, data = df, family = 'binomial')

#Visualize GLM regression
stargazer(sig_b_glm, neg_b_glm, med_b_glm, ob_b_glm, type="text",
          covariate.labels = c("Women Signatories", "Women Negotiators", "Women Mediators", "Women Observers"),
          dep.var.caption = "Women Delegates per Negotiation (Binary)",
          dep.var.labels = "Provisions for Women")

#Interpreting GLMs using marginal effects 
#Signatories
margins(sig_b_glm)
summary(margins(sig_b_glm))

#negotiators
margins(neg_b_glm)
summary(margins(neg_b_glm))

#mediators
margins(med_b_glm)
summary(margins(med_b_glm))

#observers
margins(ob_b_glm)
summary(margins(ob_b_glm))
