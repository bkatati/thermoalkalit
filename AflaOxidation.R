
#######################################################################

# TITLE:  Aflatoxin-B1 Degradation and Nutrient Retention 
#         During Thermal-alkaline Preparation of Nshima
#         from Maize Flour Porridge 

# DOI: (under peer review)


#######################################################################


# DATA ANALYSIS:

########################################################################
### OBJECTIVE 1): AFLATOXIN DEGRADATION DURING NSHIMA PREPARATION ######
########################################################################

my_data <- read.csv(url("https://github.com/bkatati/thermoalkalit/blob/main/AflOxidation.csv"))
# the analysis uses LoD of 5 ug/kg

# NB: if file path error occurs, download csv file "AflOxidation.csv" 
# from site "https://github.com/bkatati/thermoalkalit" 
# to your PC, and create appropriate local drive path then change above file path to your local path.


print(my_data)
  dim(my_data)
prep <- subset(my_data, stage != "control") # to exclude control and HCl
tradx <- subset(prep, untreat == "yes")
alkali <- subset(prep, treat == "yes")
dim(tradx)
dim(alkali)

# Check normality of the distribution

Nshima <- tradx$ID=="Nshima"
MMeal <- tradx$ID=="MMeal (Raw) 1:9 5G58:Blank"
Porrid <- tradx$ID =="Porridge"
NshimOxid <- alkali$ID =="Nshima+HCO3"
PorridOx <- alkali$ID =="Porridge+HCO3"
QControl <- my_data$ID =="MMeal (Raw)+HCO3 1:9 5G58:Blank "

Stomach <- alkali$ID =="Nshima+HCO3+HCL "

#         Shapiro-Wilk normality test B1

shapiro.test(tradx$B1[MMeal])      # W = 0.90295, p-value = 0.4264
shapiro.test(tradx$B1[Porrid])     # W = 0.95485, p-value = 0.7793
shapiro.test(tradx$B1[Nshima])     # W = 0.93062, p-value = 0.5849
shapiro.test(alkali$B1[PorridOx])   # W = 0.96433, p-value = 0.8524
shapiro.test(alkali$B1[NshimOxid])  # W = 0.97513, p-value = 0.925

shapiro.test(my_data$B1[QControl])  # W = 0.93618, p-value = 0.6312

shapiro.test(alkali$LbB1[Stomach])  # (lower-bound B1) 0.71327, p-value = 0.01311



# Figure 5 Boxplot showing change in quantity of aflatoxin-B1:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# BOXPLOT VISUALISATION
# ~~~~~~~~~~~~~~~~~~~~~

require(ggplot2)

nplot <- subset(prep, stage == "prepared") # to exclude control and HCl

y.expression <- expression("B1  ug/kg") # symbol for minus is " - " (with "Windows 1252" encoding format)

boxplot(B1 ~ Medium, data = nplot,
        xlab = "",
        las = 0,
        ylab = "",
        cex.axis = 0.7,   # smaller axis labels
        cex.main = 0.9,   # ↓ smaller title
        main = expression("Change in B1 During Traditional and Thermal-Alkaline " * italic(Nshima) * " Preparation"))

title(ylab = y.expression, line = 2.3)
title(xlab = "Treatment", line = 3.5)


# SIGNIFICANCE OF MODEL:


# I) Traditional (control) Preparation of Maize Flour into Nshima
#.................................................................

# For normal distributed datum:
# One-way ANOVA:

# Set treatment as a factor:
tradx$Medium <- factor(tradx$Medium)

# anova model for B1

require(car)
leveneTest(B1 ~ factor(Medium), tradx)

lm.B1 <- lm(B1 ~ Medium, data = tradx)
anova(lm.B1)

# Analysis of Variance Table

# Response: B1
#           Df Sum Sq Mean Sq F value Pr(>F)
# Medium     2 1387.2  693.60  2.5159 0.1166
# Residuals 14 3859.6  275.69 

# model for B1 is NOT significant. No Posthoc.


# ii) Thermo-alkali (Treatment) Preparation of Maize Flour into Nshima
# ....................................................................

leveneTest(B1 ~ factor(Medium), alkali)

lm.B1 <- lm(B1 ~ Medium, data = alkali)
anova(lm.B1)

# Analysis of Variance Table

# Response: B1
#           Df  Sum Sq Mean Sq F value    Pr(>F)    
# Medium     3 28334.6  9444.9  68.211 2.334e-10 ***
# Residuals 19  2630.8   138.5       

# Model for B1 is significant (p < 0.001). Proceed to Posthoc:

# POSTHOC (Bonferroni)

pairwise.t.test(alkali$B1, alkali$Medium, p.adjust="bonf", pool.sd=T)

# Pairwise comparisons using t tests with pooled SD 

# data:  alkali$B1 and alkali$Medium 

#                 [A] MFlour [D] Porridge_Ox [E] Nshima_Ox
#  [D] Porridge_Ox 4.5e-09    -               -            
#  [E] Nshima_Ox   9.1e-07    0.02            -            
#  [F] Nshima_Ox+H 2.1e-10    0.18            9.9e-05      

#  P value adjustment method: bonferroni 

######
# Q&A:
######

# Traditional Preparation
#.........................

# Q1: Did B1 reduce from maize flour to Porridge?
# Did not reduce: Model not significant.

# Q2: Did B1 reduce from maize flour to Nshima?
# Did not reduce: Model not significant.

# Q3: Did B1 reduce from porridge to Nshima?
# Did not reduce: Model not significant.

# Thermo-alkali Treatment
#.........................
# Q4: Did B1 reduce from maize flour to oxidised Porridge?
# Model significant, P < 0.001.
# posthoc: MFlour --> Porridge, p < 0.001 (Reduced!)

# Q5: Did B1 reduce from maize flour to oxidised Nshima?
# Model significant, P < 0.001.
# posthoc: MFlour --> Nshima, p < 0.001 (Reduced!)

# Q6: Did B1 change from oxidised porridge to oxidised Nshima?
# Model significant, P < 0.001.
# posthoc: Porridge --> Nshima, p = 0.02 (increased!)

# ACID TREATMENT

# Q6: Did B1 change from oxidised Nshima to acidified nshima?
# Model significant, P < 0.001.
# posthoc: Nshima --> acidified-Nshima, p < 0.001 (reduced!)


# MEANS
#######

MfloB1 <- mean(tradx$B1[MMeal])
MfloB1  # 96.44407 ug/kg

PorB1 <- mean(tradx$B1[Porrid])   
PorB1    # 94.47656 ug/kg

NshiB1 <- mean(tradx$B1[Nshima])
NshiB1    # 76.5404 ug/kg

PoroxB1 <- mean(alkali$B1[PorridOx])
PoroxB1   # 16.23572 ug/kg

NshoxB1 <- mean(alkali$B1[NshimOxid])
NshoxB1   # 39.0686 ug/kg

stomacLbB1 <- max(alkali$LbB1[Stomach])
stomacLbB1  # Range: 0 - 0.4877023 ug/kg => < LoD (5 ug/kg)
stomacB1 <- mean(alkali$B1[Stomach])
stomacB1 # 5 ug/kg (LoD)


# Quantifying Change in B1 in Boxplot (Figure 5)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Mmeal -> oxidised Porridge (Boxplot [A] to [D])

d1 <- (MfloB1 - PoroxB1)*100/MfloB1
d1 # B1 reduced by 83.2%


# Mmeal -> oxidised Nshima  (Boxplot [A] to [E])

d2 <- (MfloB1 - NshoxB1)*100/MfloB1
d2 # B1 reduced by 59.5%


# oxidised Porridge -> oxidised Nshima  (Boxplot [D] to [E])

d3 <- (PoroxB1 - NshoxB1)/PoroxB1
d3 # B1 increased ~1.4-fold (-1.406336)


# oxidised Nshima -> Stomach@LoD

d4 <- (NshoxB1 - stomacB1)*100/NshoxB1
d4 # reduced by 87.202%

# Quality Control
#################

# Did Bicarbonate during extraction enhance B1 degradation?

# Mean: 
SodaB1 <- mean(my_data$B1[QControl])
SodaB1 # 94.85145 ug/kg

B1soda <- my_data$B1[QControl]
B1flour <- tradx$B1[MMeal]

sd(B1soda)
sd(B1flour)

t.test(B1soda,B1flour, conf.level=0.95)

# No, bicarbonate did not enhance the aflatoxin extraction (p-value = 0.894).
# Welch Two Sample t-test

# data:  B1soda and B1flour
# t = -0.13822, df = 6.9802, p-value = 0.894
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -28.85467  25.66943
# sample estimates:
#   mean of x mean of y 
# 94.85145  96.44407 

# Maize flour mean B1 extracted without alkali = 96.44407 ug/kg.
# Maize flour mean B1 extracted with alkali = 94.85145ug/kg.

# Effect of Acid Treatment
##########################

# Did acid treatment change B1 levels?

acid_B1 <- alkali$B1[Stomach]
acid_B1

Nshox_B1 <- alkali$B1[NshimOxid]

sd(acid_B1)
sd(Nshox_B1)

t.test(acid_B1,Nshox_B1, conf.level=0.95)

# Yes, there was a change due to acidification (p-value = 0.0009487).
# B1 reduced from 39.1 ug/kg to below LoD (5 ug/kg)

# data:  acidB1 and Nshox_B1":

# t = -6.9481, df = 5, p-value = 0.0009487
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -46.67298 -21.46422
# sample estimates:
#  mean of x mean of y 
# 5.0000   39.0686


########################################################################
### OBJECTIVE 2): NUTRIENT RETENTION DURING THERMAL-ALKALINE TREATMENT #
########################################################################

# [B] How did the nutritional profile change

nutri <- read.csv(url("https://github.com/bkatati/thermoalkalit/blob/main/Nutrition.csv"))
# View(nutri)

# NB: if file path error occurs, download csv file "Nutrition.csv" 
# from site "https://github.com/bkatati/thermoalkalit" 
# to your PC, and create appropriate local drive path then change above file path to your local path.


# Q2.1) Was there a change in Ash Content between treated and untreated Nshima?

# we set treatment as factors:
treat <- nutri$Mode=="Treated"
control <- nutri$Mode=="Untreated"

kta <- nutri$Ash[treat]
kca <- nutri$Ash[control]

sd(kta)
sd(kca)

t.test(kta,kca, conf.level=0.95)

#       Welch Two Sample t-test

# data:  kta and kca
# t = 3.4911, df = 3.3625, p-value = 0.03318
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  0.0490251 0.6435554
# sample estimates:
#  mean of x mean of y 
# 1.440197  1.093907 

# Yes, there was a change in Ash content between treatment and control:

# Mean: 
AshC <- mean(nutri$Ash[control]) 
AshT <- mean(nutri$Ash[treat]) 
AshC # 1.093907
AshT # 1.440197

# Change:
d <- (AshT - AshC)*100/AshC
d # Ash content increased by 31.65628 

# Q2.2) Was there a change in Ash Content between treated and untreated Nshima?

ktp <- nutri$Protein[treat]
kcp <- nutri$Protein[control]

sd(ktp)
sd(kcp)

t.test(ktp,kcp, conf.level=0.95)

#       Welch Two Sample t-test

# t = -4.7724, df = 2.2621, p-value = 0.0322
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.54313751 -0.05751313
# sample estimates:
#  mean of x mean of y 
# 10.73936  11.03969  

# There was no change in crude protein content between treatment and control:

# Mean: 
ProtC <- mean(nutri$Protein[control]) 
ProtT <- mean(nutri$Protein[treat]) 
ProtC # 11.03969
ProtT # 10.73936

# Q2.3) Was there a change in fat content between treated and untreated Nshima?

ktf <- nutri$Fat[treat]
kcf <- nutri$Fat[control]

sd(ktf)
sd(kcf)

t.test(ktf,kcf, conf.level=0.95)

#       Welch Two Sample t-test

# data:  ktf and kcf
# t = -3.6229, df = 3.9653, p-value = 0.02264
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.74224340 -0.09691161
# sample estimates:
#  mean of x mean of y 
# 2.254162  2.673740

# Yes, there was change in crude fat content between treatment and control:

fatC <- mean(nutri$Fat[control]) 
fatT <- mean(nutri$Fat[treat]) 
fatC # 2.67374
fatT # 2.254162

# Change:
d <- (fatT - fatC)*100/fatC
d # Fat content reduced by -15.69253%


# Q2.4) Was there a change in fibre content between treated and untreated Nshima?

ktfb <- nutri$Fiber[treat]
kcfb <- nutri$Fiber[control]

sd(ktfb)
sd(kcfb)

t.test(ktfb,kcfb, conf.level=0.95)

#       Welch Two Sample t-test

# data:  ktfb and kcfb
# t = -3.3602, df = 3.9394, p-value = 0.02897
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.62010352 -0.05711781
# sample estimates:
#  mean of x mean of y 
# 1.479483  1.818094 

# Yes, there was change in crude fibre content between treatment and control:

# Mean:
fibC <- mean(nutri$Fiber[control]) 
fibT <- mean(nutri$Fiber[treat]) 
fibC # 1.818094%
fibT # 1.479483%

# Change:
d <- (fibT - fibC)*100/fibC
d # Fibre content reduced by -18.62449%

# Q2.5) Was there a change in carbohydrate content between treated and untreated Nshima?

ktc <- nutri$Carbohydrate[treat]
kcc <- nutri$Carbohydrate[control]

sd(ktc)
sd(kcc)

t.test(ktc,kcc, conf.level=0.95)

#       Welch Two Sample t-test

# data:  ktc and kcc
# t = 4.1789, df = 3.9008, p-value = 0.01468
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  0.2342351 1.1902114
# sample estimates:
#  mean of x mean of y 
# 84.08680  83.37457  

# There was negligible change in carbohydrate content between treatment and control:

# Mean:
carbC <- mean(nutri$Carbohydrate[control]) 
carbT <- mean(nutri$Carbohydrate[treat]) 
carbC # 83.37457%
carbT # 84.0868%

# Change:
d <- (carbT - carbC)*100/carbC
d # Carbohydrate content increased by 0.8542452%

# Q2.6) Was there a change in calorific value between treated and untreated Nshima?

kte <- nutri$Energy[treat]
kce <- nutri$Energy[control]

sd(kte)
sd(kce)

t.test(kte,kce, conf.level=0.95)

#       Welch Two Sample t-test

# data:  kte and kce
# t = -3.1068, df = 3.9651, p-value = 0.03641
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -4.0375123 -0.2196992
# sample estimates:
#  mean of x mean of y 
# 399.5921  401.7207  

# There was a change in calorific value between treatment and control:

# Mean:
kcalC <- mean(nutri$Energy[control]) 
kcalT <- mean(nutri$Energy[treat]) 
kcalC # 401.7207 kcal/100g
kcalT # 399.5921 kcal/100g

# Change:
d <- (kcalT - kcalC)*100/kcalC
d # Change in energy content was negligible (reduced by -0.5298721%).

# Q2.7) Was there a change in final moisture content between treated and untreated Nshima?

ktm <- nutri$Moisture[treat]
kcm <- nutri$Moisture[control]

sd(ktm)
sd(kcm)

t.test(ktm,kcm, conf.level=0.95)

#       Welch Two Sample t-test

# data:  ktm and kcm
# t = 0.58598, df = 3.9819, p-value = 0.5895
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -0.1315598  0.2017876
# sample estimates:
#  mean of x mean of y 
# 75.80906  75.77394

# There was no change in final moisture content between treatment and control:

moistC <- mean(nutri$Moisture[control]) 
moistT <- mean(nutri$Moisture[treat]) 
moistC # 75.77394%
moistT # 75.80906%

# <<<<<<<<<<<<<<<<<<<<< end of script >>>>>>>>>>>>>>>>>>>
