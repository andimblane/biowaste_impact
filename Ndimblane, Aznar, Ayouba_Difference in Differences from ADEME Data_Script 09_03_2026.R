#===========================================#
#======     Difference in Differences ======#
#======.       from "ADEME Data"    ========#       
#===========================================#

### author        : Aissatou NDIMBLANE, Olivier AZNAR, Kassoum AYOUBA 
### last modified : 09.03.2026											                              

#==============    load packages    ========# 
library(dplyr)
library(writexl)
library(openxlsx)
library(MatchIt)
library(fixest)
library(ggplot2)
library(did)

#==============      load data       ========# 
### Treatment database
Traite_CS <- read.csv2("CS_Rapport_ADEME.csv", header = TRUE , dec = ",")
Traite_CS <- Traite_CS[, c("Code.SINOE", "Année.de.démarrage.CS.biodéchets")] 
Traite_CS <- Traite_CS %>% rename(CODE = Code.SINOE)
Traite_CS <- Traite_CS %>% rename(ANNEE_MPCS = Année.de.démarrage.CS.biodéchets)

### data 2005-2021
col_2005 = read.csv2("Collecte_2005.csv", header = TRUE , dec = ",")                                       # 2005
col_2007 = read.csv2("Collecte_2007.csv", header = TRUE , dec = ",")                                       # 2007
col_2009 = read.csv2("Collecte_2009.csv", header = TRUE , dec = ",")                                       # 2009
col_2011 = read.csv2("Collecte_2011.csv", header = TRUE , dec = ",")                                       # 2011
col_2011 <- col_2011 %>% rename(POPULATION_DESSERVIE = Population.desservie.OMR)
col_2011 <- col_2011 %>% rename(TONNAGE_OMR = Tonnage.collecté.OMR)
col_2011 <- col_2011 %>% rename(OMR = Ratio.de.collecte.OMR)
col_2013 = read.csv2("Collecte_2013.csv", header = TRUE , dec = ",")                                       # 2013
col_2015 = read.csv2("Collecte_2015.csv", header = TRUE , dec = ",")                                       # 2015
col_2017 = read.csv2("Collecte_2017.csv", header = TRUE , dec = ",")                                       # 2017
col_2019 = read.csv2("Collecte_2019.csv", header = TRUE , dec = ",")                                       # 2019
col_2021 = read.csv2("Collecte_2021.csv", header = TRUE , dec = ",")                                       # 2021
col_2021 <- col_2021 %>% rename(POPULATION_DESSERVIE = Population.desservie.OMR)
col_2021 <- col_2021 %>% rename(TONNAGE_OMR = Tonnage.collecté.OMR)
col_2021 <- col_2021 %>% rename(OMR = Ratio.de.collecte.OMR)

B_col <- rbind(col_2005, col_2007, col_2009, col_2011, col_2013, col_2015, col_2017, col_2019, col_2021)   # Combine data
Base1 <- merge(Traite_CS, B_col,by = "CODE", all.x=TRUE, all.y = TRUE)                                     # database join     

### Database processing 
Traite_CS$ANNEE_MPCS[Traite_CS$ANNEE_MPCS <= 2004] <- 2005
Traite_CS$ANNEE_MPCS[Traite_CS$ANNEE_MPCS == 2006] <- 2007
Traite_CS$ANNEE_MPCS[Traite_CS$ANNEE_MPCS == 2008] <- 2009
Traite_CS$ANNEE_MPCS[Traite_CS$ANNEE_MPCS == 2010] <- 2011
Traite_CS$ANNEE_MPCS[Traite_CS$ANNEE_MPCS == 2012] <- 2013
Traite_CS$ANNEE_MPCS[Traite_CS$ANNEE_MPCS == 2014] <- 2015
Traite_CS$ANNEE_MPCS[Traite_CS$ANNEE_MPCS == 2016] <- 2017
Traite_CS$ANNEE_MPCS[Traite_CS$ANNEE_MPCS == 2018] <- 2019
Traite_CS$ANNEE_MPCS[Traite_CS$ANNEE_MPCS == 2020] <- 2021
Traite_CS$Traitment <- ifelse (Traite_CS$ANNEE_MPCS == 0, 0, 1)                                            # Treatment

Base2 <- subset(Base1, !is.na(OMR) & !is.na(TONNAGE_OMR) & OMR != 0 & TONNAGE_OMR != 0)
Base2 <- Base2 %>% mutate(ANNEE_MPCS = ifelse(is.na(ANNEE_MPCS), 0, ANNEE_MPCS))
Base2 <- Base2[!grepl("Commune", Base2$NOM), ]

#==============   Descriptive statistics   ========#
### Never treated
jamais_traite <- Base2 %>%
  group_by(CODE) %>%
  summarise(max_MPCS = max(ANNEE_MPCS, na.rm = TRUE)) %>%
  filter(max_MPCS == 0)
nrow(jamais_traite)
S=jamais_traite$CODE

### Treated
traite <- Base2 %>%
  group_by(CODE) %>%
  summarise(max_MPCS = max(ANNEE_MPCS, na.rm = TRUE)) %>%
  filter(max_MPCS > 0)
nrow(traite)
L= traite$CODE                                                                                             # Code treated

### Treated by year
nb_traite_par_annee <- Base2 %>%
  filter(ANNEE_MPCS > 0) %>%         
  distinct(CODE, .keep_all = TRUE) %>%  
  count(ANNEE_MPCS)
nb_traite_par_annee
nb_traite_par_annee %>%
  mutate(cumul = cumsum(n))


#============== Model estimation: staggered DiD ========#
### ATT(g,t)
est <- att_gt(
  yname = "OMR",
  tname = "ANNEE",
  idname = "CODE",
  gname = "ANNEE_MPCS",
  xformla = ~1,
  data = Base2,
  control_group = "nevertreated",
  allow_unbalanced_panel = TRUE,
)
summary(est)
ggdid(est)

### Aggregation of ATT(g,t) in several ways
aggte(est, type = "simple")                                                                                # Overall ATT
aggte(est, type = "group")                                                                                 # ATT for each group
aggte(est, type = "calendar")                                                                              # ATT for each calendar year
aggte(est, type = "dynamic")                                                                               # ATT for each calendar year


#============== Heterogeneity analysis ========#
### Import socioeconomic Variables 
Tourisme_EPCI_2009 <- read.csv2("Tourisme_EPCI_2009.csv", header = TRUE , dec = ",")                       # Tourist Accomodations (2011)
Tourisme_EPCI_2009 <- aggregate(NBRE_HEBERG_TOUR ~ CODE, data = Tourisme_EPCI_2009, FUN = function(x) sum(x, na.rm = TRUE))

Densite_EPCI_2009 <- read.csv2("Densite_EPCI_2009.csv", header = TRUE , dec = ",")                         # Density (2009)
Densite_EPCI_2009 <- aggregate(DENSITE ~ CODE, data = Densite_EPCI_2009, FUN = function(x) mean(x, na.rm = TRUE))
Densite_EPCI_2009 <- Densite_EPCI_2009 %>%
                     mutate(DENSITE = replace(DENSITE, DENSITE == "Inf", NA))

Revenu_EPCI_2009 <- read.csv2("revenu_moyen_EPCI_2011.csv", header = TRUE , dec = ",")                     # Income (2011) 
Revenu_EPCI_2009 <- aggregate(REVENU ~ CODE, data = Revenu_EPCI_2009, FUN = function(x) mean(x, na.rm = TRUE))

Tarification_2013 = read.csv2("Tarification2013.csv", header = TRUE , dec = ",")                           # Pricing (2013)
Tarification_2013$TEOM_REOM = 0
Tarification_2013$TEOM_REOM[Tarification_2013$TEOM == "Oui"] = 1
Tarification_2013$TEOM_REOM[Tarification_2013$REOM.classique == "Oui"] = 1
Tarification_2013$TEOM_REOMinc = 0
Tarification_2013$TEOM_REOMinc[Tarification_2013$TEOM.incitative == "Oui"] = 1
Tarification_2013$TEOM_REOMinc[Tarification_2013$REOM.incitative == "Oui"] = 1
Tarification_2013$TEOM_REOMinc[Tarification_2013$Redevance.spéciale == "Oui"] = 1
Tarification_2013$MODE_TARIFICATION = NA
Tarification_2013$MODE_TARIFICATION[Tarification_2013$TEOM_REOM == 1] = "T_CLASSIQUE"
Tarification_2013$MODE_TARIFICATION[Tarification_2013$TEOM_REOMinc == 1] = "T_INCITATIVE"
Tarification_2013$MODE_TARIFICATION[Tarification_2013$TEOM_REOM == 1 & Tarification_2013$TEOM_REOMinc == 1] = "T_MIXTE"
Tarification_2013F <- subset(Tarification_2013, select = c(Code_SINOE , MODE_TARIFICATION))                # subset

### Database processing 
merged_data <- merge(Tourisme_EPCI_2009, RS_EPCI_2009, by = "CODE", all = TRUE)                            # Database join
merged_data <- merge(merged_data, Densite_EPCI_2009, by = "CODE", all = TRUE)
merged_data <- merge(merged_data, Revenu_EPCI_2009, by = "CODE", all = TRUE)
B_VarApparie <- merge(BF, merged_data , by = "CODE", all.x = TRUE)
B_Varcontrol1 <- B_VarApparie 
B_Varcontrol <- merge(B_Varcontrol1, Tarification_2013F, by.x = "CODE", by.y = "Code_SINOE", all.x = TRUE) # database join
write.csv2(B_Varcontrol, 'B_Varcontrol.csv')                                                               # Export                

CODE_DEP = read.csv2("CODE_DEP.csv", header = TRUE , dec = ",")                                            # Imputation of missing socioeconomic variables based on the department average 
B_Varcontrol <- merge(B_Varcontrol, CODE_DEP, by = "CODE", all.x = TRUE)
vars_a_imputer <- c("NBRE_HEBERG_TOUR", "DENSITE", "REVENU")
impute_mean_by_dept <- function(data, dep_col, var_col) {
    data %>%
    group_by_at(dep_col) %>% 
    mutate_at(vars(var_col), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>% 
    ungroup()
}
for (var in vars_a_imputer) {
  B_Varcontrol <- impute_mean_by_dept(B_Varcontrol, "COD_DEPARTEMENT", var)
}
B_Varcontrol <- B_Varcontrol %>% select(-ANNEE)


#==============   Descriptive statistics   ========#
### Residual household waste by group and year
STAT1 <- B_Varcontrol %>% filter(T.CS == 1)
STAT0 <- B_Varcontrol %>% filter(T.CS == 0)
SummarySTAT1 <- summary(STAT1)
SummarySTAT0 <- summary(STAT0)
write.csv2(SummarySTAT1, 'SummarySTAT1.csv')
write.csv2(SummarySTAT0, 'SummarySTAT0.csv')

STAT1 <- B2011 %>% filter(T.CS == 1)
STAT0 <- B2011 %>% filter(T.CS == 0)
SummarySTAT1 <- summary(STAT1)
SummarySTAT0 <- summary(STAT0)
summary(B2011)

STAT1 <- B2021 %>% filter(T.CS == 1)
STAT0 <- B2021 %>% filter(T.CS == 0)
SummarySTAT1 <- summary(STAT1)
SummarySTAT0 <- summary(STAT0)
summary(B2021)

### Pricing
tarif_vec <- ifelse(is.na(B_Varcontrol$MODE_TARIFICATION), "Données manquantes", B_Varcontrol$MODE_TARIFICATION)
desired_order <- c("T_CLASSIQUE", "T_INCITATIVE", "T_MIXTE", "Données manquantes")
tarif_vec <- factor(tarif_vec, levels = desired_order)
Counts <- table(tarif_vec)
Labels <- names(Counts)
Labels_english <- c("Classic pricing", "incentive pricing", "mixed pricing", "missing data")
colors <- gray.colors(4, start = 0.8, end = 0.3)
densities <- c(90, 20, NA, NA)   
angles    <- c(135, 0, 45, 0)     
colors[4] <- "white"
bar_positions <- barplot(
  Counts,
  names.arg = Labels_english,
  col = colors,
  density = densities,
  angle = angles,
  ylim = c(0, 190),
  main = "",
  ylab = "Number of intermunicipal cooperation entities",
  xlab = "",
  border = "black"
)
text(
  x = bar_positions,
  y = Counts + max(Counts) * 0.05,
  labels = Counts,
  cex = 0.8
)
box()
legend(
  x = max(bar_positions) * 0.93,  
  y = max(Counts) * 1.22,
  legend = Labels_english,
  fill = colors,
  density = c(80, 40, NA, NA),
  angle = c(135, 0, 45, 0),
  title = "Pricing system",
  cex = 0.8
)


#=========== Model estimation : Canonical DiD ========#
### Database with 2 periods
B2011_2021 <- bind_rows(B2011, B2021)
B2011_2021 <- B2011_2021 %>%
  mutate(ANNÉE = ifelse(ANNEE == 2011, 0, ifelse(ANNEE == 2021, 1, NA)))
BASEF <- merge(B2011_2021, B_Varcontrol , by = "CODE", all.x = TRUE)
summary(BASEF)

names(BASEF)[names(BASEF) == "TRAITEMENT"] <- "TREATMENT"                                                  # Rename variables
names(BASEF)[names(BASEF) == "ANNÉE"] <- "YEAR"
names(BASEF)[names(BASEF) == "NBRE_HEBERG_TOUR"] <- "TOURIST_ACCOMMODATION"
names(BASEF)[names(BASEF) == "DENSITE"] <- "DENSITY"
names(BASEF)[names(BASEF) == "REVENU"] <- "INCOME"
names(BASEF)[names(BASEF) == "MODE_TARIFICATION"] <- "PRICING"
names(BASEF)[names(BASEF) == "OMR"] <- "RHW"
names(BASEF)[names(BASEF) == "CODE"] <- "ID"
names(BASEF)[names(BASEF) == "T.CS.x"] <- "SEPARATE_COLLECTION"
names(BASEF)[names(BASEF) == "home_composting.y"] <- "HOME_COMPOSTING"
BASEF <- BASEF %>%
  mutate(PRICING = case_when(
    PRICING == "T_INCITATIVE" ~ "INCENTIVE",
    PRICING == "T_MIXTE" ~ "MIXED",
    PRICING == "T_CLASSIQUE" ~ "CLASSIC",
    TRUE ~ as.character(PRICING) 
  ))


### 1 -  Check Parallels trends : placebo tests using pseudo-treatment periods (2007-2011 & 2009-2011)
# 2007-2011
B2011_2007 <- bind_rows(B2011, B2007)
B2011_2007 <- B2011_2007 %>%
  mutate(ANNÉE = ifelse(ANNEE == 2011, 1, ifelse(ANNEE == 2007, 0, NA)))
BasePlacebo2011_2007 <- merge(B2011_2007, B_Varcontrol , by = "CODE", all.x = TRUE)

BasePlacebo2011_2007 <- BasePlacebo2011_2007  %>%                                                          # Rename variables
  rename(TRAITEMENT_CS = T.CS.x)
BasePlacebo2011_2007  <- BasePlacebo2011_2007  %>%
  rename(TRAITEMENT_CD = T.CD.x)
BasePlacebo2011_2007  <- BasePlacebo2011_2007  %>%
  rename(TRAITEMENT_CC = T.CC.x)

BasePlacebo2011_2007$d = BasePlacebo2011_2007$TRAITEMENT_CS*BasePlacebo2011_2007$ANNÉE                     
mCS <- feols(OMR ~  d + TRAITEMENT_CD + TRAITEMENT_CC |TRAITEMENT_CS+ANNÉE, data = BasePlacebo2011_2007)  # Model
etable(mCS)

# 2009-2011
B2011_2009 <- bind_rows(B2011, B2009)
B2011_2009 <- B2011_2009 %>%
  mutate(ANNÉE = ifelse(ANNEE == 2011, 1, ifelse(ANNEE == 2009, 0, NA)))
BasePlacebo2011_2009 <- merge(B2011_2009, B_Varcontrol , by = "CODE", all.x = TRUE)

BasePlacebo2011_2009 <- BasePlacebo2011_2009  %>%                                                          # Rename variables
  rename(TRAITEMENT_CS = T.CS.x)
BasePlacebo2011_2009  <- BasePlacebo2011_2009  %>%
  rename(TRAITEMENT_CD = T.CD.x)
BasePlacebo2011_2009  <- BasePlacebo2011_2009  %>%
  rename(TRAITEMENT_CC = T.CC.x)

BasePlacebo2011_2009$d = BasePlacebo2011_2009$TRAITEMENT_CS*BasePlacebo2011_2009$ANNÉE                     
mCS <- feols(OMR ~  d + TRAITEMENT_CD + TRAITEMENT_CC |TRAITEMENT_CS+ANNÉE, data = BasePlacebo2011_2009)  # Model
etable(mCS)

### 2 -  trends are not parallels ==> Combining matching with difference-in-differences  

# Matching data
DonneesMatch2 <- merge(B2011, B_Varcontrol , by = "CODE", all.x = TRUE)
DonneesMatch2 <- DonneesMatch2 %>%
  filter(!is.na(T.CS.x),                                                                                  # propensity score matching      
         !is.na(REVENU),
         !is.na(DENSITE),
         !is.na(NBRE_HEBERG_TOUR),
         !is.na(MODE_TARIFICATION),
         !is.na(PPLUS65ans))
match_out2 <- matchit(
  T.CS.x ~ REVENU + DENSITE + NBRE_HEBERG_TOUR + MODE_TARIFICATION,
  data = DonneesMatch2,
  method = "nearest",   
  ratio = 1
)
matched_data2 <- match.data(match_out2)                                                                   # matched data
ids_matched2 <- matched_data2$CODE
B_matched_long2 <- B2011_2021 %>% filter(CODE %in% ids_matched2)

# Descriptive statistics for matching data
B_Varcontrol <- B_Varcontrol %>% select(-ANNEE)
summary(B_Varcontrol)
B_Varcontrol_filtre <- B_Varcontrol[B_Varcontrol$CODE %in% B_matched_long2$CODE, ]

STAT1 <- B_Varcontrol %>% filter(T.x == 1)
STAT0 <- B_Varcontrol %>% filter(T.x == 0)

SummarySTAT1 <- summary(STAT1)
SummarySTAT0 <- summary(STAT0)

write.csv2(SummarySTAT1, 'SummarySTAT1.csv')
write.csv2(SummarySTAT0, 'SummarySTAT0.csv')

# Graphical representation : Check in Parallel trends for matching data
B_tot_filtre <- B_tot[B_tot$CODE %in% B_matched_long2$CODE, ]
result_omr <- B_tot_filtre %>%
  filter(!is.na(T.CS)) %>%
  group_by(ANNEE, T.CS) %>%
  summarise(Moyenne_OMR = mean(OMR, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(ANNEE) & !is.na(Moyenne_OMR) & ANNEE > 2005)
omr_plot <- ggplot(result_omr, aes(x = ANNEE, y = Moyenne_OMR, color = as.factor(T.CS))) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "SEPARATE COLLECTION",
       x = "Year",
       y = "Residual Household Waste (kg/capita)",
       color = "") +
  scale_y_continuous(limits = c(200, 350)) +
  scale_x_continuous(breaks = seq(2007, 2021, by = 2)) +  
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("0" = "Control group", "1" = "Treatment Group")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.line = element_line(size = 0.5, color = "black")
  )
print(omr_plot)

### Placebo tests using pseudo-treatment periods (2007-2011 & 2009-2011) : Check in Parallel trends for matching data

# 2007-2011
B2011_2007 <- bind_rows(B2011, B2007)
B2011_2007 <- B2011_2007 %>%
  mutate(ANNÉE = ifelse(ANNEE == 2011, 1, ifelse(ANNEE == 2007, 0, NA)))
BasePlacebo2011_2007 <- merge(B2011_2007, B_Varcontrol , by = "CODE", all.x = TRUE)
BasePlacebo2011_2007 <- BasePlacebo2011_2007 %>% filter(CODE %in% ids_matched1) 
BasePlacebo2011_2007 <- BasePlacebo2011_2007  %>%                                                              # Rename variables
  rename(TRAITEMENT_CS = T.CS.x)
BasePlacebo2011_2007  <- BasePlacebo2011_2007  %>%
  rename(TRAITEMENT_CD = T.CD.x)
BasePlacebo2011_2007  <- BasePlacebo2011_2007  %>%
  rename(TRAITEMENT_CC = T.CC.x)
BasePlacebo2011_2007$d <- BasePlacebo2011_2007$TRAITEMENT_CC *BasePlacebo2011_2007$ANNÉE                       # Model
mCS_CD_CC <- feols(OMR ~  d|TRAITEMENT+ANNÉE, data = BasePlacebo2011_2007)

# 2009-2011
B2011_2009 <- bind_rows(B2011, B2009)
B2011_2009 <- B2011_2009 %>%
  mutate(ANNÉE = ifelse(ANNEE == 2011, 1, ifelse(ANNEE == 2009, 0, NA)))
BasePlacebo2011_2009 <- merge(B2011_2009, B_Varcontrol , by = "CODE", all.x = TRUE)
BasePlacebo2011_2009 <- BasePlacebo2011_2009 %>% filter(CODE %in% ids_matched1) 
BasePlacebo2011_2009 <- BasePlacebo2011_2009  %>%                                                              # Rename variables
  rename(TRAITEMENT_CS = T.CS.x)
BasePlacebo2011_2009  <- BasePlacebo2011_2009  %>%
  rename(TRAITEMENT_CD = T.CD.x)
BasePlacebo2011_2009  <- BasePlacebo2011_2009  %>%
  rename(TRAITEMENT_CC = T.CC.x)
BasePlacebo2011_2009$d <- BasePlacebo2011_2009$TRAITEMENT_CC *BasePlacebo2011_2009$ANNÉE                       # Model
mCS_CD_CC <- feols(OMR ~  d|TRAITEMENT+ANNÉE, data = BasePlacebo2011_2009)


### 3 - Trends are parallels ==> Estimation with fixest
B_matched_long2$ATT = B_matched_long2$T.CS*B_matched_long2$ANNÉE 
MODEL_SC <- feols(OMR ~  ATT + home_composting|T.CS+ANNÉE, data = B_matched_long2, cluster = ~CODE)
etable(MODEL_SC)

tab <- etable(MODEL_SC)                                                                                        # Export results
tab_df <- as.data.frame(tab) 
write.csv(tab_df, "RESULTS.csv", row.names = FALSE) 

#=========== Effect by economic and sociodemographic characteristics ========#
# Data
B_heter = merge(B_matched_long2, B_Varcontrol , by = "CODE", all.x = TRUE)
summary (B_heter)

# Tourist Accommodations
B_heter$NBRE_HEBERG_TOUR2 <- ifelse(B_heter$NBRE_HEBERG_TOUR >= median(B_heter$NBRE_HEBERG_TOUR, na.rm = TRUE), "High", "Low")
B_heter$NBRE_HEBERG_TOUR2 <- factor(B_heter$NBRE_HEBERG_TOUR2)
SEPARATE_COLLECTION <- feols(OMR ~  T.CS.x*ANNÉE*NBRE_HEBERG_TOUR2 + home_composting.y, data = B_heter, cluster = ~CODE)
etable(SEPARATE_COLLECTION)

# Density
B_heter$DENSITE2 <- ifelse(B_heter$DENSITE >= median(B_heter$DENSITE, na.rm = TRUE), "High", "Low")
B_heter$DENSITE2 <- factor(B_heter$DENSITE2)
SEPARATE_COLLECTION <- feols(OMR ~  T.CS.x*ANNÉE*DENSITE2 + home_composting.y, data = B_heter, cluster = ~CODE)
etable(SEPARATE_COLLECTION)

# Income
B_heter$INCOME2 <- ifelse(B_heter$REVENU >= median(B_heter$REVENU, na.rm = TRUE), "High", "Low")
B_heter$INCOME2 <- factor(B_heter$INCOME2)
SEPARATE_COLLECTION <- feols(OMR ~  T.CS.x*ANNÉE*INCOME2 + home_composting.y, data = B_heter, cluster = ~CODE)
etable(SEPARATE_COLLECTION)

# Pricing
SEPARATE_COLLECTION <- feols(OMR ~  T.CS.x*ANNÉE*MODE_TARIFICATION + home_composting.y, data = B_heter, cluster = ~CODE)
etable(SEPARATE_COLLECTION)
#===========       the end       ========#



















