##################################################
# Bone collagen - Quality Control
# Workshop May 2023 Kiel

# Angela Trentacoste 

# Scatter plots for bone collagen SI values
# Various plots for enamel SI values
##################################################

#load packages. Install these first if you do not have them.
library(tidyverse)
library(readxl)
library(openxlsx) #saving excel files
library(ggpubr)


# Start by organizing your files
# What is the working directory?
getwd()
# Make sure this where your want to be working. 
# Otherwise set the working directory or make a new project.

# Load data
# First I make two objects with the names of the files I want
input_file_collagen <- "Input/UrbanHerdsCollagenResultsWkShp.xlsx"
input_file_info <- "Input/UrbanHerds_SampleList_Analysis.xlsx"
# Then I read in the data
ColResults <- read_excel(input_file_collagen,  .name_repair = "universal")
ColResults$ASIL <- as.numeric(ColResults$ASIL)
SpecimenInfo <- read_excel(input_file_info, sheet = "SpecimenInfo",  .name_repair = "universal")
SpecimenInfo <- subset(SpecimenInfo, Material == "Bone")
ColSampleInfo <- read_excel(input_file_info, sheet = "CollagenInfo",  .name_repair = "universal")

# Join results and sample info
# Drop extra columns from results
ColResults <- ColResults %>% 
  select(ASIL, d15N, d13C, per.C, per.N, Date)

#Join sample info to specimen info, join to results
# what samples are missing:
missing.joins <- anti_join(SpecimenInfo, ColSampleInfo, by = "ASIL")
# join
All.Info <- left_join(SpecimenInfo, ColSampleInfo, by = "ASIL")
# Check for missing joins then join
missing.joins <- anti_join(ColResults, All.Info, by = "ASIL")

###### AN ERROR!
# Fix by changing the ASIL data class in ColResults df

# Finishing joining
Coldata <- left_join(ColResults, All.Info, by = "ASIL")


# I want to analyse my taxa in groups
# What taxa do we have?
unique(Coldata$Taxon)

# Let's make a new Taxon group column 
Coldata <- Coldata %>% 
  mutate(TaxonGroup = Taxon,
         TaxonGroup = replace(TaxonGroup, TaxonGroup %in% c("Ovis?", "Ovis", "Capra", "Capra?"), "Ovis/Capra"),
         TaxonGroup = replace(TaxonGroup, TaxonGroup %in% c("Canis", "Vulpes"), "Canis/Vulpes"))

# Quality indicators ---------------------------------------------------------------

# Collagen yield
# > 0.5-2% 
#Ambrose, 1990; DeNiro and Weiner, 1988; Dobberstein et al., 2009; Van Klinken, 1999 
ggplot(Coldata, aes(x="Collagen yield", y=ColPer, label = ASIL, color = Site)) +
  geom_jitter() + 
  geom_hline(yintercept = 0.005, color = "red") + 
  geom_hline(yintercept = 0.01, color = "red", linetype = "dashed")


# C:N
# Guiry and Szpak 2021 = 3.15–3.45/5 (conservative), 3.9 (liberal)
# van Klinken 1999: C:N = 3.1-3.65
# EDTA == 2.8-3.25  
# first calculate the C:N ratio
Coldata$C.N <- Coldata$per.C/Coldata$per.N*14/12
# then plot
ggplot(Coldata, aes(x="C/N", y=C.N, label = ASIL, color = Site)) +
  geom_jitter() +
  geom_hline(yintercept = 2.9, color = "red") + 
  geom_hline(yintercept = 3.6, color = "red") +
  geom_hline(yintercept = 3.1, color = "red", linetype="dashed") + 
  geom_hline(yintercept = 3.5, color = "red", linetype="dashed")

# Remove crazy samples and then put remaining data in new Coldata.qual df
Coldata.qual <- subset(Coldata, C.N > 3 & C.N < 3.5)

# Percent C and N
# van Klinken 1999: %C = 30-46%, %N = 10-17% 
# Ambrose 1990: %c > 13, %N > 4.8
ggplot(Coldata.qual, aes(per.C, per.N, color = Site, label = ASIL)) +
  geom_point() + 
  scale_x_continuous(breaks = seq(from = 15, to = 50, by = 1)) +
  scale_y_continuous(breaks = seq(from = 5, to = 20, by = 1)) +
  geom_vline(xintercept = 30) + geom_vline(xintercept = 46) + 
  geom_hline(yintercept = 10) + geom_hline(yintercept = 17) #+ geom_text(hjust = -0.5)

# Remove crazy samples
Coldata.qual <- subset(Coldata.qual, per.C > 30)


# d13C vs C:N
# here we are using a new fucntion ggscatter() from the package ggpubr
# this makes it easier to compute and visualise statistics
ggscatter(Coldata.qual, 
          x = "C.N", y = "d13C", 
          cor.method = "pearson",
          add = "reg.line",
          cor.coef = TRUE) +
  facet_wrap(Site~.)
# Can also wrap by Site and taxon group:  facet_wrap(Site~TaxonGroup)

# d15N vs C:N
ggscatter(Coldata.qual, 
          x = "C.N", y = "d15N", 
          cor.method = "pearson",
          add = "reg.line",
          cor.coef = TRUE) +
  facet_wrap(Site~.)

# Replicates and duplicates
# We want to pull out replicates
Reps <- Coldata.qual %>%    # from Coldata.qual
  group_by(ASIL, Site) %>%   # group by ASIL and Site
  filter(n()>1) %>%         # Filter values > 1, i.e. select groups     
  mutate(C.diff = max(d13C)-min(d13C),  # add new columns with the range of SI values
         N.diff = max(d15N)-min(d15N))

ggplot(Reps, aes(x = C.diff, y = N.diff, color = Site)) +
  geom_point() +
  geom_text(data = subset(Reps, C.diff > 0.4 | N.diff >0.4),
                   aes(x = C.diff, y = N.diff, 
                   color = Site, label = ASIL), nudge_x = 0.02)


# Remove ASIL with >0.5 diff between reps
bad.reps <- subset(Reps, N.diff > 0.5 | C.diff > 0.5) %>%
  pull(ASIL)

Coldata.qual <- subset(Coldata.qual, !(ASIL %in% bad.reps))

# If we run check the replicates again, now only replicates with <0.5 difference remain
# Average remaning reps
Coldata.qual.clean <- Coldata.qual %>% 
  group_by(ASIL, Site, Taxon, TaxonGroup) %>% 
  summarise(d15N.m = mean(d15N),
            d13C.m = mean(d13C))

# Save this file
write.xlsx(Coldata.qual.clean, "Output/QCCollagenClean.xlsx")





