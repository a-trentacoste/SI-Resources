##################################################
# Getting started in R
# Workshop May 2023 Kiel

# Angela Trentacoste 
##################################################

#load packages. Install these first if you do not have them.
library(tidyverse) # family of packages including dplyr and ggplot
library(readxl) #importing excel files
library(openxlsx) #saving excel files

# Start by organizing your files
# What is the working directory?
getwd()
# Make sure this where your want to be working. 
# Otherwise set the working directory or make a new project.

# Let's get in some data!
UHCollData <- read_xlsx("Input/UrbanHerdsCollagenResultsWkShp.xlsx", .name_repair = "universal")
# Let's see what we have imported  
str(UHCollData) #check the structure of the dataframe using str() function
# We get an error if we are not using a valid object
# e.g. if we misspell the name or have things in lowercase
str(UHcolldata)

# Key syntax
# <-  'put into'
# %>%  tidyverse pipe, 'then'
# Example A:  Make a new dataframe called SIvalues with just ASIL no., d13C, and d15N columns
SIvalues.A <- UHCollData %>%  # make new object, CollData, then
            select(ASIL, d13C, d15N)  # select columns

# Example B:  We can do the same thing in base R, but it is less intuitive
SIvalues.B <- UHCollData[,c("ASIL", "d13C", "d15N")]

#################################################################
# TASK 1
#################################################################

# Make a new dataframe from CollData called 'SIvalues' 
# that contains columns for ASIL, Date, per.C, per.N, d13C, d15N 
# Tip: Copy and past the code from example A on line2 31-32
# then update name of the df and the column names to match those you want 
# write your code here:

SIvalues <- UHCollData %>%  # make new object, CollData, then
  select(ASIL, d13C, d15N, per.C, per.N, Date)  # select column

#################################################################
=
# We can add and manipulate columns using base R or tidyverse
# Add a new column called "C.N" with the atomic C:N ratio

# Base R - new colum C.N
SIvalues$C.N <- SIvalues$per.C/SIvalues$per.N*14/12

# tidyverse - new column called C.N2
SIvalues <- SIvalues %>%  
  mutate(C.N = (per.C/per.N)*(14/12))   # create a new column


########################################################

# Import data from 
# Vaiglova, P., J. Coleman, C. Diffey, V. Tzevelekidi, M. Fillios, M. Pappa, 
# P. Halstead, S. M. Valamoti, W. Cavanagh, J. Renard, M. Buckley and A. Bogaard (2021). 
# "Exploring Diversity in Neolithic Agropastoral Management in Mainland Greece Using Stable Isotope Analysis." 
# Environmental Archaeology: 1-24.
# https://doi.org/10.1080/14614103.2020.1867292


#################################################################
# TASK 2
#################################################################

# I have made an excel from the supplemental animal 
# data called Vaiglova_2023_collagen.xlsx
# import it into a new object called NeoGreece 
# copy/paste and adjust the code from line 20 above
# write your code here:

NeoGreece <- read_xlsx("Vaiglova_2023_collagen.xlsx", 
                       .name_repair = "universal")

#####################################################################

# Check that numeric columns are numeric
str(NeoGreece)

# Some columns have been imported with crazy names
# so let's change them to something easier to remember using the rename() function

NeoGreece2 <- NeoGreece %>% 
  rename(Element = Element..animal.bone....Seed.fragments..plants.,  #new name = old name (copy the old name from the console)
         d13C = d13CVPDB, 
         d15N = d15NAIR,
         perC = ..C,
         perN = ..N,
         C.N = atomic.C.N)

# What taxa do we have? 
# Want to get unique/distinct values from the Taxon column
# We can do this in base R using unique() and the $ sign
unique(NeoGreece2$Taxon)
# And we can do it in tidyverse
NeoGreece2 %>% distinct(Taxon)

# For our analysis we will want to group sheep/goats and red/roe deer
# Let's make a new column and then update the values for each Taxon 
NeoGreece2 <- NeoGreece2 %>%        # NeoGreece2 then...
  mutate(TaxonGroup = Taxon)  %>%  #
  mutate(TaxonGroup = replace(TaxonGroup, TaxonGroup == "sheep", "sheep/goat"))

# We can do the same thing in Base R
NeoGreece2$TaxonGroup <- NeoGreece2$Taxon # Make new column TaxonGroup and fill it with column Taxon
# In TaxonGroup column, where TaxonGroup column is “sheep” put in with “sheep/goat”
NeoGreece2$TaxonGroup[NeoGreece2$TaxonGroup == 'sheep'] <- 'sheep/goat'

#################################################################
# TASK 3
#################################################################
# In the TaxonGroup column replace 'goat' with 'sheep/goat' and
# both 'roe deer' and 'red deer' with 'deer.
# Copy/paste either the code either from lines 107-112 or 117

NeoGreece2 <- NeoGreece2 %>%        # NeoGreece2 then...
  mutate(TaxonGroup = Taxon)  %>%  
  mutate(TaxonGroup = replace(TaxonGroup, TaxonGroup == "sheep", "sheep/goat"),
         TaxonGroup = replace(TaxonGroup, TaxonGroup == "goat", "sheep/goat"))



#####################################################################





# Now let's write our new dataframe as a csv
#write as a cvs() NeoGreece2, "save it at this location"
write.csv(NeoGreece2, "Output/NeoGreeceCollagen.csv") # use this if you use . as a decimal and , as seperator 
write.csv2(NeoGreece2, "Output/NeoGreeceCollagen2.csv") # use this if you use comma , as a decimal and ; as seperator 
write.xlsx(NeoGreece2,"Output/NeoGreeceCollagen3.xlsx")  # as excel file
# you can leave notes


# TASK 1: SIvalues <- UHCollData %>% select(ASIL, d13C, d15N, per.C, per.N)  # make new object, UHCollData, then select columns

# TASK 2: NeoGreece <- read_xlsx("Vaiglova_2023_collagen.xlsx", .name_repair = "universal")

#TASK 3 
# NeoGreece2 <- NeoGreece2 %>%        # NeoGreece2 then...
 # mutate(TaxonGroup = Taxon )  %>%  # make new column (new name = old name)
  # now change the value of each taxon
  # mutate(TaxonGroup = replace(TaxonGroup, TaxonGroup == "sheep", "sheep/goat"),  
    #     TaxonGroup = replace(TaxonGroup, TaxonGroup == "goat", "sheep/goat"),   
     #    TaxonGroup = replace(TaxonGroup, TaxonGroup == "roe deer", "deer"),
      #   TaxonGroup = replace(TaxonGroup, TaxonGroup == "red deer", "deer"))


