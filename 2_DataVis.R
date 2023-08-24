##################################################
# Data Vis
# Workshop May 2023 Kiel

#### Angela Trentacoste

# Scatter plots for bone collagen SI values
# Various plots for enamel SI values
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

# Import Greek data that we organised last session
Data <- read_xlsx("Output/NeoGreeceCollagen3.xlsx", .name_repair = "universal")
# Make sure our columns look ok
str(Data)

### COLLAGEN -------------------------------------

# Scatter plots of increasing complexity ------------
ggplot(Data, aes(x=d13C, y=d15N))

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point()

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup))

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup))

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup), size = 3) # because size should apply to all points, it goes outside aes()

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup), size = 3) +
  facet_wrap(.~Site)      # keeps scales the same in each plot

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup), size = 3) +
  facet_grid(.~Site)      # keeps scales the same in each plot

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup), size = 3) +
  facet_wrap(.~Site) +
  theme_bw() # change theme to black/white

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup), size = 3) +
  facet_wrap(.~Site) +
  theme_bw(base_size = 15) # change theme to black/white and increase text

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup), size = 3) +
  facet_wrap(.~Site) +
  theme_bw(base_size = 15) + # change theme to black/white and increase text
  theme(strip.background = element_rect(fill = "white", linetype = "blank")) # change facet label style

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup), size = 3) +
  facet_wrap(.~Site) +
  theme_bw(base_size = 15) + # change theme to black/white and increase text
  theme(strip.background = element_rect(fill = "white", linetype = "blank")) +  
  labs(   # Add delta notationn
    y = expression(paste(delta^{15}, "N (\u2030)")), 
    x = expression(paste(delta^{13}, "C (\u2030)"))
    )

ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup), size = 3) +
  facet_wrap(.~Site) +
  theme_bw(base_size = 15) + # change theme to black/white and increase text
  theme(strip.background = element_rect(fill = "white", linetype = "blank")) +  
  labs(   # Add delta notationn
    y=expression(paste(delta^{15}, "N (\u2030)")), 
    x = expression(paste(delta^{13}, "C (\u2030)")),
    colour = "Taxon", shape = "Taxon"   #change legend label to 'Taxon'
      )

##### Changing the order of the legend using factor() 

# step 1 - make a vector with the words in the order you want using the combine c() function
taxon.order <- c("sheep/goat", "cattle", "deer","hare", "fox","fish") # each word must have quotes and a comma

# Here we use the base R function factor ()
# to recode TaxonGroup as a factor vector not just a character vector
# factor vectors have order
Data$TaxonGroup <- factor(Data$TaxonGroup, levels=taxon.order)

# TaxonGroup has changed from a character vector to a factor with 6 levels
str(Data)

# Plot again and the legend is now in order
ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup), size = 3) +
  facet_wrap(.~Site) +
  theme_bw(base_size = 15) + # change theme to black/white and increase text
  theme(strip.background = element_rect(fill = "white", linetype = "blank")) +  
  labs(   # Add delta notation
    y=expression(paste(delta^{15}, "N (\u2030)")), 
    x = expression(paste(delta^{13}, "C (\u2030)")),
    colour = "Taxon", shape = "Taxon"   #change legend label to 'Taxon'
  )

###### Scatter plots - Stat ellipses -----
# By default, the stat_ellipse function assumes a multivariate t-distribution (type = "t"). 
# However, you can set type = "norm" to assume a multivariate normal distribution.
ggplot(Data, aes(x=d13C, y=d15N)) + 
  geom_point(aes(colour = TaxonGroup, shape = TaxonGroup), size = 3) +
  # add ellipse, fill based on TaxonGroup, use geom = poly to fill it in, make it transparent with alpha
  stat_ellipse(aes(fill = TaxonGroup), geom = "polygon",  alpha = 0.2) + 
  facet_wrap(.~Site) +
  theme_bw() +                      
  theme(strip.background = element_rect(fill = "white", linetype = "blank")) +  
  labs(   # Add delta notationn
    y=expression(paste(delta^{15}, "N (\u2030)")), 
    x = expression(paste(delta^{13}, "C (\u2030)")),
    colour = "Taxon", shape = "Taxon", fill = "Taxon"    # here we need to add fill = "Taxon" or ggplot will make an extra legend
  )



###### Scatter plots - sd range bars -----
# First we need to calculate the mean and sd of our data
DataSummary <- Data %>%             # make new object called DataSummary. From Data then....
  group_by(Site, TaxonGroup) %>%    # make groups according to Site and TaxonGroup...
  summarise(                        # for each group summmarise...
            Cmean = mean(d13C),     # make new column Cmean and fill it with the mean() of d13C
            Csd = sd(d13C),         # make new column Csd and fill it with the sd() of d13C
            Nmean = mean(d15N),
            Nsd = sd(d15N),
            Nmin = min(d15N),
            Nmax = max(d15N),
            Nrange = Nmax - Nmin)

ggplot() + # here we just call ggplot() because we are using different dataframes for big and small points
  geom_point(data = Data, aes(x=d13C, y=d15N, 
                              colour = TaxonGroup, shape = TaxonGroup), size = 2) +  # individuals
  geom_point(data = DataSummary, aes(x = Cmean, y=Nmean, colour = TaxonGroup), size = 5) +
  geom_errorbar(data = DataSummary, aes(x=Cmean, ymin=Nmean-Nsd, ymax=Nmean+Nsd, colour = TaxonGroup)) +
  geom_errorbarh(data = DataSummary, aes(y = Nmean, xmin=Cmean-Csd, xmax=Cmean+Csd, colour = TaxonGroup)) +
  facet_wrap(.~Site) +
  theme_bw() 

#####################################################################################################

#### TEETH   -------------------

#####################################################################################################

# I have my tooth metadaa and SI values in different excel files, so we need to join them first
# File with the enamel SI values
# This is an edited version of the file I get from Nils, which the SI values and other indo
input_file_enamel <- "Input/UrbanHerdsEnamelResults.xlsx"
# File with the tooth data (taxon, which tooth, upper/lower), ERJs and notes
input_file_info <- "Input/UrbanHerds_SampleList_Analysis.xlsx"

# Read data - here the excel file has different sheets, so we need to say which sheets we want
EnamelSI <- read_excel(input_file_enamel, sheet = "EnamelAllSites")

# I want two sheets from this excel file, so I load each into a df
SpecimenInfo <- read_excel(input_file_info, sheet = "SpecimenInfo")
EnSampleInfo <- read_excel(input_file_info, sheet = "EnamelInfo")

# Join results and sample info
# Drop extra columns from results - I only need the ASIL and SI values
EnamelSI <- EnamelSI %>% 
  select(Sample_ID, d13C, d18O)

# Join sample info to results, join to specimen info
# Are we missing any joins?
missing.joins <- anti_join(EnamelSI, EnSampleInfo, by = "Sample_ID")
# if not then we can join the data
Enameldata <- left_join(EnamelSI, EnSampleInfo, by = "Sample_ID")
# check for missing joins again
missing.joins <- anti_join(Enameldata, SpecimenInfo, by = "ASIL")
# and then join
Enameldata <- left_join(Enameldata, SpecimenInfo, by = "ASIL")


# Let's plot some SI curves!
# Here for d18O 
ggplot(Enameldata, aes(ERJ, d18O)) +     # make a ggplot() with ERJ and d18O
  geom_line() +                         # add line geometry
  geom_point(colour = "darkblue") +      # add point geometry. Make all points dark blue
  facet_wrap(vars(ASIL)) +              # wrap by ASIL, we need vars() to define the variables to wrap by (you can do more than one)
  theme_minimal()                     #nice white minimal theme


# Same for d13C 
ggplot(Enameldata, aes(ERJ, d13C)) +
  geom_line() +
  geom_point(colour = "darkgreen") +
  facet_wrap(vars(ASIL)) +
  theme_minimal()

# Here we put them both together using geom_line() and geom_point() x 2 - once for each SI
ggplot() +
  geom_line(data = Enameldata, aes(ERJ, d13C)) +                        #d13C
  geom_point(data = Enameldata, aes(ERJ, d13C), colour = "darkgreen") + #d13C
  geom_line(data = Enameldata, aes(ERJ, d18O)) +                        #d18O
  geom_point(data = Enameldata, aes(ERJ, d18O),colour = "darkblue") +   #d18O 
  facet_wrap(vars(ASIL)) 


########################################################################################
### Second axis -----------------------------
#######################################################################################

# Add a second axis for d13C - this needs to be scaled compared to d18O

ggplot() +
  geom_line(data = Enameldata, aes(ERJ, d13C + 6)) +                        #I want to lift d13C by 6 in the plot
  geom_point(data = Enameldata, aes(ERJ, d13C + 6), colour = "darkgreen") + #I want to lift d13C by 6 in the plot
  geom_line(data = Enameldata, aes(ERJ, d18O)) +                        
  geom_point(data = Enameldata, aes(ERJ, d18O),colour = "darkblue") +   
  facet_wrap(vars(ASIL)) +                                       
  theme_minimal() +
  # Set second axis = and name it d13C. This must be scaled in the opposite direction to line up with the d13C points
  # so I subtract 6 
  scale_y_continuous(name="d18O", sec.axis = sec_axis(~ . - 6, name="d13C"))

# Now its in the right place, so lets make the colours easier to read and adjust the tick/break positions
ggplot() +
  geom_line(data = Enameldata, aes(ERJ, d13C)) +                        #I want to lift d13C by 6 in the plot
  geom_point(data = Enameldata, aes(ERJ, d13C), colour = "darkgreen") + #I want to lift d13C by 6 in the plot
  geom_line(data = Enameldata, aes(ERJ, d18O)) +                        
  geom_point(data = Enameldata, aes(ERJ, d18O),colour = "darkblue") +   
  facet_wrap(vars(ASIL)) +                                      
  theme_minimal() +
  scale_y_continuous(name="d18O", 
                   breaks = seq(from = -12, to = 0, by = 2),
                   sec.axis = sec_axis(~ ., name="d13C",
                                       breaks = seq(-18, 6, 2))) +
  theme(axis.ticks.y.right = element_line(color = "darkgreen"),
          axis.text.y.right = element_text(color = "darkgreen"),
          axis.title.y.right = element_text(colour = "darkgreen"),
          axis.ticks.y.left = element_line(color = "darkblue"),
          axis.text.y.left = element_text(color = "darkblue"),
          axis.title.y.left = element_text(colour = "darkblue"))

########################################################################################
### SUBSETTING -------------------
########################################################################################

# Too many plots!! Let's seperate out the Sites into different dfs
# So we make a new df that is a subset our main df to select only values were Site = Orvieto

ORV <- subset(Enameldata, Site == "Orvieto")

######################################
# TASK 
#####################################
# Now do the same for the Marzabotto and Tarquinia
# Make new dataframes called "MAR" and "TAR"

MAR <- subset(Enameldata, Site == "Marzabotto")
TAR <- subset(Enameldata, Site == "Tarquinia")





########################################################################################
### COLOURS AND SHAPES 
########################################################################################

# Let's plot the d18O curves from ORV
# First, are these all mandibular teeth? Do you remember how to check?

unique(ORV$Element)
# Ok good, all mandibular, so we'll plot them together
# and we'll put the plot in its own object

ggplot(ORV) +
  geom_line(aes(ERJ, d18O, group = ASIL)) +
  geom_point(aes(ERJ, d18O, group = ASIL)) 

# We can to change the colour and fill of the points/line based on the ASIL
# This does inside aes() because we want it to change with each ASIL no.
ggplot(ORV) +
  geom_line(aes(ERJ, d18O, group = ASIL, colour = ASIL)) +
  geom_point(aes(ERJ, d18O, group = ASIL, colour = ASIL, fill = ASIL)) 
# But now we have a continous scale, because R thinkgs ASIL is a number (it is)
# but we want it to treat it like a factor, not a continuous variable so we need to tell it that
ggplot(ORV) + 
  geom_line(aes(ERJ, d18O, group = factor(ASIL), colour = factor(ASIL))) +
  geom_point(aes(ERJ, d18O, group = factor(ASIL), colour = factor(ASIL), fill = factor(ASIL)))



######################################
# TASK 
#####################################
# We also want to change the shape of the points with the ASIL
# To do that we use 'shape ='
# Edit the code below so that the point shape varies with ASIL

ORV.teeth <- ggplot(ORV) + 
  geom_line(aes(ERJ, d18O, group = factor(ASIL), shape = factor(ASIL), colour = factor(ASIL))) +
  geom_point(aes(ERJ, d18O, group = factor(ASIL), shape = factor(ASIL), colour = factor(ASIL), fill = factor(ASIL)))


#####################################


#ORV.teeth <- ggplot(ORV) + 
 # geom_line(aes(ERJ, d18O, group = factor(ASIL), colour = factor(ASIL))) +
  #geom_point(aes(ERJ, d18O, group = factor(ASIL), colour = factor(ASIL), fill = factor(ASIL), shape = factor(ASIL)))



# R doesn't have enough shapes for us, so we have to manually tell it what shapes to use
# we do that with scale_shape_manual() and assign a vector of values for the shapes we want:
# http://www.sthda.com/sthda/RDoc/images/points-symbols.png

ORV.teeth <- ORV.teeth + scale_shape_manual(values = c(15, 8, 17, 18, 19, 3, 4, 8, 15, 16, 17, 18, 19, 3, 4, 8))

# # Add nice delta notation to y axis and x axis label
ORV.teeth <- ORV.teeth + labs( 
     y = expression(paste(delta^{18}, "O (\u2030)")), 
     x = "ERJ (mm)", 
     colour = "ASIL",   # we can also adjust the legend titles so they just say ASIL
     shape = "ASIL", 
     fill = "ASIL")

# Now we want to change the colours 
# we can assign them manually and make a vector of values like for shapes
# e.g. scale_colour_manual(values = c(....))
# but lets use a pre-made palettes from the Wes Anderson package


ORV.teeth + scale_colour_manual(values = c("blue", "green", "green", "green", "green", "green", "green", "green", "green") )

# install package
library(wesanderson) # load package
# Here are the palette names:
names(wes_palettes)
# create a new palette using the wes_palette() function:
# wes_palette(name, n, type = c("discrete", "continuous"))
wespal1 <- wes_palette("GrandBudapest1", 9, type = c("continuous"))

# scale the plot's colours using the values in the new palette
# and add a minimal white theme to see them better
ORV.teeth + scale_colour_manual(values = wespal1) + theme_minimal()


######################################
# TASK 
#####################################
# Copy, paste and edit the code from lines 347 and 351 to define your own 
# wed wndersen palette and apply it to the ORV.teeth plot

wespal1 <- wes_palette("GrandBudapest1", 9, type = c("continuous"))

# scale the plot's colours using the values in the new palette
# and add a minimal white theme to see them better
ORV.teeth + scale_colour_manual(values = wespal1) + theme_minimal()



####################################

# Saving
# Navigate to the top menu and click Plots > save as image / save as PDF
# Or use code - this is great when you know the finished size you want for a plot
# e.g. for a journal
# if you don't define a plot, ggsave autoselects uses the last plot 
ggsave("Output/OrvietoEnamel.tiff", units = c("mm"), dpi = 200, height = 100, width = 200) # setfile name / type, units, size


# If you want to save a particular plot include the name of the plot object:
ORV.teeth <- ORV.teeth + scale_colour_manual(values = wespal1) + theme_minimal()
ggsave("Output/OrvietoEnamel02.pdf", ORV.teeth, units = c("mm"), dpi = 200, height = 100, width = 200) # setfile name / type, units, size






