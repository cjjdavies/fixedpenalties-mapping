## 10 Apr 2021 - Analysis of fixed penalty notices issued in Scotland 2018-19
## Dataset obtained from https://www.scotcourts.gov.uk/docs/default-source/aboutscs/reports-and-data/quarterly-fines-reports/qfr47/quarterly-fines-report-47---2020-21-q2.pdf?sfvrsn=2
## Page 12 has a link to data tables from which the spreadsheet can be downloaded
## I did a bit of formatting to get the data into an R-friendly format, and saved as a .csv file

## Check working directory and set if necessary
getwd()
setwd("/home/chantel/Dropbox/gri ltd/Career/police_fines/")

## Read necessary libraries in the following order
library(sf)
library(raster)
library(dplyr)
library(spData)

## Had to be installed from the GitHub repo this way
install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(spDataLarge)
library(tmap)
library(leaflet)
library(ggplot2)

## Read main data set
fines <- read.csv("/home/chantel/Dropbox/gri ltd/Career/police_fines/Police Fines data - interview analysis.csv", header = T, sep=",")

## Examine data
head(fines)
tail(fines)
names(fines)
dim(fines)
str(fines)

## I created a new vector to categorise the percent data, but ended up using a different variable, but the code is here for posterity
#summary(fines$cr_ValFinesptd_percent)
#breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1)
#tags <- c("[0-10%)", "[10-20%)", "[20-30%)", "[30-40%)", "[40-50%)", "[50-60%)", "[60-70%)", "[70-80%)", "[80-90%)", "[90-100%)", "[100%)")
#group_tags <- cut(fines$cr_ValFinesptd_percent, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = tags)
#summary(group_tags)
#v <- fines %>% select(cr_ValFinesptd_percent)
#vgroup <- as_tibble(v) %>%
#  mutate(tag = case_when(
#    cr_ValFinesptd_percent < 0.1 ~ tags[1],
#    cr_ValFinesptd_percent >= 0.1 & cr_ValFinesptd_percent < 0.2 ~ tags[2],
#    cr_ValFinesptd_percent >= 0.2 & cr_ValFinesptd_percent < 0.3 ~ tags[3],
#    cr_ValFinesptd_percent >= 0.3 & cr_ValFinesptd_percent < 0.4 ~ tags[4],
#    cr_ValFinesptd_percent >= 0.4 & cr_ValFinesptd_percent < 0.5 ~ tags[5],
#    cr_ValFinesptd_percent >= 0.5 & cr_ValFinesptd_percent < 0.6 ~ tags[6],
#    cr_ValFinesptd_percent >= 0.6 & cr_ValFinesptd_percent < 0.7 ~ tags[7],
#    cr_ValFinesptd_percent >= 0.7 & cr_ValFinesptd_percent < 0.8 ~ tags[8],
#    cr_ValFinesptd_percent >= 0.8 & cr_ValFinesptd_percent < 0.9 ~ tags[9],
#    cr_ValFinesptd_percent >= 0.9 & cr_ValFinesptd_percent < 1 ~ tags[10],
#    cr_ValFinesptd_percent >= 1 & cr_ValFinesptd_percent < 1.1 ~ tags[11]
#  ))
#summary(vgroup)
#vgroup$tag <- factor(vgroup$tag,
#                     levels = tags)
#vgroup$tag

## Convert columns from character/ numeric to factors
fines$Year <- factor(fines$Year)
fines$Sheriffdom <- factor(fines$Sheriffdom)
fines$Court <- factor(fines$Court)
fines$Postcode <- factor(fines$Postcode)

## Convert the dataframe to an sf object, using the coordinates to create the reference points
fines_sf <- st_as_sf(fines, coords = c("Longitude", "Latitude"), crs= "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
str(fines_sf)
st_crs(fines_sf)
st_write(fines_sf, "/home/chantel/Dropbox/gri ltd/Career/police_fines", driver = "ESRI Shapefile", delete_layer = TRUE)

## Create a map of Scotland, by first producing a UK map, then subsetting to the territory of Scotland
GBR <- getData('GADM', country="GBR", level=1)
scotland <- GBR[GBR$NAME_1 == "Scotland",]

## Alternatively, an existing shapefile of Scotland can be used from another dataset
scotland_sf <- st_read("/home/chantel/Dropbox/gri ltd/Career/police_fines/District_21_1")

## To save a map as a shapefile, use writeOGR from the RGDAL package
writeOGR(scotland, "/home/chantel/Dropbox/gri ltd/Career/police_fines", layer = "map", driver = "ESRI Shapefile")

## Create a plot of points corresponding to value of fines imposed, using Sheriffdom as colour variable
fines_sf %>%
  ggplot() +
  geom_sf(aes(size = fp_ValImp, color = Sheriffdom)) +
  xlab("Longitude") +
  ylab("Latitude")

## Subset years - multiple for facet plot
years3<-fines_sf %>%
  filter(Year %in% c("y17_18", "y18_19", "y19_20"))

## Or single year for one plot
years<-fines_sf %>%
  filter(Year %in% c("y18_19"))

## Create a base map with minimal formatting - this can be overridden with the main map as required
base_map <- ggplot() +
  geom_sf(data = scotland_sf, color = NA, fill = "lightgray") +
  theme(panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

## View base map
base_map

## Set final parameters of map using the geom_sf function for managing shapefiles and formatting
## Datapoints will reflect the size associated with fp_NumImp (fixed penalty number of imposed fines) and colour-coded by categorical variable, Sheriffdom
## The parameter scale_size_continuous allows for the changing of the legend from fp_NumImp to 'Number Imposed'; range is the size range for the points;
## Breaks are the bins, decided arbitrarily based on summary data and instinct, but I have also calculated bins based on Sturge's Rule
## The parameter scale_color_manual 'values' is used to change the colours for Sheriffdom
## I changed the theme slightly to theme_bw() - I could go back and alter the base map, but I like its simplicity and would prefer to keep it for future use
## Theme elements have been used to change font sizes for easier viewing once exported and used in a Word file - play around and see what they do
## Guides parameter is used to increase the size of the legend key based on the color specification
final_map <- base_map +
  geom_sf(data = years, aes(size = fp_NumImp, color = Sheriffdom)) +
  labs(title = "Police Fixed Penalty Notices \n2018-2019", caption = "Source: SCTS Official Statistics") +
  scale_size_continuous(name = "Number Imposed",
                        range = c(2, 9),
                        breaks = c(0, 100, 250, 500, 750, 1500, 2000, 2500)) +
                        #breaks = c(0, 392, 784, 1176, 1568, 1960, 2352)) + Sturge's Rule to calculate more accurate bins
                        #https://www.statisticshowto.com/choose-bin-sizes-statistics/
  scale_color_manual(values = c("#d73027", "#fc8d59", "#8c510a", "#542788", "#91bfdb", "#4575b4")) +
  theme_bw() +
  theme(title = element_text(size = 20),
        plot.caption = element_text(size = 11, face = "italic"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.key.size = unit(0.75, "cm")) +
  guides(color = guide_legend(override.aes = list(size = 6)))

## View final map
final_map