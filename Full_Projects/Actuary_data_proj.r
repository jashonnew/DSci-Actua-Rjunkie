pacman::p_load(tidyverse,sf,digest, USAboundaries,leaflet, scales)

#Possible text to use

#Part super-hero. Part fortune-teller. Part trusted advisor



#An actuary is a business professional who analyzes the financial consequences of risk. Actuaries use mathematics, statistics, and financial theory to study uncertain future events, especially those of concern to insurance and pension programs. Actuaries may work for insurance companies, consulting firms, government, employee benefits departments of large corporations, hospitals, banks and investment firms, or, more generally, in businesses that need to manage financial risk. A career as an Actuary is better described as a "business" career with a mathematical basis than as a "technical" mathematical career.


# Cleaning of City Data
changer <- read_csv("../../data/msatofips.csv") %>% 
  rename(countyfp = `FIPS County Code`, AREA = `CBSA Code`) %>% 
  select(countyfp, AREA)
  
cit <- read_csv("../../data/MSA_M2018_dl.csv") %>% filter(OCC_TITLE == "Actuaries" & TOT_EMP != "**") %>%
  select(-ANNUAL, -HOURLY)
4citdat <- left_join(cit, changer, key = AREA)
counties <- us_counties()
citloc <- left_join(counties,citdat, key = countyfp) %>% 
  select(AREA_NAME, OCC_TITLE, TOT_EMP, A_MEDIAN, A_PCT90, geometry, A_PCT25)
dat2 <- citloc[c(1,232, 430, 519, 595, 739, 1000,1316,1317,1318,1962,2162, 2241, 2365, 2496, 2632,3263,3369, 3528,3972,4342,4343,4344, 4624, 4625, 4740, 5213, 5632, 6023, 6239, 6595, 6792, 6837, 7014, 7921, 8116, 8661, 8832, 9135, 9424, 9897, 10177, 11506, 11507, 11661, 11749, 11867, 12028, 12107, 12221, 12301, 12419, 12583 ), ]  %>% 
  filter(!is.na(A_MEDIAN)) %>% 
  st_as_sf()

# Cleaning of State


act <- read_csv("../../data/state_M2018_dl.csv") %>% filter(OCC_TITLE == "Actuaries" & TOT_EMP != "**") %>%
  select(-ANNUAL, -HOURLY)
colmea <- read_csv("../../data/field_descriptions.csv")

states <- us_states() %>% 
  filter(stusps != "HI" & stusps != "AK"& stusps != "PR") %>% 
  rename(ST = stusps)
dat <- left_join(states,act, key = ST) %>% 
  mutate(emp = parse_number(TOT_EMP))


bins1 <- c(0, 100, 200, 500, 800, 1100, 1600, 2500)
pal1 <- colorBin("Reds", domain = dat$emp, bins = bins1)

bins2 <- c(0, 50000, 65000, 80000, 95000, 110000, 125000, 140000)
pal2 <- colorBin("YlGnBu", domain = dat$A_MEDIAN, bins = bins2)

bin3 <- c(0, 130000, 145000, 160000, 175000, 190000,Inf)
pal3 <- colorBin("RdPu", domain = dat$A_PCT90, bins = bin3)

epsg2163 <- leafletCRS(
  crsClass = "L.Proj.CRS",
  code = "EPSG:2163",
  proj4def = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs",
  resolutions = 2^(16:7))

labels <- sprintf(
  "<strong>%s</strong><br/>%g Actuaries Employed",
  dat$name, dat$emp
) %>% lapply(htmltools::HTML)

labels2 <- sprintf(
  "<strong>%s</strong><br/>%g Actuaries Employed",
  dat2$AREA_NAME, parse_number(dat$TOT_EMP)
) %>% lapply(htmltools::HTML)

leaflet(options = leafletOptions(crs = epsg2163)) %>% 
  addLayersControl(baseGroups = c("Jobs Available", "Median Salary", "Salary of 90th Percentile"),
                   options = layersControlOptions(collapsed = TRUE)) %>% 
  addPolygons(data = dat, fillColor = ~pal1(emp),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              group = "Jobs Available",
              highlight = highlightOptions(
                bringToFront = FALSE,
                dashArray = "",
                weight = 5
              ),
              label = labels) %>%
  addLegend(data = dat,pal = pal1, values = ~emp, title = "Actuary Jobs Available",group = "Jobs Available", layerId = 1) %>% 
  addPolygons(data = dat, fillColor = ~pal2(A_MEDIAN),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              group = "Median Salary",
              highlight = highlightOptions(
                bringToFront = FALSE,
                dashArray = "",
                weight = 5
              ),
              label = labels) %>% 
  addLegend(data = dat,pal = pal2, values = ~A_MEDIAN, title = "Actuary Median Salary",group = "Median Salary", layerId = 2) %>% 
  addPolygons(data = dat, fillColor = ~pal3(A_PCT90),
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                bringToFront = FALSE,
                dashArray = "",
                weight = 5
              ),
              label = labels) %>% 
  addLegend(data = dat,pal = pal3, values = ~A_PCT90, title = "90th Percentile Salary",group = "Salary of 90th Percentile", layerId = 3) #%>% 
  addPolygons(data = dat2,
              weight = 2,
              opacity = 1,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                bringToFront = TRUE,
                weight = 5
              ), label = labels2)




