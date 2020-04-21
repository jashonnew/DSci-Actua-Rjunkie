#[ ] Use the correct functions from library(haven) , library(readr), and library(readxl) to load the 6 data sets listed here
#[ ] Tidy the Worldwide estimates .xlsx file
#[ ] Make sure the file is in long format with year as a column. See here for an example of the final format.
#[ ] Use the separate() and mutate() functions to create a decade column.
#[ ] Import the other five datasets into R and combine them into one tidy dataset.
#[ ] This dataset should have the following columns - birth_year, height.cm, height.in, and study_id
#[ ] The BLS wage data does not have birth information. Let’s assume it is mid-twentieth century and use 1950.
#[ ] See the reading of Task 8 for how to read in dbf files.
#[ ] Save the two tidy datasets to your repository - The world country estimates and the row-combined individual measurements.
#[ ] Make a plot with decade on the x-axis and height in inches on the y-axis with the points from Germany highlighted based on the data from the .xlsx file.
#[ ] Make a small-multiples plot of the five studies to examine the question of height distribution across centuries
#[ ] Create an .Rmd file with 1-2 paragraphs summarizing your graphics and how those graphics answer the driving question
#[ ] Compile your .md and .html file into your git repository
#[ ] Find two other student’s compiled files in their repository and provide feedback using the issues feature in GitHub (If they already have three issues find a different student to critique)
#[ ] Address 1-2 of the issues posted on your project and push the updates to GitHub

library(readxl)

bob0 <- tempfile()
download("https://byuistats.github.io/M335/data/heights/Height.xlsx", bob0, mode = "wb")
dat <- read_xlsx(bob0, skip = 2) %>% 
  gather(`1800`:`2011`, key = "Year_Decade", value = "Height_cm" ) %>% 
  filter(!is.na(Height_cm)) %>% 
  rename(Country = `Continent, Region, Country`) %>% 
  separate(Year_Decade, c("Century-1","Decade","Year"), sep = c(2,3), remove = FALSE) %>% 
  mutate(Height_in = conv_unit(Height_cm, from = "cm", to = "inch"))
  
  
  
  
  
View(dat)

BLSHT <- read_csv("https://github.com/hadley/r4ds/raw/master/data/heights.csv") %>% 
  select(height) %>% 
  rename(Height_in = height) %>% 
  mutate(Height_cm = conv_unit(Height_in, from = "inch", to = "cm")) %>% 
  mutate(birth_year = 1950) %>%
  mutate(study_id = "BLS Wage")



nsHT <- read_sav("http://www.ssc.wisc.edu/nsfh/wave3/NSFH3%20Apr%202005%20release/main05022005.sav") %>% 
  select(DOBY,RT216I, RT216F) %>% 
  filter(RT216F >= 4) %>% 
  filter(RT216I < 12 & RT216I > -1) %>% 
  mutate(RT216F = RT216F * 12) %>% 
  mutate(Height_in = RT216F + RT216I)
nsH <- nsHT %>% 
  select(DOBY, Height_in) %>% 
  mutate(Height_cm = conv_unit(Height_in, from = "inch", to = "cm")) %>% 
  mutate(study_id = "National Heights") %>% 
  mutate(birth_year = DOBY + 1900) %>% 
  select(birth_year, Height_in, Height_cm, study_id)



germcon <- read_dta("https://byuistats.github.io/M335/data/heights/germanconscr.dta") %>% 
  select(bdec, height) %>% 
  rename(birth_year = bdec) %>% 
  rename(Height_cm = height) %>% 
  mutate(Height_in = conv_unit(Height_cm, from = "cm", to = "inch")) %>% 
  mutate(study_id = "Bavarian Conscripts II")
  




germpri <- read_dta("https://byuistats.github.io/M335/data/heights/germanprison.dta") %>% 
  select(bdec, height) %>% 
  rename(Height_cm = height) %>% 
  rename(birth_year = bdec) %>% 
  mutate(Height_in = conv_unit(Height_cm, from = "cm", to = "inch")) %>% 
  mutate(study_id = "Bavarian Conscripts I")


seasol <- read.dbf("../../data/B6090.DBF") %>% 
  select(RECNO, CMETER,GEBJ) %>% 
  rename(Height_cm = CMETER) %>% 
  rename(birth_year = GEBJ) %>% 
  rename(study_id = RECNO) %>%
  mutate(Height_in = conv_unit(Height_cm, from = "cm", to = "inch")) %>% 
  mutate(study_id = "German Soldiers")

Heights <- bind_rows(seasol, germpri, germcon, nsH, BLSHT)
  


saveRDS(Heights, "Combined_Heights.rds")
saveRDS(dat, "World_Height")

dat1 <- filter(dat, Country == c("Germany", "Federal Republic of Germany (until 1990)")) %>% 
  mutate(Century = case_when(
  `Century-1` == 18 ~ "1800's",
  TRUE ~ "1900's"
))

dat %>% mutate(Century = case_when(
  `Century-1` == 18 ~ "1800's",
  TRUE ~ "1900's"
)) %>% 
  ggplot() +
  geom_violin(aes(Decade, Height_in), fill = "goldenrod1") +
  geom_boxplot(aes(Decade, Height_in), fill = "darkorchid1", width =.5) +
  geom_point(data = dat1, aes(Decade, Height_in), color = "blue", size = 3, shape = 15) +
  facet_wrap(~`Century`, scale = "free_x", nrow = 1) +
  theme_dark() +
  scale_y_continuous(breaks = seq(60, 75, by = 5)) +
  labs(y = "Height in inches")



Heights$study_id <- 
  factor(Heights$study_id, levels = c("German Soldiers", "Bavarian Conscripts I", "Bavarian Conscripts II", "National Heights", "BLS Wage")) 
  
Heights %>% 
  rename(`Study ID` = study_id) %>% 
  rename(`Birth Year` = birth_year) %>% 
  ggplot() +
  geom_jitter(aes(`Birth Year`, Height_in, color = `Birth Year`)) +
  geom_boxplot(aes(`Birth Year`, Height_in, fill = `Study ID`), color = "white") +
  facet_wrap(~`Study ID`, scale = "free_x", nrow = 1) +
  theme_dark() +
  labs(y = "Height (in Inches)", title = "Though Some Change in Median Occurs, Height Distributions Skew Right Over Time", subtitle = "German Heights in inches")
