#CS7


pacman::p_load(tidyverse, stringr, readr, rio, purrr, stringi, Rcolorbrewer)

# Get the scripture and savior name data into R
# Download the data from http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip
# Read in the .csv file that was in the zip file and examine the structure of the data
# Use read_rds(gzcon(url("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds"))) to download and load the Savior names table into R
# Use the list of Savior names and the Book of Mormon verses to figure out the average number of words between references to the Savior
# Find each instance of a Savior name in the Book of Mormon
# Split on those instances and then count the number of words between each instance
# Use the example code below for some hints on how to tackle this task
# Report the average number of words between each Savior name
# Create an .Rmd file with 1-2 paragraphs summarizing your graphic that shows how the distance between Savior names is distributed across the Book of Mormon
# Compile your .md and .html file into your git repository
# Find two other student’s compiled files in their repository and provide feedback using the issues feature in GitHub (If they already have three issues find a different student to critique)
# Address 1-2 of the issues posted on your project and push the updates to GitHub



scriptures <- import("http://scriptures.nephi.org/downloads/lds-scriptures.csv.zip")

bom <- scriptures %>% filter(volume_title == "Book of Mormon")
onenep <- bom %>% filter(book_title == "1 Nephi")
twonep <- bom %>% filter(book_title == "2 Nephi")
jacob <- bom %>% filter(book_title == "Jacob")
omni <- bom %>% filter(book_title == "Omni")
jarom <- bom %>% filter(book_title == "Jarom")
woromo <- bom %>% filter(book_title == "Words of Mormon")
mosiah <- bom %>% filter(book_title == "Mosiah")
alma <- bom %>% filter(book_title == "Alma")
helaman <- bom %>% filter(book_title == "Helaman")
threenep <- bom %>% filter(book_title == "3 Nephi")
fournep <- bom %>% filter(book_title == "4 Nephi")
moroni <- bom %>% filter(book_title == "Moroni")


bmnames <- read_rds(gzcon(url("https://byuistats.github.io/M335/data/BoM_SaviorNames.rds")))

#In Class Ecample

allnames <- str_c(bmnames$name, collapse = "|")
str_count(nephi11$scripture_text, bmnames)



### 


#FOR LOOP
scr_text <- str_c(nephi11$scripture_text , collapse = "")
scr_i <- scr_text




for(i in seq_along(names)){
 
  ireplace <- str_c("Jesus", i, sep = "")
  
  #ireplace <-  str_replace(scr_i, names[i], "JESUS1")
  
  idat <- vector("list", length = length(names))
  print(ireplace)
}


for(i in seq_along(names)){
  verse <- str_split(verse, names[i]) %>% unlist()
}  
  
segs <- str_split_all(scr_i, sep = "JESUS1")
for(i in segs){
  stri_stats_latex()
}
  

  #i <- 22
  print(names[i])
  str_replace(scr_text, names[i], "JESUS1")
  
  
  
idat <- vector("list", length = length(names))













#LEts Do This - Practice With 1st Nephi

dat <- scriptures %>% 
  filter( book_short_title == "1 Ne.") %>% 
  mutate(count = str_count(scripture_text, allnames),
         key = cumsum(count)) %>% 
  select(book_short_title, chapter_number, verse_number,count, key) %>% 
  filter(!duplicated(key)) %>% 
  fill(., key)

allnames <- str_c(bmnames$name, collapse = "|")
verse <- vector("character", 9001)
nephi1 <- filter(scriptures, book_title == "1 Nephi" ) 
Nephi1 <- str_c(nephi1$scripture_text, collapse = " ")
nephi11 <- filter(scriptures, verse_title == "1 Nephi 1:1" )
nephi114 <- filter(scriptures, verse_title == "1 Nephi 1:14" )
names <- bmnames$name

for(i in seq_along(allnames)){
  verse <- str_split(Nephi1, allnames[i]) %>% 
    unlist() %>% as.tibble(.)
  
  
  
}  

verse <- verse %>% 
  mutate(key = row_number())
data <- right_join(dat, verse, by = "key")
data <- data %>% 
  rename(verse_start = verse_number) %>% 
  mutate(verse_end = lead(verse_start)) #%>% 
  mutate()

data$verse_start[1] = 1
dat2 <- data %>% 
  mutate(start = case_when(
    !is.na(verse_start) ~ verse_start,
    TRUE ~ lead(verse_start)
  )) %>% 
  mutate(start = case_when(
    !is.na(start) ~ start,
    TRUE ~ lead(start)
  )) %>% 
  mutate(start = case_when(
    !is.na(start) ~ start,
    TRUE ~ lead(start)
  )) %>% 
  mutate(start = case_when(
    !is.na(start) ~ start,
    TRUE ~ lead(start)
  )) %>%
  mutate(end = lead(start)) %>% 
  select(book_short_title, chapter_number, key, value, start, end)








data1 <- data %>% fill(.direction = "up")




data1 <- data1$book_short_title %>% fill(.direction = "down")


#LEts Do This - BOM FULL

dat <- scriptures %>% 
  filter( volume_id == 3) %>% 
  mutate(count = str_count(scripture_text, allnames),
         key = cumsum(count)) %>% 
  select(book_short_title, chapter_number, verse_number,count, key) %>% 
  filter(!duplicated(key)) %>% 
  fill(., key)

allnames <- str_c(bmnames$name, collapse = "|")
verse <- vector("character", 9001)
bom <- scriptures %>% filter(volume_title == "Book of Mormon")
BOM <- str_c(bom$scripture_text, collapse = " ")
names <- bmnames$name

for(i in seq_along(allnames)){
  verse <- str_split(BOM, allnames[i]) %>% 
    unlist() %>% as.tibble(.)
  
  
  
}  

verse <- verse %>% 
  mutate(key = row_number())
data <- right_join(dat, verse, by = "key")
data <- data %>% 
  rename(verse_start = verse_number)

data$verse_start[1] = 1
dat2 <- data %>% 
  mutate(start = case_when(
    !is.na(verse_start) ~ verse_start,
    TRUE ~ lead(verse_start)
  )) %>% 
  mutate(start = case_when(
    !is.na(start) ~ start,
    TRUE ~ lead(start)
  )) %>% 
  mutate(start = case_when(
    !is.na(start) ~ start,
    TRUE ~ lead(start)
  )) %>% 
  mutate(start = case_when(
    !is.na(start) ~ start,
    TRUE ~ lead(start)
  )) %>%
  mutate(start = case_when(
    !is.na(start) ~ start,
    TRUE ~ lead(start)
  )) %>%
  mutate(start = case_when(
    !is.na(start) ~ start,
    TRUE ~ lead(start)
  )) %>%
  mutate(start = case_when(
    !is.na(start) ~ start,
    TRUE ~ lead(start)
  )) %>%
  mutate(end = lead(start)) %>% 
  select(book_short_title, key, value, start, end)

dat2 <- dat2 %>% mutate(Book = case_when(
  key < 464 ~ "1 Nephi" ,
  key < 1059 ~ "2 Nephi" ,
  key < 1213 ~ "Jacob" ,
  key < 1235 ~ "Enos" ,
  key < 1243 ~ "Jarom"  ,
  key < 1266 ~ "Omni" ,
  key < 1279 ~ "Words of Mormon" ,
  key < 1739 ~ "Mosiah" ,
  key < 2759 ~ "Alma" ,
  key < 2968 ~ "Helaman",
  key < 3425 ~ "3 Nephi" ,
  key < 3466 ~ "4 Nephi" ,
  key < 3659 ~ "Mormon" ,
  key < 3887 ~ "Ether" ,
  TRUE ~ "Moroni",
)) %>% 
  select(Book, key, value, start, end)

dat2$end[4070] = 34
dat2$end[4071] = 34
dat2$start[4071] = 34




data1 <- data %>% fill(.direction = "up")




data1 <- data1$book_short_title %>% fill(.direction = "down")


#Vizualization 








