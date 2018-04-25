 # read in excel files from Ul Balis
# approach with purrr, based on
# serialmentor.com/blog/2016/6/13/
# reading-and-combining-many-tidy-data-files-in-R

library(tidyverse) #includes purrr
library(readxl)
library(janitor)
library(lubridate)
library(ggalt)
library(forcats)

#look in project folder (dir)
files <- dir(pattern = "*.xlsx") #store vector of excel files 

data <- files %>% # take the files, then
  map_dfr(read_excel) %>% # collect files into one data file
  clean_names() %>%  #clean up the names
  select(-c(mrn, csn, ordnum)) %>% # remove 3 columns
  arrange(desc(coll_dt)) #sort by collection date

# what count of each test is used?
# by year

table(data$ord_test)

data %>% 
  mutate(year = lubridate::year(coll_dt)) %>% 
  group_by(ord_test, year) %>% 
  summarize(count= n()) %>% 
  ggplot(aes(x=year, y=count, col=ord_test)) +
  geom_point()

ggsave("counts_byyear.png", width = 9, height = 6, units = "in")

#select doctor_id and last_name for later joining
data %>% 
  select(c(doctor_id, last_name)) %>% 
  unique() ->
names

# which docs are using mostly one test or the other?
data %>% 
  group_by(doctor_id, ord_test) %>% 
  summarize(count= n()) %>% 
  spread(key = ord_test, value = count) %>% 
  mutate_if(is.integer, funs(replace(., is.na(.), 0))) %>% 
  rename(metab = `6MMP`, thiomon= TMON) %>% 
  mutate(total = metab + thiomon) %>% 
  mutate(cost = metab *210) %>% 
  left_join(names) %>% 
  filter(total > 20) ->
counts

counts %>% 
  ggplot(aes(x=thiomon, xend = metab, 
             y=fct_reorder(last_name, total))) +
  geom_dumbbell(size=3, color="#e3e2e1", colour_x ="blue",
                colour_xend = "red") +
  geom_text(aes(x = 380, y= last_name, label = paste(cost))) +
  ggtitle("Counts of Thiomon (blue) vs. Metabolites (red)",
          subtitle = "Dollars spent on Metabolites at Right") +
  xlab("Count of Tests Ordered") +
  ylab("") +
  theme_bw() 

ggsave("counts_bydoc.png", width = 9, height = 6, units = "in")

