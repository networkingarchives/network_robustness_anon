# Robustness Data

library(dplyr, warn.conflicts = FALSE)
library(tidyverse)
library(tidygraph)
library(igraph)
library(data.table)
library(ggraph)
library(snakecase)
library(lubridate)
library(furrr)

# Make SPO network from edge list, create include year and folio columns:

spo_raw = read_delim('fromto_all_place_mapped_stuart_sorted', delim = '\t', col_names = F )
spo_network = spo_raw %>% 
  dplyr::select(X1, X2, X3, X5, X8) %>% 
  mutate(X8 = str_remove(X8, "\\sf\\.[0-9]{1,}")) %>% 
  mutate(year_date = year(ymd(X3))) %>% 
  select(X1, X2, year_date, letter_id = X5, X8) %>% 
  mutate(folio_or_catalogue = str_remove(X8, "f\\.[0-9]{1}")) %>% 
  mutate(folio_or_catalogue = trimws(folio_or_catalogue, which = 'both')) %>% select(-X8) %>% data.table()

# Make EMLO network. First load full data to extract catalogue info:

work <- read_csv("work.csv", col_types = cols(.default = "c"))
colnames(work) = to_snake_case(colnames(work))

# Load edge list

emlo_raw = read_delim('emlo_full_network.dat', delim = '\t', col_names = F)

# Load a list of letters to remove (duplicates and some unknowns)

to_remove = read_csv('to_remove_list_with_unknown.csv')

# Make network from edge list with catalogue and year information

emlo_network = emlo_raw %>% 
  filter(!X5 %in% to_remove$value) %>% 
  left_join(work %>%
              mutate(emlo_letter_id_number = as.numeric(emlo_letter_id_number)) %>%
              select(emlo_letter_id_number, original_catalogue_name), 
            by = c('X5' = 'emlo_letter_id_number')) %>%
  filter(original_catalogue_name != 'Bodleian card catalogue') %>% 
  mutate(year_date = year(ymd(X3))) %>% 
  select(X1, X2, year_date, letter_id = X5, folio_or_catalogue = original_catalogue_name )

# Load shelfmark info to join to BCC data:

shelfmarks = read_delim('shelfmarks.txt', delim = '\t')

colnames(shelfmarks) = to_snake_case(colnames(shelfmarks))
folio_section = shelfmarks %>% filter(str_detect(shelfmark_and_pagination, "fol"))

bcc_with_f = folio_section %>% 
  separate(shelfmark_and_pagination, into = c('ms', 'folio'), sep = "fol|fols") %>% 
  mutate(ms_name = str_remove(ms, "[0-9]{1,}")) %>% 
  mutate(ms_no = str_extract(ms, "[0-9]{1,}")) %>% 
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Rawl. letters"), "MS Rawl. letters", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Ashmole"), "MS Ashmole", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Cherry"), "MS Cherry", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Eng. letters"), "MS Eng. letters", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Wood F."), "MS Wood F.", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Ashmole"), "MS Ashmole", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Eng. Mis"), "MS Eng. mis", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Eng. mis"), "MS Eng. mis", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Smith"), "MS Smith", ms_name)) %>%
  mutate(ms_name = ifelse(str_detect(ms_name,"MS Eng. hist"), "MS Eng. hist", ms_name)) %>%
  mutate(ms_name = str_remove_all(ms_name,"z")) %>%
  mutate(ms_name = str_remove_all(ms_name,", ")) %>%
  mutate(ms_name = str_remove_all(ms_name, "a\\.|b\\.|c\\.|d\\.")) %>% 
  select(work_letter_id, ms_name, ms_no) %>% 
  mutate(folio_or_catalogue = paste0(ms_name, "_", ms_no)) %>% select(work_letter_id, folio_or_catalogue)

# Join this to the BCC portion of EMLO:

bcc_network = emlo_raw %>% 
  filter(!X5 %in% to_remove$value) %>% 
  left_join(work %>%
              mutate(emlo_letter_id_number = as.numeric(emlo_letter_id_number)) %>%
              select(emlo_letter_id_number, original_catalogue_name), 
            by = c('X5' = 'emlo_letter_id_number')) %>%
  filter(original_catalogue_name == 'Bodleian card catalogue') %>% 
  mutate(year_date = year(ymd(X3))) %>% 
  select(X1, X2, year_date, letter_id = X5) %>% left_join(bcc_with_f, by = c('letter_id' = 'work_letter_id'))  