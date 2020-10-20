
library(janitor)
library(tidyverse)

raw_data <- read_csv("time-stress-cleaning/time-stress-raw.csv")
dict <- read_lines("time-stress-cleaning/dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("time-stress-cleaning/labels.txt")

variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))

labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

labels_raw_tibble
labels_raw_tibble <- labels_raw_tibble %>% mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))



add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}


cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)

cw_statements

cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))

cw_statements

gss <- raw_data %>% 
  select(CASEID, 
         wght_per,
         agegr5,
         agegr10,
         sex,
         marstat,
         chrinhsdc,
         gtu_q110,
         epi4500,
         epi4700,
         epi8660,
         epi8671,
         srh_q110,
         srh_q115,
         incm,
         incmhsd)

gss <- gss %>% mutate_at(vars(wght_per:incmhsd), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))



# Fix the names
gss <- gss %>% 
  clean_names() %>% 
  rename(age_5 = agegr5,
         age_10 = agegr10,
         # wght_per = person_weight,
         person_weight = wght_per,
         gender = sex,
         num_child = chrinhsdc,
         marital = marstat,
         how_often_rushed = gtu_q110,
         occ_essential_sleep = epi4500,
         occ_relx_thik_smke = epi4700,
         occ_chat_group = epi8660,
         occ_social_net = epi8671,
         health = srh_q110,
         mental_health = srh_q115,
         annual_income = incm,
         household_income = incmhsd,)