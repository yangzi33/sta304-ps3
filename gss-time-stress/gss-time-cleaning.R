#### Preamble ####
# Purpose: The purpose of this code is to clean-up the 2017 GSS data obtained 
# from the U of T library. That data is available to U of T students, but it needs 
# to be put into a tidy format before it can be analysed. This code does that.
# The main issue is that the data are released with codes for variables, whereas,
# we want the variable. e.g. sex is 1 or 2, but we want sex is female or male. (This
# sounds trite in that case, but gets more difficult with more involved variables.)
# So we create a dictionary type dataset that has the variable names and their 
# possible values. In that we embed some R code that will do a replacement. We 
# then apply that dataset to the raw dataset. Finally we do all the usual cleaning.
# to the dataset. You will end up with a dataset called gss.csv.
# Authors: Rohan Alexander and Sam Caetano
# Contact: rohan.alexander@utoronto.ca
# Date: 7 October 2020
# License: MIT
# Pre-reqs: You need to have downloaded the data from the library. To do that: 
  ## 1. Go to: http://www.chass.utoronto.ca/
  ## 2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
  ## 3. Click SDA @ CHASS, should redirect to sign in. Sign in.
  ## 4. Continue in English (you're welcome to use the French, but we probably can't
  ## help you too much).
  ## 5. Crtl F GSS, click
  ## 6. Click "Data" on the one you want. We used 2017, but you may want a different 
  ## wave. In particular the General Social Survey on social identity (cycle 27), 
  ## 2013 has some variables on voter participation if you're into that sort of 
  ## thing. You're welcome to pick any year but this code applies to 2017.
  ## 7. Click download
  ## 8. Select CSV data file, data definitions for STATA (gross, but stick with it for now).
  ## 9. Can select all variables by clicking button next to green colored "All". Then continue.
  ## 10. Create the files, download and save
# Check: 
  ## You WILL need to change the raw data name. Search for .csv - line 41
  ## You may need to adjust the filepaths depending on your system. Search for: read_


#### Workspace set-up ####
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
# raw_data <- read_csv("gss-time-stress/AAKQKMYl.csv")
# dict <- read_lines("gss-time-stress/dict.txt", skip = 18) # skip is because of preamble content
# # Now we need the labels because these are the actual responses that we need
# labels_raw <- read_file("gss-time-stress/labels.txt")


#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))
 
# Now we want a variable name and the possible values
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

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
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
# %>%
  # mutate_at(vars(agedc:fi_110), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  # mutate_at(.vars = vars(sex:fi_110),
            # .funs = funs(eval(parse(text = cw_statements %>%
                                      # filter(variable_name==deparse(substitute(.))) %>%
                                      # select(cw_statement) %>%
                                      # pull()))))

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

# #### Clean up ####
# gss <- gss %>% 
#   mutate_at(vars(age:future_children_intention), 
#             .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Not stated", "NA", .))) 

# gss <- gss %>% 
#   mutate(is_male = ifelse(sex=="Male", 1, 0)) 

# gss <- gss %>% 
#   mutate_at(vars(fin_supp_child_supp:fin_supp_other), .funs = funs(case_when(
#     .=="Yes"~1,
#     .=="No"~0,
#     .=="NA"~as.numeric(NA)
#   )))

# main_act <- raw_data %>% 
#   mutate(main_activity = case_when(
#     mpl_105a=="Yes"~ "Working at a paid job/business",
#     mpl_105b=="Yes" ~ "Looking for paid work",
#     mpl_105c=="Yes" ~ "Going to school",
#     mpl_105d=="Yes" ~ "Caring for children",
#     mpl_105e=="Yes" ~ "Household work", 
#     mpl_105i=="Yes" ~ "Other", 
#     TRUE~ "NA")) %>% 
#   select(main_activity) %>% 
#   pull()

# age_diff <- raw_data %>% 
#   select(marstat, aprcu0c, adfgrma0) %>% 
#   mutate_at(.vars = vars(aprcu0c:adfgrma0),
#             .funs = funs(eval(parse(text = cw_statements %>%
#                                       filter(variable_name==deparse(substitute(.))) %>%
#                                       select(cw_statement) %>%
#                                       pull())))) %>% 
#   mutate(age_diff = ifelse(marstat=="Living common-law", aprcu0c, adfgrma0)) %>% 
#   mutate_at(vars(age_diff), .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Not stated", "NA", .))) %>% 
#   select(age_diff) %>% 
#   pull()

# gss <- gss %>% mutate(main_activity = main_act, age_diff = age_diff)

# # Change some from strings into numbers
# gss <- gss %>% 
#   rowwise() %>% 
#   mutate(hh_size = str_remove(string = hh_size, pattern = "\\ .*")) %>% 
#   mutate(hh_size = case_when(
#     hh_size=="One" ~ 1,
#     hh_size=="Two" ~ 2,
#     hh_size=="Three" ~ 3,
#     hh_size=="Four" ~ 4,
#     hh_size=="Five" ~ 5,
#     hh_size=="Six" ~ 6
#   )) 

# gss <- gss %>% 
#   rowwise() %>% 
#   mutate(number_marriages = str_remove(string = number_marriages, pattern = "\\ .*")) %>% 
#   mutate(number_marriages = case_when(
#     number_marriages=="No" ~ 0,
#     number_marriages=="One" ~ 1,
#     number_marriages=="Two" ~ 2,
#     number_marriages=="Three" ~ 3,
#     number_marriages=="Four" ~ 4
#   )) 

# gss <- gss %>% 
#   rowwise() %>% 
#   mutate(number_total_children_known = ifelse(number_total_children_intention=="Don't know"|number_total_children_intention=="NA", 0, 1)) %>% 
#   mutate(number_total_children_intention = str_remove(string = number_total_children_intention, pattern = "\\ .*")) %>% 
#   mutate(number_total_children_intention = case_when(
#     number_total_children_intention=="None" ~ 0,
#     number_total_children_intention=="One" ~ 1,
#     number_total_children_intention=="Two" ~ 2,
#     number_total_children_intention=="Three" ~ 3,
#     number_total_children_intention=="Four" ~ 4,
#     number_total_children_intention=="Don't" ~ as.numeric(NA)
#   )) 

write_csv(gss, "gss-time-stress/gss-time-stress.csv")
