#========================================================
# @Project: Just some dplyr functionalities
# @Name: dplyr_tips
# @author: Many
# @date: 2017/03
#========================================================


library(dplyr)
str(mtcars)

## --- Filtering, grouping and summarizing
mtcars %>%
  filter(gear > 1 ) %>%
    group_by(cyl, carb) %>%
        summarise(
                wt = sum(wt),
                qsec = sum(qsec)) 


## --- Remove one column
mtcars %>% 
  select(-disp) %>% 
  head()


## --- re-ordering columns
mtcars %>% 
  select(cyl, disp, hp, everything()) %>% 
  head()


## --- renaming columns with rename()
mtcars <- rename(mtcars, spam_mpg = mpg)
mtcars <- rename(mtcars, spam_disp = disp)
mtcars <- rename(mtcars, spam_hp = hp)


## --- selecting columns with a regexp
mtcars %>% 
  select(contains("spam")) %>% 
  head()


## --- create new columns with mutate() and ifelse()
mtcars %>% 
  mutate(vs_new = if_else(
    vs == 1, 
    "one", 
    "zero", 
    NA_character_)) %>% 
  head()


## --- create a new variable conditionally on several values of another column

mtcars %>% 
  mutate(carb_new = case_when(.$carb == 1 ~ "one",
                              .$carb == 2 ~ "two",
                              .$carb == 4 ~ "four",
                              TRUE ~ "other")) %>% 
  head(15)

## --- apply a function to certain columns only, by rows
mtcars %>%
  select(am, gear, carb) %>%
  purrr::by_row(sum, .collate = "cols", .to = "sum_am_gear_carb") -> mtcars2
head(mtcars2)


## --- use do() to do any arbitrary operation
mtcars %>% 
  group_by(cyl) %>% 
  do(models = lm(spam_mpg ~ drat + wt, data = .)) %>% 
  broom::tidy(models)