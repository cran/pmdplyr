## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pmdplyr)

## ---- eval=FALSE--------------------------------------------------------------
#  pibble(...,
#    .i = NULL,
#    .t = NULL,
#    .d = 1,
#    .uniqcheck = FALSE
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  as_pibble(x,
#    .i = NULL,
#    .t = NULL,
#    .d = 1,
#    .uniqcheck = FALSE,
#    ...
#  )

## -----------------------------------------------------------------------------
# .d = 1 by default, so in this data,
# a = 1, b = 3 comes one period after a = 1, b = 2.
basic_pibble <- pibble(
  a = c(1, 1, 1, 2, 2, 2),
  b = c(1, 2, 3, 2, 3, 3),
  c = 1:6,
  .i = a,
  .t = b
)

data(SPrail)
# In SPrail, insert_date does not imply regular gaps between
# time periods, so we set .d = 0
declared_pibble <- as_pibble(SPrail,
  .i = c(origin, destination),
  .t = insert_date,
  .d = 0
)

## ---- eval = FALSE------------------------------------------------------------
#  panel_convert(
#    data,
#    to,
#    ...
#  )

## ---- eval=FALSE--------------------------------------------------------------
#  id_variable(...,
#    .method = "number",
#    .minwidth = FALSE
#  )

## -----------------------------------------------------------------------------
df <- data.frame(
  country = c(
    "US", "US", "US", "US",
    "ENG", "ENG", "ENG", "ENG"
  ),
  city = c(
    "NYC", "NYC", "Cambridge", "NYC",
    "Cambridge", "London", "Manchester", "Manchester"
  )
) %>%
  mutate(
    numeric_ID = id_variable(country, city),
    random_ID = id_variable(country, city, .method = "random"),
    char_ID = id_variable(country, city, .method = "character")
  )

df

## ---- eval = FALSE------------------------------------------------------------
#  time_variable(...,
#    .method = "present",
#    .datepos = NA,
#    .start = 1,
#    .skip = NA,
#    .breaks = NA,
#    .turnover = NA,
#    .turnover_start = NA
#  )

## -----------------------------------------------------------------------------
data(SPrail)

# Since we have a date variable, we can easily create integers that increment for each
# year, or for each month, etc.
# Likely we'd only really need one of these four, depending on our purposes
SPrail <- SPrail %>%
  dplyr::mutate(
    year_time_id = time_variable(insert_date, .method = "year"),
    month_time_id = time_variable(insert_date, .method = "month"),
    week_time_id = time_variable(insert_date, .method = "week"),
    day_time_id = time_variable(insert_date, .method = "day")
  )

# Let's see what we've got
SPrail %>% 
  select(insert_date, ends_with("time_id")) %>% 
  head()

# Perhaps I'd like quarterly data
# (although in this case there are only two months, not much variation there)
SPrail <- SPrail %>%
  dplyr::mutate(quarter_time_id = time_variable(insert_date,
    .method = "month",
    .breaks = c(1, 4, 7, 10)
  ))
# Should line up properly with month
SPrail %>% 
  count(month_time_id, quarter_time_id)

# Maybe I'd like Monday to come immediately after Friday!
SPrail <- SPrail %>%
  dplyr::mutate(weekday_time_id = time_variable(insert_date,
    .method = "day",
    .skip = c(6, 7)
  ))

# Perhaps I'm interested in ANY time period in the data and just want to enumerate them in order
SPrail <- SPrail %>%
  dplyr::mutate(any_present_time_id = time_variable(insert_date,
    .method = "present"
  ))

# Note the weekday_time_id NAs - these are weekends! We told it to skip those.
head(SPrail %>% select(insert_date, day_time_id, weekday_time_id, any_present_time_id))

# Maybe instead of being given a nice time variable, I was given it in string form
SPrail <- SPrail %>% dplyr::mutate(time_string = as.character(insert_date))
# As long as the character positions are consistent we can still use it
SPrail <- SPrail %>%
  dplyr::mutate(day_from_string_id = time_variable(time_string,
    .method = "day",
    .datepos = c(3, 4, 6, 7, 9, 10)
  ))
# Results are identical from using the actual Date variable
cor(SPrail$day_time_id, SPrail$day_from_string_id)

