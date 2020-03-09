## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(pmdplyr)

## ---- eval = FALSE------------------------------------------------------------
#  panel_fill(.df,
#    .set_NA = FALSE,
#    .min = NA,
#    .max = NA,
#    .backwards = FALSE,
#    .group_i = TRUE,
#    .flag = NA,
#    .i = NULL,
#    .t = NULL,
#    .d = 1,
#    .uniqcheck = FALSE,
#    .setpanel = TRUE
#  )

## -----------------------------------------------------------------------------
# Note the gap between periods 2 and 4 for person 1.
df <- pibble(
  i = c(1, 1, 1, 2, 2, 2),
  t = c(2, 4, 5, 1, 2, 3),
  x = 1:6,
  y = 7:12,
  .i = i,
  .t = t
)

panel_fill(df, .set_NA = "y", .flag = "new_obs")
panel_fill(df, .set_NA = "y", .backwards = TRUE)$x

## -----------------------------------------------------------------------------
panel_fill(df, .min = min(df$t), .max = max(df$t))

## ---- eval = FALSE------------------------------------------------------------
#  panel_locf(.var,
#    .df = get(".", envir = parent.frame()),
#    .fill = NA,
#    .backwards = FALSE,
#    .resolve = "error",
#    .group_i = TRUE,
#    .i = NULL,
#    .t = NULL,
#    .d = 1,
#    .uniqcheck = FALSE
#  )

## -----------------------------------------------------------------------------
df <- pibble(
  i = c(1, 1, 1, 2, 2, 2),
  t = c(1, 2, 3, 2, 3, 4),
  x = c(1, NA, 3, NA, -3, 4),
  .i = i,
  .t = t
)

# Notice that the fourth observation doesn't get filled in
# because it's the first observation for person 2, so nothing to fill in from
df %>%
  mutate(x_filled = panel_locf(x))

## -----------------------------------------------------------------------------
df %>% mutate(
  x_filled = panel_locf(x, .backwards = TRUE),
  x_no_neg3 = panel_locf(x, .backwards = TRUE, .fill = c(NA, -3))
)

## -----------------------------------------------------------------------------
inconsistent_df <- pibble(
  i = c(1, 1, 1, 2, 2, 2),
  t = c(1, 1, 2, 1, 2, 3),
  x = c(1, 2, NA, 1, 2, 3),
  .i = i,
  .t = t
)

inconsistent_df %>% mutate(
  x_filled =
    panel_locf(x, .resolve = mean)
)

## -----------------------------------------------------------------------------
df <- data.frame(
  continent = c("Asia", "Europe", "Europe", "S America", "S America"),
  country = c("France", "France", "France", "Brazil", "Brazil"),
  year = c(2000, 2001, 2002, 2000, 2001)
)

df

## ---- eval = FALSE------------------------------------------------------------
#  fixed_check(.df,
#    .var = NULL,
#    .within = NULL
#  )

## -----------------------------------------------------------------------------
fixed_check(df, .var = continent, .within = country)$continent

## -----------------------------------------------------------------------------
consistent_df <- data.frame(
  state = c(1, 1, 1, 2, 2, 2),
  year = c(2000, 2001, 2001, 2000, 2000, 2001),
  treatment = c(F, T, T, T, T, F),
  outcome = c(4.4, 3.2, 3.4, 5.5, 5.6, 8)
)

# Since this policy treatment is administered on the state level,
# everyone in the same state/year should get the same treatment.
# And they do!
fixed_check(consistent_df, .var = treatment, .within = c(state, year))

## ---- eval = FALSE------------------------------------------------------------
#  fixed_force(..df,
#    .var = NULL,
#    .within = NULL,
#    .resolve = mode_order,
#    .flag = NA
#  )

## -----------------------------------------------------------------------------
fixed_force(df, .var = continent, .within = country, .flag = "altered")

## -----------------------------------------------------------------------------
fixed_force(df, .var = continent, .within = country, .resolve = "drop")

## ---- eval = FALSE------------------------------------------------------------
#  between_i(.var,
#    .df = get(".", envir = parent.frame()),
#    .fcn = function(x) mean(x, na.rm = TRUE),
#    .i = NULL,
#    .t = NULL,
#    uniqcheck = FALSE
#  )

## -----------------------------------------------------------------------------
df <- pibble(
  i = c(1, 1, 2, 2),
  x = 1:4,
  .i = i
) %>%
  mutate(between_x = between_i(x))

# Notice that the grand mean is...
mean(df$x)
# And the mean within groups is...
df %>%
  group_by(i) %>%
  summarize(x = mean(x))

# So the between calculation should be
# 1.5 - 2.5 = -1 and 3.5 - 2.5 = 1 for the different groups:
df$between_x

## ---- eval = FALSE------------------------------------------------------------
#  within_i(.var,
#    .df = get(".", envir = parent.frame()),
#    .fcn = function(x) mean(x, na.rm = TRUE),
#    .i = NULL,
#    .t = NULL,
#    uniqcheck = FALSE
#  )

## -----------------------------------------------------------------------------
df <- pibble(
  i = c(1, 1, 2, 2),
  x = 1:4,
  .i = i
) %>%
  mutate(within_x = within_i(x))

# Notice that the mean within groups is...
df %>%
  group_by(i) %>%
  summarize(x = mean(x))

# So the between calculation should be
# 1 - 1.5 = -.5 and 2 - 1.5 = .5 for individual 1
# and 3 - 3.5 = -.5 and 4 - 3.5 = .5 individual 2:
df$within_x

## -----------------------------------------------------------------------------
# 2 appears twice while everything else appears once; 2 is the mode.
x <- c(1, 2, 2, NA, 5, 3, 4)
mode_order(x)

# 1 or 2 could be the mode.
# Ties are broken by order in the vector.
x <- c(2, 2, 1, 1)
mode_order(x)

