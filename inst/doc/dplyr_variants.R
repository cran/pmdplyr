## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning = FALSE,
  message = FALSE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(pmdplyr)

## ------------------------------------------------------------------------
left_df <- data.frame(
  i = c(1, 1, 1, 2, 2, 2),
  t = c(1, 2, 3, 1, 2, 3),
  v1 = 1:6
)
right_df <- data.frame(
  i = c(1, 1, 1, 2, 2, 2),
  t = c(0, 2, 4, 0, 2, 4),
  v2 = 7:12
)

# It automatically detects that i and t are the shared variables
# and finds two combinations of those in left_df that are also
# in right_df: i = 1, t = 2, and i = 2, t = 2. So it brings the
# v2 values it can match up in to the joined data.
# Other observations don't find a match
left_join(left_df, right_df)

## ---- eval = FALSE-------------------------------------------------------
#  inexact_left_join(x, y,
#    by = NULL,
#    copy = FALSE,
#    suffix = c(".x", ".y"),
#    ...,
#    var = NULL,
#    jvar = NULL,
#    method,
#    exact = TRUE
#  )

## ------------------------------------------------------------------------
right_df <- right_df %>%
  rename(t_right = t)

## ------------------------------------------------------------------------
inexact_left_join(left_df,
  right_df,
  var = t, jvar = t_right,
  method = "last"
)

## ------------------------------------------------------------------------
inexact_left_join(left_df,
  right_df,
  var = t, jvar = t_right,
  method = "next"
)

## ------------------------------------------------------------------------
inexact_left_join(left_df,
  right_df,
  var = t, jvar = t_right,
  method = "closest"
)

## ------------------------------------------------------------------------
right_df <- right_df %>%
  rename(t_bottom = t_right) %>%
  mutate(t_top = t_bottom + 2)

inexact_left_join(left_df,
  right_df,
  var = t, jvar = c(t_bottom, t_top),
  method = "between"
)

## ---- eval = FALSE-------------------------------------------------------
#  safe_join(x, y,
#    expect = NULL,
#    join = NULL,
#    ...
#  )

## ------------------------------------------------------------------------
# left is panel data and i does not uniquely identify observations
left <- data.frame(
  i = c(1, 1, 2, 2),
  t = c(1, 2, 1, 2),
  a = 1:4
)
# right is individual-level data uniquely identified by i
right <- data.frame(
  i = c(1, 2),
  b = 1:2
)

# I think that I can do a one-to-one merge on i
# Forgetting that left is identified by i and t together
# So, this produces an error
try(
  safe_join(left, right, expect = "1:1", join = left_join)
)

# If I realize I'm doing a many-to-one merge, that is correct,
# so safe_join will return TRUE if we don't specify a join
# or perform the join for us if we do
safe_join(left, right, expect = "m:1")
safe_join(left, right, expect = "m:1", join = left_join)

## ---- eval = FALSE-------------------------------------------------------
#  mutate_subset(.df,
#    ...,
#    .filter,
#    .group_i = TRUE,
#    .i = NULL,
#    .t = NULL,
#    .d = NA,
#    .uniqcheck = FALSE,
#    .setpanel = TRUE
#  )

## ------------------------------------------------------------------------
df <- pibble(
  state = c("CA", "CA", "CA", "NV", "NV", "NV"),
  college = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  earn = c(1, 2, 3, 2, 3, 2),
  .i = state
)

df %>%
  # Calculate average earnings of college grads
  mutate_subset(college_earnings = mean(earn), .filter = college == TRUE) %>%
  # And compare to our own earnings
  mutate(earnings_vs_college = earn - college_earnings)

## ---- eval = FALSE-------------------------------------------------------
#  mutate_cascade(.df,
#    ...,
#    .skip = TRUE,
#    .backwards = FALSE,
#    .group_i = TRUE,
#    .i = NULL,
#    .t = NULL,
#    .d = NA,
#    .uniqcheck = FALSE,
#    .setpanel = TRUE
#  )

## ------------------------------------------------------------------------
df <- pibble(
  t = c(1, 2, 3, 4, 5),
  payout = c(3, 4, 2, 2, 4),
  .t = t
) %>%
  mutate(PDV = payout) %>%
  mutate_cascade(PDV = payout + .95 * tlag(PDV, .n = -1), .backwards = TRUE)

df

## ---- eval = FALSE-------------------------------------------------------
#  tlag(.var,
#    .df = get(".", envir = parent.frame()),
#    .n = 1,
#    .default = NA,
#    .quick = FALSE,
#    .resolve = "error",
#    .group_i = TRUE,
#    .i = NULL,
#    .t = NULL,
#    .d = NA,
#    .uniqcheck = FALSE
#  )

## ------------------------------------------------------------------------
df <- pibble(
  i = c(1, 1, 1, 2, 2, 2),
  t = c(1, 2, 3, 1, 2, 3),
  x = 1:6,
  .i = i,
  .t = t
) %>%
  # A lag and a lead, filling in the lead with 0 instead of NA
  mutate(
    x_lag = tlag(x),
    x_lead = tlag(x, .n = -1, .default = 0),
    # Our data satisfies the .quick conditions so we can
    # do that for a little extra speed
    x_quicklag = tlag(x, .quick = TRUE)
  )

df

## ------------------------------------------------------------------------
df <- pibble(
  i = c(1, 1, 1, 2, 2, 2),
  t = c(1, 1, 2, 1, 1, 2),
  x = 1:6,
  .i = i,
  .t = t
) %>%
  mutate(x_lag = tlag(x, .resolve = mean))

df

