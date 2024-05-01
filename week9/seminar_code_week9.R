library(fpp3)

# US Monthly Electricity -----------------------------------------------------

usmelec <- as_tsibble(fpp2::usmelec) |>
  rename(Month = index, Generation = value)

usmelec |> gg_season(
  Generation
)

usmelec |> autoplot(
  Generation
  )

usmelec |> autoplot(
  log(Generation)
  )

usmelec |> autoplot(
  log(Generation) |> difference(12)
  )

usmelec |> gg_tsdisplay(
  log(Generation) |> difference(12), 
  plot_type = "partial"
  )
# You could possible work with this

usmelec |> autoplot(
  log(Generation) |> difference(12) |> difference()
  )

usmelec |> gg_tsdisplay(
  log(Generation) |> difference(12) |> difference(),
  plot_type = "partial"
  )

fit <- usmelec |>
  model(arima = ARIMA(log(Generation) ~ pdq(0,1,3) + PDQ(0,1,1)))

fit |> report()

fit |> gg_tsresiduals()

fit <- usmelec |>
  model(arima = ARIMA(log(Generation)))

fit |> report()

fit |> gg_tsresiduals()

augment(fit) |>
  features(.innov, ljung_box, lag = 24, dof = 5)
# residuals are not perfect but not too bad

# Don't run takes too long - see model below
usmelec |>
  model(arima = ARIMA(log(Generation), 
                      stepwise = FALSE, 
                      approximation = FALSE)
        ) |> report()

fit <- usmelec |>
  model(arima = ARIMA(log(Generation)~pdq(1,1,1)+PDQ(2,1,2)))
gg_tsresiduals(fit)

usmelec |>
  model(arima = ARIMA(log(Generation))) |>
  forecast(h = "3 years") |>
  autoplot(usmelec)

usmelec |>
  model(arima = ARIMA(log(Generation))) |>
  forecast(h = "3 years") |>
  autoplot(filter(usmelec, year(Month) >= 2005))
# Maybe peaks a little too low but not much we can do

# US Leisure and Hospitality -------------

leisure <- us_employment |>
  filter(Title == "Leisure and Hospitality", year(Month) > 2000) |>
  mutate(Employed = Employed/1000) |> select(Month, Employed)

autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

leisure |> gg_season(Employed)

leisure |>
  gg_tsdisplay(difference(Employed, 12),
               plot_type="partial", lag=36) +
  labs(title="Seasonally differenced", y="")
# Clearly non-stationary

leisure |>
  gg_tsdisplay(difference(Employed, 12) |> difference(),
               plot_type="partial", lag=36) +
  labs(title = "Double differenced", y="")

# Lets think about models
# Start with the seasonal component

# DO NOT RUN
# I have already run this 
fit_leisure <- leisure |>
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit_leisure |> 
  pivot_longer(everything(), names_to = "Model name", values_to = "Orders")

glance(fit_leisure) |> arrange(AICc) |> select(.model:BIC)
fit_leisure |> select(auto) |>  report()
fit_leisure |> select(auto) |> gg_tsresiduals()
fit_leisure |> select(auto) |> augment() |> 
  features(.innov, ljung_box, lag=24, dof=4)

forecast(fit_leisure, h=36) |>
  filter(.model=='auto') |>
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")
# Question: where does the trend come from?

# h02 drugs -------------------------

h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost))

h02 |> autoplot(Cost)

## Models using logs
h02 |> autoplot(log(Cost))
# You may choose not to take logs

# Clearly seasonal
h02 |> 
  gg_tsdisplay(difference(log(Cost),12), 
               lag_max = 36, 
               plot_type='partial')
# Debatable whether I should take another difference.
# Stick with d=0, D=1 for the moment

# My best guess
# Easier to look at the PACF for both seas and non-seas
fit <- h02 |>
  model(best = ARIMA(log(Cost) ~ pdq(3,0,0) + PDQ(2,1,0)))
report(fit)
# Note w/ drift has been selected

fit |> gg_tsresiduals(lag_max=36)
# not too bad

augment(fit) |>
  features(.innov, ljung_box, lag =24, dof = 6)
# Change lag to 24 - 36 is very long 
# Spikes are small and far away
# So overall happy with this

# Go to book and show plausible alternative models 
# You will need to do this for IA4

# Best of the alternative plausible models
fit <- h02 |>
  model(best = ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
fit |> report()
fit |> gg_tsresiduals(lag_max=36)
# better I think 
# no spike at lag 4

augment(fit) |>
  features(.innov, ljung_box, lag = 36, dof = 6)
# Still failing at lag 36
# How about lag 24

# Letting R choose
fit <- h02 |> 
  model(auto = ARIMA(log(Cost), stepwise = FALSE))
# No better than my 'best' model with stepwise=TRUE
# Try with turning this off
report(fit)
gg_tsresiduals(fit, lag_max=36)
# Resids look even better - still using 6 parameters 
augment(fit) |>
  features(.innov, ljung_box, lag = 36, dof = 6)
# Still failing at lag 36 - but getting closer
# How about lag 24 - clearly cannot reject WN

# Getting R to work really hard now 
# DO NOT RUN
fit_h02 <- h02 |>
  model(best = ARIMA(log(Cost), stepwise = FALSE,
                 approximation = FALSE,
                 order_constraint = p + q + P + Q <= 9 & (constant + d + D <= 2)
                 )
  )
# This will take a while now
# You should be experimenting with these in IA4 and GA4
# Start early because these will take a while
# Remember you may not be able to deal with all the autocorrelations
# Evaluate your significant spikes and where they are

# Remember every model is a crude approximation of reality
# No data comes from a model unless you are simulating data

fit_h02 |> report()
# Now a very complex model - I could not pick this
# Note: it cannot be compared to my model. Why?

gg_tsresiduals(fit_h02, lag_max=36)
# Looking better

augment(fit_h02) |>
  features(.innov, ljung_box, lag = 36, dof = 9)
# Success :-)

# However we have a problem? 
# d=1, D=1 - cannot compare this to my models

# How are we going to do that
# Test-set evaluation - see book

# The forecasts
fit_h02 |> forecast() |> autoplot(h02) +
  labs(y="H02 Expenditure ($AUD)")


# Example: Australian population -------------
# Non-seasonal
aus_economy <- global_economy |> filter(Code == "AUS") |>
  mutate(Population = Population/1e6)

aus_economy |> autoplot(Population)

# We want to consider ETS and ARIMA models
# Let's set up a 1-step cross-validation

aus_economy |>
  slice(-n()) |> # take out the last row
  stretch_tsibble(.init = 10) |> # stretch out starting with 10 obs
  # 48 folds - 48 .ids
  model(ets = ETS(Population),
        arima = ARIMA(Population)
  ) |>
  forecast(h = 1) |>
  accuracy(aus_economy) |>
  select(.model, ME:RMSSE)
# ETS seems better 

# Forecasts
aus_economy |>
  model(ETS(Population)) |>
  forecast(h = "5 years") |>
  autoplot(aus_economy) +
  labs(title = "Australian population",
       y = "People (millions)")

# Let's zoom in a little
aus_economy |>
  model(ETS(Population)) |>
  forecast(h = "5 years") |>
  autoplot(aus_economy |> filter(Year > 1990)) +
  labs(title = "Australian population",
       y = "People (millions)")


# Example: Cement production ------------
# Seasonal data

cement <- aus_production |>
  select(Cement) |>
  filter_index("1988 Q1" ~ .)

cement |> autoplot(Cement) 
# Seasonality changing 

# We are doing a train-test split 
# Best to do a cv stretch_tsibble
# You will do that in your assignment 
# but it will take longer - so be prepared - start early

train <- cement |> filter_index(. ~ "2007 Q4")

fit <- train |>
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement)
  )

fit |>
  select(arima) |>
  report()
gg_tsresiduals(fit |> select(arima), lag_max = 16)

fit |>
  select(arima) |>
  augment() |>
  features(.innov, ljung_box, lag = 16, dof = 6)


fit |>
  select(ets) |>
  report()
gg_tsresiduals(fit |> select(ets), lag_max = 16)

# Both models seem to be very suitable
fit |>
  forecast(h = "2 years 6 months") |>
  accuracy(cement) |>
  select(-ME, -MPE, -ACF1)
# ARIMA does slightly better 
# Remember only 10 obs 
# So do not get too excited about it
# But I still go with the ARIMA

fit |>
  select(arima) |>
  forecast(h="3 years") |>
  autoplot(cement) +
  labs(title = "Cement production in Australia- ARIMA",
       y = "Tonnes ('000)")

fit |>
  select(ets) |>
  forecast(h="3 years") |>
  autoplot(cement) +
  labs(title = "Cement production in Australia - ETS",
       y="Tonnes ('000)")

# Possibly ARIMA looks a bit too optimistic
# ETS wider intervals are possibly better

# If we are interested in intervals
fit |>
  forecast(h = "2 years 6 months") |>
  accuracy(cement, measures=interval_accuracy_measures, level=95)

fit |>
  forecast(h = "2 years 6 months") |>
  accuracy(cement, measures=interval_accuracy_measures, level=80)

# If we are interested in the whole distributions
fit |>
  forecast(h = "2 years 6 months") |>
  accuracy(cement, measures=distribution_accuracy_measures)
# ETS does better for the narrower intervals
