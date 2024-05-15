library(fpp3)
library(lubridate)

## US GASOLINE ---------------------------------------------------
# Weekly data
us_gasoline |> autoplot(Barrels)

# The ugly way
# Assuming 52 weeks in the year
# What can K go up to?

# ARIMA deals with change in trends
gas_fit <- us_gasoline |>
  model(
    F1 = ARIMA(Barrels ~ fourier(K = 1) + PDQ(0,0,0)),
    F2 = ARIMA(Barrels ~ fourier(K = 2) + PDQ(0,0,0)),
    F3 = ARIMA(Barrels ~ fourier(K = 3) + PDQ(0,0,0)),
    F4 = ARIMA(Barrels ~ fourier(K = 4) + PDQ(0,0,0)),
    F5 = ARIMA(Barrels ~ fourier(K = 5) + PDQ(0,0,0)),
    F6 = ARIMA(Barrels ~ fourier(K = 6) + PDQ(0,0,0)),
    F7 = ARIMA(Barrels ~ fourier(K = 7) + PDQ(0,0,0)),
    F8 = ARIMA(Barrels ~ fourier(K = 8) + PDQ(0,0,0)),
    F9 = ARIMA(Barrels ~ fourier(K = 9) + PDQ(0,0,0)),
    F10 = ARIMA(Barrels ~ fourier(K = 10) + PDQ(0,0,0)),
    F11 = ARIMA(Barrels ~ fourier(K = 11) + PDQ(0,0,0)),
    F12 = ARIMA(Barrels ~ fourier(K = 12) + PDQ(0,0,0)),
    F13 = ARIMA(Barrels ~ fourier(K = 13) + PDQ(0,0,0)),
    F14 = ARIMA(Barrels ~ fourier(K = 14) + PDQ(0,0,0)),
    best_lm = TSLM(Barrels ~ trend(knots = yearweek(c("2006 W1", "2013 W1"))) + fourier(K = 6)), #these are identical
    best_lm2 = ARIMA(Barrels ~ trend(knots = yearweek(c("2006 W1", "2013 W1"))) + fourier(K = 6)
                     + pdq(0,0,0) + PDQ(0,0,0)),
    best_lm3 = ARIMA(Barrels ~ trend(knots = yearweek(c("2006 W1", "2013 W1"))) + fourier(K = 6)
                     + PDQ(0,0,0))
  )

glance(gas_fit) |> arrange(AICc)

gas_fit |>
  select(F6) |>
  report()

gas_fit |>
  select(F6) |>
  gg_tsresiduals()

gas_fit |>
  select(best_lm3) |>
  report()

gas_fit |>
  select(best_lm3) |>
  gg_tsresiduals()

gas_fit |>
  select(F6, best_lm3) |>
  forecast(h = "3 years") |>
  autoplot(us_gasoline)

# Prediction intervals much better - although a bit of  hetero left over
# This is the only way to handle weekly data



## VICTORIAN ELECTRICITY ---------------------------------------------
# Daily data with annual and weekly seasonality

vic_elec
vic_elec |> tail()
# Half-hourly over three years

# Turn half-hourly data into daily
vic_elec_daily <- vic_elec |>
  index_by(Date = date(Time)) |> # index by date to turn into daily
  summarise(                        # summarise() below
    Demand = sum(Demand)/1e3,       # Total daily and scaling Mega to Gigawatts
    Temperature = max(Temperature), # take highest temperature for the day
    Holiday = any(Holiday)          # Hol for any half hour is Hol for day
  ) |> # create new variable Day_Type
  mutate(Day_Type = case_when(       # Separate weekdays, weekends and holidays
    Holiday ~ "Holiday",             # If Holiday=TRUE call it a Holiday
    wday(Date) %in% 2:6 ~ "Weekday", # wday() returns 1:7 starting from a Sunday
    TRUE ~ "Weekend"                 # Call everything else a weekend
  ))

vic_elec_daily

# Seasonal patterns
vic_elec_daily |> gg_season(period = "week")
# Weekdays higher than weekends
vic_elec_daily |> gg_season(period = "year")
# higher demand in summer and in winter days
# higher variation in summer

# Highly non-linear pattern
# Heating for below maybe 18 degrees
# min demand (18-25)
# Cooling above 25 degrees
# Weekdays higher than weekends
# Holidays clustered within/similar to Weekends
# Some really extreme days

# Make longer to plot time series nicely
vic_elec_daily |>
  pivot_longer(c(Demand, Temperature),
               names_to = "Variable", values_to = "Value") |>
  ggplot(aes(x = Date, y = Value, colour=Variable)) +
  geom_line() +
  facet_grid(vars(Variable), scales = "free_y") +
  guides(colour="none")

# Scatter plot is useful in this case
vic_elec_daily |>
  ggplot(aes(x=Temperature, y=Demand, colour=Day_Type)) +
  geom_point() +
  labs(x = "Maximum temperature", y = "Electricity demand (GW)")
# Higher demand during higher temperatures - nonlinear
# Notice holidays clustered within Weekends although they
# may weekdays

# Let's try three different models

# DO NOT RUN
elec_fit <- vic_elec_daily |>
  model(
    ets = ETS(Demand),
    arima = ARIMA(log(Demand)),
    dhr = ARIMA(log(Demand) ~ Temperature + I(Temperature^2) +
                  (Day_Type == "Weekday") +
                  fourier(period = "year", K = 4))
  )

# I() treats this a new variable rather than interaction
# Modelling with quadratic is fine in this case
# within the range of the data/temperature
# y=f(x,x^2) not y=f(t,t^2)


# On the training data
accuracy(elec_fit)
# dhr by far the best one

# ETS
elec_fit |>
  select(ets) |>
  report()

elec_fit |>
  select(ets) |>
  gg_tsresiduals()

# highly correlated residuals
# heteroscedasticity - as a result long tail in the histogram

elec_fit |>
  select(ets) |>
  augment() |>
  filter(Date <= "2014-03-31") |>
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")

# It doesn't do too bad - but we can do better

# ARIMA
elec_fit |>
  select(arima) |>
  report()

elec_fit |>
  select(arima) |>
  gg_tsresiduals()

# lots of autocorrelation and hetero again

elec_fit |>
  select(arima) |>
  augment() |>
  filter(Date <= "2014-03-31") |>
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")
# similar to the ETS

# DHR
elec_fit |>
  select(dhr) |>
  report()

# Fewer ARMA parameters

elec_fit |>
  select(dhr) |>
  gg_tsresiduals()
# Still autocorrelation - but the values/size of these are smaller

elec_fit |>
  select(dhr) |>
  augment() |>
  filter(Date <= "2014-03-31") |>
  ggplot(aes(x = Date, y = Demand)) +
  geom_line() +
  geom_line(aes(y = .fitted), col = "red")


# Let's make ARIMA work a little harder

# DO NOT RUN
fit_better <- vic_elec_daily |>
  model(
    ARIMA(
      Demand ~ Temperature + I(Temperature^2) +
        (Day_Type == "Weekday") +
        fourier(period="year",K=4) +
        PDQ(period = "week") +
        pdq(0:7),
      stepwise=FALSE, order_constraint = p + q + P + Q <= 10)
  )

fit_better |> report()
fit_better |> gg_tsresiduals()


# Let's forecast the next day
vic_next_day <- new_data(vic_elec_daily, 1) |>
  mutate(Temperature = 26, Day_Type = "Holiday")

forecast(elec_fit, new_data = vic_next_day) |>
  autoplot(vic_elec_daily |> tail(50), level = 80) +
  labs(y = "Electricity demand (GW)")

# Let's forecast a few days ahead
# In practice you would use many more scenarios
# BOM forecasts are very accurate 4-5 days ahead
# They will give you forecasts up to to 7-days ahead
# You can simulate future scenarios of temperature for
# longer horizons from a temperature model
# and get many demand profiles.

# Forecast 14 days ahead
vic_elec_future <- new_data(vic_elec_daily, 14) |>
  mutate(
    Temperature = c(rep(32, 7), rep(25, 7)),
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
forecast(elec_fit, new_data = vic_elec_future) |>
  autoplot(vic_elec_daily |> tail(50), level = 80) +
  labs(y = "Electricity demand (GW)")


# Forecast a year ahead using last year's temperatures
vic_elec_future <- new_data(vic_elec_daily, 365) |>
  mutate(
    Temperature = tail(vic_elec_daily$Temperature, 365),
    Holiday = Date %in% as.Date(c(
      "2015-01-01", "2015-01-26", "2015-03-09",
      "2015-04-03", "2015-04-06", "2015-04-25",
      "2015-06-08", "2015-10-02", "2015-11-03",
      "2015-12-25"
    )),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

forecast(elec_fit, new_data = vic_elec_future) |>
  filter(.model == "dhr") |>
  autoplot(vic_elec_daily |> tail(365), level = 80) +
  labs(y = "Electricity demand (GW)")

# The mean is not too bad but the PIs are not.
# Obviously we can do better
# but we now have the annual seasonal pattern

# High variance in warmer months - lower in cooler months
# so monotonic transformations will not work. We need to
# do something else. In fact the models used are much more
# complicated cubic splines (similar to piecewise linear)
# are used instead of quadratic terms. A separate model for
# each day of the week - days are very different
# separate models for seasons. But this gives you a good
# starting point.



## AUSTRALIAN CAFE DATA --------------------------------------------------

# Do this quickly
aus_cafe <- aus_retail |> filter(
  Industry == "Cafes, restaurants and takeaway food services",
  year(Month) %in% 2004:2018
) |> summarise(Turnover = sum(Turnover)) # add up across the states
aus_cafe |> autoplot(Turnover)
# Total monthly turnover across all states

# Monthly data so usually don't need Fourier terms
# An easy example to start with
# 6 is the max Fourier terms and ARIMA deals only with non-seaosal bits
cafe_fit <- aus_cafe |> model(
  `K = 1` = ARIMA(log(Turnover) ~ fourier(K = 1) + PDQ(0,0,0)),
  `K = 2` = ARIMA(log(Turnover) ~ fourier(K = 2) + PDQ(0,0,0)),
  `K = 3` = ARIMA(log(Turnover) ~ fourier(K = 3) + PDQ(0,0,0)),
  `K = 4` = ARIMA(log(Turnover) ~ fourier(K = 4) + PDQ(0,0,0)),
  `K = 5` = ARIMA(log(Turnover) ~ fourier(K = 5) + PDQ(0,0,0)),
  `K = 6` = ARIMA(log(Turnover) ~ fourier(K = 6) + PDQ(0,0,0))
)

cafe_fit |> select("K = 2") |> report()
glance(cafe_fit) |>
  select(.model, sigma2, log_lik, AIC, AICc, BIC)
# Not surprising that we need all terms to deal
# with this complicated
# seasonal pattern - so using max dof to use



## AUSTRALIAN VISITORS -------------------------------------------------

aus_visitors <- as_tsibble(fpp2::austa)
# Total international visitors to Australia (in millions). 1980-2015.
# Different from the slides just to show you another one

aus_visitors |>
  autoplot(value) +
  labs(x = "Year", y = "millions of people",
       title = "Total annual international visitors to Australia")

fit_deterministic <- aus_visitors |>
  model(Deterministic = ARIMA(value ~ 1 + trend() + pdq(d = 0)))
report(fit_deterministic)

fit_stochastic <- aus_visitors |>
  model(Stochastic = ARIMA(value ~ 1 +pdq(d=1)))
report(fit_stochastic)

bind_cols(fit_deterministic, fit_stochastic) |>
  rename(`Deterministic trend` = 1, `Stochastic trend` = 2) |>
  forecast(h = 10) |>
  autoplot(aus_visitors) +
  facet_grid(vars(.model)) +
  labs(y = "Air passengers (millions)",
       title = "Forecasts from trend models") +
  guides(colour = FALSE)

aus_visitors |>
  autoplot(value) +
  autolayer(fit_stochastic |> forecast(h = 20),
            colour = "#0072B2", level = 95) +
  autolayer(fit_deterministic |> forecast(h = 20),
            colour = "#D55E00", alpha = 0.7, level = 95) +
  labs(y = "Air passengers (millions)",
       title = "Forecasts from trend models")

## AUSTRALIAN AIR PASSENGERS -------------------------------------------------
aus_airpassengers |>
  autoplot(Passengers) +
  labs(y = "Passengers (millions)",
       title = "Total annual air passengers")

fit_deterministic <- aus_airpassengers |>
  model(deterministic = ARIMA(Passengers ~ 1 + trend() + pdq(d = 0)))
report(fit_deterministic)

fit_stochastic <- aus_airpassengers |>
  model(stochastic = ARIMA(Passengers ~ 1 + pdq(d = 1)))
report(fit_stochastic)

fc_deterministic <- forecast(fit_deterministic, h = 200)
fc_stochastic <- forecast(fit_stochastic, h = 200)

aus_airpassengers |>
  autoplot(Passengers) +
  autolayer(fc_stochastic, colour = "#0072B2", level = 95) +
  autolayer(fc_deterministic, colour = "#D55E00", alpha = 0.65, level = 95) +
  labs(y = "Air passengers (millions)",
       title = "Forecasts from trend models")

