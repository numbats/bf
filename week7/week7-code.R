library(fpp3)

# Workshop Activity 1 -----------------------------------------
# What sorts of transformations and differencing are needed
# to make the `Cement` series from `aus_production` stationary?
# Do the tests agree with your decisions?


# Australia Cement production
aus_production |>
  autoplot(Cement) +
  labs(title = "Cement production in Australia")
  # Fails stationarity in all three ways
  # - not horizontal - trending
  # - increasing variance - multiplicative seasonality
  # - seasonal component predictable in the long run

# Stabilise variance
aus_production |>
  autoplot(
    log(Cement)
    ) +
  labs(title = "Cement production in Australia")
  # log is good enough

# Account for seasonality
aus_production |>
  autoplot(
    log(Cement) |> difference(lag=12)
    ) +
  labs(title = "Cement production in Australia")
  # is this now stationary?


# Let's get some help from gg_tsdisplay
aus_production |>
  gg_tsdisplay(
    log(Cement) |> difference(lag=12)
  )
  # Clearly need another difference

aus_production |>
  autoplot(
    log(Cement) |> difference(12) |> difference(1)
    ) +
  labs(title = "Cement production in Australia")
  # Now looks stationary

aus_production |>
  gg_tsdisplay(
    log(Cement) |> difference(12) |> difference(1)
  )
  # verified by the ACF - however notice it is definitely NOT WN

# Let's verify through more official
aus_production |>
  features(log(Cement), feat_stl)
  # Relevant feature is the seasonal_strength

aus_production |>
  features(log(Cement),
           unitroot_nsdiffs)
  # Based on seasonal_strength feature

aus_production |>
  features(log(Cement) |> difference(12),
           unitroot_ndiffs)
  # Based on KPSS


# Workshop Activity 2 -----------------------------------------
# Generate the a10 and the h02 series from the PBS tsibble we explored
# earlier using the code below.

a10 <- PBS |>
  filter(ATC2 == "A10") |>
  summarise(Cost = sum(Cost))

a10 |> autoplot(log(Cost))

# Difference without transformation - what do you see?
a10 |> autoplot(
  difference(Cost,12)
  )

a10 |> autoplot(
  difference(log(Cost),12)
  )
  # Do I need another difference?
  # Possibly

a10 |>
  features(
    difference(log(Cost),12),
    unitroot_ndiffs)

## h02
h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost))

h02 |>
  autoplot(Cost)

h02 |>
  autoplot(log(Cost))


h02 |> autoplot(
  difference(log(Cost),12)
  )

h02 |>
  features(
    difference(log(Cost),12), unitroot_ndiffs)

h02 |>
  features(
    difference(log(Cost),12), unitroot_kpss
    )


# Workshop Activity 3 -----------------------------------------
# Explore the the Algerian exports series from the `global_economy`
# tsibble. Is the series stationary? Is the series white noise?

algeria <-global_economy |>
  filter(Country == "Algeria")

algeria |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Algerian Exports")

# Data is stationary - tested
algeria |>
  features(Exports,unitroot_ndiffs)

# But data is not WN
algeria |>
  ACF(Exports) |>
  autoplot()

# Clearly not WN
# This is what we will be modelling


