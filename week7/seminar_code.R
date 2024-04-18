library(fpp3)

#1. Australia Cement production ------------

aus_production |>
  autoplot(Cement) +
  labs(title = "Cement production in Australia")
  #Fails stationarity in all three possible ways

aus_production |>
  autoplot(
    log(Cement)
  ) +
  labs(title = "Cement production in Australia")

aus_production |>
  autoplot(
    log(Cement) |> difference(lag=12)
  ) +
  labs(title = "Cement production in Australia")

# Let's get some help from gg_tsdisplay
aus_production |>
  gg_tsdisplay(
    log(Cement) |> difference(lag=12)
  )
#Needs another difference

aus_production |>
  autoplot(
    log(Cement) |> difference(12) |> difference(1)
  ) +
  labs(title = "Cement production in Australia")

aus_production |>
  gg_tsdisplay(
    log(Cement) |> difference(12) |> difference(1)
  )

# Let's verify through more official
aus_production |>
  features(log(Cement), feat_stl)

aus_production |>
  features(log(Cement),
           unitroot_nsdiffs)

aus_production |>
  features(log(Cement) |> difference(12),
           unitroot_ndiffs)


#2. Australia Gas production ------------

aus_production |>
  autoplot(Gas)

aus_production |>
  autoplot(log(Gas))

aus_production |>
  features(Gas, guerrero)

aus_production |>
  autoplot(box_cox(Gas,0.11))

aus_production |>
  gg_tsdisplay(
    box_cox(Gas,0.11) |> difference(lag = 4)
  )

aus_production |>
  gg_tsdisplay(
    box_cox(Gas,0.11) |> difference(lag = 4) |> difference()
  )

aus_production |>
  features(
    box_cox(Gas,0.11),
    feat_stl
  )

aus_production  |>
  features(
    box_cox(Gas,0.11),
    unitroot_nsdiffs
  )

aus_production  |>
  features(
    box_cox(Gas,0.11)|> difference(lag=4),
    unitroot_ndiffs
  )

#3. A10 drugs --------------

a10 <- PBS |>
  filter(ATC2 == "A10") |>
  summarise(Cost = sum(Cost) / 1e6)
# Fails stationarity in all three ways


a10 |> autoplot(Cost)

a10 |>
  autoplot(log(Cost))

a10 |>
  gg_tsdisplay(log(Cost),lag_max = 48)

a10 |>
  features(log(Cost), feat_stl)

a10 |>
  features(log(Cost), unitroot_nsdiffs)

a10 |>
  autoplot(
  log(Cost) |> difference(lag = 12)
  )

a10 |>
  gg_tsdisplay(log(Cost) |> difference(lag=12))

a10 |>
  features(log(Cost) |> difference(lag=12), feat_stl)

a10 |>
  features(log(Cost) |> difference(lag=12), unitroot_ndiffs)

##3. H02 drugs -------

h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost) / 1e6)

h02 |> autoplot(Cost)

h02 |> autoplot(log(Cost))

h02 |>
  autoplot(
    log(Cost) |> difference(12)
    )

h02 |>
  features(difference(log(Cost), 12), unitroot_ndiffs)

h02 |>
  gg_tsdisplay(
    log(Cost) |> difference(12) |> difference()
    )




#4. ALGERIAN EXPORTS -----------------------------------

algeria <-global_economy %>%
  filter(Country == "Algeria")

algeria %>% autoplot(Exports) +
  labs(y = "% of GDP", title = "Algerian Exports")

# Data is stationary - tested
algeria %>%
  features(Exports,unitroot_ndiffs)

# But data is not WN
algeria %>% ACF(Exports) %>% autoplot()

# This is what we will be modelling



# Unit root tests
fb  |> features(Close, unitroot_kpss)
fb  |> features(Close |> difference(), unitroot_kpss)
fb  |> features(Close, unitroot_ndiffs)

## Seasonal strength
aus_production  |>
  features(
    box_cox(Bricks, lambda = 0.3),
    feat_stl
  )
aus_production  |>
  features(
    box_cox(Bricks, lambda = 0.3),
    unitroot_nsdiffs
  )

## Unit root
aus_production  |>
  features(
    box_cox(Bricks, lambda = 0.3) |> difference(lag = 4),
    unitroot_kpss
  )
aus_production  |>
  features(
    box_cox(Bricks, lambda = 0.3) |> difference(lag = 4),
    unitroot_ndiffs
  )

