library(fpp3)

# Workshop Activity 1 -----------------------------------------
# Use the tsibble created from `tourism` for holiday travel in Victoria and Queensland. 
# Plot the series to remind yourself what these look like.
# Australian holiday tourism 


holidays <- tourism |>
  filter(State %in% c("Victoria", "Queensland")) |> 
  filter(Purpose == "Holiday") |>
  as_tibble() |>
  summarise(Trips = sum(Trips), .by = c("State", "Quarter")) |>
  as_tsibble(index = Quarter, key = State)

holidays |> autoplot()

# Workshop Activity 2 -----------------------------------------
# Use the `ETS()` function to fit models with additive and 
# multiplicative errors to both series. Also let `ETS()` 
# auto select models. Explore the fitted models and their residuals. 

fit <- holidays |>
   model(
      additive = ETS(Trips ~ error("A")),
      multiplicative = ETS(Trips ~ error("M")),
      auto = ETS(Trips)
   )

fit

fit |>
  filter(State=="Victoria") |> 
  select(multiplicative) |>
  report()

fit |> 
  filter(State=="Victoria") |> 
  components() |> 
  autoplot()

fit |>
  filter(State=="Victoria") |> 
  select(multiplicative) |>
  components() |>
  autoplot()
  # Comment on the model specification and the states

fit |>
  filter(State=="Victoria") |> 
  select(additive) |> 
   augment()

fit |>
  filter(State=="Victoria") |> 
  select(multiplicative) |> 
  augment()

fit |> 
  filter(State=="Victoria") |> 
  select(multiplicative) |> 
  residuals()
  # This is a bit confusing but be aware of it

fit |> 
   filter(State=="Victoria") |> 
   select(multiplicative) |> 
   residuals(type="innovation")

fit |> 
  filter(State=="Victoria") |> 
  select(multiplicative) |> 
  residuals(type = "response")

fit |>
   filter(State=="Victoria") |> 
   select(multiplicative) |>
   gg_tsresiduals()


fit |>
  filter(State=="Queensland") |> 
  select(additive) |>
  gg_tsresiduals()


fit
fit |> glance()

# Workshop Activity 3 -----------------------------------------
# Generate forecasts from the fitted models. 
# Why is the 
# multiplicative model needed for Victoria? 


fc <- fit |> 
   forecast()

fc |>
  autoplot(holidays) + 
  labs(title="Overnight trips (thousands)")

fc |> 
  filter(State=="Victoria", !.model=="auto") |> 
   autoplot(holidays) + 
   labs(title="Victoria: Overnight trips (thousands)")+
   facet_wrap(vars(.model), ncol = 1)

fc |> 
  filter(State=="Queensland", !.model=="auto") |> 
  autoplot(holidays) + 
  labs(title="Queensland: Overnight trips (thousands)")+
  facet_wrap(vars(.model), ncol = 1)


# Workshop Activity 4 -----------------------------------------
# Generate the `h02` series from the `PBS` tsibble we explored earlier using the code below.
# Plot the data and study it's features. What ETS model would be appropriate?


h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost))

h02 |>
  autoplot(Cost)

# Workshop Activity 5 -----------------------------------------
# Find an `ETS` model and study it. 

h02 |>
  model(ETS(Cost)) |>
  report()
# Comment on the damped trend


# Workshop Activity 6 -----------------------------------------
# Generate forecasts for the next few years. 

h02 |>
  model(ETS(Cost~ trend("A"))) |> # change to Ad
  forecast(h="8 years") |> #increase to 20 years
  autoplot(h02)


# Workshop Activity 7 -----------------------------------------
# Combine `STL` decomposition with `ETS` to forecast the `h02` series.

h02 |> 
  model(STL(Cost)) |> 
  components() |> 
  autoplot()
# Looking at this you could take logs

h02 |> 
  model(STL(log(Cost)~season(window = "periodic"))) |> #
  components() |> 
  autoplot()
# You could play with the trend and season window

stl_fit <- h02 |>
  model(
    decomposition_model(
      STL((Cost)), #notice the selected ETS(M,Ad,N) - take log() - 
      ETS(season_adjust),
      SNAIVE(season_year)  #Change to ETS
    )
  )
stl_fit |> report()


stl_fit |>
  forecast(h = "20 years") |>
  autoplot(h02)
