library(fpp3)

## Example: Australian holiday tourism ------

holidays <- tourism |>
  filter(State %in% c("Victoria", "Queensland")) |> 
  filter(Purpose == "Holiday") |>
  as_tibble() |>
  summarise(Trips = sum(Trips), .by = c("State", "Quarter")) |>
  as_tsibble(index = Quarter, key = State)

holidays |> autoplot()

fit <- holidays |>
  model(
    additive = ETS(Trips ~ error("A") + trend("A") + season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") + season("M")),
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
  select(additive) |> augment()

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
  residuals(type = "response")

fit |>
  filter(State=="Victoria") |> 
  select(additive) |>
  gg_tsresiduals()


fit
fit |> glance()

fc <- fit |> forecast()

fc |>
  autoplot(holidays) + 
  labs(Title="Overnight trips (thousands)")

fc |> 
  filter(State=="Victoria", !.model=="auto") |> 
  autoplot(holidays) + 
  labs(Title="Overnight trips (thousands)")+
  facet_wrap(vars(.model), ncol = 1)

fc |> 
  filter(State=="Queensland", !.model=="auto") |> 
  autoplot(holidays) + 
  labs(Title="Overnight trips (thousands)")+
  facet_wrap(vars(.model), ncol = 1)


## H02 ------------------------------------

h02 <- PBS |>
  filter(ATC2 == "H02") |>
  summarise(Cost = sum(Cost))

h02 |>
  autoplot(Cost)

h02 |>
  model(ETS(Cost)) |>
  report()
# Comment on the damped trend

h02 |>
  model(ETS(Cost ~ error("A") + trend("A") + season("A"))) |>
  report()

h02 |>
  model(ETS(Cost~ trend("A"))) |>
  forecast(h=200) |>
  autoplot(h02)


# Example of STL + ETS

stl_fit <- h02 |>
  model(
    decomposition_model(
      STL(Cost), 
      ETS(season_adjust),
      SNAIVE(season_year)
    )
  )
stl_fit |> report()

stl_fit |>
  forecast(h = 24) |>
  autoplot(h02)
