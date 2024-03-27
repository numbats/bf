library(fpp3)

## 1.-----------------------------------------
## Use the tsibble created from `tourism` 
## for holiday travel in Victoria and Queensland. 
## Plot the series to remind yourself what these look like. 

holidays <- tourism |>
  filter(State %in% c("Victoria", "Queensland")) |> 
  filter(Purpose == "Holiday") |>
  as_tibble() |>
  summarise(Trips = sum(Trips), .by = c("State", "Quarter")) |>
  as_tsibble(index = Quarter, key = State)

holidays |> autoplot()

## 2.-----------------------------------------
## Use the `ETS()` function to fit models to both series.
## Explore the resulting `mable` using `report()`, `glance()` 
## and `tidy()`

fit <- holidays |>
  model(ets = ETS(Trips))

fit
  # Two very different models fitted
  # Notice how quickly that happened

fit |>
  report()
  # Why the warning message?
  
  # Hence the above has reverted to 
fit |> glance() 
  # glance() reports something very different to report()

  # let's try report for each model
fit |>
  filter(State == "Queensland") |>
  report() 

fit |>
  filter(State == "Victoria") |>
  report() 

  # Another useful function
fit |> tidy()
  # Notice the parameters estimated


## 3.-----------------------------------------
## Plot the estimated components of each model.

fit |>
#  filter(State == "Queensland") |>
  components(fit) |>
  autoplot()

fit |>
  filter(State == "Victoria") |>
  components(fit) |>
  autoplot()

## 4.-----------------------------------------
## Generate forecasts using `forecast()`.

fit |>
  forecast() |>
  autoplot(holidays, show_gap = FALSE) +
  xlab("Year") + ylab("Overnight trips (thousands)")

## 5.-----------------------------------------
## Plot the exports data for Algeria from the 
## `global_economy` tsibble. Is this time series white noise?
## What ETS model would be appropriate?

algeria_economy <- global_economy  |> 
  filter(Country == "Algeria")

algeria_economy |> autoplot(Exports)

algeria_economy |> ACF(Exports) |> autoplot()

## 6.-----------------------------------------
## Use the `ETS()` function to fit appropriate models
## with both additive and multiplicative errors. What model
## is chosen automatically? Explore the estimated models.

fit <- algeria_economy |>
  model(
    ANN = ETS(Exports ~ error("A") + trend("N") + season("N")),
    MNN = ETS(Exports ~ error("M") + trend("N") + season("N")),
    autoNN = ETS(Exports ~ trend("N") + season("N")), #it will choose error("")
    auto = ETS(Exports), #it will choose everything
  )
  
fit
  # an MNN is automatically chosen

fit |> select(ANN) |> report()
fit |> select(MNN) |> report()
  # Notice the different parameters and sigma

fit |> tidy()
fit |> glance() 


# You can do various overrides 
algeria_economy |>
  model(
    ANN = ETS(Exports ~ error("A") + trend("N", alpha=0.2) 
              + season("N"))
  ) |> 
  report()

algeria_economy |>
  model(
    ANN = ETS(Exports ~ error("A") + trend("N", alpha_range=c(0.2, 0.7)) 
              + season("N"))
  ) |> 
  report()
  # Normally we leave these alone

# Drop the automated
fit <- fit |>
  select(Country, ANN, MNN)

fit |> tidy()

## 7.-----------------------------------------
## Plot the components of the two models. What do you see?

fit |>
  components() 

fit |>
  components() |> 
  autoplot()

fit |>
  select(MNN) |> 
  components() |> 
  autoplot()

  # Join in the fitted values
fit |> 
  select(Country, ANN) |> 
  components() |>
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

fit |> 
  select(Country, MNN) |> 
  components() |>
  left_join(fitted(fit), by = c("Country", ".model", "Year"))

## 8.-----------------------------------------
## Explore the residuals of the two models. What do you see? 

fit |> 
  select(ANN) |> 
  augment()

fit |> 
  select(MNN) |> 
  augment()
  # Notice the difference in .innov and .resid
  # Why the difference?

fit |> 
  select(ANN) |> 
  gg_tsresiduals()

fit |> 
  select(MNN) |> 
  gg_tsresiduals()

## 9.-----------------------------------------
# Generate and plot forecasts.

  # Recall point forecasts are the mean of all possible futures
  # i.e., expected values. 
  # Notice the point forecasts are flat. 
fit |>
  forecast(h = 5) |>
  autoplot(algeria_economy) +
  labs(y= "Exports (% of GDP)")

fit |>
  select(Country, MNN) |> 
  forecast(h = 5) |>
  autoplot(algeria_economy) +
  labs(y="Exports (% of GDP)")

  # equivalently
fit |>
  forecast(h = 5) |>
  filter(.model=="MNN") |> 
  autoplot(algeria_economy) +
  ylab("Exports (% of GDP)") + xlab("Year")


