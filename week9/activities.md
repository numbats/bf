1. Identify, estimate and generate forecasts from ARIMA models for the `usmelec`, `leisure` and `h02` as specified below:

    ```r
    usmelec <- as_tsibble(fpp2::usmelec) |>
      rename(Month = index, Generation = value)
    
    leisure <- us_employment |>
      filter(Title == "Leisure and Hospitality", year(Month) > 2000) |>
      mutate(Employed = Employed/1000) |> select(Month, Employed)
  
    h02 <- PBS |>
      filter(ATC2 == "H02") |>
      summarise(Cost = sum(Cost))
    ```
2. Identify, estimate and generate forecasts from ARIMA and ETS models for the `aus_economy` and `cement` as specified below:

    ```r
    aus_economy <- global_economy |> filter(Code == "AUS") |>
      mutate(Population = Population/1e6)
  
    cement <- aus_production |>
      select(Cement) |>
      filter_index("1988 Q1" ~ .)
    ```
