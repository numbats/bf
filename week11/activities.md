1. Review the model for US gasoline data from last week to now be a dynamic harmonic regression model. 


2. Turn the half hourly electricity data into daily data using the following code. Try and understand what each line does.

    ```r
    vic_elec_daily <- vic_elec |>   
      index_by(Date = date(Time)) |> 
      summarise(                        
        Demand = sum(Demand)/1e3,       
        Temperature = max(Temperature), 
        Holiday = any(Holiday)          
      ) |> 
      mutate(Day_Type = case_when(       
        Holiday ~ "Holiday",             
        wday(Date) %in% 2:6 ~ "Weekday", 
        TRUE ~ "Weekend"                 
      )) 
    ```
    Explore the seasonal patterns.
    
    a. Fit an ETS, ARIMA and a dynamic harmonic regression model using the following code:
    
    ```r
    elec_fit <- vic_elec_daily |>
      model(
        ets = ETS(Demand),
        arima = ARIMA(log(Demand)),
        dhr = ARIMA(log(Demand) ~ Temperature + I(Temperature^2) + 
                      (Day_Type == "Weekday") + 
                      fourier(period = "year", K = 4))
      )
    ```
    Explore the model fits and residuals. 
    
    b. Generate forecast for 14-days-ahead using the following code. 
    
    ```r
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
    ```

