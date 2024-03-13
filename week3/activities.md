
1. For the following series, find an appropriate Box-Cox transformation in order to stabilise the variance.

    * United States GDP from `global_economy`
    * Slaughter of Victorian “Bulls, bullocks and steers” in `aus_livestock`
    * Victorian Electricity Demand from `vic_elec`.
    * Gas production from `aus_production`

2. Why is a Box-Cox transformation unhelpful for the `canadian_gas` data?

3. Produce the following decomposition for the number (in thousands) of of people employed in Retail Trade in the US

    ```r
    us_retail_employment <- us_employment |>
      filter(year(Month) >= 1990, Title == "Retail Trade") |>
      select(-Series_ID)

    dcmp <- us_retail_employment |>
            model(stl = STL(Employed)) 
    ```
    
    a. Plot the decomposition.
    
    b. Fit the trend component over the data [Hint: you can use `autolayer()` to add `trend` to the plot above. `trend` is one of the variables returned by `STL()`. ]
    
    c. Fit the trend and the seasonally adjusted [Hint: `seas_adjust` is one of the variables returned by `STL`. ]
    
    d. How does the seasonal shape change over time? [Hint: Try plotting the seasonal component using `gg_season()`.]
    
    e. What happens as you change the values of the two `window` arguments?
    
    f. Can you produce a plausible seasonally adjusted series? 
