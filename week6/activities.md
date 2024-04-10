1. Use the tsibble created from `tourism` for holiday travel in Victoria and Queensland. Plot the series to remind yourself what these look like.

2. Use the `ETS()` function to fit models with additive and multiplicative errors to both series. Also let `ETS()` auto select models. Explore the fitted models and their residuals. 

3. Generate forecasts from the fitted models. Why is the multiplicative model needed for Victoria? 

4. Generate the `h02` series from the `PBS` tsibble we explored earlier using the code below.

    ```r
    h02 <- PBS |>
      filter(ATC2 == "H02") |>
      summarise(Cost = sum(Cost))
    ```
5. Find an `ETS` model and study it. Why has a damped trend been selected? 

6. Generate forecasts for the next few years. 

7. Combine `STL` decomposition with `ETS` to forecast the `h02` series.
