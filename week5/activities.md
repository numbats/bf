1. Use the tsibble created from `tourism` for holiday travel in Victoria and Queensland. Plot the series to remind yourself what these look like.

2. Use the `ETS()` function to fit models to both series. Explore the resulting `mable` using `report()`, `glance()` and `tidy()`

3. Plot the estimated components of each model.

4. Generate forecasts using `forecast()`.

5. Plot the exports data for Algeria from the `global_economy` tsibble. Is this time series white noise? What ETS model would be appropriate?

6. Use the `ETS()` function to fit appropriate models with both additive and multiplicative errors. What model is chosen automatically? Explore the estimated models.

7. Plot the components of the two models. What do you see?

8. Explore the residuals of the two models. What do you see?

9. Generate and plot forecasts.
