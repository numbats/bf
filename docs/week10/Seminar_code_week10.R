library(fpp3)

# US change ------------------------------------------------------------------

# Percentage changes of these variables
us_change

us_change |>
  pivot_longer(-Quarter, names_to = "Measure", values_to = "Change") |>
  autoplot(Change) +
  facet_grid(Measure ~ ., scales = "free_y")+
  labs(y="")+
  guides(colour="none")

# GGally package - Di Cook
us_change %>%
  GGally::ggpairs(columns = 2:6) 

my_fn <- function(data, mapping, ...){
    p <- ggplot(data = data, mapping = mapping) + 
      geom_point() + 
      geom_smooth(method=lm, se=FALSE )
  }

us_change %>%
  GGally::ggpairs(columns = 2:6,
                  lower = list(continuous = my_fn)) 
  
# Select variables  

us_change

2^4 #models
2^10
2^20 # over a million 

# Notice how fast
fit_all <- us_change %>%
  model(
    TSLM(Consumption ~ Income + Production + Unemployment + Savings),
    TSLM(Consumption ~ Production + Unemployment + Savings),
    TSLM(Consumption ~ Income + Unemployment + Savings),
    TSLM(Consumption ~ Income + Production + Savings),
    TSLM(Consumption ~ Income + Production + Unemployment),
    TSLM(Consumption ~ Income + Production),
    TSLM(Consumption ~ Income + Unemployment),
    TSLM(Consumption ~ Income + Savings),
    TSLM(Consumption ~ Production + Unemployment),
    TSLM(Consumption ~ Production + Savings),
    TSLM(Consumption ~ Unemployment + Savings),
    TSLM(Consumption ~ Income),
    TSLM(Consumption ~ Production),
    TSLM(Consumption ~ Unemployment),
    TSLM(Consumption ~ Savings),
    TSLM(Consumption ~ 1 ),
  )

glance(fit_all) %>%
  select(.model, AICc, BIC, CV) %>%
  arrange(AICc)

# Interaction effects
fit_inter <- us_change %>%
  model(
    TSLM(Consumption ~ Income + Savings + Production + Unemployment 
         + Income * Unemployment),
  ) 

fit_inter %>% glance() %>% select(.model, AICc,CV)

fit_consBest <- us_change %>%
  model(
    TSLM(Consumption ~ Income + Savings + Production + Unemployment)
  )


report(fit_consBest)

augment(fit_consBest) |>
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(
    y = NULL,
    title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values = c(Data = "black", Fitted = "#D55E00")) +
  guides(colour = guide_legend(title = NULL))

augment(fit_consBest) |>
  ggplot(aes(y = .fitted, x = Consumption)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percentage change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)

fit_consBest |> gg_tsresiduals()

# Checking residuals patterns
augment(fit_consBest) |>
  left_join(us_change) |>
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_point()

us_change |>
  left_join(residuals(fit_consBest), by = "Quarter") |>
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") |>
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

# Scenario based forecasting

future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income=1, Savings=0.5, Unemployment=0, Production=0),
  Decrease = new_data(us_change, 4) %>%
    mutate(Income=-1, Savings=-0.5, Unemployment=0, Production=0),
  names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)

us_change %>% autoplot(Consumption) +
  labs(y="% change in US consumption") +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")


# US Gasoline ------------------------
# Weekly data with Fourier terms

us_gasoline  |>
  autoplot(Barrels) +
  labs(y = "Weekly US finished motor gasoline product supplied (million barrels)")

us_gasoline |> 
  gg_season(period = "year")

us_gasoline |> 
  model(STL()) |> 
  components() |> 
  autoplot()

fit <- us_gasoline |>
  model(
    K01 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 1)),
    K05 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 5)),
    K06 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2013 W1"))) + fourier(K = 6)),
    K07 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 7)),
    K08 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 8)),
    K09 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 9)),
    K10 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 10)),
    K15 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 15)),
    K20 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 20)),
    K25 = TSLM(log(Barrels) ~ trend(knots = yearweek(c("2006 W1", "2011 W1"))) + fourier(K = 25))
  )

# Just to think about the dof
fit |> select(K01) |> report()

glance(fit) |>
  select(.model, r_squared, df, AICc, CV) |>
  arrange(CV)

augment(fit) |>
  filter(.model %in% c("K06", "K01", "K25")) |>
  ggplot(aes(x = Week, y = Barrels)) +
  geom_line() +
  geom_line(aes(y = .fitted, col = .model)) +
  facet_grid(.model ~ .)

fit |>
  select(K06) |>
  gg_tsresiduals()

fit |>
  select(K06) |>
  forecast(h="3 years") |> 
  autoplot(us_gasoline)


