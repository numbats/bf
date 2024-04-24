library(fpp3)

# 1. EGYPTIAN EXPORTS -----------------------------------

egypt <-global_economy |>
  filter(Code == "EGY")

egypt |> autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian Exports")

# Two questions
# 1. is data stationary?
# 2. what ARIMA model?

# Data is stationary - tested
egypt |>
  features(Exports,unitroot_ndiffs)

# But data is not WN
egypt |> ACF(Exports) |> autoplot()

fit <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports)) # Fully automated
report(fit)
# See the written model in the extra slide

gg_tsresiduals(fit)
# These look pretty good

augment(fit) |>
  features(.innov, ljung_box, lag = 10, dof = 3)
# Cannot reject H0: WN

fit |>
  forecast(h=60) |>
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian Exports")
# Cyclical

# Picking an ARIMA model
egypt |> ACF(Exports) |> autoplot()
egypt |> PACF(Exports) |> autoplot()

egypt |>
  gg_tsdisplay(Exports, plot_type = "partial")

fit1 <- global_economy |>
  filter(Code == "EGY") |>
  model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit1)

# Let's have a look at the residuals
fit1 |> gg_tsresiduals()

# Compare the AICc values


# 2. CAF EXPORTS ----------------------------------

global_economy |>
  filter(Code == "CAF") |>
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")
# Is this stationary?

# Remember signs of non-stationarity
global_economy |>
  filter(Code == "CAF") |>
  gg_tsdisplay(Exports, plot_type='partial')

# Let's difference
global_economy |>
  filter(Code == "CAF") |>
  gg_tsdisplay(difference(Exports), plot_type='partial')

# Very messy ACF and PACF - very usual - theory is hard
caf_fit <- global_economy |>
  filter(Code == "CAF") |>
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        fullsearch = ARIMA(Exports,
                           stepwise=FALSE,
                           trace = TRUE,
                           approximation = FALSE))
?ARIMA
# order_constraint = p + q + P + Q <= 6 & (constant + d + D <= 2)

caf_fit

glance(caf_fit) |> arrange(AICc) |> select(.model:BIC)

glance(caf_fit) |> arrange(AICc) |> select(.model:BIC) |>
  mutate(across(where(is.numeric), ~ num(., digits = 3)))

caf_fit |>
  select(fullsearch) |>
  gg_tsresiduals()

caf_fit |>
  select(fullsearch) |>
  augment() |>
  features(.innov, ljung_box, lag = 10, dof = 3)

caf_fit |>
  select(fullsearch) |>
  report()

caf_fit |>
  forecast(h=10) |>
#  filter(.model=='fullsearch') |>
  autoplot(global_economy)

caf_fit |> tidy()
# Continue with example below


# US Consumption ------------------------

us_change
# Quarterly changes in these variables

us_change |> autoplot(Consumption) +
  labs(y="Quarterly percentage change", title="US consumption")
# Is it stationary - already differenced data

us_change |> gg_tsdisplay(Consumption, plot_type = 'partial')

# Indicates possible AR(3)
# But let's fit an AR(2) model
fit <- us_change |>
  model(arima = ARIMA(Consumption ~ pdq(2,0,0) + PDQ(0,0,0)))

augment(fit) |> gg_tsdisplay(.innov, plot_type = 'partial')

# Let's correct it
fit <- us_change |>
  model(arima = ARIMA(Consumption ~ pdq(3,0,0) + PDQ(0,0,0)))
fit |> report()
augment(fit) |> gg_tsdisplay(.innov, plot_type = 'partial')

# Alternatively
us_change |> gg_tsdisplay(Consumption, plot_type = 'partial')
fit <- us_change |>
  model(arima = ARIMA(Consumption ~ pdq(0,0,3) + PDQ(0,0,0)))
fit |> report()
augment(fit) |> gg_tsdisplay(.innov, plot_type = 'partial')

# I can let ARIMA choose
fit <- us_change |>
  model(arima = ARIMA(Consumption ~ PDQ(0,0,0)))
fit |> report()
# It chooses based on AICc

# I can let ARIMA choose - only some bits
fit <- us_change |>
  model(arima = ARIMA(Consumption  ~ pdq(d=0) + PDQ(0,0,0)))
fit |> report()
# It chooses the rest based on AICc

# I can restrict the constant
fit <- us_change |>
  model(arima = ARIMA(Consumption  ~ 0 + pdq(d=0) + PDQ(0,0,0)))
fit |> report()
# It chooses the rest based on AICc

# The rest is the same - just use the forecast function
fit <- us_change |>
  model(arima = ARIMA(Consumption ~ PDQ(0,0,0),
                      stepwise = FALSE, trace = TRUE,
                      approximation = FALSE))

fit |> report()
fit |> tidy()
fit |> gg_tsresiduals()

fit |> forecast(h=10) |>
  autoplot(us_change)

fit |> forecast(h=10) |>
  autoplot(tail(us_change, 80))

# Returns to the mean of the historical data
fit |> forecast(h=50) |>
  autoplot(us_change)

# MINKS ---------------

mink <- as_tsibble(fma::mink)
mink |> autoplot(value) +
  labs(y="Minks trapped (thousands)",
       title = "Annual number of minks trapped")

mink |> gg_tsdisplay(value, plot_type = "partial")
# Remember we are looking at the last significant one

# Let's choose a model
mink |> model(ARIMA(value~pdq(4,0,0))) |> report()

mink |> model(ARIMA(value)) |> report()
# Not the best model - look at AICc of my manual one
# Also I need cycles - hence p>=2
mink |> model(ARIMA(value)) |> forecast(h=20) |> autoplot(mink)

# Let's make it work harder
mink |> model(ARIMA(value, stepwise=FALSE)) |> report()
# Will look at every possibility up to p+q<(max_order)

fit <- mink |>
  model(
    ar4 = ARIMA(value ~ pdq(4,0,0)),
    auto = ARIMA(value),
    best = ARIMA(value, stepwise=FALSE, approximation=FALSE)
  )

fit
glance(fit) |> arrange(AICc)
fit |> select(best) |> report()
fit |> select(best) |> gg_tsresiduals()
fit |> select(best) |> forecast(h=20) |> autoplot(mink)

?ARIMA
# Scroll down to specials
# Can look at more models
mink |>
  model(ARIMA(value, stepwise=FALSE, order_constraint = p+q+P+Q<=6)) |>
  report()

# Can look harder by not making any approximations
mink |>
  model(ARIMA(value, stepwise=FALSE, trace=TRUE,
              order_constraint = p+q+P+Q<=12, #There is a 10 upper limit here
              approximation = FALSE)) |>
  report()

mink |>
  model(ARIMA(value~pdq(p=0:6,q=0:6), stepwise=FALSE,
              order_constraint = p+q+P+Q<=12,
              approximation = FALSE, trace=TRUE)) |>
  report()


# Commonly switching off stepwise will find a reasonable model

mink |>
  model(ARIMA(value, stepwise=FALSE)) |>
  forecast(h=100) |>
  autoplot(mink)

## Web-usage ---------------------------

web_usage <- as_tsibble(WWWusage)
web_usage |> gg_tsdisplay(value, plot_type = 'partial')

web_usage |> gg_tsdisplay(difference(value), plot_type = 'partial')

fit <- web_usage |>
  model(arima = ARIMA(value ~ pdq(3, 1, 0))) |>
  report()

web_usage |>
  model(auto = ARIMA(value ~ pdq(d=1))) |>
  report()

web_usage |>
  model(auto2 = ARIMA(value ~ pdq(d=1),
       stepwise = FALSE, approximation = FALSE)) |>
  report()

gg_tsresiduals(fit)

augment(fit) |>
  features(.innov, ljung_box, lag = 10, dof = 3)

fit |> forecast(h = 10) |> autoplot(web_usage)


# Electrical Equipment --------------------------------------------------------

elecequip <- as_tsibble(fpp2::elecequip)
dcmp <- elecequip |>
  model(STL(value ~ season(window = "periodic"))) |>
  components() |>
  select(-.model)
dcmp |> as_tsibble |>
  autoplot(season_adjust) + xlab("Year") +
  ylab("Seasonally adjusted new orders index")

dcmp |> gg_tsdisplay(difference(season_adjust), plot_type = 'partial')

fit <- dcmp |>
  model(
    arima310 = ARIMA(season_adjust ~ pdq(3,1,0) + PDQ(0,0,0)),
    arima410 = ARIMA(season_adjust ~ pdq(4,1,0) + PDQ(0,0,0)),
    arima014 = ARIMA(season_adjust ~ pdq(0,1,4) + PDQ(0,0,0)),
    arima311 = ARIMA(season_adjust ~ pdq(3,1,1) + PDQ(0,0,0))
  )

glance(fit)

fit <- dcmp |>
  model(
    arima = ARIMA(season_adjust ~ PDQ(0,0,0))
    )
fit |> report()

fit <- dcmp |>
  model(
    arima = ARIMA(season_adjust ~ PDQ(0,0,0), stepwise = FALSE)
  )
fit |> report()
# Be warned

fit <- dcmp |>
  model(
    arima = ARIMA(season_adjust ~ PDQ(0,0,0), stepwise = FALSE,
                  approximation = FALSE)
  )
fit |> report()


gg_tsresiduals(fit)

augment(fit) |>
  features(.innov, ljung_box, lag = 24, dof = 4)

fit |> forecast |> autoplot(dcmp)

# GDP --------------------------------------------------------------------------

global_economy

# Pick a couple of countries - see logs
global_economy |>
  filter(Country=="Australia") |>
  autoplot(log(GDP))

global_economy |>
  filter(Country=="United States") |>
  autoplot(log(GDP))

# Find a suitable ARIMA model for each one of the 263 countries
fit <- global_economy |>
  model(
    ARIMA(log(GDP))
  )
# Has not worked for some countries because of no data being present

fit
fit |> tail()

fit |>
  filter(Country == "Australia") |>
  report()
fit |>
  filter(Country == "Australia") |>
  gg_tsresiduals()
# Some tiny correlations

fit |>
  filter(Country == "Australia") |>
  augment() |>
  features(.innov, ljung_box, dof=2, lag=10)
# Push it out to 15 and you get some significant dynamics

fit |>
  filter(Country == "Australia") |>
  forecast(h=10) |>
  autoplot(global_economy)
# fairly wide prediction intervals
+ scale_y_log10()
