library(fpp3)

# 1. -------------------------------------------------------
# For the following series, find an appropriate Box-Cox 
# transformation in order to stabilise the variance.

## us_economy ----------------------------------------------
us_economy <- global_economy |>
  filter(Country == "United States")

us_economy |>
  autoplot(GDP)

us_economy |>
  autoplot(log(GDP))

us_economy |> 
  autoplot(box_cox(GDP, 0)) 

us_economy |>
  autoplot(box_cox(GDP, 0.3))

us_economy |> 
  features(GDP, features = guerrero) -> lambda

lambda$lambda_guerrero

us_economy |>
  autoplot(box_cox(GDP, lambda$lambda_guerrero))

## aus_livestock ----------------------------------------------
aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  autoplot(Count)

  # Try a log transformation 
  # What does Guerrero stat suggest?

## vic_elec ---------------------------------------------------

vic_elec |>
  autoplot(Demand)
  # Tricky because of the non-monotonic changes in variance
  # There are multiple seasonal patterns here:

  # Time of day hidden due to density of ink.
vic_elec |> gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")

  # Day-of-week seasonality just visible.
vic_elec |> gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")

  # Time-of-year seasonality is clear with increasing 
vic_elec |> gg_season(Demand, period = "year") +
    labs(y="MWh", title="Electricity demand: Victoria")
  # variance in winter and high skewness in summer.

  # Try a log transformation 

## aus_production ------------------------------------------------

aus_production |> 
  autoplot(Gas)
  
aus_production |> 
  autoplot(log(Gas))
  # Maybe log is too strong 
  # Maybe a weaker transformation is needed

aus_production |> 
  features(Gas, guerrero) 

aus_production |> 
  autoplot(box_cox(Gas, 0.1))
  # Guerrero seems to be useful in this case

# 2.-----------------------------------------
# Why is a Box-Cox transformation unhelpful for the `canadian_gas` data?

  # Monthly Canadian gas production, 
  # billions of cubic metres, January 1960 - February 2005
canadian_gas |>
  autoplot(Volume)
  # Again a very tricky one
  # Notice the shape of seasonality also changes over time
  # Between 1975 and 1990 the gas production is moderate, 
  # and the variation is large.
  # Possibly due to changes in regulation with Gas production

  # try a Box Cox with lambda=0.6

# 3. ---------------------------------------------------------
## STL decomposition 
## US retail employment

us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
  # A single series - thousands of people employed 
  # in Retail Trade in the US

  # Aim: Can we pull out the trend/cycle and seasonal component
us_retail_employment |>
  autoplot(Employed) +
  labs(y = "Persons (thousands)", 
       title = "Total employment in US retail")
  # Lots of interesting features - what are they?
  # Questions:
  #   1. transformation or not?
  #   2. additive or multiplicative?
  # Cyclical and seasonal
  # Recall we do not aim to separate trend/cycle

dcmp <- us_retail_employment |>
  model(stl = STL(Employed))
# STL 
# Seasonal Trend decomposision
# using LOcal regrESSions 


## a. Plot the decomposition.---------------------------------
dcmp

dcmp |> components()

dcmp |> components() |> autoplot() +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")
  # Components add up to data
  # Seasonal changes over time slowly

## b. Fit the trend over the data ---------------------------

us_retail_employment |>
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), trend, color='#D55E00') +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

## c. Fit the trend and the seasonally adjusted --------------

us_retail_employment |>
  autoplot(Employed, color='gray') +
  autolayer(components(dcmp), trend, color='#D55E00') +
  autolayer(components(dcmp), season_adjust, color='#0072B2') +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

## d. How does the seasonal shape change over time? --------------

  # Hint: pipe the seasonal component into gg_subseries()

# What happens with periodic
us_retail_employment |>
  model(stl = STL(Employed~season(window="periodic"))) |> 
  components() |> 
  autoplot()

us_retail_employment |>
  model(stl = STL(Employed~season(window="periodic"))) |> 
  components()|> 
  gg_subseries(season_year)

## c. What happens as you change the values of the two `window` arguments? -------------

# Let's start with the basic model 

# Let's play around with some parameters 
# and let's see what happens

# Let's control the seasonal component
# season(window=13) - Default setting - no science behind
# trend selected by formula see slide 71

#Default 
m=12 #monthly seasonality
s.window=13
# the next odd of:
ceiling((1.5*m)/(1-(1.5/s.window)))

# set season(window=91) equivalent to "periodic"
# set season(window=13)
# set season(window=9)
# set season(window=3)

us_retail_employment |>
  model(STL(Employed ~ season(window=3))) |>
  components() |> 
  autoplot() +
  ggtitle("STL decomposition: US retail employment")

# Let's control the trend 

# Default is complex automated algorithm depends on size of 
# season and noise - works fairly well so we usually 
# leave it alone

# Default setting for monthly data is 21
# set trend(window=5) - notice the remainder
# set trend(window=11) - after this remainder has the dip
# set trend(window=29, 999)

us_retail_employment |>
  model(STL(Employed ~ trend(window=21))) |>
  components() |> 
  autoplot() +
  ggtitle("STL decomposition: US retail employment")

# Robust option - make this true and any outliers should 
# show up in remainder
# Helps with the smoothness of the trend
# Basically the algorithm uses an iterative process
# it down weighs any large obs in the remainder

# not many outliers here - hence not too much difference
us_retail_employment |>
  model(STL(Employed, robust=TRUE)) |>
  components() |> 
  autoplot() +
  ggtitle("STL decomposition: US retail employment")
#

us_retail_employment |>
  model(STL(Employed, robust=TRUE)) |>
  components() |> 
  autoplot() +
  ggtitle("STL decomposition: US retail employment")
