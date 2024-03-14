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
  # log may seem too strong 
  # Hard to see the damping effect on variance due 
  # to the lack of noise

  # Note: can also take the log-transformation 
  # using the Box-Cox function 
us_economy |> 
  autoplot(box_cox(GDP, 0)) 

  # Let's try something weaker
us_economy |>
  autoplot(box_cox(GDP, 0.3))
  # Seems to work fairly well

  # Let's see what the Guerrero measure give us
us_economy |> 
  features(GDP, features = guerrero) -> lambda
  # Attempts to balance the seasonal fluctuations 
  # and random variation across the series.
  # Always check the results.

lambda$lambda_guerrero

us_economy |>
  autoplot(box_cox(GDP, lambda$lambda_guerrero))
  # More or less the same. Box-Cox transformations 
  # are usually insensitive to the choice of lambda 
  # at the second decimal place.

## aus_livestock ----------------------------------------------
aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  autoplot(Count)
  # A bit tougher to see what is needed

# Over to you 
# Try a log transformation 
# What does Guerrero stat suggest?



aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  autoplot(log(Count))
  # Yes may be it has helped

aus_livestock |>
  filter(Animal == "Bulls, bullocks and steers", State == "Victoria") |>
  features(Count, guerrero) 
  # suggesting a log so we might go with that

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

vic_elec |>
  autoplot(log(Demand))
  # Makes the variance more even and reduces the skewness  
  # But we can do better 
  # Need to deal with this differently 


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

canadian_gas |>
  autoplot(box_cox(Volume, 0.6))
  # Summary: we can only fix monotonic changes in variance
  # i.e., variance increases or decreases with the 
  # level of the data 


# BACK TO SLIDES

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
  # Lots of interesting features - what are they
  # Questions:
  #   1. transformation or not?
  #   2. additive or multiplicative?
  # Cyclical and seasonal
  # Recall we do not aim to separate trend/cycle


# BACK TO SLIDES TO THINK ABOUT MA BEFORE STL

# 3. ---------------------------------------------------------
## STL decomposition 
## US retail employment

dcmp <- us_retail_employment |>
  model(stl = STL(Employed))
# STL 
# Seasonal Trend decomposision
# using LOcal regrESSions 

## a. Plot the decomposition.---------------------------------
dcmp

dcmp |> components()
  # Y_t = T_t + S_t + R_t
  # SeasAdjusted = Y_t-S_t
  
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
# media commentators analyse and comment on every blip on the 
# seasonally adjusted data - they should be using the trend estimate
# which is much smoother

## d. How does the seasonal shape change over time? --------------

  # Let's have a look at how the seasonal component changes
dcmp |> components() |> gg_subseries(season_year)
  # Mostly pretty flat but allowed to change
  # Most action in Sep and Dec

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

# Seasonal window
s.window=13

# Trend Window - the next odd of:
ceiling((1.5*m)/(1-(1.5/s.window)))

# set season(window=91) equivalent to "periodic"
# set season(window=13)
# set season(window=9)
# set season(window=3)

us_retail_employment |>
  model(STL(Employed ~ season(window=13))) |>
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
  model(STL(Employed ~ trend(window=11))) |>
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
