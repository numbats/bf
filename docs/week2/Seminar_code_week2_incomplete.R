library(fpp3)

# Explore some rich tsibbles ----------------------------------------


## Olympics ---------------------------------------------------------
olympic_running
olympic_running |> distinct(Length)
olympic_running$Year |> range()

olympic_running |>
  filter(Length=="100") |>
  autoplot(Time) # add geom_point()

# Just to show you what you can do 
olympic_running |> 
  ggplot(aes(x=Year, y = Time, colour = Sex)) +
  geom_line() +
  geom_point(size = 1) +
  facet_wrap(~ Length, scales = "free_y", nrow = 2) + 
  theme_minimal() + 
  scale_color_brewer(palette = "Dark2") + 
  theme(legend.position = "bottom", legend.title = element_blank()) +
  labs(title = "Olympic running times",
       y="Seconds")

## PBS --------------------------------------------------------------

?PBS
PBS 
  # 4 Keys [336 time series = 84 (ATC2) * 2 (Concession) * 2 (Type-patient) ]
  # 2 variables/measures
84*2*2

PBS |> View()

PBS |> distinct(ATC1)
PBS |> distinct(ATC2)
PBS |> distinct(Concession, Type)

PBS |> tail()

PBS |>
  filter(ATC2 == "A10") 

PBS |> 
  filter(ATC2=="A10") |>
  select(Cost) # Notice Keys and Index are automatically selected

PBS |>
  filter(ATC2 == "A10") |> 
  select(Cost) |>
  filter_index(~"1991 Jul")

  # Let's sum Cost of A10 across Concession and Type, i.e., total A10 cost   
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10

a10

# Time Series patterns ----------------------------------------

a10 |> 
  autoplot() + #Cost
  labs(
    title = "Australian antidiabetic drug sales",
    y = "$ (millions)" 
    )
  # Note - it will pick the first variable it sees - hence select appropriate variable 

  # I could do it with ggplot()
a10 |> 
  ggplot(aes(x=Month, y=Cost)) +
  geom_line() + 
  labs(
    title = "Australian antidiabetic drug sales",
    y = "$ (millions)"
    )

a10 |> 
  autoplot() + 
  geom_point() + 
  labs(
    title = "Australian antidiabetic drug sales",
    y = "$ (millions)"
    ) 

aus_production |>
  filter(year(Quarter) >= 1980) |>
  autoplot(Electricity) +
  labs(
    title = "Australian electricity production",
    y = "GWh"
    )

us_employment |>
  filter(Title == "Retail Trade", year(Month) >= 1980) |>
  autoplot(Employed / 1e3) +
  labs(
    y = "Million people",
    title = "Retail employment, USA"
    )


## Google, Apple, Facebook, Amazon
gafa_stock |>
  filter(Symbol == "AMZN", year(Date) >= 2018) |>
  autoplot(Close) +
  labs(
    title = "Amazon closing stock price", 
    y="$"
    ) 
  # No seasonality in stock prices - especially if market is efficient
  # Seems no trend or cycle because only looking at 1-year
  # Change year(Date) >= 2014 to see what happens


## Explore the pelt tsibble
pelt #fur
help(pelt)
?pelt #https://en.wikipedia.org/wiki/Hudson%27s_Bay_Company
pelt |> autoplot(Lynx)

  # A bit of a plotting trick/lesson that may be useful later on
pelt |> autoplot()

  # Add some axis labels and title
pelt |>
  pivot_longer(2:3, names_to = "Animal")  |> 
  autoplot() +  
  labs(title="Lynx eating Hare",
       y="No. of animals")
  # Note this is annual data - talk about population cycle
  # Lynx eating Hare 


# ACF  -------------

## Aus bricks - you have seen this in the video
aus_production |>
  filter(year(Quarter) >= 1979) |> 
  autoplot(Beer) +
  labs(
    title = "Australian beer production",
    y="megalitres"
  ) 

aus_production |>
  filter(year(Quarter) >= 1979) |> 
  gg_season(Beer)

aus_production |>
  filter(year(Quarter) >= 1979) |> 
  gg_lag(Beer)

aus_production |>
  filter(year(Quarter) >= 1979) |> 
  gg_lag(Beer, geom = "point", lags = 1:12)

aus_production |>
  filter(year(Quarter) >= 1979) |> 
  ACF(Beer, lag_max = 20)

aus_production |>
  filter(year(Quarter) >= 1979) |> 
  ACF(Beer) |> 
  autoplot()


## Contrast this with Brick production
aus_production |>
  autoplot(Bricks) +
  labs(
    title = "Australian clay brick production",
    y="million units"
  ) 

aus_production |>
  ACF(Bricks, lag_max = 48) |> 
  autoplot()

## An even more pronounced trend

a10 |> 
  autoplot() + 
  geom_point() + 
  labs(
    title = "Australian antidiabetic drug sales",
    y = "$ (millions)"
  ) 

a10 |> 
  ACF(Cost, lag_max = 48) |> 
  autoplot()

## Lynx ACF 
pelt |> autoplot(Lynx)

pelt |> 
  ACF(Lynx) |> 
  autoplot()
# you'll see plenty of examples in the tutes. 

# Your turn  ----------------------------------------------------------

## Snowy mountains tourism --------------------------------------------

snowy <- tourism |>
  filter(Region == "Snowy Mountains") 

# Notice there are 4 purposes of travel
# sum over all four of them before you continue

# Use autoplot(), gg_season(), gg_subseries()
# gg_lag(), ACF() to explore feature of the time series



## Snowy (more on tourism) ----------------------------------------------------- 
  # Or in the same way

dat <- filter(tourism, 
              Region %in% c("Snowy Mountains","Gold Coast")
              ) |>
  group_by(Region) |> 
  summarise(Trips=sum(Trips))

dat |> autoplot(Trips) 
dat |> autoplot(Trips) + geom_point()
  # Very different patterns between the two
  # Let's see it better

dat |> gg_season(Trips,labels="right")
  # Very different patterns between the two 

dat |> gg_subseries(Trips)

## Australian state tourism
  # We explored this in the lecture video
  # If we have time we will go through it
  # Just quickly show plotting on the log-scale
  # skip the rest

holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

  # original scale
holidays |> autoplot(Trips) +
  labs(
    title= "Australian domestic holiday nights",
    y = "thousands of trips", x= "Year"
    )
  
  # log scale
holidays |> autoplot(Trips) +
  scale_y_log10()+
  labs(
    title= "Australian domestic holiday nights",
    y = "thousands of trips", x= "Year"
  )

  # let's filter the top three
top3_tourism <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  filter(State %in% c("New South Wales", "Victoria", "Queensland")) |>
  summarise(Trips = sum(Trips)) 


top3_tourism |> autoplot(Trips) +
  labs(
    title= "Australian domestic holiday nights",
    y = "thousands of trips", x= "Year"
    )


  # Show these and comment here - slides are a little too squashed 
top3_tourism |> gg_season(Trips) + 
  labs(
    title= "Australian domestic holiday nights",
    y = "thousands of trips", x= "Year"
    )

top3_tourism |>
  gg_subseries(Trips) + 
  labs(
    title= "Australian domestic holiday nights",
    y = "thousands of trips", x= "Year"
  )

# WHITE NOISE ------------------------------------------

seq_len(36)
1:36
rnorm(36)

set.seed(6)

wn <- tsibble(t = 1:36, y = rnorm(36), index = t)

wn |> autoplot() + labs(title = "White noise")
wn |> ACF() |> autoplot()

set.seed(6)
wn <- tsibble(t = seq_len(3600), y = rnorm(3600), 
              index = t) 
wn |> autoplot() + labs(title = "White noise")
wn |> ACF() |> autoplot()

set.seed(6)
wn <- tsibble(t = seq_len(3600), y = rnorm(3600,mean=100,sd=100), 
              index = t)
autoplot(wn)+ggtitle("White noise")
ACF(wn) |> autoplot()


# Your turn  ------------------------------------------------------

## Differencing google closing price ------------------------------

dgoog <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index=trading_day, regular=TRUE) |>
  mutate(diff = difference(Close))

# Pigs -------------------------------------------------------------

pigs <- aus_livestock |>
  filter(State == "Victoria",
         Animal == "Pigs",
         year(Month) >= 2014)

pigs |> autoplot(Count/1e3) +
  labs(title = "Number of pigs slaughtered in Victoria",
       y="Thousands") 

pigs |> gg_season()
pigs |> ACF() |> autoplot()


