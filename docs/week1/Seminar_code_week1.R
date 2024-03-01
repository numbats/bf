library(fpp3)

# 1. I have already downloaded the excel file

# 2. Create a tsibble which is identical to the tourism tsibble from the tsibble package.

# Here is the tourism tsibble
tourism

my_tourism <- readxl::read_excel("tourism.xlsx") |>
  mutate(Quarter = yearquarter(Quarter)) |>
  as_tsibble(
    index = Quarter,
    key = c(Region, State, Purpose)
  )
my_tourism

# 3. Find what combination of Region and Purpose had
#    the maximum number of overnight trips on average.

my_tourism  |>
  as_tibble()  |>
  group_by(Region, Purpose) |>
  summarise(Trips = mean(Trips))  |>
  ungroup() |>
  filter(Trips == max(Trips))

# 4. Create a new tsibble which combines the Purposes and Regions,
#    and just has total trips by State.

state_tourism <- my_tourism |>
  group_by(State) |>
  summarise(Trips = mean(Trips)) |>
  ungroup()

state_tourism
