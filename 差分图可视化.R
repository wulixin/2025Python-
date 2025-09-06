#-----------------------------------------------
#
#        主要用途发现两个月之间差距的每个细分指标差
#
#-----------------------------------------------


remotes::install_github("hoxo-m/TheseusPlot")
library(dplyr)
library(nycflights13)

data <- flights |> 
  filter(!is.na(arr_delay)) |>
  mutate(on_time = arr_delay <= 0) |>  # Arrived on time
  left_join(airlines, by = "carrier") |>
  mutate(carrier = name) |>  # Convert carrier abbreviations to full names
  select(year, month, day, origin, dest, carrier, dep_delay, on_time)

data |> head()

data_Nov <- data |> filter(month == 11)
data_Dec <- data |> filter(month == 12)

data_Nov |> summarise(on_time_rate = mean(on_time)) |> pull(on_time_rate)

data_Dec |> summarise(on_time_rate = mean(on_time)) |> pull(on_time_rate)


library(TheseusPlot)

ship <- create_ship(data_Nov, data_Dec, y = on_time, labels = c("November", "December"))

ship$plot(origin)

ship$table(origin)

ship$plot_flip(carrier)

ship$plot_flip(carrier, n = 5)


ship$plot_flip(dep_delay)

ship$plot_flip(dep_delay, continuous = continuous_config(n = 5))




