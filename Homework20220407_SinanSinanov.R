# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.
#####Problem 1#####
# 1.Download the stock prices for AMZN, FB, NFLX, stocks from 2019-01-01 
# to 2021-04-01. Keep only the symbol/date/adjusted columns.

data<-tidyquant::tq_get(c("AMZN","FB","NFLX"),
                        from = "2019-01-01",
                        to = "2021-04-01") %>%
  dplyr::select(symbol, date, adjusted)

# 2.Add all the missing dates(such as 2019-01-01), so that we have 
# observations for every single date. Fill in the missing values for adjusted 
# with the last non-missing observation.

dates<-base::data.frame(Dates = base::rep(base::seq.Date(from = lubridate::ymd("2019-01-01"),
                                                         to = lubridate::ymd("2021-04-01"),
                                                         by = "day"),3),
                        Symbol = c(base::rep("AMZN",822), base::rep("NFLX",822), base::rep("FB",822)))


Final <- dates %>%
  dplyr::left_join(data, by = c("Dates" = "date", "Symbol" = "symbol"))%>%
  dplyr::group_by(Symbol)%>%
  tidyr::fill(adjusted, .direction = "downup")

# 3.Create a new data frame, which consist only of stocks from AMZN or FB and 
# has observations from 2019-01-01 to 2019-07-01 or 2020-04-01 to 2020-07-01. 
# Arrange the data frame first by the symbol name and by the date in 
# descending order.

ResultS <- Final %>% 
  dplyr::filter((Dates >= lubridate::ymd("2019-01-01") & Dates <= lubridate::ymd("2019-07-01")) |
                  (Dates >= lubridate::ymd("2020-04-01") & Dates <= lubridate::ymd("2020-07-01")),
                Symbol %in% c("AMZN", "FB")) %>%
  dplyr::arrange(Symbol, dplyr::desc(Dates))

# 4.Select the first and last observation of the aforementioned dataframe
# for each of the two stocks - AMZN and FB.

select <- ResultS %>%
  dplyr::group_by(Symbol)%>%
  dplyr::slice(c(1, n()))%>%
  dplyr::ungroup()

# 5.Select the last observation for each stock, for each month. 
# In order to do this, first create a new column, which will show you the 
# year and the month. You can do this using the functions substr() or floor_date.

select2 <- Final %>%
  dplyr::mutate(DatesNew = base::substr( Dates , 1, 7)) %>%
  dplyr::group_by(Symbol, DatesNew) %>%
  dplyr::slice_tail()%>%
  dplyr::ungroup()


#####Problem 2#####
#Use the dataframe from problem 1.2.
# Use the SMA function from the tidyquant package to calculate the 10day SMA 
# and the 26 day SMA for each of the 3 stocks. 
# How many times did the 10 day SMA line cross 26 day SMA line from below? 
# How many times did the 10 day SMA line cross 26 day SMA line from above?
# You can take a look at this article: https://www.investopedia.com/trading/macd/
# Essentially by cross from above/below I want you to find the buy/sell signals.
#####Problem 2#####

SMA <- Final %>%
  dplyr::mutate(sma10 =TTR::SMA(adjusted, n = 10),
                sma26 =TTR::SMA(adjusted, n = 26),
                LagSma10 = dplyr::lag(sma10),
                LagSma26 = dplyr::lag(sma26)) %>%
  dplyr::filter(!is.na(sma26)) %>%
  dplyr::mutate(cross = dplyr::case_when(LagSma10 > LagSma26 & sma10 < sma26 ~ "from above",
                                         LagSma10 < LagSma26 & sma10 > sma26 ~ "from below",
                                         TRUE ~ "no cross"))