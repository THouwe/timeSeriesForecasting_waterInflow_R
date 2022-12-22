####################################################################################
############################# CHANGE ME: ###########################################
####################################################################################
myDir <- ('C:/Users/Houwelin2/Desktop/SDG_homework_TS/')


####################################################################################
#### check requirements
if(!require("ggplot2")){
  install.packages("ggplot2")
  library("ggplot2")
}

if(!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}

####

if(!require("zoo")){
  install.packages("zoo")
  detach("zoo")
}
if(!require("lubridate")){
  install.packages("lubridate")
  detach("lubridate")
}
if(!require("forecast")){
  install.packages("forecast")
  detach("forecast")
}
if(!require("fable")){
  install.packages("fable")
  detach("fable")
}
if(!require("scales")){
  install.packages("scales")
  detach("scales")
}
if(!require("stats")){
  install.packages("stats")
  detach("stats")
}
if(!require("readr")){
  install.packages("readr")
  detach("readr")
}


####################################################################################
#### setup
myDir <- ('C:/Users/Houwelin2/Desktop/SDG_homework_TS/')
df <- read.csv(paste0(myDir, 'flow (1).csv'))
id <- unique(df$id_sector)
for (i in 1:length(id)) {
  outDir <- paste0(myDir,'sector_', as.character(id[i]), '/')
  
  d <- df[df$id_sector == id[i], ]
  d$utc <- as.POSIXct(d$ts_measurement, tz = "UTC")
  
  
  ####################################################################################
  #### create zoo object (period = 15 min)
  z <- zoo::zoo(d$flow, as.POSIXct(d$ts_measurement, tz = "UTC"))
  reg <- seq(min(d$utc), max(d$utc), by = "15 min")
  zr <- zoo::zoo(1:length(z), reg)
  zm <- merge(zr, z, all = TRUE)
  
  
  ####################################################################################
  #### Agrregate over hours
  zh <-
    aggregate(zm$z,
              by = lubridate::floor_date(zoo::index(zm), unit = "1 hour"),
              FUN = mean)
  
  DFh <- data.frame(time = zoo::index(zh),
                    values = zoo::coredata(zh))
  
  
  ####################################################################################
  #### plot data
  ggplot(DFh, aes(time, values)) +
    geom_line() +
    xlab("Time (year)") + ylab("Water Flow (o.u.)") +
    ggtitle(paste0("Sector ", as.character(id[i]), ": Hourly Data"))
  
  
  ####################################################################################
  #### interpolate outliers
  out <- forecast::tsoutliers(zh)
  if (i == 5 || i == 6) {
    zh[out$index] <- out$replacements
  }
  
  
  ####################################################################################
  #### trim
  idxna <-
    which(is.na(zoo::coredata(zh))) 
  naseq <-
    unname(tapply(idxna, cumsum(c(1, diff(
      idxna
    )) != 1), range)) 
  
  seqLen <- replicate(nrow(naseq), 0)
  for (j in 1:nrow(naseq)) {
    seqLen[j] <- naseq[[j]][2] - naseq[[j]][1]
  }
  
  if (i == 1) {
    N <- 1000
  } else if (i == 2) {
    N <- 200
  }
  
  length(seqLen[seqLen > N]) 
  which(seqLen > N) 

  if (i == 1) {
    zh <- zh[-(1:naseq[[26]][2])]
  } else if (i == 2) {
    zh <- zh[-(1:naseq[[163]][2])]
  }
  
  
  ####################################################################################
  #### plot trimmed data
  DFh <- data.frame(time = zoo::index(zh),
                    values = zoo::coredata(zh))
  
  pdf(
    file = paste0(outDir, 'sector_', as.character(id[i]), '_hourlyDataTrimmed.pdf'),
    width = 4,
    height = 4
  )
  
  ggplot(DFh, aes(time, values)) +
    geom_line() +
    xlab("Time (year)") + ylab("Water Flow (o.u.)") +
    ggtitle(paste0("Sector ", as.character(id[i]), ": Hourly Data Trimmed"))
  
  dev.off()

  
  ####################################################################################
  #### replace missing values with mean over previous and successive values
  # (when p ossible, otherwise use na.approx)
  coreVals <- zoo::coredata(zh)
  idxna2 <- which(is.na(coreVals))
  naseq2 <-
    unname(tapply(idxna2, cumsum(c(1, diff(
      idxna2
    )) != 1),
    range)) # get ranges of missing values segments
  vals2replace <- replicate(nrow(naseq2),-99)
  for (j in 1:nrow(naseq2)) {
    if (naseq2[[j]][1] > 4 * 24 * 5 &&
        naseq2[[j]][2] < length(coreVals) - (4 * 24 * 5)) {
      IDXstart1 <- naseq2[[j]][1] - (4 * 24 * 5)
      IDXend1 <- naseq2[[j]][1] - 1
      IDXstart2 <- naseq2[[j]][2] + 1
      IDXend2 <- naseq2[[j]][2] + (4 * 24 * 5)
      
      vals2replace[j] <- mean(c(coreVals[IDXstart1:IDXend1],
                                coreVals[IDXstart2:IDXend2]),
                              na.rm = TRUE)
    }
  }
  
  TS <- zoo::na.approx(zh)
  for (j in 1:length(vals2replace)) {
    if (vals2replace[j] != -99) {
      # here we replace with better estimates (when possible)
      # print('replacing...') # very slow computing
      startIDX <- naseq2[[j]][1]
      endIDX <- naseq2[[j]][2]
      TS[startIDX:endIDX] <- vals2replace[j]
    }
  }
  
  
  ####################################################################################
  #### inspect data
  # 1. Compute moving averages to estimate variance components and look for long-term trends
  flow_m_01h <- zoo::rollmean(TS, 1, fill = NA)
  flow_m_03h <- zoo::rollmean(TS, 3, fill = NA)
  flow_m_01d <- zoo::rollmean(TS, 24, fill = NA)
  flow_m_03d <- zoo::rollmean(TS, 24 * 3, fill = NA)
  flow_m_05d <- zoo::rollmean(TS, 24 * 5, fill = NA)
  flow_m_01w <- zoo::rollmean(TS, 24 * 7, fill = NA)
  flow_m_03w <- zoo::rollmean(TS, 24 * 21, fill = NA)
  flow_m_01m <- zoo::rollmean(TS, 24 * 30, fill = NA)
  flow_m_01y <- zoo::rollmean(TS, 24 * 365, fill = NA)
  
  
  # convert to data frame to plot with ggplot
  dayTime <- zoo::index(TS)
  values <- zoo::coredata(TS)
  ma_01h <- zoo::coredata(flow_m_01h)
  ma_03h <- zoo::coredata(flow_m_03h)
  ma_01d <- zoo::coredata(flow_m_01d)
  ma_03d <- zoo::coredata(flow_m_03d)
  ma_05d <- zoo::coredata(flow_m_05d)
  ma_01w <- zoo::coredata(flow_m_01w)
  ma_03w <- zoo::coredata(flow_m_03w)
  ma_01m <- zoo::coredata(flow_m_01m)
  ma_01y <- zoo::coredata(flow_m_01y)
  
  DF <- data.frame(
    dayTime,
    values,
    ma_01h,
    ma_03h,
    ma_01d,
    ma_03d,
    ma_05d,
    ma_01w,
    ma_03w,
    ma_01m,
    ma_01y
  )
  
  
  ####################################################################################
  #### plot MAs:
  pdf(
    file = paste0(outDir, 'sector_', as.character(id[i]), '_MAs.pdf'),
    width = 4,
    height = 4
  )
  
  ggplot(DF, aes(dayTime)) +
    geom_line(aes(y = values, colour = "real")) +
    # geom_line(aes(y=ma_01h,colour="1h MA")) +
    geom_line(aes(y = ma_01d, colour = "1d MA")) +
    # geom_line(aes(y=ma_03d,colour="3d MA")) +
    geom_line(aes(y = ma_01w, colour = "1w MA")) +
    geom_line(aes(y = ma_01m, colour = "1m MA")) +
    geom_line(aes(y = ma_01y, colour = "1y MA")) +
    xlab("Time (year)") + ylab("Water Flow (o.u.)") +
    ggtitle(paste0("Sector ", as.character(id[i]), ": Data & Moving Averages (MA)"))
  
  dev.off()


  ####################################################################################  
  # 2. Search for higher-frequency seasonalities by looking at the autocorrelation at short lags:
  pdf(
    file = paste0(outDir, 'sector_', as.character(id[i]), '_ACF_2wk.pdf'),
    width = 4,
    height = 4
  )
  
  AC <- acf(zoo::coredata(TS), plot = FALSE, lag.max = 24 * 15)
  plot(
    AC,
    xaxt = "n",
    main = paste0("Sector ", as.character(id[i]), ": Autocorrelation Plot"),
    xlab = "Lag (days)"
  )
  xtick <- seq(0, 24 * 15, by = 24)
  axis(side = 1,
       at = xtick,
       labels = 0:15)
  
  dev.off()

  
  ####################################################################################
  #### convert to tsibble and check for inconsistencies:
  df_ts <- data.frame(values, dayTime)
  df_tsib <- tsibble::as_tsibble(df_ts)
  tsibble::has_gaps(df_ts %>% tsibble::as_tsibble(., index = dayTime)) # Luckily not
  tsibble::scan_gaps(df_ts %>% tsibble::as_tsibble(., index = dayTime))
  
  # visualize:
  df_tsib %>%
    autoplot(values)
  
  
  ####################################################################################
  #### model with fable:
  
  # if enough resources (multicore needed) run the following:
  # library("future")
  # install.packages("future.apply")
  # plan(multisession)
  # fit_ts <- df_ts %>% as_tsibble(., index = dayTime) %>%
  #   model(arima = ARIMA(values ~
  #                         season("day") + season("week") + season("year")))
  
  # else...
  fit_ts <- df_ts %>% tsibble::as_tsibble(., index = dayTime) %>%
    fabletools::model(arima = fable::ARIMA(values))
  
  
  ####################################################################################
  #### get descriptives:
  # fit_ts %>%
  #   fabletools::glance()
  
  fit_ts %>%
    fabletools::report()
  
  # fit_ts %>%
  #   fabletools::augment()
  
  
  ####################################################################################
  #### predict:
  library("fable") # finally safe to load library
  fc <- fit_ts %>%
    forecast(h = "1 week")
  
  
  ####################################################################################
  #### extract CIs:
  fc_ci <- hilo(fc, level = c(80, 95))
  fc_80 <- unpack_hilo(fc_ci,"80%")
  fc_80_95 <- unpack_hilo(fc_80,"95%")
  
  
  ####################################################################################
  #### plot predictions:
  pdf(file = paste0(outDir,'sector_',as.character(id[i]),'_forecast_07gg.pdf'),
      width = 4, height = 4)
  
  fc %>%
    autoplot(df_tsib)
  
  dev.off()

  
  ####################################################################################
  #### save data, model, and predictions:
  save(
    df_ts,
    fit_ts,
    fc,
    file = paste0(
      outDir,
      'sector_',
      as.character(id[i]),
      '_model_and_predictions.RData'
    )
  )
  
  readr::write_csv(fc_80_95,
                   paste0(outDir,'sector_',as.character(id[i]),'_predictions.csv'))
  
}
