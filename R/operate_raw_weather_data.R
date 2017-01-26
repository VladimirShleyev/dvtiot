#' Load, calc, combine and filter history weather data & prediction
#' @name opweather
NULL

#' @rdname opweather
#' @export
gatherRawWeatherData <- function() {
  # получаем из гитхаба предобработанные исторические данные по погоде -----------------
  # пока мы не детализируем, как эти данные получены, прямой ли загрузкой, либо через фоновый git pull и открытие файла
  # на выходе либо данные, либо NA в случае ошибки

  history_url <- "https://raw.githubusercontent.com/iot-rus/agri-iot-data/master/weather_history.csv"
  # %>% '[['("result")

  resp <- safely(read_csv)(history_url)
  if(!is.null(resp$error)){
    flog.error(resp$error)
    return(NA)
  }
  weather_hist <- resp$result %>%
    select(-starts_with("human_")) # удалим все данные-дубликаты, предназначенные для человеческого представления
  flog.debug("History weather data loaded successfully")

  # получаем прогноз через API --------------------------------------------------------
  reqstring <- paste0("api.openweathermap.org/data/2.5/",
                      "forecast?id=",
                      '524901', # MoscowID
                      "&APPID=",
                      '19deaa2837b6ae0e41e4a140329a1809') # "weather?id="

  resp <-  safely(GET)(reqstring)

  if(!is.null(resp$error)){
    flog.error(resp$error)
    return(NA)
  }
  flog.debug("Predicted weather data loaded successfully")

  # парсим погодные данные
  m <- content(resp$result)$list

  # заменили на FP подход
  ll <- m %>%
    map(function(x){
      ldate <- getElement(x, 'main')
      ldate$timestamp <- getElement(x, 'dt')
      # мм осадков за предыдущие 3 часа (Rain volume for last 3 hours, mm)
      # http://openweathermap.org/forecast5#parameter
      ldate$rain3h <- getElement(x, 'rain')[['3h']]
      ldate
    })

  l2 <- melt(ll)
  # нормализуем под колонки, которые есть в исторических данных
  l3 <- tidyr::spread(l2, L2, value) %>%
    select(-L1, -temp_kf) %>%
    mutate(timestamp = as.integer(timestamp))

  # объединяем и вычищаем --------------------------------------------------------

  weather_df <- weather_hist %>%
    mutate(rain3h=NA) %>% # зимой дождя может не быть, а колонка нужна
    bind_rows(l3) %>%
    select(-temp_max, -temp_min, -sea_level, -grnd_level) %>%
    distinct() %>% # удаляем дубли, которые навыдавал API
    mutate(temp = round(temp - 273.15, 1)) %>% # пересчитываем из кельвинов в градусы цельсия
    mutate(pressure = round(pressure * 0.75006375541921, 0)) %>% # пересчитываем из гектопаскалей (hPa) в мм рт. столба
    mutate(humidity = round(humidity, 0)) %>%
    mutate(timestamp = as.POSIXct(timestamp, origin='1970-01-01')) %>%
    mutate(timegroup = hgroup.enum(timestamp, time.bin = 1)) %>% # сделаем почасовую группировку
    # разметим данные на прошлое и будущее. будем использовать для цветовой группировки
    mutate(time.pos = if_else(timestamp < now(), "PAST", "FUTURE"))

  weather_df
}

#' @rdname opweather
#' @export
extractWeather <- function(raw_weather, timeframe) {
  # timeframe -- [POSIXct min, POSIXct max]
  # для устранения обращений к внешним источникам, теперь на вход
  # получаем предварительно скомпонованные предобработанные данные
  # raw_weather <- combineRawWeatherData()

  # browser()
  # причешем данные для графика у Паши + проведем усреднение по часовым группам
  # есть нюансы, связанные с выдачей данных из прогноза.
  # rain3h соотв. прогнозу осадков в мм, на предыдущих три часа
  # за консистентность информации (нарезка тиков 3-х часовыми интервалами) отвечает API.
  # поэтому что mean, что sum -- все одно. timegroup для каждого прогнозного измерения должна быть ровно одна
  res_DF <- raw_weather %>%
    filter(timegroup >= timeframe[1]) %>%
    filter(timegroup <= timeframe[2]) %>%
    group_by(timegroup, time.pos) %>%
    summarise(temp = mean(temp),
              pressure = mean(pressure),
              humidity = mean(humidity),
              rain3h_av = mean(rain3h)) %>%
    ungroup

  # чтобы график не был разорванным, надо продублировать максимальную точку из PAST в группу FUTURE
  POI_df <- res_DF %>%
    filter(time.pos == 'PAST') %>%
    filter(timegroup == max(timegroup)) %>%
    mutate(time.pos = 'FUTURE')

  res_DF <- res_DF %>%
    bind_rows(POI_df) %>%
    arrange(timegroup)

  res_DF
}

#' @rdname opweather
#' @export
calcRainPerDate <- function(raw_weather) {
  # считаем осадки за сутки ------------------------------
  # timestamp temp.min pressure humidity precipitation temp.max     temp           timegroup
  #    (time)    (dbl)    (dbl)    (dbl)         (dbl)    (dbl)    (dbl)              (time)

  # полагаем, что идентичность выпавших осадков с точностью до третьего знака просто означает дублирование показаний!!!!
  dfw0 <- raw_weather %>%
    select(timestamp, rain3h) %>%
    filter(!is.na(rain3h)) %>% # записи без дождя нас вообще не интересуют
    distinct() %>% # полностью дублирующиеся записи также неинтересны
    # mutate(date = lubridate::date(timestamp)) %>%
    mutate(date=as.Date(timestamp)) %>%
    group_by(date, rain3h) %>% # собираем агрегаты по суткам, а потом по повторяющимся значениям,
    # может быть погрешность по переходам через сутки,
    # но при группировке по значениям можем случайно объединить данных с разных дат
    # в каждой группе посчитаем временную протяженность события
    arrange(timestamp) %>%
    mutate (dtime=as.numeric(difftime(timestamp, min(timestamp), unit="min")))

  # теперь мы можем проверить, чтобы максимальное значение в группе не превышало 180 мин (3 часа)
  # поглядел на данные, таких групп нет за месяц не нашел, решил пока для простоты забить
  dfw1 <- dfw0 %>%
    # в каждой группе выберем значение с минимальным временем измерения
    filter(timestamp==min(timestamp)) %>% # см. допущение об идентичности показаний
    ungroup() %>%
    arrange(timestamp)

  # а теперь посчитаем агрегаты по суткам
  dfw2 <- dfw1 %>%
    select(-dtime) %>%
    group_by(date) %>%
    summarise(rain=sum(rain3h)) %>% # пытаемся высчитать агрегат за сутки
    ungroup %>%
    mutate(rain=as.numeric(rain)) %>% # но если все будет NA, то надо явно привести к NA_real_
    #mutate(timegroup = as.numeric(as.POSIXct(date, origin='1970-01-01'))) %>%
    mutate(human_timestamp=force_tz(with_tz(as.POSIXct(date), tz = "GMT"), tz = "Europe/Moscow")) %>%
    mutate(timestamp=as.numeric(human_timestamp)) %>%
    arrange(date)

  flog.info("Rain calculation finished")
  flog.info(capture.output(print(dfw2)))

  dfw2
}
