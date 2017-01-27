#' Load and parse real field data from github storage
#' –азбираемс€ с проблемой отображени€ UTF

#' @export
getSensorData1 <- function() {
  # забираем данные по сенсорам в новом формате из репозитори€
  # на выходе либо данные, либо NA в случае ошибки

  callingFun = as.list(sys.call(-1))[[1]]
  calledFun = deparse(sys.call()) # as.list(sys.call())[[1]]

  # получаем исторические данные по погоде из репозитори€ √арика --------------------------------------------------------
  # https://cran.r-project.org/web/packages/curl/vignettes/intro.html
  resp <- try({
    curl_fetch_memory("https://github.com/iot-rus/Moscow-Lab/raw/master/result_lab.txt")
  })

  # browser()
  # проверим только 1-ый элемент класса, поскльку при разных ответах получаетс€ разное кол-во элементов
  if(class(resp)[[1]] == "try-error" || resp$status_code != 200) {
    # http://stackoverflow.com/questions/15595478/how-to-get-the-name-of-the-calling-function-inside-the-called-routine
    flog.error(paste0("Error in ", calledFun, " called from ", callingFun, ". Class(resp) = ", class(resp)))
    # в противном случае мы сигнализируем о невозможности обновить данные
    return(NA)
  }

  # ответ есть, и он корректен. ¬ этом случае осуществл€ем пребразование
  temp.df <- read_delim(rawToChar(resp$content),
                        delim = ";",
                        quote = "\"",
                        # дата; врем€; им€; широта; долгота; минимум (0% влажности); максимум (100%); текущие показани€
                        col_names = c(
                          "date",
                          "time",
                          "rawname",
                          "type",
                          "lat",
                          "lon",
                          "yl",
                          "xl",
                          "yr",
                          "xr",
                          "measurement",
                          "pin"
                        ),
                        col_types = "Dccc????????",
                        locale = locale("ru", encoding = "windows-1251", tz = "Europe/Moscow"),
                        # таймзону, в принципе, можно установить здесь
                        progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5

  problems(temp.df)

  # расчитываем необходимые данные
  df <- temp.df %>%
    # линейна€ нормализаци€
    mutate(value = yl + (yr-yl)/(xr-xl) * (measurement - xl), type = factor(type)) %>%
    # получим временную метку
    mutate(timestamp = ymd_hms(paste(date, time), truncated = 3, tz = "Europe/Moscow")) %>%
    # упростим им€ сенсора
    mutate(label = gsub(".*:", "", rawname, perl = TRUE)) %>%
    # и разделим на им€ и адрес
    separate(label, c('ipv6', 'name'), sep = '-', remove = TRUE) %>%
    select(timestamp, name, type, value, measurement, lat, lon, pin) %>%
    mutate(location = "Moscow Lab")


  # 2. постпроцессинг дл€ разных типов датчиков
  flog.info(paste0(calledFun, " - sensors data from GitHub recieved. Last records:"))
  flog.info(capture.output(print(head(arrange(df, desc(timestamp)), n = 4))))

  # 3. частный построцессинг
  # постпроцессинг дл€ датчиков влажности

  df %<>% postprocess_ts_field_data()

  #    browser()
  df
}
