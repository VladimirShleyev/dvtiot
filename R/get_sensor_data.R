#' Load and parse real field data from github storage
#' Разбираемся с проблемой отображения UTF

#' @export
getSensorData <- function() {
  # забираем данные по сенсорам в новом формате из репозитория
  # на выходе либо данные, либо NA в случае ошибки

  data_url <- "https://github.com/iot-rus/Moscow-Lab/raw/master/result_lab.txt"

  # дата; время; имя; широта; долгота; минимум (0% влажности); максимум (100%); текущие показания
  cnames <- c("date", "time", "rawname", "type", "lat", "lon", "yl", "xl", "yr", "xr", "measurement", "pin")

  resp <- purrr::safely(read_delim)(data_url,
                                    delim=";",
                                    quote="\"",
                                    col_names=cnames,
                                    col_types="Dccc????????",
                                    # таймзону, в принципе, можно установить здесь
                                    locale=locale("ru", encoding="windows-1251", tz="Europe/Moscow")
                                    #progress = interactive()
  ) # http://barryrowlingson.github.io/hadleyverse/#5)

  if(!is.null(resp$error)){
    flog.error(resp$error)
    return(NA)
  }

  # расчитываем необходимые данные
  df <- resp$result %>%
    # линейная нормализация
    mutate(value=yl + (yr-yl)/(xr-xl) * (measurement - xl), type=factor(type)) %>%
    # получим временную метку
    mutate(timestamp=lubridate::ymd_hms(paste(date, time), truncated=3, tz="Europe/Moscow")) %>%
    # упростим имя сенсора
    mutate(label=gsub(".*:", "", rawname, perl=TRUE)) %>%
    # и разделим на имя и адрес
    separate(label, c('ipv6', 'name'), sep='-', remove=TRUE) %>%
    select(timestamp, name, type, value, measurement, lat, lon, pin) %>%
    mutate(location="Moscow Lab")


  # постпроцессинг для датчиков влажности
  df %<>% markFieldData()

  df
}

getMoistureLevels <- function() {
  # определяем категории value, приведенных к диапазону [0; 100] с допущением 3.3 вольта в максимуме
  # в терминах этой же нормировки на максимально возможные 3.3 вольта

  levels <- list(category=c(2100, 2210, 2270, 2330, 2390, 2450, 2510) * 1000,
                 labels=c('WET++', 'WET+', 'WET', 'NORM', 'DRY', 'DRY+'))
  # возвращаем асинхронные списки: граница, именование
  # порядок менять крайне не рекомендуется -- порядок следования явно используется в других функциях
  levels
}

markFieldData <- function(df){
  # на вход получаем data.frame с временным рядом измерений
  # проводим постпроцессинг по масштабированию измерений, их категоризации и пр.

  # 3. частный построцессинг
  # датчики влажности
  levs <- getMoistureLevels()

  # browser()
  # пересчитываем value для датчиков влажности
  df %<>%
    # пока делаем нормировку к [0, 100] из диапазона 3.3 V грязным хаком
    mutate(value=ifelse(type == 'MOISTURE', measurement/1000, value)) %>%
    # и вернем для влажности в value редуцированный вольтаж
    # откалибруем всплески
    # кстати, надо подумать, возможно, что после перехода к категоризации, мы можем просто отсекать NA
    mutate(work.status = (type == 'MOISTURE' &
                            value >= head(levs$category, 1) &
                            value <= tail(levs$category, 1)))

  # если колонки с категориями не было создано, то мы ее инициализируем
  if(!('level' %in% names(df))) df$level <- NA
  df %<>%
    # считаем для всех, переносим потом только для тех, кого надо
    # превращаем в character, иначе после переноса factor теряется, остаются только целые числа
    mutate(marker=as.character(arules::discretize(value, method = "fixed",
                                                  categories = levs$category,
                                                  labels = levs$labels))) %>%
    mutate(level=ifelse(type == 'MOISTURE', marker, level)) %>%
    select(-marker)

  df
}
