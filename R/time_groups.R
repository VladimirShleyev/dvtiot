#' Different functions to group measurements with seconds resolution into high ordered time intervals
#' @name tgroups
NULL

#' @rdname tgroups
#' @export
hgroup.enum0 <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток +-1/2 интервала, к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  tick_time <- date + minutes(time.bin * 60)/2 # сдвигаем на пол интервала вперед
  n <- floor(hour(tick_time) / time.bin)
  floor_date(tick_time, unit = "day") + hours(n * time.bin)
}

#' @rdname tgroups
#' @export
hgroup.enum1 <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00
  tick_time <- date
  n <- floor(hour(tick_time) / time.bin)
  floor_date(tick_time, unit = "day") + hours(n * time.bin)
}

#' @rdname tgroups
#' @export
hgroup.enum <- function(date, time.bin = 4){
  # привязываем все измерения, которые попали в промежуток [0, t] к точке измерения.
  # точки измерения могут быть кратны 1, 2, 3, 4, 6, 12 часам, определяется time.bin
  # отсчет измерений идет с 0:00

  # поправка для лаборатории. для группировки меньше часа допускается указывать числа меньше 1
  # 0.5 -- раз в полчаса.0.25 -- раз в 15 минут

  tick_time <- date
  if (time.bin < 1 & !(time.bin %in% c(0.25, 0.5))) time.bin = 1
  n <- floor((hour(tick_time)*60 + minute(tick_time))/ (time.bin * 60))
  floor_date(tick_time, unit = "day") + minutes(n * time.bin *60)
}
