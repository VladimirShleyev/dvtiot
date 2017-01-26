#' Get appropriate timeframe window from current time.
#' Timeframe adjusted up to day unit and has forward and backward extension.

#' @export
getTimeframe <- function(days_back = 7, days_forward = 3){
  # если по каким-либо причинам наверху не определились с прогнозом (NA),
  # то полагаем что он есть и он равен базовому горизонту
  days_formard <- ifelse(is.na(days_forward), 3, days_forward)
  min_lim <- floor_date(now() - days(days_back), unit = "day")
  # поскольку будущее округляем вниз, то надо добавить еще сутки (+1)
  max_lim <- ceiling_date(now() + days(days_forward), unit = "day")
  timeframe <- c(min_lim, max_lim)

  timeframe
}
