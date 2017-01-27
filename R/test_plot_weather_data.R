#' Генерация стекированных графиков.
#' Разбираемся с проблемой отображения UTF

#' @export
testplotWeatherData <- function(weather_df, rain_df, timeframe) {
  # timeframe -- [POSIXct min, POSIXct max]
  # агрегат осадков за сутки
  # чтобы график нарисовался столбиками строго по дням, необходимо пропущенные дни добить нулями
  dft <- tibble(date=seq.Date(as.Date(timeframe[1]), as.Date(timeframe[2]), by="1 day"))

  df2 <- dft %>%
    left_join(rain_df, by="date") %>%
    mutate(rain=if_else(is.na(rain), 0, rain)) %>%
    select(date, rain) %>%
    mutate(timestamp=force_tz(with_tz(as.POSIXct(date), tz="GMT"), tz="Europe/Moscow")) %>%
    filter(timestamp>=timeframe[1]) %>%
    filter(timestamp<=timeframe[2])

  # погода
  df <- weather_df %>%
    filter(timegroup>=timeframe[1]) %>%
    filter(timegroup<=timeframe[2])

  lims <- timeframe
  # схлопнем рисование графика
  ## brewer.pal.info
  # https://www.datacamp.com/community/tutorials/make-histogram-ggplot2
  pp <- ggplot(df) +
    # ggtitle("График температуры") +
    # scale_fill_brewer(palette="Set1") +
    # scale_fill_brewer(palette = "Paired") +
    # geom_ribbon(aes(ymin = temp.min, ymax = temp.max, fill = time.pos), alpha = 0.5) +
    # geom_point(shape = 1, size = 3) +
    # geom_line(lwd = 1, linetype = 'dashed', color = "red") +
    scale_x_datetime(labels=date_format("%d.%m", tz="Europe/Moscow"),
                     breaks=date_breaks("1 days"),
                     #minor_breaks = date_breaks("6 hours"),
                     limits=lims) +
    theme_igray() +
    theme(legend.position="none",
          axis.title.y=element_text(vjust=0)
    ) +
    geom_vline(xintercept=as.numeric(now()), linetype="dotted", color="yellowgreen", lwd=1.1) +
    xlab("Дата")

  p1 <- pp +
    geom_line(aes(timegroup, temp, colour=time.pos), lwd=1.2) +
    scale_color_manual(values=brewer.pal(n=9, name="Oranges")[c(3, 7)]) +
    ylab("Температура,\n град. C")
  p2 <- pp +
    geom_line(aes(timegroup, humidity, colour=time.pos), lwd=1.2) +
    scale_color_manual(values=brewer.pal(n=9, name="Blues")[c(4, 7)]) +
    ylim(0, 100) +
    ylab("Влажность\nвоздуха, %")
  # по просьбе Игоря даем сдвижку к столбику + 12 часов для попадания столбика ровно в сутки
  p3 <- pp +
    geom_bar(data=df2 %>% mutate(timestamp=timestamp + hours(12)),
             aes(timestamp, rain), fill=brewer.pal(n=9, name="Blues")[4], alpha=0.5, stat="identity") +
    ylim(0, NA) +
    ylab("Осадки\n(дождь), мм")

  # grid.arrange(p1, p2, p3, ncol=1) # возвращаем ggplot
  grid.newpage()
  #grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size="first"))
  rbind(ggplotGrob(p1), ggplotGrob(p2), ggplotGrob(p3), size="first")

}
