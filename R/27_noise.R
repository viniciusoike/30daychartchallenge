library(dplyr)
library(ggplot2)
library(patchwork)

N <- 2500

dat <- tibble(
  y = rnorm(N),
  x = seq_along(y)
)

colors <- c("gray15")
offwhite <- "#f5f5f4"
font <- "DIN Alternate"

theme_plot <- theme_minimal(base_family = font) +
  theme(
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.margin = margin(20, 10, 5, 10),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = 2),
    plot.title = element_text(hjust = 0, size = 22),
    axis.text = element_text(color = "#6B6865"),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )

theme_title <- theme(
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.margin = margin(20, 10, 5, 10),
    plot.title = element_text(hjust = 0, size = 22, family = font),
  )

p1 <- ggplot(dat, aes(x, y)) +
  geom_line(linewidth = 0.8, color = "#114787") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(breaks = seq(-4, 4, 2), limits = c(-4, 4)) +
  labs(x = NULL, y = NULL, subtitle = "Gaussian white noise") +
  theme_plot +
  theme(
    axis.text.x = element_text(size = 12)
  )

p2 <- ggplot(dat, aes(y, lag(y))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  stat_bin2d(color = "white", linewidth = 0.01, binwidth = 0.25) +
  guides(fill = "none") +
  scale_x_continuous(limits = c(-4, 4)) +
  scale_y_continuous(limits = c(-4, 4)) +
  scale_fill_gradient(high = "#669bbc", low = "#003049") +
  labs(x = expression(y[t]), y = expression(y[t - 1]), subtitle = "Lag plot (t-1)") +
  theme_plot

# p3 <- ggplot(dat, aes(1, y)) +
#   geom_boxplot(fill = colors, alpha = 0.6) +
#   scale_y_continuous(limits = c(-4, 4)) +
#   coord_flip() +
#   theme_plot +
#   theme(
#     axis.text.y = element_blank(),
#     axis.title = element_blank()
#   )

p4 <- ggplot(dat) +
  geom_histogram(aes(x = y, y = after_stat(density)), position = "identity", bins = 50, color = "white", fill = "#114787") +
  geom_hline(yintercept = 0) +
  stat_function(fun = dnorm, colour = colors, xlim = c(-4, 4), linewidth = 1) +
  scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-4, 4)) +
  labs(x = NULL, y = NULL, subtitle = "Histogram") +
  theme_plot

yacf <- acf(ts(dat$y, frequency = 12), lag.max = 48, plot = FALSE)
num_acf <- yacf$acf[-1, , 1]

dat_acf <- tibble(
  x = seq_along(num_acf),
  y = num_acf
)

p5 <- ggplot(dat, aes(sample = y)) +
  geom_qq(shape = 21, alpha = 0.25) +
  geom_qq_line(color = "#114787", linewidth = 1) +
  labs(x = NULL, y = NULL, subtitle = "QQ plot") +
  theme_plot +
  theme(
    panel.grid.major.x = element_line(linetype = 1)
  )

ci <- qnorm((1 + 0.95)/2)/sqrt(N)

p6 <- ggplot(dat_acf, aes(x, y)) +
  geom_col(width = 0.1) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = c(-ci, ci), linetype = 2, color = "#114787") +
  scale_x_continuous("Frequency", breaks = seq(0, 84, 12), labels = 0:7) +
  scale_y_continuous(limits = c(-ci - 0.005, ci + 0.005), breaks = seq(-0.03, 0.03, 0.03)) +
  labs(y = NULL, subtitle = "ACF") +
  theme_plot

p7 <- ggplot(dat, aes(x, y^2)) +
  geom_line(color = "#114787") +
  geom_hline(yintercept = 0) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  labs(x = NULL, y = NULL, subtitle = expression(paste("Squared series (", y^2, ")"))) +
  theme_plot +
  theme(
    axis.text.x = element_text(size = 12)
  )

panel <- p1 / (p4 | p2) / (p5 | p6) / p7

panel <- panel +
  plot_annotation(title = "No signal in this noise", subtitle = "Various plots illustrating statistical properties of Gaussian white noise: ") &
  theme_title

gg


cars <- rbcb::get_series(1378, as = "ts")

cars <- log(cars)

carspec <- bspec::bspec(diff(cars, 12))

expectation(bspec(cars, priorscale = 0.6, priordf = 2))


dat_cars <- tibble(
  date = zoo::as.Date.ts(time(cars)),
  cars = zoo::coredata(cars)
  )

stl_cars <- forecast::mstl(cars, s.window = 27, robust = TRUE)

ts_to_tibble <- function(ts) {

  dplyr::tibble(
    date = zoo::as.Date.ts(stats::time(ts)),
    as.data.frame(zoo::coredata(ts)),
    .name_repair = janitor::make_clean_names
  )

}

dat_stl <- ts_to_tibble(stl_cars)
dat_cars <- ts_to_tibble(cars)

ggplot(dat_cars, aes(date, cars)) +
  geom_line()

ggplot(dat_stl, aes(date, trend)) +
  geom_line()

ggplot(dat_stl, aes(date, seasonal12)) +
  geom_line()

ggplot(dat_stl, aes(date, remainder)) +
  geom_line()

forecast::autoplot(stl_cars)

holt <- HoltWinters(cars, seasonal = "additive")

model_cars <- forecast::tbats(cars, use.trend = TRUE)

tbats_cars <- forecast::tbats.components(model_cars)

dat_tbats <- tibble(
  date = zoo::as.Date.ts(time(tbats_cars)),
  as.data.frame(zoo::coredata(tbats_cars))
)

forecast::autoplot(tbats_cars)

dat_hw <- as_tibble(as.data.frame(holt$fitted))

dat_holt <- tibble(
  date = zoo::as.Date.ts(time(holt$fitted)),
  as.data.frame(zoo::coredata(holt$fitted))
)

dat <- inner_join(dat_cars, dat_holt)

ggplot(dat, aes(date, cars)) +
  geom_line(linewidth = 0.8, color = "#114787")

ggplot(dat, aes(date, level)) +
  geom_line(linewidth = 0.6, color = "black")

ggplot(dat, aes(date, season)) +
  geom_line(linewidth = 0.6)

ggplot(dat, aes(date, trend)) +
  geom_line()



ggplot(dat_cars, aes(x = date, y = cars)) +
  geom_line()

ggplot(dat_holt) +
  geom_line(aes(x = date, y = level))

ggplot(dat_holt) +
  geom_line(aes(x = date, y = trend))

ggplot(dat_holt) +
  geom_line(aes(x = date, y = season))



str(dat_holt)

names(dat_holt)

zoo::as.Date.ts(time(holt$fitted))

dat_hw$date <- dat_cars$date


ggplot(dat_hw, aes())

dat <- as_tibble(as.data.frame(stl_cars))
plot.ts(dat$Remainder)
Box.test(dat$Remainder, lag = 24, fitdf = 3, type = "Ljung")



ggplot(stl_cars, aes())


kalman <- StructTS(cars, type = "level")

plot(cars)
lines(tsSmooth(kalman), lty = 3, col = "red")
lines(fitted(kalman), lty = 2)

plot(residuals(kalman))

plot(kalman$fitted)

plot(tsSmooth(kalman))

series <- rbcb::get_series(
  c("cars" = 1378, "electric_energy" = 1403),
  start_date = "1980-01-01"
  )

energy <- ts(log(series$electric_energy$electric_energy), start = c(1980, 1), frequency = 12)

forecast::ggseasonplot(energy)

plot(forecast::mstl(energy))

ggplot(series$electric_energy, aes(date, electric_energy)) +
  geom_line()

ggplot(series$cars, aes(date, log(cars))) +
  geom_line()

xcar <- ts(log(series$cars$cars), start = c(1980, 1), frequency = 12)



