##second-degree polynomial

library(plm)

df <- ANALISI
anni <- length(unique(df$ANNO))
countries <- c("Brazil", "Russia", "India", "China", "South_Africa")
df$country <- rep(countries, each = anni)

names(df) <- c("year", "co2", "gdp", "gdp2", "gdp3", "country")

pdata <- pdata.frame(df, index = c("country", "year"))

model <- plm(
  co2 ~ gdp + gdp2 + factor(year),
  data = pdata,
  model = "within",
  effect = "twoways"
)

summary(model)

b <- coef(model)
b1 <- b["gdp"]
b2 <- b["gdp2"]


gdp_seq <- seq(min(df$gdp, na.rm = TRUE), max(df$gdp, na.rm = TRUE), length.out = 900)
co2_pred <- b1 * gdp_seq + b2 * gdp_seq^2

plot(gdp_seq, co2_pred, type = "l", lwd = 5, col = "red",
     xlab = "GDP per capita(log)", ylab = "CO2 per capita(log)",
     main = "EKC")
grid()

