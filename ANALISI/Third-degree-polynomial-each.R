###THIRD-DEGREE POLYNOMIAL
plot_ekc_country <- function(df, country_name, confidence = FALSE) {
  
  df_country <- subset(df, country == country_name)
  
  
  if (nrow(df_country) == 0) {
    warning(paste("Nessun dato trovato per il paese:", country_name))
    return(NULL)
  }
  
  # Modello
  model <- lm(co2 ~ gdp + gdp2 + gdp3, data = df_country)
                    #####RISULTATI#####
  lista_paesi <- split(df, df$country)
  
  risultati <- lapply(lista_paesi, function(d) {
    lm(co2 ~ gdp + gdp2 + gdp3, data = d)
  })
  
  lapply(risultati, summary)
  for (c in names(risultati)) {
  cat("###", c, "###\n")
  print(summary(risultati[[c]]))
  cat("\n\n")
}
  
  
  # GDP
  gdp_seq <- seq(min(df_country$gdp), max(df_country$gdp), length.out = 300)
  new_data <- data.frame(
    gdp = gdp_seq,
    gdp2 = gdp_seq^2,
    gdp3 = gdp_seq^3
  )
  if (confidence) {
    pred <- predict(model, newdata = new_data, interval = "confidence")
    fit <- pred[, "fit"]
    lwr <- pred[, "lwr"]
    upr <- pred[, "upr"]
  } else {
    fit <- as.numeric(predict(model, newdata = new_data))
  }
  
  # Grafico 
  plot(gdp_seq, fit, type = "l", lwd = 2, col = "blue",
       xlab = "GDP per capita(log)", ylab = "CO2 per capita(log)",
       main = paste("EKC -", country_name))
  grid()
  
}
plot_ekc_country(df, "Brazil")
plot_ekc_country(df, "Brazil", confidence = TRUE)
for (c in unique(df$country)) {
  plot_ekc_country(df, c)
  readline(prompt = "Premi [Invio] per continuare al grafico successivo...")
}
