#Funzione per calcolare i turning points e convertirli in scala reale
calculate_turning_points_real <- function(b1, b2, b3) {
  a <- 3 * b3
  b <- 2 * b2
  c <- b1
  
  discriminant <- b^2 - 4 * a * c
  
  if (discriminant < 0) {
    return("No real turning points (complex roots)")
  } else {
    root1_log <- (-b + sqrt(discriminant)) / (2 * a)
    root2_log <- (-b - sqrt(discriminant)) / (2 * a)
    
    #da log a scala reale
    root1_real <- exp(root1_log)
    root2_real <- exp(root2_log)
    
    return(data.frame(
      log_GDP = sort(c(root1_log, root2_log)),
      real_GDP = sort(c(root1_real, root2_real))
    ))
  }
}




# Brazil
calculate_turning_points_real(b1 = 26.03040, b2 = -3.01195, b3 = 0.11700)
# Russia
calculate_turning_points_real(b1 = 22.47924, b2 = -2.58224, b3 = 0.09856)
# India
calculate_turning_points_real(b1 = 5.09131, b2 = -0.67903, b3 = 0.03320)
# China
calculate_turning_points_real(b1 = -6.388302, b2 = 0.922448, b3 = -0.041079)
# South Africa
calculate_turning_points_real(b1 = -67.7999, b2 = 7.8610, b3 = -0.3028)




