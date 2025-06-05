#real-time abruptshift detection with the moving average:
#Input:
##df:           A data.frame which include a column about temperature:
##window_lower: The lower bound of 
##n_end:        End value for the real-time abrupt shift detection.
##
rt_abruptshiftdetection_wrapper <- function(df, window_lower = 5, window_higher = 35, n_end = 2000,
                                            warmup_period = 200, moving_average = 30
){
  #
  df_output <- data.frame(
    temperature = df1$temperature,
    detected_value_rt
  )
  df1 <- read.csv(filename)
  df_output <- data.frame(
    temperature = df1$temperature[1:n_end],
    detected_value_rt = numeric(n_end),
    detected_offline = numeric(n_end)
  )
  ts1 <- ts(df$temperature[1:warmup_period])
  detect1 <- as_detect(ts1, lowwl = 5, highwl = 10)
  df_output$detected_value_rt[1:warmup_period] <- detect1
  for(i in (warmup_period+1):n_end){
    ts1 <- ts(df1$temperature[1:i])
    detect1 <- as_detect(ts1, lowwl = window_lower, highwl = window_higher)
    df_output$detected_value_rt[i] <- mean(detect1[(i-moving_average+1):i])
  }
  #offline detection result:
  ts1 <- ts(df1$temperature[1:n_end])
  df_output$detected_offline[1:n_end] <- as_detect(ts1, lowwl = window_lower, highwl = window_higher)
  df_output <- df_output[1:n_end, ]
  return(df_output)
}
#Generate the alerting result, the earliest time that can detect thermal runaway - we record the first time point that the real-time moving average detected value larger than 0.8:
generate_alert <- function(df_output, thres = 0.8, time_interval = 0.1){
  index_selected <- which(df_output$detected_value_rt > thres)
  return(index_selected * time_interval)
}
#drawing the detection diagnostic plot:
draw_plot <- function(df_output, filename){
  maximal_indice <- which(df_output$detected_offline == max(df_output$detected_offline))[1]
  start_indice <- max(1, maximal_indice - 150)
  end_indice <- min(maximal_indice + 100, nrow(df_output))
  png(paste(filename, "rt_detected_value_moving_average.png", sep = "_"), width = 800, height = 600)
  plot(df_output$detected_value_rt[start_indice:end_indice], type = "l", col = "black", ylab = "detected_value",
       main = paste(original_file, "moving_average"))
  par(new = TRUE)
  n_points <- end_indice - start_indice + 1
  plot(c(1:n_points),df1$temperature[start_indice:end_indice], type = "l", col = "red", 
       axes = FALSE, xlab = "", ylab = "")
  axis(side = 4)
  mtext("temperature", side = 4, col="blue", cex = 1.5)
  dev.off()     
}
