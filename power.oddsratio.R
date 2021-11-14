##Sabin Gaire
#BIOS 5510 final project
#I have included extra credit portion "alternative"

power.oddsratio <- function(OR, N,
                           p1, alpha = 0.05, r = 1, alternative = "two.sided", plot = FALSE, ...){
  
  OR_0 <- 1
  
  switch(alternative,
                      "two.sided" = { y <- "2 i.e OR != 1" },
                      "less" = { y <- "1 i.e OR < 1" },
                      "greater" = { y <- "1 i.e OR > 1" },
  )
  
  #Print out the header
  cat("Pearson Chi-square Test for Odds Ratio\n","\n",
      "Fixed Scenario Elements\n",
      "-----------------------\n",
      format("\t", width = 44), "Value\n",
      format("Distribution",width = 50), "Asymptotic normal\n",
      format("Method", width = 50), "Normal Approximation\n",
      format("Reference (Group 1) Proportion", width = 50), p1, "\n",
      format("Number of Sides",width =50), y, "\n",
      format("Null Odds Ratio",width =50) ,"1", "\n",
      format("Alpha ", width =50), alpha, "\n",
      format("Group 1 Weight",width =50), round(1 / ( r + 1), digits = 3), "\n",
      format("Group 2 Weight", width =50),round(1- (1 / ( r + 1)), digits = 3), "\n",
      sep= ""
      
  )
  cat("\n",
      "Computed Power\n",
      "--------------\n")
  
  #Creating a empty dataframe of all arguments and power
  new_data <- data.frame(OR = double(),
                         N = double(),
                         p1 =double(),
                         alpha = double(),
                         r = double(),
                         alternative= character(),
                         Power = double())
  
  
  n_length <- length(N)
  or_length <- length(OR)
  #Using for loop to go through all combinations of OR and N
  for (i in seq_len(n_length)){
    for(j in seq_len(or_length)){
      
      p2 <- OR[j] * p1 /(1- p1 + OR[j] * p1)
      p10 <- p1
      p20 <- OR_0 * p10 / (1 - p10 + OR_0 * p10)
      p0 <- p20 - p10 
      n_firstgroup <- N[i]/(1+r)
      n_secondgroup <- (r*N[i] )/ (1+r)
      w1 <- n_firstgroup/ N[i]
      w2 <- n_secondgroup / N[i]
      z_value_two <- qnorm(1 - alpha/2)
      z_value_one <- qnorm(1 - alpha)
      #Checking for three alternatives
      if ( alternative == "two.sided"){
        power1_num_partial <- (w1 * p1 + w2 * p2) * (1 - w1 * p1 - w2 * p2)
        power1_num <- (p2 - p1 - p0) * sqrt(N[i] * w1 * w2)- z_value_two * sqrt(power1_num_partial)
        power1_denom_partial <- w2 * p1 * (1 - p1) + w1 * p2 *(1 - p2)
        power1 <- power1_num/sqrt(power1_denom_partial)
        power2_num <- -(p2 - p1 -p0 ) * sqrt(N[i] * w1 * w2)- z_value_two * sqrt(power1_num_partial)
        power2 <- power2_num/sqrt(power1_denom_partial)
        power = pnorm(power1) + pnorm(power2)
      }
      if (alternative == "greater"){
        power1_num_partial <- (w1 * p1 + w2 * p2) * (1 - w1 * p1 - w2 * p2)
        power1_num <- (p2 - p1 - p0) * sqrt(N[i] * w1 * w2)- z_value_one * sqrt(power1_num_partial)
        power1_denom_partial <- w2 * p1 * (1 - p1) + w1 * p2 *(1 - p2)
        power1 <- power1_num/sqrt(power1_denom_partial)
        power = pnorm(power1)
        
      }
      if (alternative == "less"){
        power1_num_partial <- (w1 * p1 + w2 * p2) * (1 - w1 * p1 - w2 * p2)
        power1_num <- -(p2 - p1 - p0) * sqrt(N[i] * w1 * w2)- z_value_one * sqrt(power1_num_partial)
        power1_denom_partial <- w2 * p1 * (1 - p1) + w1 * p2 *(1 - p2)
        power1 <- power1_num/sqrt(power1_denom_partial)
        power = pnorm(power1)
      }
      new <- list(OR[j],(N[i]), p1, alpha, r, alternative, power)
      new_data[nrow(new_data) + 1,] <- new
      
    }
  }
  #Using plot and lines to create plot of Power against N
  print(new_data[,c("OR", "N", "Power")])
  if( plot == TRUE){
    new_data1 <- new_data[order(new_data$N),]
    uni_n <- sort(N)
    uni_OR <- OR
    plot(new_data1$N, new_data1$Power, xlab = "N", ylab = "Power", main =" Test for Odds Ratio",
         type = "n")
    
      for(i in seq_len(length(uni_OR))){
        Po_val <- new_data1$Power[seq(i, length(new_data1$Power), length(uni_OR))]
        lines(uni_n, Po_val, lty = i, type ="b", col =i, pch =i)
      }
      legend("topleft", legend = as.character(uni_OR),inset = 0.01,
             lty =1:length(uni_OR), col=1:length(uni_OR), title = "OR", pch = 1:length(uni_OR))
    
    
  }
  
  return(new_data)
}






