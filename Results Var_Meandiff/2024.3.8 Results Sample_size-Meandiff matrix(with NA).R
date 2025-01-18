# Parameters series

# True_mean_S <- seq(from = 10,to = 20,by = 0.1)
True_sd_S <- 1
# True_mean_L <- seq(from = 10,to = 20,by = 0.1)
True_sd_L <- 1
# True_mean_L/True_mean_S-1
Mean_diff_half <- seq(from = 1,to = 2.5,by = 0.1)
Location_mean <- seq(from = 15,to = 30,by = 1)
# True_sd_L/True_sd_S[1]
# SD_ratio <- 1.5
Pop_ratio <- seq(from = 1,to = 5,by = 0.1)
Skewness_gamma <- seq(-0.95,0.95,by = 0.05)

Mean_com <- 
  tibble(Location_mean_col = rep(Location_mean,times = length(Mean_diff_half)),
         Mean_diff_half_col = rep(Mean_diff_half,each = length(Location_mean))) %>% 
  mutate(True_mean_S_col = Location_mean_col - Mean_diff_half_col,
         True_mean_L_col = Location_mean_col + Mean_diff_half_col,
         Mean_diff_col = Mean_diff_half_col * 2,
         .before = Location_mean_col)

# Define the times of repetition
RepTimes <- 500

# Define the sample size
sample_size <- c(seq(from = 10,to = 90,by = 10),
                 seq(from = 100,to = 180,by = 20),
                 seq(from = 200,to = 1000,by = 50)) # Set the sample size for each sample

# Parameters setting
# Group 1:length(SD_ratio)
# M1 = 1;True_mean_S[M1]
# M2 = 1;True_mean_L[M2]
# True_mean_L[M2]/True_mean_S[M1]-1
# S1 = 1;True_sd_S[S1]
# S2 = 1;True_sd_L[S2]
SK = 20;Skewness_gamma[SK]
PR = 1;Pop_ratio[PR]
Test_on_mod0_1 = TRUE
Test_on_mod0_2 = FALSE

# Define different Quantile matrix
# 50%
# Quantile0.5_matrix <- tibble(Sample_size = sample_size)
# 95%
Quantile0.95_matrix <- tibble(Sample_size = 0,
                              True_mean_s = 0,
                              True_mean_l = 0,
                              Location_mean = 0,
                              Mean_diff = 0)
# 99%
# Quantile0.99_matrix <- tibble(Sample_size = sample_size)

# Compute sample size, when 95%P = 0.05
library(LearnGeom)
Model <- function(Vect){
  i = 31
  repeat{
    if(Vect[i] <= 0.05&
       Vect[i-1]>0.05){
      P1 <- c(sample_size[i],Vect[i])
      P2 <- c(sample_size[i-1],Vect[i-1])
      Line1 <- CreateLinePoints(P1, P2)
      P3 <- c(0,0.05)
      P4 <- c(1,0.05)
      Line2 <- CreateLinePoints(P3, P4)
      intersection <- IntersectLines(Line1, Line2)
      return(intersection[1])
      break
    }
    if(i == 2&
       Vect[i] <= 0.05&
       Vect[i-1] <= 0.05){
      return(10)
      break
    }
    if(i == 2){
      return(NA)
      # return(1001)
      break
    }
    i = i-1
  }
}

# Define quantile function
Quantile0.95 <- function(Vect){
  return(quantile(Vect,0.95))
}
# Quantile0.99 <- function(Vect){
#   return(quantile(Vect,0.99))
# }

# Compute model matrix
Loop_counter <- 0
for (Row_num in 1:nrow(Mean_com)) {
  # Row_num = 10 # Test
  
  True_mean_S <- Mean_com$True_mean_S_col[Row_num]
  True_mean_L <- Mean_com$True_mean_L_col[Row_num]
  
  path_name <- paste0("HouXudong//Results Var_Meandiff//2024.3.8 Results matrix//",
                      "M_S_",round(True_mean_S,3),
                      "_M_L_",round(True_mean_L,3),
                      ".xlsx")
  Pvalue_mod0_1 <- readxl::read_xlsx(path = path_name,
                                     sheet = 1)
  Pvalue_vect <- apply(as.matrix(Pvalue_mod0_1[,-1]),2,Quantile0.95)
  # Add new data
  Quantile0.95_matrix <- add_row(Quantile0.95_matrix,
                                 Sample_size = Model(Pvalue_vect),
                                 True_mean_s = True_mean_S,
                                 True_mean_l = True_mean_L,
                                 Location_mean = Mean_com$Location_mean_col[Row_num],
                                 Mean_diff = Mean_com$Mean_diff_col[Row_num])
  Loop_counter <- Loop_counter+1
  print(Loop_counter)
}
print(Loop_counter)
Quantile0.95_matrix <- Quantile0.95_matrix[-1,]

write_rds(Quantile0.95_matrix,
          file = "HouXudong//Results Var_Meandiff//Quantile0.95_matrix(with NA) 2024.3.8.rds")
# Quantile0.95_matrix <- 
#   read_rds(file = "HouXudong//Results Var_Meandiff//Quantile0.95_matrix(with NA) 2024.3.8.rds")

write_xlsx(Quantile0.95_matrix,
           path = "HouXudong//Results Var_Meandiff//Quantile0.95_matrix(with NA) 2024.3.8.xlsx")

