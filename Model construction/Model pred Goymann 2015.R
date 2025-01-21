library(caret)

# read in data and examine structure
Quantile0.95_matrix_Mean_SD_Pop <-
  read_rds(file = "HouXudong//Results_matrix(with NA) Mean_SD_Pop for model construction.rds") %>% 
  filter(Mean_diff != 0) %>%  
  filter(!is.na(Sample_size)) %>% 
  # filter(Sample_size != 10) %>% 
  mutate(CD = (SD_s + SD_l)/Mean_diff) %>% 
  mutate(log_sample_size = log(Sample_size))

str(Quantile0.95_matrix_Mean_SD_Pop)

ggplot(data = Quantile0.95_matrix_Mean_SD_Pop,
       mapping = aes(x = CD,
                     y = log_sample_size)) +
  geom_point(aes(color = Sample_size))

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# apply normalization to entire data frame
Quantile0.95_matrix_Mean_SD_Pop_norm <- as.data.frame(lapply(Quantile0.95_matrix_Mean_SD_Pop, normalize))

# confirm that the range is now between zero and one
summary(Quantile0.95_matrix_Mean_SD_Pop_norm$log_sample_size)

# compared to the original minimum and maximum
summary(Quantile0.95_matrix_Mean_SD_Pop$log_sample_size)

# create training and test data
# Data Splitting
# 5 folds 5 times
# set.seed(123)
# trainIndex_MultiFolds <- createMultiFolds(Quantile0.95_matrix_Mean_SD_Pop_norm$log_sample_size,
#                                           k = 5,
#                                           times = 5)

library(neuralnet)

# create an unnormalize function to reverse the normalization
unnormalize <- function(x) {
  return(x * (max(Quantile0.95_matrix_Mean_SD_Pop$log_sample_size) -
                min(Quantile0.95_matrix_Mean_SD_Pop$log_sample_size)) +
           min(Quantile0.95_matrix_Mean_SD_Pop$log_sample_size))
}

Mean_SD_Pop_model <-
  read_rds(file =
             paste0("HouXudong//Model construction//Model ANN Mean_SD_Pop HPC//2024.04.08 Results//Model_construction_HPC_Mean_SD_Pop_ANN_Final_model_32//Mean_SD_Pop_model_32 Final model.rds"))

# visualize the network topology
# plot(Mean_SD_Pop_model)

# Load test data
data_Goymann_2015 <- 
  readxl::read_xlsx(path =
                      paste0("Dataset in literature//Goymann 2015//Goymann_2015.xlsx"),
                    na = "NA") %>% 
  dplyr::mutate(Sex = factor(Sex,
                             levels = c("male", "female"),
                             ordered = T))

# Preparation of package
library(multimode)
set.seed(123)
ACR <- 
  modetest(data_Goymann_2015$body_mass,mod0 = 1,method = "ACR",B = 10000,
           submethod = NULL,n = NULL,tol = NULL,tol2 = NULL,gridsize = NULL)

# Extract raw data vect  
df_M <- subset(x = data_Goymann_2015,
               subset = (Sex == "male"))
df_F <- subset(x = data_Goymann_2015,
               subset = (Sex == "female"))

Vect_M <- na.omit(df_M$body_mass)
Vect_F <- na.omit(df_F$body_mass)

Mean_SD_Pop_Goymann_2015 <- 
  tibble(Mean_diff = round(mean(Vect_F) - mean(Vect_M),3),
         Mean_S = round(mean(Vect_M),3),
         Mean_L = round(mean(Vect_F),3),
         SD_S = round(sd(Vect_M),3),
         SD_L = round(sd(Vect_F),3),
         N_S = length(Vect_M),
         N_L = length(Vect_F),
         N = length(Vect_M) + length(Vect_F),
         Raw_pvalue = ACR[["p.value"]],
         # Pop_Ratio = round(length(Vect_F)/length(Vect_M),3),
         Pop_Ratio = 1) %>%
  dplyr::mutate(CD = (SD_S + SD_L)/Mean_diff,
                Abs_SD = sqrt(SD_S * SD_L),
                SDR = SD_L/SD_S,
                Pop_ratio = Pop_Ratio)

# apply normalization to entire data frame
df_Mean_SD_Pop_Goymann_2015 <- 
  Mean_SD_Pop_Goymann_2015 %>% 
  dplyr::select(c(11:14)) %>% 
  dplyr::mutate(CD = (CD - min(Quantile0.95_matrix_Mean_SD_Pop$CD)) / (max(Quantile0.95_matrix_Mean_SD_Pop$CD) - min(Quantile0.95_matrix_Mean_SD_Pop$CD)),
                Abs_SD = (Abs_SD - min(Quantile0.95_matrix_Mean_SD_Pop$Abs_SD)) / (max(Quantile0.95_matrix_Mean_SD_Pop$Abs_SD) - min(Quantile0.95_matrix_Mean_SD_Pop$Abs_SD)),
                SDR = (SDR - min(Quantile0.95_matrix_Mean_SD_Pop$SDR)) / (max(Quantile0.95_matrix_Mean_SD_Pop$SDR) - min(Quantile0.95_matrix_Mean_SD_Pop$SDR)),
                Pop_ratio = (Pop_ratio - min(Quantile0.95_matrix_Mean_SD_Pop$Pop_ratio)) / (max(Quantile0.95_matrix_Mean_SD_Pop$Pop_ratio) - min(Quantile0.95_matrix_Mean_SD_Pop$Pop_ratio))) %>% 
  as.data.frame()

# obtain model results
model_results <- compute(Mean_SD_Pop_model, df_Mean_SD_Pop_Goymann_2015)

# obtain predicted values
predicted_log_sample_size <- model_results$net.result

# calculate true sample size
Vect_log_sample_size_pred <- unnormalize(model_results$net.result)
Vect_sample_size_pred <- exp(Vect_log_sample_size_pred)

df_sample_size <-
  Mean_SD_Pop_Goymann_2015 %>% 
  dplyr::select(N, Raw_pvalue) %>% 
  dplyr::mutate(Sample_size_pred = Vect_sample_size_pred)

# Out put
write_xlsx(df_sample_size,
           path = "HouXudong//Model construction//Model pred Goymann 2015.xlsx")



