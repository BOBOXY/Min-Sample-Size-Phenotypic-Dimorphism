# read data
Quantile0.95_matrix <-
  read_rds(file = "HouXudong//Results Var_Meandiff//Quantile0.95_matrix(with NA) 2024.3.8.rds") %>% 
  mutate(Mean_diff = round(Mean_diff,digits = 2))

# clean data
Quantile0.95_matrix_Location_mean_15_30 <- 
  Quantile0.95_matrix %>% 
  filter(Location_mean == 15|Location_mean == 20|Location_mean == 25|Location_mean == 30) %>% 
  mutate(Location_mean = factor(Location_mean,
                                levels = c(15,20,25,30),
                                ordered = TRUE))

Quantile0.95_matrix_Mean_diff_3_5 <- 
  Quantile0.95_matrix %>% 
  filter(Mean_diff == 3.0|Mean_diff == 3.4|Mean_diff == 3.8|Mean_diff == 4.2|Mean_diff == 4.6|Mean_diff == 5.0) %>% 
  mutate(Mean_diff = factor(Mean_diff,
                            levels = c(3.0,3.4,3.8,4.2,4.6,5.0),
                            ordered = TRUE))

# Plot
P_Location_mean_Mean_diff_3_5 <- 
  ggplot(data = Quantile0.95_matrix_Mean_diff_3_5,
         mapping = aes(x = Location_mean,
                       y = Sample_size,
                       # fill = DI,
                       color = Mean_diff)) +
  geom_line(alpha = 1,
            linewidth = 0.2) +
  geom_point(size = 0.1,
             shape = 16,
             alpha = 0.9) +
  labs(x = "Abs mean",
       y = "Sample Size",
       color = "Mean diff") +
  scale_x_continuous(
    # trans = "log10",
    breaks = c(15,20,25,30),
    expand = c(0.05,0)) +
  scale_y_continuous(
    # trans = "log10",
    breaks = c(0,250,500,750,1000),
    limits = c(0,1010),expand = c(0.05,0)) +
  # scale_color_discrete() +
  scale_color_brewer(type = "div",
                     palette = "RdYlBu") +
  theme_classic() +
  theme(axis.line = element_line(linewidth = 0.1,color = "#333333")) +
  theme(axis.title.x = element_text(size = 8,family = "ARL",color = "#000000"), # axis.title.x代表x轴标题
        axis.title.y = element_text(size = 8,family = "ARL",color = "#000000"),
        axis.text.x = element_text(size = 6,family = "ARL",color = "#333333"), # axis.text.x则表示x轴刻度标签
        axis.text.y = element_text(size = 6,family = "ARL",color = "#333333")) +
  theme(axis.ticks = element_line(linewidth = 0.1,color = "#333333"),
        axis.ticks.length = unit(0.1,"cm")) + # 坐标轴刻度线的设置
  theme(legend.title = 
          element_text(size = 8,family = "ARL",color = "#000000",face = "plain"), # 修改图例标题外观
        legend.text = 
          element_text(size = 6,family = "ARL",color = "#000000"), # 修改图例标签
        legend.position = "right",# 修改图例位置
        legend.spacing = unit(0,"mm"),
        legend.key.width = unit(3,"mm"),
        legend.key.height = unit(3,"mm")) + # 图例大小
  theme(legend.position = "NA") + # 不显示图例
  theme(plot.margin = unit(c(1,1,1,1),"lines")) + # 边界距离上右下左 单位inch或者lines
  theme(aspect.ratio = 1) +
  # guides(color = guide_legend(ncol = 2)) +
  annotate("text",x = -Inf,y = Inf,label = "B",family = "ARL",color = "#000000",size = 6/.pt,hjust = -1,vjust = 1)
print(P_Location_mean_Mean_diff_3_5)

P_Mean_diff_Location_mean_15_30 <- 
  ggplot(data = Quantile0.95_matrix_Location_mean_15_30,
         mapping = aes(x = Mean_diff,
                       y = Sample_size,
                       # fill = DI,
                       color = Location_mean)) +
  geom_line(alpha = 1,
            linewidth = 0.2) +
  geom_point(size = 0.1,
             shape = 16,
             alpha = 0.9) +
  labs(x = "Mean differnece",
       y = "Sample Size",
       color = "Abs mean") +
  scale_x_continuous(
    # trans = "log10",
    breaks = c(2,3,4,5),
    expand = c(0.05,0)) +
  scale_y_continuous(
    # trans = "log10",
    breaks = c(0,250,500,750,1000),
    limits = c(0,1010),expand = c(0.05,0)) +
  # scale_color_discrete() +
  scale_color_brewer(type = "div",
                     palette = "RdYlBu") +
  theme_classic() +
  theme(axis.line = element_line(linewidth = 0.1,color = "#333333")) +
  theme(axis.title.x = element_text(size = 8,family = "ARL",color = "#000000"), # axis.title.x代表x轴标题
        axis.title.y = element_text(size = 8,family = "ARL",color = "#000000"),
        axis.text.x = element_text(size = 6,family = "ARL",color = "#333333"), # axis.text.x则表示x轴刻度标签
        axis.text.y = element_text(size = 6,family = "ARL",color = "#333333")) +
  theme(axis.ticks = element_line(linewidth = 0.1,color = "#333333"),
        axis.ticks.length = unit(0.1,"cm")) + # 坐标轴刻度线的设置
  theme(legend.title = 
          element_text(size = 8,family = "ARL",color = "#000000",face = "plain"), # 修改图例标题外观
        legend.text = 
          element_text(size = 6,family = "ARL",color = "#000000"), # 修改图例标签
        legend.position = "right",# 修改图例位置
        legend.spacing = unit(0,"mm"),
        legend.key.width = unit(3,"mm"),
        legend.key.height = unit(3,"mm")) + # 图例大小
  theme(legend.position = "NA") + # 不显示图例
  theme(plot.margin = unit(c(1,1,1,1),"lines")) + # 边界距离上右下左 单位inch或者lines
  theme(aspect.ratio = 1) +
  # guides(color = guide_legend(ncol = 2)) +
  annotate("text",x = -Inf,y = Inf,label = "C",family = "ARL",color = "#000000",size = 6/.pt,hjust = -1,vjust = 1)
print(P_Mean_diff_Location_mean_15_30)

# Output plot
Plot_filename_Mean_diff_Location_mean <- 
  paste0("HouXudong//Results Var_Meandiff//2024.3.8 Results process//",
         "Samplesize_Mean_diff_Location_mean")

CairoPDF(file = paste0(Plot_filename_Mean_diff_Location_mean,"_B.pdf"),width = 2.5, height = 6)
showtext_begin()
print(P_Location_mean_Mean_diff_3_5)
showtext_end()
dev.off()

CairoPDF(file = paste0(Plot_filename_Mean_diff_Location_mean,"_C.pdf"),width = 2.5, height = 6)
showtext_begin()
print(P_Mean_diff_Location_mean_15_30)
showtext_end()
dev.off()
