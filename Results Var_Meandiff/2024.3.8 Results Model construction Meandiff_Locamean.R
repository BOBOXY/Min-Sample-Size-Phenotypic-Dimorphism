# Plot
Quantile0.95_matrix <-
  read_rds(file = "HouXudong//Results Var_Meandiff//Quantile0.95_matrix(with NA) 2024.3.8.rds")

Break_lines <- seq(0,1000,by = 100)
P_Meandiff_Locamean <- 
  ggplot(data = Quantile0.95_matrix,
         mapping = aes(x = Location_mean,
                       y = Mean_diff,
                       z = Sample_size)) +
  geom_tile(aes(fill = Sample_size)) +
  # geom_contour(aes(colour=..level..),
  #              breaks = 1000,
  #              alpha=1,
  #              color = "#000000",
  #              linewidth=0.2)+
  scale_x_continuous(expand = c(0.01,0)) +
  # scale_y_continuous(expand = c(0.01,0)) +
  # scale_x_continuous(limits = c(-0.1,1.1),expand=c(0,0)) + # 修改x轴连续性坐标起点
  scale_y_continuous(breaks = c(2,3,4,5),limits = c(1.5,5.5),expand = c(0,0)) + # 修改y轴连续性坐标起点
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,"Spectral")))(32),
                       breaks = c(10,100,250, 500, 750, 1000),
                       labels = c("10", "100", "250", "500", "750", "1000"),
                       guide  = guide_colourbar(ticks = T, 
                                                label = T,
                                                # nbin = 50,
                                                # barheight=.5, 
                                                barwidth = 1)) +
  
  labs(x = "Abs mean",
       y = "Mean difference",
       fill = "Sample size") +
  theme_classic() +
  theme(axis.line = element_line(linewidth = 0.1,color = "#333333"))+
  theme(axis.title.x = element_text(size = 12,family = "ARL",color = "#000000"), # axis.title.x代表x轴标题
        axis.title.y = element_text(size = 12,family = "ARL",color = "#000000"),
        axis.text.x = element_text(size = 10,family = "ARL",color = "#333333"), # axis.text.x则表示x轴刻度标签
        axis.text.y = element_text(size = 10,family = "ARL",color = "#333333")) +
  theme(axis.ticks = element_line(linewidth = 0.1,color = "#333333"),
        axis.ticks.length = unit(0.1,"cm")) + # 坐标轴刻度线的设置
  theme(legend.title = element_text(size = 8,family = "ARL",color = "#000000",face = "italic")) + # 修改图例标题外观
  theme(legend.text = element_text(size = 8,family = "ARL",color = "#000000")) + # 修改图例标签
  theme(legend.position = "right", # 修改图例位置
        legend.spacing = unit(0,"mm"),
        legend.direction = "vertical") +
  # theme(legend.key.size = unit(3,"mm")) + # 图例大小
  theme(plot.margin = unit(c(1,1,1,1),"lines")) + # 边界距离上右下左 单位inch或者lines
  theme(aspect.ratio = 1) +
  annotate("text",x = Inf,y = Inf,label = "",family = "ARL",color = "#000000",size = 8/.pt,hjust = 1.05,vjust = 2)
print(P_Meandiff_Locamean)

P_Meandiff_Locamean_width_4 <- 
  ggplot(data = Quantile0.95_matrix,
         mapping = aes(x = Location_mean,
                       y = Mean_diff,
                       z = Sample_size)) +
  geom_tile(aes(fill = Sample_size)) +
  # geom_contour(aes(colour=..level..),
  #              breaks = 1000,
  #              alpha=1,
  #              color = "#000000",
  #              linewidth=0.2)+
  scale_x_continuous(expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0.01,0)) +
  # scale_x_continuous(limits = c(-0.1,1.1),expand=c(0,0))+#修改x轴连续性坐标起点
  # scale_y_continuous(breaks=c(0,),limits = c(0.5,5),expand=c(0.05,0))+#修改y轴连续性坐标起点
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,"Spectral")))(32),
                       breaks = c(10,100,250, 500, 750, 1000),
                       labels = c("10", "100", "250", "500", "750", "1000"),
                       guide  = guide_colourbar(ticks = T, 
                                                label = T,
                                                # nbin = 50,
                                                # barheight=.5, 
                                                barwidth = 0.5)) +
  labs(x = "Abs mean",
       y = "Mean difference",
       fill = "Sample size") +
  theme_classic() +
  theme(axis.line = element_line(linewidth = 0.1,color = "#333333"))+
  theme(axis.title.x = element_text(size = 8,family = "ARL",color = "#000000"), # axis.title.x代表x轴标题
        axis.title.y = element_text(size = 8,family = "ARL",color = "#000000"),
        axis.text.x = element_text(size = 6,family = "ARL",color = "#333333"), # axis.text.x则表示x轴刻度标签
        axis.text.y = element_text(size = 6,family = "ARL",color = "#333333")) +
  theme(axis.ticks = element_line(linewidth = 0.1,color = "#333333"),
        axis.ticks.length = unit(0.1,"cm")) + # 坐标轴刻度线的设置
  theme(legend.title = element_text(size = 8,family = "ARL",color = "#000000",face = "italic")) + # 修改图例标题外观
  theme(legend.text = element_text(size = 6,family = "ARL",color = "#000000")) + # 修改图例标签
  theme(legend.justification = c(0,1),# 调整图例的基准点和相对位置 # 将图例的右上角(1,1)设为基准点
        legend.position = c(0.01-0.001,0.99+0.001),# 调整图例的相对位置
        # legend.position = "right", # 修改图例位置
        # legend.background = element_blank(),
        legend.background = element_rect(color = NA,
                                         fill = "white",
                                         # color = "#333333",
                                         # fill = NA,
                                         linewidth = 0.1,
                                         linetype = 0),
        legend.spacing = unit(0,"mm"),
        legend.direction = "vertical") +
  # theme(legend.position = "right", # 修改图例位置
  #       legend.spacing = unit(0,"mm"),
  #       legend.direction = "vertical") +
  theme(legend.key.height = unit(5,"mm"),
        legend.key.width = unit(2.5,"mm")) + # 图例大小
  theme(plot.margin = unit(c(1,1,1,1),"lines")) + # 边界距离上右下左 单位inch或者lines
  theme(aspect.ratio = 1) +
  annotate("text",x = Inf,y = Inf,label = "",family = "ARL",color = "#000000",size = 8/.pt,hjust = 1.05,vjust = 2)
print(P_Meandiff_Locamean_width_4)

P_Meandiff_Locamean_width_3 <- 
  ggplot(data = Quantile0.95_matrix,
         mapping = aes(x = Location_mean,
                       y = Mean_diff,
                       z = Sample_size)) +
  geom_tile(aes(fill = Sample_size)) +
  # geom_contour(aes(colour=..level..),
  #              breaks = 1000,
  #              alpha=1,
  #              color = "#000000",
  #              linewidth=0.2)+
  scale_x_continuous(expand = c(0.01,0)) +
  # scale_y_continuous(expand = c(0.01,0)) +
  # scale_x_continuous(limits = c(-0.1,1.1),expand=c(0,0)) + # 修改x轴连续性坐标起点
  scale_y_continuous(breaks = c(2,3,4,5),limits = c(1.5,5.5),expand = c(0,0)) + # 修改y轴连续性坐标起点
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,"Spectral")))(32),
                       breaks = c(10,100,250, 500, 750, 1000),
                       labels = c("10", "100", "250", "500", "750", "1000"),
                       guide  = guide_colourbar(ticks = T, 
                                                label = T,
                                                # nbin = 50,
                                                # barheight=.5, 
                                                barwidth = 0.5)) +
  labs(x = "Abs mean",
       y = "Mean difference",
       fill = "Sample size") +
  theme_classic() +
  theme(axis.line = element_line(linewidth = 0.1,color = "#333333"))+
  theme(axis.title.x = element_text(size = 8,family = "ARL",color = "#000000"), # axis.title.x代表x轴标题
        axis.title.y = element_text(size = 8,family = "ARL",color = "#000000"),
        axis.text.x = element_text(size = 6,family = "ARL",color = "#333333"), # axis.text.x则表示x轴刻度标签
        axis.text.y = element_text(size = 6,family = "ARL",color = "#333333")) +
  theme(axis.ticks = element_line(linewidth = 0.1,color = "#333333"),
        axis.ticks.length = unit(0.1,"cm")) + # 坐标轴刻度线的设置
  theme(legend.title = element_text(size = 8,family = "ARL",color = "#000000",face = "italic")) + # 修改图例标题外观
  theme(legend.text = element_text(size = 6,family = "ARL",color = "#000000")) + # 修改图例标签
  theme(legend.justification = c(0,1),# 调整图例的基准点和相对位置 # 将图例的右上角(1,1)设为基准点
        legend.position = c(0.01-0.001,0.99+0.001),# 调整图例的相对位置
        # legend.position = "right", # 修改图例位置
        # legend.background = element_blank(),
        legend.background = element_rect(color = NA,
                                         # fill = "white",
                                         # color = "#333333",
                                         fill = NA,
                                         linewidth = 0.1,
                                         linetype = 0),
        legend.spacing = unit(0,"mm"),
        legend.direction = "vertical") +
  # theme(legend.position = "right", # 修改图例位置
  #       legend.spacing = unit(0,"mm"),
  #       legend.direction = "vertical") +
  theme(legend.key.height = unit(5,"mm"),
        legend.key.width = unit(2.5,"mm")) + # 图例大小
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,1,1,1),"lines")) + # 边界距离上右下左 单位inch或者lines
  theme(aspect.ratio = 1) +
  annotate("text",x = Inf,y = Inf,label = "",family = "ARL",
           color = "#000000",size = 6/.pt,hjust = 1.05,vjust = 2)
print(P_Meandiff_Locamean_width_3)

P_Meandiff_Locamean_width_2.36525 <- 
  ggplot(data = Quantile0.95_matrix,
         mapping = aes(x = Location_mean,
                       y = Mean_diff,
                       z = Sample_size)) +
  geom_tile(aes(fill = Sample_size)) +
  # geom_contour(aes(colour=..level..),
  #              breaks = 1000,
  #              alpha=1,
  #              color = "#000000",
  #              linewidth=0.2)+
  scale_x_continuous(expand = c(0.01,0)) +
  scale_y_continuous(expand = c(0.01,0)) +
  # scale_x_continuous(limits = c(-0.1,1.1),expand=c(0,0)) + # 修改x轴连续性坐标起点
  # scale_y_continuous(breaks = c(2,3,4,5),limits = c(1.5,5.5),expand = c(0,0)) + # 修改y轴连续性坐标起点
  scale_fill_gradientn(colours = colorRampPalette(rev(brewer.pal(11,"Spectral")))(32),
                       breaks = c(10,100,250, 500, 750, 1000),
                       labels = c("10", "100", "250", "500", "750", "1000"),
                       guide  = guide_colourbar(ticks = T, 
                                                label = T,
                                                # nbin = 50,
                                                # barheight=.5, 
                                                barwidth = 0.5)) +
  labs(tag = "",
       x = "Abs mean",
       y = "Mean difference",
       fill = "Sample size") +
  theme_classic() +
  theme(axis.line = element_line(linewidth = 0.1,color = "#333333"))+
  theme(axis.title.x = element_text(size = 8,family = "ARL",color = "#000000"), # axis.title.x代表x轴标题
        axis.title.y = element_text(size = 8,family = "ARL",color = "#000000"),
        axis.text.x = element_text(size = 6,family = "ARL",color = "#333333"), # axis.text.x则表示x轴刻度标签
        axis.text.y = element_text(size = 6,family = "ARL",color = "#333333")) +
  theme(axis.ticks = element_line(linewidth = 0.1,color = "#333333"),
        axis.ticks.length = unit(0.1,"cm")) + # 坐标轴刻度线的设置
  theme(legend.title = element_text(size = 8,family = "ARL",color = "#000000",face = "italic")) + # 修改图例标题外观
  theme(legend.text = element_text(size = 6,family = "ARL",color = "#000000")) + # 修改图例标签
  theme(legend.justification = c(0,1),# 调整图例的基准点和相对位置 # 将图例的右上角(1,1)设为基准点
        legend.position = c(0.01-0.001,0.99+0.001),# 调整图例的相对位置
        # legend.position = "right", # 修改图例位置
        # legend.background = element_blank(),
        legend.background = element_rect(color = NA,
                                         # fill = "white",
                                         # color = "#333333",
                                         fill = NA,
                                         linewidth = 0.1,
                                         linetype = 0),
        legend.spacing = unit(0,"mm"),
        legend.direction = "vertical") +
  # theme(legend.position = "right", # 修改图例位置
  #       legend.spacing = unit(0,"mm"),
  #       legend.direction = "vertical") +
  theme(legend.key.height = unit(5,"mm"),
        legend.key.width = unit(2.5,"mm")) + # 图例大小
  theme(legend.position = "none") +
  theme(plot.margin = unit(c(1,1,1,1),"lines")) + # 边界距离上右下左 单位inch或者lines
  theme(aspect.ratio = 1) +
  annotate("text",x = -Inf,y = Inf,label = "A",family = "ARL",
           color = "#000000",size = 6/.pt,hjust = -1,vjust = 1) +
  annotate("text",x = Inf,y = Inf,label = "",family = "ARL",
           color = "#000000",size = 6/.pt,hjust = 1.05,vjust = 1)
print(P_Meandiff_Locamean_width_2.36525)

# Output plot
Plot_filename_Meandiff_Locamean <- paste0("HouXudong//Results Var_Meandiff//2024.3.8 Results process//",
                                          "Meandiff_Locamean Absskew_-0.5 2024.3.8 ",
                                          "_SD_",1,
                                          "_Pop_ratio_",1)
CairoSVG(file = paste0(Plot_filename_Meandiff_Locamean,".svg"),width = 7.25,hight = 10)
showtext_begin()
print(P_Meandiff_Locamean)
showtext_end()
dev.off()

CairoPDF(file = paste0(Plot_filename_Meandiff_Locamean,".pdf"),width = 6, height = 6)
showtext_begin()
print(P_Meandiff_Locamean)
showtext_end()
dev.off()

CairoPDF(file = paste0(Plot_filename_Meandiff_Locamean,"_width_4.pdf"),width = 4, height = 6)
showtext_begin()
print(P_Meandiff_Locamean_width_4)
showtext_end()
dev.off()

CairoPDF(file = paste0(Plot_filename_Meandiff_Locamean,"_width_3.pdf"),width = 3, height = 6)
showtext_begin()
print(P_Meandiff_Locamean_width_3)
showtext_end()
dev.off()

CairoPDF(file = paste0(Plot_filename_Meandiff_Locamean,"_width_2.36525.pdf"),width = 2.36525, height = 6)
showtext_begin()
print(P_Meandiff_Locamean_width_2.36525)
showtext_end()
dev.off()
