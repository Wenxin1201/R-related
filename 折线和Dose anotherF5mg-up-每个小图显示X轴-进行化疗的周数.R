# 加载必要的库
library(ggplot2)
library(dplyr)
library(ggforce)
library(readxl)

# 设置工作目录（请根据您的实际情况修改）
setwd('D:/0. XLT/SAL0951/SAL0951B201/探索基线和Hb上升')

# 1. 读取数据
# 1.1 读取主数据 (hbper5dose.xlsx)
df <- read_excel("hbper5dose.xlsx")
names(df)[names(df) == "hb"] <- "Hb"  # 列名标准化

# 1.2 读取TURANWK1数据
tuwk_data <- read_excel("turanwk1fiv.xlsx")

# 2. 数据预处理与合并
# 2.1 提取SUBJID的数字部分，并确保数据类型一致
df$SUBJID <- gsub(",.*", "", df$subject)  # 从subject中提取SUBJID
tuwk_data$SUBJID <- as.character(tuwk_data$SUBJID) # 确保SUBJID为字符型

# 2.2 合并数据
df <- left_join(df, tuwk_data[, c("SUBJID", "tuwk")], by = "SUBJID")

# 3. 绘图
# 3.1 确定每页显示的分面数量
subjects_per_page <- 4
total_pages <- ceiling(n_distinct(df$subject) / subjects_per_page)

# 3.2 创建虚拟数据点用于图例
dummy_df <- data.frame(
  x = NA_real_,
  y = NA_real_,
  label = c("剂量值", "Hb值")
)

# 3.3 循环绘制分页图形
for (page in 1:total_pages) {
  p <- ggplot(df, aes(x = visit)) +  # 使用visit作为x轴
    # 绘制背景网格（剂量柱）
    geom_rect(
      aes(
        xmin = visit - 0.3,
        xmax = visit + 0.3,
        ymin = 80,
        ymax = (dose - 1) * 6.25 + 80,
        fill = "剂量柱"
      ),
      alpha = 0.5
    ) +
    # 绘制Hb折线图和点
    geom_line(aes(y = Hb, color = "Hb曲线"), linewidth = 1) +
    geom_point(aes(y = Hb, color = "Hb曲线"), size = 2) +
    # 添加Hb值标签
    geom_text(aes(y = Hb, label = Hb), 
              vjust = -0.5,
              size = 3,
              color = "black") +
    # 添加dose值标签
    geom_text(aes(y = (dose - 1) * 6.25 + 80, label = dose),
              vjust = -3.2,
              size = 3,
              color = "blue") +
    
    # 在y=80处添加tuwk值的点
    geom_point(data = ~ .x %>% filter(!is.na(tuwk)),  # 过滤掉tuwk缺失的行
               aes(y = 70, x = tuwk, color = "进行化疗的周数"),  # 设置y值为80，x值为tuwk
               size = 2,
               shape = 17) +  # 16是实心圆点，17是三角形标记
    
    # 添加tuwk值标签
    geom_text(data = ~ .x %>% filter(!is.na(tuwk)),
              aes(y = 70, x = tuwk+0.4, label = round(tuwk, 2)),
              vjust = -0.5,  # 调整垂直位置，使标签位于点上方
              size = 3,
              color = "darkgreen") +
    
    # 添加虚拟点用于剂量值和Hb值图例
    geom_point(data = dummy_df, aes(x = x, y = y, color = label), show.legend = TRUE) +
    
    # 设置双轴
    scale_y_continuous(
      name = "Hb (g/L)",
      limits = c(60, 160),
      breaks = seq(60, 160, by = 10),
      sec.axis = sec_axis(
        trans = ~ (. - 80) / 6.25 + 2,
        name = "剂量 (mg)",
        breaks = seq(1, 10, by = 1)
      )
    ) +
    
    # 设置颜色
    scale_color_manual(
      values = c(
        "Hb曲线" = "red",
        "剂量值" = "blue",
        "Hb值" = "black",
        "进行化疗的周数" = "darkgreen"  # 为tuwk值的点设置颜色
      ),
      name = "",
      breaks = c("Hb曲线", "剂量值", "Hb值", "进行化疗的周数")  # 确保图例中包含 "tuwk值"
    ) +
    
    # 设置填充色
    scale_fill_manual(
      values = c("剂量柱" = "lightblue"),
      name = ""
    ) +
    
    # 设置X轴刻度, 并根据tuwk的最大值调整上限
    scale_x_continuous(
      breaks = c(0, 2, 4, 6, 8,10,12,14,16),
      limits = c(-0.5, 16.5)
    ) +
    
    # 分页与分面
    facet_wrap_paginate(
      ~ subject, 
      ncol = 2, 
      nrow = 2, 
      page = page,
      scales = "free_x"
    ) +
    
    # 主题与样式
    labs(
      x = "访视 (周)",
      title = paste("Hb 与 剂量 随访变化图 (第", page, "页) 5mg组", sep = "")
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "top",
      legend.box = "horizontal",
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
      strip.text = element_text(size = 8),
      panel.spacing = unit(1, "lines"),
      axis.text.x = element_text(size = 8, angle = 0),
      strip.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "black", fill = NA),
      axis.line = element_line(color = "black"),
      panel.grid.major = element_line(color = "grey90"),
      panel.grid.minor = element_blank()
    ) +
    guides(
      color = guide_legend(nrow = 1),
      fill = guide_legend(nrow = 1, override.aes = list(shape = NA))
    )
  
  # 显示当前页
  print(p)
  
  # 保存每页图像
  ggsave(paste0("Hb_Dose_Page_", page, ".png"), p, width = 12, height = 9, dpi = 300)
}
