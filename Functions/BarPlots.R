# Proportion Plots

#===== Lactobacillus Abundance ======#

# Bar Chart 
func_barchart_lactoabun <- function(data, x_var, lacto_colors){
  # Aesthetics
  domdep_colors <- c("#1f78b4", "#e31a1c")
  depdom_colors <- c("#e31a1c", "#1f78b4")
  
  # Plot
  ggplot(data, aes(x = x_var, fill = LactoDomDep)) + 
    geom_bar(position = position_dodge()) + 
    geom_text(stat = 'count', 
              aes(label = ..count..), 
              position = position_dodge(0.9), 
              vjust = -0.2 , 
              size = 5) + 
    scale_fill_manual(values = lacto_colors) + 
    labs(title = "Lactobacillus Abundance") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = "bottom")
}

# Stacked Bar 
func_stackedbar_lactoabun <- function(data, x_var, lacto_colors, title, subtitle, nrow_num, legend_position){
  # Aesthetics
  depdom_colors <- c("#e31a1c", "#1f78b4")
  domdep_colors <- c("#1f78b4", "#e31a1c")
  
  # Percentage 
  percentage <- data %>% 
    group_by_(x_var) %>% 
    count(LactoDomDep) %>% 
    mutate(Percent = round(n/sum(n)*100, 2),
           Percentage = round(n/sum(n)*100)) %>% 
    ungroup() %>%
    as.data.frame()
  
  # Stacked Bar
  ggplot(percentage, 
         aes(x = percentage[,1], y = Percent, fill = LactoDomDep)) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = round(Percentage)), 
              position = position_stack(vjust = 0.5), size = 3) + 
    scale_fill_manual(values = lacto_colors, guide = guide_legend(nrow = nrow_num)) + 
    labs(title = title, 
         subtitle = subtitle,
         y = "Percentage (%)", 
         fill = "Lactobacillus") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = legend_position) 
  
}


#===== Vaginal Microbiome Groups =====#

# Bar Chart 
func_barchart_vmg <- function(data, x_var){
  # Aesthetics
  VMG_colors <- c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c", "#fb9a99", 
                  "#6a3d9a", "#a6cee3", "#cab2d6", "#C0C0C0")
  VMG_labels <- c("VMG I", "VMG II", "VMG III", "VMG IV-A", "VMG IV-B", 
                  "VMG V", "VMG VI", "VMG VII", "Other")
  
  #Plot
  ggplot(data, aes(x = x_var, fill = VMG)) + 
    geom_bar(position = position_dodge()) + 
    geom_text(stat='count', 
              aes(label = ..count..), 
              position = position_dodge(0.9), 
              vjust = -0.2 , 
              size = 5) + 
    scale_fill_manual(values = VMG_colors, labels = VMG_labels) + 
    labs(title="Vaginal Microbiome Group") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = "bottom")
}

# Stacked Bar 
func_stackedbar_vmg <- function(data, x_var, title, subtitle, nrow_num, legend_position){
  # Aesthetics
  VMG_colors <- c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c", "#fb9a99", 
                  "#6a3d9a", "#a6cee3", "#cab2d6", "#C0C0C0")
  
  # Percentage 
  percentage <- data %>% 
    group_by_(x_var) %>% 
    count(VMG) %>% 
    mutate(Percent = round(n/sum(n)*100, 2),
           Percentage = round(n/sum(n)*100)) %>% 
    ungroup() %>%
    as.data.frame()
  
  # Stacked Bar
  ggplot(percentage, 
         aes(x = percentage[,1], y = Percent, fill = VMG)) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = round(Percentage)), 
              position = position_stack(vjust = 0.5), size = 3) + 
    scale_fill_manual(values = VMG_colors, guide = guide_legend(nrow = nrow_num)) + 
    labs(title = title, 
         subtitle = subtitle,
         y = "Percentage (%)", 
         fill = "VMG") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = legend_position) 
  
}

#====== Birth Outcome (Preterm/Term) ======#

# Bar Chart 
func_barchart_ptb <- function(data, x_var){
  # Aesthetics
  ptb_colors <- c("#ff595e", "#1982c4")
  
  # Plot
  ggplot(data, aes(x = x_var, fill = Group_PretermTerm)) + 
    geom_bar(position = position_dodge()) + 
    geom_text(stat = 'count', 
              aes(label = ..count..), 
              position = position_dodge(0.9), 
              vjust = -0.2 , 
              size = 5) + 
    scale_fill_manual(values = ptb_colors) + 
    labs(title = "Birth Outcome") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = "bottom")
}

# Stacked Bar 
func_stackedbar_ptb <- function(data, x_var, title, subtitle, nrow_num, legend_position){
  # Aesthetics
  ptb_colors <- c("#ff595e", "#1982c4")
  
  # Percentage 
  percentage <- data %>% 
    group_by_(x_var) %>% 
    count(Group_PretermTerm) %>% 
    mutate(Percent = round(n/sum(n)*100, 2),
           Percentage = round(n/sum(n)*100)) %>% 
    ungroup() %>%
    as.data.frame()
  
  # Stacked Bar
  ggplot(percentage, 
         aes(x = percentage[,1], y = Percent, fill = Group_PretermTerm)) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = round(Percentage)), 
              position = position_stack(vjust = 0.5), size = 3) + 
    scale_fill_manual(values = ptb_colors, guide = guide_legend(nrow = nrow_num)) + 
    labs(title = title, 
         subtitle = subtitle,
         y = "Percentage (%)", 
         fill = "Birth Outcome") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = legend_position) 
  
}


#====== Sialidase Activity ======#

# Bar Chart 
func_barchart_sial <- function(data, x_var){
  # Aesthetics
  sial_colors <- c("#c51b7d", "#4d9221")
  
  # Plot
  ggplot(data, aes(x = x_var, fill = SialidaseActivity)) + 
    geom_bar(position = position_dodge()) + 
    geom_text(stat='count', 
              aes(label = ..count..), 
              position = position_dodge(0.9), 
              vjust = -0.2 , 
              size = 5) + 
    scale_fill_manual(values = sial_colors) + 
    labs(title="Sialidase Activity") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = "bottom")
}

# Stacked Bar 
func_stackedbar_sial <- function(data, x_var, title, subtitle, nrow_num, legend_position){
  # Aesthetics
  sial_colors <- c("#c51b7d", "#4d9221")
  
  # Percentage 
  percentage <- data %>% 
    group_by_(x_var) %>% 
    count(SialidaseActivity) %>% 
    mutate(Percent = round(n/sum(n)*100, 2),
           Percentage = round(n/sum(n)*100)) %>% 
    ungroup() %>%
    as.data.frame()
  
  # Stacked Bar
  ggplot(percentage, 
         aes(x = percentage[,1], y = Percent, fill = SialidaseActivity)) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = round(Percentage)), 
              position = position_stack(vjust = 0.5), size = 3) + 
    scale_fill_manual(values = sial_colors, guide = guide_legend(nrow = nrow_num)) + 
    labs(title = title, 
         subtitle = subtitle,
         y = "Percentage (%)", 
         fill = "Sialidase") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = legend_position) 
  
}

#====== Leukocytes ======#

# Bar Chart 
func_barchart_leuk <- function(data, x_var){
  # Aesthetics
  leuk_colors <- c("#e31a1c", "#33a02c")
  
  # Plot
  ggplot(data, aes(x = x_var, fill = Leukocyte)) + 
    geom_bar(position = position_dodge()) + 
    geom_text(stat='count', 
              aes(label = ..count..), 
              position = position_dodge(0.9), 
              vjust = -0.2 , 
              size = 5) + 
    scale_fill_manual(values = leuk_colors) + 
    labs(title="Leukocyte") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = "bottom")
}

# Stacked Bar 
func_stackedbar_leuk <- function(data, x_var, title, subtitle, nrow_num, legend_position){
  # Aesthetics
  leuk_colors <- c("#e31a1c", "#33a02c")
  
  # Percentage 
  percentage <- data %>% 
    group_by_(x_var) %>% 
    count(Leukocyte) %>% 
    mutate(Percent = round(n/sum(n)*100, 2),
           Percentage = round(n/sum(n)*100)) %>% 
    ungroup() %>%
    as.data.frame()
  
  # Stacked Bar
  ggplot(percentage, 
         aes(x = percentage[,1], y = Percent, fill = Leukocyte)) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = round(Percentage)), 
              position = position_stack(vjust = 0.5), size = 3) + 
    scale_fill_manual(values = leuk_colors, guide = guide_legend(nrow = nrow_num)) + 
    labs(title = title, 
         subtitle = subtitle,
         y = "Percentage (%)", 
         fill = "Leukocyte") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = legend_position) 
  
}


#====== Chorioamnionitis ======#

# Bar Chart 
func_barchart_chorio <- function(data, x_var){
  # Aesthetics
  chorio_colors <- c("#fe6d73", "#227c9d")
  
  # Plot
  ggplot(data, aes(x = x_var, fill = Chorioamnionitis)) + 
    geom_bar(position = position_dodge()) + 
    geom_text(stat = 'count', 
              aes(label = ..count..), 
              position = position_dodge(0.9), 
              vjust = -0.2 , 
              size = 5) + 
    scale_fill_manual(values = chorio_colors) + 
    labs(title = "Chorioamnionitis") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = "bottom")
}

# Stacked Bar 
func_stackedbar_chorio <- function(data, x_var, title, subtitle, nrow_num, legend_position){
  # Aesthetics
  chorio_colors <- c("#fe6d73", "#227c9d")
  
  # Percentage 
  percentage <- data %>% 
    group_by_(x_var) %>% 
    count(Chorioamnionitis) %>% 
    mutate(Percent = round(n/sum(n)*100, 2),
           Percentage = round(n/sum(n)*100)) %>% 
    ungroup() %>%
    as.data.frame()
  
  # Stacked Bar
  ggplot(percentage, 
         aes(x = percentage[,1], y = Percent, fill = Chorioamnionitis)) + 
    geom_bar(stat = "identity") + 
    geom_text(aes(label = round(Percentage)), 
              position = position_stack(vjust = 0.5), size = 3) + 
    scale_fill_manual(values = chorio_colors, guide = guide_legend(nrow = nrow_num)) + 
    labs(title = title, 
         subtitle = subtitle,
         y = "Percentage (%)", 
         fill = "Chorioamnionitis") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          plot.subtitle = element_text(hjust = 0.5, size = 18), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 18), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18), 
          legend.position = legend_position) 
  
}

#====== Bar plots and stacked bars (general) ======#

func_barchart_stackedbar <- function(data, x_var, fill_var, fill_var_colors, title, lab_x, lab_fill, x_guide){
  # Bar chart 
  barcharts <- ggplot(data, aes(x = x_var, fill = fill_var)) +
    geom_bar(position = position_dodge()) +
    geom_text(stat = 'count', aes(label = ..count..), 
              position = position_dodge(0.9), vjust = -0.2 , size = 5) +
    scale_fill_manual(values = fill_var_colors, name = lab_fill) +
    labs(title = title, 
         subtitle = "Bar Chart: Counts",
         x = lab_x) + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, size = 17),
          plot.subtitle = element_text(hjust = 0.5, size = 16),
          axis.title.x = element_text(size = 17),  
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_text(size = 17),
          axis.text.y = element_text(size = 16), 
          legend.title = element_text(size = 17),
          legend.text = element_text(size = 16),
          legend.position = "none") +
    guides(x = x_guide)
  
  # Stacked bar 
  stackedbar_pc <- 
    data %>% 
    group_by(all_of(x_var)) %>% 
    count(all_of(fill_var)) %>% 
    mutate(Percentage = round(n/sum(n)*100, 2), 
           Percent = round(n/sum(n)*100)) %>% 
    ungroup() %>%
    as.data.frame()
  
  stackedbar <- ggplot(stackedbar_pc,
                       aes(x = stackedbar_pc[,1],
                           y = stackedbar_pc$Percentage,
                           fill = stackedbar_pc[,2])) +
    geom_bar(stat = "identity") + 
    geom_text(aes(label = round(Percentage)), 
              position = position_stack(vjust = 0.5), size = 5) +
    scale_fill_manual(values = fill_var_colors, name = lab_fill) + 
    labs(subtitle = "Stacked Bar: Proportions", 
         x = lab_x,
         y = "Percentage (%)") + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5, size = 17),
          plot.subtitle = element_text(hjust = 0.5, size = 16),
          axis.title.x = element_text(size = 17), 
          axis.text.x = element_text(size = 16), 
          axis.title.y = element_text(size = 17), 
          axis.text.y = element_text(size = 16),
          legend.title = element_text(size = 17),
          legend.text = element_text(size = 16),
          legend.position = "right") +
    guides(x = x_guide)
  
  # Combined plot
  print(stackedbar_pc)
  plt_combined <- plot_grid(barcharts, stackedbar, 
                            rel_widths = c(1.5,1), ncol = 2, align = "h")
  print(plt_combined)
  
}
