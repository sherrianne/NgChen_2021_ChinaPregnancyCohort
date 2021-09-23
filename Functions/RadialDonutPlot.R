# Script for radial and donut plot 

species_levels <- c("Lactobacillus_crispatus", "Lactobacillus_gasseri", "Lactobacillus_iners",
                    "Gardnerella_spp.", "Prevotella_spp.", "Atopobium_vaginae", "Megasphaera_spp.",
                    "Shuttleworthia_spp.", "Veillonella_spp.", "Streptococcus_agalactiae",
                    "Streptococcus_anginosus", "Streptococcus_luteciae", "Streptococcus_spp.",
                    "Anaerococcus_spp.", "Aerococcus_spp.", "Lactobacillus_jensenii",
                    "Lactobacillus_delbrueckii", "Lactobacillus_spp.", "Bifidobacterium_spp.", "Species_Other")

species_labels <- c("Lactobacillus crispatus", "Lactobacillus gasseri", "Lactobacillus iners",
                    "Gardnerella spp.", "Prevotella spp.", "Atopobium vaginae", "Megasphaera spp.",
                    "Shuttleworthia spp.", "Veillonella spp.", "Streptococcus agalactiae",
                    "Streptococcus anginosus", "Streptococcus luteciae", "Streptococcus spp.",
                    "Anaerococcus spp.", "Aerococcus spp.", "Lactobacillus jensenii",
                    "Lactobacillus delbrueckii", "Lactobacillus spp.", "Bifidobacterium spp.", "Species Other")


# InputDF should be col1=RowNum, col2=VMG, col3=Species, col4=value in long format.

# Radial plot
myRadialStackedBarPlot <- function(inputDF) {
  p <- ggplot(inputDF, aes(RowNum, value, fill = Species, group = value)) + 
    geom_bar(stat = 'identity', position = 'stack') + 
    scale_fill_manual(values = species_color, guide = guide_legend(nrow = 7), labels = species_labels) + 
    coord_polar() + 
    labs(x = "Sample", y = "Relative Abundance") + 
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.title = element_blank(), 
          panel.grid = element_blank(), 
          panel.border = element_blank(), 
          panel.background = element_rect(fill = "transparent", color = NA), 
          plot.background = element_rect(fill = "transparent", color = NA), 
          axis.line = element_blank(), 
          legend.title = element_text(size = 22),
          legend.text = element_text(size = 22, face = "italic"), 
          legend.position = "bottom") 
  return(p)
}

# Donut plot
myDonutPlot <- function(inputDF, lab_fill) {
  myVMGOrder <- c("VMG I", "VMG II", "VMG III", "VMG IV-A", "VMG IV-B", "VMG V", "VMG VI", "VMG VII")
  donut.tbl <- inputDF %>%
    select(RowNum, VMG) %>%
    unique() %>%
    dplyr::count(VMG) %>%
    mutate(VMG = factor(VMG, levels = myVMGOrder)) %>%
    arrange(VMG) %>%
    mutate(fraction = n / sum(n)) %>%
    mutate(ymax = cumsum(fraction)) %>%
    mutate(ymin = c(0, head(ymax, n = -1))) %>%
    mutate(VMG = factor(VMG, levels = myVMGOrder))
  
  p <- ggplot(donut.tbl, aes(fill = VMG, ymax = ymax, ymin = ymin, xmax = 7, xmin = 6.5)) +
    geom_rect(colour = "white") +
    coord_polar(theta = "y") +
    xlim(c(0, 7)) + 
    scale_fill_manual(values = vmg_color) + 
    labs(fill = lab_fill, title = "Vaginal Microbiome Groups") +
    theme(plot.title = element_text(hjust = 0.5, size = "24"),
      panel.grid = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(),
          # legend.position = "bottom",
          legend.text = element_text(size = 22), 
          legend.title = element_text(size = 22)) 
  
  return(p)
}
