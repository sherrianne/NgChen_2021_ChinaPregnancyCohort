# Subcohort Microbiome Profiles 

#===== Figure 2a ======#

# Prepare data for microbiome profiles
func_sialdata <- function(data, sialidase = NA){
  subcohort_meta_genera %>%
    filter(SialidaseActivity == sialidase) %>%
    arrange(desc(Lactobacillus)) %>%
    rowid_to_column(var = "RowNumber")
}

func_sialdata_mp <- function(data, sialidase = NA){
  subcohort_meta_genera %>%
    filter(SialidaseActivity == sialidase) %>%
    arrange(desc(Lactobacillus)) %>%
    rowid_to_column(var = "RowNumber") %>%
    select(RowNumber, Sample_ID, Lactobacillus:Bacteroides) %>%
    pivot_longer(cols = Lactobacillus:Bacteroides) %>%
    rename(Genus = "name")
}

# Aesthetics
genus_color <- c("#212f3d","#a50026","#d73027","#f46d43",
               "#fdae61","#fee08b","#d9ef8b","#a6d96a",
               "#66bd63","#1a9850","#006837","#7a0177",
               "#ae017e","#dd3497","#f768a1","#fa9fb5",
               "#fcc5c0","#9e9ac8","#2166ac","#4393c3",
               "#92c5de","#d1e5f0","#bdbdbd")
genus_label <- c("Lactobacillus","Gardnerella","Atopobium","Prevotella",
               "Megasphaera","Shuttleworthia","Peptostreptococcus",
               "Bacteroides","Fusobacterium","Veillonella","Ureaplasma",
               "Streptococcus","Staphylococcus","Enterococcus","Campylobacter",
               "Anaerococcus","Aerococcus","Bifidobacterium","Chlamydia",
               "Clostridium","Dialister","Finegoldia","Genus_Other")

# Theme
main_theme <- theme(
  plot.title = element_text(hjust = 0.5, size = 19),
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.title.y = element_text(size = 19, margin = margin(t = 0, r = 10, b = 0, l = 0)),
  axis.text.y = element_text(size = 18),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "white"),
  legend.position = "none")

#===== Figure S3 ======#

# Microbiome Profiles 
func_lacto_profiles <- function(data, title, subtitle, legend_position, nrow_num){
  # Aesthetics
  spec_level <- c("Lactobacillus_crispatus","Lactobacillus_gasseri",
                  "Lactobacillus_iners","Lactobacillus_jensenii",
                  "Lactobacillus_delbrueckii","Lactobacillus_spp.",
                  "Bifidobacterium_spp.","Gardnerella_spp.","Prevotella_spp.",
                  "Megasphaera_spp.","Atopobium_vaginae","Shuttleworthia_spp.",
                  "Aerococcus_spp.","Streptococcus_agalactiae",
                  "Streptococcus_anginosus","Anaerococcus_spp.",
                  "Streptococcus_luteciae","Streptococcus_spp.",
                  "Veillonella_spp.","Species_Other")
  spec_label <- c("Lactobacillus crispatus","Lactobacillus gasseri",
                  "Lactobacillus iners","Lactobacillus jensenii",
                  "Lactobacillus delbrueckii","Lactobacillus spp.",
                  "Bifidobacterium spp.","Gardnerella spp.","Prevotella spp.",
                  "Megasphaera spp.","Atopobium vaginae","Shuttleworthia spp.",
                  "Aerococcus spp.","Streptococcus agalactiae",
                  "Streptococcus anginosus","Anaerococcus spp.",
                  "Streptococcus luteciae","Streptococcus spp.",
                  "Veillonella spp.","Species Other")
  spec_color <- c("#1f78b4","#33a02c","#ff7f00","#6a3d9a",
                  "#6baed6","#c6dbef","#cab2d6","#99000d",
                  "#ef3b2c","#fc9272","#fee0d2","#ffeda0",
                  "#005a32","#41ab5d","#a1d99b","#e5f5e0",
                  "#fcc5c0","#fa9fb5","#f768a1","#dd3497")
  
  # Plot
  ggplot(data, 
         aes(x = Sample_ID, y = Abundance, 
             fill = factor(Species, levels = spec_level))) +
    scale_fill_manual(values = spec_color, 
                      labels = spec_label, 
                      guide = guide_legend(nrow = nrow_num)) + 
    geom_bar(stat = "identity", width = 1, preserve = "single") + 
    labs(title = title,
         subtitle = subtitle,
         fill="Species", 
         y="Relative Abundance") + 
    facet_grid(~ VMG, scales = "free", space = "free") + 
    theme(plot.title = element_text(hjust = 0.5, size = 19), 
          plot.subtitle = element_text(hjust = 0.5, size = 18),
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.title.x = element_blank(),
          axis.text.x = element_blank(), 
          axis.title.y = element_text(size = 19), 
          axis.text.y = element_text(size = 18), 
          axis.ticks = element_blank(), 
          strip.background = element_rect(color = "black", size = 1, linetype = "solid"),
          strip.text.x = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 19),
          legend.text = element_text(size = 18, face = "italic"),
          legend.position = legend_position)
}
