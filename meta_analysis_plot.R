plot_meta <- function(){
  library(ggplot2)
  library(ggrepel)
  cf <- read.csv("cluster_freq.csv", stringsAsFactors = F)
  df_meta <- read.csv("meta-analytic-effects.csv", stringsAsFactors = F)
df_meta$Variable <- trimws(tolower(df_meta$V1))
all(df_meta$Variable %in% tolower(cf$Construct))
df_meta$Variable[!df_meta$Variable %in% tolower(cf$Construct)]

df_meta$Frequency <- as.integer(cf$Frequency[match(df_meta$Variable, cf$Construct)])
df_meta$effectsize <- factor(substr(df_meta$V2, 1,1), levels = c("d", "r"))
df_meta$Value <- abs(as.numeric(gsub("^.+?=(.+)\\).?", "\\1", df_meta$V2)))

levels(df_meta$effectsize) <- paste0(levels(df_meta$effectsize), ", ", tapply(df_meta, df_meta$effectsize, function(x){
  print(cor.test(x$Value, log(x$Frequency), use = "pairwise.complete.obs"))
  formatC(cor(x$Value, log(x$Frequency), use = "pairwise.complete.obs"), digits = 2, format = "f")
}))


df_plot <- df_meta[, c("Variable", "Value", "Frequency", "effectsize")]
df_plot <- na.omit(df_plot)

p <- ggplot(df_plot, aes(x = Value, y = Frequency, shape = effectsize, color = effectsize, label = Variable)) +
  #geom_point() +
  ggrepel::geom_text_repel(max.overlaps = 20, size = 4) +
  #geom_text(aes(label = Variable, size = 1)) +
  theme_bw()+
  theme(legend.position = c(.9,.9), legend.title = element_blank())+
  scale_y_log10()
ggsave("meta_analysis_plot.svg", p, device = "svg", width = 200, height = 180, units = "mm")
}
# df_meta$Variable[df_meta$Variable %in% tolower(cf$Construct)]c
