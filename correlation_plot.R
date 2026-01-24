# load necessary libraries
library(linkET) # for correlation matrix calculation
library(ggplot2)
library(reshape2)

#read data from vegan package
data("varechem", package = "vegan")
chem <- varechem[-c(12,13)]

#generate correlation matrix
corr <- correlate(chem)

# make correlation matrix and p value matrix
corr_matrix <- corr[["r"]]
pvalue_matrix <- corr[["p"]]

#convert matrix into df
corr_df <- melt(corr_matrix)
pvalue_df <- melt(pvalue_matrix)

#combine correlation and pvalue
corr_df2 <- cbind(corr_df,pvalue_df$value)
#change col names
colnames(corr_df2) <- c("Var1", "Var2","cor", "pvalue")

# Add significance stars
corr_df2$significance <- cut(
  corr_df2$pvalue,
  breaks = c(-Inf, 0.001, 0.01, 0.05, Inf),
  labels = c("***", "**", "*", "")
)

# for Removing half mirror image
corr_df3 <- corr_df2[as.numeric(corr_df2$Var1) > as.numeric(corr_df2$Var2),]


#plot correlation matrix with pvalue using ggplot2
ggplot(corr_df3, aes(Var1, Var2))+
  geom_tile(color = "black",fill="white")+
  geom_point(aes(size =abs(cor), color=cor))+
  geom_text(aes(label = significance), color = "black", size = 3.5, fontface = "bold") +
  scale_color_gradient2(low = "blue",mid = "white",high = "red", midpoint = 0, limits=c(-0.85, 0.85), breaks=c(-0.8, -0.4,0,0.4,0.8))+
  scale_x_discrete(limits =rev(levels(factor(corr_df3$Var1))))+
  coord_fixed()+
  theme_minimal()+
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
    axis.title = element_blank(),
    legend.position = "bottom",
    )+
  guides(
    size = "none",
    color = guide_colorbar(
      title= NULL,
      direction = "horizontal",
      barwidth  = unit(0.5, "npc"),
      barheight = unit(0.4, "cm")
      )
    )

# linkET qcorrplot can also be used
# qcorrplot(corr, type="lower", diag=FALSE)+
#   geom_square()

ggsave("plot4.svg",dpi = 1200, width = 7, height = 4)
svg("plot2.svg")
dev.off()

