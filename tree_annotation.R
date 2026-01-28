##Visualization of Maximum likelihood trees

#Load necessary libraries
library(ggplot2) #used geom tile
library(ggtree) #for reading and displaying tree
# library(dplyr)
library(ape) #for rooting tree using out-group
# library(treeio) 
library(ggnewscale) #for assigning new scale color
library(ggtreeExtra) #for heatmap/color_strip
library(RColorBrewer) #for using color pellet

#other useful packages
library(phytools) #for midpoint
# library(tidyr)
# library(tidytree)

#read in tree
tree <- read.tree("iqtree_all.treefile")

#visualize tree
ggtree(rooted_tree)

#root the tree at outgroup and ladderize 
rooted_tree <- root(tree, outgroup = "H37Rv")
rooted_tree <- ladderize(rooted_tree, right = FALSE)
ggtree(rooted_tree)

#drop the out group
rooted_tree <- drop.tip.phylo(rooted_tree, tip = "H37Rv")
ggtree(rooted_tree)

#to visualize tree in circular layout
# p <- ggtree(rooted_tree, layout = "circular")
# p + geom_rootedge(rootedge = 0.0005) 

#root the tree at midpoint
# midrooted_tree <- midpoint_root(tree)
# midrooted_tree <- drop.tip.phylo(rooted_tree, tip = "H37Rv")
# ggtree(midrooted_tree, layout = "circular")

#read in metadata
metadata <- read.csv("metadata.csv")

#tip color color ac to metadata (i.e. country)
annotated_tree <- ggtree(rooted_tree, color='#676767',size=0.5) %<+% 
  metadata +
  geom_tiplab(align = TRUE, linetype = NA, size=2.5, offset = 0.0001,hjust=1)+
  geom_tippoint(aes(fill=Country), size=2, shape=21) +
  scale_fill_manual(values = c("yellow","orange",'#4875e7ff',"#eb4037ff","magenta","green","brown","cyan"),
                    guide=guide_legend(order = 1))+
  geom_treescale(x=0, y=0.05,fontsize=3)+
  ylim(c(0, 150))+
  theme(legend.key.size =unit(0.4, "cm"),
        legend.title = element_text(size = 12))

annotated_tree

#to display color palette from RColorBrewer
# display.brewer.all()

#selecting color palette
color_year = colorRampPalette(brewer.pal(9,"Set1"))
color_host = colorRampPalette(brewer.pal(8,"Accent"))

#Adding 1st (year) color strip
annotated_tree01 <- annotated_tree +
  new_scale_fill() +
  geom_fruit(geom = geom_tile,
             mapping = aes(fill = factor(Year)),width=0.000025, offset =0.265 )+
  scale_fill_manual(values = c(color_year(17)),
                    name = "Year",
                    guide=guide_legend(reverse = TRUE, order = 2))+
  # guides(fill = guide_legend(reverse = TRUE))+
  # labs(fill ="Year")+
  theme(legend.key.size =unit(0.4, "cm"), legend.title = element_text(size = 12))

annotated_tree01

#Adding 2nd (host) color strip
annotated_tree02 <- annotated_tree01 +
  new_scale_fill() +
  geom_fruit(geom = geom_tile,
             mapping = aes(fill = host_name),width=0.000025, offset =0.065 )+
  scale_fill_manual(values = c(color_host(10)),
                    name = "Host",
                    guide=guide_legend(order = 3))

annotated_tree02

#save last plot in svg format
ggsave("annotated_tree.svg", scale = 3.2, width = 600,height = 1300, units = "px")