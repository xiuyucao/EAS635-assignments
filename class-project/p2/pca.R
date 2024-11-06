## MVA 1 --> PCA
## Code reference - Migliavacca et al., 2021
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(ggpubr)


## --------------------------------------- Load Data --------------------------------------- ##
trees <- readRDS('data/proj/input_trees.rds') %>%
  filter(if_all(c(HT, DIA, CR, DRYBIO_STEM, DRYBIO_STEM_BARK, DRYBIO_BRANCH, DRYBIO_FOLIAGE), ~ !is.na(.)))  # filter out rows with these cols having NAs


## ------------------------------------------ PCA ------------------------------------------ ##
trees4pca <- trees %>%  # select continuous variables
  select(HT:CR, DRYBIO_STEM:DRYBIO_FOLIAGE)

trees.pca <- PCA(scale(trees4pca), graph=F)  # do PCA
ind <- get_pca_ind(trees.pca)  # get result

# combine the results with the original data
result <- trees
result$PC1 <- ind$coord[,1]
result$PC2 <- ind$coord[,2]
result$PC3 <- ind$coord[,3]


## ---------------------------------------- Figures ---------------------------------------- ##
## Scree plot
p.scree <- fviz_eig(trees.pca, addlabels=T,  
                    barfill="white", barcolor ="darkblue",
                    ncp = 6, labelsize = 2,
                    linecolor ="red") + 
  ylim(0, 63) + 
  theme_classic() + 
  labs(title = "", x = "Principal Components", y = "Explained variance (%)") + 
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10))


## Biplot
p.biplot <- fviz_pca_biplot(trees.pca,
                            fill.ind=trees$GENUS, col.ind='white', palette='npg', pointshape=21, pointsize=1.5,
                            label='var', labelsize=3, repel=T,  # set loading label paras
                            col.var='black') +  # color of the loading arrows
  labs(title = "", fill = "Genus",
       x = "Principal Component 1",  
       y = "Principal Component 2") + 
  theme(legend.position = "bottom",
        legend.key.size = unit(1, "cm"), legend.key.width = unit(.5,"cm"),
        legend.text=element_text(size=10), legend.title=element_blank(), 
        text = element_text(size = 10),
        axis.title = element_text(size = 10),axis.text = element_text(size = 10)) +
  guides(fill = guide_legend(nrow = 2)) + 
  xlim(-10, 10) + ylim(-10, 10)


## loadings
# Extract the loadings of the PCA
var <- trees.pca$var
df.loadings <- data.frame(tree = rep(row.names(var$coord),3), ########################### set the high and low here
                          val = c(var$coord[,1],var$coord[,2], var$coord[,3]),
                          contrib = c(var$contrib[,1],var$contrib[,2], var$contrib[,3]),
                          PC = c(rep("PC1",length(row.names(var$coord))),
                                 rep("PC2",length(row.names(var$coord))),
                                 rep("PC3",length(row.names(var$coord)))))

p.loadings <- ggplot() +  # loading values
  geom_bar(data=df.loadings, aes(x=tree, y=val), fill='darkorange3', stat="identity", position=position_dodge()) + 
  facet_grid(. ~ PC, scales = "free_y") +
  coord_flip() + 
  theme_minimal() + 
  labs(y="Loadings", x = "") + 
  theme(legend.position='none', 
        text = element_text(size = 10),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10))

p.contribution <- ggplot() +  # percentage contribution
  geom_bar(data=df.loadings, aes(x=tree, y=contrib), fill='darkorange3', stat="identity", position=position_dodge()) + 
  facet_grid(. ~ PC, scales = "free_y") +
  coord_flip() + 
  theme_minimal() + 
  labs(y="Contribution (%)", x = "") + 
  theme(legend.position='none', 
        text = element_text(size = 10),
        axis.title = element_text(size = 10), axis.text = element_text(size = 10))


## final figure
stats <- ggarrange(p.scree, p.loadings, p.contribution,
                   labels = c("b", "c", "d"),
                   ncol=1, nrow=3)
fig <- ggarrange(p.biplot, stats, 
                 labels = c("a", ""),
                 ncol=2, nrow=1)

ggsave('class-project/figs/p2-fig1.png', fig, width=12.5, height=10)
