######################################
###        Interspeech 2023        ###
###     Marc Allassonnière-Tang    ###
###       &  Mathilde Hutin        ###
###           27/04/2023           ###
######################################

### Upload data
library(readr) # to read csv files
DM_a_k <- read_csv("C:/Users/mhutin/Documents/PPaDisM/DM_a_k_IS2023.csv") # to upload the file
View(DM_a_k) # to view the dataframe

### Subset for investigations on only /a/
# Since F1 and F2 (and consequently HNR) do not apply to /k/ itself but are indicative of the following phone, we propose to compare the results on both /a/ and /k/ to the ones on only /a/.
# Preview: For all three graphs presented in the paper (vowel chart, decision tree and cluster analysis), results on /a/ are similar to those on /a/ and /k/.
DM_a <- DM_a_k [DM_a_k$label=="a",]
View(DM_a) # to view the dataframe

###############################

### Statistics

### To test the data with a decision tree

### Install necessary packages
install.packages("dplyr")
# Call necessary packages
library(party)
library(dplyr)

tree_a_k <- ctree(pos_dismo_large
                  ~ duration # to add the duration of segments as random variable
                  + `mean_F0(Hz)` # to add the mean F0 of segments as random variable
                  + `mean_F1(Hz)` # to add the mean F1 of segments as random variable
                  + `mean_F2(Hz)` # to add the mean F2 of segments as random variable
                  + `mean_HNR(dB)` # to add the mean HNR of segments as random variable
                  + speaker_gender
                  + speaker_nationality
                  + formality
                  + label,
                  #+ name_textgrid_file, 
                  data=DM_a_k %>% mutate(pos_dismo_large = as.factor(pos_dismo_large),
                                         `mean_F0(Hz)` = as.numeric(`mean_F0(Hz)`),
                                         `mean_F1(Hz)` = as.numeric(`mean_F1(Hz)`),
                                         `mean_F2(Hz)` = as.numeric(`mean_F2(Hz)`),
                                         `mean_HNR(dB)` = as.numeric(`mean_HNR(dB)`),
                                         speaker_gender = as.factor(speaker_gender),
                                         speaker_nationality = as.factor(speaker_nationality),
                                         formality = as.factor(formality),
                                         label = as.factor(label)
                                         #name_textgrid_file = as.factor(name_textgrid_file)
                  ))
plot(tree_a_k)

# To test the accuracy of the tree

output.inf.tree <- cbind(predict(tree_a_k) %>% as.character(),
                         DM_a_k$pos_dismo_large %>% as.character()) %>%
  as.data.frame() %>%
  rename(Prediction = 1, Actual = 2) %>%
  mutate(Performance = case_when(Prediction == Actual ~ "Correct",
                                 Prediction != Actual ~ "Wrong"))

#the columns are the real/reference values, the rows are the predicted values
caret::confusionMatrix(data = as.factor(output.inf.tree$Prediction),
                       reference = as.factor(output.inf.tree$Actual),
                       # select the metrics we want to show
                       mode = "prec_recall")
# for kappa https://stats.stackexchange.com/questions/82162/cohens-kappa-in-plain-english
# Kappa = (observed accuracy - expected accuracy)/(1 - expected accuracy)
# kappas > 0.75 as excellent, 0.40-0.75 as fair to good, and < 0.40 as poor.
# for Acc > NIR (no information rate = majority baseline)
# https://stats.stackexchange.com/questions/154479/definition-of-p-value-in-carets-confusion-matrix-method


### Analysis with only /a/

tree_a <- ctree(pos_dismo_large
                  ~ duration # to add the duration of segments as random variable
                  + `mean_F0(Hz)` # to add the mean F0 of segments as random variable
                  + `mean_F1(Hz)` # to add the mean F1 of segments as random variable
                  + `mean_F2(Hz)` # to add the mean F2 of segments as random variable
                  + `mean_HNR(dB)` # to add the mean HNR of segments as random variable
                  + speaker_gender
                  + speaker_nationality
                  + formality
                  + label,
                  #+ name_textgrid_file, 
                  data=DM_a %>% mutate(pos_dismo_large = as.factor(pos_dismo_large),
                                         `mean_F0(Hz)` = as.numeric(`mean_F0(Hz)`),
                                         `mean_F1(Hz)` = as.numeric(`mean_F1(Hz)`),
                                         `mean_F2(Hz)` = as.numeric(`mean_F2(Hz)`),
                                         `mean_HNR(dB)` = as.numeric(`mean_HNR(dB)`),
                                         speaker_gender = as.factor(speaker_gender),
                                         speaker_nationality = as.factor(speaker_nationality),
                                         formality = as.factor(formality),
                                         label = as.factor(label)
                                         #name_textgrid_file = as.factor(name_textgrid_file)
                  ))
plot(tree_a)

# To test the accuracy of the tree

output.inf.tree <- cbind(predict(tree_a) %>% as.character(),
                         DM_a$pos_dismo_large %>% as.character()) %>%
  as.data.frame() %>%
  rename(Prediction = 1, Actual = 2) %>%
  mutate(Performance = case_when(Prediction == Actual ~ "Correct",
                                 Prediction != Actual ~ "Wrong"))

#the columns are the real/reference values, the rows are the predicted values
caret::confusionMatrix(data = as.factor(output.inf.tree$Prediction),
                       reference = as.factor(output.inf.tree$Actual),
                       # select the metrics we want to show
                       mode = "prec_recall")
# for kappa https://stats.stackexchange.com/questions/82162/cohens-kappa-in-plain-english
# Kappa = (observed accuracy - expected accuracy)/(1 - expected accuracy)
# kappas > 0.75 as excellent, 0.40-0.75 as fair to good, and < 0.40 as poor.
# for Acc > NIR (no information rate = majority baseline)
# https://stats.stackexchange.com/questions/154479/definition-of-p-value-in-carets-confusion-matrix-method


###################################

### Plots

# To map vowels on a 2-dimensional F1/F2 chart according to their PoS

# call packages
library(ggplot2) # for plots
library(hrbrthemes) # for plots

f1_V <- as.numeric(DM_a_k$`mean_F1(Hz)`) # for the values in the column "mean_F1(Hz)" to be considered as numerical values
f2_V <- as.numeric(DM_a_k$`mean_F2(Hz)`) # for the values in the column "mean_F2(Hz)" to be considered as numerical values

PoSxF1_F2_ak <- ggplot(DM_a_k, aes(x=f1_V, y=f2_V, shape=DM_a_k$label, color=DM_a_k$pos_dismo_large)) + 
  geom_point(size=3) +
  xlab("F1 (Hz)") +
  ylab("F2 (Hz)") +
  theme_ipsum() +
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(legend.text = element_text(size=15)) +
  labs(color="PoS") +
  labs(shape="label") +
  geom_smooth(method = "lm", se = FALSE)
PoSxF1_F2_ak # to see the plot


### Analysis with only /a/

f1_a <- as.numeric(DM_a$`mean_F1(Hz)`) # for the values in the column "mean_F1(Hz)" to be considered as numerical values
f2_a <- as.numeric(DM_a$`mean_F2(Hz)`) # for the values in the column "mean_F2(Hz)" to be considered as numerical values

PoSxF1_F2_a <- ggplot(DM_a, aes(x=f1_a, y=f2_a, color=DM_a$pos_dismo_large)) + 
  geom_point(size=3) +
  xlab("F1 (Hz)") +
  ylab("F2 (Hz)") +
  theme_ipsum() +
  theme(axis.title.x = element_text(size=12)) +
  theme(axis.title.y = element_text(size=12)) +
  theme(legend.text = element_text(size=15)) +
  labs(color="PoS") +
  geom_smooth(method = "lm", se = FALSE)
PoSxF1_F2_a # to see the plot


#############################################

#Data wrangling

#Basic settings, e.g., we read the basic packages and set the seed for reproducibility. The most relevant is the tidyverse work environment. See https://www.tidyverse.org/ for more details.
## ```{r message=FALSE, warning=FALSE}
# remove scientific notations such as 10000=1e04
options(scipen=3) 
# remove basic settings of factors
options(stringsAsFactors = FALSE)
# settings for plots
options(ggrepel.max.overlaps = Inf)
# set seed for reproducibility
set.seed(101) 
# install non-installed packages
install.packages("ggplot")
install.packages("ggplot2")
install.packages("ggfortify")
# read the packages for plot
library(tidyverse)
library(ggplot2)
library(GGally)
library(ggfortify)
library(ggrepel)
# read the package for Excel files
library(readxl)
# read the packages for clustering
library(cluster)
library(factoextra)
## ```

# read the data
#data <- read.csv("C:/Users/mhutin/Documents/PPaDisM/IS_2023_VOWELS.csv") %>%
data <- DM_a_k %>%
  # keep relevant variables and rename
  select(pos_dismo = pos_dismo_large, duration, 
         meanF0 = `mean_F0(Hz)`, meanF1 = `mean_F1(Hz)`, meanF2 = `mean_F2(Hz)`,
         meanHNR = `mean_HNR(dB)`, file = name_textgrid_file) %>%
  # change to numeric when needed
  mutate(meanF0 = as.numeric(meanF0)) %>%
  mutate(meanF1 = as.numeric(meanF1)) %>%
  mutate(meanF2 = as.numeric(meanF2)) %>%
  mutate(meanHNR = as.numeric(meanHNR)) %>%
  # change to factor when needed
  mutate(pos_dismo = as.factor(pos_dismo)) %>%
  # remove NAs
  drop_na() %>%
  # add a row for IDs
  mutate(ID = row_number())
# visual check
glimpse(data)

#Look at the distribution of variables
data$pos_dismo %>% table


# First, we try to visualize how the data points are clustered or not according to the considered variables. For that, we do a PCA (Principal component analysis). Each point represents a data point. The arrows indicate the influence of the variables. The length of the arrows indicate the magnitude of the effect for each variable.
## ```{r fig.width=6, fig.height=6}
# keep the columns with the numeric values for the PCA
con_data <- data  %>%
  # remove columns with metadata
  select(-c(pos_dismo, file, ID)) %>%
  # change to matrix
  as.matrix()
# specify row names for the plot later
rownames(con_data) <- data$ID
# make the plot
autoplot(prcomp(con_data),
         data = data,
         # specify the color of the labels
         colour = 'pos_dismo',
         # specify the size of the labels
         label = FALSE, 
         label.size = 3,
         shape = TRUE,
         size = 2,
         # adding the loading arrows labels
         loadings = TRUE, 
         loadings.label = TRUE,
         # specifying the colors for the arrows and labels
         loadings.colour = c('gray'),
         loadings.label.colour = "blue",
         # specifying the size of the loadings
         loadings.label.size = 4,
         loadings.label.vjust = 1.3,
         # avoid label overlap with ggfortify
         loadings.label.repel=T,
         frame = FALSE) + 
  # use color-blind palette if needed
  #scale_color_viridis_d(alpha = 0.6) +
  theme_bw() +
  #guides(colour = guide_legend(override.aes = list(size = 3))) +
  # basic plot settings
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.title=element_text(size=12),
        legend.text = element_text(size = 12),
        title = element_text(size = 30),
        legend.title = element_blank())


# We can make a correlation plot of all the continuous variables we are looking at. The correlation coefficient r can be interpreted as follows:
# - measure of linear relationship
# - between -1.0 (perfectly negative) and +1.0 (perfectly positive)
# - 0 means no (linear) relationship

# The visual can show us which variables are interacting with each other. For example, if some variables are highly correlated with each other.
data %>%
  # remove the metadata
  select(-c(pos_dismo, file, ID)) %>%
  # make the plot
  ggpairs(lower=list(continuous=wrap("smooth", colour="black")),
          upper = list(continuous = wrap("cor", size=4, colour = "black"))) +
  # basic plot settings
  theme(strip.text = element_text(size = 8),
        axis.text = element_text(size = 8))

#We can also try to cluster the languages based on the feature values they have. We currently use hierarchical clustering. This will normally also reflect the PCA visualization. The output can also be compared with the family grouping of the languages.
# generate the clusters
hclust_avg <- hclust(daisy(con_data))
# plot the clusters
plot(hclust_avg)
# add squares around the clusters
rect.hclust(hclust_avg , k = 4, border = 1:4)

# In the current preliminary analysis, we set the number of clusters to the same amount of group, which is 3 (i.e., Adv, CC, and SC). However, we can also see what would be the mathematically ideal number of clusters.
# see how many clusters is ideal
fviz_nbclust(con_data, FUN = hcut, method = "silhouette") 
# The answer is 3.

#Now, we plot the generated clusters. We plot the labels based on their affiliated group in the data.
#```{r fig.height=10}
# cut the data into clusters
clus = cutree(hclust_avg, 3)

test <- clus %>%
  # change format of the data
  as.data.frame() %>%
  # rename the column
  rename(Cluster = 1) %>%
  # add the language names
  mutate(Name = names(clus)) %>%
  # add the metadata
  merge(data %>% select(Name = ID, file, pos_dismo), by = "Name", all.x = TRUE) %>%
  # add a column for the color of the groups
  mutate(Color = case_when(pos_dismo == "Adv" ~ "red",
                           pos_dismo == "CC" ~ "yellow",
                           pos_dismo == "Ij" ~ "green",
                           pos_dismo == "SC" ~ "blue"))

# match the order of rows in the data with the order of tip labels
test <- test[match(names(clus), test$Name),]
# make a plot
plot(ape::as.phylo(hclust_avg), 
     # change the visualization type if needed
     #type = "fan",
     tip.color = test$Color,
     # plot settings
     label.offset = 0.01, cex = 0.7, no.margin = TRUE)

# We can then compare the clusters and the actual groups

# Get principal component vectors for 1-9
pc <- prcomp(con_data)
# First four principal components
pc <- data.frame(pc$x[,1:4])

# add PCA coordinates to the data
test$PC1 <- pc$PC1
test$PC2 <- pc$PC2

#getting the convex hull of each unique point set
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]
hulls <- plyr::ddply(test, "Cluster", find_hull)
hulls <- hulls %>% mutate(Cluster = factor(Cluster))

test %>%
  mutate(Cluster = factor(Cluster)) %>%
  ggplot(aes(x = PC1, y = PC2)) +
  #ggrepel::geom_text_repel(aes(label = Language, color = Family),
  #                         max.overlaps = 20) +
  geom_point(aes(color = pos_dismo)) +
  scale_color_manual(values = c("red","yellow","green","blue")) +
  theme_bw() +
  xlab("PC1 (86.62%)") +
  theme(axis.title.x = element_text(size=12)) +
  ylab("PC2 (12.10%)") +
  theme(axis.title.y = element_text(size=12)) +
  geom_polygon(data=hulls, aes(fill=Cluster), alpha = 0.3) +
  scale_fill_manual(values = c("red","darkgreen","darkblue","yellow")) +
  theme(legend.text = element_text(size=15)) +
  labs(color="PoS")
# save the figure
#ggsave('Figures/Cluster_1to9.png', width = 7, height = 5, dpi = 600)


#To evaluate the performance across the clusters, we assess how similar the clusters are with the original pos_dismo. To do so, we use the Rand Index and the Adjusted Rand Index. The adjusted Rand Index (ARI) should be interpreted as follows: ARI >= 0.90 excellent recovery; 0.80 =< ARI < 0.90 good recovery; 0.65 =< ARI < 0.80 moderate recovery; ARI < 0.65 poor recovery.
install.packages("ClusterR")
suppressPackageStartupMessages(library(ClusterR))
external_validation(test$Cluster %>% as.numeric(),
                    test$pos_dismo %>% as.numeric(),
                    summary_stats = T)

## Extracting distances

# We can extract the pairwise distance between the points of the data set. First, we can visualize the distances in a two-dimensional space. Normally, we expect that this visualization matches the output of the PCA.
# change the content to a distance matrix
distances <- con_data %>% dist(method = "euclidean")

# change the distances to multidimensional scaling
fit <- cmdscale(distances, 
                eig = TRUE, 
                # set number of dimensions
                k = 2) 

# extract the two dimensions
cbind(fit$points[,1], fit$points[,2]) %>%
  # change the formant
  as.data.frame() %>%
  # rename the columns
  rename(x = 1, y = 2) %>%
  # add metadata
  mutate(Name = data$ID,
         pos_dismo = data$pos_dismo) %>%
  # make the plot
  ggplot(aes(x = x, y = y, label = Name, color = pos_dismo)) +
  geom_point() +
  # add text instead of points
  #geom_text_repel() +
  # white background
  theme_bw() +
  # basic plot settings
  theme(legend.position = "top") +
  # manually set the colors of the text labels
  scale_color_manual(values = c("red","blue","green","orange","black","purple",
                                "pink"))


# Then, we can also extract the pairwise distance between and across groups of pos_dismo.
# identify the points of each group
ADV <- data %>% filter(pos_dismo == "Adv") %>% pull(ID)
CC <- data %>% filter(pos_dismo == "CC") %>% pull(ID)
SC <- data %>% filter(pos_dismo == "SC") %>% pull(ID)
IJ <- data %>% filter(pos_dismo=="Ij") %>% pull(ID)

distances %>%
  # change formant
  as.matrix() %>%
  reshape2::melt() %>%
  # keep pairs with different languages
  filter(Var1 != Var2) %>%
  # identify the groups
  mutate(Group = case_when(#Var1 %in% ADV & Var2 %in% ADV ~ "ADV",
    #Var1 %in% CC & Var2 %in% CC ~ "CC",
    #Var1 %in% IJ & Var2 %in% IJ ~ "IJ",
    #Var1 %in% SC & Var2 %in% SC ~ "SC",
    Var1 %in% ADV & Var2 %in% CC ~ "ADV_CC",
    Var1 %in% CC & Var2 %in% ADV ~ "ADV_CC",
    Var1 %in% ADV & Var2 %in% IJ ~ "ADV_IJ",
    Var1 %in% IJ & Var2 %in% ADV ~ "ADV_IJ",
    Var1 %in% ADV & Var2 %in% SC ~ "ADV_SC",
    Var1 %in% SC & Var2 %in% ADV ~ "ADV_SC",
    Var1 %in% CC & Var2 %in% IJ ~ "CC_IJ",
    Var1 %in% IJ & Var2 %in% CC ~ "CC_IJ",
    Var1 %in% SC & Var2 %in% CC ~ "CC_SC",
    Var1 %in% CC & Var2 %in% SC ~ "CC_SC",
    Var1 %in% IJ & Var2 %in% SC ~ "IJ_SC",
    Var1 %in% SC & Var2 %in% IJ ~ "IJ_SC")) %>% 
  # keep pairs within the same group
  filter(!is.na(Group)) %>%
  # make the plot
  ggplot(aes(x = Group, y = value)) +
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  # expand the y axis for the significance plot following
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  # add significance testing
  ggpubr::stat_compare_means(comparisons = list(c("ADV_CC","ADV_IJ"),
                                                c("ADV_CC","ADV_SC"),
                                                c("ADV_IJ","ADV_SC"),
                                                c("ADV_CC","CC_IJ"),
                                                c("ADV_CC","CC_SC"),
                                                c("ADV_IJ", "CC_IJ"),
                                                c("ADV_IJ","IJ_SC"),
                                                c("CC_IJ","CC_SC"),
                                                c("CC_IJ","IJ_SC")),
                             aes(label=..p.adj..), p.adjust.methods = "bonferroni",
                             method = "t.test", label = "p.signif", 
                             size = 7, hide.ns = FALSE) +
  stat_summary(fun = mean, colour="black", geom="text", size = 5,
               show.legend = FALSE, 
               vjust = 4, hjust = 1.5,
               aes( label=round(..y.., digits=0)))


### Analysis on only /a/

# read the data
#data <- read.csv("C:/Users/mhutin/Documents/PPaDisM/IS_2023_VOWELS.csv") %>%
data <- DM_a %>%
  # keep relevant variables and rename
  select(pos_dismo = pos_dismo_large, duration, 
         meanF0 = `mean_F0(Hz)`, meanF1 = `mean_F1(Hz)`, meanF2 = `mean_F2(Hz)`,
         meanHNR = `mean_HNR(dB)`, file = name_textgrid_file) %>%
  # change to numeric when needed
  mutate(meanF0 = as.numeric(meanF0)) %>%
  mutate(meanF1 = as.numeric(meanF1)) %>%
  mutate(meanF2 = as.numeric(meanF2)) %>%
  mutate(meanHNR = as.numeric(meanHNR)) %>%
  # change to factor when needed
  mutate(pos_dismo = as.factor(pos_dismo)) %>%
  # remove NAs
  drop_na() %>%
  # add a row for IDs
  mutate(ID = row_number())
# visual check
glimpse(data)

#Look at the distribution of variables
data$pos_dismo %>% table


# First, we try to visualize how the data points are clustered or not according to the considered variables. For that, we do a PCA (Principal component analysis). Each point represents a data point. The arrows indicate the influence of the variables. The length of the arrows indicate the magnitude of the effect for each variable.
## ```{r fig.width=6, fig.height=6}
# keep the columns with the numeric values for the PCA
con_data <- data  %>%
  # remove columns with metadata
  select(-c(pos_dismo, file, ID)) %>%
  # change to matrix
  as.matrix()
# specify row names for the plot later
rownames(con_data) <- data$ID
# make the plot
autoplot(prcomp(con_data),
         data = data,
         # specify the color of the labels
         colour = 'pos_dismo',
         # specify the size of the labels
         label = FALSE, 
         label.size = 3,
         shape = TRUE,
         size = 2,
         # adding the loading arrows labels
         loadings = TRUE, 
         loadings.label = TRUE,
         # specifying the colors for the arrows and labels
         loadings.colour = c('gray'),
         loadings.label.colour = "blue",
         # specifying the size of the loadings
         loadings.label.size = 4,
         loadings.label.vjust = 1.3,
         # avoid label overlap with ggfortify
         loadings.label.repel=T,
         frame = FALSE) + 
  # use color-blind palette if needed
  #scale_color_viridis_d(alpha = 0.6) +
  theme_bw() +
  #guides(colour = guide_legend(override.aes = list(size = 3))) +
  # basic plot settings
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.title=element_text(size=12),
        legend.text = element_text(size = 12),
        title = element_text(size = 30),
        legend.title = element_blank())


# We can make a correlation plot of all the continuous variables we are looking at. The correlation coefficient r can be interpreted as follows:
# - measure of linear relationship
# - between -1.0 (perfectly negative) and +1.0 (perfectly positive)
# - 0 means no (linear) relationship

# The visual can show us which variables are interacting with each other. For example, if some variables are highly correlated with each other.
data %>%
  # remove the metadata
  select(-c(pos_dismo, file, ID)) %>%
  # make the plot
  ggpairs(lower=list(continuous=wrap("smooth", colour="black")),
          upper = list(continuous = wrap("cor", size=4, colour = "black"))) +
  # basic plot settings
  theme(strip.text = element_text(size = 8),
        axis.text = element_text(size = 8))

#We can also try to cluster the languages based on the feature values they have. We currently use hierarchical clustering. This will normally also reflect the PCA visualization. The output can also be compared with the family grouping of the languages.
# generate the clusters
hclust_avg <- hclust(daisy(con_data))
# plot the clusters
plot(hclust_avg)
# add squares around the clusters
rect.hclust(hclust_avg , k = 4, border = 1:4)

# In the current preliminary analysis, we set the number of clusters to the same amount of group, which is 3 (i.e., Adv, CC, and SC). However, we can also see what would be the mathematically ideal number of clusters.
# see how many clusters is ideal
fviz_nbclust(con_data, FUN = hcut, method = "silhouette") 
# The answer is 2.

#Now, we plot the generated clusters. We plot the labels based on their affiliated group in the data.
#```{r fig.height=10}
# cut the data into clusters
clus = cutree(hclust_avg, 2)

test <- clus %>%
  # change format of the data
  as.data.frame() %>%
  # rename the column
  rename(Cluster = 1) %>%
  # add the language names
  mutate(Name = names(clus)) %>%
  # add the metadata
  merge(data %>% select(Name = ID, file, pos_dismo), by = "Name", all.x = TRUE) %>%
  # add a column for the color of the groups
  mutate(Color = case_when(pos_dismo == "Adv" ~ "red",
                           pos_dismo == "CC" ~ "yellow",
                           pos_dismo == "Ij" ~ "green",
                           pos_dismo == "SC" ~ "blue"))

# match the order of rows in the data with the order of tip labels
test <- test[match(names(clus), test$Name),]
# make a plot
plot(ape::as.phylo(hclust_avg), 
     # change the visualization type if needed
     #type = "fan",
     tip.color = test$Color,
     # plot settings
     label.offset = 0.01, cex = 0.7, no.margin = TRUE)

# We can then compare the clusters and the actual groups

# Get principal component vectors for 1-9
pc <- prcomp(con_data)
# First four principal components
pc <- data.frame(pc$x[,1:4])

# add PCA coordinates to the data
test$PC1 <- pc$PC1
test$PC2 <- pc$PC2

#getting the convex hull of each unique point set
find_hull <- function(df) df[chull(df$PC1, df$PC2), ]
hulls <- plyr::ddply(test, "Cluster", find_hull)
hulls <- hulls %>% mutate(Cluster = factor(Cluster))

test %>%
  mutate(Cluster = factor(Cluster)) %>%
  ggplot(aes(x = PC1, y = PC2)) +
  #ggrepel::geom_text_repel(aes(label = Language, color = Family),
  #                         max.overlaps = 20) +
  geom_point(aes(color = pos_dismo)) +
  scale_color_manual(values = c("red","yellow","green","blue")) +
  theme_bw() +
  xlab("PC1 (86.62%)") +
  theme(axis.title.x = element_text(size=12)) +
  ylab("PC2 (12.10%)") +
  theme(axis.title.y = element_text(size=12)) +
  geom_polygon(data=hulls, aes(fill=Cluster), alpha = 0.3) +
  scale_fill_manual(values = c("red","darkgreen","darkblue","yellow")) +
  theme(legend.text = element_text(size=15)) +
  labs(color="PoS")
# save the figure
#ggsave('Figures/Cluster_1to9.png', width = 7, height = 5, dpi = 600)


#To evaluate the performance across the clusters, we assess how similar the clusters are with the original pos_dismo. To do so, we use the Rand Index and the Adjusted Rand Index. The adjusted Rand Index (ARI) should be interpreted as follows: ARI >= 0.90 excellent recovery; 0.80 =< ARI < 0.90 good recovery; 0.65 =< ARI < 0.80 moderate recovery; ARI < 0.65 poor recovery.
install.packages("ClusterR")
suppressPackageStartupMessages(library(ClusterR))
external_validation(test$Cluster %>% as.numeric(),
                    test$pos_dismo %>% as.numeric(),
                    summary_stats = T)

## Extracting distances

# We can extract the pairwise distance between the points of the data set. First, we can visualize the distances in a two-dimensional space. Normally, we expect that this visualization matches the output of the PCA.
# change the content to a distance matrix
distances <- con_data %>% dist(method = "euclidean")

# change the distances to multidimensional scaling
fit <- cmdscale(distances, 
                eig = TRUE, 
                # set number of dimensions
                k = 2) 

# extract the two dimensions
cbind(fit$points[,1], fit$points[,2]) %>%
  # change the formant
  as.data.frame() %>%
  # rename the columns
  rename(x = 1, y = 2) %>%
  # add metadata
  mutate(Name = data$ID,
         pos_dismo = data$pos_dismo) %>%
  # make the plot
  ggplot(aes(x = x, y = y, label = Name, color = pos_dismo)) +
  geom_point() +
  # add text instead of points
  #geom_text_repel() +
  # white background
  theme_bw() +
  # basic plot settings
  theme(legend.position = "top") +
  # manually set the colors of the text labels
  scale_color_manual(values = c("red","blue","green","orange","black","purple",
                                "pink"))


# Then, we can also extract the pairwise distance between and across groups of pos_dismo.
# identify the points of each group
ADV <- data %>% filter(pos_dismo == "Adv") %>% pull(ID)
CC <- data %>% filter(pos_dismo == "CC") %>% pull(ID)
SC <- data %>% filter(pos_dismo == "SC") %>% pull(ID)
IJ <- data %>% filter(pos_dismo=="Ij") %>% pull(ID)

distances %>%
  # change formant
  as.matrix() %>%
  reshape2::melt() %>%
  # keep pairs with different languages
  filter(Var1 != Var2) %>%
  # identify the groups
  mutate(Group = case_when(#Var1 %in% ADV & Var2 %in% ADV ~ "ADV",
    #Var1 %in% CC & Var2 %in% CC ~ "CC",
    #Var1 %in% IJ & Var2 %in% IJ ~ "IJ",
    #Var1 %in% SC & Var2 %in% SC ~ "SC",
    Var1 %in% ADV & Var2 %in% CC ~ "ADV_CC",
    Var1 %in% CC & Var2 %in% ADV ~ "ADV_CC",
    Var1 %in% ADV & Var2 %in% IJ ~ "ADV_IJ",
    Var1 %in% IJ & Var2 %in% ADV ~ "ADV_IJ",
    Var1 %in% ADV & Var2 %in% SC ~ "ADV_SC",
    Var1 %in% SC & Var2 %in% ADV ~ "ADV_SC",
    Var1 %in% CC & Var2 %in% IJ ~ "CC_IJ",
    Var1 %in% IJ & Var2 %in% CC ~ "CC_IJ",
    Var1 %in% SC & Var2 %in% CC ~ "CC_SC",
    Var1 %in% CC & Var2 %in% SC ~ "CC_SC",
    Var1 %in% IJ & Var2 %in% SC ~ "IJ_SC",
    Var1 %in% SC & Var2 %in% IJ ~ "IJ_SC")) %>% 
  # keep pairs within the same group
  filter(!is.na(Group)) %>%
  # make the plot
  ggplot(aes(x = Group, y = value)) +
  geom_violin() +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  # expand the y axis for the significance plot following
  scale_y_continuous(expand = expansion(mult = c(0, .2))) +
  # add significance testing
  ggpubr::stat_compare_means(comparisons = list(c("ADV_CC","ADV_IJ"),
                                                c("ADV_CC","ADV_SC"),
                                                c("ADV_IJ","ADV_SC"),
                                                c("ADV_CC","CC_IJ"),
                                                c("ADV_CC","CC_SC"),
                                                c("ADV_IJ", "CC_IJ"),
                                                c("ADV_IJ","IJ_SC"),
                                                c("CC_IJ","CC_SC"),
                                                c("CC_IJ","IJ_SC")),
                             aes(label=..p.adj..), p.adjust.methods = "bonferroni",
                             method = "t.test", label = "p.signif", 
                             size = 7, hide.ns = FALSE) +
  stat_summary(fun = mean, colour="black", geom="text", size = 5,
               show.legend = FALSE, 
               vjust = 4, hjust = 1.5,
               aes( label=round(..y.., digits=0)))



