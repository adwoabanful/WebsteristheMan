---
  title: "Mobile Telecom Segmentation"
author: "Group Great"
output:
  html_document:
  css: AnalyticsStyles/default.css
theme: paper
toc: yes
toc_float:
  collapsed: no
smooth_scroll: yes
pdf_document:
  includes:
  in_header: AnalyticsStyles/default.sty
always_allow_html: yes
---
  
  ```{r setuplibraries, echo=FALSE, message=FALSE}
suppressWarnings(source("Library/library.R"))
#source("Library/heatmapOutput.R")
#source("Library/MartrixOperations.R")
#source("Library/allPublicLibraries.R")
# Package options
suppressWarnings(ggthemr('fresh'))  # ggplot theme
opts_knit$set(progress=FALSE, verbose=FALSE)
opts_chunk$set(echo=FALSE, fig.align="center", fig.width=10, fig.height=6.35, results="asis")
options(knitr.kable.NA = '')
```

#1) Context of the Project

We are using a database with mobile telecommunication consumption data.

The data belongs to a <b> mobile telecom company </b>that operates in an emerging market (name undisclosed to maintain confidentiality), and our focus will be exclusively on customers who buy prepaid packages (similar to Starhub prepaid packages)

The company uses an old customer segmentation to design service packages (voice, roaming, data, etc) and to target sales initiatives. However, there is the perception that <b> the segmentation is outdated and not working as effectively as it should.</b>
  <br>
  
  #2) The Business Question
  
  The objective is to create a new segmentation based on data of customer usage of the different services, so the company can better offer and design the right packages to the right customer segments. Our project's aim is to create that segmentation by answering the following question.

<b> What are the main customer segments based on historical usage of the different services (voice, data, etc)? </b>


#3) The Data

##3.1)   Treat and Load the Data

```{r setupdata1E, echo=TRUE, tidy=TRUE}
# Please ENTER the name of the file with the data used. The file should be a .csv with one row per observation (e.g. person) and one column per attribute. Do not add .csv at the end, make sure the data are numeric.
BDASmallTableClean = "Library/BDASmallTableNew.csv"

#>>>>>>> bfff9c1f85926b4164661820aa734f6312e97bf8 (WHAT IS THIS??)

# Please enter the minimum number below which you would like not to print - this makes the readability of the tables easier. Default values are either 10e6 (to print everything) or 0.5. Try both to see the difference.
MIN_VALUE = 0.5

# Please enter the maximum number of observations to show in the report and slides.
# DEFAULT is 10. If the number is large the report may be slow.
max_data_report = 10
```

```{r}
ProjectData <- read.csv(BDASmallTableClean)
ProjectData <- data.matrix(ProjectData)
ProjectData_INITIAL <- ProjectData

```

##3.2) Describe the Data

In this dataset, we have <b>`r nrow(ProjectData)` rows </b>and <b>`r ncol(ProjectData)` columns</b>. The rows indicate the number of customers we are dealing with in this sample and the columns describe those customers' usage pattern. 


#4) PART 1: Factor/Component selection

In this part we identify the underlying factors that best capture the behaviours on the various variables.

```{r setupfactor, echo=TRUE, tidy=TRUE}
# Please ENTER the original raw attributes to use.
# Please use numbers, not column names, e.g. c(1:5, 7, 8) uses columns 1,2,3,4,5,7,8
factor_attributes_used = c(3:ncol(ProjectData))

# Please ENTER the selection criterions for the factors to use.
# Choices: "eigenvalue", "variance", "manual"
factor_selectionciterion = "eigenvalue"

# Please ENTER the desired minumum variance explained
# (Only used in case "variance" is the factor selection criterion used).
minimum_variance_explained = 80  # between 1 and 100

# Please ENTER the number of factors to use
# (Only used in case "manual" is the factor selection criterion used).
manual_numb_factors_used = 10

# Please ENTER the rotation eventually used (e.g. "none", "varimax", "quatimax", "promax", "oblimin", "simplimax", and "cluster" - see help(principal)). Default is "varimax"
rotation_used = "varimax"

```

```{r}
factor_attributes_used <- intersect(factor_attributes_used, 1:ncol(ProjectData))
ProjectDataFactor <- ProjectData[1:nrow(ProjectData)-1,factor_attributes_used]
ProjectDataFactor <- ProjectData <- data.matrix(ProjectDataFactor)
```


## Steps 1-2: Check the Data

We are going to start by doing some basic visual exploration of a few data:
  
  ```{r}
rownames(ProjectDataFactor) <- paste0("Obs.", sprintf("%02i", 1:nrow(ProjectDataFactor)))
iprint.df(t(head(round(ProjectDataFactor, 2), max_data_report)), scale = TRUE)
```

The data we use here have the following descriptive statistics:
  
  ```{r}
iprint.df(round(my_summary(ProjectDataFactor), 2), scale = FALSE)
```

## Step 3: Check Correlations

This is the correlation matrix of the customer responses to the `r ncol(ProjectDataFactor)` attitude questions - which are the only questions that we will use for the segmentation (see the case):
  
  ```{r}
thecor = round(cor(ProjectDataFactor),2)
iprint.df(round(thecor,2), scale=TRUE)
```


## Step 4: Choose number of factors

The dataset recorded all possible types of usage that can be made by a customer. The data allows us to understand the underlying behavior of the customer by doing a factor analysis. The factors will identify the behaviors of customers based on their usage pattern. 


```{r}
# Principal Function
UnRotated_Results<-principal(ProjectDataFactor, nfactors=ncol(ProjectDataFactor), rotate="none",score=TRUE)
UnRotated_Factors<-round(UnRotated_Results$loadings,2)
UnRotated_Factors<-as.data.frame(unclass(UnRotated_Factors))
colnames(UnRotated_Factors)<-paste("Comp",1:ncol(UnRotated_Factors),sep="")
```

```{r}
# PCA Function
Variance_Explained_Table_results<-PCA(ProjectDataFactor, graph=FALSE)
Variance_Explained_Table<-Variance_Explained_Table_results$eig
Variance_Explained_Table_copy<-Variance_Explained_Table

rownames(Variance_Explained_Table) <- paste("Component", 1:nrow(Variance_Explained_Table), sep=" ")
colnames(Variance_Explained_Table) <- c("Eigenvalue", "Pct of explained variance", "Cumulative pct of explained variance")
```

Let's look at the **variance explained** as well as the **eigenvalues** :

```{r}
iprint.df(round(Variance_Explained_Table, 2), scale = FALSE)
```

```{r}
eigenvalues  <- Variance_Explained_Table[, "Eigenvalue"]
df           <- cbind(as.data.frame(eigenvalues), c(1:length(eigenvalues)), rep(1, length(eigenvalues)))
colnames(df) <- c("eigenvalues", "components", "abline")
iplot.df(melt(df, id="components"))
```

## Step 5: Interpret the factors

Let's now see how the factors look like:
  
  ```{r}
if (factor_selectionciterion == "eigenvalue")
  factors_selected = sum(Variance_Explained_Table_copy[,1] >= 1)
if (factor_selectionciterion == "variance")
  factors_selected = 1:head(which(Variance_Explained_Table_copy[,"cumulative percentage of variance"]>= minimum_variance_explained),1)
if (factor_selectionciterion == "manual")
  factors_selected = manual_numb_factors_used
```

To better visualize them, we will use what is called a "rotation". In this case we selected the `r rotation_used` rotation. For our data, the `r factors_selected` selected factors look as follows after this rotation:
  
  ```{r}
Rotated_Results<-principal(ProjectDataFactor, nfactors=max(factors_selected), rotate=rotation_used,score=TRUE)
Rotated_Factors<-round(Rotated_Results$loadings,2)
Rotated_Factors<-as.data.frame(unclass(Rotated_Factors))
colnames(Rotated_Factors)<-paste("Comp.",1:ncol(Rotated_Factors),sep="")

sorted_rows <- sort(Rotated_Factors[,1], decreasing = TRUE, index.return = TRUE)$ix
Rotated_Factors <- Rotated_Factors[sorted_rows,]

iprint.df(Rotated_Factors, scale=TRUE)
```

To better visualize and interpret the factors we often "suppress" loadings with small values, e.g. with absolute values smaller than 0.5. In this case our factors look as follows after suppressing the small numbers:
  
  ```{r}
Rotated_Factors_thres <- Rotated_Factors
Rotated_Factors_thres[abs(Rotated_Factors_thres) < MIN_VALUE]<-NA
colnames(Rotated_Factors_thres)<- colnames(Rotated_Factors)
rownames(Rotated_Factors_thres)<- rownames(Rotated_Factors)

iprint.df(Rotated_Factors_thres, scale=TRUE)
```

To understand if there was any redundancy between factors (i.e. high correlation) we took a look at correlation between the rotated factors 
```{r}
factcor = round(cor(Rotated_Factors),2)
iprint.df(round(factcor,2), scale=TRUE)
```

## Step 6:  Save factor scores

We can now either replace all initial variables used in this part with the factors scores or just select one of the initial variables for each of the selected factors in order to represent that factor. The factor scores look like the following for the first customers in our dataset:
  
  ```{r}
NEW_ProjectData <- round(Rotated_Results$scores[1:nrow(Rotated_Results$scores)-1,1:factors_selected,drop=F],2)
colnames(NEW_ProjectData)<-paste(c("Offnet Voice + SMS ( < 55%)", "Recharge", "Data Packages", "Out Roaming", "OG Intl Voice", "OG Offnet SMS + Intl IC SMS ( < 55%)", "Voice Package", "VAS Package","OG Intl SMS, SMS Package", "Pay-as-you-go Data", "OG SMS Onnet", "Content Provider VAS Package", "Blackberry Package", "Games", "IVR Shortcodes", "Onnet Voice OG", "IC Intl Voice", "OG Airtime Gifts", "Active Days", "IC Airtime Gifts"),1:ncol(NEW_ProjectData),sep=" ")

iprint.df(t(head(NEW_ProjectData, 10)), scale=TRUE)
```


<hr>\clearpage


#5)         PART 2: Segmentation



```{r setupcluster, echo=TRUE, tidy=TRUE}
# Please ENTER then original raw attributes to use for the segmentation (the "segmentation attributes")
# Please use numbers, not column names, e.g. c(1:5, 7, 8) uses columns 1,2,3,4,5,7,8
segmentation_attributes_used = NEW_ProjectData[1:ncol(NEW_ProjectData)] #c(10,19,5,12,3) 

# Please ENTER the number of clusters to eventually use for this report
numb_clusters_used = 7 #Use this to "play" with the data until finding a suitable segmentation

# Please enter the method to use for the segmentation:
profile_with = "kmeans" #"hclust" or "kmeans"

# Please ENTER the distance metric eventually used for the clustering in case of hierarchical clustering 
# (e.g. "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski" - see help(dist)). 
# DEFAULT is "euclidean"
distance_used = "manhattan"

# Please ENTER the hierarchical clustering method to use (options are:
# "ward", "single", "complete", "average", "mcquitty", "median" or "centroid").
# DEFAULT is "ward"
hclust_method = "ward.D"

# Please ENTER the kmeans clustering method to use (options are:
# "Hartigan-Wong", "Lloyd", "Forgy", "MacQueen").
# DEFAULT is "Lloyd"
kmeans_method = "Lloyd"

```

```{r}
# Same as the initial data
ProjectData <- ProjectData_INITIAL

segmentation_attributes_used <- intersect(segmentation_attributes_used, 1:ncol(ProjectData))
#profile_attributes_used <- intersect(profile_attributes_used, 1:ncol(ProjectData))

ProjectData_segment <- ProjectData[,segmentation_attributes_used]
#ProjectData_profile <- ProjectData[,profile_attributes_used]

#ProjectData_scaled <- apply(ProjectData, 2, function(r) if (sd(r)!=0) (r-mean(r))/sd(r) else 0*r)
```

##Step 1: Decide on pair-wise distance metric

We need to define a distance metric that measures how different people (observations in general) are from each other. This can be an important choice. Here are the differences between the observations using the distance metric we selected:
  
  ```{r}
euclidean_pairwise <- as.matrix(dist(head(NEW_ProjectData, max_data_report), method="manhattan"))
euclidean_pairwise <- euclidean_pairwise*lower.tri(euclidean_pairwise) + euclidean_pairwise*diag(euclidean_pairwise) + 10e10*upper.tri(euclidean_pairwise)
euclidean_pairwise[euclidean_pairwise==10e10] <- NA
rownames(euclidean_pairwise) <- colnames(euclidean_pairwise) <- sprintf("Obs.%02d", 1:max_data_report)

iprint.df(round(euclidean_pairwise))
```
<<<<<<< HEAD


=======
  
  We can see the histogram of, say, the first 2 variables

```{r}
variables_to_plot = 1:2
do.call(iplot.grid, lapply(variables_to_plot, function(n){
  iplot.hist(NEW_ProjectData[, n], breaks=5, xlab = paste("Variable", n))
}))
```

>>>>>>> 8aa7b416b736fd0f9729206ec331b6d0bda85d48

##Step 2: Segmentation and Robustness Analysis

###i.     Segmentation/Robustness Analysis


We need to select the clustering method to use, as well as the number of clusters. It may be useful to see the dendrogram from Hierarchical Clustering of a sample of the factored data, to have a quick idea of how the data may be segmented and how many segments there may be. **Not that there is vector size limit to hclust method, thus we use a sample of, say, a 1000 observations**. 
<br>
  Here is the dendrogram for our data:
  
  ```{r}
Hierarchical_Cluster_distances <- dist(NEW_ProjectData[1:1000,], method=distance_used)
Hierarchical_Cluster <- hclust(Hierarchical_Cluster_distances, method=hclust_method)
# Display dendogram
iplot.dendrogram(Hierarchical_Cluster)
# TODO: Draw dendogram with red borders around the 3 clusters
#rect.hclust(Hierarchical_Cluster, k=numb_clusters_used, border="red") 
``` 

We can also plot the "distances" traveled before we need to merge any of the lower and smaller in size clusters into larger ones - the heights of the tree branches that link the clusters as we traverse the tree from its leaves to its root. If we have n observations, this plot has n-1 numbers, we see the first 20 here. 
```{r}
num <- nrow(NEW_ProjectData[1:1000,]) - 1
df1 <- cbind(as.data.frame(Hierarchical_Cluster$height[length(Hierarchical_Cluster$height):1]), c(1:num))
colnames(df1) <- c("distances","index")
iplot.df(melt(head(df1, 20), id="index"), xlab="Number of Components")
``` 

Here is the segment membership of the first `r max_data_report` respondents if we use hierarchical clustering:
  
  ```{r}
cluster_memberships_hclust <- as.vector(cutree(Hierarchical_Cluster, k=numb_clusters_used)) 
cluster_ids_hclust=unique(cluster_memberships_hclust)

ProjectData_with_hclust_membership <- cbind(1:length(cluster_memberships_hclust),cluster_memberships_hclust)
colnames(ProjectData_with_hclust_membership)<-c("Observation Number","Cluster_Membership")

iprint.df(round(head(ProjectData_with_hclust_membership, max_data_report), 2))
```

while this is the segment membership if we use k-means:
  
  ```{r}
kmeans_clusters <- kmeans(NEW_ProjectData,centers= numb_clusters_used, iter.max=2000, algorithm=kmeans_method)

ProjectData_with_kmeans_membership <- cbind(1:length(kmeans_clusters$cluster),kmeans_clusters$cluster)
colnames(ProjectData_with_kmeans_membership)<-c("Observation Number","Cluster_Membership")

iprint.df(round(head(ProjectData_with_kmeans_membership, max_data_report), 2))
```

#c.          Step 3: Profile and interpret the above defined segments
<br>
  This is the final output. The list of segments and their definitions
Having decided (for now) how many clusters to use, we would like to get a better understanding of who the customers in those clusters are and interpret the segments. 

> Data analytics is used to eventually make decisions, and that is feasible only when we are comfortable (enough) with our understanding of the analytics results, including our ability to clearly interpret them. 

To this purpose, one needs to spend time visualizing and understanding the data within each of the selected segments. For example, one can see how the summary statistics (e.g. averages, standard deviations, etc) of the **profiling attributes** differ across the segments. 

In our case, assuming we decided we use the `r numb_clusters_used` segments found using `r profile_with` as outlined above (similar profiling can be done with the results of other segmentation methods), we can see how the responses to our survey differ across segments. The average values of our data for the total population as well as within each customer segment are:
  
  ```{r}
cluster_memberships_kmeans <- kmeans_clusters$cluster 
cluster_ids_kmeans <- unique(cluster_memberships_kmeans)

if (profile_with == "hclust"){
  cluster_memberships <- cluster_memberships_hclust
  cluster_ids <-  cluster_ids_hclust  
}
if (profile_with == "kmeans"){
  cluster_memberships <- cluster_memberships_kmeans
  cluster_ids <-  cluster_ids_kmeans
}

# SAVE THE DATA in the cluster file
NewData = matrix(cluster_memberships,ncol=1)
write.csv(NewData,file=cluster_file)

population_average = matrix(apply(ProjectData_segment, 2, mean), ncol=1)
colnames(population_average) <- "Population"
Cluster_Profile_mean <- sapply(sort(cluster_ids), function(i) apply(ProjectData_segment[(cluster_memberships==i), ], 2, mean))
if (ncol(ProjectData_segment) <2)
  Cluster_Profile_mean=t(Cluster_Profile_mean)
colnames(Cluster_Profile_mean) <- paste("Segment", 1:length(cluster_ids), sep=" ")
cluster.profile <- cbind (population_average,Cluster_Profile_mean)

knitr::kable(round(cluster.profile, 2))
```

```{r}
population_average_matrix <- population_average[,"Population",drop=F] %*% matrix(rep(1,ncol(Cluster_Profile_mean)),nrow=1)
cluster_profile_ratios <- (ifelse(population_average_matrix==0, 0,Cluster_Profile_mean/population_average_matrix-1))
colnames(cluster_profile_ratios) <- paste("Segment", 1:ncol(cluster_profile_ratios), sep=" ")
rownames(cluster_profile_ratios) <- colnames(ProjectData)[profile_attributes_used]
## printing the result in a clean-slate table
knitr::kable(round(cluster_profile_ratios, 2))
``