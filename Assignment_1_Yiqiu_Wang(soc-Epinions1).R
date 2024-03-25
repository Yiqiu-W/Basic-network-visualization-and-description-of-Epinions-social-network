# The data set used is Epinions social network(soc-Epinions1), a who-trust-whom online social network(directed) of a general consumer review site Epinions.com. 
# Epinions.com was a consumer review site in which users could view reviews about various products. Members of the site can decide whether to "trust" each other. 
# All the trust relationships interact and form the Web of Trust which is then combined with review ratings to determine which reviews are shown to the users.
# please note, the data set is very big, so you can just work with a subset of it (in case it should melt your computer)

rm(list=ls())

library(sna) 
library(igraph)
library(ggplot2) 
library(data.table)
library(gt)

# 1 
# Write an R-script that reads your data. Transform your data to a data frame. 
# Argue about the format you have chosen to analyze large network data.

# Read the txt file
ep <- read.table("C:/Users/wangy/Documents/soc-Epinions1.txt", header = TRUE)
head(ep,5)
# Is it a data frame
is.data.frame(ep)

# Answers:
# The original txt file is downloaded from http://snap.stanford.edu/data/soc-Epinions1.html. We use read.table() in R to read the data. Then we have a data frame with two columns "FromNodeId" and "ToNodeId" and we name the data frame "ep" here. In the data frame, for example, a "FromNodeId" 0 connected to a "ToNodeId" 4 means that the user with ID 0 trusted the user with ID 4.
# We use a data frame here so that we could refer to column names more conveniently and the data frame here clearly represented the edge information. Data frames can contain different types(e.g. integers and characters) of data while matrices cannot. Although we do not have another data frame about node information(e.g. gender), using a data frame would just be easier.
# Since we will use ggplot2 R package for visualization later and ggplot() in general works with data frames, it is better that we start with a data frame produced by using read.table() instead of a matrix here until further requirements.


# 2 
# Make sure that before or while doing any calculations or plots that you handle nonresponse/ missing data meaningfully if there is any.

# Is there Na in ep
which(is.na(ep), arr.ind=TRUE) 
# Check if there is any tie duplicated
any(duplicated(ep)) 

# Answers:
# ep is an edge list(FromNodeId ---> ToNodeId).
# There is no missing data(NA) in ep. There is no duplicated tie.


# 3 
# Justify what you do with isolates and multiple components if there are any.
# First, we convert the data frame to igraph format and name it "gr_ep"

# As ep is a data frame, we use graph_from_data_frame() here
gr_ep <- graph_from_data_frame(ep,directed = TRUE)
# Then, we can plot network
plot(gr_ep, vertex.label = "", vertex.size = 4, layout = layout.fruchterman.reingold, main = 'Epinions social network(soc-Epinions1)')
#  Number of nodes and ties
vcount(gr_ep) 
ecount(gr_ep) 
# Components
components(gr_ep)$no # Number of components
components(gr_ep)$csize # How many nodes are there in each group
components(gr_ep)$csize[components(gr_ep)$csize > 1] 
sum(degree(gr_ep) == 0) # isolates

# Answers:
# There are a total of 75879 nodes and 508837 edges. 
# We found two components in the network. This means there are two separate group where there is no tie between them. There is no isolate in the network. This makes sense because these nodes are either "trust issuer" or "trust receiver". If a person neither gives the ID of a user he/she trusts or is regard as trust worthy by another user, this person would not appear in the original data set. The first group consists of 75877 nodes(users) while the second group is rather small and has only 2 nodes(users). 
# It could be hard to observe the second group when we look at the plot of the network. It is possible that  because of the size of the plot or the size of nodes(vertexes), the two nodes appear to overlap. But we could know the two nodes form a group in the network by using components().


# 4 
# Analyze the density of your network. Create a table that contains further descriptive network statistics for your network. 
# Please include average in-degree, average out-degree, standard deviation of out-degrees and of in-degrees, a reciprocity, and a transitivity index.

# Density
# Or graph.density(gr_ep)
edge_density(gr_ep) 

# Reciprocity
rec <- reciprocity(gr_ep)

# Transitivity
trans <- transitivity(gr_ep) 

# Degree distribution
average_in_degree <- mean(degree(gr_ep,mode="in"))

average_out_degree <- mean(degree(gr_ep,mode="out"))

# Standard deviations
sd_in_degree <- sd(degree(gr_ep,mode='in'))
sd_out_degree <-  sd(degree(gr_ep,mode='out'))

# Range
range(degree(gr_ep,mode='out'))
range(degree(gr_ep,mode='in'))
options(digits = 4) 
# Descriptive table (table 1)
table_1 <- data.table("average in-degree" = average_in_degree,
                      "average out-degree" = average_out_degree,
                      "standard deviation of out-degrees" = sd_out_degree,
                      "standard deviation of in-degrees" = sd_in_degree,
                      "reciprocity index" = rec,
                      "transitivity index" = trans)
table_1 |>  
  gt() |> 
  tab_header(title = "Table 1 descriptive network statistics of Epinions social network") |> 
  tab_source_note(source_note = "Data source: Epinions social network(soc-Epinions1)")
# The table was too long, so I broke table 1 into 2 parts(table_1 and table_2) in the pdf file.
# table_1 <- data.table("average in-degree" = average_in_degree,
# "average out-degree" = average_out_degree,
#"standard deviation of out-degrees" = sd_out_degree)
# table_1 |>  
# gt() |> 
# tab_header(title = "Table 1 descriptive network statistics of Epinions social network") |> 
# tab_source_note(source_note = "Data source: Epinions social network(soc-Epinions1)")
# table_2 <- data.table("standard deviation of in-degrees" = sd_in_degree,
#                     "reciprocity index" = rec,
#                     "transitivity index" = trans)
# table_2 |>  
# gt() |> 
# tab_header(title = "Table 1 descriptive network statistics of Epinions social network") |> 
# tab_source_note(source_note = "Data source: Epinions social network(soc-Epinions1)")

# Answers:
# The density is about 0.00009(8.83774e-5). Density is the number of actual ties over number of potential ties. In this case, this network is not very well connected.
# Table 1 shows that descriptive network statistics of Epinions social network.
# Since the network is directed which means that the nodes are not merely connected but pointing at one another(who trust whom), we have two different degrees, the in-degree and out-degree. In the table, it appears that the average in-degree and average out-degree are the same. This is because the difference between average in-degree and average out-degree is not so big(6.70590018318638 and 6.70590018318639). On average, a node has about 6 ties coming out from itself and coming in its direction.
# The standard deviation of out-degree is 26.01659 and the standard deviation of in-degree is 33.68148 which means the number of edges coming out from a node and coming to a node is not clustered near the average in-degree and out-degree.
# The reciprocity index shows the likelihood of links in the network being mutual. In our network, this likelihood of the established trust relationship being mutual on Epinion.com (You trust me and I trust you) is about 41%. 
# The transitivity index 0.06567883 is calculated by dividing transitivity triads with potentially transitivity triads. The probability of the network having adjacent nodes interconnected is rather low so it is not so likely to see tightly connected groups in out network.


# 5 
# Create a graph of the out-degree and the in-degree distributions in your network. 
# What do you observe? What do you imply from the observed degree distributions?

# The out-degree and the in-degree distributions
dgr <- data.frame(personid = rep(V(gr_ep)$name,times=2),
                  degree= c(degree(gr_ep,mode='out'),degree(gr_ep,mode='in')),
                  type = rep(c('outdegree','indegree'),each=vcount(gr_ep)))
head(dgr,10)

# Visualization
ggplot(data=dgr) +
  geom_histogram(aes(x=degree),color='black',fill='orange') +
  facet_wrap(~type,scales='free') +
  theme_classic()+
  labs(title = "Graph 1: The in-degree and the out-degree distributions",
       caption = "Data source:Epinions social network(soc-Epinions1)")

# The graph might not look very clear so we use sum() to see number of the count given a certain range of degree
sum(degree(gr_ep,mode='out') < 100) #75043
sum(degree(gr_ep,mode='out') >= 100 & degree(gr_ep,mode='out') < 200) #638
sum(degree(gr_ep,mode='out') >= 200 & degree(gr_ep,mode='out') < 300) #130
sum(degree(gr_ep,mode='out') >= 300 & degree(gr_ep,mode='out') < 400) #37
sum(degree(gr_ep,mode='out') >= 400 & degree(gr_ep,mode='out') < 500) #10

sum(degree(gr_ep,mode='in') < 100) #74909
sum(degree(gr_ep,mode='in') >= 100 & degree(gr_ep,mode='in') < 200) #601
sum(degree(gr_ep,mode='in') >= 200 & degree(gr_ep,mode='in') < 300) #194
sum(degree(gr_ep,mode='in') >= 300 & degree(gr_ep,mode='in') < 400) #87
sum(degree(gr_ep,mode='in') >= 400 & degree(gr_ep,mode='in') < 500) #35

# Answers:
# Graph 1 shows the in-degree and out-degree distributions of Epinion social network.
# The majority of nodes(users) have very limited number of users whom they trust(out-degree) as well as of users who trust them(in-degree). 75043 of 75879 users are trusted by less than 100 other users. 74909 of 75879 users trust less than 100 other users. As in-degree increases, the number of such more trusted users decreases. The decline is most obvious when the degree moves from less than 100 to between 100 and 200. After that, the number of users continues decreasing steadily. This tendency applies to out-degree as well.
# It is most common that an individual are only connected with a relatively small amount of other people, but such limited social contacts/small groups then put an individual into a larger network.


# 6
# A, If there are node-level variables in the data: Make a table that contains descriptive information for node level variables. 
# B, If there are no node-level variables: Make a table that contains distribution of node-level centrality values.

# There are no node-level variables: 
# Make a table that contains distribution of node-level centrality values
mean_be <- mean(betweenness(gr_ep)) # ~133194
med_be <- median(betweenness(gr_ep)) # 0
min_be <- min(betweenness(gr_ep)) # 0
max_be <- max(betweenness(gr_ep)) # ~95831448
sd_be <- sd(betweenness(gr_ep)) # ~1185528

# NaN(not a number) is found when calculating the mean,median,minimum and maximum value and standard deviation of closeness centrality
# We exclude NaN here and put closeness centrality of the rest nodes in a new vector clo_new
sum(is.nan(closeness(gr_ep)))
x <- closeness(gr_ep)
clo_new <- x[!is.nan(x)]
mean_clo <- mean(clo_new) # ~0.05
med_clo <- median(clo_new) # ~0.000004
min_clo <- min(clo_new) # ~0.000002
max_clo <- max(clo_new) # ~1
sd_clo <- sd(clo_new) # ~0.21
options(digits = 4) 
# Named tabel_3 because table_2 already used when breaking Table 1 into two parts
# table_3 is used to make Table 2 in the pdf file.
table_3 <- data.table("Centrality measure" = c("Betweenness","Closeness"), 
                      "Mean" = c(mean_be,mean_clo), 
                      "Median" = c(med_be,med_clo),
                      "Min" = c(min_be,min_clo),
                      "Max" = c(max_be,max_clo),
                      "SD" = c(sd_be,sd_clo))

table_3 |> 
  gt() |> 
  tab_header(title = "Table 2 Distribution of node-level centrality values") |> 
  tab_source_note(source_note = "Data source: Epinions social network(soc-Epinions1)")

# Answers:
# The betweenness centrality for each node is the number of shortest paths that pass through the node. Although the mean of betweenness of all the nodes is high. The majority of nodes are not "brokers" in the network and hardly have shortest paths that pass through them.
# The closeness centrality is calculated as the reciprocal of the sum of the length of the shortest paths between the node and all other nodes. High closeness centrality indicates that there is no need for currencies such as information to travel long distances to reach other nodes in the network. In our case, however, closeness centrality of most nodes is low. 


# 7
# Considering the large network size, select a form of visualization that could be meaningful. 
# Visualize your network and color the nodes according to a selected actor-variable or according to a selected measure of centrality.

# Color the vertices based on in-degree
V(gr_ep)$indegree <- degree(gr_ep,mode='in')
plot(gr_ep, vertex.label = "", vertex.size = 4, vertex.color = V(gr_ep)$indegree, layout = layout.fruchterman.reingold, main = 'Epinions social network(soc-Epinions1)')

# Answers:
# The nodes are colored based on in-degree. 


# 8
# Check assortativity in the network by centrality measures. 
# What do you observe? Provide an interpretation of your findings. What kind of theoretical arguments could possibly explain your results?

# Homophily/assortativity
assortativity_degree(gr_ep,directed=TRUE) # Assortativity degree

# Answers:
# The assortativity degree is negative(-0.04) in the network. This suggests that the nodes in the network have the tendency to connect with nodes of different types/ with dissimilar properties. However, social networks of human individuals tend to show positive assortativity which means nodes are more likely to connect with other similar nodes(e.g. gender, ethnicity). The result we get could make sense because on the Epionions platform, users who are trusted by many(with a high degree centrality) tend to be trusted by users who are trusted by few(probably new users, with a low degree centrality) which again increase their degree centrality and tie the two different types of nodes together. However, it is also possible that the number of hub nodes is relatively small which has a negative effect on  maintaining assortativity.


# 9
# And finally, a thought exercise. Assume that the robustness or vulnerability of the network is examined. 
# Try to come up with a measurement of robustness / vulnerability and argue for the usefulness of your measure. 
# Speculate about some implications for the concrete network.

# Connectivity
vertex.connectivity(gr_ep) 
edge.connectivity(gr_ep)
# Number of cut vertices
ep.cut.vertices <- articulation.points(gr_ep) 
length(ep.cut.vertices)

# Answers:
# The vertex and edge connectivity are both 0. In the Epinions social network, a total of 15936(about 3.1% of 508837) vertices are cut vertices. A vertex-cut is a particular set of vertices in a graph which once removed would disconnect the graph. so identifying these vertices help us check the vulnerability of the network. The number of cut vertices over all vertices is low in our network. However, by removing certain nodes, the flow of information through these nodes is then disrupted. In our case, if we remove some users trusted by many other users, users who are connected only because they all trust those well-trusted users would be disconnected and more components could appear in the network. This could later have a negative effect on the credibility of the reviews on Epionions.com as users of a certain component are not encouraged(without a trustworthy user to connect two components as one) to trust another user of another component.