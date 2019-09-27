# install.packages("igraph")
# install.packages("ggplot2")
library('igraph')
library("ggplot2")

#read in data
edges1 = read.table(file = "out.subelj_jdk_jdk")

# convert to matrix:  
em <-as.matrix(edges1)

# extract vectors convert to dataframe: 
relations <- as.data.frame(em)
g<-graph.data.frame(relations,directed=TRUE)

#detemine number of degrees and plot...
# g_degrees = degree(g)
# 
# g_degrees_histogram <- as.data.frame(table(g_degrees))
# 
# g_degrees_histogram[,1] <- as.numeric(g_degrees_histogram[,1])
# 
# g_degrees_histogram_filt = g_degrees_histogram[g_degrees_histogram$Freq < 400,]
# 
# ggplot(g_degrees_histogram, aes(x = g_degrees, y = Freq)) +
#   geom_bar() +
#   scale_x_continuous("Degree\n(nodes with this amount of connections)",
#                      breaks = c(1, 3, 10, 30, 100, 300),
#                      trans = "log10") +
#   scale_y_continuous("Frequency\n(how many of them)",
#                      breaks = c(1, 3, 10, 30, 100, 300, 1000),
#                      trans = "log10") +
#   ggtitle("Degree Distribution (log-log)") +
#   theme_bw()

#simplify graph
g_simp = simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = igraph_opt("edge.attr.comb"))

#filter graph so that only nodes with degree above 200 remain
g_simp = delete.vertices(g_simp, degree(g_simp) < 200  )

# plot(g_simp)

#find best layout by plotting various layouts
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 

# Remove layouts that do not apply to our graph.
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(3,3), mar=c(1,1,1,1))

for (layout in layouts) {
  print(layout)
  l <- do.call(layout, list(g_simp)) 
  plot(g_simp, edge.arrow.mode=0, layout=l, main=layout) }


# part 4: graph Metrics
# 1) density
graph.density(g_simp, loops=TRUE) 

# connectedness(g_simp) # requires r graph.. package ‘rgraph’ is not available (for R version 3.4.4)

# shortest path
# distance_table(g_simp, directed = TRUE)

# egocentric network of a vertex v is a subgraph consisting of v and its immediate neighbors
# ego <- ego_extract(g_simp)

#2) closeness centrality
closeness(g_simp)

# 3) betweenness
betweenness(g_simp)

# 4) Eigenvector Centrality
eigen_centrality(g_simp, directed = TRUE)

# 5) Page Rank
page_rank(g_simp,  directed = TRUE)

# 6) Centralizing
centr_betw(g_simp, directed = TRUE)

# 7) Vertex Attributes
vertex.attributes(g_simp)

# 8) adjacent matrix
adjmatrix <- as_adjacency_matrix(g_simp)
adjmatrix

# 9) adding weight to edges and vetrices
E(g_simp)$weight <- rnorm(ecount(g_simp))
V(g_simp)$weight <- rnorm(vcount(g_simp))

g_simp[1:5,1:9]

# 9cont) add to visalization  
# sg <- induced_subgraph(g_simp, g_simp$weight)
# plot(sg, edge.label = round(E(sg)$weight,3))
