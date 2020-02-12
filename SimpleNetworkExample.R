library(igraph)
library(GGally) # contains ggnet2

nam <- c("A", "B", "C") # Node name

g <- sample_pa(3,m=3)  # generate graph
g <- igraph::as_data_frame(g) # create df
g <- rbind(g$to,g$from) # create matrix

colfunc <- colorRampPalette(c("black", "red")) #set colours

net.bg <- make_graph(g, 3, directed = FALSE) #make graph
E(net.bg)$weight <- c(1,4,5) #  Edge width (add more values to increase edges)
V(net.bg)$size <- c(5,10,15) # Node size (add more values to increase amount of nodes)


#  Basic Plot -------------------------------------------------------------

# install.packages("intergraph")
library(intergraph)
# for some reason, the intergraph package is not required by GGally.
# though ggnet2 looks for it.

p <- ggnet2(net.bg, mode = "circle",  #plot graph
            size = V(net.bg)$size,
            node.color = "blue",
            label = nam,
            edge.size = E(net.bg)$weight,
            edge.label = E(net.bg)$weight,
            edge.color = colfunc(length(E(net.bg)$weight)))
p


# Thresholding Edges Example: -------------------------------------------------

a <- sort(E(net.bg)$weight, decreasing  = TRUE) # sort edge width from highest to lowest
cut.off <- a[1:2] # select highest 2 values
`%notin%` <- Negate(`%in%`)
net.sp  <- delete_edges(net.bg, E(net.bg)[E(net.bg)$weight %notin% cut.off]) # delete edges below cut-off value

weightDF <- get.data.frame(net.sp) # get df of graph attributes
EdgeWidth1 <- weightDF$weight  # select edge weight

pp <- ggnet2(net.sp, mode = "circle",
             size = V(net.bg)$size,
             color = "blue",
             label = nam,
             edge.label = EdgeWidth1,
             edge.size = EdgeWidth1,
             edge.color = colfunc(length(E(net.sp)$weight)))
pp


# Clustering Example: -----------------------------------------------------

# Currently not working
clp <- cluster_optimal(net.bg) #using igraph clustering
plot(clp, net.bg)


## Not working with ggnet2 as clp is not an igraph object) ##
ppp <- ggnet2(clp, mode = "circle",
             size = V(net.bg)$size,
             color = "blue",
             label = nam,
             edge.label = E(net.bg)$weight,
             edge.size = E(net.bg)$weight,
             edge.color = colfunc(length(E(net.bg)$weight)))

# Throws back error saying: could not coerce net to a network object
ppp

class(net.bg)
class(clp)


