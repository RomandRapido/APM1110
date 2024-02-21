library(igraph)

#problem

#data
dataTable <- data.frame(
  message = c(0, 1),
  trans_prob = c(0.70, 0.30),
  rec_prob = c(0.95, 0.75)
)


#show tree
vertices <- c("Sender", "0", "1", "0_0", "0_1", "1_0", "1_1", "Receiver")

edges <- matrix(c(
  "Sender", "0",
  "Sender", "1",
  "0", "0_0",
  "0", "0_1",
  "1", "1_0",
  "1", "1_1",
  "0_0", "Receiver",
  "0_1", "Receiver",
  "1_0", "Receiver",
  "1_1", "Receiver"
), byrow = TRUE, ncol = 2)

edge_labels <- c("0.70", "0.30", "0.95", "0.05", "0.25","0.75", "", "", "", "")

g <- graph_from_edgelist(edges, directed = TRUE)
E(g)$label <- edge_labels

V(g)$label <- vertices

layout <- matrix(c(
  0.5, 0, #sender
  -1, -1, #0
  2, -1, #1
  -2, -2,#0_0
  0, -2, #0_1
  1, -2,#1_0
  3, -2, #1_1
  0.5, -3 #Receiver
), ncol = 2, byrow = TRUE)

V(g)$x <- layout[,1]
V(g)$y <- layout[,2]

par(mar = c(0, 0, 0, 0))

plot(g, layout = layout, edge.label = E(g)$label, edge.label.cex = 0.8, vertex.label.cex = 0.8,
     vertex.shape = "circle", vertex.size = 30, edge.arrow.size = 0.5)

label_coords <- matrix(c(
  -0.9, 0.66,
  1.34, -0.01
), ncol = 2, byrow = TRUE)


text(label_coords[1,1], label_coords[1,2], labels = "Probability of being TRANSMITTED", pos = 3, cex = 0.8)
text(label_coords[2,1], label_coords[2,2], labels = "Probability of being RECEIVED", pos = 3, cex = 0.8)

#item 1_a

#by Multiplication Law of prob

#show with probability

#by law of total probability