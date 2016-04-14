# Load data
egdes = read.csv("./edges.csv")
users = read.csv("./users.csv")

# Locale
table(users$locale)

# School gender
table(users$gender, users$school)

# Packages
install.packages("igraph")
library(igraph)

?graph.data.frame()
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)

# Size friendship
degree(g)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

# Color gender
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

?igraph.plotting

