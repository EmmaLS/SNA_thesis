`
# SNA_thesis
# Social network analysis used in PhD thesis
# Create token to access Twitter API:
# Load packages
library(rtweet)
library(igraph)
library(dplyr)
library(tidygraph)
library(ggraph)
library(tidyverse)
token = create_token(app = "xxxxxxxxxxx",
                     consumer_key = "xxxxxxxxxxxxxxxxxxxx",
                     consumer_secret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                     access_token = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                     access_secret = " xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")
                     
# Search for tweets:
translation_search1 = search_tweets2(c("\"translation\"",
"#xl8 OR #1tn"), n = 50000, retryonratelimit = TRUE)

# Combine all searches and remove duplicate tweets:
translation_search = rbind(translation_search1, translation_search2) %>%
	distinct()
# Create dataframe for analysis:
tweets_df = data.frame(translation_search$user_id, translation_search$screen_name, 
		translation_search$created_at, translation_search$text, stringsAsFactors = FALSE)
names(tweet_df) = c(“user_id”, “screen_name”, “created_at”, “tweet_text”)

# Get friends and followers from top 100 tweeters in dataset:
top_tweeters =  as.data.frame(table(tweets_df$user_id)) 
top_tweeters = arrange(top_tweeters, desc(Freq))
network = as.vector(top_tweeters[['Var1']])
friends = get_friends(network[1:100], retryonratelimit = TRUE)
network_filtered = network[1:100]
followers_list = vector(mode = "list", length = length(network_filtered))
for (i in seq_along(network_filtered)) {
  followers_list[[i]] = get_followers(network_filtered[i], n = "all", retryonratelimit = TRUE)
}
followers_flat = unlist(followers_list)

# Get user names of friends and followers:
top_tweeters_info = lookup_users(network_filtered, parse = TRUE) 
friends_info_list = vector(mode = "list", length = length(friends$user_id))
for (i in seq_along(friends$user_id)) {
  friends_info_list[[i]] = lookup_users(friends$user_id[i])
}
friends_info_list_flat = flatten(friends_info_list)

followers_info_list = vector(mode = "list", length = length(followers_flat))
for (i in seq_along(followers_flat)) {
  followers_info_list[[i]] = lookup_users(followers_flat[i])
}
top_list = as.list(network_filtered)
followers_id = vector(mode = "list", length = length(followers_list))
for (i in seq_along(followers_list)) {
  followers_id[[i]] = cbind(followers_list[[i]], top_list[[i]])
}
followers_df = bind_rows(followers_id)
names(followers_df) = c(“from_id”, “to_id”)

# Create nodes and edges lists by filtering for Twitter users appear in dataset:
filtered_friends = friends %>% 
  filter(to_id %in% tweets_df$user_id)
filtered_followers = followers_df %>% 
  filter(from_id %in% tweets_df$user_id)
filtered_edges = rbind(filtered_followers, filtered_friends)
filtered_edges = unique(filtered_edges)
filtered_nodes = unique(c(filtered_edges$from_id, filtered_edges$to_id))
filtered_nodes_df = as.data.frame(filtered_nodes)
names(filtered_nodes_df) = c("Var1")

# Attach number of tweets to nodes:
tweet_count = inner_join(filtered_nodes_df, top_tweeters)

# Create igraph object and carry out basic analyses:
filtered_g = graph_from_data_frame(d = filtered_edges, vertices = filtered_nodes)
gorder(filtered_g)
gsize(filtered_g)
edge_density(filtered_g)
transitivity(filtered_g)
modularity(filtered_g)

# Anonymise tweets:
anon_vector = 1:528
anon_vector = paste("TwUser", anon_vector) 
anon = as.data.frame(anon_vector)
names_network = data.frame(tweets_df$screen_name)
names_anon = cbind(names_network, anon)

# Set anonymous names as vertex label:
vertex_attr(filtered_g, "label") = V(filtered_g)$name
V(filtered_g)$name = anon_vector

# Plot network:
plot(filtered_g,
    edge.arrow.size = 0.1,
    vertex.color = "yellow",
    vertex.label.family = "Helvetica",
    vertex.label.font = 2,
    layout = layout)

# Set no. of tweets as vertex size and plot:
freq_tweet = tweet_count$Freq
V(filtered_g2)$color = "blue"
plot.igraph(filtered_g, vertex.size = sqrt(freq_tweet)+1, vertex.label = NA, edge.arrow.size = 0.2)

# Set no. of tweets as attribute and create subgraph:

vertex_attr(filtered_g, "tweet_count", index = V(filtered_g)) = freq_tweet)
V(filtered_g)$size = sqrt(freq_tweet) + 1
freq_tweeters = induced.subgraph(filtered_g, vids = which(V(filtered_g)$tweet_count >= 30))
V(filtered_g2)$name = anon_vector
V(filtered_g2)$size = 8
plot.igraph(freq_tweeters, edge.arrow.size = 0.2, vertex.color = "skyblue", asp = 0, main = "TEF Tweets no. > 30")

# Calculate centrality measures
# Calculate in-degree, out-degree, create dataframe and plot:
g.outd = as.data.frame(degree(filtered_g, mode = c("out")))
which.max(g.outd)
g.ind = as.data.frame(degree(filtered_g, mode = c("in")))
which.max(g.ind)
degree_df = data.frame(names$screen_name, g.outd, g.ind, row.names = c())
names(degree_df) = c("Screen_Name", "Out_Degree", "In_Degree")
out_deg = degree(filtered_g, mode = c("out"))
plot.igraph(filtered_g, vertex.size = sqrt(out_deg)+1, vertex.color = "blue", vertex.label = NA, asp = 0, edge.arrow.size = 0.2)
in_deg = degree(filtered_g, mode = c("in"))
plot.igraph(filtered_g, vertex.size = sqrt(in_deg)+1, vertex.color = "blue", vertex.label = NA, asp = 0, edge.arrow.size = 0.2)

# Calculate betweenness and plot with vertex size based on betweenness:
g.b = betweenness(filtered_g, directed = TRUE)
g.b_df = as.data.frame(g.b, names_network$screen_name)
g.b_df = g.b_df %>% 
  mutate("screen_name" = names_network$screen_name)
names(g.b_df) = c("betweenness")
which.max(g.b)
g.b_df = g.b_df[order(-g.b_df$betweenness),]

head(g.b_df)
plot(filtered_g, 
     edge.color = 'black',
     vertex.size = sqrt(g.b)+1,
     vertex.label = NA,
     vertex.color = "blue",
     edge.arrow.size = 0.05,
     layout = layout_nicely(filtered_g))

# Get 0.99 quantile of betweenness 
btw = betweenness(filtered_g)
betweenness_q99 = quantile(btw, 0.90)

# Get top 1% of vertices by betweenness
top_btw = btw[btw > betweenness_q99]

# Transform betweenness: add 2 then take natural log
transformed_btw = log(btw + 2)

# Make transformed_btw the size attribute of the vertices
V(filtered_g)$size = transformed_btw

# Plot the graph
plot(
  filtered_g, vertex.label = NA, edge.arrow.width = 0.2,
  edge.arrow.size = 0.0, vertex.color = "blue")

# Subset nodes for betweenness greater than 0.99 quantile
vertices_high_btw <- V(filtered_g)[btw > betweenness_q99]

# Induce a subgraph of the vertices with high betweenness
filtered_g_subgraph <- induced_subgraph(filtered_g, vertices_high_btw)

# Plot the subgraph
plot.igraph(filtered_g_subgraph, edge.arrow.size = 0.1, vertex.label.cex = 0.8, 
	vertex.color = "skyblue", asp = 0, main = "TEF Top Betweenness")

# Create table including tweet count and all centrality measures
cent_measures = cbind(g.ind, g.outd, g.b)
names(cent_measures) = c("In_Degree", "Out_Degree", "Betweenness")
merged_names = merge(tweet_count, by.x = "Var1", names_anon, by.y = "user_id")
merge_dgs2 = merge(merged_names, by.x = "screen_name", cent_measures, by.y = "rowname")
tweet_table2 = data.frame(merge_dgs2$anon, merge_dgs2$Freq, merge_dgs2$In_Degree.x, merge_dgs2$Out_Degree.x, merge_dgs2$Betweenness) 
names(tweet_table2) = c("ID", "Number", "In_Degree", "Out_Degree", "Betweenness")

# Arrange by number of tweets and create csv file
tweet_table2 %>% 
  arrange(desc(Number))
write_as_csv(tweet_table2, "anon_table2", na = "", fileEncoding = "UTF-8")


## Eigen centrality and plot (not used in thesis)
V(filtered_g)$name = names_vector
g.ec = eigen_centrality(filtered_g)
which.max(g.ec$vector)
g.ec_df = as.data.frame(g.ec, row.names = c())
g.ec_df = g.ec_df[order(-g.ec_df$vector),]

plot(filtered_g,
     vertex.label = NA,
  vertex.color = "blue", 
  vertex.label.cex = 0.6,
  vertex.size = 25*(g.ec$vector),
  edge.arrow.size = 0.1,
  edge.color = 'gray88')

# Get 0.99 quantile of eigen-centrality
eigen_centrality_q99 = quantile(g.ec, 0.90)

# Subset nodes for eigen centrality greater than 0.99 quantile
vertices_high_ec = V(filtered_g)[g.ec > eigen_centrality_q99]

# Induce a subgraph of the vertices with high ec
filtered_g2_subgraph2 = induced_subgraph(filtered_g2, vertices_high_ec)

# Plot the subgraph
plot.igraph(filtered_g2_subgraph2, edge.arrow.size = 0.1, vertex.label.cex = 0.8, 
	vertex.color = "skyblue", asp = 0)

# Calculate communities using edge betweenness and plot
V(filtered_g)$size = 4
gc = edge.betweenness.community(filtered_g)
sizes(gc)
plot(gc, filtered_g, vertex.label = NA, edge.arrow.size = 0.2, asp = 0)

# Using k-core to show clustering
coreness = graph.coreness(filtered_g)
table(coreness)
maxCoreness = max(coreness)
maxCoreness

# Use coreness to colour vertices
V(filtered_g)$color = coreness + 1
V(filtered_g)$size = 8
plot.igraph(filtered_g, asp = 0, vertex.label = coreness)
colors = rainbow(maxCoreness)
op = par(mar = rep(0, 4))
plot.igraph(filtered_g, vertex.label = coreness, vertex.color = colors[coreness], asp = 0, 
	edge.arrow.size = 0.1, vertex.label.cex = 0.9)
# Peeling away layers of k-core values
V(filtered_g)$name = coreness
V(filtered_g)$color = colors[coreness]
V(filtered_g)$label.cex = 0.7
kcg = filtered_g
kcg_1 = induced.subgraph(filtered_g, vids = which(coreness > 5))
kcg_2 = induced.subgraph(filtered_g, vids = which(coreness > 10))
kcg_3 = induced.subgraph(filtered_g, vids = which(coreness > 15))
kcg_4 = induced.subgraph(filtered_g, vids = which(coreness > 20))
kcg_5 = induced.subgraph(filtered_g, vids = which(coreness > 25))
kcg_6 = induced.subgraph(filtered_g, vids = which(coreness > 30))
kcg_7 = induced.subgraph(filtered_g, vids = which(coreness > 35))
kcg_8 = induced.subgraph(filtered_g, vids = which(coreness >= 38))

# Plot highest k-core
plot(kcg_8, edge.arrow.size = 0.1, edge.arrow.width = 0.1, layout = layout_nicely(kcg_8))

# plot top k cores with anonymised names
V(filtered_g2)$name = anon_vector
V(filtered_g2)$color = colors[coreness]
V(filtered_g2)$label.cex = 0.8
kcg_7 = induced.subgraph(filtered_g2, vids = which(coreness > 35))
plot.igraph(kcg_7, edge.arrow.size = 0.1, layout = lay[which = coreness > 35,], asp = 0, main = "k-cores 36-38")
`

