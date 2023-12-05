### Inflation interactive network

# Based on https://aurelien-goutsmedt.com/post/extracting-biblio-data-2/

## Taking cross-references data 
references_filtered = readRDS("references_filtered.Rdata")

## Edges
edges <- biblionetwork::biblio_cocitation(filter(dimensions_direct_citation, publication_id %in% references_filtered$publication_id), 
                                          "citing_id", 
                                          "publication_id",
                                          weight_threshold = 3)


cocitnet = function(nodes, edges){
  ## First step: creating the network and keeping the main component
  
  grafico <- tbl_main_component(nodes = nodes, edges = edges, directed = FALSE, node_key = "ItemID_Ref", nb_components = 1)
  
  ## Finding communities
  grafico <- add_clusters(grafico, clustering_method = "leiden",objective_function = "modularity")
  
  ## Give colors to each community
  palette <- sample(scico::scico(n = 500, palette = "bam"))
  grafico <- community_colors(grafico, palette, community_column = "cluster_leiden")
  
  # calculating the degree of nodes
  grafico <- grafico %>% 
    activate(nodes) %>%
    mutate(size = n,# will be used for size of nodes
           first_author = str_extract(authors, "(?<=\\[)(.+?)(?=,)"),
           label = paste0(first_author, "-", year)) 
  
  # giving names to communities
  grafico <- community_names(grafico, 
                             ordering_column = "size", 
                             naming = "label", 
                             community_column = "Com_ID")
  
  return(grafico)
}


# creating my interactive net

ggiraph_net = function(grafico){
  
  top_nodes  <- top_nodes(grafico, top_n = 5, top_n_per_com = 0, ordering_column = "size")
  
  # building the graph 
  grafico <- ggraph(grafico, "manual", x = x, y = y) + 
    geom_edge_arc(aes(color = color_edges, width = weight), alpha = 0.4, strength = 0.2, show.legend = FALSE) +
    scale_edge_width_continuous(range = c(0.1,2)) +
    scale_edge_colour_identity() +
    geom_point_interactive(aes(x = x, y = y, tooltip = paste0(Label,", ",doi,", ",source), data_id = Com_ID, size = size, fill = color), pch = 21, alpha = 0.9, show.legend = FALSE) +
    scale_size_continuous(range = c(0.2,13)) +
    scale_fill_identity() +
    new_scale("size") +
    geom_text(data=top_nodes, aes(x=x, y=y, label = Label), size = 3, fontface="bold", alpha = 1, show.legend = FALSE) +
    scale_size_continuous(range = c(0.5,5)) +
    theme_void()
  
  # transforming the graph in an interactive graph with ggiraph
  x <- girafe(ggobj = grafico)
  x <- girafe_options(x = x,
                      opts_tooltip(opacity = .8, use_fill = TRUE),
                      opts_zoom(min = 0.8, max = 4),
                      sizingPolicy(defaultWidth = "100%", defaultHeight = "250px"),
                      opts_hover_inv(css = "opacity:0.1;"),
                      opts_hover(css = "opacity:1;r:5pt;"))
  #frameWidget(x)
  
  return(x)
  
}

ggiraph_net(graph)
 

