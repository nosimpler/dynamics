mat2 <- mat %>% filter(component=="Gamma") %>% select(3:92) %>% as.matrix()
um <- umap::umap(mat2, n_neighbors=4)
um <- as.tibble(um$layout)
um$ID <- unique(W_cfs$ID)
um2 <- left_join(um %>% split_id(), demo_cfs)
ggplot(um2, aes(x=V1, y=V2, color=age))+geom_point()

