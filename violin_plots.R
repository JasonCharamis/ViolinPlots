
dependencies <- ( "svglite", "ggplot2", "ggrepel", "ggthemes", "stringr", "dplyr", "ggbreak")

package_install(dependencies)


## genes to mark in violin plot

#gene_colors1 <- setNames(rep("red",length(selected_genes1)), selected_genes1)
#gene_colors2 <- setNames(rep("green",length(selected_genes2)), selected_genes2)

violin_plot <- function ( filename, title, ... ) {
    
                         ## open file and keep Species name ##
                         expression_data <- read.delim(filename, header = F)
                         species_name <- gsub("_.*", "",expression_data[,1])
                         expression_data$Species <- species_name
            
                         ## create dataframe
                         expression_data <- data.frame(GeneID = expression_data[,1], expression_data[,2:ncol(expression_data)])
            
                         ## collapse three replicates to one - keep mean values for each species
                         for ( i in 1:nrow(expression_data)) {
                           
                           ## most species have three replicates, while some have two replicates
                              if ( is.na(expression_data[i,4]) ) {
                              expression_data$Mean_Expression[i] <- rowMeans(expression_data[i,2:3])
                              }
                              
                              else {
                              expression_data$Mean_Expression[i] <- rowMeans(expression_data[i,2:4])
                              }
                         }
                                                              
                        expression_data <- expression_data %>%
                                                                mutate(PointColor = case_when(
                                                                                GeneID %in% selected_genes1 ~ "red",
                                                                                GeneID %in% selected_genes2 ~ "lightblue",
                                                                                TRUE ~ "black" ))
                         
                         ## Using ggplot2 to create the violin plot
                         ggplot( expression_data, aes(x=Species, y=Mean_Expression) ) +
                            geom_violin(trim = FALSE, fill="grey", draw_quantiles = c(0.25, 0.5, 0.75), scale = "area", alpha=0.1) + 
                            geom_jitter(aes(color = PointColor,fill=PointColor), width = 0.4, size = 2.5) +
                            scale_color_identity() + scale_fill_identity() +
                            labs(title = title, y="Normalized Expression") +
                            theme_minimal() 
  }
