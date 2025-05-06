

get_gene_counts <- function(dge_list) {
  gene_counts <- sapply(dge_list, nrow)
  gene_counts_df <- data.frame(
    contrast = names(gene_counts),
    gene_count = gene_counts
  )
  return(gene_counts_df)
}



plot_gene_counts <- function(gene_counts_df, contrast_order, contrast_colors, title = "Number of DE Genes per Contrast") {
  gene_counts_df$contrast <- factor(gene_counts_df$contrast, levels = contrast_order)
  
  ggplot(gene_counts_df, aes(x = contrast, y = gene_count, fill = contrast)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = contrast_colors) +
    theme_minimal() +
    labs(title = title, x = "Contrast", y = "Gene Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
