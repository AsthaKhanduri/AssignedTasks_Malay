# Load required library
library(ggplot2)

# Read data from the file
gene_info <- read.table("Homo_sapiens.gene_info.gz", header = TRUE, sep = "\t", quote = "", comment.char = "")

# Filter out rows where chromosome value contains a |
gene_info <- gene_info[!grepl("\\|", gene_info$chromosome), ]

# Filter out rows with "-" values in chromosome
gene_info <- gene_info[gene_info$chromosome != "-", ]

# Count the number of genes per chromosome
gene_count <- table(gene_info$chromosome)

# Convert gene count to data frame
gene_count_df <- data.frame(chromosome = names(gene_count), count = as.numeric(gene_count))

# Define the order of chromosome levels
chromosome_levels <- c(as.character(1:22), "X", "Y", "MT", "Un")

# Ensure that chromosome is an ordered factor with the desired levels
gene_count_df$chromosome <- factor(gene_count_df$chromosome,
                                   levels = chromosome_levels,
                                   ordered = TRUE)

# Create bar plot 
ggplot(subset(gene_count_df, count > 0), aes(x = chromosome, y = count)) +
  geom_bar(stat = "identity", fill = "grey40") +
  labs(title = "Number of genes in each chromosome",
       x = "Chromosomes",
       y = "Gene Count") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) + # Add a gap between the bottom of the y-axis and the bars
  theme( axis.line = element_line(color = "black", size = 1.0),  # Set axis lines to black
        axis.text = element_text(size = 12),  # Adjust axis text size
        axis.title = element_text(size = 14), # Adjust axis title size
        plot.title = element_text(hjust = 0.5),# Center the title
        panel.background = element_rect(fill = "white"),   # Set panel background color to white
        plot.background = element_rect(fill = "white"))    # Set plot background color to white
