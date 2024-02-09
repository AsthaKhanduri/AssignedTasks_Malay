# Read the gene_info file
gene_info <- read.table("Homo_sapiens.gene_info.gz", header = TRUE, sep = "\t", quote = "", comment.char = "", stringsAsFactors = FALSE)

# Extract necessary columns
subset_gene_info <- gene_info[, c(2, 3, 5)]

# Function to create a mapping of symbols to Entrez IDs
create_symbol_to_entrez_map <- function(subset_gene_info) {
  # Initialize an empty data frame to store the mapping
  symbol_entrez_df <- data.frame(Symbol = character(), EntrezID = character(), stringsAsFactors = FALSE)
  
  # Extract unique symbols and synonyms
  symbols <- unique(c(subset_gene_info$Symbol, unlist(strsplit(as.character(subset_gene_info$Synonyms), "\\|"))))
  
  # Iterate over each symbol
  for (symbol in symbols) {
    # Extract corresponding GeneIDs for the symbol and its synonyms
    entrez_ids <- subset_gene_info$GeneID[subset_gene_info$Symbol == symbol]
    entrez_ids <- c(entrez_ids, subset_gene_info$GeneID[sapply(strsplit(as.character(subset_gene_info$Synonyms), "\\|"), function(x) symbol %in% x)])
    
    # Remove NA values and duplicates
    entrez_ids <- unique(na.omit(entrez_ids))
    
    # Append the mapping to the data frame
    symbol_entrez_df <- rbind(symbol_entrez_df, data.frame(Symbol = symbol, EntrezID = paste(entrez_ids, collapse = ","), stringsAsFactors = FALSE))
  }
  
  return(symbol_entrez_df)
}

# Create the mapping of symbols to Entrez IDs
symbol_entrez_df <- create_symbol_to_entrez_map(subset_gene_info)

# Print the mapping
print(symbol_entrez_df)

# Read the GMT file line by line and replace gene symbols with Entrez IDs
input_file <- "h.all.v2023.1.Hs.symbols.gmt"
output_file <- "updated_pathways.gmt"

# Open input and output files
input <- file(input_file, "r")
output <- file(output_file, "w")

# Process each line in the GMT file
while (TRUE) {
  line <- readLines(input, n = 1)
  if (length(line) == 0) {
    break  # End of file
  }
  
  # Split the line by tabs
  fields <- strsplit(line, "\t")[[1]]
  pathway_name <- fields[1]
  pathway_description <- fields[2]
  gene_symbols <- fields[-(1:2)]  # Exclude pathway name and description
  
  # Replace gene symbols with Entrez IDs
  entrez_ids <- lapply(gene_symbols, function(symbol) {
    if (symbol %in% names(symbol_entrez_df)) {
      return(symbol_entrez_df[[symbol]])
    } else {
      return(symbol)  # If symbol not found in mapping, keep it as is
    }
  })
  
  # Flatten the resulting list
  entrez_ids <- unlist(entrez_ids)
  
  # Combine the updated pathway information
  updated_line <- paste(pathway_name, pathway_description, paste(entrez_ids, collapse = ","), sep = "\t")
  
  # Write updated pathway information to output file
  cat(updated_line, file = output, append = TRUE)

}

# Close input and output files
close(input)
close(output)

# Print message
cat("Updated GMT file has been created:", output_file, "\n")