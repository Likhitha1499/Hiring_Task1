# Hiring_Task1
#Load libraries
library(data.table)

#Define the file path
gene_info_path <- "C:/Users/likhi/Downloads/Homo_sapiens.gene_info.gz"
gmt_path <- "C:/Users/likhi/Downloads/h.all.v2023.1.Hs.symbols.gmt"
output_gmt_path <- "C:/Users/likhi/Downloads/h.all.v2023.1.Hs.entrez_ids.gmt"

install.packages('R.utils')

#Read the gene info file
gene_info <- fread(gene_info_path, select = c("GeneID", "Symbol", "Synonyms"), data.table = FALSE)

#Mapping from Symbols and Synonyms to GeneID
symbol_to_geneid <- with(gene_info, setNames(GeneID, Symbol))
synonyms_to_geneid <- with(gene_info, setNames(rep(GeneID, lengths(strsplit(Synonyms, "[|]"))), unlist(strsplit(Synonyms, "[|]"))))
symbol_to_geneid <- c(symbol_to_geneid, synonyms_to_geneid)

# Replace symbols with Entrez IDs
replace_symbols_with_entrez <- function(line, mapping) {
  elements <- strsplit(line, "\t")[[1]]
  pathway_name <- elements[1]
  pathway_desc <- elements[2]
  gene_symbols <- elements[-c(1, 2)]
  entrez_ids <- sapply(gene_symbols, function(x) mapping[[x]])
  c(pathway_name, pathway_desc, entrez_ids)
}

# Process the GMT file
con <- file(gmt_path, open = "r")
con_out <- file(output_gmt_path, open = "w")

while (length(line <- readLines(con, n = 1, warn = FALSE)) > 0) {
  new_line <- replace_symbols_with_entrez(line, symbol_to_geneid)
  writeLines(paste(new_line, collapse = "\t"), con_out)
}

close(con)
close(con_out)

# Print completion message
cat("GMT file with Entrez IDs has been written to", output_gmt_path, "\n")

#checking
# Reading first 5 lines from the original and new GMT files
original_gmt_sample <- readLines(gmt_path, n = 5) # read first 5 lines
new_gmt_sample <- readLines(output_gmt_path, n = 5) # read first 5 lines

# Print out the samples for manual inspection
original_gmt_sample
new_gmt_sample

# write a quick check for whether all symbols in a sample of the new GMT file
# are in the symbol_to_geneid mapping (they should be replaced by GeneID)
all(sapply(strsplit(new_gmt_sample, "\t"), function(line) {
  # Skip the first two elements which are pathway name and description
  all(line[-c(1, 2)] %in% symbol_to_geneid)
}))
