#!/bin/bash

# Count the number of sequences in the FASTA file
num_sequences=$(grep -c '^>' NC_000913.faa)

# Extract the sequences and calculate the total number of amino acids
total_aa=$(grep -v '^>' NC_000913.faa | tr -d '\n' | wc -c)

# Calculate the average length
average_length=$((total_aa / num_sequences))

# Output the result
echo "Average length of proteins: $average_length amino acids"
