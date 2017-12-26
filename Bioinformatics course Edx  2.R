
library(Biostrings)

library(seqinr)


## Input Sequence

AB003468 <- readDNAStringSet("AB003468.fasta")

AB003468 <- as.character(AB003468)

sequence <- AB003468


# Create start and stop codon variables

start_codon <- "ATG"

stop_codons <- c("TGA" , "TAA","TAG")

# Create variables to store the results in

start_pos <- c()

revstart_pos <- c()

stop_pos <- c()

revstop_pos <- c()


# Finding START codons

# First reading frames

matches <- matchPattern(start_codon,sequence)

start_pos <- c(start_pos, start(matches))

# Reverse complement reading frames

revmatches <- matchPattern(reverseComplement(DNAString(start_codon)), sequence)

revstart_pos <- c(revstart_pos, start(revmatches))

# Sort the results

start_pos <- sort(start_pos)

revstart_pos <- sort(revstart_pos, decreasing = TRUE)



# Finding STOP condons

for (codon in stop_codons) {
        
        matches <- matchPattern(codon, sequence)
        
        stop_pos <- c(stop_pos, start(matches))
        
        revmatches <- matchPattern(reverseComplement(DNAString(codon)),sequence)
        
        revstop_pos <- c(revstop_pos, start(revmatches))
}


#  Sort the results

stop_pos <- sort(stop_pos)

revstop_pos <- sort(revstop_pos, decreasing = TRUE)



## Putting it all together

# First set the threslhold for minimum ORF size

k <- 150 # k is our threshold - we're setting that so that our minimum ORF is 150 (or 50 amino acids).

lengths <- vector(mode="numeric")

# Forward 3 frames

stop_pointers <- c(0,0,0) # The stop_pointers will hold the location of the STOPS in each reading frame for each iteration of our code.

count <- 0 # Count is simply the number of ORFs we find.

for (current_start in start_pos) {
        
        frame <- (current_start%%3) + 1
        
        stop_pointer <- stop_pointers[frame]
        
        if (stop_pointer <= length(stop_pos) && (stop_pointer == 0 || stop_pos[stop_pointer] < current_start)) {
                
                stop_pointer <- stop_pointer + 1
                
                while (( stop_pointer <= length(stop_pos)) && (( stop_pos[stop_pointer] <= current_start) || (((stop_pos[stop_pointer]%%3) + 1) != frame))) {
                        
                        stop_pointer <- stop_pointer + 1
                }
                
                stop_pointers[frame] <- stop_pointer
                
                if(stop_pointer <= length(stop_pos)) {
                        
                        if((stop_pos[stop_pointer] + 2 - current_start + 1) > k) {
                                
                                count <- count + 1
                                
                                print(count)
                                print("Frame:")
                                print(frame)
                                print("Start:")
                                print(current_start)
                                print("Stop:")
                                print(stop_pos[stop_pointer])
                                print("Length:")
                                lengths <- c(lengths, (stop_pos[stop_pointer]+ 2 - current_start + 1))
                                print(stop_pos[stop_pointer] + 2 - current_start + 1)
                                print("Sequence:")
                                print(subseq(sequence, current_start,stop_pos[stop_pointer]+2))
                        }
                }
                        
        }
}




# Reverse 3 frames

revstop_pointers <- c(0,0,0) # The stop_pointers will hold the location of the STOPS in each reading frame for each iteration of our code.

for (current_revstart in revstart_pos) {
        
        current_revstart <- current_revstart +2
        
        frame <- (current_revstart%%3) + 1
        
        revstop_pointer <- revstop_pointers[frame]
        
        if (revstop_pointer <= length(revstop_pos) && (revstop_pointer == 0 || stop_pos[revstop_pointer] > current_revstart)) {
                
                revstop_pointer <- revstop_pointer + 1
                
                while ( (revstop_pointer <= length(revstop_pos)) && (( revstop_pos[revstop_pointer] +2 >= current_revstart) || ((((revstop_pos[revstop_pointer]+2)%%3) + 1) != frame))) {
                        
                        revstop_pointer <- revstop_pointer + 1
                }
                
                revstop_pointers[frame] <- revstop_pointer
                
                if(revstop_pointer <= length(revstop_pos)) {
                        
                        if((current_revstart - revstop_pos[revstop_pointer]) + 1  > k) {
                                
                                count <- count + 1
                                
                                print(count)
                                print("Frame:")
                                print(frame)
                                print("Start:")
                                print(current_revstart)
                                print("Stop:")
                                print(revstop_pos[revstop_pointer] +2)
                                print("Length:")
                                lengths <- c(lengths, (current_revstart - revstop_pos[revstop_pointer]))
                                print(current_revstart - revstop_pos[revstop_pointer])
                                print(stop_pos[revstop_pointer] + 2 - current_revstart + 1)
                                print("Sequence:")
                                print(subseq(sequence, revstop_pos[revstop_pointer] , current_revstart))
                        }
                }
                
        }
}

lengths <- sort(lengths)

barplot(lengths)

plot(density(lengths))

bins <- seq(0,1000,50)

hist(lengths, breaks=bins, col="red", xlim=c(0,1000))


#source("http://bioconductor.org/biocLite.R")

#biocLite("biomaRt")

library(biomaRt)

listMarts() # Biomart lets you access ENSEMBLE genomes directly and download relevant information - for example, we can see a list of available databases (marts) using this command

ensembl <- useMart("ensembl") # Let's pick the ensemble database. This is just an easy way for us to load the ensemble top-level data into a variable we can read anytime.

listDatasets(ensembl) #Let's see what datasets are available in this mart


# Get Datasets for Chimp and Human

# Let's go ahead and load the data for the Chimpanzee and the Human datasets.
# We aren't retrieving the full genome sequences, just the metadata and descriptors about the genes.

Chimpanzee <- useDataset("ptroglodytes_gene_ensembl", mart = ensembl)

Human <- useDataset("hsapiens_gene_ensembl", mart = ensembl)


# Retrieve attribute lists

ChimpanzeeAttributes <- listAttributes(Chimpanzee)

HumanAttributes <- listAttributes(Human)

HumanAttributes


# Retrieve Ensemble GENE ID´s

#The ensemble Gene ID is a unique gene identifier and can be used to indicate a specific gene
#regardless of which species it is in - let's use that in our analysis.

#The getBM command retrieves the data from the ensembl servers, using the mart ID's we set up previously.

ChimpanzeeGenes <- getBM(attributes = c("ensembl_gene_id", "gene_biotype"), mart=Chimpanzee)

HumanGenes <- getBM(attributes = c("ensembl_gene_id", "gene_biotype"), mart=Human)


# Parse Data into names, types vectors

# We can parse the gene names and gene types into two separate vectors for each species, just to make things easier later.

ChimpanzeeGeneNames <- ChimpanzeeGenes[[1]]

ChimpanzeeGeneTypes <- ChimpanzeeGenes[[2]]

HumanGeneNames <- HumanGenes[[1]]

HumanGeneTypes <- HumanGenes[[2]]


# Create Table of Gene types

# We can summarize the Gene Types using the TABLE command.

ChimpanzeeTypesTable <- table(ChimpanzeeGeneTypes)

HumanTypesTable <- table(HumanGeneTypes)


# Count Protein Coding Genes

# Let's compare the number of protein coding genes in humans vs chimpanzees.

ChimpanzeeProteins <- ChimpanzeeTypesTable["protein_coding"]

HumanProteins <- HumanTypesTable["protein_coding"]



# Let's see if we can identify which genes Chimpanzees and Humans have in common.
# One of the attributes in the Chimpanzee dataset is called "ens_hs_gene" - this is the gene identifier for the human gene that is orthologous to the chimpanzee gene.

ChimpHSNum <- getBM(attributes = "ens_hs_gene", mart = Chimpanzee)

# This line retrieves the list of both Chimpanzee gene ID's along with their human orthologs (if available) in a dataframe that you can browse and/or retrieve the ID's from.

ChimpanzeeHS <- getBM(attributes = c("ensembl_gene_id","ens_hs_gene"), mart = Chimpanzee)


# Let's do the reverse now using the Human mart - the HumanAttributes table shows us
# that ptroglodytes_homolog_ensembl_gene is the attribute that list the Chimpanzee equivalent of
# human genes.

HumanCNum <- getBM(attributes = "ptroglodytes_homolog_ensembl_gene", mart = Human)

HumanC <- getBM(attributes = c("ensembl_gene_id","ptroglodytes_homolog_ensembl_gene"), mart = Human)

# The HumanCNum tells us that humans have 19k genes in common with Chimpanzees (out of their 63k).
#The HumanC table lists them by ID.