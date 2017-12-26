

library(seqinr)

library(ape)

library(rentrez)


# Read a local FASTA file

cox1 <- read.fasta(file= "cox1.fasta" , seqtype = "AA")

length(cox1)

seq1 <- cox1[1]

# Retrieve a GENBANK sequence as a binary object

AB003468 <- read.GenBank("AB003468", as.character = "TRUE")

# Save Genbank sequence as FASTA format

write.dna(AB003468, file ="AB003468.fasta", format = "fasta", append = FALSE, nbcol = 6, colsep = " ", colw = 10)



entrez_fetch  <- entrez_search(db="nucleotide", term="human superoxide dismutase") ## search online


CloningVector <- AB003468[[1]] ## convert data to a vector

count <- count(CloningVector,1) # This retrieves the total count of each nucleotide (A, C, G, and T) and stores it in the variable count.

count(CloningVector,2)

count(CloningVector,3)

GC <- GC(CloningVector) ## Calculates the fractional G+C content of nucleic acid sequences.

# The GC content tells us what the ratio of G (guanines) and C (cystosines) residues compared to A
# (adenine) and T (thymidine) residues in a sequence is
# This is important because coding regions tend to be higher in GC.
# GC content also affects the "melting" temperature of DNA.

GCwindow <- seq(1, length(CloningVector)-200, by = 200) ## break the sequence into chunks of 200.


n <- length(GCwindow) # Find the length of the vector GCwindow

Chunks <- numeric(n) # Make a vector the same length but "blank" for us to fill

# FOR loop to compute GC per chunk

for ( i in 1:n) {
        
        chunk <- CloningVector[GCwindow[i]:(GCwindow[i]+199)]
        chunkGC <- GC(chunk)
        print(chunkGC)
        Chunks[i] <- chunkGC
}

Chunks

plot(GCwindow,Chunks,type="b",xlab="Nucleotide start position",ylab="GC content") ## This tells R to generate a plot using Gcwindow as our X axis (recall that the windows are the 200bp spans) and Chunks as our Y axis (where Chunks is the actual GC value).

slidingwindowGCplot <- function(windowsize,inputseq) {
        
        GCwindow <- seq(1, length(inputseq)-windowsize, by = windowsize)
        # Find the length of the GCwindow
        n <- length(GCwindow)
        # Make a vector the same length but "blank" for us to fill
        Chunks <- numeric(n)
        
        for (i in 1:n) {
                
                chunk <- inputseq[GCwindow[i]:(GCwindow[i]+windowsize-1)]
                
                chunkGC <- GC(chunk)   
                
                print(chunkGC)   
               
                Chunks[i] <- chunkGC  
        }
        
        plot(GCwindow,Chunks,type="b",xlab="Nucleotide start position",ylab="GC content",main=paste("GC Plot with windowsize ", windowsize))
}

slidingwindowGCplot(50,CloningVector)



library(Peptides)

aaComp(cox1[1]) ## Compute the amino acid composition of a protein sequence

aIndex(cox1) ## Compute the aliphatic index of a protein sequence

charge(cox1) ## Compute the theoretical net charge of a protein sequence

hydrophobicity(cox1[1]) ## Compute the hydrophobicity index of a protein sequence


CloningVector <- cox1[[1]] ## convert data to a vector


slidingwindow <- function(windowsize,inputseq) {
        
        window <- seq(1, length(inputseq)-windowsize, by = windowsize)
        # Find the length of the GCwindow
        n <- length(window)
        # Make a vector the same length but "blank" for us to fill
        Chunks <- numeric(n)
        
        for (i in 1:n) {
                
                chunk <- inputseq[window[i]:(window[i]+windowsize-1)]
                
                chunkGC <- hydrophobicity(chunk) 
                
                print(chunkGC)   
                
                Chunks[i] <- chunkGC  
        }
        
        plot(window,Chunks,type="b",xlab="Nucleotide start position",ylab="Hidrophobicity",main=paste("Hidrophobicity Plot with windowsize ", windowsize))
}


slidingwindow( 5 , CloningVector)





#################################

        ##  SEMANA 3 ##

#################################

#source("https://bioconductor.org/biocLite.R")

#biocLite("Biostrings")

library(Biostrings)

library(seqinr)

# read prok.fasta into a new variable called prokaryotes

prokaryotes <- read.fasta(file = "prok.fasta", seqtype = "DNA")


# Read the first two sequences in prokaryotes into seq1 and seq2 as character strings

#The first command in each pair just parses the first sequence from prokaryotes into it's own variable.
#The second command converts that into a simple text (character) string. This is necessary for Biostrings to align the sequences.

seq1 <- as.character(prokaryotes[[1]])

seq1 <- paste(seq1,collapse = "")

seq2 <- as.character(prokaryotes[[2]])

seq2 <- paste(seq2,collapse = "")


## Align seq1 and seq2 using the default settings of Biostrings

pairwiseAlignment(pattern=seq2, subject=seq1)

pairalign <- pairwiseAlignment(pattern = seq2, subject = seq1)

summary(pairalign)


# COnvert aligment to FASTA and save


pairalignString <- BStringSet( c( toString( subject(pairalign) ), toString(pattern(pairalign)))) # First we need to convert the alignment to strings, using a different Biostrings command called BStringSet

writeXStringSet(pairalignString, "aligned.txt", format="FASTA") # write an XStringSet object to a file


coxgenes <- read.fasta(file = "cox1multi.fasta", seqtype="AA")

cox1 <- as.character(coxgenes[[1]])

cox2 <- as.character(coxgenes[[2]])

# A simple dotplot

dotPlot(cox1, cox2, main = "Human vs Mouse Cox1 Dotplot")

# A nicer, windowed dotplot

dotPlot(cox1, cox2, wsize = 3, wstep = 3, nmatch = 3, main = "Human vs Mouse Cox1 Dotplot\nwsize = 3, wstep = 3, nmatch = 3")

# A dotplot of the first 100 residues of the two sequences

dotPlot(cox1[1:100], cox2[1:100], wsize = 3, wstep = 3, nmatch = 3, main = "Human vs Mouse Cox1 first 100 AA Dotplot\nwsize = 3, wstep = 3, nmatch = 3")

#From your DNA and Proteins classes you should remember the difference between local and global alignments:
#        In a local alignment, you are matching your query with a substring (fragment) of your subject sequence. 
#        In a global alignment you perform an end to end alignment between the two.
#        You may end up with a lot of gaps in global alignment if the sizes of query and subject are dissimilar).
#        Local alignments may also have gaps.

#       There is in fact a third option - the overlap alignment.  Overlap alignments are used when you are

#       trying to assemble overlapping sequences, e.g. from multiple sequencing runs in a genome
# assembly.

#       Overlap alignments focus on making the best alignments between the end of one sequence and
#the end of another.



#source("https://bioconductor.org/biocLite.R")

#biocLite("Biostrings")

#biocLite("msa")

library(msa)

# Multiples Sequence Aligments

# These two lines read in the exact same fasta files we used previously, but create StringSets

coxAA <- readAAStringSet("cox1multi.fasta")

prokDNA <- readDNAStringSet("prok.fasta")

## Align the sequences

coxAligned <- msa(coxAA)

coxAligned

print(coxAligned, show="complete")

prokAligned <- msa(prokDNA)

prokAligned

print(prokAligned, show="complete")


# Export aligments as FASTA

prokAlignStr <- as(prokAligned, "DNAStringSet")

writeXStringSet(prokAlignStr, file="prokAligned.fasta")


coxAlignStr <- as(coxAligned, "AAStringSet")

writeXStringSet(coxAlignStr, file="coxAligned.fasta")

# Export alignment in phylip format

write.phylip(coxAligned, "coxAligned.phylip")

###################################################
###### Phylogenetic Reconstruction ################
###################################################

# Convert prokaryotic aligment to seqinr format

prokAligned2 <- msaConvert(prokAligned, type="seqinr::alignment")

library(seqinr)

# Generate distance matrix using seqinr
prokdist <- dist.alignment(prokAligned2, "identity")

prokdist

library(ape)

# Generate Neighbor - Joining Distance Tree
prokTree <- nj(prokdist)

plot(prokTree) 


#### Maximum Parsimony Tree #####

library(phangorn) 

prokAligned3 <- msaConvert(prokAligned, type="phangorn::phyDat")

ParsTree <- pratchet(prokAligned3)

plot(ParsTree)

#### Maximum Likelihood Tree ###

## Create initial statistical test of the distance matrix with the dataset
fit <- pml(prokTree, prokAligned3)
## attempt to improve the tree using Jukes-Cantor model
fitJC <- optim.pml(fit, model = "JC", rearrangement = "stochastic")
## attempt to improve the tree using K80 model
fitK80 <- optim.pml(fit, model = "K80", rearrangement = "stochastic")

plot(fitJC)

plot(fitK80)

# Bootstrap the optimized ML tree
bootstrapped <- bootstrap.pml(fitJC, bs=100, optNni=TRUE, multicore=FALSE, control = pml.control(trace=0))
# Plot the bootstrapped tree
plotBS(midpoint(fitJC$tree), bootstrapped, p = 50, type="p")
