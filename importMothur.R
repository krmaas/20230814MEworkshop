### Initial stats on ws23 microbiome

### setup

# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")

library(ggplot2)
library(dplyr)
library(tidyverse)


### read in data

otu <- read.table(file="../ws23.trim.contigs.good.unique.good.filter.precluster.denovo.vsearch.pick.opti_mcc.0.03.subsample.shared",
                  header=T, stringsAsFactors = FALSE, row.names = 2)
str(otu)
alpha <- read.table(file="../ws23.trim.contigs.good.unique.good.filter.precluster.denovo.vsearch.pick.opti_mcc.groups.ave-std.summary",
                    header=T, stringsAsFactors = FALSE)
str(alpha)

#### challenge Filter alpha to keep all columns but only rows that have average (ave) values

alpha <- filter(alpha,method == "ave")
# alpha <- filter(alpha, method != "std")

# read in environmental data, sample name linker
expdata <- read.table(file= "../may18ws.env.txt", header=T, stringsAsFactors = TRUE)
samples <- read.table(file = "../may18ws.sample.txt", header=T)

expdata <- left_join(expdata, samples, by="Sample")

# join expdata with alpha, selects just the samples with mothur data
alpha.expdata <- left_join(alpha, expdata, by="group")
expdata.alpha <- left_join(expdata, alpha, by="group")


# search help
?read.table


# manupilate data
#base R 
#square bracket object[row,column]
otu2 <- otu[,-1]
otu2 <- otu2[,-1]

#tidyverse
otu <- select(otu, -label, -numOtus)

maxab <- apply(otu, 2,max)
str(maxab)
# make vector all names of OTUs that are less than 1% of any sample (subsample to 7500)
n1 <- names(which(maxab<75))
str(n1)

#make otu table with just abundant OTUs
otu.ab <- otu[,-which(names(otu) %in% n1)]


### Alpha diversity!

ggplot(data=alpha.expdata, mapping=aes(x=Type, y=sobs))+
           geom_boxplot()
 
## Challenge make boxplots of samples grouped by Type for coverage, shannon, and invsimpson

ggplot(data=alpha.expdata, mapping=aes(x=Type, y=coverage))+
    geom_boxplot()

ggplot(data=alpha.expdata, mapping=aes(x=Type, y=shannon))+
    geom_boxplot()

ggplot(data=alpha.expdata, mapping=aes(x=Type, y=invsimpson))+
    geom_boxplot(varwidth = T, outlier.shape = NA)+
    geom_jitter(width=0.1, color="blue", size=4)+
    labs(y="Inverse Simpson")+
    ggtitle("Bacterial diversity by Sample Type")+
    theme_bw()



    
    
    
    

