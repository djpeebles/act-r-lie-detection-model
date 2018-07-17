setwd("/home/david/Documents/actr7/models")
packages <- c("reshape2", "ggplot2")

for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package, repos="https://www.stats.bris.ac.uk/R/")
        library(package, character.only=TRUE)
    }
}

dat <- read.csv("data.csv", header=TRUE)
dat <- melt(dat,id.vars=c("Agent"))  #melt reshapes it from wide to long

pdf("data.pdf", width=8.0, height=8.0, onefile=FALSE, paper="special")
plt1 <- ggplot(dat, aes(variable, value, group=Agent)) + geom_line(aes(color=Agent)) + geom_point(aes(color=Agent)) + scale_y_continuous(breaks = seq(0, 1, .1), limits = c(0, 1)) + labs(x = "Proportion of cues indicating truth in the training phase") + labs(y = "Proportion of truth judgements in the test phase") + scale_x_discrete(breaks=c("c1","c2","c3","c4","c5","c6","c7"), labels=c("0.2","0.3","0.4","0.5","0.6","0.7","0.8"))
plt1 <- plt1  + geom_segment(aes(x = "c1", y = 0.2, xend = "c7", yend = 0.8), size=0.2, color="yellow4", linetype="dashed")
print(plt1)
dev.off()
