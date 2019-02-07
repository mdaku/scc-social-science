library(tidyverse)
library(readxl)
library(ggtheme)
library(ggpubr)

# Mark Daku
# mark.daku@gmail.com

# Load data
scc <- read_excel("./scc-dataset.xlsx")

scc <- rbind(scc, c(1999, 0,0,0,0,0,"Pre","Globe and Mail Breaking News","Rodriguez"))
scc <- rbind(scc, c(2999, 0,0,0,0,0,"Post","Globe and Mail Breaking News","Rodriguez"))
scc <- rbind(scc, c(3999, 0,0,0,0,0,"Pre","Montreal Gazette","Rodriguez"))
scc <- rbind(scc, c(4999, 0,0,0,0,0,"Post","Montreal Gazette","Rodriguez"))

# We need to gather this data first
scc <- gather(data=scc, key="frame", value="framecount", -newspaper, -case, -id, -prepost)

# Export Plot Function

export_plot <- function(gplot, filename, width=10, height=10) {
  # Export plot in PDF and EPS.
  # Notice that A4: width=11.69, height=8.27
  ggsave(paste(filename, '.pdf', sep=""), gplot, width = width, height = height)
  postscript(file = paste(filename, '.eps', sep=""), width = width, height = height)
  print(gplot)
  dev.off()
  png(file = paste(filename, '_.png', sep=""), width = width * 100, height = height * 100)
  print(gplot)
  dev.off()
}

# Now let's clean it a bit
scc$newspaper <- factor(scc$newspaper)
scc$case <- factor(scc$case, levels=c("Rodriguez","Carter"))
scc$frame <-factor(scc$frame)
levels(scc$frame) <- c("Legal Frame", "Legislative frame", "Medical frame", "Social science frame", "Strategic frame")
levels(scc$frame) <- c("Legal", "Legislative", "Medical", "Social science", "Strategic")


scc$prepost <- factor(scc$prepost, levels=c("Pre","Post"))
scc$framecount <- as.numeric(scc$framecount)

plot_specs <- theme_bw() + theme(line = element_blank(),
                    # axis.text = element_blank(),
                    # text = element_text(size=20), 
                    panel.border=element_blank(),
                    legend.position="none") + 
                    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) + 
                    theme(axis.title.x=element_text(face="italic"))


# The first thing we need to do is summarize 

carter.p1 <- scc %>%
  subset(case=="Carter") %>%
  group_by(frame) %>%
  summarise(mean = mean(framecount)) %>%
  ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") +
  scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
  xlab("Carter") + 
  ylab("") +
  ylim(0,16) +
  plot_specs + theme(axis.title.x=element_text(face="italic")) + theme(axis.text.x=element_text(angle=90))
  

rodriguez.p1 <- scc %>%
  subset(case=="Rodriguez") %>%
  group_by(frame) %>%
  summarise(mean = mean(framecount)) %>%
  ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") +
  scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
  xlab("Rodriguez") + 
  ylab("Mean number of mentions") +
  ylim(0,16) +
  plot_specs + theme(axis.title.x=element_text(face="italic"))  + theme(axis.text.x=element_text(angle=90))


# Combine the two plots
figure <- ggarrange(rodriguez.p1, carter.p1, 
                    ncol = 2, nrow = 1)
figure

export_plot(figure, "figure_1_final", 10,5)

# 1. Create mean of each category, by judge
ggplot(data=scc, aes(case, framecount, fill=frame)) + geom_bar(stat="identity") 
  


# 2. Create mean of each category, pre- and post- decision
# Pre is six months before the decision, post is six months after 

carter.p2.pre <- scc %>%
  subset(case=="Carter") %>% 
  subset(prepost=="Pre") %>%
  group_by(frame) %>%
  summarise(mean = mean(framecount)) %>%
  ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + 
  scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
  xlab("Carter") + 
  ylab("") +
  ylim(0,16) +
  plot_specs + theme(axis.title.x=element_text(face="italic")) + theme(axis.text.x=element_text(angle=90))

carter.p2.pre

carter.p2.post <- scc %>%
  subset(case=="Carter") %>% 
  subset(prepost=="Post") %>%
  group_by(frame) %>%
  summarise(mean = mean(framecount)) %>%
  ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + 
  scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
  xlab("Carter") + 
  ylab("") +
  ylim(0,16) +
  plot_specs + theme(axis.title.x=element_text(face="italic"))  + theme(axis.text.x=element_text(angle=90))

carter.p2.post



  carter.p2.post <- scc %>%
    subset(case=="Carter") %>% 
    group_by(frame, prepost) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ prepost) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Carter") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic")) + theme(axis.text.x=element_text(angle=90))


  figure <- ggarrange(rodriguez.p1, carter.p1, 
                      ncol = 2, nrow = 1)
  figure
  
  
  
  # We should be able to break this down inside here
  carter.p2.pre.post <- scc %>%
    subset(case=="Carter") %>% 
    group_by(frame, prepost) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ prepost) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Carter") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) +  theme(axis.text.x=element_text(angle=90))
  
  carter.p2.pre.post
  
  rodriguez.p2.pre.post <- scc %>%
    subset(case=="Rodriguez") %>% 
    group_by(frame, prepost) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ prepost) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Rodriguez") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + theme(axis.text.x=element_text(angle=90))
  
  
  rodriguez.p2.pre.post
  
  
  figure <- ggarrange(rodriguez.p2.pre.post, carter.p2.pre.post, 
                      ncol = 2, nrow = 1)
  figure
  
  
  ### THE ABOVE IS WRONG, WE NEED IT BROKEN DOWN SLIGHTLY DIFFERENTLY
  
  carter.p2.pre.post <- scc %>%
    subset(prepost=="Pre") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Pre") + 
    ylab("Mean number of mentions") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) + theme(axis.text.x=element_text(angle=90))
  
  carter.p2.pre.post
  
  rodriguez.p2.pre.post <- scc %>%
    subset(prepost=="Post") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Post") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) +  theme(axis.text.x=element_text(angle=90))
  
  
  rodriguez.p2.pre.post
  
  
  figure <- ggarrange(carter.p2.pre.post, rodriguez.p2.pre.post, 
                      ncol = 2, nrow = 1)
  figure
  
  
  export_plot(figure, "figure_2_final", 10, 5)
  

# 3. Media coverage by newspaper
# mean of each, divided up by judge, but faceted based on newspaper
  
  # We need to cheat a bit here
  #scc$framecount[scc$framecount==0] <-0.0001
  
  paper.1 <- scc %>%
    subset(newspaper=="Financial Post") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Financial Post") + 
    ylab("Mean number of mentions") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5))
  
  paper.1
  
  
  paper.2 <- scc %>%
    subset(newspaper=="Globe and Mail") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Globe & Mail") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5))
  
  paper.2
  

  paper.3 <- scc %>%
    subset(newspaper=="Globe and Mail Breaking News") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey","grey")) +
    xlab("Globe & Mail breaking news") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5))
  
  paper.3
  
  paper.4 <- scc %>%
    subset(newspaper=="Montreal Gazette") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Montreal Gazette") + 
    ylab("Mean number of mentions") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5))
  
  paper.4
  
  paper.5 <- scc %>%
    subset(newspaper=="Toronto Star") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Toronto Star") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5)) 
  
  paper.5
  
  
  paper.6 <- scc %>%
    subset(newspaper=="Winnipeg Free Press") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Winnipeg Free Press") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5))
  
  paper.6
  
  figure.1 <- ggarrange(paper.1, paper.2, paper.3, paper.4, paper.5, paper.6, ncol=3, nrow=2)
  figure.1 
  
  figure <- ggarrange(paper.1, paper.2, paper.5, paper.6, 
                      ncol = 4)
  figure
  
  
  
   ### VARIATION 2
  paper.1 <- scc %>%
    subset(newspaper=="Financial Post") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Financial Post") + 
    ylab("Mean number of mentions") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_blank())
  
  paper.1
  
  
  paper.2 <- scc %>%
    subset(newspaper=="Globe and Mail") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Globe & Mail") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_blank())
  
  paper.2
  
  
  
  paper.3 <- scc %>%
    subset(newspaper=="Globe and Mail Breaking News") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey","grey")) +
    xlab("Globe & Mail breaking news") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_blank())
  
  paper.3
  
  paper.4 <- scc %>%
    subset(newspaper=="Montreal Gazette") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Montreal Gazette") + 
    ylab("Mean number of mentions") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5),
                       strip.text.x = element_blank())
  
  paper.4
  
  paper.5 <- scc %>%
    subset(newspaper=="Toronto Star") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Toronto Star") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5),
                       strip.text.x = element_blank())
  
  paper.5
  
  
  paper.6 <- scc %>%
    subset(newspaper=="Winnipeg Free Press") %>% 
    group_by(frame, case) %>%
    summarise(mean = mean(framecount)) %>%
    ggplot(aes(frame, mean, fill=frame)) + geom_bar(stat="identity") + facet_wrap(~ case) +
    scale_fill_manual(values=c("grey", "grey", "grey","grey","grey")) +
    xlab("Winnipeg Free Press") + 
    ylab("") +
    ylim(0,16) +
    plot_specs + theme(axis.title.x=element_text(face="italic"),
                       axis.text.x=element_text(angle=90, hjust=1, vjust=.5),
                       strip.text.x = element_blank())
  
  paper.6
  
  figure.2 <- ggarrange(paper.1, paper.2, paper.3, paper.4, paper.5, paper.6, ncol=3, nrow=2)
  figure.2 
  
  
  export_plot(figure.2, "figure_3_final", 6.5,5)
  