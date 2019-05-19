
n <- as.integer(readline(prompt = 'Number of people:'))
nc <- as.integer(readline(prompt = 'Number of people in which marker follows trait:'))

d <- NULL
for (i in seq(from = 0.0001, to = 0.5, by = 0.01)) {
 
  x <- (((i/2)^(n - nc)))*(((1-i)/2)^(nc))
  z <- 0.25^n
  y <- log10(x/z)
  
 # print(paste0('The LOD score for theta = ', i, ' is ', y, 'and the score is ',x/z ))
  d = rbind(d, data.frame(i, x,y))  
}
d$la <- ifelse(d$y == max(d$y), d$i, ' ')
library(ggplot2)
library(ggrepel)

p <- ggplot(d, aes(x = i, y = y, label = la)) +  geom_line(size = 2) + geom_point(size = 3, color = 'red')  +
  geom_text_repel(segment.color = 'transparent') + theme(legend.position = 'none') + 
  ggtitle('LOD Score Calculator') + ylab('LOD Score') + xlab('Recombination frequency (θ)')
#ggsave('lodsc.png', plot = p)
p