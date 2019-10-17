library(ggplot2)
library(reshape2)

mock = NULL
con = NULL
for (i in 1:30) {
    con <- rbind(con, sprintf('Condition_%d',i))
    mock <- rbind(mock, sample(-1:1,1900,replace=T))
}

heatmap(mock)
heatmap(mock, Colv = NA, Rowv = NA, scale="column")

mock <- cbind(con, mock)
mock <- data.frame(mock)
colnames(mock) <- c('Condition', sprintf('%d',1:1900))
mock <- melt(mock, id.vars = 'Condition')

ggplot(mock) +
  geom_tile(aes(x = Condition, y = variable, fill = value)) +
  theme(axis.text.x = element_text(angle = 90))
