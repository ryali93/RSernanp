# http://www.sthda.com/english/wiki/r-base-graphs
head(iris)
####################################################################
# Scatter plot
x <- mtcars$wt
y <- mtcars$mpg
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE)
plot(x, y, main = "Main title", xlab = "X axis title", ylab = "Y axis title", pch = 19, frame = FALSE)
abline(lm(y ~ x, data = mtcars), col = "blue")

plot(x, y, main = "Main title", xlab = "X axis title", ylab = "Y axis title", pch = 19, frame = FALSE)
lines(lowess(x, y), col = "blue")

####################################################################
# Scatter plot 3d
library(scatterplot3d)

# Prepare the data set
x <- iris$Sepal.Length
y <- iris$Sepal.Width
z <- iris$Petal.Length
grps <- as.factor(iris$Species)

scatterplot3d(x, y, z, pch = 16)

colors <- c("#999999", "#E69F00", "#56B4E9")
scatterplot3d(x, y, z, pch = 16, color = colors[grps],
              grid = TRUE, box = FALSE, xlab = "Sepal length", 
              ylab = "Sepal width", zlab = "Petal length")

####################################################################
# Scatter plot entre matrices
pairs(iris[,1:4], pch = 19)

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(iris[,1:4], pch = 19,  cex = 0.5,
      col = my_cols[iris$Species],
      lower.panel=NULL)

####################################################################
VADeaths
x <- VADeaths[1:3, "Rural Male"]
barplot(x)
barplot(x, horiz = T)

barplot(VADeaths,
        col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"),
        legend = rownames(VADeaths))

barplot(VADeaths,
        col = c("lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"),
        legend = rownames(VADeaths), beside = TRUE)
