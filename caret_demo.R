library(caret)

data("iris")

confusionMatrix(data = iris$Species, 
                reference = sample(iris$Species))
