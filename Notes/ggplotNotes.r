# Alex Gibbons
# March 21, 2024
# CMPT 363 – Data Mining
# Dr. Ankur Agrawal
library("tidyverse")
library("datasets")
head(iris)
head(ChickWeight)

# as tibble function
# converts passed argument to a tibble
irises <- as_tibble(iris)
irises

chicken <- as_tibble(ChickWeight)
chicken

chicken <- rename(chicken, Weight = weight)
names(chicken)

filter(chicken, Diet == 2)

filter(irises, Sepal.Length > 5)

ggplot(data = chicken, mapping = aes(x = Time, y = Weight, color = Chick))

ggplot(chicken, aes(x = Time, y = Weight, color = Chick)) + geom_line()

ggplot(irises, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point(size = 3)
ggplot(irises, aes(x = Species, fill = Species)) + geom_bar()
ggplot(filter(irises, Sepal.Lenght >5.5), aes(x = Species, fill = Species)) + geom_bar()
ggplot(irises, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_boxplot()
ggplot(irises, aes(x = Sepal.Length, fill = Species)) + geom_histogram(bins = 20)
ggplot(irises, aes(x = Sepal.Length, color = Species)) + geom_density(size = 1)
ggplot(irises, aes(x = Sepal.Length, color = Species)) + stat_ecdf(size = 1)


ggplot(irises, aes(x = Sepal.Length)) + stat_ecdf(size = 1)

ggplot(data = chicken, aes(x = Time, y = Weight, color = Chick)) + geom_line() + geom_point()

ggplot(chicken, aes(x = Time, y = Weight, color = Chick)) + geom_line() +
  facet_wrap(facets = vars(Diet), nrow = 2, ncol = 2)
