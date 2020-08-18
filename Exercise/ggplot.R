### Importing and testing ggplot in R

# Import ggplo2
library(ggplot2)

# Plot iris data
ggplot(data = iris, aes(x = Sepal.Length,
                        y = Sepal.Width)) +
                        geom_point()

# Plot Iris with color
ggplot(data = iris, aes(Sepal.Length,
                        Sepal.Width,
                        color = Species,
                        shape = Species)) +
                        geom_point()
