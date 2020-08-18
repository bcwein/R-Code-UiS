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

# Boxplot of Iris Dataset
ggplot(data = iris,
       aes(Species,
           Sepal.Length
           )
       ) +
       geom_boxplot()

# Violinplot of Iris Dataset
ggplot(data = iris,
       aes(Species,
           Sepal.Length
          )
      ) +
      geom_violin()

# Hororizontal plot boxplot
ggplot(data = iris,
       aes(Species,
           Sepal.Length
          )
      ) +
      geom_boxplot() +
      coord_flip()

# Hororizontal plot violin
ggplot(data = iris,
       aes(Species,
           Sepal.Length
          )
      ) +
        geom_violin() +
        coord_flip()


# Plot MPG engine dislacement
ggplot(mpg,
       aes(
          displ,
          hwy
          )
      ) +
      geom_point() +
      facet_wrap(~ class)
