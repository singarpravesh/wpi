# install.packages("factoextra")
library(factoextra)

# Step 1: Calculate principal components
data(iris)
iris_pca <- prcomp(iris[, 1:4], scale = TRUE)
names(iris_pca)
summary(iris_pca)
iris_pca$rotation
# Access the unnormalized weights (loadings) for each principal component
weights_unnormalized <- iris_pca$rotation

# Normalize the weights using the custom function
weights_normalized <- scale(weights_unnormalized, scale = FALSE) / sqrt(rowSums(weights_unnormalized^2))



# Step 2: Ideal number of components
library(factoextra)
fviz_eig(iris_pca, addlabels = TRUE)

# Step 3: biplot
fviz_pca_biplot(iris_pca, label = "var",
                habillage = iris$Species)



#-----------------------------------
# Load the Iris dataset
data(iris)

# Standardize the data
scaled_data <- scale(iris[, 1:4])

# Perform PCA
pca_result <- prcomp(scaled_data)

# View the summary of PCA
summary(pca_result)

# Access the loadings (weights)
loadings_matrix <- pca_result$rotation

print("Loadings (Weights):")
print(loadings_matrix)

# Access the scores (transformed data in the principal component space)
scores <- pca_result$x
print("Scores (Transformed Data in PC Space):")
print(scores)

# Plot the scree plot to visualize the proportion of variance explained by each PC
plot(pca_result, type = "l", main = "Scree Plot")

# Biplot to visualize both loadings and scores
biplot(pca_result)
