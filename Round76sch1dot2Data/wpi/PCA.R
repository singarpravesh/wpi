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
