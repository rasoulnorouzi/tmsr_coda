# In this file, write the R-code necessary to load your original data file
# (e.g., an SPSS, Excel, or SAS-file), and convert it to a data.frame. Then,
# use the function open_data(your_data_frame) or closed_data(your_data_frame)
# to store the data.

# 1. Rasoul: Extract text data (nouns and adjectives from abstracts)
# 2. Rasoul: Generate vector embeddings of text data using sciBERT
# 3. Caspar: Apply HDBSCAN to those vectors to group the terms
# 4. Caspar: Label each cluster summarizing the terms contained in it
# 5. Caspar: Visualize the results

library(worcs)
library(arrow)

df <- arrow::read_feather("coda_average_embeddings.feather")
unique_words = !duplicated(df$word)
df <- df[unique_words,]
open_data(data = df, filename = "embeddings.rdata", codebook = NULL, value_labels = NULL,
          save_expression = saveRDS(object = data, file = filename),
          load_expression = readRDS(file = filename))
worcs::git_ignore("embeddings.rdata")
