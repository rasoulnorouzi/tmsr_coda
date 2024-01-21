library(textrank)
library(udpipe)
kw_tr <- textrank::textrank_keywords(x = df$word, ngram_max = 2, sep = " ")
df_plot <- data.frame(x = 1:length(kw_tr$keywords$freq), freq = kw_tr$keywords$freq)
ggplot(df_plot, aes(x=x, y = freq))+
         geom_point()+
         scale_y_log10()

# Drop rare bigrams?

# Merge back with original data
words_merged <- txt_recode_ngram(df$word,
                                  compound = kw_tr$keywords$keyword,
                                  ngram = kw_tr$keywords$ngram, sep = " ")
words_merged[!words_merged %in% kw_tr$keywords$keyword]) <- NA

