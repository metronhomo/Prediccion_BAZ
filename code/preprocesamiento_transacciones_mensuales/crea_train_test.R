library(dplyr)

df <- readRDS("../data/df.RDS")

claves <- unique(df$FIID_CLIENTE_UNICO)
train_idx <- sample(1:length(claves), 700000)

claves_train <- claves[train_idx]
claves_test <- claves[-train_idx]

claves_train_sample <- sample(claves_train, 50000)
claves_test_sample <- sample(claves_test, 50000)

train <- df %>% filter(FIID_CLIENTE_UNICO %in% claves_train)
saveRDS(train, file = "../data/train.RDS")
rm(train)

train_sample <- df %>% filter(FIID_CLIENTE_UNICO %in% claves_train_sample)
saveRDS(train_sample, file = "../data/train_sample.RDS")
rm(train_sample)

test <- df %>% filter(FIID_CLIENTE_UNICO %in% claves_test)
saveRDS(test, file = "../data/test.RDS")
rm(test)

test_sample <- df %>% filter(FIID_CLIENTE_UNICO %in% claves_test_sample)
saveRDS(test_sample, file = "../data/test_sample.RDS")
rm(test_sample)