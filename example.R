## ------------------------------------------------------------------------
library(tidyverse)
library(lda.svi)
library(tm)

data("acq")
dtm <- DocumentTermMatrix(acq,
                          control = list(
                            weighting =
                              function(x)
                                weightTf(x),
                            stopwords = TRUE
                          ))


## ----cache=TRUE----------------------------------------------------------
bench::system_time(lda_fit <- lda_svi(dtm = dtm, K = 50, passes = 100))

## ------------------------------------------------------------------------
beta <- lda_fit$beta %>% 
  reshape2::melt() %>% 
  transmute(topic=variable,term=term,beta=value)

theta <-
  lda_fit$theta %>% 
  as.data.frame() %>% 
  # rownames_to_column('document') %>% 
  gather(-document, key = 'topic', value = 'theta')
# theta

## ------------------------------------------------------------------------
beta %>%
  group_by(variable) %>%
  top_n(value, n = 6) %>%
  arrange(desc(value)) %>%
  ggplot(aes(term, value)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  facet_wrap( ~ variable, scales = 'free')

## ------------------------------------------------------------------------
#apply(lda_fit$gamma,FUN=function(x)x/sum(x),MARGIN=1) %>%
#  colSums()

