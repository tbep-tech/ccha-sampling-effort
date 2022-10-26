library(tidyverse)
library(purrr)
library(here)
library(grid)
library(flextable)

# downsampled vegetation surveys
data(downsmps)

# richness table ------------------------------------------------------------------------------

rchests <- downsmps %>%
  mutate(
    spprch = map(downsmp, function(downsmp){
      
      downsmp %>%
        filter(!species %in% rmv) %>%
        pull(species) %>%
        unique %>%
        length
      
    })
  ) %>% 
  select(-downsmp) %>%
  unnest('spprch') %>%
  group_by(site, sample, sampint) %>%
  summarise(
    spprchvar = var(spprch),
    spprch = mean(spprch), 
    .groups = 'drop'
  )

totab <- rchests %>% 
  mutate(
    sample = factor(paste('Year', sample))
  ) %>% 
  group_by(site, sample) %>% 
  mutate(
    per = 100 * (spprch - max(spprch)) / max(spprch) 
  ) %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(
    lofit = purrr::map(data, function(x){
      
      prddat <- x %>% 
        select(sampint) %>% 
        unique
      
      spprchloess <- loess(spprch ~ sampint, x) %>% 
        predict(newdat = prddat)
      
      perloess <- loess(per ~ sampint, x) %>% 
        predict(newdata = prddat)
      
      meanrich <- x %>% 
        group_by(sample) %>% 
        filter(sampint == 0.5) %>% 
        pull(spprch) %>% 
        mean()
      
      out <- tibble(
        meanrich = meanrich, 
        sampint = prddat$sampint,
        spprchloess = spprchloess, 
        perloess = perloess
      ) 
      
      return(out)
      
    })
  )

totab1 <- toplo %>% 
  select(-lofit) %>% 
  unnest('data') %>% 
  select(-spprchvar) %>% 
  mutate(
    per = paste0(' (', abs(round(per, 0)), ')'), 
    spprch = round(spprch, 1), 
    per = ifelse(sampint == 0.5, '', per), 
    sample = gsub('Year ', '', sample)
  ) %>% 
  filter(sampint %in% c(0.5, 1:10)) %>% 
  unite('spprch', spprch, per, sep = '') %>%  
  pivot_wider(names_from = sampint, values_from = spprch) %>% 
  rename(
    Site = site, 
    Year = sample, 
  )
  as_grouped_data('site')

richtab <- flextable(totab1) %>% 
  flextable::fontsize(size = 8, part = 'body', j = 3:13) %>% 
  add_header_row(values = c('', 'Sample interval every x meters'), colwidths = c(2, 11)) %>% 
  flextable::theme_booktabs()

save(richtab, file = here('data/richtab.RData'))
                      