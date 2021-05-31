library(lhs)
library(dplyr)
library(magrittr)
library(glue)
library(yaml)
library(purrr)
library(readr)
library(purrr)

pars_config <- yaml::read_yaml("config/varied_pars_med.yaml")

varied_pars <- names(pars_config)

ranges <- rbind(map_dbl(pars_config,"min"),
               map_dbl(pars_config,"max"))

k <- length(varied_pars)
N <- 400

D <- lhs::maximinLHS(n=N, k=k, method="build", dup=4)

# put within the right ranges (bit ugly)
D <- t(t(D) * apply(ranges, 2, diff) + ranges[1,])

Df <- data.frame(D)
colnames(Df) <- varied_pars
Df %<>% mutate(Float64="{Float64}", seed=sample(12345234, size = N,replace = F))

time_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

dir.create(file.path("designs",
                     "lhs"),
           recursive = T,
           showWarnings = F)

params <- read_file("config/params.jl_template")

Df %<>% mutate(param_file=glue(params))

dir.create(file.path("param_files"),
           recursive = T,
           showWarnings = F)

# will only work for one design at a time
iwalk(Df$param_file, function(pars,i){
  write_file(pars, 
             file.path("param_files",
               paste0("param_", i, ".jl")))
})

write.csv(Df %>% select(-param_file),
          file=file.path("designs",
                         "lhs",
                         paste0("lhs_",time_stamp, ".csv")),
          row.names = F)



