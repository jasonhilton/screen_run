library(readr)
library(glue)
library(lhs)
library(yaml)
library(purrr)
library(dplyr)
library(magrittr)

pars_config <- yaml::read_yaml("config/varied_pars_med.yaml")

varied_pars <- names(pars_config)

ranges <- rbind(map_dbl(pars_config,"min"),
               map_dbl(pars_config,"max"))

# exactly 0 probability might be problematic



library(sensitivity)

# 25 starting points.

D <- morris(factors=length(varied_pars), r=c(20,100), 
             design=list(type="oat",
                         levels=6,
                         grid.jump=2))

nobs <- dim(D$X)[1]

plot(D$X)


# 'true' minimums are zero at the moment- but this might be problematic
# is some 0-probabilities lead to no-one going anywhere
ranges[1, ]  <- ranges[1, ] + 0.025

# put within the right ranges (bit ugly)
DX <- t(t(D$X) * apply(ranges, 2, diff) + ranges[1,])

Df <- data.frame(DX)

colnames(Df) <- varied_pars

Df %<>% mutate(Float64="{Float64}", 
               seed=sample(12345234, size = nobs,replace = F))



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


time_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")


dir.create(file.path("designs",
                     "morris"),
           recursive = T,
           showWarnings = F)

dir.create(file.path("designs",
                     "morris_obj"),
           recursive = T,
           showWarnings = F)


write.csv(Df %>% select(-param_file), 
          file=file.path("designs",
                         "morris",
                         paste0("morris_",time_stamp, ".csv")),
          row.names = F)

saveRDS(D, file=file.path("designs", "morris_obj",
                          paste0("morris_",time_stamp, ".rds")))









