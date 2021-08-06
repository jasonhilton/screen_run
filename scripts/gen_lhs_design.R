library(lhs)
library(yaml)
library(purrr)

cmd_arg <- commandArgs(trailingOnly = T)

config_file <- cmd_arg[1]
pars_config <- yaml::read_yaml(config_file)

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

time_stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")


dir.create(file.path("designs",
                     "lhs"),
           recursive = T,
           showWarnings = F)


write.csv(Df, 
          file=file.path("designs",
                         "lhs",
                         paste0("lhs_",time_stamp, ".csv")),
          row.names = F)











