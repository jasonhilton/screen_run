library(readr)
library(tibble)
library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(stringi)
library(sensitivity)
library(lhs)
library(yaml)



results_date <- "20190529_134410"
res_path <- file.path("results", results_date)
files <- list.files(res_path, pattern = "log*")


log_1_df <- read_delim(file.path(res_path,files[1]), delim="\t")


log_1_df %<>% mutate(Step=1:n())


log_1_df %>% ggplot(aes(x=Step, y= n_arrived)) + geom_line() + 
  geom_line(aes(y=n_migrants), col="red")

parse_file <- function(file_name, res_path){
  res_df <- read_delim(file.path(res_path,file_name), delim="\t")
  point_no <- stri_extract(file_name, regex="(?<=_)[0-9]+(?=_)")
  rep_no <- stri_extract(file_name, regex="(?<=_)[0-9]+(?=\\.)")
  res_df %<>% mutate(Point=point_no, 
                    Repetition=rep_no)
  return(res_df)
}

logs <- map_df(files, parse_file, res_path=res_path)

logs %<>% group_by(Point, Repetition) %>% mutate(Step=1:n())

ggplot(logs, aes(x=Step, y=n_arrived, group=interaction(Point, Repetition)))  +
  geom_line() + 
  geom_line(aes(y=n_migrants), colour="red")

logs %<>% rename(mean_cap = `# mean_cap`)

ggplot(logs, aes(x=Step, y=mean_cap, group=interaction(Point, Repetition)))  +
  geom_line()
  
ggplot(logs, aes(x=Step, y=var_cap, group=interaction(Point, Repetition)))  +
  geom_line()

ggplot(logs, aes(x=Step, y=mean_count, group=interaction(Point, Repetition)))  +
  geom_line()

dir.create(file.path("results", "summary", results_date), recursive=T)
saveRDS(logs, file.path("results", "summary",results_date, "log.rds"))

logs_end <- logs %>% filter(Step==500)

logs_end %>% ggplot(aes(x=n_arrived)) + geom_histogram()
logs_end %>% ggplot(aes(x=mean_cap)) + geom_histogram() # what does this mean
logs_end %>% ggplot(aes(x=var_n_link)) + geom_histogram()

## we need to give the observations back to a morris object
## but because we didn't save the morris thing.
## we have to generate a new one and overwrite the design

design_morris <- read.csv("designs/morris/morris_20190529_134410.csv")


pars_config <- yaml::read_yaml("varied_pars.yaml")

varied_pars <- names(pars_config)

ranges <- rbind(map_dbl(pars_config,"min"),
                map_dbl(pars_config,"max"))

# 20 starting points.

D <- morris(factors=length(varied_pars), r=c(20,100), 
            design=list(type="oat",
                        levels=6,
                        grid.jump=2))

nobs <- dim(D$X)[1]

DX <- as.matrix(design_morris)


# 'true' minimums are zero at the moment- but this might be problematic
# is some 0-probabilities lead to no-one going anywhere
ranges[1, ]  <- ranges[1, ] + 0.025

# get D back to [0,1]
dif_range <- apply(ranges, 2, diff)

for (i in 1:dim(DX)[2]){
  DX[,i] <- (DX[,i] - ranges[1,i])/dif_range[i]
}

D$X <- DX

y1 <- logs_end %>% ungroup() %>% group_by(Point) %>% 
  summarise(n_arrived = mean(n_arrived)) %>% mutate(Point=as.numeric(Point))

y1 %<>% arrange(Point)

# check 

design_morris %<>% mutate(n_arrived=y1$n_arrived)

ggplot(design_morris, aes(x=costs_stay, y=n_arrived)) + geom_point()

design_morris %>% gather(Param, Value, -n_arrived) %>% 
  ggplot(aes(x=Value, y=n_arrived)) + geom_point() + 
  facet_wrap(~Param, scales="free")


n_arr_anova <- lm(n_arrived ~ as.factor(Point),data=logs_end) %>% anova()
(n_arr_anova$`Sum Sq`)/sum(n_arr_anova$`Sum Sq`)


D_arrived<-D
tell(D_arrived, y1$n_arrived) # tell modifies in place!!

arrived_sens_df <- tibble(Parameter=colnames(D_arrived$ee),
                 mu=apply(D_arrived$ee, 2, function(x) mean(x)),
                 mu_star=apply(D_arrived$ee, 2, function(x) mean(abs(x))),
                 sigma=apply(D_arrived$ee, 2, sd))

library(ggrepel)
ggplot(arrived_sens_df, 
       aes(x=mu, y= sigma, colour=Parameter)) + 
  geom_point(size=2) + 
  geom_text_repel(aes(mu, sigma, label=Parameter))



### cities

files <- list.files(res_path, pattern = "cities*")

res_df <- read_delim(file.path(res_path, files[[1]]), delim="\t")

parse_city_file <- function(file_name, res_path){
  # bit duplicated but whatevs.
  res_df <- read_delim(file.path(res_path, file_name), delim="\t")
  point_no <- stri_extract(file_name, regex="(?<=_)[0-9]+(?=_)")
  rep_no <- stri_extract(file_name, regex="(?<=_)[0-9]+(?=\\.)")
  res_df %<>% filter(type=="EXIT") %>% rename(id=`# id`) %>%
    select(-x, -qual,-N) %>% 
    mutate(Point=point_no, 
           Repetition=rep_no)
  return(res_df)
}

exit_df <- map_df(files, parse_city_file, res_path=res_path)

saveRDS(exit_df, file.path("results", "summary",results_date, "exits.rds"))
exit_df <- readRDS(file.path("results", "summary",results_date, "exits.rds"))

exit_df %<>% group_by(Point, Repetition) %>% 
  mutate(p_count = count/sum(count)) 

exit_ent <-  exit_df %>% group_by(Point, Repetition) %>%
  filter(p_count!=0) %>% # plogp =0 when p is 0...
  summarise(Log_ent_count = log(-sum(p_count*log(p_count))))

## so this describes how variable the dist is over cities.

## We want to also know the degree of sameness over repetitions. 
## Is the distribution in repetition 1 correlated with the distribution in rep. 2?
## The question is - what is the volume of the correlation matrix
## which should be n_reps * n_reps


# perfectly uncorrelated (unit diagonals) correlation matrix have 
# a determinant of exactly 1 - eg prod(diag(diag(50)))
# randomly sampled values will be a little bit correlated in a small sample
# by chance, and therefore will be less than 1
# e.g. mean(map_dbl(1:10000,function(x) determinant(cor(matrix(rnorm(50),10,5)))$modulus))

get_log_det <- function(D){
  X <- D %>% spread(Repetition, count) %>% 
    select(-id) %>% as.matrix()
  return(determinant(cor(X))$modulus)
}


det_df <- exit_df %>% ungroup()%>% select(id,Point,Repetition, count) %>% nest(-Point) %>%
  mutate(log_det_exit_cor = map_dbl(data, get_log_det))

det_df %<>% mutate(Point=as.numeric(Point))

det_df %>% ggplot(aes(Point, log_det_exit_cor)) + geom_point()

def_ent <- left_join(det_df, exit_ent %>% ungroup() %>% mutate(Point=as.numeric(Point)) %>% 
            group_by(Point) %>%
            summarise(Log_ent_count=mean(Log_ent_count)) %>% ungroup())


