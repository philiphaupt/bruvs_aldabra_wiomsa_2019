library(tidyverse)


#read BRUVs data
dat_full <- read_rds("E:/stats/aldabra/BRUVs/01_prep_data/04_size_classes_2_from_chapter_4/fish.df4.RDS")
bruvs_dat <- dat_full %>%
        select(opcode,
               depth,
               treatment = comment,
               class = Class,
               order = Order,
               family,
               maxn,
               species_name = vsppname,
               pres_abs = pres.abs) #%>%
        #filter(treatment == "baited") 
               

#number of baited samples - for checking results (result = 115 baited samples)
bruvs_dat %>% distinct(opcode)

# make sure that class is a factor - this will allow the .frop argument to work properly - and not remove zero counts
bruvs_dat$class <- as.factor(bruvs_dat$class)

#summary of elasmobranchs in BRUVs
sample_summary = bruvs_dat %>% group_by(opcode,
                                        treatment,#to make sure that all samples are included
                                        class) %>% #to sepearate results between bony fish and alsmobranchs
                       summarise(sum_maxn = sum(maxn),
                                 sum_pres = sum(pres_abs)
                       )

# obtain zero counts where there were absences!
complete_sample_summary <- sample_summary %>% complete(class, nesting(
                                                       opcode),
                                                       fill = list(sum_maxn = 0,
                                                                   sum_pres = 0))
#reset "summed" presence absence to 1/0 (this summed presence is effectively a species count at this point, but we are interested int eh percentage of samples which had elasmobranchs in them)
complete_sample_summary$pa <- 0
complete_sample_summary$pa[complete_sample_summary > 0] <- 1

#summary of results for WIOMSA
#class level sumamry and treatment
class_summary <- complete_sample_summary %>%
        group_by(treatment,
                 class) %>%
        summarise(mean_maxn = mean(sum_maxn))

#class == "Elasmobranchii")
               
               
