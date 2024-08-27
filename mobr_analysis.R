#mobr analysis
install.packages('mobr')
library(mobr)
inv_mob_in = make_mob_in(mobr_inv, mobr_inv_attr, coord_names = c('x', 'y'))
inv_stats = get_mob_stats(inv_mob_in, 'group', ref_level = 'remeandered')
plot(inv_stats)
inv_deltaS = get_delta_stats(inv_mob_in, 'group', ref_level='remeandered',
                             type='discrete', log_scale=TRUE, n_perm = 200)
plot(inv_deltaS, 'b1')



#simper analysis
library(vegan)
inv_simper <- with(mscdissdata_invertdata_[,c(1:25)], simper(as.matrix(mscdissdata_invertdata_[, 26:91]), meander_type, permutations = 100))
inv_simper
summary(inv_simper)
