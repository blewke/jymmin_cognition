#check model
require(brms)
require(ggplot2)

m = mAsim

#m = brm(file = '../results/modelB_noslope_20210330')

#m = brm(file = '../results/ModelB_sim_20210330')
#m = brm(file = '../results/ModelA_sim_20210330')
m = brm(file = '../results/ModelB_20210326')
#m = brm(file = '../results/ModelA_20210322')


pars = parnames(m)[!startsWith(parnames(m), 'z_') & !startsWith(parnames(m), 'L_')  & !startsWith(parnames(m), 'Intercept')]
#pars = parnames(toy_B)


expose_functions(m, vectorize = TRUE)

# visually check the fit:
bayesplot::color_scheme_set('blue')
cs = bayesplot::color_scheme_get()
cs = c(cs[[1]],cs[[2]],cs[[3]],cs[[4]],cs[[5]],cs[[6]])

#pp_check(m, resp = 'ResponseTime', nsamples = 20)+
pp_check(m, resp = 'ResponseTime', nsamples = NULL)+
  xlim(0,1000)+
  xlab('ResponseTime')+
  #scale_color_discrete(name = '', labels = c(y = "data", yrep ="model predictions"))+
  scale_color_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+
  
  #scale_color_identity(name = '', labels = c("data","model predictions"))+
  #legend_relabel(c("data", "model prediction"))+
  bayesplot::legend_move(c(0.75, 0.5))
    
  
  
#pp_check(m, resp = 'ResponseTime',type = 'dens_overlay_grouped')


#cs = c(cs[[1]],cs[[2]],cs[[3]],cs[[4]],cs[[5]],cs[[6]])
cs_new = cs
cs_new[6] = cs[2]
cs_new[1] = cs[5]
cs_new[2] = cs[6]
bayesplot::color_scheme_set(cs_new)
pp_check(m, resp = 'nCorrect', type = 'bars', nsamples = NULL, prob = 1) +
  xlab('nCorrect')+
  scale_fill_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+
  scale_color_manual(name = '', values = c(cs[2], cs[5]), labels = c(y = "data", yrep ="model predictions"))



# pp_check(m, resp = 'nCorrect', type = 'bars_grouped', nsamples = 20, group = 'Jymmin') +
#   xlab('nCorrect')+
#   scale_fill_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+
#   scale_color_manual(name = '', values = c(cs[2], cs[5]), labels = c(y = "data", yrep ="model predictions"))



bayesplot::color_scheme_set(cs)





#check rhats

min(rhat(m)[pars])
max(rhat(m)[pars])


#check n_eff
f = rstan::summary(m$fit)$summary
min(f[pars,'n_eff'])
max(f[pars,'n_eff'])

# log posterior density

mean(posterior_samples(m)[,'lp__'])

#hist(posterior_samples(m)[,'lp__'])


pB = predict(mB, probs = c(0.025, 0.25, 0.75, 0.975))

pdB = cbind(mB$data,pB)


cap_at = 80
pdB$Q97.5.ResponseTime_capped = NA
pdB$Q97.5.ResponseTime_capped = mapply(min,pdB$Q97.5.ResponseTime, cap_at*pdB$ClocksInSet)

#write.csv(pdB, '../jym_data/predictionsModelB_olddata.csv')

pdB = read.csv('../jym_data/predictionsModelB_olddata.csv')

pdB$SubjectCode =  substr(pdB$SubjectCode,1,2)

fB =fitted(mB)

fdB = cbind(mB$data,fB)

###### for paper ######

jym_colors = c('orange2', 'darkred')




pdBAcc = ggplot(pdB, aes(x = nTrialScaled, y = nCorrect/ClocksInSet, color = Jymmin))+
  geom_ribbon(aes(ymin = Q2.5.nCorrect/ClocksInSet, ymax = Q97.5.nCorrect/ClocksInSet), alpha = 0.25, color = NA)+
  geom_point(alpha = 0.3, size = 0.01)+
  #geom_ribbon(aes(ymin = Q25.nCorrect/ClocksInSet, ymax = Q75.nCorrect/ClocksInSet), alpha = 0.4, color = NA, fill = 'deepskyblue1')+
  geom_line(aes(y = Estimate.nCorrect/ClocksInSet), color = 'black')+
  facet_grid(Level~SubjectCode)+
  xlab(TeX('$T$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  ylab('(number of correct responses)/(set size)')+
  scale_x_continuous(breaks= c(0,1,2),labels = c(0,1,2))+
  scale_colour_manual(values = jym_colors,
                      name = '',
                      labels = c('control', 'Jymmin'),
                      guide = guide_legend(override.aes = list(size = 1, alpha = 1) ))+
  theme(panel.grid.minor.x = element_blank(),
        #panel.grid.major = element_line(colour = "grey"),
        panel.background = element_rect(fill = "grey96"
                                        #, colour = "grey50"
                                        ))

legend_Jym_points_bottom = cowplot::get_legend(
  pdBAcc+ 
    guides(color = guide_legend(nrow = 1, override.aes = list(size = 1, alpha = 1))) +
    #guide = guide_legend(override.aes = list(size = 1, alpha = 1) )+
    theme(legend.position = "bottom")
)




pdBRT = ggplot(pdB, aes(x = nTrialScaled, y = ResponseTime/ClocksInSet, color = Jymmin))+
  
  geom_ribbon(aes(ymin = Q2.5.ResponseTime/ClocksInSet, ymax = Q97.5.ResponseTime_capped/ClocksInSet), alpha = 0.25, color = NA)+
  #geom_ribbon(aes(ymin = Q25.ResponseTime/ClocksInSet, ymax = Q75.ResponseTime/ClocksInSet), alpha = 0.4, color = NA)+
  geom_point(alpha = 0.3, size = 0.01)+
  geom_line(aes(y = Estimate.ResponseTime/ClocksInSet), color = 'black')+
  facet_grid(Level~SubjectCode)+
  ylim(0,80)+
  xlab(TeX('$T$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  ylab('(response time)/(set size)')+
  scale_x_continuous(breaks= c(0,1,2),labels = c(0,1,2))+
  scale_colour_manual(values = unname(jym_colors),
                      guide = guide_legend(override.aes = list(size = 1, alpha = 1) ))+
  theme(panel.grid.minor.x = element_blank(),
        #panel.grid.major = element_line(colour = "grey"),
        panel.background = element_rect(fill = "grey96"
                                        #, colour = "grey50"
        ))
  


cowplot::plot_grid(add_label_strips(pdBRT +  theme(legend.position="none")),
                   add_label_strips(pdBAcc +  theme(legend.position="none")),
                   legend_Jym_points_bottom,
                   rel_heights = c(1, 1, 0.1),
                   labels=c("A", "B" ,''),
                   #align = 'hv',
                   #legend,
                   label_fontfamily = "serif",
                   ncol = 1,
                   nrow = 3)




### predictive fit Model A


pA = predict(mA, probs = c(0.025, 0.25, 0.75, 0.975))

pdA = cbind(mB$data,pA) #### appended to the data of model B so I know which trials were jymmin or not

cap_at = 80
pdA$Q97.5.ResponseTime_capped = NA
pdA$Q97.5.ResponseTime_capped = mapply(min,pdA$Q97.5.ResponseTime, cap_at*pdA$ClocksInSet)

write.csv(pdA, '../jym_data/predictionsModelA_olddata.csv')

#pdA = read.csv('../jym_data/predictionsModelA_olddata.csv')






fA =fitted(mA)

fdA = cbind(mA$data,fA)

ggplot(pdA, aes(x = nTrialScaled, y = nCorrect/ClocksInSet, color = Jymmin))+
  geom_point(alpha = 0.3, size = 0.1)+
  geom_ribbon(aes(ymin = Q2.5.nCorrect/ClocksInSet, ymax = Q97.5.nCorrect/ClocksInSet), alpha = 0.2, color = NA)+
  geom_ribbon(aes(ymin = Q25.nCorrect/ClocksInSet, ymax = Q75.nCorrect/ClocksInSet), alpha = 0.4, color = NA)+
  geom_line(aes(y = Estimate.nCorrect/ClocksInSet), color = 'black')+
  facet_grid(Level~SubjectCode)+
  ggtitle('Model A')+
  scale_x_continuous(breaks= c(0,1,2),labels = c(0,1,2))


cap_at = 80
pdA$Q97.5.ResponseTime_capped = NA
pdA$Q97.5.ResponseTime_capped = mapply(min,pdA$Q97.5.ResponseTime, cap_at*pdA$ClocksInSet)

ggplot(pdA, aes(x = nTrialScaled, y = ResponseTime/ClocksInSet, color = Jymmin))+
  geom_point(alpha = 0.3, size = 0.1)+
  geom_ribbon(aes(ymin = Q2.5.ResponseTime/ClocksInSet, ymax = Q97.5.ResponseTime_capped/ClocksInSet), alpha = 0.2, color = NA)+
  geom_ribbon(aes(ymin = Q25.ResponseTime/ClocksInSet, ymax = Q75.ResponseTime/ClocksInSet), alpha = 0.4, color = NA)+
  geom_line(aes(y = Estimate.ResponseTime/ClocksInSet), color = 'black')+
  facet_grid(Level~SubjectCode)+
  ylim(0,80)+
  ggtitle('Model A')+
  scale_x_continuous(breaks= c(0,1,2),labels = c(0,1,2))

    