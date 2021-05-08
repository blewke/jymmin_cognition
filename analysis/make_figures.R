## make the figures in for the document



########## loading, precomputing #########-------------------------------



### load all the models and functions
require(brms)
require(tibble)

#only for plotting
require(ggplot2)
require(cowplot)
require(latex2exp)
library(grid)
library(gtable)



# response distributions 'families'
source('../helper_functions/sum_shifted_lognormal family.R')
source('../helper_functions/beta binomial family.R')

#the fitted models
mA = brm(file = '../results/ModelA_20210322')
mB = brm(file = '../results/ModelB_20210326')
mBsim = brm(file = '../results/ModelB_sim_20210330')
mAsim = brm(file = '../results/ModelA_sim_20210330')
mBnoslope = brm(file = '../results/modelB_noslope_20210330')


# a convenient plotting function:
source('../analysis/plot_add_label_strips.R')

# define colors
jym_colors = c('orange2', 'darkred')




########## predictions from models #########-------------------------------
## to save computing time, the commented lines have been run once and the results have been saved to use again in the plots


#when predictions are not already precomputed and loaded from files, this line is needed
#expose_functions(mA, vectorize = TRUE)


#### load predictions from model B, on original data set:

#pB = predict(mB, probs = c(0.025, 0.25, 0.75, 0.975))
#pdB = cbind(mB$data,pB)
#cap_at = 80
#pdB$Q97.5.ResponseTime_capped = NA
#pdB$Q97.5.ResponseTime_capped = mapply(min,pdB$Q97.5.ResponseTime, cap_at*pdB$ClocksInSet)
#write.csv(pdB, '../jym_data/predictionsModelB_olddata.csv')

pdB = read.csv('../jym_data/predictionsModelB_olddata.csv')
pdB$SubjectCode =  substr(pdB$SubjectCode,1,2)

#### load predictions from model A, on original data

#pA = predict(mA, probs = c(0.025, 0.25, 0.75, 0.975))
#pdA = cbind(mB$data,pA) #### appended to the data of model B so I know which trials were jymmin or not
#cap_at = 80
#pdA$Q97.5.ResponseTime_capped = NA
#pdA$Q97.5.ResponseTime_capped = mapply(min,pdA$Q97.5.ResponseTime, cap_at*pdA$ClocksInSet)
#write.csv(pdA, '../jym_data/predictionsModelA_olddata.csv')

pdA = read.csv('../jym_data/predictionsModelA_olddata.csv')
pdA$SubjectCode =  substr(pdA$SubjectCode,1,2)


###load predictions for new data points, model B:


#nd = expand.grid(SubjectCode = unique(mB$data$SubjectCode)[order(unique(mB$data$SubjectCode))], ClocksInSet = 25, Level = c(5,10,15,20), nTrialScaled = seq(from =0, to = 3.5, by = 0.1), Jymmin = c('No', 'Yes'))

#pmBnd = cbind(nd, predict(mB, newdata = nd, probs = c(0.025, 0.25, 0.75, 0.975)))

#write.csv(pmBnd, '../jym_data/predictionsModelB_newdata.csv')

pmBnd = read.csv('../jym_data/predictionsModelB_newdata.csv')
pmBnd$SubjectCode =  substr(pmBnd$SubjectCode,1,2)

example_participants = c('A3', 'B1','E2', 'C1', 'F2', 'H3')

pmBnd_subset = subset(pmBnd, Level == 10 & SubjectCode %in% example_participants & nTrialScaled < 2.1)

pmBnd_subset = subset(pmBnd, Level == 10 & SubjectCode %in% example_participants & nTrialScaled < 2.1)





##### predictions on set level from Model B

#nd2 = expand.grid(SubjectCode = example_participants, ClocksInSet = c(1,25), Level = c(10), #nTrialScaled = c(0,1), Jymmin = c('No', 'Yes'))
#pnd_samples = predict(mB, nd2, summary = FALSE)
#nd2_rep = nd2[rep(seq_len(nrow(nd2)),4800),]
#Acc_pnd_samples = as_tibble(pnd_samples[,,'nCorrect'])
#RT_pnd_samples = as_tibble(pnd_samples[,,'ResponseTime'])
#Acc_long = tidyr::pivot_longer(Acc_pnd_samples, cols = colnames(Acc_pnd_samples), values_to = 'nCorrect')
#RT_long = tidyr::pivot_longer(RT_pnd_samples, cols = colnames(RT_pnd_samples), values_to = 'ResponseTime')
#sample_data = cbind(nd2_rep, nCorrect = Acc_long$nCorrect, ResponseTime = RT_long$ResponseTime)
#write.csv(sample_data, '../jym_data/predictionsModelB_newdata_noSummary.csv')

sample_data = read.csv('../jym_data/predictionsModelB_newdata_noSummary.csv')
sample_data$SubjectCode =  substr(sample_data$SubjectCode,1,2)




##load predictions from model C (or mB_noslope)


#pnd_samples_ns = predict(mBnoslope, nd2, summary = FALSE)
#Acc_pnd_samples_ns = as_tibble(pnd_samples_ns[,,'nCorrect'])
#RT_pnd_samples_ns = as_tibble(pnd_samples_ns[,,'ResponseTime'])
#Acc_long_ns = tidyr::pivot_longer(Acc_pnd_samples_ns, cols = colnames(Acc_pnd_samples_ns), values_to = 'nCorrect')
#RT_long_ns = tidyr::pivot_longer(RT_pnd_samples_ns, cols = colnames(RT_pnd_samples_ns), values_to = 'ResponseTime')
#sample_data_ns = cbind(nd2_rep, nCorrect = Acc_long_ns$nCorrect, ResponseTime = RT_long_ns$ResponseTime)
#write.csv(sample_data_ns, '../jym_data/predictionsModelC_newdata_noSummary.csv')
sample_data_ns = read.csv('../jym_data/predictionsModelC_newdata_noSummary.csv')
sample_data_ns$SubjectCode =  substr(sample_data_ns$SubjectCode,1,2)




########## figure 1 #########-------------------------------
#Posterior predictive checks



#####model B

bayesplot::color_scheme_set('blue')
cs = bayesplot::color_scheme_get()
cs = c(cs[[1]],cs[[2]],cs[[3]],cs[[4]],cs[[5]],cs[[6]])

RTB = pp_check(mB, resp = 'ResponseTime', nsamples = NULL)+
  xlim(0,1000)+
  xlab('response time in seconds')+
  #scale_color_discrete(name = '', labels = c(y = "data", yrep ="model predictions"))+
  scale_color_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+

  #scale_color_identity(name = '', labels = c("data","model predictions"))+
  #legend_relabel(c("data", "model prediction"))+
  bayesplot::legend_move(c(0.75, 0.5))+
  ylab('density')+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        axis.text.y = element_text(),
        axis.ticks.y  = element_line())+
  scale_y_continuous(breaks=seq(0,0.008, 0.002))+
  ylim(0,0.0075)



cs_new = cs
cs_new[6] = cs[2]
cs_new[1] = cs[5]
cs_new[2] = cs[6]
bayesplot::color_scheme_set(cs_new)

AccB = pp_check(mB, resp = 'nCorrect', type = 'bars', nsamples = NULL, prob = 1) +
  xlab('number of correct responses')+
  ylab('count')+
  scale_fill_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+
  scale_color_manual(name = '', values = c(cs[2], cs[5]), labels = c(y = "data", yrep ="model predictions"))+
  bayesplot::legend_move(c(0.4, 0.5))



bayesplot::color_scheme_set(cs)




######model A

bayesplot::color_scheme_set('teal')
cs = bayesplot::color_scheme_get()
cs = c(cs[[1]],cs[[2]],cs[[3]],cs[[4]],cs[[5]],cs[[6]])

RTA = pp_check(mA, resp = 'ResponseTime', nsamples = NULL)+
  xlim(0,1000)+
  xlab('response time in seconds')+
  #scale_color_discrete(name = '', labels = c(y = "data", yrep ="model predictions"))+
  scale_color_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+

  #scale_color_identity(name = '', labels = c("data","model predictions"))+
  #legend_relabel(c("data", "model prediction"))+
  bayesplot::legend_move(c(0.75, 0.5))+
  ylab('density')+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        axis.text.y = element_text(),
        axis.ticks.y  = element_line())+
  scale_y_continuous(breaks=seq(0,0.008, 0.002))+
  ylim(0,0.0075)


cs_new = cs
cs_new[6] = cs[2]
cs_new[1] = cs[5]
cs_new[2] = cs[6]
bayesplot::color_scheme_set(cs_new)
AccA  = pp_check(mA, resp = 'nCorrect', type = 'bars', nsamples = NULL, prob = 1) +
  xlab('number of correct responses')+
  ylab('count')+
  scale_fill_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+
  scale_color_manual(name = '', values = c(cs[2], cs[5]), labels = c(y = "data", yrep ="model predictions"))+
  bayesplot::legend_move(c(0.4, 0.5))




bayesplot::color_scheme_set(cs)


plot_grid(RTA,RTB,AccA,AccB,
          labels=c("A", "B", 'C','D' ),
          ncol = 2, 
          nrow = 2,
          align = 'hv',
          label_fontfamily = "serif")






########## figure 2 #########-------------------------------
# Model fit of Model A



pdAAcc = ggplot(pdA, aes(x = nTrialScaled, y = nCorrect/ClocksInSet, color = Jymmin))+
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
  pdAAcc+ 
    guides(color = guide_legend(nrow = 1, override.aes = list(size = 1, alpha = 1))) +
    #guide = guide_legend(override.aes = list(size = 1, alpha = 1) )+
    theme(legend.position = "bottom")
)




pdART = ggplot(pdA, aes(x = nTrialScaled, y = ResponseTime/ClocksInSet, color = Jymmin))+
  
  geom_ribbon(aes(ymin = Q2.5.ResponseTime/ClocksInSet, ymax = Q97.5.ResponseTime_capped/ClocksInSet), alpha = 0.25, color = NA)+
  #geom_ribbon(aes(ymin = Q25.ResponseTime/ClocksInSet, ymax = Q75.ResponseTime/ClocksInSet), alpha = 0.4, color = NA)+
  geom_point(alpha = 0.3, size = 0.01)+
  geom_line(aes(y = Estimate.ResponseTime/ClocksInSet), color = 'black')+
  facet_grid(Level~SubjectCode)+
  ylim(0,80)+
  xlab(TeX('$T$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  ylab('(response time in seconds)/(set size)')+
  scale_x_continuous(breaks= c(0,1,2),labels = c(0,1,2))+
  scale_colour_manual(values = unname(jym_colors),
                      guide = guide_legend(override.aes = list(size = 1, alpha = 1) ))+
  theme(panel.grid.minor.x = element_blank(),
        #panel.grid.major = element_line(colour = "grey"),
        panel.background = element_rect(fill = "grey96"
                                        #, colour = "grey50"
        ))
  


cowplot::plot_grid(add_label_strips(pdART +  theme(legend.position="none")),
                   add_label_strips(pdAAcc +  theme(legend.position="none")),
                   legend_Jym_points_bottom,
                   rel_heights = c(1, 1, 0.1),
                   labels=c("A", "B" ,''),
                   #align = 'hv',
                   #legend,
                   label_fontfamily = "serif",
                   ncol = 1,
                   nrow = 3)





########## figure 3 #########-------------------------------
#Model fit of Model B

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

# legend_Jym_points_bottom = cowplot::get_legend(
#   pdBAcc+ 
#     guides(color = guide_legend(nrow = 1, override.aes = list(size = 1, alpha = 1))) +
#     #guide = guide_legend(override.aes = list(size = 1, alpha = 1) )+
#     theme(legend.position = "bottom")
# )




pdBRT = ggplot(pdB, aes(x = nTrialScaled, y = ResponseTime/ClocksInSet, color = Jymmin))+
  
  geom_ribbon(aes(ymin = Q2.5.ResponseTime/ClocksInSet, ymax = Q97.5.ResponseTime_capped/ClocksInSet), alpha = 0.25, color = NA)+
  #geom_ribbon(aes(ymin = Q25.ResponseTime/ClocksInSet, ymax = Q75.ResponseTime/ClocksInSet), alpha = 0.4, color = NA)+
  geom_point(alpha = 0.3, size = 0.01)+
  geom_line(aes(y = Estimate.ResponseTime/ClocksInSet), color = 'black')+
  facet_grid(Level~SubjectCode)+
  ylim(0,80)+
  xlab(TeX('$T$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  ylab('(response time in seconds)/(set size)')+
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







########## figure 4 #########-------------------------------
#Prior and posterior for Jym params in Model B



jym_pars = c('b_nCorrect_JymminYes',
               'b_nCorrect_JymminYes:nTrialScaled',
               'b_ResponseTime_JymminYes',
               'b_ResponseTime_JymminYes:nTrialScaled')

psB = posterior_samples(mB,pars = jym_pars)
names(psB) <- sub(':', '', names(psB))

prsB = prior_samples(mB, jym_pars)
names(prsB) <- sub(':', '', names(prsB))

bayesplot::color_scheme_set('blue')
cs = bayesplot::color_scheme_get()
posterior = cs[[4]]
prior = cs[[2]]


colorsB = c('prior' = prior, 'posterior' = posterior)

#first subplot

alphaRT = ggplot(posterior_samples(mB), aes(x = b_ResponseTime_JymminYes))+
  geom_density(aes(fill = 'posterior'), alpha = 0.8)+
  geom_density(data = prior_samples(mB), aes(fill = 'prior'), alpha = 0.5)+
  geom_vline(aes(xintercept = 0), color = 'black', linetype = 'dashed')+
  xlim(-1.5,1.5)+
  xlab(TeX('$\\alpha_{RT,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  scale_fill_manual(name = '', values = colorsB ,labels = c('posterior', 'prior'))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))

 
# get the legened
legend_b = cowplot::get_legend(
  alphaRT + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

#next subplot

alphaAcc = ggplot(posterior_samples(mB), aes(x = b_nCorrect_JymminYes))+
  geom_density(fill = posterior, alpha = 0.8)+
  geom_density(data = prior_samples(mB), fill = prior, alpha = 0.5)+
  geom_vline(aes(xintercept = 0), color = 'black', linetype = 'dashed')+
  xlim(-2,2)+
  xlab(TeX('$\\alpha_{Acc,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))


#next subplot
betaAcc = ggplot(psB, aes(x = b_nCorrect_JymminYesnTrialScaled))+
  geom_density(fill = posterior, alpha = 0.8)+
  geom_density(data = prsB, fill = prior, alpha = 0.5)+
  geom_vline(aes(xintercept = 0), color = 'black', linetype = 'dashed')+
  xlim(-2,2)+
  xlab(TeX('$\\beta_{Acc,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))



#next subplot
betaRT = ggplot(psB, aes(x = b_ResponseTime_JymminYesnTrialScaled))+
  geom_density(fill = posterior, alpha = 0.8)+
  geom_density(data = prsB, fill = prior, alpha = 0.5)+
  geom_vline(aes(xintercept = 0), color = 'black', linetype = 'dashed')+
  xlim(-1.5,1.5)+
  xlab(TeX('$\\beta_{RT,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))


# put the plots together
p = cowplot::plot_grid(alphaRT + theme(legend.position="none"),
                   alphaAcc,
                   betaRT,
                   betaAcc,
                   labels=c("A", "B", 'C','D' ),
                   ncol = 2,
                   nrow = 2,
                   align = 'hv',
                   label_fontfamily = "serif")


# add the legend
cowplot::plot_grid(p,legend_b, ncol = 1,rel_heights = c(1, .1))

########## figure 5 #########-------------------------------
#posterior predictive RT


ggplot(sample_data[sample_data$nTrialScaled ==0 & sample_data$ClocksInSet == 25,])+
  geom_density(aes(x = ResponseTime/ClocksInSet, fill= Jymmin, y = ..density..), alpha = 0.5, size = 0.1, position="identity",binwidth = 1)+
  facet_wrap(~SubjectCode,scales="free",nrow = 2)+
  xlim(0,50)+
  xlab('(response time in seconds)/(set size)')+
  scale_fill_manual(values = unname(jym_colors), name = '', labels = c('control', 'Jymmin'))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
        ))



########## figure 6 #########-------------------------------
#Posterior predictive learning rate


ggplot(pmBnd_subset, aes(x = nTrialScaled, y = Estimate.ResponseTime/ClocksInSet, color = Jymmin))+
  geom_ribbon(aes(ymin = Q2.5.ResponseTime/ClocksInSet, ymax = Q97.5.ResponseTime_capped/ClocksInSet, fill = Jymmin), alpha = 0.2, color = NA)+
  geom_ribbon(aes(ymin = Q25.ResponseTime/ClocksInSet, ymax = Q75.ResponseTime_capped/ClocksInSet, fill = Jymmin), alpha = 0.4, color = NA)+
  geom_line(aes(y = Estimate.ResponseTime/ClocksInSet, color = Jymmin))+
  facet_wrap(~SubjectCode, scales = 'free')+
  ylim(0,45)+
  ylab('(response time in seconds)/(set size)')+
  xlab(TeX('$T$',italic=TRUE))+
  scale_x_continuous(breaks= c(0,1,2),labels = c(0,1,2))+
  scale_colour_manual(values = unname(jym_colors), name = '', labels = c('control', 'Jymmin'))+
  scale_fill_manual(values = unname(jym_colors), name = '', labels = c('control', 'Jymmin'))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))




########## figure 7#########-------------------------------
#Posterior Predictive Acc


ggplot(sample_data[sample_data$nTrialScaled == 0 & sample_data$ClocksInSet == 25,])+
  geom_histogram(aes(x = nCorrect/ClocksInSet, fill= Jymmin, 
                     color = Jymmin,
                     y = ..density..),
                 alpha = 0.5, size = 0.1, position="identity",binwidth = 1/25)+
  facet_wrap(~SubjectCode, scales = 'free_x')+
  xlim(0,1)+
  xlab('(number of correct responses)/(set size)')+
  scale_colour_manual(values = unname(jym_colors), name = '', labels = c('control', 'Jymmin'))+
  scale_fill_manual(values = unname(jym_colors), name = '', labels = c('control', 'Jymmin'))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
        ))


########## figure 8 #########-------------------------------
#Posterior predictive Acc learning rate

ggplot(pmBnd_subset, aes(x = nTrialScaled, y = Estimate.nCorrect/ClocksInSet, color = Jymmin))+
  geom_ribbon(aes(ymin = Q2.5.nCorrect/ClocksInSet, ymax = Q97.5.nCorrect/ClocksInSet, fill = Jymmin), alpha = 0.2, color = NA)+
  geom_ribbon(aes(ymin = Q25.nCorrect/ClocksInSet, ymax = Q75.nCorrect/ClocksInSet, fill = Jymmin), alpha = 0.4, color = NA)+
  geom_line(aes(y = Estimate.nCorrect/ClocksInSet))+
  facet_wrap(~SubjectCode, scales = 'free')+
  xlim(0,2)+
  ylim(0.4,1)+
  ylab('(number of correct responses)/)set size)')+
  xlab(TeX('$T$',italic=TRUE))+
  scale_x_continuous(breaks= c(0,1,2),labels = c(0,1,2))+
  scale_colour_manual(values = unname(jym_colors), name = '', labels = c('control', 'Jymmin'))+
  scale_fill_manual(values = unname(jym_colors), name = '', labels = c('control', 'Jymmin'))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))


########## figure 9 #########-------------------------------
#Parameter correlation intercept and learning rate


jym_pars = c('b_nCorrect_JymminYes',
             'b_nCorrect_JymminYes:nTrialScaled',
             'b_ResponseTime_JymminYes',
             'b_ResponseTime_JymminYes:nTrialScaled')


psB = posterior_samples(mB,pars = jym_pars)
names(psB) <- sub(':', '', names(psB))

bayesplot::color_scheme_set('blue')
cs = bayesplot::color_scheme_get()


# first subplot
corAcc = ggplot(psB,aes(x = b_nCorrect_JymminYesnTrialScaled, y = b_nCorrect_JymminYes))+
  geom_point(size = 0.1, alpha = 0.3, color = cs[5])+
  xlab(TeX('$\\beta_{Acc,Jymmin}$',italic=TRUE))+
  ylab(TeX('$\\alpha_{Acc,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))

# second subplot
corRT = ggplot(psB,aes(x = b_ResponseTime_JymminYesnTrialScaled, y = b_ResponseTime_JymminYes))+
  geom_point(size = 0.1, alpha = 0.3, color = cs[5])+
  xlab(TeX('$\\beta_{RT,Jymmin}$',italic=TRUE))+
  ylab(TeX('$\\alpha_{RT,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))

#put them together
cowplot::plot_grid(corRT + theme(legend.position="none"),
                   corAcc +  theme(legend.position="none"),
                   labels=c("A", "B"),
                   ncol = 2,
                   nrow = 1,
                   align = 'hv',
                   label_fontfamily = "serif")
  




########## figure 10 #########-------------------------------
#late trials


group.colors <- c("blue1",'red',"dodgerblue2","deepskyblue")

# make the plot
pLate = ggplot(mB$data[mB$data$nTrialScaled > 1.65,], aes(x = nTrialScaled, y = nCorrect/ClocksInSet, shape = Jymmin))+
  geom_point(aes(color = substr(SubjectCode,1,2)), size = 0.7, alpha = 0.8)+
  facet_wrap(~Level)+
  scale_color_manual(name = "participant", values=group.colors)+
  scale_shape_manual(name = "condition", values = c(19,17), labels = c('control', 'Jymmin'))+
  ylim(0.55,1)+
  xlab(TeX('$T$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  ylab('(number of correct reponses)/(set size)')+
  scale_x_continuous(breaks=c(1.7,1.9,2.1))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))

#add some whitespace above the figure, only necessary because of its placement in the document
pLate = pLate + theme(plot.margin = unit(c(1.5, 0.1, 0.1, 0.1), "cm"))

### now add a label srip to the top, minimally adapted from:
##https://stackoverflow.com/questions/36941197/overall-label-for-facets


labelT = "level"

# Get the ggplot grob
z <- ggplotGrob(pLate)

# Get the positions of the strips in the gtable: t = top, l = left, ...
posT <- subset(z$layout, grepl("strip-t", name), select = t:r)

# and a new row on top of current top strips
height <- z$heights[min(posT$t)]  # height of current top strips

z <- gtable_add_rows(z, height, min(posT$t)-1)

stripT <- gTree(name = "Strip_top", children = gList(
  rectGrob(gp = gpar(col = NA, fill = "grey85")),
  textGrob(labelT, gp = gpar(fontsize = 8.8, col = "grey10"))))

# Position the grobs in the gtable
z <- gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")

# Add small gaps between strips
z <- gtable_add_rows(z, unit(1/5, "line"), min(posT$t))

# Draw it
grid.newpage()
grid.draw(z)



########## figure 11 #########-------------------------------
#Posterior distributions Jym params Model C

jym_pars_incpt = c('b_nCorrect_JymminYes',
                   'b_ResponseTime_JymminYes')

cs = bayesplot::color_scheme_set('blue')
posterior = cs[[4]]
prior = cs[[2]]

cs = bayesplot::color_scheme_set('pink')
posterior_ns = cs[[4]]

bayesplot::color_scheme_set('blue')

ps_ns = posterior_samples(mBnoslope,pars = jym_pars_incpt)

colors = c('prior' = prior, 'posterior B' = posterior, 'posterior C' = posterior_ns)

# first subplot
alphaRTC = ggplot(posterior_samples(mBnoslope, pars = jym_pars_incpt), aes(x = b_ResponseTime_JymminYes))+
  geom_density(data = prior_samples(mBnoslope, pars = jym_pars_incpt), aes(fill = 'prior'), alpha = 0.5)+
  geom_density(data = posterior_samples(mB, pars = jym_pars_incpt), aes(fill = 'posterior B'), alpha = 0.8)+
  geom_vline(aes(xintercept = 0), color = 'black', linetype = 'dashed')+
  geom_density(aes(fill = 'posterior C'), alpha = 0.8)+
  xlim(-0.15,0.15)+
  xlab(TeX('$\\alpha_{RT,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  scale_fill_manual(name = '', values = colors,labels = c('posterior in Model B', 'posterior in Model C', 'prior'))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))

# get the legend
legend_bC = cowplot::get_legend(
  alphaRTC + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)


#second subplot
alphaAccC = ggplot(posterior_samples(mBnoslope, pars = jym_pars_incpt), aes(x = b_nCorrect_JymminYes))+
  geom_density(data = prior_samples(mBnoslope, pars = jym_pars_incpt ), aes( fill = 'prior'), alpha = 0.5)+
  geom_density(data = posterior_samples(mB, pars = jym_pars_incpt), aes(fill = 'posterior B'), alpha = 0.8)+
  geom_vline(aes(xintercept = 0), color = 'black', linetype = 'dashed')+
  geom_density(aes(fill = 'posterior C'), alpha = 0.8,)+
  xlim(-0.45,0.45)+
  xlab(TeX('$\\alpha_{Acc,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  scale_fill_manual(name = '', values = colors,labels = c('posterior in Model B', 'posterior in Model C', 'prior'))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))


# add plots
pC = cowplot::plot_grid(alphaRTC + theme(legend.position="none"),
                       alphaAccC +  theme(legend.position="none"),
                       labels=c("A", "B"),
                       ncol = 2,
                       nrow = 1,
                       align = 'hv',
                       #legend,
                       label_fontfamily = "serif")


#add legend
cowplot::plot_grid(pC,legend_bC, ncol = 1,rel_heights = c(0.5, .1))


########## figure 12 #########-------------------------------
#Posterior predictive Acc Model C


ggplot(sample_data_ns[sample_data_ns$nTrialScaled ==0 & sample_data_ns$ClocksInSet == 25,])+
  geom_histogram(aes(x = nCorrect/ClocksInSet, fill= Jymmin, 
                     color = Jymmin,
                     y = ..density..),
                 alpha = 0.5, size = 0.1, position="identity",binwidth = 1/25)+
  facet_wrap(~SubjectCode, scales = 'free_x')+
  xlim(0,1)+
  xlab('(number of correct responses)/(set size)')+
  scale_colour_manual(values = unname(jym_colors), name = '', labels = c('control', 'Jymmin'))+
  scale_fill_manual(values = unname(jym_colors), name = '', labels = c('control', 'Jymmin'))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
        ))




########## figure 13 #########-------------------------------
# simulated data set


simAcc = ggplot(mBsim$data, aes(x = nTrialScaled, y = nCorrect/ClocksInSet, color = Jymmin))+
  #geom_ribbon(aes(ymin = Q2.5.nCorrect/ClocksInSet, ymax = Q97.5.nCorrect/ClocksInSet), alpha = 0.25, color = NA)+
  geom_point(alpha = 0.3, size = 0.01)+
  #geom_ribbon(aes(ymin = Q25.nCorrect/ClocksInSet, ymax = Q75.nCorrect/ClocksInSet), alpha = 0.4, color = NA, fill = 'deepskyblue1')+
  #geom_line(aes(y = Estimate.nCorrect/ClocksInSet), color = 'black')+
  facet_grid(Level~SubjectCode)+
  #ggtitle('Simulated data')+
  scale_x_continuous(breaks= c(0,1,2),labels = c(0,1,2))+
  xlab(TeX('$T$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  ylab('(number of correct responses)/(set size)')+
  #scale_colour_manual(values = unname(jym_colors),
  scale_colour_manual(values = jym_colors,
                      name = 'condition',
                      labels = c('control', 'Jymmin'),
                      guide = guide_legend(override.aes = list(size = 1, alpha = 1) ))+
  theme(panel.grid.minor.x = element_blank(),
        #panel.grid.major = element_line(colour = "grey"),
        panel.background = element_rect(fill = "grey96"
                                        #, colour = "grey50"
        ))


# Labels 
#labelR = "level"
#labelT = "participant"

# Get the ggplot grob
zsimAcc = add_label_strips (simAcc + theme(legend.position="none"))

# zsimAcc <- ggplotGrob(simAcc + theme(legend.position="none"))
# 
# # Get the positions of the strips in the gtable: t = top, l = left, ...
# posR <- subset(zsimAcc$layout, grepl("strip-r", name), select = t:r)
# posT <- subset(zsimAcc$layout, grepl("strip-t", name), select = t:r)
# 
# # Add a new column to the right of current right strips, 
# # and a new row on top of current top strips
# width <- zsimAcc$widths[max(posR$r)]    # width of current right strips
# height <- zsimAcc$heights[min(posT$t)]  # height of current top strips
# 
# zsimAcc <- gtable_add_cols(zsimAcc, width, max(posR$r))  
# zsimAcc <- gtable_add_rows(zsimAcc, height, min(posT$t)-1)
# 
# # Construct the new strip grobs
# stripR <- gTree(name = "Strip_right", children = gList(
#   rectGrob(gp = gpar(col = NA, fill = "grey85")),
#   textGrob(labelR, rot = -90, gp = gpar(fontsize = 8.8, col = "grey10"))))
# 
# stripT <- gTree(name = "Strip_top", children = gList(
#   rectGrob(gp = gpar(col = NA, fill = "grey85")),
#   textGrob(labelT, gp = gpar(fontsize = 8.8, col = "grey10"))))
# 
# # Position the grobs in the gtable
# zsimAcc <- gtable_add_grob(zsimAcc, stripR, t = min(posR$t)+1, l = max(posR$r) + 1, b = max(posR$b)+1, name = "strip-right")
# zsimAcc <- gtable_add_grob(zsimAcc, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")
# 
# # Add small gaps between strips
# zsimAcc <- gtable_add_cols(zsimAcc, unit(1/5, "line"), max(posR$r))
# zsimAcc <- gtable_add_rows(zsimAcc, unit(1/5, "line"), min(posT$t))

# Draw it
#grid.newpage()
#grid.draw(zsimAcc)



#cowplot::plot_grid(zsimAcc)






##### RT

simRT = ggplot(mBsim$data, aes(x = nTrialScaled, y = ResponseTime/ClocksInSet, color = Jymmin))+
  
  #geom_ribbon(aes(ymin = Q2.5.ResponseTime/ClocksInSet, ymax = Q97.5.ResponseTime_capped/ClocksInSet), alpha = 0.25, color = NA)+
  #geom_ribbon(aes(ymin = Q25.ResponseTime/ClocksInSet, ymax = Q75.ResponseTime/ClocksInSet), alpha = 0.4, color = NA)+
  geom_point(alpha = 0.3, size = 0.01)+
  #geom_line(aes(y = Estimate.ResponseTime/ClocksInSet), color = 'black')+
  facet_grid(Level~SubjectCode)+
  ylim(0,85)+
  #ggtitle('Simulated data')+
  xlab(TeX('$T$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  ylab('(response time in seconds)/(set size)')+
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



zsimRT = add_label_strips (simRT + theme(legend.position="none"))
# 
# zsimRT<- ggplotGrob(simRT +  theme(legend.position="none"))
# 
# # Get the positions of the strips in the gtable: t = top, l = left, ...
# posR <- subset(zsimRT$layout, grepl("strip-r", name), select = t:r)
# posT <- subset(zsimRT$layout, grepl("strip-t", name), select = t:r)
# 
# # Add a new column to the right of current right strips, 
# # and a new row on top of current top strips
# width <- zsimRT$widths[max(posR$r)]    # width of current right strips
# height <- zsimRT$heights[min(posT$t)]  # height of current top strips
# 
# zsimRT <- gtable_add_cols(zsimRT, width, max(posR$r))  
# zsimRT <- gtable_add_rows(zsimRT, height, min(posT$t)-1)
# 
# # Construct the new strip grobs
# stripR <- gTree(name = "Strip_right", children = gList(
#   rectGrob(gp = gpar(col = NA, fill = "grey85")),
#   textGrob(labelR, rot = -90, gp = gpar(fontsize = 8.8, col = "grey10"))))
# 
# stripT <- gTree(name = "Strip_top", children = gList(
#   rectGrob(gp = gpar(col = NA, fill = "grey85")),
#   textGrob(labelT, gp = gpar(fontsize = 8.8, col = "grey10"))))
# 
# # Position the grobs in the gtable
# zsimRT <- gtable_add_grob(zsimRT, stripR, t = min(posR$t)+1, l = max(posR$r) + 1, b = max(posR$b)+1, name = "strip-right")
# zsimRT <- gtable_add_grob(zsimRT, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")
# 
# # Add small gaps between strips
# zsimRT <- gtable_add_cols(zsimRT, unit(1/5, "line"), max(posR$r))
# zsimRT <- gtable_add_rows(zsimRT, unit(1/5, "line"), min(posT$t))
# 
# 
# 
# # legend_bsimdat = cowplot::get_legend(
# #   simRT + 
# #     guides(color = guide_legend(nrow = 1, override.aes = list(size = 1, alpha = 1))) +
# #     #guide = guide_legend(override.aes = list(size = 1, alpha = 1) )+
# #     theme(legend.position = "bottom")
# # )
# 


#grid.newpage()
#grid.draw(zsimRT)



cowplot::plot_grid(zsimRT, #+  theme(legend.position="none"),
                   zsimAcc, #+  theme(legend.position="none"),
                   legend_Jym_points_bottom,
                   rel_heights = c(1, 1, 0.1),
                   labels=c("A", "B" ,''),
                   #align = 'hv',
                   #legend,
                   label_fontfamily = "serif",
                   ncol = 1,
                   nrow = 3)


########## figure 14 #########-------------------------------
#pp_checks simulation models




#####model B

bayesplot::color_scheme_set('blue')
cs = bayesplot::color_scheme_get()
cs = c(cs[[1]],cs[[2]],cs[[3]],cs[[4]],cs[[5]],cs[[6]])

RTB = pp_check(mBsim, resp = 'ResponseTime', nsamples = NULL)+
  xlim(0,1000)+
  xlab('response time in seconds')+
  #scale_color_discrete(name = '', labels = c(y = "data", yrep ="model predictions"))+
  scale_color_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+

  #scale_color_identity(name = '', labels = c("data","model predictions"))+
  #legend_relabel(c("data", "model prediction"))+
  bayesplot::legend_move(c(0.75, 0.5))+
  ylab('density')+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        axis.text.y = element_text(),
        axis.ticks.y  = element_line())+
  scale_y_continuous(breaks=seq(0,0.005, 0.002))+
  ylim(0,0.0045)
  




#cs = c(cs[[1]],cs[[2]],cs[[3]],cs[[4]],cs[[5]],cs[[6]])
cs_new = cs
cs_new[6] = cs[2]
cs_new[1] = cs[5]
cs_new[2] = cs[6]
bayesplot::color_scheme_set(cs_new)

AccB = pp_check(mBsim, resp = 'nCorrect', type = 'bars', nsamples = NULL, prob = 1) +
  xlab('number of correct responses')+
  ylab('count')+
  scale_fill_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+
  scale_color_manual(name = '', values = c(cs[2], cs[5]), labels = c(y = "data", yrep ="model predictions"))+
  bayesplot::legend_move(c(0.36, 0.8))



bayesplot::color_scheme_set(cs)




######model A

bayesplot::color_scheme_set('teal')
cs = bayesplot::color_scheme_get()
cs = c(cs[[1]],cs[[2]],cs[[3]],cs[[4]],cs[[5]],cs[[6]])

RTA = pp_check(mAsim, resp = 'ResponseTime', nsamples = NULL)+
  xlim(0,1000)+
  xlab('response time in seconds')+
  #scale_color_discrete(name = '', labels = c(y = "data", yrep ="model predictions"))+
  scale_color_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+

  #scale_color_identity(name = '', labels = c("data","model predictions"))+
  #legend_relabel(c("data", "model prediction"))+
  bayesplot::legend_move(c(0.75, 0.5))+
  ylab('density')+
  theme(plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
        axis.text.y = element_text(),
        axis.ticks.y  = element_line())+
  scale_y_continuous(breaks=seq(0,0.005, 0.002))+
  ylim(0,0.0045)



#cs = c(cs[[1]],cs[[2]],cs[[3]],cs[[4]],cs[[5]],cs[[6]])
cs_new = cs
cs_new[6] = cs[2]
cs_new[1] = cs[5]
cs_new[2] = cs[6]
bayesplot::color_scheme_set(cs_new)
AccA  = pp_check(mAsim, resp = 'nCorrect', type = 'bars', nsamples = NULL, prob = 1) +
  xlab('number of correct responses')+
  ylab('count')+
  scale_fill_manual(name = '', values = c(cs[5], cs[2]), labels = c(y = "data", yrep ="model predictions"))+
  scale_color_manual(name = '', values = c(cs[2], cs[5]), labels = c(y = "data", yrep ="model predictions"))+
  bayesplot::legend_move(c(0.36, 0.8))




bayesplot::color_scheme_set(cs)



#grid.arrange(RTA, RTB, AccA,AccB,nrow = 2, labels=c('A', 'B', 'C', 'D'))


plot_grid(RTA,RTB,AccA,AccB,
          labels=c("A", "B", 'C','D' ),
          ncol = 2, 
          nrow = 2,
          align = 'hv',
          label_fontfamily = "serif")

#plot_grid(RTA,RTB, labels=c("A", "B" ), ncol = 2, nrow = 1)


########## figure 15 #########-------------------------------
#Posterior parameter distribution Jym params Model B_sim



trial_scale_factor = 1/mBsim$data$nTrialScaled[1]

bayesplot::color_scheme_set('blue')
cs = bayesplot::color_scheme_get()

posterior = cs[[4]]
prior = cs[[2]]
red = 'red'
colorsSim = c('prior' = prior, 'posterior' = posterior, 'true' = red)

ps = posterior_samples(mBsim)
names(ps) <- sub(':', '', names(ps))

prs = prior_samples(mBsim)
names(prs) <- sub(':', '', names(prs))


# first subplot
alphaRTsim = ggplot(posterior_samples(mBsim), aes(x = b_ResponseTime_JymminYes))+
  geom_density(aes(fill = 'posterior'), alpha = 0.8)+
  geom_vline(aes(xintercept = -0.2, color = 'true'))+
  geom_density(data = prior_samples(mBsim), aes(fill = 'prior'), alpha = 0.5)+
  scale_fill_manual(name = '', values = colorsSim ,labels = c('posterior', 'prior'))+
  scale_color_manual(name = '', values = colorsSim,labels = c('true value'))+
  xlim(-1.5,1.5)+
  xlab(TeX('$\\alpha_{RT,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))

#get the legend
legend_bSim = cowplot::get_legend(
  alphaRTsim + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)


# next subplot
alphaAccsim = ggplot(posterior_samples(mBsim), aes(x = b_nCorrect_JymminYes))+
  geom_density(fill = posterior, alpha = 0.8)+
  geom_vline(aes(xintercept = 0.5), color = 'red')+
  geom_density(data = prior_samples(mBsim), fill = prior, alpha = 0.5)+
  xlim(-2,2)+
  xlab(TeX('$\\alpha_{Acc,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))


# next subplot
betaAccsim = ggplot(ps, aes(x = b_nCorrect_JymminYesnTrialScaled))+
  geom_density(fill = posterior, alpha = 0.8)+
  geom_vline(aes(xintercept = 0.0001*trial_scale_factor), color = 'red')+
  geom_density(data = prs, fill = prior, alpha = 0.5)+
  xlim(-2,2)+
  xlab(TeX('$\\beta_{Acc,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))


# next subplot
betaRTsim = ggplot(ps, aes(x = b_ResponseTime_JymminYesnTrialScaled))+
  geom_density(fill = posterior, alpha = 0.8)+
  geom_vline(aes(xintercept = -0.0001*trial_scale_factor), color = 'red')+
  geom_density(data = prs, fill = prior, alpha = 0.5)+
  xlim(-1.5,1.5)+
  xlab(TeX('$\\beta_{RT,Jymmin}$',italic=TRUE))+
  theme(text = element_text(family = "serif"))+
  theme(panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "grey96"
                                        ))

#put plots together
pSim = cowplot::plot_grid(alphaRTsim + theme(legend.position="none"),
                       alphaAccsim,
                       betaRTsim,
                       betaAccsim,
                       labels=c("A", "B", 'C','D' ),
                       ncol = 2,
                       nrow = 2,
                       align = 'hv',
                       label_fontfamily = "serif")

#add the legend
cowplot::plot_grid(pSim ,legend_bSim, ncol = 1,rel_heights = c(1, .1))



 
