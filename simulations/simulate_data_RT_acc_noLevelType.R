#### simulate data 
# 2 variables dependent:
# RT: shifted lognormal
# Acc: bernoulli

#predictor variables
# treatment (yes/no)
# level

set.seed(567)

require(tibble)

require(brms)
require(gtools)

nSubjects = 23
nLevels = 4

RT_BetweenSubjectMean = 2
RT_BetweenSubjectVariation = 0.25
#EndpointRange = c(0.9,1)
RT_BetweenLevelMean = 0
RT_BetweenLevelVariation = 0.25
#WithinLevelTypeVariation = 0.2

trial_scale_factor = 4000


Acc_BetweenSubjectMean = 0.0
Acc_BetweenSubjectVariation = 0.8
#EndpointRange = c(0.9,1)
Acc_BetweenLevelMean = 0.7
Acc_BetweenLevelVariation = 0.5
#WithinLevelTypeVariation = 0.2




RT_BetweenSubjectSlopeMean = -0.000025
RT_BetweenSubjectSlopeVariation = 0.0001
RT_BetweenLevelSlopeMean = -0.00015
RT_BetweenLevelSlopeVariation = 0.0001
#WithinLevelTypeSlopeVariation = 2


Acc_BetweenSubjectSlopeMean = 0.00005
Acc_BetweenSubjectSlopeVariation = 0.000025
Acc_BetweenLevelSlopeMean = 0.0001
Acc_BetweenLevelSlopeVariation = 0.0001
#WithinLevelTypeSlopeVariation = 2



RT_sd = 1.3
RT_shift = 2




Acc_JymminIntercept = 1
#JymminSlopeFactor = 1
Acc_JymminSlopeAdd = 0.0002


RT_JymminIntercept = -0.2
#JymminSlopeFactor = 1
RT_JymminSlopeAdd = -0.0001


theta = 30

stopRatio = 0.9
setsAfterStopRatio = c(1, 100)
max_TrialsPerSet = 2.5*trial_scale_factor


#nTrialsMin = 50
#nTrialsMax = 6000

SetMin = 1
SetMax = 25

#how many of the sets have max set as number?
SetMaxRatio = 0.95

#from https://bookdown.org/content/3890/monsters-and-mixtures.html#beta-binomial.
betaABfromMeanKappa <- function(mean, kappa) {
  if (mean <= 0 | mean >= 1) stop("must have 0 < mean < 1")
  if (kappa <= 0) stop("kappa must be > 0")
  a <- mean * kappa
  b <- (1.0 - mean) * kappa
  return(list(a = a, b = b))
}



RT_SubjectIntercepts = rnorm(nSubjects, RT_BetweenSubjectMean, RT_BetweenSubjectVariation)
RT_LevelIntercepts = rnorm(nLevels, RT_BetweenLevelMean, RT_BetweenLevelVariation)

Acc_SubjectIntercepts = rnorm(nSubjects, Acc_BetweenSubjectMean, Acc_BetweenSubjectVariation)
Acc_LevelIntercepts = rnorm(nLevels, Acc_BetweenLevelMean, Acc_BetweenLevelVariation)


#LevelIntercepts = array(NA, dim=c(nLevelTypes,nLevelsPerType))
#for (i in 1:nLevelTypes) {
#  LevelIntercepts[i,] = rnorm(nLevelsPerType, LevelTypeIntercepts[i], WithinLevelTypeVariation)
#}

Acc_SubjectSlopes = rnorm(nSubjects, Acc_BetweenSubjectSlopeMean, Acc_BetweenSubjectSlopeVariation)
#SubjectSlopes = rgamma(nSubjects, scale = BetweenSubjectSlopeMean/BetweenSubjectSlopeVariation, shape = BetweenSubjectSlopeVariation)
Acc_LevelSlopes = rnorm(nLevels, Acc_BetweenLevelSlopeMean, Acc_BetweenLevelSlopeVariation)
#LevelSlopes = rgamma(nLevels, scale = BetweenLevelSlopeMean/BetweenLevelSlopeVariation, shape = BetweenLevelSlopeVariation)


RT_SubjectSlopes = rnorm(nSubjects, RT_BetweenSubjectSlopeMean, RT_BetweenSubjectSlopeVariation)
#SubjectSlopes = rgamma(nSubjects, scale = BetweenSubjectSlopeMean/BetweenSubjectSlopeVariation, shape = BetweenSubjectSlopeVariation)
RT_LevelSlopes = rnorm(nLevels, RT_BetweenLevelSlopeMean, RT_BetweenLevelSlopeVariation)
#LevelSlopes = rgamma(nLevels, scale = BetweenLevelSlopeMean/BetweenLevelSlopeVariation, shape = BetweenLevelSlopeVariation)


#LevelSlopes = array(NA, dim=c(nLevelTypes,nLevelsPerType))
#for (i in 1:nLevelTypes) {
  #LevelSlopes[i,] = exp(rnorm(nLevelsPerType, LevelTypeSlopes[i], WithinLevelTypeSlopeVariation))
#  LevelSlopes[i,] = rgamma(nLevelsPerType, scale = LevelTypeSlopes[i]/WithinLevelTypeSlopeVariation, shape = WithinLevelTypeSlopeVariation)
#}


#TrialsPerLevel = array(round(runif(nSubjects*nLevelTypes*nLevelsPerType,nTrialsMin,nTrialsMax)), dim = c(nSubjects,nLevelTypes,nLevelsPerType))


sim_dat = tibble(SubjectCode = NA, Level = NA, nTrialLevel = NA, ClocksInSet = NA,  Accuracy = NA, Jymmin = NA, RT = NA)
r = 1

for(s in 1:nSubjects){
  #cat('subject ', s)
#for (lt in 1:nLevelTypes){
    for (l in 1:nLevels){
      #trials = TrialsPerLevel[s,lt,l]
      
      Jymmin = sample(c(0,1),1)
      
      #print(Jymmin)
      
      nTrials = 1
      
      set = 1
      acc = 0
      setsToGo = Inf
      
      
      Acc_Intercept = Acc_SubjectIntercepts[s] + Acc_LevelIntercepts[l]
      Acc_Slope = Acc_SubjectSlopes[s] + Acc_LevelSlopes[l]
      
      #print(Acc_Slope)
      
      RT_Intercept = RT_SubjectIntercepts[s] + RT_LevelIntercepts[l]
      RT_Slope = RT_SubjectSlopes[s] + RT_LevelSlopes[l]
      
      if(Jymmin){
        RT_Intercept = RT_Intercept + RT_JymminIntercept
        RT_Slope =  RT_Slope + RT_JymminSlopeAdd
        Acc_Intercept = Acc_Intercept + Acc_JymminIntercept
        Acc_Slope =  Acc_Slope + RT_JymminSlopeAdd
      }
      
      
      #print(intercept)
      #print(slope)
      
      go = TRUE
      
      #while(trials > 0){
      #while(acc/set < stopRatio){
      while(go){
        
        sim_dat = rbind(sim_dat,NA)
        sim_dat$SubjectCode[r] = s
        #sim_dat$LevelType[r] = lt
        sim_dat$Level[r] = l
        sim_dat$nTrialLevel[r] = nTrials
        sim_dat$Jymmin[r] = Jymmin
        
        #endpoint = runif(1,EndpointRange)
        
        #set = round(runif(1,SetMin,SetMax))
        
        set = sample( c(25,0), 1, prob = c(SetMaxRatio, 1 - SetMaxRatio))
        
        if(!set){
          set = sample (1:24, 1)
        }
        
        #set = ifelse(set < trials, set, trials)
        
        sim_dat$ClocksInSet[r] = set
        
        Acc_pmean = inv_logit_scaled(Acc_Intercept + nTrials * Acc_Slope)
        
        #pmean = inv.logit(logit(endpoint) -  nTrials * SubjectSlopeIntercepts[s])
        
        
        ab = betaABfromMeanKappa(Acc_pmean, theta)
        
        p = rbeta(1,ab$a,ab$b)
        
        acc = rbinom(1,set,p)
        sim_dat$Accuracy[r] = acc
        
        
        RT_mean = RT_Intercept + nTrials * RT_Slope
        
        RT_sum = sum(RT_shift + exp(rnorm(set, RT_mean, RT_sd)))
        
        sim_dat$RT[r] = RT_sum
        
        
        nTrials = nTrials + set
        #trials = trials - set
        r = r+1
        setsToGo = setsToGo - 1
        
        if(nTrials >= max_TrialsPerSet){
          #print('max trials reached')
          #print(s)
          #print(l)
          go = FALSE
        }
        
        
        
        if (setsToGo == 0 && acc/set >= stopRatio){
          go = FALSE
        } else if (setsToGo == 0 | (acc/set >= stopRatio & setsToGo == Inf)){
          setsToGo = round(runif(1,setsAfterStopRatio[1], setsAfterStopRatio[2]))
          #print(setsToGo)
        }
        
        
        
      }
    }
  #}
}

sim_dat = sim_dat[!is.na(sim_dat$SubjectCode),]

nfrom_level = c(20,12,5,3)


from_level1 = sample(1:23, nfrom_level[1])
from_level2 = sample(1:23, nfrom_level[2])
from_level3 = append(sample(from_level1, nfrom_level[3]), c(1:23)[!1:23 %in% from_level1])
from_level4 = sample(1:23, nfrom_level[4])



# sim_dat_select = sim_dat[sim_dat$Level == 1 & sim_dat$SubjectCode %in% sample(1:23, from_level[1])
#                         |sim_dat$Level == 2 & sim_dat$SubjectCode %in% sample(1:23, from_level[2])
#                         |sim_dat$Level == 3 & sim_dat$SubjectCode %in% sample(1:23, from_level[3])
#                         |sim_dat$Level == 4 & sim_dat$SubjectCode %in% sample(1:23, from_level[4])
#                            
#                          ,]


sim_dat_select = sim_dat[sim_dat$Level == 1 & sim_dat$SubjectCode %in% from_level1
                         |sim_dat$Level == 2 & sim_dat$SubjectCode %in% from_level2
                         |sim_dat$Level == 3 & sim_dat$SubjectCode %in% from_level3
                         |sim_dat$Level == 4 & sim_dat$SubjectCode %in% from_level4
                         
                         ,]




write.csv(sim_dat, '../jym_data/sim_dat.csv')
write.csv(sim_dat_select, '../jym_data/sim_dat_select.csv')

# # 
# print(nrow(sim_dat))
# # 
# require(ggplot2)
# #ggplot(sim_dat, aes(x = nTrialLevel, y = Accuracy/ClocksInSet, color = as.factor(Jymmin)))+
# ggplot(sim_dat_select, aes(x = nTrialLevel, y = Accuracy/ClocksInSet, color = as.factor(Jymmin)))+ 
#   geom_line(alpha = 0.5)+
#   facet_grid(Level~SubjectCode)
# 
# 
# ggplot(sim_dat, aes(x = nTrialLevel, y = RT/ClocksInSet, color = as.factor(Jymmin)))+
#   geom_line(alpha = 0.5)+
#   facet_grid(Level~SubjectCode)
# 
# #hist(sim_dat$RT)
# 
# 
# 
# 
# 
# ggplot(sim_dat, aes(x = nTrialLevel, y = RT/ClocksInSet))+
#   geom_point(size = 0.01)+
#   facet_grid(Level~SubjectCode)
# 
# ggplot(sim_dat, aes(x = nTrialLevel, y = Accuracy/ClocksInSet))+
#   geom_point(size = 0.01)+
#   facet_grid(Level~SubjectCode)
# 





