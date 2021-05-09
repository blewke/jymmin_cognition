#### simulate data 
# 2 variables dependent:
# RT: shifted lognormal, then summed
# Acc: binomial distribution

#predictor variables
# Jymmin
# level
# participant
# nTrial

set.seed(123)

require(tibble)

require(brms)
#require(gtools)


####### defining paramters ######

#how many subjects and levels?
nSubjects = 23
nLevels = 4


# define all model paramters
RT_BetweenSubjectMean = 2
RT_BetweenSubjectVariation = 0.25
RT_BetweenLevelMean = 0
RT_BetweenLevelVariation = 0.25

trial_scale_factor = 4000


Acc_BetweenSubjectMean = 0.0
Acc_BetweenSubjectVariation = 0.8
Acc_BetweenLevelMean = 0.9
Acc_BetweenLevelVariation = 0.5




RT_BetweenSubjectSlopeMean = -0.000025
RT_BetweenSubjectSlopeVariation = 0.0001
RT_BetweenLevelSlopeMean = -0.00015
RT_BetweenLevelSlopeVariation = 0.0001


Acc_BetweenSubjectSlopeMean = 0.0001
Acc_BetweenSubjectSlopeVariation = 0.00003
Acc_BetweenLevelSlopeMean = 0.0000
Acc_BetweenLevelSlopeVariation = 0.00007



RT_sd = 1.3
RT_shift = 2

theta = 30 # also called phi, the precsion parameter of the beta binomial


#Jymmin paramters

Acc_JymminIntercept = 0.5
Acc_JymminSlopeAdd = 0.0001


RT_JymminIntercept = -0.2
RT_JymminSlopeAdd = -0.0001



## some variables governing how many trials are generated per level
# accuracy at which trials can be stopped (sometimes)
stopRatio = 0.9 
#if the accuracy is above the stop ratio, how many more sets are done (lower,upper)
setsAfterStopRatio = c(1, 100)
# maximim number of trials per set
max_TrialsPerSet = 2.5*trial_scale_factor

# how many trials per set?
SetMin = 1
SetMax = 25

#how many of the sets have max set as number?
SetMaxRatio = 0.95


# a function transforming paramters between the different paramtrisations for the beta distribution
#from https://bookdown.org/content/3890/monsters-and-mixtures.html#beta-binomial.
betaABfromMeanKappa <- function(mean, kappa) {
  if (mean <= 0 | mean >= 1) stop("must have 0 < mean < 1")
  if (kappa <= 0) stop("kappa must be > 0")
  a <- mean * kappa
  b <- (1.0 - mean) * kappa
  return(list(a = a, b = b))
}

###### simulating ########

## first, generate individual intercepts and learning rates for subjects and levels

RT_SubjectIntercepts = rnorm(nSubjects, RT_BetweenSubjectMean, RT_BetweenSubjectVariation)
RT_LevelIntercepts = rnorm(nLevels, RT_BetweenLevelMean, RT_BetweenLevelVariation)

Acc_SubjectIntercepts = rnorm(nSubjects, Acc_BetweenSubjectMean, Acc_BetweenSubjectVariation)
Acc_LevelIntercepts = rnorm(nLevels, Acc_BetweenLevelMean, Acc_BetweenLevelVariation)

Acc_SubjectSlopes = rnorm(nSubjects, Acc_BetweenSubjectSlopeMean, Acc_BetweenSubjectSlopeVariation)
Acc_LevelSlopes = rnorm(nLevels, Acc_BetweenLevelSlopeMean, Acc_BetweenLevelSlopeVariation)

RT_SubjectSlopes = rnorm(nSubjects, RT_BetweenSubjectSlopeMean, RT_BetweenSubjectSlopeVariation)
RT_LevelSlopes = rnorm(nLevels, RT_BetweenLevelSlopeMean, RT_BetweenLevelSlopeVariation)

### now simulate performance 

sim_dat = tibble(SubjectCode = NA, Level = NA, nTrialLevel = NA, ClocksInSet = NA,  Accuracy = NA, Jymmin = NA, RT = NA)
r = 1


#generate trials for every combination of subject+level
for(s in 1:nSubjects){
    for (l in 1:nLevels){
      
      # is this combination of level and participant in the Jymmin condtion or not 
      # (idealizing assumption, in real data set sometimes mixed)
      Jymmin = sample(c(0,1),1)

      
      nTrials = 1
      
      set = 1
      acc = 0
      setsToGo = Inf
      
      ##get intercepts and slope for this subjects+level combination
      
      Acc_Intercept = Acc_SubjectIntercepts[s] + Acc_LevelIntercepts[l]
      Acc_Slope = Acc_SubjectSlopes[s] + Acc_LevelSlopes[l]
      
      RT_Intercept = RT_SubjectIntercepts[s] + RT_LevelIntercepts[l]
      RT_Slope = RT_SubjectSlopes[s] + RT_LevelSlopes[l]
      
      if(Jymmin){
        RT_Intercept = RT_Intercept + RT_JymminIntercept
        RT_Slope =  RT_Slope + RT_JymminSlopeAdd
        Acc_Intercept = Acc_Intercept + Acc_JymminIntercept
        Acc_Slope =  Acc_Slope + Acc_JymminSlopeAdd
      }
      
      # generate more trials until some condtions are met
      go = TRUE
 
      while(go){
        
        sim_dat = rbind(sim_dat,NA)
        sim_dat$SubjectCode[r] = s
        sim_dat$Level[r] = l
        sim_dat$nTrialLevel[r] = nTrials
        sim_dat$Jymmin[r] = Jymmin
        
        # is this a 'full' set of 25?
        set = sample( c(25,0), 1, prob = c(SetMaxRatio, 1 - SetMaxRatio))
        
        # if not a 'full' set, draw a lower number
        if(!set){
          set = sample (1:24, 1)
        }
        

        
        sim_dat$ClocksInSet[r] = set
        
        
        ##simulate the number of correct trials for this set
        
        Acc_pmean = inv_logit_scaled(Acc_Intercept + nTrials * Acc_Slope)
        
        ab = betaABfromMeanKappa(Acc_pmean, theta)
        
        p = rbeta(1,ab$a,ab$b)
        
        acc = rbinom(1,set,p)
        sim_dat$Accuracy[r] = acc
        
        
        ## simulate summed response time for this set
        
        RT_mean = RT_Intercept + nTrials * RT_Slope
        
        RT_sum = sum(RT_shift + exp(rnorm(set, RT_mean, RT_sd)))
        
        sim_dat$RT[r] = RT_sum
        
        
        ## can we stop generating trials?
        nTrials = nTrials + set
        r = r+1
        setsToGo = setsToGo - 1
        
        if(nTrials >= max_TrialsPerSet){
          go = FALSE
        }

        
        if (setsToGo == 0 && acc/set >= stopRatio){
          go = FALSE
        } else if (setsToGo == 0 | (acc/set >= stopRatio & setsToGo == Inf)){
          setsToGo = round(runif(1,setsAfterStopRatio[1], setsAfterStopRatio[2]))
        }
        
      }
    }
}

#delete the row with only NAs
sim_dat = sim_dat[!is.na(sim_dat$SubjectCode),]

## in the real data set, not every subject+level combination was present.
## now select which ones make the data set


# from how many subjects do we have data on each level? (as in real data set) 
nfrom_level = c(20,12,5,3)

# which paricipant is selected in each level?
from_level1 = sample(1:23, nfrom_level[1])
from_level2 = sample(1:23, nfrom_level[2])
from_level3 = append(sample(from_level1, nfrom_level[3]), c(1:23)[!1:23 %in% from_level1]) # select the 3 participants that had not been selected in level 1
from_level4 = sample(1:23, nfrom_level[4])


# get the data fro the selected subject+level combos
sim_dat_select = sim_dat[sim_dat$Level == 1 & sim_dat$SubjectCode %in% from_level1
                         |sim_dat$Level == 2 & sim_dat$SubjectCode %in% from_level2
                         |sim_dat$Level == 3 & sim_dat$SubjectCode %in% from_level3
                         |sim_dat$Level == 4 & sim_dat$SubjectCode %in% from_level4
                         
                         ,]

#how many data points? real data has 7233
print(nrow(sim_dat_select))

#save the simulated data
write.csv(sim_dat, '../jym_data/sim_dat.csv')
write.csv(sim_dat_select, '../jym_data/sim_dat_select.csv')






