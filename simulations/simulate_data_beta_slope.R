#simulate data Accuracy
require(tibble)
require(gtools)

nSubjects = 10
nLevelTypes = 2
nLevelsPerType = 4

BetweenSubjectMean = -1
BetweenSubjectVariation = 2
#EndpointRange = c(0.9,1)
BetweenLevelTypeMean = 0
BetweenLevelTypeVariation = 1
WithinLevelTypeVariation = 0.2

BetweenSubjectSlopeMean = 0.001
BetweenSubjectSlopeVariation = 4
BetweenLevelTypeSlopeMean = 0.001
BetweenLevelTypeSlopeVariation = 3
WithinLevelTypeSlopeVariation = 2

theta = 30

stopRatio = 0.9
setsAfterStopRatio = c(1, 10)


#nTrialsMin = 50
#nTrialsMax = 6000

SetMin = 5
SetMax = 25


#from https://bookdown.org/content/3890/monsters-and-mixtures.html#beta-binomial.
betaABfromMeanKappa <- function(mean, kappa) {
  if (mean <= 0 | mean >= 1) stop("must have 0 < mean < 1")
  if (kappa <= 0) stop("kappa must be > 0")
  a <- mean * kappa
  b <- (1.0 - mean) * kappa
  return(list(a = a, b = b))
}



SubjectIntercepts = rnorm(nSubjects, BetweenSubjectMean, BetweenSubjectVariation)
LevelTypeIntercepts = rnorm(nLevelTypes, BetweenLevelTypeMean, BetweenLevelTypeVariation)

LevelIntercepts = array(NA, dim=c(nLevelTypes,nLevelsPerType))
for (i in 1:nLevelTypes) {
LevelIntercepts[i,] = rnorm(nLevelsPerType, LevelTypeIntercepts[i], WithinLevelTypeVariation)
}

#SubjectSlopes = exp(rnorm(nSubjects, BetweenSubjectSlopeMean, BetweenSubjectSlopeVariation))
SubjectSlopes = rgamma(nSubjects, scale = BetweenSubjectSlopeMean/BetweenSubjectSlopeVariation, shape = BetweenSubjectSlopeVariation)
#LevelTypeSlopes = exp(rnorm(nLevelTypes, BetweenLevelTypeSlopeMean, BetweenLevelTypeSlopeVariation))
LevelTypeSlopes = rgamma(nLevelTypes, scale = BetweenLevelTypeSlopeMean/BetweenLevelTypeSlopeVariation, shape = BetweenLevelTypeSlopeVariation)


LevelSlopes = array(NA, dim=c(nLevelTypes,nLevelsPerType))
for (i in 1:nLevelTypes) {
  #LevelSlopes[i,] = exp(rnorm(nLevelsPerType, LevelTypeSlopes[i], WithinLevelTypeSlopeVariation))
  LevelSlopes[i,] = rgamma(nLevelsPerType, scale = LevelTypeSlopes[i]/WithinLevelTypeSlopeVariation, shape = WithinLevelTypeSlopeVariation)
}


#TrialsPerLevel = array(round(runif(nSubjects*nLevelTypes*nLevelsPerType,nTrialsMin,nTrialsMax)), dim = c(nSubjects,nLevelTypes,nLevelsPerType))


sim_dat = tibble(SubjectCode = NA, LevelType = NA, Level = NA, nTrialLevel = NA, ClocksInSet = NA,  Accuracy = NA)
r = 1

for(s in 1:nSubjects){
  cat('subject ', s)
  for (lt in 1:nLevelTypes){
    for (l in 1:nLevelsPerType){
      #trials = TrialsPerLevel[s,lt,l]
      nTrials = 1
      
      set = 1
      acc = 0
      setsToGo = Inf
      
      
      intercept = inv.logit(SubjectIntercepts[s] + LevelTypeIntercepts[lt] + LevelIntercepts[lt,l])
      slope = (SubjectSlopes[s] + LevelTypeSlopes[lt] + LevelSlopes[lt,l] )/10
      print(intercept)
      print(slope)
      
      go = TRUE
      
      #while(trials > 0){
      #while(acc/set < stopRatio){
      while(go){
        
        sim_dat = rbind(sim_dat,NA)
        sim_dat$SubjectCode[r] = s
        sim_dat$LevelType[r] = lt
        sim_dat$Level[r] = l
        sim_dat$nTrialLevel[r] = nTrials
        
        #endpoint = runif(1,EndpointRange)
        
        set = round(runif(1,SetMin,SetMax))
        #set = ifelse(set < trials, set, trials)
        
        sim_dat$ClocksInSet[r] = set
        
        pmean = intercept + nTrials * slope
        
        #pmean = inv.logit(logit(endpoint) -  nTrials * SubjectSlopeIntercepts[s])
        
        pmean = ifelse(pmean >= 1,0.9999999999999,pmean)
        
        pmean = ifelse(pmean <= 0,0.0000000000001,pmean)
        
        ab = betaABfromMeanKappa(pmean, theta)
        
        p = rbeta(1,ab$a,ab$b)
 
        acc = rbinom(1,set,p)
        sim_dat$Accuracy[r] = acc
        
        nTrials = nTrials + set
        #trials = trials - set
        r = r+1
        setsToGo = setsToGo - 1
        
        if(nTrials >= 20000){
          cat('loads of trials')
        }
        
        
        
        if (setsToGo == 0 && acc/set >= stopRatio){
          go = FALSE
        } else if (setsToGo == 0 | (acc/set >= stopRatio & setsToGo == Inf)){
          setsToGo = round(runif(1,setsAfterStopRatio[1], setsAfterStopRatio[2]))
          print(setsToGo)
        }
        
        
        
      }
    }
  }
}

sim_dat = sim_dat[!is.na(sim_dat$SubjectCode),]

write.csv(sim_dat, 'sim_dat_beta_slope.csv')


