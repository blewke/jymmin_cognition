#simulate data Accuracy
require(tibble)
require(gtools)

nSubjects = 10
nLevelTypes = 2
nLevelsPerType = 4

BetweenSubjectMean = 0
BetweenSubjectVariation = 1
BetweenLevelTypeMean = -1
BetweenLevelTypeVariation = 1
WithinLevelTypeVariation = 0.2

nTrialsMin = 50
nTrialsMax = 6000

SetMin = 5
SetMax = 25


SubjectIntercepts = rnorm(nSubjects, BetweenSubjectMean, BetweenSubjectVariation)
LevelTypeIntercepts = rnorm(nLevelTypes, BetweenLevelTypeMean, BetweenLevelTypeVariation)

LevelIntercepts = array(NA, dim=c(nLevelTypes,nLevelsPerType))
for (i in 1:nLevelTypes) {
LevelIntercepts[i,] = rnorm(nLevelsPerType, LevelTypeIntercepts[i], WithinLevelTypeVariation)
}


TrialsPerLevel = array(round(runif(nSubjects*nLevelTypes*nLevelsPerType,nTrialsMin,nTrialsMax)), dim = c(nSubjects,nLevelTypes,nLevelsPerType))


sim_dat = tibble(SubjectCode = NA, LevelType = NA, Level = NA, nTrialLevel = NA, ClocksInSet = NA,  Accuracy = NA)
r = 1

for(s in 1:nSubjects){
  for (lt in 1:nLevelTypes){
    for (l in 1:nLevelsPerType){
      trials = TrialsPerLevel[s,lt,l]
      nTrials = 1
      while(trials > 0){
        sim_dat = rbind(sim_dat,NA)
        sim_dat$SubjectCode[r] = s
        sim_dat$LevelType[r] = lt
        sim_dat$Level[r] = l
        sim_dat$nTrialLevel[r] = nTrials
        
        set = round(runif(1,SetMin,SetMax))
        set = ifelse(set < trials, set, trials)
        
        sim_dat$ClocksInSet[r] = set
        
        p = inv.logit(SubjectIntercepts[s] + LevelTypeIntercepts[lt] + LevelIntercepts[lt,l])
        
        sim_dat$Accuracy[r] = rbinom(1,set,p)
        
        nTrials = nTrials + set
        trials = trials - set
        r = r+1
        
      }
    }
  }
}





