#define the priors for the models



priors_duration = #prior(student_t(3,2,8), class = Intercept, resp = 'DurationInSeconds'),
   c(prior(student_t(3,2,2.5), class = Intercept, resp = 'DurationInSeconds'), ## new!
     
     
     #prior(student_t(3,0,2), class = sd, group = 'Level', resp = 'DurationInSeconds'),
     prior(student_t(3,0,1.5), class = sd, group = 'Level', resp = 'DurationInSeconds'), # new!!
     prior(student_t(3,0,1.5), class = sd, group = 'SubjectCode', resp = 'DurationInSeconds'),
     prior(student_t(3,0,5), class = sigma, resp = 'DurationInSeconds'),
     
     ##!!prior(normal(1,2), class = shift, resp = 'DurationInSeconds'),
     #prior(student_t(3,2,3), class = shift, resp = 'DurationInSeconds'),
     prior(gamma(2,1), class = shift, resp = 'DurationInSeconds'),
     
     prior(student_t(3, -0.5, 1.5), class = b, coef = 'nTrialLevelScale', resp = 'DurationInSeconds'),
     prior(student_t(3,0,0.5), class = sd, coef = 'nTrialLevelScale', group = 'SubjectCode', resp = 'DurationInSeconds'),
     prior(student_t(3,0,0.5), class = sd, coef = 'nTrialLevelScale', group = 'Level', resp = 'DurationInSeconds')
)


priors_accuracy = c(prior(student_t(3,0,2.5), class = Intercept, resp = 'nCorrect'),
                    prior(student_t(3,0,2), class = sd, group = 'Level', resp = 'nCorrect'),
                    prior(student_t(3,0,2), class = sd,group = 'SubjectCode', resp = 'nCorrect'),
                    prior(gamma(3,0.1), class = phi, resp = 'nCorrect'),
                    
                    #prior(student_t(3, 0.5, 8), class = b, coef = 'nTrialLevelScale', resp = 'nCorrect'),
                    prior(student_t(3, 0.5, 2), class = b, coef = 'nTrialLevelScale', resp = 'nCorrect'), ## new!!!!!
                    
                    prior(student_t(3,0,1.5), class = sd, coef = 'nTrialLevelScale', group = 'SubjectCode', resp = 'nCorrect'),
                    prior(student_t(3,0,1.5), class = sd, coef = 'nTrialLevelScale', group = 'Level', resp = 'nCorrect')#,)
)

  
priors_jymmin = c( prior(student_t(3,0,1.5), class = b, coef = 'JymminYes', resp = 'DurationInSeconds'),
                   prior(student_t(3,0,1), class = b, coef = 'JymminYes:nTrialLevelScale', resp = 'DurationInSeconds'),
                   
                   prior(student_t(3,0,2), class = b, coef = 'JymminYes', resp = 'nCorrect'),
                   prior(student_t(3,0,2), class = b, coef = 'JymminYes:nTrialLevelScale', resp = 'nCorrect')
)



  
  
  
priorModelA = c(
    
    #prior(student_t(3,2,8), class = Intercept, resp = 'DurationInSeconds'),
    prior(student_t(3,2,3), class = Intercept, resp = 'DurationInSeconds'), ## new!
    
    
    #prior(student_t(3,0,2), class = sd, group = 'Level', resp = 'DurationInSeconds'),
    prior(student_t(3,0,3), class = sd, group = 'Level', resp = 'DurationInSeconds'), # new!!
    
    
    
    
    prior(student_t(3,0,3), class = sd, group = 'SubjectCode', resp = 'DurationInSeconds'),
    prior(student_t(3,0,5), class = sigma, resp = 'DurationInSeconds'),
    prior(normal(1,2), class = shift, resp = 'DurationInSeconds'),
    prior(student_t(3, 0.5, 2), class = b, coef = 'nTrialLevelScale', resp = 'DurationInSeconds'),
    prior(student_t(3,0,0.5), class = sd, coef = 'nTrialLevelScale', group = 'SubjectCode', resp = 'DurationInSeconds'),
    prior(student_t(3,0,0.5), class = sd, coef = 'nTrialLevelScale', group = 'Level', resp = 'DurationInSeconds'),
    
    prior(student_t(3,0,2.5), class = Intercept, resp = 'nCorrect'),
    prior(student_t(3,0,2), class = sd, group = 'Level', resp = 'nCorrect'),
    prior(student_t(3,0,2), class = sd,group = 'SubjectCode', resp = 'nCorrect'),
    prior(gamma(3,0.1), class = phi, resp = 'nCorrect'),
    
    #prior(student_t(3, 0.5, 8), class = b, coef = 'nTrialLevelScale', resp = 'nCorrect'),
    prior(student_t(3, 0.5, 3), class = b, coef = 'nTrialLevelScale', resp = 'nCorrect'), ## new!!!!!
    
    prior(student_t(3,0,4), class = sd, coef = 'nTrialLevelScale', group = 'SubjectCode', resp = 'nCorrect'),
    prior(student_t(3,0,4), class = sd, coef = 'nTrialLevelScale', group = 'Level', resp = 'nCorrect')#,
    
    #prior(student_t(3,0,4), class = b, coef = 'JymminYes', resp = 'DurationInSeconds'),
    #prior(student_t(3,0,4), class = b, coef = 'JymminYes:nTrialLevelScale', resp = 'DurationInSeconds'),
    
    #prior(student_t(3,0,8), class = b, coef = 'JymminYes', resp = 'nCorrect'),
    #prior(student_t(3,0,2), class = b, coef = 'JymminYes', resp = 'nCorrect'),###new!
    
    #prior(student_t(3,0,4), class = b, coef = 'JymminYes:nTrialLevelScale', resp = 'nCorrect')
  )


