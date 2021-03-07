#define the priors for the models



priors_duration = #prior(student_t(3,2,8), class = Intercept, resp = 'DurationInSeconds'),
   c(
    #prior(student_t(3,2,2.5), class = Intercept, resp = 'DurationInSeconds'), ## new!
     #prior(student_t(3,1,1), class = Intercept, resp = 'DurationInSeconds'),
     prior(normal(1.5,1.75), class = Intercept, resp = 'DurationInSeconds'),
     
     #prior(student_t(3,0,1.5), class = sd, group = 'Level', resp = 'DurationInSeconds'), # new!!
     #prior(student_t(3,0,1.5), class = sd, group = 'SubjectCode', resp = 'DurationInSeconds'),
     prior(normal(0,0.5), class = sd, group = 'Level', resp = 'DurationInSeconds'), 
     prior(normal(0,0.5), class = sd, group = 'SubjectCode', resp = 'DurationInSeconds'),
     
     #prior(student_t(3,0,5), class = sigma, resp = 'DurationInSeconds'),
     prior(normal(0,1), class = sigma, resp = 'DurationInSeconds'),
     
     ##!!prior(normal(1,2), class = shift, resp = 'DurationInSeconds'),
     #prior(student_t(3,2,3), class = shift, resp = 'DurationInSeconds'),
     prior(gamma(2,1), class = shift, resp = 'DurationInSeconds'),
     
     #prior(student_t(3, -0.5, 1.5), class = b, coef = 'nTrialLevelScale', resp = 'DurationInSeconds'),
     prior(normal(-0.25, 0.5), class = b, coef = 'nTrialScale', resp = 'DurationInSeconds'),
     
     #(student_t(3,0,0.5), class = sd, coef = 'nTrialLevelScale', group = 'SubjectCode', resp = 'DurationInSeconds'),
     #prior(student_t(3,0,0.5), class = sd, coef = 'nTrialLevelScale', group = 'Level', resp = 'DurationInSeconds')

     prior(normal(0,0.25), class = sd, coef = 'nTrialScale', group = 'SubjectCode', resp = 'DurationInSeconds'),
     prior(normal(0,0.25), class = sd, coef = 'nTrialScale', group = 'Level', resp = 'DurationInSeconds')
     
     )


priors_accuracy = c(
                    #prior(student_t(3,0,2.5), class = Intercept, resp = 'nCorrect'),
                    prior(normal(0,2), class = Intercept, resp = 'nCorrect'),
                    
                    #prior(student_t(3,0,2), class = sd, group = 'Level', resp = 'nCorrect'),
                    prior(normal(0,1), class = sd, group = 'Level', resp = 'nCorrect'),
                    #prior(student_t(3,0,2), class = sd,group = 'SubjectCode', resp = 'nCorrect'),
                    prior(normal(0,1), class = sd, group = 'SubjectCode', resp = 'nCorrect'),
                    
                    prior(gamma(3,0.1), class = phi, resp = 'nCorrect'),
                    
                    #prior(student_t(3, 0.5, 8), class = b, coef = 'nTrialLevelScale', resp = 'nCorrect'),
                    #prior(student_t(3, 0.5, 2), class = b, coef = 'nTrialScale', resp = 'nCorrect'),
                    prior(normal(1, 1.5), class = b, coef = 'nTrialScale', resp = 'nCorrect'),
                    
                    #prior(student_t(3,0,1.5), class = sd, coef = 'nTrialScale', group = 'SubjectCode', resp = 'nCorrect'),
                    #prior(student_t(3,0,1.5), class = sd, coef = 'nTrialScale', group = 'Level', resp = 'nCorrect')
                    prior(normal(0,0.75), class = sd, coef = 'nTrialScale', group = 'SubjectCode', resp = 'nCorrect'),
                    prior(normal(0,0.75), class = sd, coef = 'nTrialScale', group = 'Level', resp = 'nCorrect')
                    
)

  
priors_jymmin = c( prior(normal(0,0.5), class = b, coef = 'JymminYes', resp = 'DurationInSeconds'),
                   prior(normal(0,0.5), class = b, coef = 'JymminYes:nTrialScale', resp = 'DurationInSeconds'),
                   
                   prior(normal(0,0.75), class = b, coef = 'JymminYes', resp = 'nCorrect'),
                   prior(normal(0,0.75), class = b, coef = 'JymminYes:nTrialScale', resp = 'nCorrect')
)



priors_period = c( prior(normal(0,0.5), class = b, coef = 'Period2', resp = 'DurationInSeconds'),
                   prior(normal(0,0.5), class = b, coef = 'Period2:nTrialScale', resp = 'DurationInSeconds'),
                   
                   prior(normal(0,0.75), class = b, coef = 'Period2', resp = 'nCorrect'),
                   prior(normal(0,0.75), class = b, coef = 'Period2:nTrialScale', resp = 'nCorrect')
)

