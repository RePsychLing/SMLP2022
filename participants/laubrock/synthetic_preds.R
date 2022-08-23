## Jochen Laubrock (2021/22)
##
## - Motivation: use predictability for diagnosis of MCI / early stages of Alzheimer's disease
## - Problem: Cloze predictabilities are costly to obtain
## - Proposed solution: use language models from computational linguistics to extract synthetic predictabilities
## - Here we show that synthetic predictabilities explain eye movements, by re-analyzing eye movement recordings by Chandra et al. (2020)
## - In additional analyses (not reported here), we show that synthetic predictabilities are correlated with cloze predictabilities, and that synthetic predictabilities explain eye movements across several languages and corpora

## Chandra, J., Krügel, A., & Engbert, R. (2020). Modulation of oculomotor control during reading of mirrored and inverted texts. Scientific Reports, 10 (1), 1-15. doi: 10.1038/s41598-020-60833-6
## https://osf.io/bmvrx/

load("data2.rda")

# Full model (e.g. based on Kuperman, V., Dambacher, M., Nuthmann, A., & Kliegl. R. (2010). The effect of word position on eye-movements in sentence and paragraph reading.The Quarterly Journal of Experimental Psychology, 63:9, 1838-1857) )
#  data_clean
d_clean<- data2[valid_ix ==TRUE,]

# M_base = model for Word N
M_base<- lmer(log(sfd) ~ poly(relwn,3) + logfreq + w_length +
                  saclenchar_in + charInWord + charInWord^2 +
                  (1 | vp) + (1 | satznr) + (1 | w_order),
                data= d_clean)
summary(M_base)

# M_min1 = M_base + Word N-1
M_min1<- lmer(log(sfd) ~ poly(relwn,3) + logfreq + w_length +
                  logfreq1 + w_length1 +
                  saclenchar_in + charInWord + charInWord^2 +
                  (1 | vp) + (1 | satznr) + (1 | w_order),
                data= d_clean)

summary(M_min1)

# M_plus1 = M_base + Word N+1
M_plus1<- lmer(log(sfd) ~ poly(relwn,3) + logfreq + w_length +
                logfreq2 + w_length2 +
                saclenchar_in + charInWord + charInWord^2 +
                (1 | vp) + (1 | satznr) + (1 | w_order),
              data= d_clean)
summary(M_plus1)

# M_nopred =  M_base + Word N-1 + Word N+1 (model without predictions as predictors)
M_nopred<- lmer(log(sfd) ~ poly(relwn,3) + logfreq + w_length +
                  logfreq1 + w_length1 +
                  logfreq2 + w_length2 +
                  saclenchar_in + charInWord + charInWord^2 +
                  (1 | vp) + (1 | satznr) + (1 | w_order),
                data= d_clean)

summary(M_nopred)
### compare models (M_base, M_min1, M_nopred)
test_nopred<- anova(M_base, M_min1, M_plus1, M_nopred)
test_nopred


## add synthetic predictabilities
# Mfull = M_nopred + predictions
Mfull <- lmer(log(sfd) ~ poly(relwn,3) + logfreq + w_length + lgtpred +
                        logfreq1 + w_length1 + lgtpred1 +
                        logfreq2 + w_length2 + lgtpred2 +
                        saclenchar_in + charInWord + charInWord^2 +
                        (1 | vp) + (1 | satznr) + (1 | w_order),
             data= d_clean)

summary(Mfull)
print(Mfull, corr=F)

# compare M_nopred and M_full
anova(M_nopred, Mfull)

## Synthetic predictabilities are the strongest predictors, explaining more deviance than frequencies, word length, or even incoming saccade length
## Interestingly, synthetic predictabilities of past, present, and in particular of the future word are all negatively correlated with single fixation durations on the current word
