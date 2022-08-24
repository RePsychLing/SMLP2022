## Jochen Laubrock (2021/22)
##
## - Motivation: use predictability for diagnosis of MCI / early stages of Alzheimer's disease
## - Problem: Cloze predictabilities are costly to obtain
## - Proposed solution: use language models from computational linguistics to extract synthetic predictabilities
## - Here we show that synthetic predictabilities explain eye movements, by re-analyzing eye movement recordings by Chandra et al. (2020)
## - In additional analyses (not reported here), we show that synthetic predictabilities are correlated with cloze predictabilities, and that synthetic predictabilities explain eye movements across several languages and corpora

## Chandra, J., Krügel, A., & Engbert, R. (2020). Modulation of oculomotor control during reading of mirrored and inverted texts. Scientific Reports, 10 (1), 1-15. doi: 10.1038/s41598-020-60833-6
## https://osf.io/bmvrx/

library(arrow)
library(tidyverse)
library(easystats)
library(summarytools)

library(lme4)
library(remef)

load("./data/data2.rda")

# Preprocessing (similar to Hohenstein et al., 2017, Table 1, PsychonomBullRev)

d_clean <- 
  data2 |> 
  filter(valid_ix & firstpass==1 & saclenchar_in > 0) |>
  mutate(Subj = as.factor(vp),
         Sent = as.factor(satznr),
         OrdW = as.factor(w_order),
         lls = log2(saclenchar_in - charInWord),
         rfl = charInWord/(wordlen+1) -.5,
         rwn = relwn - .5,
         l  = 1/w_length,
         l1 = 1/w_length1,
         l2 = 1/w_length2,
         f  = logfreq,
         f1 = logfreq1,
         f2 = logfreq2,
         p  = lgtpred,
         p1 = lgtpred1,
         p2 = lgtpred2,
         lfd = log(sfd)
         ) |> 
  select(Subj:lfd)

stview(dfSummary(d_clean))         

write_feather(d_clean, "./data/Laubrock_EM.arrow")

# M_base = model for Word N
M_base <- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) + f + l + 
                    (1 |  Subj) + (1 | Sent) + (1 | OrdW),
                data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))
print(summary(M_base), corr=FALSE)

# M_min1 = M_base + Word N-1
M_min1<- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) + f + l + f1 + l1 + 
                   (1 |  Subj) + (1 | Sent) + (1 | OrdW),
              data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))
print(summary(M_min1), corr=FALSE)

# M_plus1 = M_base + Word N+1
M_plus1<- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) + f + l + f2 + l2 + 
                    (1 |  Subj) + (1 | Sent) + (1 | OrdW),
               data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))
print(summary(M_plus1), corr=FALSE)

# M_nopred =  M_base + Word N-1 + Word N+1 (model without predictions as predictors)
M_nopred<- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) + f + l + f1 + l1 + f2 + l2 + 
                     (1 |  Subj) + (1 | Sent) + (1 | OrdW),
                data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))
print(summary(M_nopred), cor=FALSE)

### compare models (M_base, M_min1, M_nopred)
anova(M_base, M_min1, M_nopred)


## add synthetic predictabilities
# Mfull = M_nopred + predictions
Mfull <- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) +
                        f + l + f1 + l1 + f2 + l2 + p + p1 + p2 +
                   (1 |  Subj) + (1 | Sent) + (1 | OrdW),
              data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))

print(summary(Mfull), cor=FALSE)

# RK: GM of VC for OrdW is very small. Do we need it? Answer: Not really, at least for now.
Mfull2 <- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) +
                f + l + f1 + l1 + f2 + l2 + p + p1 + p2 +
                (1 |  Subj) + (1 | Sent),
              data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))


# compare M_nopred and M_full
anova(M_nopred, Mfull2, Mfull)

## JL:
## Synthetic predictabilities are the strongest predictors, 
## explaining more deviance than frequencies, word length, or even incoming saccade length
## Interestingly, synthetic predictabilities of past, present, and in particular of the future
## word are all negatively correlated with single fixation durations on the current word

# RK:
# We had that before in Ong’s dissertation that computational alternatives of predictability 
# behaved more like frequency than cloze predictability.  For example, it competes with 
# n+1 fequency f2 which is no longer significant when adding n+1 predictability p2. Do you have 
# cloze predictability as well?

# RK: 
## Expanding the random-effect structure to test whether individual differences in perceptual
## span lead to a distinct profile of correlation parameters between frequency and predictability
## effects.

# varying-only-intercepts LMM
m1_ovi <- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) +
                     f + l + f1 + l1 + f2 + l2 + p + p1 + p2 +
                    (1 |  Subj) + (1 | Sent),
            data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))

# zero-correlation parameter LMM
# RK: Note "Sent"-related VC for GM was tiny and removed.
m1_zcp <- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) +
                     f + l + f1 + l1 + f2 + l2 + p + p1 + p2 +
                    (1 + f + f1 + f2 + p + p1 + p2 || Subj) + 
                    (0 + f + f1 + f2 + p + p1 + p2 || Sent),
               data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))
summary(rePCA(m1_zcp))  # sort of ok
VarCorr(m1_zcp)
anova(m1_zcpx, m1_zcp)

# complex LMM
# RK: 
# Extremely high Sent-related CPs for - (f,f1), -(f, f2), +(f1,f2) 
# Extremely high Subj-related CPs for +(f2,p1), -(f2, p2), -(p1,p2) 
m1_cpx <- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) +
                     f + l + f1 + l1 + f2 + l2 + p + p1 + p2 +
                    (1 + f + f1 + f2 + p + p1 + p2 | Subj) + 
                    (0 + f + f1 + f2 + p + p1 + p2 | Sent),
               data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))
summary(rePCA(m1_cpx)) # not ok
VarCorr(m1_cpx)
save(m1_cpx, file="./fits/m1_cpx.rda")

# parsimonious LMM: force Subj- and Sent-related f2-CPs and Subj-related p2-CPs  to zero
m1_prsm <- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) +
                      f + l + f1 + l1 + f2 + l2 + p + p1 + p2 +
                     (1 + f + f1 + p + p1      | Subj) + (0 + f2 || Subj) +
                     (1 + f + f1 + p + p1 + p2 | Sent) + (0 + f2 || Sent),
                data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))
summary(rePCA(m1_prsm)) # ok
VarCorr(m1_prsm)
save(m1_prsm, file="./fits/m1_prsm.rda")

# Could we take out VC for f2?
m2_prsm <- lmer(lfd ~ 1 + lls + poly(rfl,2) + poly(rwn, 3) +
                  f + l + f1 + l1  + l2 + f2 +  p + p1 + p2 +
                 (1 + f + f1 + p + p1      | Subj) +
                 (1 + f + f1 + p + p1 + p2 | Sent) ,
                data= d_clean, REML=FALSE, control=lmerControl(calc.derivs=FALSE))
summary(rePCA(m2_prsm)) # ok
VarCorr(m2_prsm)
save(m2_prsm, file="./fits/m2_prsm.rda")

# compare m1_zcp and m1_cpx
anova(m1_ovi, m1_zcp, m2_prsm, m1_prsm, m1_cpx)

# RK: Defensible model with lots of interesting CPs
print(summary(m2_prsm), corr=FALSE)

# RK:
## + Subj + Sent: GM negative with f and f1,  positive with p, p1
## + Subj: negative f and p as well as f1 and p1

# RK: This needs more work, also in the fixed-effect part. Some options:
# + centering of covariates
# + polynomial trends for frequency (e.g.,cubic for f?, note that fixed effect for f is not significant)
# + consider a composite predictor based on z-scores of f, f1, f2, p2?
# + for faster fits and higher complexity (if supported by the data) -> Julia MixedModels.jl
