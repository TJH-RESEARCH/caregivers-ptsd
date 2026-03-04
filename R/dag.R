dag <- 
  dagify(
    DAS ~ PTSD + Hrs + VG + VR + Inc + Edu + Sex + Age + Race,
    VR ~ Fam + Mar,
    VG ~ Fam + SES + Sex + Age + Race,
    PTSD ~ VR + VG + Hrs + Sex + Age + Race, 
    Hrs ~ VR,
    Mar ~ VG,
    SES ~ Fam + Race,
    Edu ~ VG + SES + Sex + Age + Race,
    Inc ~ VG + SES + Edu + Sex + Age + Race,
    exposure = 'PTSD', 
    outcome = "DAS",
    #latent = c("SES", "Mar", "Fam"),
    coords = list(
       x = c(SES = -.3, Mar = -.4, Fam = -.4, VR = -.2, Hrs = -.15,VG = -.2, Edu = -.2, Inc = -.07, PTSD = 0, DAS = .16, Sex = -.1, Age = .05, Race = .12),
       y = c(SES = .3, Mar = -.07, Fam = .07, VR = -.07, Hrs = - .15, VG = .07, Edu = .2, Inc = .15, PTSD = 0, DAS = 0, Sex = .25, Age = .25, Race = .35))
  )


ggdag(dag) + ggdag::theme_dag()
adjustmentSets(dag)
ggdag_adjustment_set(dag) + theme(legend.position = 'none')

# Direct?
adjustmentSets(dag, effect = "direct")

# Latent
dag <- setVariableStatus(dag, "latent", c("SES", "Mar", "Fam"))
ggdag_adjustment_set(dag) + theme(legend.position = 'none')

# Direct with latent?
adjustmentSets(dag, effect = "direct")

ggdag_adjustment_set(dag, effect = "direct") + 
  theme(legend.position = 'none')

## So you can't get the total effect for VR with these latent variables
## I think because being a veteran affects if you care for veterans in this DAG
## via marriage.
## But the veteran recipient and giver variables are both influenced
## by the unmeasured confound of being from a military family.


# Now for the effects of VR
## And I haven't added anything about the Recipient's gender, race, etc
## which will likely lead to the same place as for the caregiver 
setVariableStatus(dag, "exposure", "VR") %>% 
  adjustmentSets(effect = "total")

setVariableStatus(dag, "exposure", "VR") %>% 
  adjustmentSets(effect = "direct")
