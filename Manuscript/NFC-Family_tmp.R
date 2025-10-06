# revised model specification

nfc_model_r <- '
    # Paths structural model
    
      afs_p ~ a1 * nfc_p
      afs_k ~ d1 * afs_p
      iko_p ~ a3 * nfc_p
      iko_k ~ d3 * iko_p
      afk_p ~ a5 * nfc_p
      afk_k ~ d5 * afk_p
      voe_p ~ a7 * nfc_p
      voe_k ~ d7 * voe_p
      
      nfc_k ~ b2 * afs_k + b4 * iko_k + b6 * afk_k + b8 * voe_k + cp * nfc_p
      
    # paths of no interest
    
      afs_k ~ a2 * nfc_p
      iko_k ~ a4 * nfc_p
      afk_k ~ a6 * nfc_p
      voe_k ~ a8 * nfc_p
      afs_p ~ b1 * nfc_k
      iko_p ~ b3 * nfc_k
      afk_p ~ b5 * nfc_k
      voe_p ~ b7 * nfc_k
      
    # Covariances
    
      nfc_k ~~ rho1 * who_k
      nfc_k ~~ rho2 * ss_k
      
    # intercorrelations <- NEW
      
      # mediators p
      afs_p ~~ iko_p
      afs_p ~~ afk_p
      afs_p ~~ voe_p
      iko_p ~~ afk_p
      iko_p ~~ voe_p
      afk_p ~~ voe_p

      # mediators k
      afs_k ~~ iko_k
      afs_k ~~ afk_k
      afs_k ~~ voe_k
      iko_k ~~ afk_k
      iko_k ~~ voe_k
      afk_k ~~ voe_k

    # Indirect effects (see https://doi.org/10.1080/00273171.2019.1618545 and https://doi.org/10.1093/ije/dyw277)
    
      ind1 :=  a1 * d1 * b2 
      ind2 :=  a3 * d3 * b4 
      ind3 :=  a5 * d5 * b6 
      ind4 :=  a7 * d7 * b8 
    
    # Total effect
    
      total := ind1 + ind2 + ind3 + ind4 + cp'

# Determine the model fit
nfc_fit_r <- lavaan::sem(
  model = nfc_model_r,
  data  = df_scales,
  estimator = "MLR"
)
summary(nfc_fit_r, fit.measures = T, standardized = T)

anova(nfc_fit_r, nfc_fit)
