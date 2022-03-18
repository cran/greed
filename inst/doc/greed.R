## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----example------------------------------------------------------------------
library(greed)
data(Books)
sol <- greed(Books$X) 

## ----sbm-example--------------------------------------------------------------
sol <- greed(Books$X,model=Sbm(),alg=Seed(),K=10)

## ----coef,K, clust------------------------------------------------------------
knitr::kable(table(Books$label,clustering(sol)))
K(sol)
coef(sol)

## ---- out.width="90%",fig.width=8,fig.height=8--------------------------------
plot(sol, type='tree') # try also: type="path"

## -----------------------------------------------------------------------------
sol_K3 = cut(sol, K=3)
K(sol_K3)
knitr::kable(table(Books$label,clustering(sol_K3)))

## ----plot,message=FALSE,results="hide", fig.show='hold', out.width="90%",fig.width=8,fig.height=8----
plot(sol,type='blocks')
plot(sol, type='nodelink')

## ---- eval=FALSE--------------------------------------------------------------
#  available_models()

## ----future-------------------------------------------------------------------
library(future)
plan(multisession, workers=2) # may be increased

