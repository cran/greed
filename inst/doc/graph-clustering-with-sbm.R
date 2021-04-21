## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(future)
library(Matrix)
library(ggplot2)
library(greed)

## ---- fig.show='hold'---------------------------------------------------------
N=400
K=6
pi=rep(1/K,K)
lambda  = 0.1
lambda_o = 0.01
Ks=3
mu = bdiag(lapply(1:(K/Ks), function(k){matrix(lambda_o,Ks,Ks)+diag(rep(lambda,Ks))}))+0.001
sbm = rsbm(N,pi,mu)

## -----------------------------------------------------------------------------
sol = greed(sbm$x,model=new("sbm"))

## ---- fig.show='hold',out.width="70%",fig.width=8,fig.height=5.5--------------
plot(sol,type='blocks')

## ---- fig.show='hold',out.width="70%",fig.width=8,fig.height=5.5--------------
plot(sol,type='nodelink')

## ---- fig.show='hold',out.width="70%",fig.width=8,fig.height=5.5--------------
plot(sol,type='tree')

## ---- fig.show='hold',out.width="70%",fig.width=8,fig.height=5.5--------------
plot(sol,type='path')

## ---- fig.show='hold',out.width="70%",fig.width=8,fig.height=5.5--------------
plot(sol,type='front')

## ---- fig.show='hold',out.width="70%",fig.width=8,fig.height=5.5--------------
sol2 = cut(sol,2)
plot(sol2,type='blocks')

