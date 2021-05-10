## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(greed)
library(mclust)
library(ggplot2)
set.seed(2134)

## ----diabetes-gmm, fig.show='hold',out.width="90%",fig.width=8,fig.height=5.5----
data(diabetes)
X=diabetes[,-1]
sol = greed(X,model=new("gmm"))
table(diabetes$cl,sol@cl)

## ----diabetes-gmm-FIG, fig.show='hold',out.width="90%",fig.width=8,fig.height=5.5----
gmmpairs(sol,X)

## ----diabetes-gmm-0.01, fig.show='hold',out.width="90%",fig.width=8,fig.height=5.5----
data(diabetes)
X=diabetes[,-1]
sol = greed(X,model=new("gmm",epsilon=0.01*diag(diag(cov(X))), tau =0.001))
gmmpairs(sol,X)
table(diabetes$cl,sol@cl)

## ----diabetes-diaggmm, fig.show='hold',out.width="90%",fig.width=8,fig.height=8----
data(diabetes)
X=diabetes[,-1]
soldiag = greed(X,model=new("diaggmm"))
gmmpairs(soldiag,X)
table(diabetes$cl,soldiag@cl)

## ----diabetes-diaggmm-cut, fig.show='hold',out.width="60%",fig.width=8,fig.height=6----
plot(soldiag,type='tree')
solK3 = cut(soldiag,3)
table(diabetes$cl,solK3@cl)

## ----diabetes-diaggmm-cut-pairs, fig.show='hold',out.width="90%",fig.width=8,fig.height=8----
gmmpairs(solK3,X)

## ----icl-comp-----------------------------------------------------------------
sol@icl
soldiag@icl

## ----diabetes-diaggmm-cut-params----------------------------------------------
params = coef(solK3)
params$Sigmak[[2]]

## ----fashion-diaggmm----------------------------------------------------------
data("fashion")
dim(fashion$X)
sol=greed(fashion$X,model=new("diaggmm"),alg=new("seed"),K=60)

## ----fashion-tree, fig.show='hold',out.width="60%",fig.width=8,fig.height=6----
plot(sol,type='tree')

## ----fashion-means, fig.show='hold',out.width="90%",fig.width=8,fig.height=8----
im_list=lapply(1:sol@K,function(k){
    data.frame(i=rep(28:1,each=28),j=rep(1:28,28),v=t(sol@obs_stats$regs[[k]]$m),k=k)
    })

ims = do.call(rbind,im_list)
ggplot(ims)+
  geom_tile(aes(y=i,x=j,fill=v))+
  scale_fill_gradientn(colors=c("#ffffff","#000000"),guide="none")+
  scale_x_continuous(breaks=c())+scale_y_continuous(breaks=c())+facet_wrap(~k)+
  coord_equal()+theme_minimal()+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank())

