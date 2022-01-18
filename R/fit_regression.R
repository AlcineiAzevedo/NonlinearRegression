#'Ajuste de modelos de regressao nao linear
#'
#' @description Esta funcao realiza o ajuste de modelos de regressao nao linear
#' considerando dados sem repeticao ou com delineamento estatistico.
#' @usage fit_regression(Data,model,start,design=1,verbose=TRUE)
#' @param Data    :Matriz contendo o conjunto de dados. A configuracao desta
#' matriz depende do design selecionado:\cr
#' 1 : Para dados sem repeticao deve haver apenas duas colunas, a primeira com
#' identificacao dos tratamentos quantitativos e a segunda da variavel resposta.\cr
#' 2 : Para dados de experimento em DIC deve haver apenas tres colunas, a primeira
#'  com identificacao dos tratamentos quantitativos, a segunda com identificacao
#'  das repeticoes e a terceira com a variavel resposta.\cr
#' 3 : Para dados com repeticao de experimento em delineamento em blocos
#' casualizados deve haver apenas tres colunas, a primeira com identificacao dos
#' tratamentos quantitativos, a segunda com identificacao dos blocos e a terceira
#' com a variavel resposta.\cr

#' @param model    :Refere-se ao modelo de regressao nao linear que deseja-se ajustar.
#' Pode ser uma formula ou um numero indicando um dos modelos abaixo:\cr
#' 1: y ~ a/(1+b*exp(b-c*x))  -> Exponencial1\cr
#' 2: y ~ a*exp(b*x) -> Exponencial2\cr
#' 3: y~ a/(1+d*exp(b*c-x))^(1/d) -> Schnute\cr
#' 4: y ~ a*(1-exp(c*b-c*x)) -> Michierlich\cr
#' 5: y~a/(1+exp(b-c*x))^(1/d)  -> Richards\cr
#' 6: y ~ a*exp(-exp(b-c*x)) -> Gompertz\cr
#' 7: y ~ a/(1+exp(b-c*x))  -> Logistico\cr
#' 8: y~a-b*exp(-c*x)  -> Meloun_I\cr
#' 9: y~a-exp(-b-c*x) ->  Meloun_II\cr
#' 10: y ~ a*(1-b*exp(-c*x))  ->  Brody\cr
#' 11: y~a*(1-b*exp(-c*x))^3  ->  VonBertalanffy\cr
#' 12: y ~ (a*x)/(x+b)   -> Michaelis_Menten\cr
#'
#' @param start    :valores iniciais (chutes) para o processo iterativo inseridos em um
#'  vetor.
#' @param design    :valor numerico que indica se:\cr
#' 1 : para dados sem repeticao.\cr
#' 2 : para dados com repeticao de experimento em delineamento
#'  inteiramente casualizado.\cr
#' 3 : para dados com repeticao de experimento em delineamento em
#' blocos casualizados.\cr
#' @param verbose    : Valor logico. Se TRUE (default) serao apresentados os
#' resultados da regressao.
#' @return A funcao apresenta os resultados dos ajustes dos modelos de regressao.
#' @author Alcinei Mistico Azevedo
#' @seealso \code{\link{aov}}, \code{\link{lm}}, \code{\link{nls}}
#' @references
#'  SILVEIRA, F.G.; SILVA, F.F.; CARNEIRO, P.S.; MALHADO, C.H.M.;  MUNIZ, J.A.
#' Analise de agrupamento na selecao de modelos de regressao nao-lineares para curvas de
#' crescimento de ovinos cruzados. Ciencia Rural, v.41, p.692- 698, 2011.

#' @examples
#' #dados sem repeticao
#' data("DadosMED")
#' modelo=y~a*x^b/(c^b+x^b)
#' chute=list(a=36,b=2,c=30)
#' Ajuste1=fit_regression(Data =DadosMED,model = modelo,start = chute,design=1)
#' plot_regression(Ajuste1,plot=2)
#' plot_regression(Ajuste1,plot=7,xlabel = "Dias",ylabel = "Altura (cm)")
#'
#' #Modelo logistico
#' Ajuste2=fit_regression(Data =DadosMED,model = 7,start = c(a=30,b=5,c=0.1),design=1)
#' plot_regression(Ajuste2,plot=7,xlabel = "Dias",ylabel = "Altura (cm)")
#'
#'
#' #dados em DIC
#' data(DadosDIC)
#' modelo=y~a*x^b/(c^b+x^b)
#' chute=list(a=36,b=2,c=30)
#' Ajuste3=fit_regression(Data =DadosDIC,model = modelo,start = chute,design=2)
#' plot_regression(Ajuste3,plot=7,xlabel = "Doses",ylabel = "Altura (cm)")
#'
#' #
#' #dados em DBC
#' data(DadosDBC)
#' modelo=y~a*x^b/(c^b+x^b)
#' chute=list(a=36,b=2,c=30)
#' Ajuste3=fit_regression(Data =DadosDBC,model = modelo,start = chute,design=3)
#' plot_regression(Ajuste3,plot=7,xlabel = "Doses",ylabel = "PROD")
#'
#' @importFrom stats AIC BIC aov coefficients cor deriv3 nls pf predict pt residuals
#' @export

fit_regression=function(Data,model,start,design=1,verbose=TRUE){


Dados=Data
  inicial=start

  chute=inicial
  if(is.numeric(model)){modelo=Models(model)}
  if(!is.numeric(model)){modelo=model}

  sq=function(Fator,Y){
    X=as.factor(Fator)
    #X=c(X)
    NumTrat=length(unique(as.factor(Fator)));
    NomeTrat=unique(as.factor(Fator))
    NumParc=length(X);

    sq=sum(tapply(Y,X,sum)^2/table(X))-sum(Y)^2/NumParc


    return(c(GL=NumTrat-1,SQ=sq))
  }


  if(design==1){

    Dmed=Data
    x=Dmed[,1]
    y=Dmed[,2]

    DDD=data.frame(x=x,y=y)
    ajuste=nls(modelo, data=DDD,start=chute)
    n=length(x)
    pred=predict(ajuste)

    Regressao= c(length(chute)-1,sum(pred^2)-sum(y)^2/n)
    Total=c(n-1,sum(y^2)-sum(y)^2/n)
    Residuos=Total-Regressao


    Anova=data.frame(rbind(Regressao,Residuos,Total))
    Anova=cbind(Anova,Anova[,2]/Anova[,1])
    Anova=cbind(Anova,Anova[,3]/Anova[2,3])
    Anova=cbind(Anova,1-pf(Anova[,4],Anova[1,1],Anova[2,1]))
    Anova[,2:5]=round(Anova[,2:5],5)
    Anova[3,3]="";  Anova[2:3,4]="" ;  Anova[2:3,5]=""
    colnames(Anova)=c("GL","SQ","QM","Fc","pValor")
Summary=summary(ajuste)
    if(verbose==TRUE){print(Anova)
    print("--------------------------------------")
    print(Summary)
    print("--------------------------------------")
    }


    R2=cor(y,pred)^2
    p=length(chute)
    R2ajust=1-(1-R2)*(n-1)/(n-p-1)




    Avaliador=c(R2=R2,R2ajust=R2ajust,AIC=AIC(ajuste),BIC=BIC(ajuste))
     Preditos=cbind(x=seq(min(x),max(x),l=20),predito=predict(ajuste,newdata = data.frame(x=seq(min(x),max(x),l=20))))
    if(verbose==TRUE){
    print("--------------------------------------")
    print(Avaliador)
    print(Preditos)
     }


  }   #media

  if(design==2){
    Dmed=D=Data ; X=Dmed[,1]; Y=Dmed[,3]

    Dmed=apply(D,2,function(x) tapply(x,as.factor(X),mean))[,-2]
    x=Dmed[,1]; y=Dmed[,2]
    DDD=data.frame(x=x,y=y)
    ajuste=nls(modelo, data=DDD,start=chute)
    n=length(X)
    pred=predict(ajuste,newdata=data.frame(x=X))


    mod=aov(Y~as.factor(X))
    anova=anova(mod)
    Trat=c(anova$Df[1],anova$`Sum Sq`[1])
    Regressao= c(length(chute)-1,sum(pred^2)-sum(Y)^2/n)
    DesvReg=Trat-Regressao
    Total=c(n-1,sum(Y^2)-sum(Y)^2/n)
    Residuos=Total-Trat


    Anova=data.frame(rbind(Trat,Regressao,DesvReg,Residuos,Total))
    Anova=cbind(Anova,Anova[,2]/Anova[,1])
    Anova=cbind(Anova,Anova[,3]/Anova[4,3])
    Anova=cbind(Anova,1-pf(Anova[,4],Anova[,1],Anova[4,1]))
    Anova[,2:5]=round(Anova[,2:5],5)
    Anova[5,3]="";  Anova[4:5,4]="" ;  Anova[4:5,5]=""
    colnames(Anova)=c("GL","SQ","QM","Fc","pValor")

    r=mean(table(X))
    QMR=sum(residuals(mod)^2)/mod$df.residual


    if(verbose==TRUE){
    print(Anova)
    print("--------------------------------------")

}
    coefs=list(a=NULL,b=NULL,c=NULL,d=NULL,e=NULL,f=NULL)

    for (i in 1:length(coefficients(ajuste))){
      coefs[[i]]=coefficients(ajuste)[i]
    }
    a=coefs[[1]];b=coefs[[2]];c=coefs[[3]];d=coefs[[4]];e=coefs[[5]];f=coefs[[6]]

    d3 =deriv3(modelo,letters[1:length(coefficients(ajuste))], function(a=coefs[[1]],b=coefs[[2]],c=coefs[[3]],d=coefs[[4]],e=coefs[[5]],f=coefs[[6]],x){NULL})
    Pred=d3(a,b,c,d,e,f,x)
    X=attr(Pred, "gradient")
    X[is.na(X)]=0
    var=diag( MASS::ginv(t(X)%*%X)*QMR/r)
    coeficientes=coefficients(ajuste)
    ErroPadrao=sqrt(var)
    tcalc=coeficientes/ErroPadrao
    pvalor=2*(1-pt(abs(tcalc),mod$df.residual))

    Sig=cbind(coeficientes=coeficientes,ErroPadrao=ErroPadrao,tcalc=tcalc,pvalor=pvalor)
    Summary=Sig
    if(verbose==TRUE){
    print(Sig)
    print("--------------------------------------")
}
    R2=cor(y,predict(ajuste))^2
    p=length(chute)
    R2ajust=1-(1-R2)*(n-1)/mod$df.residual




    Avaliador=c(R2=R2,R2ajust=R2ajust,AIC=AIC(ajuste),BIC=BIC(ajuste))
    Preditos=cbind(x=seq(min(x),max(x),l=20),predito=predict(ajuste,newdata = data.frame(x=seq(min(x),max(x),l=20))))

    if(verbose==TRUE){
    print("--------------------------------------")
    print(Avaliador)
     print(Preditos)
    }



  }  #DIC

  if(design==3){
    Dmed=D=Data ; X=Dmed[,1]; Y=Dmed[,3];Bloco=as.factor(Dmed[,2])

    Dmed=apply(D,2,function(x) tapply(x,as.factor(X),mean))[,-2]
    x=Dmed[,1]; y=Dmed[,2]
    DDD=data.frame(x=x,y=y)
    ajuste=nls(modelo, data=DDD,start=chute)
    n=length(X)
    pred=predict(ajuste,newdata=data.frame(x=X))


    mod=aov(Y~as.factor(X)+Bloco)
    anova=anova(mod)
    Trat=c(anova$Df[1],anova$`Sum Sq`[1])
    Regressao= c(length(chute)-1,sum(pred^2)-sum(Y)^2/n)
    DesvReg=Trat-Regressao
    Total=c(n-1,sum(Y^2)-sum(Y)^2/n)
    Bloco=c(anova$Df[2],anova$`Sum Sq`[2])
    Residuos=Total-Trat-Bloco


    Anova=data.frame(rbind(Trat,Regressao,DesvReg,Bloco,Residuos,Total))
    Anova=cbind(Anova,Anova[,2]/Anova[,1])
    Anova=cbind(Anova,Anova[,3]/Anova[5,3])
    Anova=cbind(Anova,1-pf(Anova[,4],Anova[,1],Anova[5,1]))
    Anova[,2:5]=round(Anova[,2:5],5)
    Anova[6,3]="";  Anova[5:6,4]="" ;  Anova[5:6,5]=""
    colnames(Anova)=c("GL","SQ","QM","Fc","pValor")

    r=mean(table(X))
    QMR=sum(residuals(mod)^2)/mod$df.residual


    if(verbose==TRUE){
    print(Anova)
    print("--------------------------------------")
}

    coefs=list(a=NULL,b=NULL,c=NULL,d=NULL,e=NULL,f=NULL)

    for (i in 1:length(coefficients(ajuste))){
      coefs[[i]]=coefficients(ajuste)[i]
    }
    a=coefs[[1]];b=coefs[[2]];c=coefs[[3]];d=coefs[[4]];e=coefs[[5]];f=coefs[[6]]

    d3 =deriv3(modelo,letters[1:length(coefficients(ajuste))], function(a=coefs[[1]],b=coefs[[2]],c=coefs[[3]],d=coefs[[4]],e=coefs[[5]],f=coefs[[6]],x){NULL})
    Pred=d3(a,b,c,d,e,f,x)
    X=attr(Pred, "gradient")
    X[is.na(X)]=0
    var=diag( MASS::ginv(t(X)%*%X)*QMR/r)
    coeficientes=coefficients(ajuste)
    ErroPadrao=sqrt(var)
    tcalc=coeficientes/ErroPadrao
    pvalor=2*(1-pt(abs(tcalc),mod$df.residual))

    Sig=cbind(coeficientes=coeficientes,ErroPadrao=ErroPadrao,tcalc=tcalc,pvalor=pvalor)
    Summary=Sig

     if(verbose==TRUE){
    print(Sig)
    print("--------------------------------------")
}
    R2=cor(y,predict(ajuste))^2
    p=length(chute)
    R2ajust=1-(1-R2)*(n-1)/mod$df.residual




    Avaliador=c(R2=R2,R2ajust=R2ajust,AIC=AIC(ajuste),BIC=BIC(ajuste))
    Preditos=cbind(x=seq(min(x),max(x),l=20),predito=predict(ajuste,newdata = data.frame(x=seq(min(x),max(x),l=20))))
    if(verbose==TRUE){
    print("--------------------------------------")
    print(Avaliador)
    print(Preditos)
    }

  }  #DBC

# if(design==5){
#
#     Dmed=Dados ;B=as.factor(Dmed[,2]); XX=Dmed[,1]; YY=Dmed[,4];Bloco=as.factor(Dmed[,3])
#
#     Predito=NULL
#     aa=0
#     for(i in unique(B)){
#       aa=aa+1
#       id=B==i
#       x=tapply(XX[id],as.factor(XX[id]),mean)
#       y=tapply(YY[id],as.factor(XX[id]),mean)
#
#     DDD=data.frame(x=x,y=y)
#     ajuste=nls(modelo, data=DDD,start=chute[[aa]])
#     n=length(XX)
#     pred=predict(ajuste,newdata=data.frame(x=XX[id]))
#     Predito=c(Predito,pred)
#     }
#
#     FA=as.factor(XX)  ; FB=as.factor(B)
#     mod=aov(YY~FA*FB+Bloco)
#     anova=anova(mod)
#     print(anova)
#     QMR=anova[[3]][5]
#     GLR=anova[[1]][5]
#     ######Desdobrando FA dentro de FB
#     iiid=T
#     SQ=NULL
#     aaa=0
# aa=1
#     for(ii in unique(B)){
#       aaa=aaa+1
#       id=B==ii
#     FatA=sq(XX[id],YY[id])
#     reg=sq(XX[id],Predito[aaa:(aaa+sum(id)-1)])
#     reg[1]=length(coefficients(ajuste))-1
#     DesvReg=FatA-reg
#     print("--------------------------------------")
#
#     print(paste("Desdobramento de FA dentro do nivel",ii,"do FB" ))
#     Anova=rbind(Regresao=reg,DesvReg=DesvReg)
#     Anova=cbind(Anova,QM=Anova[,2]/Anova[,1])
#     Anova=cbind(Anova,Fc=Anova[,3]/QMR)
#     Anova=cbind(Anova,pValor=1-pf(Anova[,4],Anova[,1],GLR))
#     print(round(Anova,5))
#     print("--------------------------------------")
#     print("--------------------------------------")
#
#
#     print("Significancia dos coeficientes de regressao")
#
#     coefs=list(a=NULL,b=NULL,c=NULL,d=NULL,e=NULL,f=NULL)
#
#     for (i in 1:length(coefficients(ajuste))){
#       coefs[[i]]=coefficients(ajuste)[i]
#     }
#     a=coefs[[1]];b=coefs[[2]];c=coefs[[3]];d=coefs[[4]];e=coefs[[5]];f=coefs[[6]]
#
#
#     x=tapply(XX[id],as.factor(XX[id]),mean)
#     y=tapply(YY[id],as.factor(XX[id]),mean)
#     DDD=data.frame(x=x,y=y)
#     ajuste=nls(modelo, data=DDD,start=chute[[aa]])
#
#
#
#
#     d3 =deriv3(modelo,letters[1:length(coefficients(ajuste))], function(a=coefs[[1]],b=coefs[[2]],c=coefs[[3]],d=coefs[[4]],e=coefs[[5]],f=coefs[[6]],x){NULL})
#     Pred=d3(a,b,c,d,e,f,x)
#     X=attr(Pred, "gradient")
#     X[is.na(X)]=0
#     r=length(unique(Bloco))
#     var=diag( MASS::ginv(t(X)%*%X)*QMR/r)
#     coeficientes=coefficients(ajuste)
#     ErroPadrao=sqrt(var)
#     tcalc=coeficientes/ErroPadrao
#     pvalor=2*(1-pt(abs(tcalc),GLR))
#
#     Sig=cbind(coeficientes=coeficientes,ErroPadrao=ErroPadrao,tcalc=tcalc,pvalor=pvalor)
#     print("--------------------------------------")
#
#     print(Sig)
#
#     print("--------------------------------------")
#
#
#
#
#
#     R2=cor(y,predict(ajuste,newdata =data.frame(x=x)))^2
#     p=length(chute)
#     R2ajust=1-(1-R2)*(n-1)/mod$df.residual
#
#
#
#
#     Avaliador=c(R2=R2,R2ajust=R2ajust,AIC=AIC(ajuste),BIC=BIC(ajuste))
#     print("--------------------------------------")
#     print(Avaliador)
#
#
#
#
#
#
#     aaa=(aaa+sum(id)-1)
#
#
#     print("--------------------------------------")
#
#
#     Preditos=cbind(x=seq(min(x),max(x),l=20),predito=predict(ajuste,newdata = data.frame(x=seq(min(x),max(x),l=20))))
#     print(Preditos)
#
#     if(iiid==T){ plot(x,y,col="white")
#       iiid=F}
#     lines(Preditos[,1],Preditos[,2],col=aa)
#     aa=aa+1
#   }  #Fatorial em DBC
#
#
#   }


   return(list(Model=modelo,Anova=Anova,Summary=Summary, Avaliador=Avaliador,Means=Dmed, Preditos=Preditos))
}
