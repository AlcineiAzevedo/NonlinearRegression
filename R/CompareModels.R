#' Comparar modelos de regressao Multipla
#'
#' @description Esta funcao compara modelos ajustados pela funcao `fit_regression`.
#' @usage CompareModels(List)
#' @param List    Lista contendo os resultados da funcao `fit_regression` ajustados para diferentes modelos.
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

#' @export

CompareModels=function(List){
  m=List
  data=NULL
  for(i in 1:length(m)){
    modd=as.character(m[[i]]$Model)
    mod=paste0(modd[2],modd[1],modd[3])


    res=c(Model=mod,round(m[[i]]$Avaliador,4))
    data=rbind(data,res)

  }

  rownames(data)=1:nrow(data)

  data2=apply(data[,-1],2,as.numeric)


  data=data.frame(Models=data[,1],data2)
  data2[,1:2]=-data2[,1:2]

  SelectedModel=apply(data2,2,function(x) order(as.numeric(x))[1])

  return(list(Table=data,SelectedModel=SelectedModel))
}
