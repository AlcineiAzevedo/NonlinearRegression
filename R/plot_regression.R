#'Criacao de grafico de regressao nao linear
#'
#' @description Esta funcao cria um grafico do ajuste do modelos de regressao nao linear.
#' @usage plot_regression(Adj,xlabel="x", ylabel="y", colPoints="blue",colLine="red",plot=7)
#' @param Adj    :Objeto criado pela funcao fit_regression.
#' @param xlabel    :Titulo no eixo x.
#' @param ylabel    :Titulo no eixo y.
#' @param colPoints    :cor dos pontos de dispersao.
#' @param colLine    : cor da linha.
#' @param plot    : Valor numerico variando entre 1 e 9 indicando o visual do grafico.
#' @return A funcao apresenta um grafico do modelo ajustado.
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
#' @export



plot_regression=function(Adj,xlabel="x",
                    ylabel="y",
                    colPoints="blue",
                    colLine="red",
                    plot=7){

Ajuste1=Adj
design=plot
x=Ajuste1$Means[,1]
y=Ajuste1$Means[,2]
Preditos=data.frame(Ajuste1$Preditos)

Dg=data.frame(x=x,y=y)
predito=0

PLOT=ggplot2::ggplot(Dg, ggplot2::aes(x=x, y=y)) +
  ggplot2::labs(x = xlabel,y=ylabel)+

  ggplot2::geom_point(col=colPoints) +
  ggplot2::geom_line(data = Preditos, ggplot2::aes(x=x, y=predito), color = colLine)

if(design==1){
  PLOT=PLOT+ggplot2::theme_grey()

}

if(design==2){
  PLOT=PLOT+ggplot2::theme_bw()

}

if(design==3){
  PLOT=PLOT+ggplot2::theme_linedraw()

}

if(design==4){
  PLOT=PLOT+ggplot2::theme_light()

}


if(design==5){
  PLOT=PLOT+ggplot2::theme_dark()

}


if(design==6){
  PLOT=PLOT+ggplot2::theme_minimal()

}

if(design==7){
  PLOT=PLOT+ggplot2::theme_classic()

}

if(design==8){
  PLOT=PLOT+ggplot2::theme_void()

}


if(design==9){
  PLOT=PLOT+ggplot2::theme_test()

}

print(PLOT)
}
