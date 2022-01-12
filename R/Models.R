Models=function(m){

Modelos=list(Exponencial1=
               y ~ a/(1+b*exp(b-c*x)),
             Exponencial2=
               y ~ a*exp(b*x),
             Schnute=
               y~ a/(1+d*exp(b*c-x))^(1/d),
             Michierlich=
               y ~ a*(1-exp(c*b-c*x)),
             Richards=
               y~a/(1+exp(b-c*x))^(1/d),
             Gompertz=
               y ~ a*exp(-exp(b-c*x)),
             Logistico=
               y ~ a/(1+exp(b-c*x)),
             Meloun_I=
               y~a-b*exp(-c*x),
             Meloun_II=
               y~a-exp(-b-c*x),
             Brody=
               y ~ a*(1-b*exp(-c*x)),
             VonBertalanffy=
               y~a*(1-b*exp(-c*x))^3,
             Michaelis_Menten=
               y ~ (a*x)/(x+b)
)
Modelos[[m]]
}
