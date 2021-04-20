install.packages("sets") # só precisa instalar uma vez 
library(sets) # carregar toda vez que inicar o R 

sets_options('universe', seq(10,70,0.1)) #definir o universo
#Granulalidade: tipo a descida do gradiente (colocar número menor pra treinar melhor o modelo)

## definir as variáveis 
variables <-
  set(peso = 
         fuzzy_variable(leve  = fuzzy_triangular(corners=c(10,20,30)),
                             medio    = fuzzy_triangular(corners=c(25,35,45)),
                             pesado  = fuzzy_triangular(corners=c(40,50,60))),
      comprimento_circunferencia = 
        fuzzy_variable(pequeno = fuzzy_triangular(corners=c(20,30,40)),
                       mediano = fuzzy_triangular(corners=c(35,45,50)),
                       grande = fuzzy_triangular(corners=c(50,60,70))),
      estado = fuzzy_partition(varnames = c(recusado = 10, ideal = 40, acima_do_ideal = 55), 
                                 sd = 5)
      
     
  )

## definir as regras
rules <- set(
  fuzzy_rule(peso %is% leve && comprimento_circunferencia %is% pequeno, estado %is% recusado),
  fuzzy_rule(peso %is% leve && comprimento_circunferencia %is% mediano, estado %is% ideal),
  fuzzy_rule(peso %is% leve && comprimento_circunferencia %is% grande, estado %is% recusado),
  fuzzy_rule(peso %is% medio && comprimento_circunferencia %is% pequeno, estado %is% acima_do_ideal),
  fuzzy_rule(peso %is% medio && comprimento_circunferencia %is% mediano, estado %is% ideal),
  fuzzy_rule(peso %is% medio && comprimento_circunferencia %is% grande, estado %is% ideal),
  fuzzy_rule(peso %is% pesado && comprimento_circunferencia %is% pequeno, estado %is% acima_do_ideal),
  fuzzy_rule(peso %is% pesado && comprimento_circunferencia %is% mediano, estado %is% acima_do_ideal),
  fuzzy_rule(peso %is% pesado && comprimento_circunferencia %is% grande, estado %is% ideal)
)


## definir o modelo
model <- fuzzy_system(variables, rules) 
print(model)
windows()
plot(model)

#Inferir valores
example.1 <- fuzzy_inference(model, list(peso = 20, comprimento_circunferencia = 58))
# Defuzzificação
gset_defuzzify(example.1, "smallestofmax")
windows()
plot(example.1)

