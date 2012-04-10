Ranking Enlace 2011
================================

Estimaciones de proporción de buenos/excelentes por escuela evaluada
en la prueba enlace según modelo jerárquico.

1. Fuentes: Tabla agregada de muncipios (INEGI ITER 2010, tab.iter.loc.RData), Resultados de Enlace 2011 (primarias.RData).



## Instrucciones (generar modelo):

1. La primera corrida es necesario correr 00\_prep_censo.R, 01\_checar_muncipios.R. Los objetos se guardan en cache y
no es necesario volver a correr estos scripts.
2. Para correr el modelo:  10\_primarias_datos_modelo.R seguido de 11\_primarias_modelo.R
3. 13\_componente\_escuelas.R requiere la corrida de 1. y la base primarias.s creada en 1 y 2. Las salidas son
los archivos en *out* primarias\_salida\_ y los archivos \*_coef con los coeficientes del modelo
                    
## Salidas

El archivo primarias_salida en *out* contiene las variables básicas de ranking y estimación encogida
de proporción de bueno/excelente. score.quintil contiene el score relativo en quintiles. p.esp y p.mate
son las estimaciones de proporción de buenos.