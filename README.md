# Problem-set-2
Taller 2 Big data
Maria Paula Osuna (202021732), Hugo Sabogal (202013538), Juan Andres Silva (201923201) y Gabriela Pérez (202013115).

# Carpetas
Scripts: En esta carpeta se encuentra el Script con el código desde la limpieza de datos hasta los modelos.
Stores: Contiene la base de datos construida a partir de los 4 archivos entregados (de entrenamiento y de prueba)
Views: Contiene las gráficas y tabla utilizadas para las estadísticas descriptivas.

# Script 
Inicialmente, se importaron los 4 archivos, 2 de entrenamiento y 2 de testeo. A partir de estos, se realizó la limpieza de los datos inicialmente para la base de personas tanto de entrenamiento como de prueba, se eliminaron de las variables categóricas las observaciones que contuviesen el número 9, pues estas eran igual a tener missing values. Adicionalmente, se imputó el valor del 30% para eliminar aquellas variables que contuviesen más de esto en missing values. Se construyó una base a nivel jefe del hogar para la construcción de variables y además, para evitar la pérdida de variables importantes para la predicción de la pobreza en Colombia como "oficio". Por último, se condensó la base de personas, con las nuevas variables y la de hogar para de ahí en adelante trabajar con estas para la construcción de una serie de modelos. El repositorio cuenta con multiples scripts, para reproducir el codigo en su totalidad desde un solo script se recomienda ejecutar el scriopt llamado MainR.R.
