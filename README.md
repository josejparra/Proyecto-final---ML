# Proyecto final
Desarrollado por Daniel Delgado, José Julian Parra Montoya y Alison Gissell Ruiz Ruiz.

### Introducción

El objetivo de este trabajo es mostrar cómo los algoritmos de clasificación pueden ser útiles para tratar problemas de vinculación de tablas de datos históricos.
Este trabajo aporta novedades a la literatura en la naturaleza de los datos usados, los algoritmos que prueba, y las variables usadas para especificar los modelos.

El análisis se desarrolla en 4 etapas:

* Se realiza un análisis descriptivo (descriptive_analysis.R)
* Se crean los bloques con el uso de la función de bloqueo (blocking_function.R)
* Se crean las variables explicativas con el uso de la función especifica para esto (features_function.R)
* Se aplican ambas funciones a los datos (data_preprocessing.R)
* Se busca el mejor modelo y se valida su desempeño (model_selection.R)

### Tabla de contenido
-  [Install](#install)
-  [Data](#data)
-  [Scripts](#scripts)
-  [Informe](#informe)

### Install

Este proyecto requiere R y las siguientes librerias instaladas

* library(stringr)
* library(openxlsx)
* library(tidyverse)
* library(stringi)
* library(haven)
* library(stringdist)
* library(doParallel)
* library(tidyverse)
* library(foreach)
* library(openxlsx)
* library(caret)

Para instalarlas se debe correr el comando install.packages, a continuación un ejemplo de esto.

```bash
install.packages("sandwich")
```

### Data

En la carpeta [`stores`](https://github.com/josejparra/Proyecto-final---ML/tree/main/stores) se encuentra información sobre los datos, los cuales son de reserva.

### Scripts

El proyecto cuenta con los siguientes scripts de R:

* [`blocking_function.R`](https://github.com/josejparra/Proyecto-final---ML/blob/main/scripts/blocking_function.R)
* [`data_preprocessing.R`](https://github.com/josejparra/Proyecto-final---ML/blob/main/scripts/data_preprocessing.R)
*[`descriptive_analysis.R`](https://github.com/josejparra/Proyecto-final---ML/blob/main/scripts/descriptive_analysis.R)
* [`features_function.R`](https://github.com/josejparra/Proyecto-final---ML/blob/main/scripts/features_function.R)
* [`model_selection.R`](https://github.com/josejparra/Proyecto-final---ML/blob/main/scripts/model_selection.R)
* [`phonetic_algorithm.py`](https://github.com/josejparra/Proyecto-final---ML/blob/main/scripts/phonetic_algorithm.py)
### Informe

El informe se encuentra en la carpeta [`files`](https://github.com/josejparra/Proyecto-final---ML/tree/main/files), en formato pdf.
