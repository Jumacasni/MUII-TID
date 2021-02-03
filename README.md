# Tratamiento Inteligente de Datos (TID)

Asignatura de Tratamiento Inteligente de Datos (TID) - Máster Profesional en Ingeniería Informática 2020/2021

<details open="open">
  <summary>Tabla de contenidos</summary>
  <ol>
    <li>
      <a href="#teoria">Teoría</a>
      <ul>
        <li><a href="#teoria1">Algoritmos de discretización</a></li>
      </ul>
    </li>
    <li>
      <a href="#practicas">Prácticas</a>
      <ul>
        <li><a href="#practica1">Preprocesamiento de datos</a></li>
        <li><a href="#practica2">Clustering</a></li>
        <li><a href="#practica3">Clasificación</a></li>
        <li><a href="#practica4">Reglas de asociación</a></li>
      </ul>
    </li>
  </ol>
</details>

<a name="teoria"></a>
## 1. Teoría

<a name="teoria1"></a>
### Algoritmos de discretización

* [Documentación](teoria/Algoritmos_discretizacion.pdf)

<a name="practicas"></a>
## 2. Prácticas

<a name="practica1"></a>
### 2.1 Preprocesamiento de datos

El objetivo del problema es el de predecir la gravedad del daño causado en un accidente (mortal, lesiones o daños materiales) en función de una serie de características como la ingesta de alcohol del conductor, hora del accidente, condición de la carretera, etc. Este tipo de predicción podrı́a ser útil, por ejemplo, para priorizar la dotación de recursos en respuesta a un accidente. Sin embargo, los datos disponibles presentan importantes deficiencias (valores perdidos, características con excesivas categorías, características e instancias prescindibles, etc.) que sugieren la necesidad de ser preprocesados antes de aplicar otras técnicas de Minería de Datos.

En esta primera práctica se pide aplicar técnicas de preprocesamiento de datos al dataset [accidentes_original.xls](practica1/accidentes_original.xls) y posteriormente realizar distintas clasificaciones con distintos preprocesamientos. Para ello, primero es necesario preprocesar ese archivo dejando en blanco las celdas que contengan valores perdidos y construir la variable clase (que es la que se clasificará) como combinación de las tres variables que describen la gravedad del accidente: ```FATALITIES```, ```INJURY CRASH``` y ```PRPTYDMG CRASH```. El dataset ya preprocesado se encuentra en [accidentes.xlsx](practica1/accidentes.xlsx).

<a name="practica1c"></a>
* [Práctica 1: Preprocesamiento de datos](practica1/practica1.R)
<a name="practica1d"></a>
* [Informe](practica1/P1_Informe.pdf)

<a name="practica2"></a>
### 2.2 Clustering

En esta segunda práctica se pide aplicar técnicas de clustering al dataset [wine.data](practica2/wine.data). Cada instancia está compuesta por trece atributos numéricos más una clase (la primera columna) que determina el nivel de alcohol del vino (tres tipos; 1, 2 y 3), y es la solución al proceso de clustering (por lo que se debe eliminar para realizar el clustering).

<a name="practica2c"></a>
* [Práctica 2: Clustering](practica2/practica2.R)
<a name="practica2d"></a>
* [Informe](practica2/P2_Informe.pdf)

<a name="practica3"></a>
### 2.3 Clasificación

La multinacional eBay Inc. desea conocer mejor el comportamiento de las transacciones producidas en su web para, a la vista de los resultados, diseñar nuevos servicios que mejoren la experiencia de los usuarios vendedores para así incrementar las ventas y, por tanto, mejorar los ingresos de la compañía. Así pues, se propone aplicar analítica empresarial de cara a extraer conocimiento útil para la toma de decisiones a partir de algunos datos disponibles. Concretamente, el objetivo es construir un modelo que clasifique las subastas entre competitivas y no competitivas.

En esta segunda práctica se pide aplicar técnicas de clasificación al dataset [eBayAuctions_original.xls](practica3/eBayAuctions_original.xls) para clasificar subastas en competitivas y no competitivas. Primero debe hacerse un preprocesamiento de datos. Para ello, se ha generado el archivo [eBayAuctions.xlsx](practica3/eBayAuctions.xlsx) que contiene un primer preprocesamiento realizado con un editor de hojas de cálculo, y es el que se ha utilizado para la realización de la práctica.

<a name="practica3c"></a>
* [Práctica 3: Clasificación](practica3/practica3.R)
<a name="practica3d"></a>
* [Informe](practica3/P3_Informe.pdf)

<a name="practica4"></a>
### 2.4 Reglas de asociación

UniversalBank desarrolló una campaña para convertir sus clientes pasivo en clientes activo. Para diseñar campañas más efectivas, se propone responder a la siguiente pregunta: "Hay varios productos/servicios que el banco ofrece como tales como cuentas de valores (securities accounts), certificados de depósitos, servicios de banca en línea o tarjetas de crédito. ¿Se puede detectar alguna relación entre estos productos para encontrar oportunidades de venta cruzada?".

En esta cuarta práctica se pide obtener reglas de asociación del dataset [prestamo.csv](practica4/prestamo.csv) para responder a la pregunta formulada.

<a name="practica4c"></a>
* [Práctica 4: Reglas de asociación](practica4/practica4.R)
<a name="practica4d"></a>
* [Informe](practica4/P4_Informe.pdf)

## Licencia 📄

Este repositorio está bajo la licencia [GPLv3](LICENSE)
