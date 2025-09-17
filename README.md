# Desigualdad y Acceso a Internet (1990–2019)

Este proyecto analiza la relación entre la **desigualdad económica** (medida a través del coeficiente de Gini) y el **acceso a Internet** (aproximado por el número de no usuarios normalizados por población) en un panel de países para el periodo 1990–2019.  

El trabajo combina integración de datos, normalización, análisis de correlaciones, modelos de panel y visualizaciones interactivas.

---

## 📂 Estructura del repositorio

proyecto-ciencia-datos/
│
├── Datos/ # Bases de datos originales y script principal
│ ├── gini.csv
│ ├── non_net_users_num.csv
│ ├── pop.csv
│ └── main_code.R # Script maestro de preparación, limpieza y análisis
│
├── Dashboard/ # Aplicación Shiny para explorar resultados
│ ├── app.R
│ ├── global.R
│ 
│
├── Informe
│ ├── informe.Rmd # Informe técnico en RMarkdown
│ ├── informe.html # Presentación en RMarkdown (ioslides / beamer)
│ 
│
└── README.md # Este archivo


---

## 🎯 Objetivos

**Objetivo principal**  
Explorar y cuantificar la relación entre desigualdad económica y acceso a Internet en un panel de países (1990–2019).  

**Objetivos específicos**
- Integrar datos de desigualdad, población y uso de Internet en un panel anual por país.  
- Normalizar los valores de no uso de Internet sobre población total.  
- Calcular correlaciones globales, *between* y *within*.  
- Generar visualizaciones dinámicas (gráficos y mapas interactivos).  
- Comunicar los resultados mediante informes reproducibles y un dashboard interactivo.  

---

## 📊 Metodología

1. **Limpieza y normalización de datos**  
   - Conversión de abreviaturas (“k”, “M”) a valores numéricos.  
   - Homogeneización de series temporales.  
   - Normalización del no uso de Internet sobre población.  

2. **Construcción del panel**  
   - Unión de Gini, población y no uso de Internet en formato largo (*tidy*).  
   - Creación de variable derivada: **ratio Gini / no uso normalizado**.  

3. **Análisis**  
   - Correlaciones (Pearson, Spearman).  
   - Separación *between* (entre países) y *within* (variación temporal).  
   - Visualización:  
     - Tablas resumen.  
     - Gráficos de dispersión y tendencias.  
     - Mapas interactivos con `leaflet`.  

4. **Comunicación**  
   - Informe reproducible (`informe.Rmd`).  
   - Presentación ejecutiva (`presentacion.Rmd`).  
   - Dashboard interactivo (`Dashboard/app.R`).  

---

## 🔍 Resultados principales

- Se observa una **correlación positiva** entre desigualdad (Gini) y no uso de Internet.  
- Los países más desiguales presentan mayores proporciones de no usuarios.  
- La relación persiste incluso después de controlar por variaciones dentro de cada país (*within*).  
- A pesar de anomalías puntuales en los datos (p.ej. porcentajes de no usuarios > 100%), la tendencia general se mantiene robusta.  

---

## 🚀 Cómo reproducir

1. Clonar el repositorio:  
   ```bash
   git clone https://github.com/usuario/proyecto-ciencia-datos.git
   cd proyecto-ciencia-datos
   
   
