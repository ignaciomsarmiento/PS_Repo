# 📘 Guía para Repositorios 

## Problem Sets – BDML

Este repositorio sirve como **plantilla y guía** para organizar repositorios reproducibles de los problem sets del curso.

El objetivo **no es solo que el código corra**, sino que el repositorio funcione como un **objeto académico reproducible**, similar a un *replication package* de un paper aplicado.

**Sobre el README:** El README debe ayudar al lector a navegar tu repositorio. Un buen README hace que tu proyecto destaque entre otros proyectos y es el primer archivo que una persona ve cuando encuentra tu repositorio. Por lo tanto, este archivo debe ser lo suficientemente detallado para enfocarse en tu proyecto y en cómo lo hace, pero no tan extenso que pierda la atención del lector. Por ejemplo, [Project Awesome](https://github.com/matiassingers/awesome-readme) tiene una lista curada de READMEs interesantes.


---

## Filosofía general

Un buen repositorio académico debe cumplir tres principios:

1. **Reproducibilidad total**
   Cualquier persona debería poder clonar el repo, correr **un solo comando**, y obtener exactamente los mismos resultados.

2. **Separación clara entre código y resultados**
   El código *produce* resultados; los resultados **no se editan manualmente**.


### Un script, una responsabilidad

Cada script debe hacer **una cosa bien definida**. No mezclar limpieza de datos, estimación y generación de outputs en el mismo archivo.

**Ejemplos de buenos nombres:**
- ✅ `01_data_scraper.R`  
- ✅ `02_tune_random_forest_hyperparameters.R` 
- ✅ `03_compute_spatial_cv_splits.R` 
- 🚫 `01_analysis_section2.R` – ¿qué hace exactamente?
- 🚫 `01_models.R` – demasiado genérico

Cuando el mismo bloque de código aparece repetido en varios scripts, es recomendable **reescribirlo como una función** y reutilizarla. Copiar y pegar suele introducir inconsistencias, dificulta el mantenimiento del código y va contra buenas prácticas de estilo.

Una opción simple es centralizar estas funciones, bien documentadas, en una carpeta dentro de `01_code/`, por ejemplo:

- `01_code/functions/`


### Documentación como defensa de decisiones

El código debe estar **documentado para defender decisiones**, no solo para explicar sintaxis.

Cada decisión metodológica, económica, o estadística debe estar explicada o justificada en comentarios.

**Convenciones de nombres** (siguiendo [tidyverse style guide](https://style.tidyverse.org/files.html)):

- Usar `snake_case` (minúsculas con guiones bajos)
- Nombres descriptivos que indican **qué hace** el script
- Prefijos numéricos para indicar orden de ejecución

> 💡 Piensa en tu repo como el *Online Appendix* de un paper.

---

## Estructura estándar del repositorio

Todos los problem sets deben respetar esta estructura base:

```
BDML-PSXX/
│
├── README.md
│
├── 01_code/
│   ├── 00_rundirectory.R
│   ├── 01_*.R
│   ├── 02_*.R
│   └── ...
│
├── 02_outputs/
│   ├── figures/
│   ├── tables 
│   └── predictions/ [solo para los problem sets 2 y 3 que tienen submissions de Kaggle]

```

### Convenciones clave

* `01_code/` contiene **todo el código**
* `02_outputs/` contiene **solo resultados generados**
* `00_rundirectory.R` es siempre el **script maestro**
* No usar paths absolutos
* No guardar datos crudos dentro del repo (salvo que se indique)

---

## El README: qué debe contener 

Cada problem set deberá tener un `README.md` que siga **este orden conceptual**, inspirado en repositorios de replicación de papers.

### Título y autores

```markdown
# Project Title
## Breve descripción del proyecto
Autores  
Curso – Universidad – Año
```

Debe permitir identificar el proyecto sin abrir el código.

---

###  Instruciones de Replicación

Debe existir **una única instrucción clara**:

```markdown
To reproduce all results, run:

source("01_code/00_rundirectory.R")
```

Y explicar **qué hace ese script**, a alto nivel.

> 🚫 Si hay que correr scripts manualmente “en orden”, el repo está mal diseñado.


### Estructura del código

Luego hay que agregar una breve descripción del rol de cada script (la tarea bien definida del nombre). Por ejemplo

- `00_rundirectory.R` master script, run to reproduce all results
- `01_data_scraper.R` scrapes the data from https://ignaciomsarmiento.github.io/GEIH2018_sample/

No explicar línea por línea: explicar **responsabilidades**.

---

### Salidas

Todos los outputs se generan automáticamente en `02_outputs/`.

- Figuras (`02_outputs/figures/`): visualizaciones generadas por el código  
- Tablas (`02_outputs/tables/`): resultados de estimaciones en formato `.tex`  
- Predicciones (`02_outputs/predictions/`): archivos `.csv` para Kaggle (PS2 y PS3)

Los nombres de los archivos deben ser **autoexplicativos**, es decir, deben ser descriptivos y permitir identificar el contenido (modelo, variable principal, especificación), usando `snake_case`.

Ejemplos de nombres:
- `boxplot_income.png`
- `estimation_age_income_profile.tex`
- `random_forest_ntrees500.csv`

---

### Software / entorno

El README debe incluir la información mínima necesaria para reproducir el entorno computacional.
```markdown
## Software
- R version
- Required packages
```

Opcional pero recomendado:

* `renv`, `sessionInfo()`, o lista explícita de paquetes.

---


## Sugerencias

### Incluir `.gitignore`

Es recomendable incluir un archivo `.gitignore` para evitar subir archivos temporales, credenciales o datos pesados.

### Comentarios de alto nivel

Es recomendable documentar brevemente al inicio de cada script qué hace y qué outputs genera.



---

## Qué NO hacer

🚫 Guardar resultados manuales

🚫 Correr scripts “a mano”

🚫 README que solo diga “ver código”

🚫 Nombres como `final_v2_REAL_final.R`

🚫 Paths absolutos (`C:/Users/...`)

---


## Checklist antes de entregar

Todo repositorio debe cumplir lo siguiente:

* [ ] Clono el repo en una carpeta vacía
* [ ] Abro R sin objetos en el environment
* [ ] Corro `source("01_code/00_rundirectory.R")`
* [ ] El código corre sin errores ni intervención manual
* [ ] Se generan automáticamente figuras, tablas y/o predicciones
* [ ] El README permite entender qué se generó y cómo reproducirlo

Si alguno de estos puntos falla → **el repo no es reproducible**.
