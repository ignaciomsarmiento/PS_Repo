# 📘 Guía para Repositorios 

## Problem Sets – BDML

Este repositorio sirve como **plantilla y guía** para organizar repositorios reproducibles de los problem sets del curso.

El objetivo **no es solo que el código corra**, sino que el repositorio funcione como un **objeto académico reproducible**, similar a un *replication package* de un paper aplicado.

**Sobre el README:** El README debe ayudar al lector a navegar tu repositorio. Un buen README hace que tu proyecto destaque entre otros proyectos y es el primer archivo que una persona ve cuando encuentra tu repositorio. Por lo tanto, este archivo debe ser lo suficientemente detallado para enfocarse en tu proyecto y en cómo lo hace, pero no tan extenso que pierda la atención del lector. Por ejemplo, [Project Awesome](https://github.com/matiassingers/awesome-readme) tiene una lista curada de READMEs interesantes.


---

## 1. Filosofía general

Un buen repositorio académico debe cumplir tres principios:

1. **Reproducibilidad total**
   Cualquier persona debería poder clonar el repo, correr **un solo comando**, y obtener exactamente los mismos resultados.

2. **Separación clara entre código y resultados**
   El código *produce* resultados; los resultados **no se editan manualmente**.


### 1.3 Un script, una responsabilidad
Cada script debe hacer **una cosa bien definida**. No mezclar limpieza de datos, estimación y generación de outputs en el mismo archivo.

**Ejemplos de buenos nombres:**
- ✅ `data_scraper.R`  
- ✅ `tune_random_forest_hyperparameters.R` 
- ✅ `compute_spatial_cv_splits.R` 
- ✅ `bootstrap_confidence_intervals.R` 
- 🚫 `analysis_section2.R` – ¿qué hace exactamente?
- 🚫 `models.R` – demasiado genérico

### 1.4 Documentación como defensa de decisiones
El código debe estar **documentado para defender decisiones**, no solo para explicar sintaxis.

Cada decisión metodológica, económica, o estadística debe estar explicada o justificada en comentarios.

> 💡 Piensa en tu repo como el *Online Appendix* de un paper.

---

## 2. Estructura estándar del repositorio

Todos los problem sets deben respetar esta estructura base:

```
project-name/
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

## 3. El README: qué debe contener 

Cada problem set deberá tener un `README.md` que siga **este orden conceptual**, inspirado en repositorios de replicación de papers.

### 3.1 Título y autores

```markdown
# Project Title
Autores  
Curso – Universidad – Año
```

Debe permitir identificar el proyecto sin abrir el código.

---

### 3.1 Replication instructions (el corazón)

Debe existir **una única instrucción clara**:

```markdown
To reproduce all results, run:

source("01_code/00_rundirectory.R")
```

Y explicar **qué hace ese script**, a alto nivel.

> 🚫 Si hay que correr scripts manualmente “en orden”, el repo está mal diseñado.


### 3.2 Code structure 

Luego hay que agregar una breve descripción del rol de cada script (la tarea bien definida del nombre). Por ejemplo

- `00_rundirectory.R` master script, run to reproduce all results
- `01_data_scraper.R` scrapes the data from https://ignaciomsarmiento.github.io/GEIH2018_sample/

No explicar línea por línea: explicar **responsabilidades**.

---

### 3.3 Outputs

Todos los outputs se generan automáticamente en `02_outputs/`

- Figuras (`02_outputs/figures/`)

   - `boxplot_income.png` – Boxplot del ingreso total laboral
   - `[nombre].png` – [Descripción]

- Tablas (`02_outputs/tables/`)

   - `estimation_unconditional_age_income_profile.tex` – Muestra los resultados de la estimación del perfil edad ingreso no condicional
   - `[nombre].tex` – [Descripción]

### Predicciones Kaggle (`02_outputs/predictions/`)

- `[nombre_del_modelo_hiperparametros].csv` – Predicciones para Kaggle


---

### 3.4 Software / environment

Debe permitir que otro reproduzca el entorno.

```markdown
## Software
- R version
- Required packages
```

Opcional pero recomendado:

* `renv`, `sessionInfo()`, o lista explícita de paquetes.


---

## 5. Qué NO hacer

🚫 Guardar resultados manuales

🚫 Correr scripts “a mano”

🚫 README que solo diga “ver código”

🚫 Nombres como `final_v2_REAL_final.R`

🚫 Paths absolutos (`C:/Users/...`)

---

## 6. Checklist antes de entregar

Todo repo debe pasar este test:

* [ ] Clono el repo en una carpeta vacía
* [ ] Abro R
* [ ] Corro `source("01_code/00_rundirectory.R")`
* [ ] Se generan outputs sin errores
* [ ] El README explica claramente qué pasó

Si algo falla → **el repo no es reproducible**.

## 7. Sugerencias

### 7.1 Incluir `.gitignore`

Es recomendable incluir un archivo `.gitignore` en tu repositorio. Esto mantiene el repo limpio y evita subir archivos temporales, credenciales, o datos pesados.


