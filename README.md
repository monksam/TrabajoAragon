# README para Repositorio de Github

## Tarea entregable: Disease Mapping - Modelo Logístico INLA y Reproducibilidad

### Descripción

Este proyecto aborda el mapeo de enfermedades mediante modelos estadísticos espaciales. El objetivo es explorar patrones de incidencia/mortalidad ajustada utilizando dos enfoques jerárquicos bayesianos: **WinBUGS** (simulaciones Monte Carlo) e **INLA** (aproximaciones determinísticas). Ambos métodos se aplican a datos de la región de Aragón, España, y se comparan sus resultados para evaluar su consistencia y eficacia.

El proyecto resalta la importancia de la reproducibilidad y replicabilidad en la investigación, proporcionando un flujo de trabajo completamente documentado que incluye preparación de datos, modelado, generación de mapas y análisis de resultados.

---

### Contenido

1. **Preparación de datos**
   - Lectura y transformación de shapefiles.
   - Creación de relaciones espaciales y vecinos.
2. **Modelado Espacial**
   - Definición de modelos jerárquicos bayesianos.
   - Configuración y ejecución de WinBUGS.
   - Ajuste de modelos con INLA, incluyendo efectos aleatorios (Besag-York-Mollié).
3. **Visualización**
   - Generación de mapas temáticos de RME y P(RME > 1).
   - Gráficos comparativos de resultados.
4. **Análisis**
   - Cálculo de correlaciones entre resultados de ambos enfoques.
   - Comparación de distribuciones y patrones espaciales.
5. **Reproducibilidad**
   - Uso de semillas para resultados consistentes.
   - Estructura clara de archivos y scripts.

---

### Requisitos

**Lenguaje:** R  
**Librerías necesarias:**
- `sf`  
- `spdep`  
- `R2WinBUGS`  
- `INLA`  
- `ggplot2`, `patchwork`, `viridis`  
- `gridExtra`, `RColorBrewer`  
- `leaflet`  
- `dplyr`, `ggthemes`, `latticeExtra`  

**Herramientas externas:**
- **WinBUGS:** Descargable desde [WinBUGS](http://www.mrc-bsu.cam.ac.uk/software/bugs/).

---

### Instalación

1. Clona este repositorio:  
   ```bash
   git clone https://https://github.com/monksam/TrabajoAragon
   cd TrabajoAragon
   ```
2. Instala las librerías requeridas en R:
   ```R
   install.packages(c("pacman", "sf", "spdep", "INLA", "ggplot2", "viridis", "dplyr"))
   pacman::p_load(sf, spdep, R2WinBUGS, INLA, ggplot2, leaflet, gridExtra)
   ```
3. Configura el entorno para ejecutar scripts:
   - Asegúrate de tener **WinBUGS** instalado y funcionando.
   - Guarda los datos en la carpeta `material/`.

---

### Ejecución

1. **Cargar y procesar datos:**
   Ejecuta los scripts de preparación de datos incluidos en `data_preparation.R`.
2. **Ajustar modelos:**
   - Ejecuta `winbugs_model.R` para el ajuste en WinBUGS.
   - Ejecuta `inla_model.R` para el ajuste en INLA.
3. **Visualizar resultados:**
   Los mapas y análisis se generan automáticamente y se guardan en la carpeta `output/`.

---

### Resultados

- Mapas de tasas de mortalidad ajustadas (RME) y probabilidades espaciales.
- Comparaciones entre metodologías (correlaciones, gráficos de dispersión).
- Evaluaciones de consistencia entre los enfoques.

---

### Contacto

**Autores:**  
- Diego Fernández Fernández
- Jorge Guitart Gil  
- Santiago Agustin Moncalero
  
Cualquier consulta puede dirigirse a los autores a través del sistema de *Issues* en este repositorio.
