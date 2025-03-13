# Instalación de R y RStudio

El presente README es un resumen de los pasos principales para la instalación de R y RStudio en las diferentes plataformas. Para los usuarios de Windows se dispone de un [**PDF en este mismo repositorio**](https://github.com/BioinformaticaIMIBIC/r-stats/blob/main/C%C3%B3mo_instalar_R_y_RStudio.pdf) con los pasos más detallados.

## Instalación de R

1. **Descargar R**
   - Ve al sitio oficial de CRAN: [https://cran.r-project.org/](https://cran.r-project.org/)
   - Selecciona tu sistema operativo:
     - **Windows**: Haz clic en "Download R for Windows" y luego en "base".
     - **Mac**: Haz clic en "Download R for macOS". Descarga el archivo [R-4.4.3-arm64.pkg](https://cran.r-project.org/bin/macosx/big-sur-arm64/base/R-4.4.3-arm64.pkg) que verás en el apartado "Latest release". La versión se actualiza muy frecuentemente, se recomienda acceder al primer enlace para disponer de la última. Instala el archivo descargado en el paso anterior. Una vez hecho esto vuelve a comprobar tu versión de R para ver si se ha actualizado.
     - **Linux**: Sigue las instrucciones específicas para tu distribución.
   
2. **Instalar R**
   - Abre el archivo descargado y sigue las instrucciones del instalador. En la página principal de este repositorio cuentas con un PDF para seguir los pasos en su instalación
   - Acepta las configuraciones predeterminadas a menos que necesites ajustes específicos.

## Instalación de RStudio

1. **Descargar RStudio**
   - Ve a la página oficial de RStudio: [https://posit.co/download/rstudio-desktop/](https://posit.co/download/rstudio-desktop/)
   - Descarga la versión gratuita de RStudio Desktop para tu sistema operativo.

2. **Instalar RStudio**
   - Abre el archivo descargado y sigue las instrucciones del instalador.
   - RStudio detectará automáticamente la instalación de R.
   - Abajo del todo verás un apartado que dice "macOs 13+" y un link para descargar que dice [
RStudio-2024.12.1-563.dmg](https://download1.rstudio.org/electron/macos/RStudio-2024.12.1-563.dmg). Descarga e instala este archivo. De nuevo, se recomienda acceder al primer enlace para disponer de la última versión.

## Verificación de la instalación

Para comprobar que todo funciona correctamente:

1. Abre **RStudio**.
2. En la consola, escribe lo siguiente y presiona Enter:
   ```r
   version
   ```
   Esto mostrará la versión instalada de R.

Si ves la versión de R sin errores, la instalación se ha realizado correctamente.

---
