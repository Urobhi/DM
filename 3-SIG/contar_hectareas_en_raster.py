
# contar hectareas
import subprocess
import glob
import os
import rasterio as rio
import sys
cultivos =['maiz','soja']
departamentos =['villegas','roca','rsp']
modelo = sys.argv[1]
print(modelo)
home_dir = os.getenv('HOME')
for cultivo in cultivos:
    for departamento in departamentos:

        archivo = f'{home_dir}/inta_pred_{modelo}.{cultivo}.{departamento}.tif'
        raster = rio.open(archivo).read()
        raster_cultivo = raster[raster==1]
        suma_pixels = raster_cultivo.sum()
        print(f'hectareas de {cultivo} en {departamento} : {0.04*suma_pixels}' )

