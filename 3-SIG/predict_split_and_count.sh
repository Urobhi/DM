IMAGEN_ENTRADA=$1
BUCKET_RUTA=$2

echo $IMAGEN_ENTRADA
echo $BUCKET_RUTA

#source ~/OTB-7.2.0-Linux64/otbenv.profile;

~/OTB-7.2.0-Linux64/bin/otbcli_ImageClassifier -in ~/final_image.tif \
                       -imstat results/image_statistics.xml \
                       -model results/$IMAGEN_ENTRADA.txt \
                       -out ~/$IMAGEN_ENTRADA.tif

gdal_calc.py \
-A ~/$IMAGEN_ENTRADA.tif \
--A_band=1 \
-B ~/$BUCKET_RUTA/adicionales/mask_inta.tif \
--B_band=1 \
--calc="((B==1)*A)+((B==0)*0)" \
--outfile ~/inta_temp_$IMAGEN_ENTRADA.tif

gdal_translate -ot UInt32 \
~/inta_temp_$IMAGEN_ENTRADA.tif \
~/inta_$IMAGEN_ENTRADA.tif

rm ~/inta_temp_$IMAGEN_ENTRADA.tif


gdal_calc.py \
-A ~/inta_$IMAGEN_ENTRADA.tif  \
--A_band=1 \
--calc="(((A==2)*1)+((A!=2)*0))" \
--outfile ~/$IMAGEN_ENTRADA.soja.tif

gdal_calc.py \
-A ~/inta_$IMAGEN_ENTRADA.tif  \
--A_band=1 \
--calc="(((A==1)*1)+((A!=1)*0))" \
--outfile ~/$IMAGEN_ENTRADA.maiz.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_roca.shp\
       	-crop_to_cutline  ~/$IMAGEN_ENTRADA.soja.tif\
       	~/inta_pred_$IMAGEN_ENTRADA.soja.roca.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_roca.shp\
       	-crop_to_cutline  ~/$IMAGEN_ENTRADA.maiz.tif\
       	~/inta_pred_$IMAGEN_ENTRADA.maiz.roca.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_villegas.shp\
       	-crop_to_cutline  ~/$IMAGEN_ENTRADA.soja.tif\
       	~/inta_pred_$IMAGEN_ENTRADA.soja.villegas.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_villegas.shp\
       	-crop_to_cutline  ~/$IMAGEN_ENTRADA.maiz.tif\
       	~/inta_pred_$IMAGEN_ENTRADA.maiz.villegas.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_rsp.shp\
       	-crop_to_cutline  ~/$IMAGEN_ENTRADA.soja.tif\
       	~/inta_pred_$IMAGEN_ENTRADA.soja.rsp.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_rsp.shp\
       	-crop_to_cutline  ~/$IMAGEN_ENTRADA.maiz.tif\
       	~/inta_pred_$IMAGEN_ENTRADA.maiz.rsp.tif

rm ~/$IMAGEN_ENTRADA.maiz.tif
rm ~/$IMAGEN_ENTRADA.soja.tif

python3 contar_hectareas_en_raster.py $IMAGEN_ENTRADA
