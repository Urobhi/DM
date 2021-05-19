##########
MODELOS=$*
MODEL_ARRAY=''
BUCKET_RUTA='bucket/tp-2'  # CAMBIAR A PIACERE

echo $MODELOS
for i in $MODELOS
do
bash -c "echo ~/$i.tif"
bash -c "gdalinfo -approx_stats ~/$i.tif"
if [ -f "/home/pmassaro/$i.tif" ]
then echo "La imagen ya existe!"
else
	echo "La imagen no existe, prediciendo"
    	~/OTB-7.2.0-Linux64/bin/otbcli_ImageClassifier -in ~/final_image.tif \
                       -imstat results/image_statistics.xml \
                       -model results/$i.txt \
                       -out ~/$i.tif
fi	
   MODEL_ARRAY="$MODEL_ARRAY ~/$i.tif"
done
echo "Las imagenes usadas van a ser:"
echo $MODEL_ARRAY

echo "Generando ensamble"
bash -c "~/OTB-7.2.0-Linux64/bin/otbcli_FusionOfClassifications -il  $MODEL_ARRAY \
                                -method         majorityvoting \
                                -nodatalabel    0 \
                                -undecidedlabel 10 \
                                -out            ~/ensamble.tif"


echo "Aplicando mascara de INTA"


gdal_calc.py \
-A ~/ensamble.tif \
--A_band=1 \
-B ~/$BUCKET_RUTA/adicionales/mask_inta.tif \
--B_band=1 \
--calc="((B==1)*A)+((B==0)*0)" \
--outfile ~/inta_temp_ensamble.tif

gdal_translate -ot UInt32 \
~/inta_temp_ensamble.tif \
~/inta_ensamble.tif

rm ~/inta_temp_ensamble.tif

echo "separando cultivos"
gdal_calc.py \
-A ~/inta_ensamble.tif  \
--A_band=1 \
--calc="(((A==2)*1)+((A!=2)*0))" \
--outfile ~/ensamble.soja.tif

gdal_calc.py \
-A ~/inta_ensamble.tif  \
--A_band=1 \
--calc="(((A==1)*1)+((A!=1)*0))" \
--outfile ~/ensamble.maiz.tif

echo "separando deptos"
gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_roca.shp\
        -crop_to_cutline  ~/ensamble.soja.tif\
        ~/inta_pred_ensamble.soja.roca.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_roca.shp\
        -crop_to_cutline  ~/ensamble.maiz.tif\
        ~/inta_pred_ensamble.maiz.roca.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_villegas.shp\
        -crop_to_cutline  ~/ensamble.soja.tif\
        ~/inta_pred_ensamble.soja.villegas.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_villegas.shp\
        -crop_to_cutline  ~/ensamble.maiz.tif\
        ~/inta_pred_ensamble.maiz.villegas.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_rsp.shp\
        -crop_to_cutline  ~/ensamble.soja.tif\
        ~/inta_pred_ensamble.soja.rsp.tif

gdalwarp -cutline ~/$BUCKET_RUTA/adicionales/departamentos/departamentos_rsp.shp\
        -crop_to_cutline  ~/ensamble.maiz.tif\
        ~/inta_pred_ensamble.maiz.rsp.tif

rm ~/ensamble.maiz.tif
rm ~/ensamble.soja.tif

echo 'contando hectareas'
python3 contar_hectareas_en_raster.py ensamble
