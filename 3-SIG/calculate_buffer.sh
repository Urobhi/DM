VERDAD_CAMPO=$1
BUCKET=$2
DIAMETRO=$3


echo $1
echo $2
echo $3

cd ~/$BUCKET/adicionales/verdad_campo
echo ' Proyeccion plana'
ogr2ogr -s_srs EPSG:4326 \
-t_srs EPSG:32721 \
32721/$VERDAD_CAMPO.shp \
4326/$VERDAD_CAMPO.shp

echo 'calculando buffer'
# Calculo buffer
ogr2ogr -f "ESRI Shapefile" -dialect SQLITE \
	-sql "SELECT c.pk_uid,c.in1,c.cultivo,c.cultivo_en, ST_Buffer(c.geometry, $DIAMETRO) FROM $VERDAD_CAMPO c" \
	32721/$VERDAD_CAMPO"_$DIAMETRO.shp" \
	32721/$VERDAD_CAMPO.shp

echo 'volviendo'
# VOLVER a la misma que los rasters
ogr2ogr -s_srs EPSG:32721 \
-t_srs EPSG:4326 \
4326/$VERDAD_CAMPO"_$DIAMETRO.shp" \
32721/$VERDAD_CAMPO"_$DIAMETRO.shp"
