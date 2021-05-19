######################

TRAIN_PATH=$1
TEST_PATH=$2
IMAGE=$3
IMAGE_PATH=$4

echo 'Train polygon'
bash -c "source ~/OTB-7.2.0-Linux64/otbenv.profile; otbcli_PolygonClassStatistics  -in $IMAGE_PATH/$IMAGE.tif \
                        -vec ../adicionales/verdad_campo/4326/$TRAIN_PATH.shp\
                        -field  cultivo_en \
                        -out results/$IMAGE\_$TRAIN_PATH\_classes_stat.xml"


echo 'train sample selection'
bash -c "source ~/OTB-7.2.0-Linux64/otbenv.profile; otbcli_SampleSelection -in $IMAGE_PATH/$IMAGE.tif \
                       -vec ../adicionales/verdad_campo/4326/$TRAIN_PATH.shp\
                       -instats results/$IMAGE\_$TRAIN_PATH\_classes_stat.xml \
                       -field cultivo_en \
                       -strategy all \
                       -outrates results/$IMAGE\_$TRAIN_PATH\_rates.csv \
                       -out results/$IMAGE\_$TRAIN_PATH\_samples.sqlite"

echo 'train sample extraction'
bash -c "source ~/OTB-7.2.0-Linux64/otbenv.profile; otbcli_SampleExtraction -in $IMAGE_PATH/$IMAGE.tif \
                        -vec results/$IMAGE\_$TRAIN_PATH\_samples.sqlite \
                        -outfield prefix \
                        -outfield.prefix.name band_ \
                        -field cultivo_en"

echo 'test polygon'
bash -c "source ~/OTB-7.2.0-Linux64/otbenv.profile; otbcli_PolygonClassStatistics  -in $IMAGE_PATH/$IMAGE.tif \
                        -vec ../adicionales/verdad_campo/4326/$TEST_PATH.shp\
                        -field  cultivo_en \
                        -out results/$IMAGE\_$TEST_PATH\_classes_stat.xml"

echo 'test Sample selection'
bash -c "source ~/OTB-7.2.0-Linux64/otbenv.profile; otbcli_SampleSelection -in $IMAGE_PATH/$IMAGE.tif \
                       -vec ../adicionales/verdad_campo/4326/$TEST_PATH.shp\
                       -instats results/$IMAGE\_$TEST_PATH\_classes_stat.xml \
                       -field cultivo_en \
                       -strategy all \
                       -outrates results/$IMAGE\_$TEST_PATH\_.rates.csv \
                       -out results/$IMAGE\_$TEST_PATH\_samples.sqlite"

echo 'test Sample extraction'
bash -c "source ~/OTB-7.2.0-Linux64/otbenv.profile; otbcli_SampleExtraction -in $IMAGE_PATH/$IMAGE.tif \
                        -vec results/$IMAGE\_$TEST_PATH\_samples.sqlite \
                        -outfield prefix \
                        -outfield.prefix.name band_ \
                        -field cultivo_en"
echo "Image statistics"
bash -c "source ~/OTB-7.2.0-Linux64/otbenv.profile; otbcli_ComputeImagesStatistics -il  $IMAGE_PATH/$IMAGE.tif \
                               -out results/$IMAGE\_image_statistics.xml"
