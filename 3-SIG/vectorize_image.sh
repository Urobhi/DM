
############

~/OTB-7.2.0-Linux64/bin/otbcli_MeanShiftSmoothing \
-in ~/ensamble.tif  \
-fout ~/ensamble_smooth.tif \
-foutpos ~/ensamble_smooth_pos.tif \
-spatialr 5 \
-ranger 0.1 \
-maxiter 100

~/OTB-7.2.0-Linux64/bin/otbcli_LSMSSegmentation \
-in ~/ensamble_smooth.tif \
-inpos ~/ensamble_smooth_pos.tif  \
-out ~/ensamble_seg.tif \           
-spatialr 5 \
-ranger 0.1 \
-minsize 5 \
-tilesizex 1024 \
-tilesizey 1024

~/OTB-7.2.0-Linux64/bin/otbcli_LSMSVectorization\
        -in ~/ensamble.tif  \
        -inseg ~/ensamble_seg.tif \
        -out ~/ensamble_seg.shp
