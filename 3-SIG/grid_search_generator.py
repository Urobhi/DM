import itertools
import subprocess
import glob
import os
import sys

diccionario_model_hyp= {
#    'rf':{
#        'classifier.rf.max':(3,5),
#        'classifier.rf.min':(1000,1250,1500,1750,2000,2250),
#        'classifier.rf.nbtrees':(50,75,100,125,150)
#    },
#    'dt':{
#        'classifier.dt.max':(3,5,8,10,13),
#        'classifier.dt.min': (5,10,15,25,50),
#        'classifier.dt.ra':(0.005,0.01,0.05,0.1)
#    },
#  'libsvm':{
#         'classifier.libsvm.k':('rbf','poly','sigmoid'),
#         'classifier.libsvm.c':(0.75,1,1.25)
#     },
  'boost':{
        'classifier.boost.t':('discrete','real','gentle','logit'),
        'classifier.boost.w':(50,100,150,200),
        'classifier.boost.r':(1,0.5,0.1),
        'classifier.boost.m':(1,3,5,10)
    },
#   'ann':{
#         'classifier.ann.t':('back','reg'),
#         'classifier.ann.sizes':('24 12 6','45 45 45'),
#     },
#   'knn':{
#         'classifier.knn.k':(20,30,40,50,80,120,200,500)
#     },
}

train=sys.argv[1]
test= sys.argv[2]

for model in diccionario_model_hyp.keys():
    parameters =list(diccionario_model_hyp[model].values())
    parameters_name = list(diccionario_model_hyp[model].keys())
    parameters=list(itertools.product(*parameters))
    for tupla in parameters:
        parameters_commands_list = []
        for i in range(0,len(tupla)):
            parameters_commands_list.append(f'-{parameters_name[i]} {tupla[i]} ')
        parameters_command=(''.join(parameters_commands_list))
        command_to_execute =f"""bash -c 'source ~/OTB-7.2.0-Linux64/otbenv.profile; otbcli_TrainVectorClassifier -io.vd results/{train}_samples.sqlite \ -io.stats results/image_statistics.xml -cfield cultivo_en \ -classifier {model} {parameters_command} -valid.vd results/{test}_samples.sqlite \ -io.out results/modelo_{model}_{'_'.join([str(i) for i in tupla])}.txt -io.confmatout results/confusion_matrix_{model}_{'_'.join([str(i) for i in tupla])}.csv -feat band_0 band_1 band_2 band_3 band_4 band_5 band_6 \  band_7 band_8 band_9 band_10 band_11 band_12 band_13 band_14 band_15 band_16 band_17 band_18 band_19 band_20 \ band_21 band_22 band_23 band_24 band_25 band_26 \ band_27 band_28 band_29 band_30 band_31 band_32 band_33 band_34 band_35 band_36 band_37 band_38 band_39 band_40 \ band_41 band_42 band_43 band_44' |  grep -o 'Kappa.*\|results/modelo.*\|Precision of class.*\|Recall of class.*\|F-score of class.*' >> grid_search.txt """
        print(command_to_execute)
