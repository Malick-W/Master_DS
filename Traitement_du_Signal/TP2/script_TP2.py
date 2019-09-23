#!/usr/bin/env python3
# -*- coding: utf-8 -*-



#######################################################################
# La fréquence de chaque note est précisée dans le tableau ci-dessous:
# Do1       Ré     Mi     Fa      Sol     La      Si      Do2
# 262Hz   294Hz  330Hz   349Hz   392Hz   440Hz   494Hz   523Hz

# lien: https://numerical-analysis.readthedocs.io/en/latest/Traitement_signal.html

############################Exercice 1###############################

import math 
import numpy as np
from matplotlib.pyplot import *

#question a:
def U(t, freq):
    s=127.5*math.sin(freq*2*math.pi*t)+127.5 #127.5 == 255/2
    return(int(s).to_bytes(1,byteorder="big"))  

hz = 44100
freq = 440
r_1 = [U(t,freq) for t in np.linspace(0,1,hz)]
r_2 = [U(t,(3*freq)/2) for t in np.linspace(0,1,hz)]
r= r_1 + r_2

file=open('la_1.pcm','wb')
for j in r:
    file.write(j)
file.close()


#question b:
#pour monter de 7 demi-tons, on multipli la fréquence par r
r = 1.05946
r_3 = [U(t,freq*7*r) for t in np.linspace(0,1,hz)]
r= r_1 + r_3

file=open('la_2.pcm','wb')
for j in r:
    file.write(j)
file.close()


############################Exercice 1###############################
# lien: https://www.deleze.name/marcel/physique/musique/Frequences.pdf

#question a:
# https://lecompositeur.com/theorie/frequence-des-notes-en-hertz

#Marcel Delèze présente une formule mathématique pour trouver la fréquence des notes 
#en partant du diapason 440. Cette formule fonctionne pour la gamme tempérée uniquement.                                       

piano = np.ones((12,8))
piano[8,:] = [440*i for i in range(8) if ]


