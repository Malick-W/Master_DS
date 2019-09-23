#!/usr/bin/env python3
# -*- coding: utf-8 -*-



#######################################################################
# La fréquence de chaque note est précisée dans le tableau ci-dessous:
# Do1       Ré     Mi     Fa      Sol     La      Si      Do2
# 262Hz   294Hz  330Hz   349Hz   392Hz   440Hz   494Hz   523Hz


# lien: https://numerical-analysis.readthedocs.io/en/latest/Traitement_signal.html



############################Exercice 1###############################

#question a,b,c

import math 
import numpy as np
from matplotlib.pyplot import *

# (1000).to_bytes(2,byteorder="big")
# (2000).to_bytes(2,byteorder="big")

# (1000).to_bytes(2,byteorder="big",signed="False")
# (2000).to_bytes(2,byteorder="big",signed="False")

# (1000).to_bytes(2,byteorder="little",signed="False")
# (2000).to_bytes(2,byteorder="little",signed="False")


#question d:
def u(t):
    s=127.5*math.sin(440*2*math.pi*t)+127.5 #127.5 == 255/2
    return(int(s).to_bytes(1,byteorder="big"))  

print(u(44100))


#question e:
hz = 44100
r = [u(t) for t in np.linspace(0,1,hz)]

# va = lambda hz : list(map(u,np.linspace(0,1,hz))) #hz est 44100
# r = va(44100)

# question f:
file=open('la.pcm','wb')
for j in r:
    file.write(j)
file.close()


# question g:
#cvlc --demux=rawaud --rawaud-channels 1 --rawaud-samplerate 44100 --rawaud-fourcc "u8  " la.pcm


# question h:
# Un entier relatif positif ou nul sera représenté en binaire (base 2) comme un entier naturel,
# à la seule différence que le bit de poids fort (le bit situé à l'extrême gauche) représente le
# signe. Il faut donc s'assurer pour un entier positif ou nul qu'il est à zéro (0 correspond à un
# signe positif, 1 à un signe négatif). Ainsi, si on code un entier naturel sur 4 bits, le nombre
# le plus grand sera 0111 (c'est-à-dire 7 en base décimale).
# 	• Sur 8 bits (1 octet), l'intervalle de codage est [-128, 127]
# 	• Sur 16 bits (2 octets), l'intervalle de codage est [-32768, 32767].
# 	• Sur 32 bits (4 octets), l'intervalle de codage est [-2147483648, 2147483647].
# D'une manière générale le plus grand entier relatif positif codé sur n bits sera 2^(n-1)-1. 

# question i:
# n = 2
# def u2(t):
#     s=127.5*math.sin((2**(8*n)/n)*440*2*math.pi*t)+127.5 #127.5 est 255/2
#     return (int(s).to_bytes(2,byteorder="big",signed='False'))
    
# hz = 44100
# r = [u2(t) for t in np.linspace(0,1,hz)]

# file=open('la_16bits.pcm','wb')
# for j in r:
#     file.write(j)
# file.close()



############################Exercice 2###############################

#question a:
A = 0.5 # représente le volume du signal

def u3(t):
    s=A*(127.5*math.sin(440*2*math.pi*t)+127.5) #127.5 == 255/2
    return(int(s).to_bytes(1,byteorder="big")) 

#question e:
hz = 44100
r = [u3(t) for t in np.linspace(0,1,hz)]


file=open('la_2.pcm','wb')
for j in r:
    file.write(j)
file.close()


#question b:
# on double la fréquence (440*2)pour avoir une octave plus aigue
def u4(t):
	freq = 440*2
	s=127.5*math.sin(freq*2*math.pi*t)+127.5 #127.5 == 255/2
	return(int(s).to_bytes(1,byteorder="big"))  

r = [u4(t) for t in np.linspace(0,1,hz)]


file=open('la_aigue.pcm','wb')
for j in r:
    file.write(j)
file.close()

#question c:
def u5(t):
	s = math.sin(392*2*math.pi*t)
	s=127.5*s+127.5 #127.5 == 255/2
	return(int(s).to_bytes(1,byteorder="big"))  

hz = 44100

r = [u(t) + u5(t) for t in np.linspace(0,1,hz)]

file=open('la_sol.pcm','wb')
for j in r:
	file.write(j)
file.close()


# La fréquence de chaque note est précisée dans le tableau ci-dessous:
# Do1       Ré     Mi     Fa      Sol     La      Si      Do2
# 262Hz   294Hz  330Hz   349Hz   392Hz   440Hz   494Hz   523Hz


def U(t, freq):
    s=127.5*math.sin(freq*2*math.pi*t)+127.5 #127.5 == 255/2
    return(int(s).to_bytes(1,byteorder="big"))  

hz = round(44100/4)
r = []

freq = [262, 294, 330, 349, 392, 440, 494, 523]

for i in freq:
	r += [U(t,i) for t in np.linspace(0,1,hz)]

file=open('musique.pcm','wb')
for j in r:
    file.write(j)
file.close()


#question d:

def simult(t,phi):
	s = math.sin(440*2*math.pi*t) + math.sin(440*2*math.pi*t + phi)
	s =16383.5*s+16383.5*2 #16383.5 = (2^15-1)/2
	return(int(s).to_bytes(2,byteorder="big"))  

hz = 44100
phi = math.pi # les deux fonctions s'annulent avec cette phase.
r = [simult(t,phi) for t in np.linspace(0,1,hz)]

file=open('la_phase.pcm','wb')
for j in r:
	file.write(j)
file.close()




############################Exercice 3###############################















