#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import math 
import numpy as np
from matplotlib.pyplot import *

(1000).to_bytes(2,byteorder="big")
(2000).to_bytes(2,byteorder="big")
(1000).to_bytes(2,byteorder="big",signed="False")
(2000).to_bytes(2,byteorder="big",signed="False")
(1000).to_bytes(2,byteorder="little",signed="False")
(2000).to_bytes(2,byteorder="little",signed="False")


#f=open('data.pcm','wb')

#def u(t):
#    for i in range(t): #44100 est t
#        s=127.5*math.sin(440*2*math.pi/44100)+127.5 #127.5 est 253/2
#        f.write(int(s).to_bytes(1,byteorder="big"))
#
#
#u(44100)
#f.close()

def u2(t):
    s=127.5*math.sin((2**(8*n)/n)440*2*math.pi*t)+127.5 #127.5 est 255/2
    return(int(s).to_bytes(2,byteorder="big",signed='False'))
    
u2(44100)


def u(t):
    s=127.5*math.sin(440*2*math.pi*t)+127.5 #127.5 est 255/2
    return(int(s).to_bytes(1,byteorder="big"))  
    
u(44100)
import math 
import numpy as np
from matplotlib.pyplot import *

(1000).to_bytes(2,byteorder="big")
(2000).to_bytes(2,byteorder="big")
(1000).to_bytes(2,byteorder="big",signed="False")
(2000).to_bytes(2,byteorder="big",signed="False")
(1000).to_bytes(2,byteorder="little",signed="False")
(2000).to_bytes(2,byteorder="little",signed="False")


#f=open('data.pcm','wb')

#def u(t):
#    for i in range(t): #44100 est t
#        s=127.5*math.sin(440*2*math.pi/44100)+127.5 #127.5 est 253/2
#        f.write(int(s).to_bytes(1,byteorder="big"))
#
#
#u(44100)
#f.close()

va=lambda hz : list(map(u,np.linspace(0,1,hz))) #hz est 44100
vb=lambda hz : list(map(u2,np.linspace(0,1,hz)))

l1=va(44100)
l2=vb(44100)

#f=open('data.pcm','rb')
#print(f.read())
#f.close()

file=open('la.pcm','wb')
for j in l1:
    file.write(j)
file.close()

