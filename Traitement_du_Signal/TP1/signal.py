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


f=open('data.pcm','wb')

def u(t):
    for i in range(t): #44100 est t
        s=127.5*math.sin(440*2*math.pi/44100)+127.5 #127.5 est 253/2
        f.write(int(s).to_bytes(1,byteorder="big"))

u(44100)
f.close()

f=open('la.pcm','rb')
print(f.read())
f.close()

#cvlc --demux=rawaud --rawaud-channels 1 --rawaud-samplerate 44100 --rawaud-fourcc "u8  " la.pcm