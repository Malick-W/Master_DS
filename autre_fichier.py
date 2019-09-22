# /usr/bin/env python3
# -*- coding: utf-8 -*-

# Modification du fichier blabla
# depuis ma machine 


import threading
import time

## Exercice 1
# question1

class MaTache(threading.Thread):
    def __init__(self, n,m,s):
        threading.Thread.__init__(self)
                    self.n = n
                            self.m = m
                                    self.s = s

                                        def run(self):
                                                while True:
                                                        	for i in range(self.n):
                                                        	        		print(self.m)
                                                        	        		        	time.sleep(self.s)

                                                        	        		        	if __name__ == '__main__':
                                                        	        		        	  matache = MaTache(3,"Hello World", 4)
                                                        	        		        	    matache.start()


                                                        	        		        	    
