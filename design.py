# -*- coding: utf-8 -*-
"""
Created on Mon Jul 31 15:45:17 2017

@author: Martin Vasilev
"""

import os

if not os.path.exists('design'):
    os.makedirs('design')

for ID in range(1, 49):

#ID= 1
	nsent= 120
	ncond= 2
	npos= 4 # num of target word positions
	#nlist= 3 # number of fully counter-balanced lists
	
	full_list= npos*ncond
	#nsub= nlist*full_list
	
	item= range(1,nsent+1)
	
	S1= [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47]
	S2= [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48]
	
	P1= [1, 2, 9, 10, 17, 18, 25, 26, 33, 34, 41, 42]# 3, 13, 14, 15, 25, 26, 27, 37, 38, 39]
	P2= [3, 4, 11, 12, 19, 20, 27, 28, 35, 36, 43, 44] #6, 16, 17, 18, 28, 29, 30, 40, 41, 42]
	P3= [5, 6, 13, 14, 21, 22, 29, 30, 37, 38, 45, 46]#9, 19, 20, 21, 31, 32, 33, 43, 44, 45]
	P4= [7, 8, 15, 16, 23, 24, 31, 32, 39, 40, 47, 48]# 12, 22, 23, 24, 34, 35, 36, 46, 47, 48]
	
	
	if ID in S1:
		sound= ["STD"]*int((nsent/ncond)) + ["DEV"]*int((nsent/ncond))	
		
	if ID in S2:
		sound= ["DEV"]*int((nsent/ncond)) + ["STD"]*int((nsent/ncond))
		
	####
	
	if ID in P1:
		pos= [2, 3, 4, 5]* int(nsent/npos)
	
	if ID in P2:
		pos= [3, 4, 5, 2]* int(nsent/npos)
		
	if ID in P3:
		pos= [4, 5, 2, 3]* int(nsent/npos)
		
	if ID in P4:
		pos= [5, 2, 3, 4]* int(nsent/npos)
	
	
	if ID%2==1: # odd numbers
		delay= [0,120]*(nsent/2);
	else: # even numbers
		delay= [120, 0]*(nsent/2);
	
	c= list(zip(item, sound, pos, delay))
	#matching = [s for s in c if s[1]=="SLC"]
	not_matching= [s for s in c if s[1]!="SLC"]
	
	STD = [i for i, s in enumerate(not_matching) if 'STD' in s[1]]
	first= [not_matching[STD[0]]]+ [not_matching[STD[1]]]+ [not_matching[STD[2]]]
	del not_matching[STD[0]]
	del not_matching[STD[1]-1]
	del not_matching[STD[2]-2]
	
	# randomise sounds' block:
	from random import shuffle
	shuffle(not_matching)
	#shuffle(matching)
	
	pract= [(121, 'PRAC', 1, 0), (122, 'PRAC', 1, 0), (123, 'PRAC', 1, 0), \
	        (124, 'PRAC', 1, 0), (125, 'PRAC', 1, 0), (126, 'PRAC', 1, 0)]
	shuffle(pract)
	
	design= pract+ first+not_matching	
	#if ID%2==1: # odd numbers
	#	B1= matching
	#	B2= not_matching
	#	design= pract+ B1+ first+B2
	#else: # even numbers
	#	B1= not_matching
	#	B2= matching
	#	design= pract+first+B1+B2
	
	print(design)
	
	thefile = open('Design/P'+ str(ID)+ '.txt', 'w')
	thefile.write("item sound pos delay\n") # columns
	
	for item in design:
	  thefile.write("%s %s %s %s\n" % item)
	thefile.close()
	
	
	# questions:
	# 13 between 1-40, 13 between 41-80, 13 between 81-120
		
