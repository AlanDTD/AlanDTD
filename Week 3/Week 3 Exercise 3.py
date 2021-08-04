# -*- coding: utf-8 -*-
"""
Created on Tue Aug  3 15:51:32 2021

@author: aland
"""

list_1 = []
list_2 = []


print("Hi There please enter 2 list of numbers separated by space and of equal length")

#uses input via spaces 
list_1 = input("list 1 = ").split()
list_2 = input("list 2 = ").split()
list_3 = list_1 + list_2


#coverts numbers strings into integers
adjlist_1 = [int(i) for i in list_1 if i.isdigit()]
adjlist_2 = [int(i) for i in list_2 if i.isdigit()]



if len(adjlist_1) == len(adjlist_2):  #Lengths match then multiple
    for i in range(len(adjlist_1)):
        print(adjlist_1[i] * adjlist_2[i])
else:
    print("Length does not match") #Checks for length
    for i in list_3:
        if i.isdigit() == False: #Checks value is digits 
            print("this is due to a letter in the input")
            break

