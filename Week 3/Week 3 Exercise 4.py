# -*- coding: utf-8 -*-
"""
Created on Wed Aug  4 21:34:03 2021

@author: aland
"""


print("Hi There please enter 2 list of numbers separated by space and of equal length")
list_1 = input("list 1 = ").split()
list_2 = input("list 2 = ").split()
list_3 = list_1 + list_2

adjlist_1 = [int(i) for i in list_1 if i.isdigit()]
adjlist_2 = [int(i) for i in list_2 if i.isdigit()]

if len(adjlist_1) == len(adjlist_2):
    for i in range(len(adjlist_1)):
        print(adjlist_1[i] * adjlist_2[i])
else:
    print("Length does not match")
    for i in list_3:
        if i.isdigit() == False:
            print("this is due to a letter in the input")
            break




player_1 = input("Player 1 - Enter a 5 numbers between 1-10")
player_2 = input("Player 1 - Enter a 5 numbers between 1-10")