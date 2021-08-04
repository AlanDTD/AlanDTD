# -*- coding: utf-8 -*-
"""
Created on Tue Aug  3 14:52:27 2021

@author: aland
"""

word_1 = input("Enter a word that you want to test \n").replace(" ","").lower()



if word_1 == word_1[::-1]:
    print("This word is a palindrome")
else:
    print("This word is not a palindrome")

#using lambda function

ispalindrome = lambda word_1: word_1.lower().replace(" ","") == word_1[::-1].lower().replace(" ","")
print(ispalindrome("Amora roma"))

