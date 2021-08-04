word_1 = "Belinda thinks this is a good exercise to try"
word_2 = ('a','e','i','o','u','A','E','I','O','U')

for i in word_2:
    word_1 = word_1.replace(i,'')

print(word_1, '\n')

        