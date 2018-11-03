from random import randint
chars = 'abcdefghijklmnopqrstuvwxyz '

def random_letter():
    ind = randint(0,len(chars)-2)
    return chars[ind]
def random_char():
    ind = randint(0,len(chars)-1)
    return chars[ind]
def random_word(max_len=None):
    word = ''
    letter = random_letter()
    if max_len:
        while letter != ' ' and len(word)<max_len:
            word += letter
            letter = random_char()
    else:
        while letter != ' ':
            word += letter
            letter = random_char()
    return word

max_iterations = 100000
current_iterations = 1

with open('words.csv','w') as f:
    f.write("words\n")
    while current_iterations <= max_iterations:
        f.write(random_word())
        if current_iterations < max_iterations:
            f.write('\n')
        current_iterations += 1
