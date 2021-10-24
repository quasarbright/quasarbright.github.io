"""
take in a message and a code text, and shift each letter of the message
by the corresponding letter in the code text
"""

alphabet = set('qwertyuiopasdfghjklzxcvbnm')
letters = 'abcdefghijklmnopqrstuvwxyz'
itoc = '\\' + letters  # to start at 1
ctoi = {c: i+1 for (i,c) in enumerate(letters)}

def shift_up(c: str, delta: str) -> str:
    i = ctoi[c] + ctoi[delta]
    if i > 26:
        i -= 26
    return itoc[i]


def shift_down(c: str, delta: str) -> str:
    i = ctoi[c] - ctoi[delta]
    if i < 1:
        i += 26
    return itoc[i]


def clean(code: str):
    code = code.lower()
    code = filter(lambda c: c in alphabet, code)
    code = ''.join(code)
    return code


def encode_decode(message, code, shift):
    message = message.lower()
    code = clean(code)
    i = 0  # msg index
    j = 0  # code index
    answer = []
    while i < len(message) and j < len(code):
        c = message[i]
        delta = code[j]
        if c in alphabet:
            answer.append(shift(c, delta))
            i += 1
            j += 1
        else:
            answer.append(c)
            i += 1
    answer = ''.join(answer)
    return answer

def encode(message, code):
    return encode_decode(message, code, shift_up)


def decode(message, code):
    return encode_decode(message, code, shift_down)


def encode_decode_io(tranform):
    with open('zodiac_message.txt', 'r') as f:
        message = f.read()
    with open('zodiac_code.txt', 'r') as f:
        code = f.read()
    print(tranform(message, code))

def decode_io():
    encode_decode_io(decode)


def encode_io():
    encode_decode_io(encode)


if __name__ == '__main__':
    encode_io()