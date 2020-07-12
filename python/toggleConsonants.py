def toggle(phrase):
    answer = ""
    for letter in phrase:
        if letter not in "aeiouAEIOU":
            answer = answer + letter.upper()
        else:
            answer = answer + letter.lower()
    return answer


print(toggle(input("Enter a phrase: ")))
