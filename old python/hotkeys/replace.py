#!/usr/bin/python3
import pynput
import sys

keyboard = pynput.keyboard.Controller()


current_word = 's'
target = 'mike'
replacement = 'rein god'

def on_press(key):
	try:
		current_word += key.char
	except UnboundLocalError:
		current_word = ''
	try:
		current_word += key.char
	except AttributeError:
		if key == pynput.keyboard.Key.esc:
			sys.exit()
	if current_word == target:
		# for x in range(len(target)):
		# 	keyboard.press(pynput.keyboard.Key.backspace)
		keyboard.type('rein god')
	elif current_word not in target:
		print(current_word)
		current_word = ''
	else:
		print(current_word)
with pynput.keyboard.Listener(on_press=on_press) as listener:
	current_word = ''
	listener.join()
