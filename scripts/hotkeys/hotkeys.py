#!/usr/bin/env python3
'''
this script can detect keypress combinations and
execute a desired script when detected.
On Mac OS, the command key cannot be used with
any character keys other than space, "`", and "-"
within a combination.
NOTE: must be ran through SUDO
'''

import sys
import subprocess
import re
from pynput import keyboard

#user-made hotkey string => terminal command
#like {"ctrl+shift+right => pip install bigbrain"}

SPECIAL_KEY_MAP = {
    "esc":[keyboard.Key.esc],
    "alt":[keyboard.Key.alt, keyboard.Key.alt_l, keyboard.Key.alt_r, keyboard.Key.alt_gr],
    "backspace":[keyboard.Key.backspace],
    "caps_lock":[keyboard.Key.caps_lock],
    "cmd":[keyboard.Key.cmd, keyboard.Key.cmd_l, keyboard.Key.cmd_r],
    "windows":[keyboard.Key.cmd, keyboard.Key.cmd_l, keyboard.Key.cmd_r],
    "ctrl":[keyboard.Key.ctrl, keyboard.Key.ctrl_l, keyboard.Key.ctrl_r],
    "del":[keyboard.Key.delete],
    "down":[keyboard.Key.down],
    "up":[keyboard.Key.up],
    "left":[keyboard.Key.left],
    "right":[keyboard.Key.right],
    "enter":[keyboard.Key.enter],
    "shift":[keyboard.Key.shift, keyboard.Key.shift_l, keyboard.Key.shift_r],
    "space":[keyboard.Key.space],
    "tab":[keyboard.Key.tab],
    "f1":[keyboard.Key.f1],
    "f2":[keyboard.Key.f2],
    "f3":[keyboard.Key.f3],
    "f4":[keyboard.Key.f4],
    "f5":[keyboard.Key.f5],
    "f6":[keyboard.Key.f6],
    "f7":[keyboard.Key.f7],
    "f8":[keyboard.Key.f8],
    "f9":[keyboard.Key.f9],
    "f10":[keyboard.Key.f10],
    "f11":[keyboard.Key.f11],
    "f12":[keyboard.Key.f12],
    "f13":[keyboard.Key.f13],
    "f14":[keyboard.Key.f14],
    "f15":[keyboard.Key.f15],
    "f16":[keyboard.Key.f16],
    "f17":[keyboard.Key.f17],
    "f18":[keyboard.Key.f18],
    "f19":[keyboard.Key.f19],
    "f20":[keyboard.Key.f20]
}


def strToKeyList(keyStr):
    '''convert a key string to a key object
    ex: strToKey('f10') -> keyboard.Key.f10
    '''
    keyStr = keyStr.lower()
    try:
        return SPECIAL_KEY_MAP[keyStr]
    except KeyError:
        # it's not a special key
        # validate
        if len(keyStr) == 1:
            return [keyboard.KeyCode.from_char(keyStr)]
        else:
            raise Exception("{0} not a valid keyStr".format(keyStr))


def parseCombinationStr(combinationStr):
    '''this parses the "ctrl+shift+right" part
    returns a frozenset of tuples of Key objects
    '''
    keyStrs = combinationStr.split('+')
    combination = set([])
    for keyStr in keyStrs:
        combination.add(tuple(strToKeyList(keyStr)))
    return frozenset(combination)


def parseHotkeyStr(hotKeyStr):
    combinationStr, commandStr = hotKeyStr.split(' => ', 1)
    combination = parseCombinationStr(combinationStr)
    return (combination, commandStr,)


def isCombination(current_combination, test_combination):
    '''compares the current key combination
    to a set of tuples of keys
    '''
    remaining_combination = set(test_combination.copy())
    # duplicate set of key tuples
    for key in current_combination:
        for tup in test_combination:
            # you actually want test_combination, not remaining_combination
            # so you don't remove from what you're looping through
            if key in tup:
                remaining_combination.remove(tup)
    if len(remaining_combination) != 0:
        return False
    return True




if __name__ == '__main__':
    hotkeyStrs = []
    with open("myHotkeys.txt", 'r') as f:
        hotkeyStrs = f.readlines()
    if len(sys.argv) == 2:
        with open(sys.argv[1], 'r') as f:
            hotkeyStrs = f.readlines()
    hotkeys = [parseHotkeyStr(e) for e in hotkeyStrs]
    print(hotkeys)
    current_combination = set([])
    def on_press(key):
        # print(key)
        current_combination.add(key)
        print(current_combination)
        if isCombination(current_combination, parseCombinationStr('ctrl+shift+esc')):
            sys.exit()
        for hotkey in hotkeys:
            if isCombination(current_combination, hotkey[0]):
                print('executing', hotkey[1])
                subprocess.call(hotkey[1], shell=True)


    def on_release(key):
        # print(current_combination)
        if key in current_combination:
            current_combination.remove(key)


    # Collect events until released
    with keyboard.Listener(on_press=on_press, on_release=on_release) as listener:
        listener.join()
