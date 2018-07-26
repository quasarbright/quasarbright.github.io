import sqlite3
connection = sqlite3.connect('media.db')
cursor = connection.cursor()

def openDB():
    global connection
    global cursor
    connection = sqlite3.connect('media.db')
    cursor = connection.cursor()
def closeDB():
    connection.commit()
    cursor.close()
    connection.close()

def DBFunc(f):
    def wrapper(*args, **kwargs):
        openDB()
        ans = f(*args, **kwargs)
        closeDB()
        return ans
    return wrapper

@DBFunc
def getSongs():
    return cursor.execute('select name from Song').fetchall()

def getTags():
    return cursor.execute('select name from Tag').fetchall()

def getPlaylists():
    return cursor.execute('select name from Playlist').fetchall()
