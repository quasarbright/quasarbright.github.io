'''this is before switching to sqlalchemy. IGNORE'''
import sqlite3
connection = sqlite3.connect('media.db')
cursor = connection.cursor()
cursor.execute('create table Song (id integer primary key not null, name text not null, bangericity real not null)')
cursor.execute('create table Tag (id integer primary key not null, name text not null)')
cursor.execute('create table Playlist (id integer primary key not null, name text not null)')
cursor.execute('create table SongTag (song_id integer not null, tag_id integer not null, primary key  (song_id, tag_id))')
cursor.execute('create table SongPlaylist (song_id integer not null, playlist_id integer not null, primary key (song_id, playlist_id))')
connection.commit()
cursor.close()
connection.close()
