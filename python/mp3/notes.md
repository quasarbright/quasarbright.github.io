# to view all song-tag pairs
```sql
select Song.name, Tag.name
from Song
inner join SongTag
on Song.id=SongTag.song_id
inner join Tag
on Tag.id=SongTag.tag_id
```
# to view a song's tags
```sql
select Tag.name
from Tag
inner join SongTag
on SongTag.tag_id=Tag.id
and SongTag.Song_id=$SONG_ID
```
# schemas:
### Song
name, tags, bangericity
### Tag
name, songs, included playlists, excluded playlists
### Playlist
name, included tags, excluded tags, minimum bangericity, maximum bangericity, and logic (boolean)  
note: and logic is true if the included tags must all be present to be in the playlist, and false if at least one tag must be present to be included
