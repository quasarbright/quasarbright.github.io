import os
import ntpath
import DBhandling as db
songDir = '/Users/mdelmonaco/OneDrive/Music/7-7-18 additions'
songNames = os.listdir(songDir)
def songPath(name):
    return os.path.join(songDir, name)
songPaths = [songPath(name) for name in songNames]
for songPath in songPaths:
    songName = ntpath.basename(songPath)
    bangericity = float(input(songName+': '))
    db.addSong(songPath, songName, bangericity)

'''
<Song(name="Dear You - Yuduki", bangericity="85.0")>,
 <Song(name="Sendan Life - Hanamori Yumiri", bangericity="87.0")>,
 <Song(name="Asymmetry - Reol", bangericity="92.0")>,
 <Song(name="Imaginary Like the Justice - GUMI", bangericity="90.0")>,
 <Song(name="Bad Apple!!", bangericity="88.0")>,
 <Song(name="Virtual Paradise - AK x LYNX ft. Veela", bangericity="89.0")>,
 <Song(name="Wareta Ringo - Shinsekai Yori ED1", bangericity="88.0")>,
 <Song(name="Koi Wa Chaos No Shimobe Nari full - Haiyore Nyaruko OP", bangericity="90.0")>,
 <Song(name="ひとり full - Darling in the FranXX ED 4", bangericity="90.0")>,
 <Song(name="A Cruel Angel's Thesis - Neon Genesis Evangelion OP", bangericity="85.0")>,
 <Song(name="Parallel Line - Sayuri", bangericity="87.0")>,
 <Song(name="Noushou Sakuretsu Girl - Hatsune Miku", bangericity="90.0")>,
 <Song(name="Sorewa Chiisana Hikari No Youna full - Erased ED", bangericity="90.0")>,
 <Song(name="Hitorigoto - Eromanga Sensei OP1", bangericity="90.0")>,
 <Song(name="Tear Rain - cYsmix", bangericity="87.0")>,
 <Song(name="Dear You (remix) - DJ Genericname", bangericity="89.0")>,
 <Song(name="(Can You) Understand Me - Komiya Mao", bangericity="90.0")>,
 <Song(name="Tower Of Heaven (You Are Slaves) - Feint", bangericity="94.0")>,
 <Song(name="Asu no Yozora Shoukaihan - Yuaru", bangericity="94.0")>,
 <Song(name="Wakusei Rabbit - Yunomi", bangericity="95.0")>,
 <Song(name="Highscore - Panda Eyes", bangericity="86.0")>,
 <Song(name="Senbonzakura - White Flame ft. Hatsune Miku", bangericity="90.0")>,
 <Song(name="[A]ddiction - Giga and Reol", bangericity="95.0")>,
 <Song(name="Kakushinteki Metamaruphose! full - Umaru Chan OP1", bangericity="90.0")>,
 <Song(name="3331 - Nameless", bangericity="90.0")>,
 <Song(name="Cold Green Eyes - Station Earth", bangericity="93.0")>,
 <Song(name="Escape full - Darling in the FranXX ED 5", bangericity="92.0")>,
 <Song(name="Remote Control - Saiya", bangericity="94.0")>]

 [<Tag(name="osu")>,
 <Tag(name="weeb")>,
 <Tag(name="anime")>,
 <Tag(name="noragami")>,
 <Tag(name="banger")>,
 <Tag(name="for the uncultured")>,
 <Tag(name="reol")>,
 <Tag(name="giga p")>]
'''
