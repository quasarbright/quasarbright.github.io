import os
import ntpath
import DBhandling as db

bangericities = [87.0, 83.0, 85.0, 87.0, 92.0, 90.0, 88.0, 89.0, 88.0, 90.0, 90.0, 85.0, 87.0, 90.0, 90.0, 90.0, 87.0, 89.0, 90.0, 94.0, 94.0, 95.0, 86.0, 90.0, 95.0, 90.0, 90.0, 93.0, 92.0, 94.0]

songDir = '/Users/mdelmonaco/OneDrive/Music/7-7-18 additions'
songNames = os.listdir(songDir)
def songPath(name):
    return os.path.join(songDir, name)
songPaths = [songPath(name) for name in songNames]
i = 0
for songPath in songPaths:
    songName = ntpath.basename(songPath)
    bangericity = bangericities[i]
    i += 1
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


'''
<Song(name="Sendan Life - Hanamori Yumiri", bangericity=87.0)> [<Tag(name="osu")>, <Tag(name="weeb")>, <Tag(name="banger")>]
<Song(name="Asymmetry - Reol", bangericity=92.0)> [<Tag(name="weeb")>, <Tag(name="osu")>, <Tag(name="banger")>, <Tag(name="reol")>, <Tag(name="for the unc
ultured")>]
<Song(name="Wareta Ringo - Shinsekai Yori ED1", bangericity=88.0)> [<Tag(name="anime")>, <Tag(name="weeb")>]
<Song(name="Highscore - Panda Eyes", bangericity=86.0)> [<Tag(name="osu")>, <Tag(name="for the uncultured")>]
<Song(name="[A]ddiction - Giga and Reol", bangericity=95.0)> [<Tag(name="osu")>, <Tag(name="weeb")>, <Tag(name="giga p")>, <Tag(name="reol")>, <Tag(name="
banger")>]
<Song(name="3331 - Nameless", bangericity=90.0)> [<Tag(name="banger")>, <Tag(name="osu")>]
<Song(name="Cold Green Eyes - Station Earth", bangericity=93.0)> [<Tag(name="osu")>, <Tag(name="banger")>, <Tag(name="for the uncultured")>]
<Song(name="Escape full - Darling in the FranXX ED 5", bangericity=92.0)> [<Tag(name="anime")>, <Tag(name="banger")>, <Tag(name="weeb")>]
'''



s = '''
Heart Realize full - Noragami ED1 { 87.0
Before My Body Is Dry (Tomatomerde Remix) - Kill La Kill OST { 83.0
Dear You - Yuduki { 85.0
Sendan Life - Hanamori Yumiri { 87.0
Asymmetry - Reol { 92.0
Imaginary Like the Justice - GUMI { 90.0
Bad Apple!! { 88.0
Virtual Paradise - AK x LYNX ft. Veela { 89.0
Wareta Ringo - Shinsekai Yori ED1 { 88.0
Koi Wa Chaos No Shimobe Nari full - Haiyore Nyaruko OP { 90.0
ひとり full - Darling in the FranXX ED 4 { 90.0
A Cruel Angel's Thesis - Neon Genesis Evangelion OP { 85.0
Parallel Line - Sayuri { 87.0
Noushou Sakuretsu Girl - Hatsune Miku { 90.0
Sorewa Chiisana Hikari No Youna full - Erased ED { 90.0
Hitorigoto - Eromanga Sensei OP1 { 90.0
Tear Rain - cYsmix { 87.0
Dear You (remix) - DJ Genericname { 89.0
(Can You) Understand Me - Komiya Mao { 90.0
Tower Of Heaven (You Are Slaves) - Feint { 94.0
Asu no Yozora Shoukaihan - Yuaru { 94.0
Wakusei Rabbit - Yunomi { 95.0
Highscore - Panda Eyes { 86.0
Senbonzakura - White Flame ft. Hatsune Miku { 90.0
[A]ddiction - Giga and Reol { 95.0
Kakushinteki Metamaruphose! full - Umaru Chan OP1 { 90.0
3331 - Nameless { 90.0
Cold Green Eyes - Station Earth { 93.0
Escape full - Darling in the FranXX ED 5 { 92.0
Remote Control - Saiya { 94.0
'''
