from sqlalchemy import create_engine, Table, Column, Integer, String, ForeignKey, Float, Boolean
from sqlalchemy.orm import relationship, sessionmaker
from sqlalchemy.ext.declarative import declarative_base

Base = declarative_base()


songTag_table = Table('songTag', Base.metadata,
    Column('song_id', Integer, ForeignKey('song.id')),
    Column('tag_id', Integer, ForeignKey('tag.id'))
)

playlistTag_table = Table('playlistTag', Base.metadata,
    Column('playlist_id', Integer, ForeignKey('playlist.id')),
    Column('tag_id', Integer, ForeignKey('tag.id'))
)

class Song(Base):
    __tablename__ = 'song'
    id = Column(Integer, primary_key=True)
    tags = relationship('Tag',
        secondary=songTag_table,
        back_populates='songs'
    )
    bangericity = Column(Float, nullable=False)
    name = Column(String, nullable=False, unique=True)
    def addTag(tag):
        tags.append(tag)
    def __repr__(self):
        return '<Song(name="{0}", bangericity={1})>'.format(
            self.name,
            self.bangericity
        )

class Tag(Base):
    __tablename__ = 'tag'
    id = Column(Integer, primary_key=True)
    songs = relationship('Song',
        secondary=songTag_table,
        back_populates='tags'
    )
    includedPlaylists = relationship('Playlist',
        secondary=playlistTag_table,
        back_populates='includedTags'
    )
    excludedPlaylists = relationship('Playlist',
        secondary=playlistTag_table,
        back_populates='excludedTags'
    )
    name = Column(String, nullable=False, unique=True)
    def __repr__(self):
        return '<Tag(name="{0}")>'.format(self.name)

class Playlist(Base):
    __tablename__ = 'playlist'
    id = Column(Integer, primary_key=True)
    includedTags = relationship('Tag',
        secondary=playlistTag_table,
        back_populates='includedPlaylists'
    )
    excludedTags = relationship('Tag',
        secondary=playlistTag_table,
        back_populates='excludedPlaylists'
    )
    minBangericity = Column(Float, nullable=False)
    maxBangericity = Column(Float, nullable=False)
    name = Column(String, nullable=False, unique=True)
    andLogic = Column(Boolean, default=True, nullable=False)
    def __repr__(self):
        return '<Playlist(name="{0}")>'.format(self.name)


if __name__ == '__main__':
    engine = create_engine('sqlite:///database.db', echo=True)
    Session = sessionmaker(bind=engine)
    session = Session()
    Base = declarative_base()
    Base.metadata.create_all(engine)
