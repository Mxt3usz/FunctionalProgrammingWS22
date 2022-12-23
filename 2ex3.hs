type Duration = Float
type TrackId = Int
data Rating = Good | Bad
    deriving Show
type Name = String
type AlbumName = String

data User = User {alias :: Name, rating :: Rating}
    deriving Show

data Album = Album {name:: AlbumName,trackList :: [TrackId]}
    deriving Show

data Track = Track {title :: Name, artist :: Name, duration :: Duration, id' :: TrackId, usr :: [User]}
    deriving Show

data MediaBib = MediaBib {track :: [Track], album :: [Album]}
    deriving Show


addAlbum :: TrackId -> AlbumName -> MediaBib -> MediaBib

addAlbum t a m = f (album m) 
    where
        f  [] = m {album = []}
        f (z:zs) |  a /= name z = (f zs) {album = z:album (f zs)} 
                     |  otherwise = (f zs) {album = Album{name = a,trackList = t:trackList z}:album (f zs)} 


a = MediaBib{track = [Track{title = "ayo",artist = "me",duration = 210, id' = 7, usr = []}], album = [Album{name = "do", trackList = [5,2]},Album{name = "yo", trackList = [3,1,9]}]}
b = User{alias = "pi",rating = Bad}
rateTrack :: User -> TrackId -> MediaBib -> MediaBib

rateTrack u t m = f (track m)
    where
        f [] = m {track = []}
        f (z:zs) | id' z /= t = (f zs) { track  = z : track (f zs)}
                 | otherwise =  (f zs) { track  = Track{title = title z,artist = artist z, duration = duration z, id' = t, usr = u : usr z } : track (f zs)}



