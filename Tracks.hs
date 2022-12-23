-- Copyright 2022 University of Freiburg
-- Janek Spaderna <janek.spaderna@pluto.uni-freiburg.de>
module Tracks where


type TrackId = Integer

type Title = String

type Artist = String

type AlbumName = String

type Duration = Integer

type TrackList = [(TrackId, Title, Artist, Duration, [AlbumName])]

-- | Tracks by "Knuth & The Gang"
list1 :: TrackList
list1 =
  [ ( 11,
      "Algorithms",
      "Knuth & The Gang",
      111,
      ["Fundamental Algorithms", "The Art Of Computer Programming - The Collection"]
    ),
    ( 12,
      "Mathematical Preliminaries",
      "Knuth & The Gang",
      222,
      ["The Art Of Computer Programming - The Collection", "Fundamental Algorithms"]
    ),
    ( 13,
      "MMIX",
      "Knuth & The Gang",
      333,
      ["The Art Of Computer Programming - The Collection", "Fundamental Algorithms"]
    ),
    ( 18,
      "Celebration",
      "Knuth & The Gang",
      296,
      ["The Art Of Computer Programming - What's To Come"]
    )
  ]

-- | Tracks by "Haskell & Curry"
list2 :: TrackList
list2 =
  [ (23, "Mrs. Lovelace", "Haskell & Curry", 231, []),
    (22, "Bridge Over Impure Water", "Haskell & Curry", 296, [])
  ]

-- | Tracks by "Pure Bandit"
list3 :: TrackList
list3 =
  [ (31, "Being Lazy With Class", "Pure Bandit", 123, ["A History of Haskell"]),
    (32, "Lazy Beings With Class", "Pure Bandit", 234, ["A History of Haskell"]),
    (33, "Beings With Lazy Class", "Pure Bandit", 345, ["A History of Haskell"])
  ]

-- | Tracks by "The Functionals"
list4 :: TrackList
list4 =
  [ (40, "Grandfather's Lisp", "The Functionals", 1958, ["Influencers"]),
    (41, "Dad's Scheme", "The Functionals", 1970, ["Influencers"]),
    (42, "Mom's Miranda", "The Functionals", 1985, ["Influencers"]),
    (43, "Hope", "The Functionals", 1970, ["Influencers"]),
    (44, "A New Hope+", "The Functionals", 1990, ["Influencers"])
  ]

-- | Concatenation of all previous track lists.
allTracks :: TrackList
allTracks = concat [list1, list2, list3, list4]

data Rating = Good | Bad
    deriving (Show,Eq)
type Name = String

data User = User {alias :: Name, rating :: Rating}
    deriving Show

data Album = Album {name:: AlbumName,trackList :: [TrackId]}
    deriving Show

data Track = Track {title :: Name, artist :: Name, duration :: Duration, id' :: TrackId, usr :: [User]}
    deriving Show

data MediaBib = MediaBib {track :: [Track], album :: [Album]}
    deriving Show

constructLib :: TrackList -> MediaBib
constructLib ts = g ts (g' ts [])
                    where
                    g' [] al' = MediaBib{track = [], album = []}
                    g' ((trackid,title,artist,duration,albumm):xs) al' | not (seen albumm al') =  (g' xs (al' ++ albumm)) {track = Track{title = title, artist = artist,
                                                                                duration = duration, id' = trackid,usr = [User{alias = "Peter",rating = Bad}]} : track (g' xs (al'++ albumm)),
                                                                                album = f albumm ++ album (g' xs (al' ++ albumm))}
                                                                        | otherwise = g' xs al'
                                                             
                        where
                        f [] = []
                        f (x:xs) = Album{name = x, trackList = [trackid]} : f xs
                        seen j [] = False
                        seen j (f:fs) | check f j = True
                                    | otherwise = seen j fs
                        check b [] = False
                        check b (n:ns) | n == b = True
                                    | otherwise = check b ns


                    g [] al = g' ts []
                    g ((trackid,title,artist,duration,albumm):xs) al | seen trackid (track al) = (g xs al) {track = Track{title = title, artist = artist,
                                                                                    duration = duration, id' = trackid,usr = [User{alias = "Peter",rating = Good}]} : track (g xs al),
                                                                                    album = k (album (g xs al)) albumm}
                                                                    | otherwise = g xs al
                        where
                        seen x [] = True
                        seen x (y:ys) | x /= id' y = seen x ys
                                    | otherwise = False
                            
                        k [] v = []
                        k (q:qs) v | e v q = Album{name = name q,trackList = trackid : trackList q} : k qs v
                                    | otherwise = q : k qs v
                        e [] l = False
                        e (m:ms) l | m == name l = True
                                    | otherwise = e ms l
                    

a = constructLib allTracks

durationAlbums :: MediaBib -> [(AlbumName,Duration)]

durationAlbums lib = f (track lib) (album lib)
        where
            f x [] = []
            f x (y:ys) = (name y,f' x (trackList y)) : f x ys
                where
                    f' x [] = 0
                    f' x (y:ys) = f'' x y + f' x ys 
                        where
                            f'' (x:xs) y | id' x == y = duration x
                                         | otherwise = f'' xs y



ratedGood :: MediaBib -> User -> [Album]

ratedGood lib usr' = f (track lib) (album lib) (alias usr')
        where
            f x [] z = []
            f x (y:ys) z | f' x (trackList y) z `foo` length (trackList y) > 0.5 = y : f x ys z
                         | otherwise = f x ys z
                where
                    foo a b = fromIntegral a / fromIntegral b
                    f' x [] z = 0
                    f' x (y:ys) z  | f'' x y z = 1 + f' x ys z
                                         | otherwise = f' x ys z
                        where
                            f'' [] y z = False
                            f'' (x:xs) y z | id' x == y && f''' (usr x) z = True
                                           | otherwise = f'' xs y z
                                            where
                                                f''' [] y = False
                                                f''' (x:xs) y | alias x == y && rating x == Good = True
                                                              | otherwise = f''' xs y

