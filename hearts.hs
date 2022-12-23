-- type of card
data Suit = Spades | Hearts | Diamonds | Clubs
    deriving (Show,Eq)

data Color = Red | Black
    deriving Show

data Rank = Numeric Integer | Jack | Queen | King | Ace
    deriving (Show, Eq, Ord)

data Card = Card {rank::Rank, suit:: Suit}
    deriving Show

cardBeats :: Card -> Card -> Bool
cardBeats givenCard c = suit givenCard == suit c && rankBeats (rank givenCard)(rank c)
rankBeats r1 r2 = r1 > r2
queenOfSpades = Card {rank = Queen,suit = Spades}
queenOfHearts = queenOfSpades{suit = Hearts}


color :: Suit -> Color
color Spades = Black
color Hearts = Red
color Diamonds = Red
color Clubs = Black


data Hand = Last Card | Next Card Hand
    deriving (Show)

topCard :: Hand -> Card
topCard (Last c) = c
topCard (Next c _) = c

chooseCard :: Card -> Hand -> Card
chooseCard gc (Last c) = c
chooseCard gc (Next c h) | cardBeats gc c = c 
                          | otherwise = chooseCard gc h