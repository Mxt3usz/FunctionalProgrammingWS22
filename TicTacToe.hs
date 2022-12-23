{-

Tic-Tac-Toe data model which gets a state of a Tic-Tac-Toe game and returns if its won by any player,
if its still in progress or drawn

format -> X * X
E -> Empty
-}



data TileType =  X | O | E
    deriving (Show,Eq)

data TicTacToe = TicTacToe{game::[[TileType]],player1 :: TileType,player2 :: TileType}
    deriving (Show,Eq)

game1 = TicTacToe{game = [[X,O,X],
                          [X,O,X],
                          [O,X,O]], player1 = X, player2 = O}


game2 = TicTacToe{game = [[O,O,X,X],
                          [X,O,X,X],
                          [O,X,O,X],
                          [X,O,X,O]], player1 = X, player2 = O}



wonLossDraw ttt = f'' (f (game ttt) (player1 ttt) (player2 ttt) 0) (length(game ttt)) 0
            where
                f'' [] y am | am /= y*y = "Still in Progress"
                            | otherwise = "Draw"
                f'' (x:xs) y am | (f''' x xs == y || f'''' x xs == y || f''''' x xs 1 == y) && x !! 1 == 1 = "Player 1 Won"
                             | (f''' x xs == y || f'''' x xs == y || f''''' x xs 1 == y) && x !! 1 == 2 = "Player 2 Won"
                             | x !! 1 == 3 = f'' xs y am
                             | otherwise = f'' xs y (am+1)
                             where
                                 f''' a [] = 1
                                 f''' a (b:bs) | a !! 2 == b !! 2 && a !! 1 == b !! 1 =  1 + f''' a bs
                                               | otherwise = f''' a bs
                                 f'''' a [] = 1
                                 f'''' a (b:bs) | a !! 0 == b !! 0 && a !! 1 == b !! 1  && a !! 2 /= b !! 2 = 1 + f'''' a bs
                                                | otherwise = f'''' a bs
                                 f''''' a [] line = 1
                                 f''''' a (b:bs) line | a !! 0 /= b !! 0 && a !! 1 == b !! 1 && a !! 2 /= b !! 2 && b !! 0 == line = 1 + f''''' a bs (line+1)
                                                 | otherwise = f''''' a bs line

                f [] p1 p2 r = []
                f (row:rows) p1 p2 r = f' row p1 p2 0 r ++ f rows p1 p2 (r+1)  -- function f that iterates over all rows
                    where
                        f' [] p1' p2' len rownr = [] -- evaluation list , function f' that iterates over all tiles in the row
                        f' (tile:rows') p1' p2' len rownr     |  p1' == tile = [len,1,rownr] : f' rows' p1' p2' (len+1) rownr
                                                              |  p2' == tile = [len,2,rownr] : f' rows' p1' p2' (len+1)  rownr
                                                              | otherwise = [len,3,rownr] : f' rows' p1' p2' (len+1) rownr
                        