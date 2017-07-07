--Circles and cross game in haskell.
--This file just contains the functions for it
--as I havent learned the IO yet..
data GameBox = GameBox {gameArray::[Int]} deriving (Show);
data Player = Cross | Circle | Empty deriving (Show,Eq);
type GameArray = [Player];
type Index = Int;
type PlayerType = Player;
type ModifiedArray = [Player];

turnOnSquare gameArray@(gameArrayHead:gameArrayTail) index playerType = if (length gameArray) <= index then error "out of range"
    else if (index < 0) then error "No negative index possible"
    else if(index == 0) then
                            if(head gameArray) == Empty then playerType:gameArrayTail
                            else error "You cannot override the other player"
    else gameArrayHead:(turnOnSquare gameArrayTail (index - 1) playerType)
    