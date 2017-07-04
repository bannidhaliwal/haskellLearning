remove::[Char]->Char->[Char];
remove [] _ = [];
remove list@(listHead:listTail) char
    |listHead == char = remove listTail char
    |otherwise = listHead:(remove listTail) char

bubbleSort::Ord a=>[a]->[a];
bubbleSort [] = [];
bubbleSort [x] = [x];
bubbleSort list@(firstElement:secondElement:listTail)
    |firstElement > secondElement = secondElement:(bubbleSort (firstElement:listTail))
    |otherwise = firstElement:(bubbleSort (secondElement:listTail))
callBubbleSort::Ord a=>[a]->[a];
callBubbleSort [] = [];
callBubbleSort list = callBubbleSort(init $ bubbleSort list) ++ [(last $ bubbleSort list)];
--First function which I have thought to do in Javascript and did it in haskell.
--This function gets the top values from three arrays and put them on the top 
--of the new array. This function is meant for a magic trick on cards which I am
--developing in javascript..
createArray::a->a->a->[a]->[a];
createArray x y z list = x:(y:(z:list));
createNewArray::[a]->[a]->[a]->[a];
createNewArray [] [] [] = [];
createNewArray a b c = createArray (head a) (head b) (head c) [] ++ createNewArray (tail a) (tail b) (tail c);
main = print $ createNewArray [1..9] [10..18] [19..27];s