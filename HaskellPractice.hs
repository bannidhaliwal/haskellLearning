remove::Eq a=>[a]->a->[a];
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
--This function removes all the characters that are not given in the list.
--This function can be used to remove the upper cases or lower cases characters from the string.
--However, use is not limited to that only. A list should be passed to this function and we can
--create new lists..
removeNonGiven::Eq a=>[a]->([a]->[a]);
removeNonGiven list [] = list;
removeNonGiven list validCharacters = [validElements | validElements<-list,validElements `elem` validCharacters];
data Either' a b = Onleft a | Onright b deriving Show;
printEither:: (Show b)=>Either' a b-> Either' a b;
printEither either = either;
main = print $ printEither (Onleft "Banni");
--Simple usage of Maybe and Either function in haskell.
--These two things are pretty usefull and power features
--of haskell. If we are expecting some two or even 10 possible returns 
--from any function it can map to that..

--My very own declaration of Either
data Either' a b = Onleft a | Onright b deriving Show;
--Again my declaration for Maybe
data Mightbe a = None | Its a deriving Show;
--Just a simple a type synonum
type Name = String;
--Again just simple type synonum
type PhoneNumber = String;
--My phone book
type PhoneBook = [(Name,PhoneNumber)];
--This function will check if we already have the number in phone book
checkIfNameExist::PhoneNumber->PhoneBook->Mightbe Name;
checkIfNameExist _ [] = None;
checkIfNameExist phnNumber phnBook@(headPhoneBook:tailPhoneBook)
    |(snd headPhoneBook) == phnNumber = Its (fst headPhoneBook)
    |otherwise = recurse
    where 
        recurse = checkIfNameExist phnNumber tailPhoneBook;
--This function would put some entry into the given phonebook.
--Before doing that it will check if number already exist and 
--if it does, then we are not putting anything anywher and returning a string 
--saying that it exists
addToPhoneBook::PhoneBook->PhoneNumber->Name->Either' PhoneBook String;
addToPhoneBook [] phnNumber name = Onleft $ [(name,phnNumber)];
addToPhoneBook phoneBook phnNumber name = case checkIfNameExist phnNumber phoneBook of
                                                            None ->Onleft $ phoneBook ++ [(name,phnNumber)]
                                                            Its a ->Onright $ ("This phone number is taken by " ++ id a)

--Example of recursive data defination and how to use infix
infixr 5 :-:
data Mylist a = None | a :-: (Mylist a) deriving Show;
infixr 5 .++
(.++)::Mylist a->Mylist a->Mylist a;
(.++) (None) listTwo = listTwo;
(.++) listOne@(listOneHead:-:listOneTail) listTwo = listOneHead :-: (listOneTail .++ listTwo);     

--Example of binary tree in Haskell. 

--We have a defination of recursive data
data Node a = None | NodeTree a (Node a) (Node a) deriving (Show,Eq);
type Name = String;
type PhoneNumber = String;
type PhoneRecord = [(Name,PhoneNumber)];
--Check if the Name already exist in phone Record..
checkIfExist::Name->PhoneRecord->Bool;
checkIfExist _ [] = False;
checkIfExist name phoneRecord = if (fst (phoneRecordHead)) == name then True 
                                else checkIfExist name phoneRecordTail
                                where (phoneRecordHead,phoneRecordTail) = (head phoneRecord,tail phoneRecord);
--Insert into the phone book
insertAtPhoneBook::Name->PhoneNumber->PhoneRecord->Either String PhoneRecord;
insertAtPhoneBook name phnNumber [] =Right$ [(name,phnNumber)];
insertAtPhoneBook name phnNumber phoneRecord = case checkIfExist name phoneRecord of
                                                True -> Left "Name already exist"
                                                False ->Right $ phoneRecord ++ ([(name,phnNumber)])
--Create Node
createNode::PhoneRecord->Node PhoneRecord;
createNode phoneRecord = (NodeTree phoneRecord (None) (None));
comparePhoneRecords::PhoneRecord->PhoneRecord->Bool;
comparePhoneRecords phoneRecordOne phoneRecordTwo = phnOneHead > phnTwoHead
                                                    where
                                                        (phnOneHead,phnTwoHead) = (fst(head phoneRecordOne),fst(head phoneRecordTwo))
--Insert at node. Here we will be checking if we need to go right or left on 
--the basis of comparing b/w records. Here we are not checking if have any existing record..
insertAtNode::PhoneRecord->Node PhoneRecord->Node PhoneRecord;
insertAtNode phoneRecord ourNode@(NodeTree nodeHead (left) (right))
    |comparePhoneRecords phoneRecord nodeHead = case right == None of
                                                    True -> (NodeTree nodeHead (left) (NodeTree phoneRecord None None))
                                                    False -> (insertAtNode phoneRecord right)
    |otherwise = case left == None of 
                    True -> (NodeTree nodeHead (NodeTree (phoneRecord) (None)(None)) (right))
                    False -> (insertAtNode phoneRecord left)

phoneRecord = [("Banni","0449964392")];
node = createNode [("Winny","0452560531")];                                                        