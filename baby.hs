returnLowerCaseOnly string = [x | x<-string,x `elem` ['a'..'z']||x `elem` ['A'..'Z']];
rightTriangle' = [(a,b,c)|a<-[1..10],b<-[1..10],c<-[1..10],a^2 + b^2 == c^2];
bigger x y = if x > y then x else y;
length' :: (Num b)=>[a]->b
length' list = sum[1|_<-list];
returnGivenNumber :: (Num a)=>a->a;
returnGivenNumber x = x;
returnGivenString :: String-> String;
returnGivenString x = x;
patternMatching :: Int->[Char]
patternMatching 7 = "You are lucky pal";
patternMatching x = "You are out of luck pal";
factorilaRecursive :: (Integral a)=> a->a;
factorilaRecursive 0 = 1;
factorilaRecursive x = x * factorilaRecursive(x-1);
insertElementAtBack :: t->[t]->[t]
insertElementAtBack a b = reverse (a:(reverse b));
lengthRecursive :: (Num a)=>[list]->a;
lengthRecursive [] = 0;
lengthRecursive (_:xs) = 1 + lengthRecursive xs;
capital :: String->String
capital all@(x:xs) = "all is "++ all;
tellIfANumberIsBiggerThanTwentyFive::(Num a,Ord a,Show a)=>a->String;
tellIfANumberIsBiggerThanTwentyFive a 
	| a > emi = "The number is bigger than "++show emi
	|otherwise = "The number is equal to "++show emi
	where emi = 25
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT  
tellBmi :: (RealFloat a)=>a->a->String
tellBmi weight height
    |bmi<=skinny = "you are skinny"
    |bmi<=normal = "you are normal"
    |bmi<=fat = "you are fat"
    |otherwise = "you are obese control it"
     where bmi = weight / height ^ 2
     	   (skinny,normal,fat) = (18.5,25.0,30.0)
getInitials::String->String->String
getInitials firstName lastName = [f] ++"."++[l]
	where
		(f:_) = firstName
		(l:_) = lastName
calcBmis xs = let bmi a = a*2 in bmi xs;

sumOfAllMultiples::Int->Int->Int->Int
sumOfAllMultiples first second limit= sum[x|x<-[first..limit-1],x `rem` first==0||x`rem` second == 0];
fibonacci 1 = 1;
fibonacci 0 = 0;
fibonacci n = fibonacci(n-1) + fibonacci(n-2);
sumOfAllNumbers::Int->Int
sumOfAllNumbers 0 = 0
sumOfAllNumbers 1 = 1
sumOfAllNumbers number = number+sumOfAllNumbers(number-1);
useOfCase :: [a]->String
useOfCase list = what list
	where
		what [] = "empty"
		what [x] = "singleton"
		what (x:_) = "this si longer"
recursion [] = "empty list"
recursion [x] = x;
recursion(x:xs) 
	|x>maxTail = x
	|otherwise = maxTail
	where
		maxTail = recursion xs
				
compress :: (Eq a) => [a] -> [a]
compress []  = []
compress [x] = [x] -- also can do (x:[]) to be clear what the pattern is.
compress (x:y:xs)
    | x == y    = (compress (x:xs))
    | otherwise = x : y : (compress xs)
inList:: (Eq a)=>[a]->a->Bool;
inList [] _ = False;
inList [x] a
    |a==x = True
    |otherwise = False
inList (listHead:listTail) a
    |a == listHead = True
    |otherwise = inList listTail a
nub::(Eq a)=>[a]->[a];
nub [] = [];
nub [x] = [x];
nub (listHead:listTail)
    |inList listTail listHead = nub listTail
    |otherwise = listHead:(nub listTail)
intersperse::[Char]->[Char]
intersperse [] = [];
intersperse [x] = [x];
intersperse (listHead:listTail) = listHead:('.':(intersperse listTail));
insertAtEndOfString::[a]->Int->Int->[a]->[a];
insertAtEndOfString [] _ _ elementToInsert = elementToInsert;
insertAtEndOfString (listHead:listTail) lengthOfString counter elementToInsert
    |counter == lengthOfString = listHead:elementToInsert
    |otherwise = listHead: (insertAtEndOfString listTail lengthOfString (counter+1) elementToInsert)
insertAtIndexGiven::[a]->(Int->(a->Int->[a]));
insertAtIndexGiven [] _ _ 0= error "Empty list";
insertAtIndexGiven [] _ _ _= [];
insertAtIndexGiven list index elementToInsert counter
    |outOfIndex && (counter == 0) = error "Index is larger than the length of the list"
    |counter == index = listHead:(elementToInsert:(insertAtIndexGiven listTail index elementToInsert (counter+1)))
    |otherwise = listHead:(insertAtIndexGiven listTail index elementToInsert (counter+1))
    where
        outOfIndex = index > ((length list)-1 )
        listHead = head list
        listTail = tail list
