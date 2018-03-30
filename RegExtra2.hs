module RegExtra where
import Mon
import Reg
import Data.List
-- It's not the final version!
data AB = A | B deriving(Eq,Ord,Show)

infix 4 ===
class Equiv a where
  (===) :: a -> a -> Bool

instance (Eq c) => Equiv (Reg c) where
  r1 === r2 = equiv r1' r2' || equiv r2' r1' where
      r1' = simpl r1
      r2' = simpl r2
      equiv s@((s1 :> s2) :> s3) t@(t1 :> (t2 :> t3)) =
        (s == t) || (s1 == t1 && s2 == t2 && s3 == t3)
      equiv s t = s == t

instance Mon (Reg c) where
  m1 = Eps
  x <> y = x :> y

simpl :: Eq c => Reg c -> Reg c
simpl r = case r of
  Many r'   -> case r' of
    Empty -> Eps
    Eps   -> Eps
    _     -> Many (simpl r')
  r1 :| r2  -> simplSum r
  r1 :> r2  -> let
    r1' = simpl r1
    r2' = simpl r2
    in case r1' of
      Empty -> Empty
      Eps -> r2'
      _   -> case r2' of
        Empty -> Empty
        Eps -> r1'
        _   -> r1' :> r2'
  _ -> r

simplSum :: Eq c => Reg c -> Reg c
simplSum reg = case reg of
  r1 :| r2 -> foldr1 (:|) (simplSum' (traver (r1 :| r2))) where
    traver r = case r of
      s :| t -> (traver s) ++ (traver t)
      _      -> [simpl r]
    simplSum' [] = []
    simplSum' (x:xs) = case x of
      Empty -> simplSum' xs
      _     ->  if    x `elem` xs then simplSum' xs
                else  x : simplSum' xs
  _ -> reg

nullable :: Reg c -> Bool
nullable r = case r of
  Empty     -> False
  Lit _     -> False
  Many _    -> True
  Eps       -> True
  r1 :> r2  -> nullable r1 && nullable r2
  r1 :| r2  -> nullable r1 || nullable r2

empty :: Reg c -> Bool
empty r = case r of
  Empty     -> True
  Lit _     -> False
  Eps       -> False
  Many _    -> False
  r1 :> r2  -> empty r1 || empty r2
  r1 :| r2  -> empty r1 && empty r2

der :: Eq c => c -> Reg c -> Reg c
der c r = case r of
  Lit c'  ->
    if c == c' then Eps
    else Empty
  Eps     -> Empty
  Empty   -> Empty
  Many r  -> let r' = der c r in
    if    empty r' then Empty
    else  (der c r) :> Many r
  r1 :> r2 ->
    if nullable r1 then (der c r1) :> r2 :| der c r2
    else (der c r1) :> r2
  r1 :| r2 -> (der c r1) :| (der c r2)

ders :: Eq c => [c] -> Reg c -> Reg c
ders xs r = foldl (flip der) r xs

accepts :: Eq c => Reg c -> [c] -> Bool
accepts r w = nullable (ders w r)

mayStart :: Eq c => c -> Reg c -> Bool
mayStart c r = not (empty (der c r))

match :: Eq c => Reg c -> [c] -> Maybe [c]
match r w = match_pref r' Nothing (inits w)
  where
    r' = simpl r
    match_pref r max_pref prefs = case prefs of
      []    -> max_pref
      x:xs  -> if   accepts r' w then match_pref r (Just x) xs
               else max_pref

search :: Eq c => Reg c -> [c] -> Maybe [c]
search r w = findInf r (tails w)
  where
    findInf r suffs = case suffs of
      []    -> Nothing
      x:xs  -> case match r x of
        Just x  -> Just x
        Nothing -> findInf r xs

findall :: Eq c => Reg c -> [c] -> [[c]]
findall r w = [c | Just c <- infixes]
  where
    infixes = [search r suf | suf <- tails w]

char :: Char -> Reg Char
char = Lit

string :: [Char] -> Reg Char
string = foldr1 (:>) . map Lit

alts :: [Char] -> Reg Char
alts = foldr1 (:|) . map Lit

letter = alts ['a'..'z'] :| alts ['A'..'Z']
digit = alts ['0'..'9']
number = digit :> Many digit
ident = letter :> Many (letter :| digit)

many1 r = r :> Many r
