elementof :: (Eq a) => a -> [a] -> Bool
elementof _ [] = False
elementof e (x:xs) = x == e || elementof e xs

union' :: (Eq t) => [t] -> [t] -> [t]
union' [] lst = lst
union' (x:xs) lst = if elementof x lst then rest else x : rest
  where rest = union' xs lst

minus' :: (Eq t) => [t] -> [t] -> [t]
minus' [] lst = []
minus' (x:xs) lst = if elementof x lst then rest else x : rest
  where rest = minus' xs lst

isClosed :: Term -> Bool
isClosed t = (freeVars t) == []

freeVars :: Term -> [String]
freeVars (Numeral _) = []
freeVars (Variable x) = [x]
freeVars (Paren t1 t2) = union' (freeVars t1) (freeVars t2)
freeVars (FuncApply t1 t2) = union' (freeVars t1) (freeVars t2)
freeVars (Lambda x _ t) = minus' (freeVars t) [x]
