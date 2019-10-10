hreverse :: [t] -> [t]

hreverse [] = []
hreverse (h:t) = (hreverse t) ++ [h]
