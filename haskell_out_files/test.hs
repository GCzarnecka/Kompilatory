module Main where

main :: IO ()

-- single line comment

{- Multiline
    comment
-}

data List = List { x :: Integer, next_node :: Maybe List }

my_length l =
    case l of
        (Nothing) -> 0
        (Just (List x y)) -> 1 + my_length y

my_list =
    Just List { 
        x = 10, next_node = Just List { 
        x = 200, next_node = Just List { 
        x = 3000, next_node = Nothing }}
}

main = print ( my_length my_list )
