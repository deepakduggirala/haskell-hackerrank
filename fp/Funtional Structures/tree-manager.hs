import           Control.Monad
import           Data.List                      ( break
                                                , isPrefixOf
                                                )
import           Data.Function                  ( (&) )

data Tree a = Tree a [Tree a] deriving (Show)
data BreadCrumb a = BreadCrumb a [Tree a] [Tree a] deriving (Show)
type Zipper a = (Tree a, [BreadCrumb a])
type ListZipper a = ([a], [a])

data Cmd = View | Modify (Zipper Int -> Maybe (Zipper Int))

changeValue :: a -> Zipper a -> Zipper a
changeValue a (Tree _ items, bs) = (Tree a items, bs)

printVal :: Zipper a -> a
printVal (Tree x items, bs) = x

visitChild :: Int -> Zipper a -> Zipper a
visitChild n (Tree p items, bs) = (c, BreadCrumb p ls rs : bs)
  where (ls, c : rs) = splitAt (n - 1) items

visitParent :: Zipper a -> Maybe (Zipper a)
visitParent (item, BreadCrumb p ls rs : bs) =
  Just (Tree p (ls ++ [item] ++ rs), bs)
visitParent (_, []) = Nothing

visitLeft :: Zipper a -> Maybe (Zipper a)
visitLeft (_, BreadCrumb _ [] _ : _) = Nothing
visitLeft (item, BreadCrumb p ls rs : bs) =
  Just (last ls, BreadCrumb p (init ls) (item : rs) : bs)
visitLeft (_, []) = Nothing

visitRight :: Zipper a -> Maybe (Zipper a)
visitRight (item, BreadCrumb p ls (r : rs) : bs) =
  Just (r, BreadCrumb p (ls ++ [item]) rs : bs)
visitRight (_, BreadCrumb _ _ [] : _) = Nothing
visitRight (_, []                   ) = Nothing

insertChild :: a -> Zipper a -> Zipper a
insertChild x (Tree p items, bs) = (Tree p ((Tree x []) : items), bs)

insertLeft :: a -> Zipper a -> Maybe (Zipper a)
insertLeft x (item, BreadCrumb p ls rs : bs) =
  Just (item, BreadCrumb p (ls ++ [Tree x []]) rs : bs)
insertLeft _ (_, []) = Nothing    -- Invalid op: Inserting any sibling of the root.

insertRight :: a -> Zipper a -> Maybe (Zipper a)
insertRight x (item, BreadCrumb p ls rs : bs) =
  Just (item, BreadCrumb p ls (Tree x [] : rs) : bs)
insertRight _ (_, []) = Nothing   -- Invalid op: Inserting any sibling of the root.

delete :: Zipper a -> Maybe (Zipper a)
delete (item, BreadCrumb p ls rs : bs) = Just (Tree p (ls ++ rs), bs)
delete (_   , []                     ) = Nothing  -- Invalid op: Deleting the root.

-- t :: Tree Int
-- t = Tree 0 []
-- z = (t, [])

parseInput :: String -> Cmd
parseInput x
  | x == "print"                  = View
  | x == "visit right"            = Modify visitRight
  | x == "visit left"             = Modify visitLeft
  | x == "visit parent"           = Modify visitParent
  | x == "delete"                 = Modify delete
  | "change" `isPrefixOf` x       = Modify $ return . changeValue (extractInt x)
  | "visit child" `isPrefixOf` x  = Modify $ return . visitChild (extractInt x)
  | "insert left" `isPrefixOf` x  = Modify (insertLeft (extractInt x))
  | "insert right" `isPrefixOf` x = Modify (insertRight (extractInt x))
  | "insert child" `isPrefixOf` x = Modify $ return . insertChild (extractInt x)

extractInt :: String -> Int
extractInt = read . last . words

-- f :: Monad m => [a -> m a] -> a -> m a
-- f = foldl (>=>) return

applyCmds :: [Cmd] -> Zipper Int -> IO ()
applyCmds []            z = return ()
applyCmds (View : cmds) z = do
  print $ printVal z
  applyCmds cmds z
applyCmds (Modify f : cmds) z = forM_ (f z) (applyCmds cmds)

main :: IO ()
main = do
  q    <- readLn :: IO Int
  cmds <- replicateM q (parseInput <$> getLine)
  applyCmds cmds (Tree 0 [], [])
