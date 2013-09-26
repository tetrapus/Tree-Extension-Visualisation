import Graphics.Gloss
import System.Environment

data BinaryTree = Branch Float BinaryTree BinaryTree
                | Leaf
                deriving (Show, Ord, Eq)

maxwidth  = 1200
maxheight = 600
node            = Color (dark green)       (ThickCircle 1.5 3.0)
niceline coords = Color (light chartreuse) (Line coords)


main = do
        fn:rest  <- getArgs
        file     <- readFile fn
        let tree = mktree [read x :: Float | x <- words file] (Branch 0 Leaf Leaf)
        simulate (InWindow 
                        "BTree Demo"                             -- Title
                        (truncate maxwidth, truncate maxheight)  -- Dimensions
                        (10, 10))                                -- Position
                    black       -- Background
                    2           -- Steps per second
                    tree        -- Initial state
                    gentree     -- Picture representation of state
                    elongate    -- Alter state



size :: BinaryTree -> Integer
size Leaf           = 0
size (Branch v l r) = 1 + (size l) + (size r)

iheight :: BinaryTree -> Float
iheight (Leaf)         = 0
iheight (Branch v l r) = v + max (iheight l) (iheight r)

insertTree :: Float -> BinaryTree -> BinaryTree
insertTree n (Branch v l r)
        | size l < size r   = Branch v (insertTree n l) r
        | otherwise         = Branch v l (insertTree n r)
insertTree n Leaf           = Branch n Leaf Leaf

mktree :: [Float] -> BinaryTree -> BinaryTree
mktree (x:xs) tree = mktree xs (insertTree x tree)
mktree []     tree = tree

height x total = x * (maxheight / 2) / total

tree :: Float -> Float -> BinaryTree -> Picture
tree total n Leaf           = Blank
tree total n (Branch i l r) = Pictures [niceline [(0,0), (n, h)],
                                        Translate n h (tree total (-n/2) l), 
                                        Translate n h (tree total (n/2)  r), 
                                        Translate n h node]
                            where h = -(height i total)

addval :: Float -> BinaryTree -> BinaryTree
addval i (Branch v l r) = Branch (v+i) l r
addval _ Leaf = Leaf

treelongate :: Float -> BinaryTree -> BinaryTree
treelongate n Leaf = Leaf
treelongate n (Branch v l r)
    | n > 0     = Branch v (treelongate (n-1) l) (treelongate (n-1) r)
    | lh > rh   = Branch v l (addval (lh - rh) r)
    | lh < rh   = Branch v (addval (rh - lh) l) r
    | otherwise = Branch v l r
    where
        lh = iheight l
        rh = iheight r

elongate _ time tree@(Branch v l r) = treelongate v (addval 1 tree)

gentree (Branch _ l r) = Translate 0 (maxheight / 3) 
                            (Pictures [tree t (-maxwidth / 5) l, 
                                       tree t (maxwidth / 5) r, 
                                       node])
                        where t = max (iheight l) (iheight r)