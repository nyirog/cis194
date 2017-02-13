import Data.Maybe

data Atom = 
    S String
  | I Int
  | D Atom Atom
  deriving (Show, Eq)

data Knowledge =
    Worker Atom
  | Boss Atom
  | Salary Atom Atom
  deriving Show

type KnowledgeBase = [Knowledge]

worker :: Atom -> KnowledgeBase -> KnowledgeBase
worker a k = (Worker a):k

is_worker :: Knowledge -> Maybe Atom
is_worker (Worker w) = Just w
is_worker _ = Nothing

boss :: Atom -> KnowledgeBase -> KnowledgeBase
boss a k = (Boss a):k

is_boss :: Knowledge -> Maybe Atom
is_boss (Boss b) = Just b
is_boss _ = Nothing

salary :: Atom -> Atom -> KnowledgeBase -> KnowledgeBase
salary p s k = (Salary p s):k

has_salary :: Knowledge -> Maybe Atom
has_salary (Salary p s) = Just (D p s)
has_salary _ = Nothing

query :: (Knowledge -> Maybe Atom) -> KnowledgeBase -> [Atom]
query f k = [a|Just a <- map f k]

intersect_duplets :: [Atom] -> [Atom] -> [Atom]
intersect_duplets duplets singles = [snd|(D fst snd) <- duplets, e <- singles, e == fst]


-- test data
worker_salary = intersect_duplets salaries workers
  where
    salaries = query has_salary knowledge_base
    workers = query is_worker knowledge_base
    knowledge_base =
        worker (S "Tamas")
      $ salary (S "Tamas") (I 123)
      $ boss (S "Peter")
      $ salary (S "Peter") (I 456)
      []

main :: IO()
main = do 
  print worker_salary
