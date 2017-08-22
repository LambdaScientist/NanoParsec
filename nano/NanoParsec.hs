--Based on : http://dev.stephendiehl.com/fun/002_parsers.html
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


module NanoParsec where 


import GHC.Types
import GHC.Num

-- Defining Error myself is a lot of work
-- Defining ConcatMap myself is a lot of work
import Prelude ( error, concatMap, Show
               , putStr, getLine, (++)
               , read, show, fromEnum
               , (<=), fromIntegral, Eq
               , elem, (==), ($)
               , undefined
               ) 

const x _               =  x

not True = False
not _ = True

id a = a 
(.) f g = \x -> f (g x)

replace :: (Functor f) => a -> f b -> f a
replace = fmap . const

print :: String -> IO()
print = putStr . show
putStrLn x = putStr (x ++ "\n")

type String = [Char] 

isDigit :: Char -> Bool
isDigit c = (fromIntegral (fromEnum c - fromEnum '0') :: Word) <= 9

-- | Sequence actions, discarding the value of the first argument.
dropThenDo :: (Applicative f) => f a -> f b -> f b
dropThenDo a1 a2 = capp (id `replace` a1) a2

flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x

item :: Parser Char
item = Parser $ \s ->
  case s of
   []     -> []
   (c:cs) -> [(c,cs)]

class Functor f where 
  fmap :: (a -> b) -> f a -> f b
class Functor f => Applicative f where
  wrap :: a -> f a
  capp :: f (a -> b) -> f a -> f b 
class Applicative m => Monad (m :: * -> *) where
  bind :: m a -> (a -> m b) -> m b
  -- chain 


-- https://stackoverflow.com/questions/10167879/distinction-between-typeclasses-monadplus-alternative-and-monoid
-- Moniod is connected to Alternative but it may be implemented diffrenting
-- Edward KMETT says: 
-- * MonadPlus is a stronger claim than Alternative,
-- * which in turn is a stronger claim than Monoid, 
-- * and while the MonadPlus and Alternative
-- * while the MonadPlus and Alternative instances for a type should be related, 
-- * the Monoid may be (and sometimes is) something completely different.
class Monoid m where
    mempty :: m
    mappend :: m -> m -> m
class Applicative f => Alternative f where
  empty :: f a 
  pick :: f a -> f a -> f a 
class (Alternative m, Monad m) => MonadPlus m where
  mplus :: m a -> m a -> m a
  -- mplus = pick

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error ("Parser did not consume entire stream." ++ rs) 
    _           -> error "Parser error."



instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  wrap a = Parser (\s -> [(a,s)])
  capp (Parser cs1) (Parser cs2) 
    = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

instance MonadPlus Parser where
  mplus p q = Parser (\s -> parse p s ++ parse q s)

instance Alternative Parser where
  empty = Parser (\cs -> [])
  pick p q = Parser 
           $ \s -> case parse p s of
                        []  -> parse q s
                        res -> res

-- | One or more.
some :: (Alternative f) => f a -> f [a]
some v = some_v
  where
    many_v = some_v `pick` wrap []
    some_v = (:) `fmap` v `capp` many_v

-- | Zero or more.
many :: (Alternative f) => f a -> f [a]
many v = many_v
  where
    many_v = some_v `pick` wrap []
    some_v = (:) `fmap` v `capp` many_v

many1 :: Parser a -> Parser [a]
many1 p = bind p 
               (\x -> bind (many p)
                           (\xs -> wrap (x:xs)))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c
  then wrap c
  else (Parser (\cs -> []))

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) `pick` wrap a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p `bind` rest -- do {a <- p; rest a}
  where rest a = (op `bind` 
                     (\f -> p `bind` 
                              (rest . f a ))) 
                 `pick` wrap a
  -- rest a = (do f <- op
  --              b <- p
  --              rest (f a b))
  --          `pick` wrap a

manyTill :: (Monad f, Alternative f) => f t -> f a -> f [t]
manyTill p end = scan
  where
    scan = pick (end `bind` (\_ -> wrap [] ) )
                (p `bind` (\x -> scan `bind` (\xs -> wrap (x:xs))))

consumeAll :: Parser String
consumeAll = many anyChar

digit :: Parser Char
digit = satisfy isDigit

parens :: Parser a -> Parser a
parens m = between (reserved "(")
                   (reserved ")")
                   m

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Int
natural = read `fmap` some (satisfy isDigit)

string :: String -> Parser String
string [] = wrap []
string (c:cs) = char c `bind` -- do { char c; string cs; wrap (c:cs)}
                       (\_ -> string cs `bind` 
                                        (\_ -> wrap (c:cs)) )

token :: Parser a -> Parser a
token p = p `bind` -- do { a <- p; spaces ; wrap a}
            (\a -> spaces `bind` 
                          (\_ -> wrap a ))

reserved :: String -> Parser String
reserved s = token (string s)

between :: Monad m => m a -> m a1 -> m b -> m b
between start end middle = start 
                    `bind` (\_ -> middle 
                           `bind` ( \n -> end  
                                   `bind` (\_ -> wrap n))) 
anyChar :: Parser Char
anyChar = satisfy (const True)


concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

noneOf :: String -> Parser Char
noneOf cs = satisfy (\c -> not (elem c cs))

newline :: Parser Char
newline = char '\n' -- # Line ending

space :: Parser Char
space = oneOf " \n\r"  

spaces :: Parser String -- # whitespace
spaces = many space

comment :: Parser String -- # comment
comment = pick commentBlock commentLine
commentBlock :: Parser String
commentBlock = bind (string "(*") (\_ -> manyTill anyChar (string "*)") 
                                  `bind` ( \n -> wrap n))
commentLine :: Parser String
commentLine = char '#'
       `bind` (\_ -> many (noneOf "\n")
              `bind` (\c -> wrap c))

floatNum :: Parser String -- float      -- # float
floatNum = bind intNum 
                (\s -> bind digitOrNothing
                            (\cs -> wrap $ (s ++ cs)))
  where digitOrNothing = pick (bind (string ".") 
                                    (\s -> bind intNum 
                                                (\cs -> wrap $ (s ++ cs))))
                              (string "")
intNum :: Parser String -- integer    -- # int
intNum = (string "-" `pick` wrap []) 
  `bind` (\s -> some digit `bind` 
                           (\cs -> wrap $ (s ++ cs)))

stringAlphabet :: [Parser String] -> Parser String
stringAlphabet xs = bind (many $ (oneOfString xs))
                         (wrap . concat)

oneOfString :: [Parser String] -> Parser String
oneOfString [] = empty
oneOfString (p:ps) = pick p (oneOfString ps)

stringLit :: Parser String -- string =      -- # string with excaped quotes
stringLit = between (char '\"')
                    (char '\"')
                    (stringAlphabet [many1 $ noneOf "\\\"", string "\\\"", many1 $ oneOf "\\"])

charLit :: Parser Char -- character  -- # single character
charLit = bind (char '\'')
               (\_ -> anyChar) 

charPeriod :: Parser Char -- period     -- # period
charPeriod = char '.'

charColon :: Parser Char -- colon      -- # colon
charColon = char ':'

charSemicolon :: Parser Char -- semicolon  -- # semicolon
charSemicolon = char ';'

charLbracket :: Parser Char -- lbracket   -- # bracket
charLbracket = char '['
charRbracket :: Parser Char -- rbracket   -- # bracket
charRbracket = char ']'

charLbrace :: Parser Char -- lbrace     -- # brace
charLbrace = char '{'
charRbrace :: Parser Char -- rbrace     -- # brace
charRbrace = char '}'

charLparen :: Parser Char -- lparen     -- # paren
charLparen = char '('
charRparen :: Parser Char -- rparen     -- # paren
charRparen = char ')'

-- keywords:: [Parser String]
-- keywords = [. LIBRA DEFINE HIDE IN END MODULE PRIVATE PUBLIC include == ; :]


-- Example
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving Show

eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n

int :: Parser Expr
int = intNum `bind` (wrap . Lit . read)
-- int = do
--   n <- number
--   wrap (Lit n)

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor = int `pick` parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x `bind` \_ -> wrap f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) `pick` (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

run :: String -> Expr
run = runParser expr

alpha,alphaLower,alphaUpper :: Parser Char
alpha = pick alphaLower alphaUpper
alphaLower = oneOf "abcdefghijklmnopqrstuvwxyz"
alphaUpper = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"












--testing 



ident :: Parser String -- ('WORD' , r"[^'][\w'!@$%^&*_+<>?|\/`~,=-]*"), # function word
ident = many1 $ pick alpha (oneOf "!@$%^&*_+<>?|\\/`~,=-")

-- oneOfString :: [Parser String] -> Parser String
-- oneOfString [] = wrap []
-- oneOfString (p:ps) = Parser 
--            $ \s -> case parse p s of
--                         []  -> parse (oneOfString ps) s
--                         res -> res


                             
foo' x = runParser x testString 

-- testString = "(* import initial library *)#meh"
testString = "\"flksad\\\"jflkjsdf\""

meh = string "#This should break"

bar = runParser floatNum
foo = foo' tP
  -- where tP = (stringAlphabet [many1 $ noneOf "\"", string "\\\""]) 
  where tP = stringLit