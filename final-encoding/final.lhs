> {-# LANGUAGE FlexibleInstances #-}
> import Control.Monad.State

Kodowanie końcowe
--------------

Klasycznie w Haskellu wyrażenia reprezentujemy w następującej postaci:

> data Exp 
>   = Lit Int
>   | Add Exp Exp
>   | Neg Exp

> exp1 :: Exp
> exp1 = Add (Add (Lit 2) (Lit 4)) (Neg (Lit 3))

Aby móc wykorzystać to wyrażenie musimy stworzyć jego interpreter

> eval :: Exp -> Int
> eval (Lit n) = n
> eval (Add e1 e2) = eval e1 + eval e2
> eval (Neg e) = - (eval e)

> pretty :: Exp -> String
> pretty (Lit n) = show n
> pretty (Add e1 e2) = "(" ++ pretty e1 ++ " + " ++ pretty e2 ++ ")"
> pretty (Neg e) = "(-" ++ pretty e ++ ")"

Dla naszego prostego DSLa, możemy zdefiniować także tzw. kodowanie końcowe

> type Repr = Int 
>
> finLit :: Int -> Repr
> finLit n = n
> 
> finAdd :: Repr -> Repr -> Repr
> finAdd e1 e2 = e1 + e2
> 
> finNeg :: Repr -> Repr
> finNeg e = - e

> finExp1 :: Repr
> finExp1 = finAdd (finAdd (finLit 2) (finLit 4)) (finNeg (finLit 3))

W tej postaci jest to tak właściwie połączenie składni z interpreterem, przez co nie możemy wykorzystać tego wyrażenia do innego celu, np. do konwersji na tekst.

Chcielibyśmy w jakiś sposób mieć możliwość zachowania możliwości wyboru interpretacji, bez konieczności definiowania nowego zestawu funkcji. Z pomocą przychodzą typeclassy!

> class FinExp repr where
>   lit :: Int -> repr
>   add :: repr -> repr -> repr
>   neg :: repr -> repr
>
> instance FinExp Int where
>   lit n = n
>   add e1 e2 = e1 + e2
>   neg e = - e
>
> exp1' = add (add (lit 2) (lit 4)) (neg (Lit 3))

Możemy teraz zdefiniować także inny interpreter:

> instance FinExp String where
>   lit n = show n
>   add e1 e2 = concat ["(", e1, " + ", e2, ")"]
>   neg e = "(-" ++ e ++ ")"

Aby wykorzytać wartość dodajemy selektor, który ustali typ:

> evalFin :: Int -> Int 
> evalFin = id

> prettyFin :: String -> String
> prettyFin = id

W tym momencie możemy też odzyskać z kodowania końcowego nasze pierwotne drzewo

> instance FinExp Exp where
>   lit = Lit
>   add = Add
>   neg = Neg
>
> initialize :: Exp -> Exp
> initialize = id

Oraz uzyskać kodowanie końcowe z drzewa

> finalize :: FinExp a => Exp -> a
> finalize (Lit n) = lit n
> finalize (Add e1 e2) = add (finalize e1) (finalize e2)
> finalize (Neg e) = neg (finalize e)

Widać zatem, że jesteśmy w stanie zakodować te same operacje, co w kodowaniu początkowym, jednak czy warto jest komplikować sobie życie przez dodawanie jakichś klas?

Rozważmy teraz następujący przypadek: nastąpiła zmiana wymagań odnośnie naszego języka. Oprócz wcześniejszych operacji, musimy wspierać także dodatkowo operację mnożenia.

W przypadku kodowania początkowego, musimy uaktualnić wszystkie już napisane interpretery, aby zapewnić całkowitość dopasowań do wzorca, co nie brzmi kusząco.

Natomiast w wypadku kodowania końcowego tworzymy nową klasę:

> class MultExp repr where
>   mult :: repr -> repr -> repr

Oraz dodajemy interpretery dla wymaganych typów reprezentacji:

> instance MultExp Int where
>   mult e1 e2 = e1 * e2

W tym momencie, cały kod korzystający jedynie z klasy `FinExp` nadal działa poprawnie, natomiast wyrażenie wykorzystujące klasę `MultExp`

> multExp :: (FinExp r, MultExp r) => r
> multExp = add (mult (lit 2) (lit 3)) (lit 1)

możemy zinterpretować jako `Int`:

    *Main λ: (multExp :: Int)
    7

W przypadku próby interpretacji jako `String` nastąpi błąd

    *Main λ: (multExp :: String)

    <interactive>:20:2: error:
        • No instance for (MultExp [Char]) arising from a use of ‘multExp’
        • In the expression: (multExp :: String)
            In an equation for ‘it’: it = (multExp :: String)

ponieważ nie zdefiniowaliśmy interpretera `MultExp` dla `String`.

Zastosowania
------------

> class Monad repr => Console repr where
>   write :: String -> repr ()
>   prompt :: repr String
>   beep :: repr ()

> instance Console IO where
>   write = putStrLn
>   prompt = getLine 
>   beep = putStrLn "beep"

> data ConsoleMock = ConsoleMock {
>   input :: [String],
>   output :: [String]      
> } deriving Show
>
> instance Console (State ConsoleMock) where
>   write str = modify (\(ConsoleMock i o) -> ConsoleMock i (str:o))
>   prompt = do
>     s <- get
>     case input s of
>       [] -> return ""
>       (i:is) -> do
>         put s{input=is}
>         return i
>   beep = modify (\(ConsoleMock i o) -> ConsoleMock i ("beep":o))

> greet :: Console c => c ()
> greet = do
>   write "Insert name:"
>   name <- prompt
>   write $ "Hello " ++ name
>   beep
>
> runMock = print $ execState greet (ConsoleMock ["Szymon"] [])

Do poczytania
--------------

http://okmij.org/ftp/tagless-final/course/lecture.pdf
