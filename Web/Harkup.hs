{-# LANGUAGE FlexibleInstances, BangPatterns #-}

{-| Minimalistic web markup for busy harkupers

@

example :: Elem
example = div § "Hello!" € [
  ul £ ("blocked", "blocked") € [
      li § \"Hello\",
      li § "You have a parachute"
      ]
  ]

@

-}
module Web.Harkup (
  tagElement,
  Elem(..),
  Attributes(..),
  ToAttr(..),
  prettyPrint,
  (£), (€), (§),
  example
  ) where


import Prelude hiding (div)
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | Markup monad, currently not used
type Harkup = Writer [Elem]

-- | An element!
data Elem = Elem {tag      :: !String,
                  attr     :: Attributes,
                  text     :: !(Maybe String),
                  children :: ![Elem]}
  deriving Show


-- | Attributes for an element
type Attributes = Map String (Maybe String)

-- | Types that can easily convert to attributes
class ToAttr a where
  toAttr :: a -> Attributes

instance ToAttr Attributes where
  toAttr = id

instance ToAttr [(String, Maybe String)] where
  toAttr = Map.fromList

instance ToAttr (String, String) where
  toAttr (k,v) = Map.singleton k $ Just v

instance ToAttr String where
  toAttr k = Map.singleton k Nothing


-- | Prettyprint some markup
prettyPrint :: Elem -> String
prettyPrint = pp 0

pp :: Int -> Elem -> String
pp i e = mconcat [indent i $ ppOpTag e,
                  ppContent (i + 1) e,
                  indent i $ ppEdTag e
                 ]

ppOpTag :: Elem -> String
ppOpTag e
    | Map.null (attr e) = mconcat ["<", tag e, ">"]
    | otherwise         = mconcat ["<", tag e, " ", ppAttr e, ">"]

ppAttr :: Elem -> String
ppAttr = concatMap putAttr . Map.toList . attr
    where putAttr a = case a of
                        (k, Just v ) -> mconcat [k, "='", v, "'"]
                        (k, Nothing) -> k

ppEdTag :: Elem -> String
ppEdTag e = case (children e, text e) of
              ([], Nothing) -> ""
              _             -> mconcat ["</", tag e, ">"]

ppContent :: Int -> Elem -> String
ppContent i e = mconcat [textContent,
                         childElements
                        ]
    where
      textContent = case text e of
                      Just t -> indent i t
                      Nothing -> ""
      childElements = case children e of
                        [] -> ""
                        cs -> concatMap (pp i) cs
indent :: Int -> String -> String
indent i str = mconcat [replicate (i*2) ' ', str, "\n"]

-- | Create a custom tag element 
tagElement :: String -> Elem
tagElement s = Elem {tag= s,
                     attr= Map.empty,
                     text= Nothing,
                     children= []}

ul :: Elem
ul  = tagElement "ul"

li :: Elem
li  = tagElement "li"

div :: Elem
div = tagElement "div"


infixr 7 §
-- | Add some text to an element
(§) :: Elem -> String -> Elem
e § str = e {text = Just str}

infixr 7 £
-- | Add some attributes to an ellement
(£) :: ToAttr attrs => Elem -> attrs -> Elem
e £ as = let as'  = toAttr as
             t    = join $ Map.lookup "textContent" as'
             as'' = Map.delete "textContent" as'
         in e { attr= (attr e) `Map.union` as'',
                text= t
              }
infixr 6 €
-- | Add some children elements to an element
(€) :: Elem -> [Elem] -> Elem
e € es = e {children = es}

-- | An example of how you could combine an element
example :: Elem
example = div § "Hello!" € [
  ul £ ("blocked", "blocked") € [
      li § "Hello",
      li § "You have a parachute"
      ]
  ]

