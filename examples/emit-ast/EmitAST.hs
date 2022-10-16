{-# LANGUAGE OverloadedStrings #-}

module EmitAST where

import Data.Text (Text)

import Text.Emit (Doc) 
import Text.Emit qualified as Emit

--------------------------------------------------------------------------------

-- data Term 
--   = Var String 
--   | Exp [Term]

-- docTerm :: Term -> Doc a 
-- docTerm (Var name) = Emit.string name
-- docTerm (Exp exps) = Emit.parens (Emit.sep ", " (map docTerm exps))

test :: Doc a
test = Emit.text "a" <> Emit.text "a"

-- main :: IO ()
-- main = pure ()