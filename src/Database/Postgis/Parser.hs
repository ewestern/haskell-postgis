{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, OverloadedStrings #-}

import qualified Data.ByteString as B
import Control.Monad.State.Lazy

type Failure   r = [String]     -> String -> Either String (r, B.ByteString)
type Success a r = B.ByteString -> a      -> Either String (r, B.ByteString)

{-type Parser = State-}
{-newtype Parse-}
newtype Parser a = Parser {
   unGet :: forall r. B.ByteString
                    -> Failure   r
                    -> Success a r
                    -> Either String (r, B.ByteString) }


