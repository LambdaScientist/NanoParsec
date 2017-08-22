{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}


import NanoParsec

import GHC.Types
import GHC.Num

-- Defining Error myself is a lot of work
-- Defining ConcatMap myself is a lot of work
import Prelude ( error, concatMap, Show
               , putStr, getLine, (++)
               , read, show, fromEnum
               , (<=), fromIntegral, Eq
               , elem, (==), ($)
               , undefined, map
               ) 



-- rules ReservedWord ::=
--     "=="     | "MODULE" | "PRIVATE" 
--   | "PUBLIC" | "END"    | "HIDE"
--   | "IN"     | "DEFINE" | "LIBRA"  ;

--  "=="     | "MODULE" | "PRIVATE" 
--   | "PUBLIC" | "END"    | "HIDE"
--   | "IN"     | "DEFINE" | "LIBRA"  ;

