{-# LANGUAGE PatternSynonyms #-}

module ElispParse.Common
    ( AST (..)
    , HashableVector (..)
    , Identifier (..)
    , BackquotedAST (..)
    , Parser
    , BaseParser
    , CompositeParser
    , Fix (..)
    , InfiniteAST
    , parseText
    , _FASTList
    , _FASTIdentifier
    , _FASTIdentifier_
    , pattern FASTList
    , pattern FASTQuote
    , pattern FASTBackquote
    , pattern FASTVector
    , pattern FASTTable
    , pattern FASTCons
    , pattern FASTCharTable
    , pattern FASTCharSubTable
    , pattern FASTByteCode
    , pattern FASTIdentifier
    , pattern FASTIdentifier_
    , pattern FASTFloat
    , pattern FASTInt
    , pattern FASTChar
    , pattern FASTString
    , pattern FASTBoolVector
    , pattern ASTIdentifier_ ) where

import ElispParse.CommonInternal
