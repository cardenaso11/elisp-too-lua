{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module ElispParse.ElispParserSpec
    ( spec
    )
where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec               as M
import           Text.Megaparsec.Char
import qualified Data.Vector                   as V
import           Data.Void
import qualified Data.Text                     as T
import Text.RawString.QQ

import           ElispParse.Common
import           ElispParse.ElispParser

spec = do
    describe "parseProgram" $ do
        let runParseProgram = parseText parseProgram

        let oneTwoThree = (ASTInt <$> [1,2,3]) in do
            it "parses quoted expressions" $ do
                shouldParse (runParseProgram
                    "'(1 2 3)")
                    (ASTQuote oneTwoThree)
                shouldParse (runParseProgram
                    "'(a b c)")
                    (ASTQuote $ ASTIdentifier . Identifier <$> ["a","b","c"])
            it "parses nested quoted expressions" $ do
                shouldParse (runParseProgram
                    "'( '(1 2 3) '(1 2 3) '(1 2 3))")
                    (ASTQuote $ replicate 3 (ASTQuote oneTwoThree))
                shouldParse (runParseProgram
                    "'( (1 2 3) (1 2 3) (1 2 3))")
                    (ASTQuote $ replicate 3 (ASTList oneTwoThree))

        let fourFiveSix = ASTInt <$> [4,5,6] in do
            it "parses lists of expressions" $ do
                shouldParse (runParseProgram
                    "(4 5 6)")
                    (ASTList fourFiveSix)
            it "parses nested lists of expressions" $ do
                shouldParse (runParseProgram
                    "(   (4 5 6) (4 5 6) (4 5 6) )")
                    (ASTList $ replicate 3 (ASTList fourFiveSix))

        do
            it "parses backquoted expressions" $ do
                shouldParse (runParseProgram
                    "`(1 ,2 3)")
                    (ASTBackquote
                         [ Quoted (ASTInt 1)
                         , Unquoted (ASTInt 2)
                         , Quoted (ASTInt 3)
                         ])

            it "parses backquoted expressions containing identifiers" $ do
                shouldParse (runParseProgram
                    "`(1 ,2 abc)")
                    (ASTBackquote
                         [ Quoted (ASTInt 1)
                         , Unquoted (ASTInt 2)
                         , Quoted (ASTIdentifier (Identifier "abc"))
                         ])

        let emptyVector = ASTVector (HashableVector V.empty)
            zeroOneTwo = ASTVector (HashableVector (V.generate 3 ASTInt)) in do
            it "parses the empty vector" $ do
                shouldParse (runParseProgram
                    "[]")
                    emptyVector
            it "parses non-empty vectors" $ do
                shouldParse (runParseProgram
                    "[0 1 2]")
                    zeroOneTwo

        let hashConstructor = ASTIdentifier (Identifier "hash-table")
            table = ASTTable (hashConstructor : map ASTInt [1, 10, 2, 20]) in do
            it "parses tables" $ do
                shouldParse (runParseProgram
                    "#s(hash-table 1 10 2 20)")
                    table

        do
            it "parses identifiers" $ do
                shouldParse (runParseProgram
                    "garfield")
                    (ASTIdentifier (Identifier "garfield"))

        let a = ASTChar 'a'
            b = ASTChar 'b'
            c = ASTChar 'c'
            newlnC = ASTChar '\n'
            tabC = ASTChar '\t' in do
            it "parses character literals" $ do
                shouldParse (runParseProgram
                    "?a")
                    a
                shouldParse (runParseProgram
                    "?b")
                    b
                shouldParse (runParseProgram
                    "?c")
                    c
                
            it "parses escaped character literals" $ do
                shouldParse (runParseProgram
                    "?\\n")
                    newlnC
                shouldParse (runParseProgram
                    "?\\t")
                    tabC
            
                    
        let owo = [r|
                Rawr x3 nuzzles how are you pounces on you you're so warm o3
                o notices you have a bulge o: someone's happy ;) nuzzles yo
                u r necky wecky~ murr~ hehehe rubbies your bulgy wolgy you'r
                e  so big :oooo rubbies more on your bulgy wolgy it doesn't 
                sto p growing ·///· kisses you and lickies your necky daddy 
                liki es (; nuzzles wuzzles I hope daddy really likes $: wigg
                les b utt and squirms I want to see your big daddy meat~ wig
                gles b utt I have a little itch o3o wags tail can you please
                get my itch~ puts paws on your chest nyea~ its a seven inch
                itch r ubs your chest can you help me pwease squirms pwetty
                pwease  sad face I need to be punished runs paws down your 
                chest and bites lip like I need to be punished really good~ 
                paws on y our bulge as I lick my lips I'm getting thirsty. I
                can go fo r some milk unbuttons your pants as my eyes glow 
                you smell s o musky :v licks shaft mmmm~ so musky drools all
                over your c ock your daddy meat I like fondles Mr. Fuzzy Ba
                lls hehe puts snout on balls and inhales deeply oh god im so
                hard~ licks  balls punish me daddy~ nyea~ squirms more and 
                wiggles butt I love your musky goodness bites lip please pun
                ish me licks l ips nyea~ suckles on your tip so good licks p
                re of your cock salty goodness~ eyes role back and goes ball
                s deep mmmm~ mo ans and suckles|]
            string2 = [r|bc#5&\\2|]
            eee = [r|#&!\\\\><///|]
            escape = read . T.unpack
            quote t = T.cons '"' $ T.snoc t '"' in do

                it "parses string literals" $ do
                    shouldParse (runParseProgram $
                        quote owo)
                        (ASTString owo)
                    shouldParse (runParseProgram $
                        quote string2)
                        (ASTString . escape . quote $ string2)
                    shouldParse (runParseProgram $
                        quote eee)
                        (ASTString . escape . quote $ eee)

        it "parses escaped string literals" $ do
            shouldParse (runParseProgram
                [r|"\n"|])
                "\n"
            shouldParse (runParseProgram
                [r|"\t"|])
                "\t"

        
        let d = ASTIdentifier $ Identifier "d"
            one = ASTInt 1
            two = ASTInt 2 in do
            it "parses individual cons cells" $ do
                shouldParse (runParseProgram
                    "(1 . 2)")
                    (ASTCons [one] two)

            it "parses improper lists as cons cells" $ do
                shouldParse (runParseProgram
                    "(1 d . 2)")
                    (ASTCons [one, d] two)
            
            it "parses character tables" $ do
                shouldParse (runParseProgram
                    "#^[1 2 d]")
                    (ASTCharTable [one, two, d])

            it "parses character subtables" $ do
                shouldParse (runParseProgram
                    "#^^[d 2 1]")
                    (ASTCharTable  [d, two, one])