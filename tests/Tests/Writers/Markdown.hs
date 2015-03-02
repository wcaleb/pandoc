{-# LANGUAGE OverloadedStrings #-}
module Tests.Writers.Markdown (tests) where

import Test.Framework
import Text.Pandoc.Builder
import Text.Pandoc
import Tests.Helpers
import Tests.Arbitrary()

markdown :: (ToString a, ToPandoc a) => a -> String
markdown = writeMarkdown def{ writerReferenceLinks = True } . toPandoc

{-
  "my test" =: X =?> Y

is shorthand for

  test markdown "my test" $ X =?> Y

which is in turn shorthand for

  test markdown "my test" (X,Y)
-}

infix 4 =:
(=:) :: (ToString a, ToPandoc a)
     => String -> (a, String) -> Test
(=:) = test markdown

tests :: [Test]
tests = [ "indented code after list"
             =: (orderedList [ para "one" <> para "two" ] <> codeBlock "test")
             =?> "1.  one\n\n    two\n\n<!-- -->\n\n    test"
        , "list with tight sublist"
             =: bulletList [ plain "foo" <> bulletList [ plain "bar" ],
                             plain "baz" ]
             =?> "-   foo\n    -   bar\n-   baz\n"
        , "shortcut reference link"
           =: (para (link "/url" "title" "foo"))
           =?> "[foo]\n\n  [foo]: /url \"title\""
        , "link followed by another link"
           =: (para ((link "/url1" "title1" "first") <> (link "/url2" "title2" "second")))
           =?> "[first] [second]\n\n  [first]: /url1 \"title1\"\n  [second]: /url2 \"title2\""
        ]
