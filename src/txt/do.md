do notation {#firstHeading .firstHeading lang="en"}
===================

Contents
--------

-   [1 Translating the *then* operator](#Translating_the_then_operator)
-   [2 Translating the *bind* operator](#Translating_the_bind_operator)
    -   [2.1 The *fail* method](#The_fail_method)
-   [3 Example: user-interactive
    program](#Example:_user-interactive_program)
-   [4 Returning values](#Returning_values)
-   [5 Just sugar](#Just_sugar)
-   [6 Notes](#Notes)

**Monads**

Using `do` blocks as an alternative monad syntax was first introduced way back in the input and output chapter. There, we used `do` to sequence input/output operations, but we hadn't introduced monads yet. Now, we can see that `IO` is yet another monad.

Since the following examples all involve `IO`, we will refer to the computations/monadic values as *actions* (as we did in the earlier parts of the book). Of course, `do` works with any monad; there is nothing specific about `IO` in how it works.

Translating the *then* operator
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

The `(>>)` (*then*) operator works almost identically in `do` notation
and in unsugared code. For example, suppose we have a chain of actions
like the following one:

``` {.de1}
putStr "Hello"   >> 
putStr " "  >> 
putStr "world!" >> 
putStr "\n"
```

We can rewrite that in `do` notation as follows:

``` {.de1}
do putStr "Hello"
      putStr " "
      putStr "world!"
      putStr "\n"
```

This sequence of instructions nearly matches that in any imperative language. In Haskell, we can chain any actions as long as all of them are in the same monad. In the context of the `IO` monad, the actions include writing to a file, opening a network connection, or asking the user for input.

Here's the step-by-step translation of `do` notation to unsugared Haskell code:

``` {.de1}
do action1
   action2
   action3
```

becomes

``` {.de1}
action1 >>
do action2
   action3
```

and so on, until the `do` block is empty.

Translating the *bind* operator
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

The `(>>=)` is a bit more difficult to translate from and to `do`
notation. `(>>=)` passes a value, namely the result of an action or
function, downstream in the binding sequence. `do` notation assigns a
variable name to the passed value using the `<-`.

``` {.de1}
do x1 <- action1
   x2 <- action2
   action3 x1 x2
```

`x1` and `x2` are the results of `action1` and `action2`. If, for
instance, `action1` is an `IO Integer` then `x1` will be bound to an
`Integer`). The stored values are passed as arguments to `action3`,
which returns a third action. The `do` block is broadly equivalent to
the following vanilla Haskell snippet:

``` {.de1}
action1 >>= \ x1 -> action2 >>= \ x2 -> action3 x1 x2
```

The second argument of `(>>=)` is a function specifying what to do with
the result of the action passed as first argument. Thus, chains of
lambdas pass the results downstream. Remember that, without extra
parentheses, a lambda extends all the way to the end of the expression.
`x1` is still in scope at the point we call `action3`. We can rewrite
the chain of lambdas more legibly by using separate lines and
indentation:

``` {.de1}
action1
  >>=
    \ x1 -> action2
      >>=
        \ x2 -> action3 x1 x2
```

That shows the scope of each lambda function clearly. To group things
more like the `do` notation, we could show it like this:

``` {.de1}
action1 >>= \ x1 ->
  action2 >>= \ x2 ->
    action3 x1 x2
```

These presentation differences are only a matter of assisting
readability.^[[1]](#cite_note-1)^

### [§](#The_fail_method "Link to this section")The *fail* method[[edit](/w/index.php?title=Haskell/do_notation&action=edit&section=3 "Edit section: The fail method")]

Above, we said the snippet with lambdas was "broadly equivalent" to the
`do` block. The translation is not exact because the `do` notation adds
special handling of pattern match failures. When placed at the left of
either `<-` or `->`, `x1` and `x2` are patterns being matched.
Therefore, if `action1` returned a `Maybe Integer` we could write a `do`
block like this...

``` {.de1}
do Just x1 <- action1
   x2      <- action2
   action3 x1 x2
```

...and `x1` be an `Integer`. In such a case, what happens if `action1`
returns `Nothing`? Ordinarily, the program would crash with an
non-exhaustive patterns error, just like the one we get when calling
`head` on an empty list. With `do` notation, however, failures are
handled with the `fail` method for the relevant monad. The `do` block
above translates to:

``` {.de1}
action1 >>= f
where f (Just x1) = do x2 <- action2
                       action3 x1 x2
      f _         = fail "..." -- A compiler-generated message.
```

What `fail` actually does depends on the monad instance. Though it will
often rethrow the pattern matching error, monads that incorporate some
sort of error handling may deal with the failure in their own specific
ways. For instance, `Maybe` has `fail _ = Nothing`; analogously, for the
list monad `fail _ = []`.^[[2]](#cite_note-2)^

The `fail` method is an artifact of `do` notation. Rather than calling
`fail` directly, you should rely on automatic handling of pattern match
failures whenever you are sure that `fail` will do something sensible
for the monad you are using.

[§](#Example:_user-interactive_program "Link to this section")Example: user-interactive program[[edit](/w/index.php?title=Haskell/do_notation&action=edit&section=4 "Edit section: Example: user-interactive program")]
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*Note*

We are going to interact with the user, so we will use `putStr` and
`getLine` alternately. To avoid unexpected results in the output, we
must disable output buffering when importing `System.IO`. To do this,
put `hSetBuffering stdout NoBuffering` at the top of your code. To
handle this otherwise, you would explicitly flush the output buffer
before each interaction with the user (namely a `getLine`) using
`hFlush stdout`. If you are testing this code with ghci, you don't have
such problems.

\
 Consider this simple program that asks the user for their first and
last names:

``` {.de1}
nameDo :: IO ()
nameDo = do putStr "What is your first name? "
            first <- getLine
            putStr "And your last name? "
            last <- getLine
            let full = first ++ " " ++ last
            putStrLn ("Pleased to meet you, " ++ full ++ "!")
```

A possible translation into vanilla monadic code:

``` {.de1}
nameLambda :: IO ()
nameLambda = putStr "What is your first name? " >>
             getLine >>= \ first ->
             putStr "And your last name? " >>
             getLine >>= \ last ->
             let full = first ++ " " ++ last
             in putStrLn ("Pleased to meet you, " ++ full ++ "!")
```

In cases like this, where we just want to chain several actions, the
imperative style of `do` notation feels natural and convenient. In
comparison, monadic code with explicit binds and lambdas is something of
an acquired taste.

Notice that example above includes a `let` statement in the `do` block.
The de-sugared version is simply a regular `let` expression where the
`in` part is whatever follows from the `do` syntax.

[§](#Returning_values "Link to this section")Returning values[[edit](/w/index.php?title=Haskell/do_notation&action=edit&section=5 "Edit section: Returning values")]
--------------------------------------------------------------------------------------------------------------------------------------------------------------------

The last statement in `do` notation is the overall result of the `do`
block. In the previous example, the result was of the type `IO ()`, i.e.
an empty value in the `IO` monad.

Suppose that we want to rewrite the example but return an `IO String`
with the acquired name. All we need to do is add a `return`:

``` {.de1}
nameReturn :: IO String
nameReturn = do putStr "What is your first name? "
                first <- getLine
                putStr "And your last name? "
                last <- getLine
                let full = first ++ " " ++ last
                putStrLn ("Pleased to meet you, " ++ full ++ "!")
                return full
```

This example will "return" the full name as a string inside the `IO`
monad, which can then be utilized downstream elsewhere:

``` {.de1}
greetAndSeeYou :: IO ()
greetAndSeeYou = do name <- nameReturn
                    putStrLn ("See you, " ++ name ++ "!")
```

Here, `nameReturn` will be run and the returned result (called "full" in
the `nameReturn` function) will be assigned to the variable "name" in
our new function. The greeting part of `nameReturn` will be printed to
the screen because that is part of the calculation process. Then, the
additional "see you" message will print as well, and the final returned
value is back to being `IO ()`.

If you know imperative languages like C, you might think `return` in
Haskell matches `return` elsewhere. A small variation on the example
will dispel that impression:

``` {.de1}
nameReturnAndCarryOn = do putStr "What is your first name? "
                          first <- getLine
                          putStr "And your last name? "
                          last <- getLine
                          let full = first++" "++last
                          putStrLn ("Pleased to meet you, "++full++"!")
                          return full
                          putStrLn "I am not finished yet!"
```

The string in the extra line *will* be printed out because `return` is
*not* a final statement interrupting the flow (as it would be in C and
other languages). Indeed, the type of `nameReturnAndCarryOn` is `IO ()`,
— the type of the final `putStrLn` action. After the function is called,
the `IO String` created by the `return full` will disappear without a
trace.

[§](#Just_sugar "Link to this section")Just sugar[[edit](/w/index.php?title=Haskell/do_notation&action=edit&section=6 "Edit section: Just sugar")]
--------------------------------------------------------------------------------------------------------------------------------------------------

As a syntactical convenience, `do` notation does not add anything
essential, but it is often preferable for clarity and style. However,
`do` is never used for a single action. The Haskell "Hello world" is
simply:

``` {.de1}
main = putStrLn "Hello world!"
```

Snippets like this one are totally redundant:

``` {.de1}
fooRedundant = do x <- bar
                  return x
```

Thanks to the [monad
laws](/wiki/Haskell/Monads#Monad_Laws "Haskell/Monads"), we can and
should write simply:

``` {.de1}
foo = bar
```

A subtle but crucial point relates to function composition: As we
already know, the `greetAndSeeYou` action in the section just above
could be rewritten as:

``` {.de1}
greetAndSeeYou :: IO ()
greetAndSeeYou = nameReturn >>= \ name -> putStrLn ("See you, " ++ name ++ "!")
```

While you might find the lambda a little unsightly, suppose we had a
`printSeeYou` function defined elsewhere:

``` {.de1}
printSeeYou :: String -> IO ()
printSeeYou name = putStrLn ("See you, " ++ name ++ "!")
```

Now, we can have a clean function definition with neither lambdas or
`do`:

``` {.de1}
greetAndSeeYou :: IO ()
greetAndSeeYou = nameReturn >>= printSeeYou
```

Or, if we had a *non-monadic* `seeYou` function:

``` {.de1}
seeYou :: String -> String
seeYou name = "See you, " ++ name ++ "!"
```

Then we can write:

``` {.de1}
-- Reminder: liftM f m == m >>= return . f == fmap f m
greetAndSeeYou :: IO ()
greetAndSeeYou = liftM seeYou nameReturn >>= putStrLn
```

Keep this last example with `liftM` in mind; we will soon return to
using non-monadic functions in monadic code, and `liftM` will be useful
there.

[§](#Notes "Link to this section")Notes
---------------------------------------

1.  [↑](#cite_ref-1) Actually, the indentation isn't needed in this
    case. This is equally valid:

    ``` {.de1}
    action1 >>= \ x1 ->
    action2 >>= \ x2 ->
    action3 x1 x2
    ```

    Of course, we could use even more indentation if we wanted. Here's
    an extreme example:

    ``` {.de1}
    action1
      >>=
        \
          x1
            ->
              action2
                >>=
                  \
                    x2
                      ->
                        action3
                          x1
                          x2
    ```

    While that indention is certainly overkill, it could be worse:

    ``` {.de1}
    action1
      >>= \
        x1
          -> action2 >>=
            \
              x2 ->
                action3 x1
                  x2
    ```

    That is valid Haskell but is baffling to read; so please don't ever
    write like that. Write your code with consistent and meaningful
    groupings.

2.  [↑](#cite_ref-2) This explains why, as we pointed out in the
    ["Pattern matching"
    chapter](/wiki/Haskell/Pattern_matching#List_comprehensions "Haskell/Pattern matching"),
    pattern matching failures in list comprehensions are silently
    ignored.

**do notation**

**Monads**

[Understanding
monads](/wiki/Haskell/Understanding_monads "Haskell/Understanding monads")
 \>\> [Maybe](/wiki/Haskell/Understanding_monads/Maybe "Haskell/Understanding monads/Maybe") \>\> [List](/wiki/Haskell/Understanding_monads/List "Haskell/Understanding monads/List")
 \>\> **`do` notation**
 \>\> [IO](/wiki/Haskell/Understanding_monads/IO "Haskell/Understanding monads/IO") \>\> [State](/wiki/Haskell/Understanding_monads/State "Haskell/Understanding monads/State")
 \>\> [Additive monads
(MonadPlus)](/wiki/Haskell/MonadPlus "Haskell/MonadPlus")  \>\> [Monad
transformers](/wiki/Haskell/Monad_transformers "Haskell/Monad transformers")

[edit this
chapter](//en.wikibooks.org/w/index.php?title=Template:Haskell_chapter/Monads&action=edit)

\

**[Haskell](/wiki/Haskell "Haskell")**

[Haskell Basics](/wiki/Haskell/Haskell_Basics "Haskell/Haskell Basics")
\>\> [Elementary
Haskell](/wiki/Haskell/Elementary_Haskell "Haskell/Elementary Haskell")
\>\> [Intermediate
Haskell](/wiki/Haskell/Intermediate_Haskell "Haskell/Intermediate Haskell")
\>\> [Monads](/wiki/Haskell/Monads "Haskell/Monads")\
 [Advanced
Haskell](/wiki/Haskell/Advanced_Haskell "Haskell/Advanced Haskell") \>\>
[Fun with Types](/wiki/Haskell/Fun_with_Types "Haskell/Fun with Types")
\>\> [Wider Theory](/wiki/Haskell/Wider_Theory "Haskell/Wider Theory")
\>\> [Haskell
Performance](/wiki/Haskell/Haskell_Performance "Haskell/Haskell Performance")\

* * * * *

[Libraries
Reference](/wiki/Haskell/Libraries_Reference "Haskell/Libraries Reference")
\>\> [General
Practices](/wiki/Haskell/General_Practices "Haskell/General Practices")
\>\> [Specialised
Tasks](/wiki/Haskell/Specialised_Tasks "Haskell/Specialised Tasks")

[edit book
structure](//en.wikibooks.org/w/index.php?title=Template:Haskell_navigation&action=edit)

![](//en.wikibooks.org/wiki/Special:CentralAutoLogin/start?type=1x1)

Retrieved from
"<http://en.wikibooks.org/w/index.php?title=Haskell/do_notation&oldid=2753465>"

[Category](/wiki/Special:Categories "Special:Categories"):

-   [Haskell](/wiki/Category:Haskell "Category:Haskell")

Navigation menu
---------------

### Personal tools {#p-personal-label}

-   [Create
    account](/w/index.php?title=Special:UserLogin&returnto=Haskell%2Fdo+notation&type=signup "You are encouraged to create an account and log in; however, it is not mandatory")
-   [Log
    in](/w/index.php?title=Special:UserLogin&returnto=Haskell%2Fdo+notation "You are encouraged to log in; however, it is not mandatory [o]")

### Namespaces {#p-namespaces-label}

-   [Book](/wiki/Haskell/do_notation "View the content page [c]")
-   [Discussion](/wiki/Talk:Haskell/do_notation "Discussion about the content page [t]")

### Variants[](#) {#p-variants-label}

### Views {#p-views-label}

-   [Read](/wiki/Haskell/do_notation)
-   [Edit](/w/index.php?title=Haskell/do_notation&action=edit "You can edit this page. Please use the preview button before saving [e]")
-   [View
    history](/w/index.php?title=Haskell/do_notation&action=history "Past revisions of this page [h]")

### More[](#) {#p-cactions-label}

### Search

[](/wiki/Main_Page "Visit the main page")

### Navigation {#p-Navigation-label}

-   [Main Page](/wiki/Main_Page "Visit the main page [z]")
-   [Help](https://www.mediawiki.org/wiki/Special:MyLanguage/Help:Contents "Find help on how to use and edit Wikibooks")
-   [Browse
    wiki](/wiki/Wikibooks:Card_Catalog_Office "Check out what Wikibooks has to offer")
-   [Cookbook](/wiki/Cookbook:Table_of_Contents "Learn recipes from around the world")
-   [Wikijunior](/wiki/Wikijunior "Books for children")
-   [Featured
    books](/wiki/Wikibooks:Featured_books "The best of Wikibooks")
-   [Recent
    changes](/wiki/Special:RecentChanges "A list of recent changes in the wiki [r]")
-   [Donations](//donate.wikimedia.org/wiki/Special:FundraiserRedirector?utm_source=donate&utm_medium=sidebar&utm_campaign=C13_en.wikibooks.org&uselang=en "Support Wikibooks")
-   [Random book](/wiki/Special:RandomRootpage)
-   [Using Wikibooks](/wiki/Using_Wikibooks)

### Community {#p-Community-label}

-   [Reading
    room](/wiki/Wikibooks:Reading_room "Discuss Wikibooks-related questions and concerns with others")
-   [Community
    portal](/wiki/Wikibooks:Community_Portal "Find your way around the Wikibooks community")
-   [Bulletin
    Board](/wiki/Wikibooks:Reading_room/Bulletin_Board "Important community news")
-   [Help
    out!](/wiki/Wikibooks:Maintenance "Frequent tasks that you can help with")
-   [Policies and
    guidelines](/wiki/Wikibooks:Policies_and_guidelines "Pages detailing important rules and procedures")
-   [Contact
    us](/wiki/Wikibooks:Contact_us "Alternative methods of communication")

### Tools {#p-tb-label}

-   [What links
    here](/wiki/Special:WhatLinksHere/Haskell/do_notation "A list of all wiki pages that link here [j]")
-   [Related
    changes](/wiki/Special:RecentChangesLinked/Haskell/do_notation "Recent changes in pages linked from this page [k]")
-   [Upload
    file](//commons.wikimedia.org/wiki/Special:UploadWizard "Upload files [u]")
-   [Special
    pages](/wiki/Special:SpecialPages "A list of all special pages [q]")
-   [Permanent
    link](/w/index.php?title=Haskell/do_notation&oldid=2753465 "Permanent link to this revision of the page")
-   [Page
    information](/w/index.php?title=Haskell/do_notation&action=info "More information about this page")
-   [Cite this
    page](/w/index.php?title=Special:CiteThisPage&page=Haskell%2Fdo_notation&id=2753465 "Information on how to cite this page")

### In other languages {#p-lang-label}

-   [](#)

### Sister projects {#p-Sister_projects-label}

-   [Wikipedia](//en.wikipedia.org/wiki/Main_Page)
-   [Wikiversity](//en.wikiversity.org/wiki/Wikiversity:Main_Page)
-   [Wiktionary](//en.wiktionary.org/wiki/Wiktionary:Main_Page)
-   [Wikiquote](//en.wikiquote.org/wiki/Main_Page)
-   [Wikisource](//en.wikisource.org/wiki/Main_Page)
-   [Wikinews](//en.wikinews.org/wiki/Main_Page)
-   [Wikivoyage](//en.wikivoyage.org/wiki/Main_Page)
-   [Commons](//commons.wikimedia.org/wiki/Main_Page)
-   [Wikidata](//www.wikidata.org/wiki/Wikidata:Main_Page)

### Print/export {#p-coll-print_export-label}

-   [Create a
    collection](/w/index.php?title=Special:Book&bookcmd=book_creator&referer=Haskell%2Fdo+notation)
-   [Download as
    PDF](/w/index.php?title=Special:Book&bookcmd=render_article&arttitle=Haskell%2Fdo+notation&oldid=2753465&writer=rdf2latex)
-   [Printable
    version](/w/index.php?title=Haskell/do_notation&printable=yes "Printable version of this page [p]")

-   This page was last modified on 9 January 2015, at 23:14.
-   Text is available under the [Creative Commons Attribution-ShareAlike
    License.](//creativecommons.org/licenses/by-sa/3.0/); additional
    terms may apply. By using this site, you agree to the [Terms of
    Use](//wikimediafoundation.org/wiki/Terms_of_Use) and [Privacy
    Policy.](//wikimediafoundation.org/wiki/Privacy_policy)

-   [Privacy
    policy](//wikimediafoundation.org/wiki/Privacy_policy "wikimedia:Privacy policy")
-   [About Wikibooks](/wiki/Wikibooks:Welcome "Wikibooks:Welcome")
-   [Disclaimers](/wiki/Wikibooks:General_disclaimer "Wikibooks:General disclaimer")
-   [Developers](https://www.mediawiki.org/wiki/Special:MyLanguage/How_to_contribute)
-   [Mobile
    view](//en.m.wikibooks.org/w/index.php?title=Haskell/do_notation&mobileaction=toggle_view_mobile)

-   [![Wikimedia
    Foundation](//bits.wikimedia.org/images/wikimedia-button.png)](//wikimediafoundation.org/)
-   [![Powered by
    MediaWiki](//bits.wikimedia.org/static-1.25wmf21/resources/assets/poweredby_mediawiki_88x31.png)](//www.mediawiki.org/)

