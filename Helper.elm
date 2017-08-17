module Helper exposing (project)

import Html exposing (Html, div, h1, text, a)
import Html.Attributes exposing (style, href)
import Markdown
import Array exposing (Array)


title : Int -> Html msg
title day =
    case Array.get (day - 1) titles of
        Nothing ->
            Debug.crash "No day like the present"

        Just str ->
            h1 [ style [ ( "padding", "5px 30px" ) ] ] [ text <| "Day " ++ toString day ++ ": " ++ str ]


link : Int -> Html msg
link day =
    case Array.get (day - 1) titles of
        Nothing ->
            a [] []

        Just str ->
            a [ href <| "http://fishgold.co/codecember16/Day" ++ toString day ]
                -- a [ href <| "http://localhost:8000/Day" ++ toString day ++ "/Main.elm" ]
                [ text <| "Day " ++ toString day ++ ": " ++ str ]


project : Int -> (model -> Html msg) -> model -> Html msg
project day proj model =
    let
        today =
            Array.get (day - 1) titles

        yesterday =
            Array.get (day - 2) titles

        tomorrow =
            Array.get (day) titles

        description =
            Array.get (day - 1) descriptions |> Maybe.withDefault ""

        header =
            div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "row" )
                    , ( "align-items", "baseline" )
                    , ( "justify-content", "center" )
                    ]
                ]
                [ link (day - 1), title day, link (day + 1) ]
    in
        div
            [ style [ ( "font-family", "sans-serif" ) ] ]
            [ div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-direction", "column" )
                    , ( "align-items", "center" )
                    ]
                ]
                [ div [ style [ ( "padding", "50px 0" ) ] ] [ proj model ]
                , header
                ]
            , Markdown.toHtml
                [ style
                    [ ( "margin", "0 auto" )
                    , ( "width", "70vw" )
                    , ( "max-width", "800px" )
                    , ( "padding-bottom", "50px" )
                    ]
                ]
                description
            ]


titles : Array String
titles =
    Array.fromList
        [ "Argyle"
        , "RYB"
        , "Anatoly"
        , "Frequency"
        , "Automaton"
        , "Poisson"
        , "Loops"
        , "Cradle"
        , "Nicky"
        , "Starfield"
        , "Parallax"
        , "Url Parallax"
        , "DVD"
        , "Moiré"
        , "Polyomino"
        , "Tiles"
        , "Headache"
        , "Rotating Polygon"
        , "Matrix"
        , "Gravity"
        , "Star System"
        , "Spirograph"
        , "Braids"
        , "Koalas"
        , "Boids"
        , "Jam"
        , "Areas"
        , "Borders"
        , "Waves"
        , "Lightning"
        ]


descriptions : Array String
descriptions =
    Array.fromList
        [ """
This was my first project.
I wanted to make something similar to [this](https://avh4.github.io/codevember-2016/day-3/) by Aaron VonderHar and I like argyle.

## Instructions
Hit enter to randomize.
"""
        , """
**Originally from May 2014**

I came across the RYB color space in [Paletton](http://paletton.com) and thought it was pretty.

I wanted to make one of my own. The weird and unintuitive equation in the Elm file comes
from the python file, which is a slight modification of the one I wrote originally,
based on [Paint Inspired Color Compositing](http://bahamas10.github.io/ryb/assets/ryb.pdf) by N. Gossett and B. Chen.

I used this day's project in many other days.
"""
        , """
**Originally from 2007**

This was inspired by some coding example by my programming teacher from seventh grade, Anatoly Peymer.
That example (of random numbers) was the first computer generated art I probably ever saw,
and it really stuck with me.

This is a little different from what Anatoly made (8-way symmetry instead of 4-)
but credit goes to him.

## Instructions
Hit space to restart the process.
Hit enter to skip to the final result.
"""
        , """
**Originally from 2013**

This is a reimplementation of [this](https://xkcd.com/1331/) by Randall Monroe.

In 2013 I implemented it in Mathematica, which was probably the weirdest choice anyone could make.
"""
        , """
I wanted to make this ever since I saw Stephen Wolfram's
[TED talk](https://www.ted.com/talks/stephen_wolfram_computing_a_theory_of_everything).

## Instructions
This works in rows. The first row is white everywhere except in the middle, where it's black.

Each row is determined pixel by pixel:
The pixel checks the three pixels above it (above to the left, just above it, and above to the right).
Based on the colors of these pixels its own color is determined by the tetris pieces.

When you click one of the tetris pieces it changes the color of the bottom pixel and the rule is applied.

### Example
If you click the tetris piece that is

    |white|black|white|
          |white|

then the bottom pixel (currently white) will turn black.
Now the rule means that whenever a pixel is black
and the pixel to the left and the one to the right are white,
then the pixel below it will be black.

Stephen Wolfram explains it better.

## Discussion

This is *very* slow.
"""
        , """
This was probably the most difficult project I made. I rewrote it about four times.

This is an example of the Poisson Disk sampling algorithm, which produces
points which are randomly distributed *and* aren't too densly packed.
This produces a more natural pattern than random sampling, where there could be
points which are very close to other points and some which are relatively isolated.

It's based on [Mike Bostock's version](https://bl.ocks.org/mbostock/19168c663618b7f07158),
which is based on [Jason Davies's version](https://www.jasondavies.com/poisson-disc/).
I first encountered this algorithm in
[Mike's very good talk](https://bost.ocks.org/mike/algorithms/) about visualizing alorithms.

## Discussion

This is *so much slower* than the two linked versions.
Probably because Elm doesn't handle random stuff very well.
I tried using ports, which helped a little.

This might also be due to some inefficient code, but I don't think an Elm version could
produce results as impressive as Jason's.
"""
        , """
**Originally from October 2014**

In 2014 someone showed me [snarXiv](http://snarxiv.org), which is a random scientific paper generator.
It uses [Backus-Naur Form](https://en.wikipedia.org/wiki/Backus–Naur_form)
to define the syntax and a perl script translates it to OCaml code.

I made a BNF parser in Python, which generated beautiful, disgusting, lazy JS code.
For some reason I decided to make a [random walk example](http://fishgold.co/BNF/paths).
The result was very pretty, so I wanted to reimplement it in a way that wasn't super weird.

## Instructions

Hit enter to randomize.
"""
        , """
In 2014 I went to a hackathon with a friend.
We made a tool that helped teach high-school students geometry using gamification (ugh.)
I was in charge of the graphic part, where you could see the diagram,
highlight parts of it (vertices, edges and angles,) and solve the given problem.

The vertices' positions and the nodes connecting them were defined in Python,
and then a horrible, disgusting, hacky JS code was generated to actually display it.
It was honestly just horrible. I used Canvas instead of SVG and wrote so many hacks.

This was the first time I wrote JS code and it actually went fine.
We were among the 10 best groups, but that was probably because we did something
educational and not because it was good.

When I showed my horrible hack to someone, he asked my why I didn't use [D3](https://d3js.org).
I probably kept a link to the D3 homepage open on my phone for a few months,
but in 2015 I finally got into D3 and reimplemented the whole thing with much fewer hacks.

Anyway, my point is that I wanted to make it easier to create these diagrams.
That idea evolved into whatever this is.

## Instructions

Use the mouse

* When you hover near a vertex you move it.
* When you hover near an edge away from a vertex you split it.
* When you hold the mouse button the above rules are ignored.
"""
        , """
This is an homage to [Nicky Case](http://ncase.me)'s
[Sight and Light](http://ncase.me/sight-and-light/).

It's more of a reimplementation than an homage, but still <3

Here's a link to [Bret Victor](http://worrydream.com), because why not.
"""
        , """
This is a Windows screen saver from my childhood.
I couldn't find a lot of information about it so I'm not even sure if it's
from Windows 98 or 95.

Anyway, this was a snack in preparation for tomorrow's project.
"""
        , """
The year was 2015. I took a Python course even though I already knew Python.
During the lectures I mostly played
[VVVVVV](http://thelettervsixtim.es)
and [Luftrausers](http://luftrausers.com).

At some point I tried to find something on GitHub and I was hypnotized by the
[parallax](https://en.wikipedia.org/wiki/Parallax) in the 404 page.
Faking 3D in CG is something I wanted to do, so here we are.

These could have been just squares, but I made them cubes because of
[xkcd](https://xkcd.com/8/)
and because it looks so much better than just squares.

This was super fun.

## Instructions

Move the mouse ¯\\\\\\_(ツ)\\_/¯
"""
        , """
After I saw the parallax in GitHub's 404 page (see previous day), I found
a jQuery library for parallax, and it even had parallax in the url.
I tried to do the same but I'm not sure I succeeded.

This was pretty much the most disappointing day.

## Instructions

Move the mouse ¯\\\\\\_(ツ)\\_/¯
"""
        , """
CHILDHOOD MEMORIES
"""
        , """
I love [Moiré](https://en.wikipedia.org/wiki/Moiré_pattern).

Especially on the chairs in the conference room where I work
(they're made of a perforated fabric that stretches around a metal frame,
so there are holes on the front and on the back and they make cool patterns.)

Here's a link to
[the Sampling Theorem](https://en.wikipedia.org/wiki/Nyquist–Shannon_sampling_theorem)
which is relevant, and to
[Seeing Circles, Sines, and Signals](https://jackschaedler.github.io/circles-sines-signals/sincos.html),
which is important to me because it led me to
[Mike Bostock's talk](https://bost.ocks.org/mike/algorithms/)
about visualizing alorithms, which was mentioned in day 6.
"""
        , """
When I was in elementary school I used to draw crosses on my grid notebooks.
This turned to interest in plane tiling, although I never actually read anything
on the subject. I'm not sure how I got to [this video](https://vimeo.com/170534791)
by [Andrew Winslow](http://andrewwinslow.com) explaining a paper of his.
I probably googled for "plane tiling polyomino".

Anyway, in this video he shows several criteria for tiling the plane with
polyominos (closed connected shapes on a two dimensional grid) and I took the simplest one,
which is the Beauquier-Nivat condition.

These randomized shapes (polyominos) satisfy the BN condition,
and therefore can tile the plane. More on that tomorrow.

Here's [something helpful maybe](http://www.crm.umontreal.ca/Words07/pdf/provencalslides.pdf)
about the topic.

I'm very disappointed in myself for not writing down my sources.

## Instructions

Hit enter to randomize.
"""
        , """
A continuation from the previous day.

I think this is nice.

## Instructions

Hit enter to randomize.
"""
        , """
This was a much needed rest from the previous days' projects,
which were a little intense.

It was inspired mostly by optical illusions.
"""
        , """
**Originally from May 2015**

I'm not sure what inspired me in May 2015. It probably had something to do with
spirographs (more on that later) and Monument Valley.
"""
        , """
Around 2008 I found an series of articles about development for beginners.
It was written by [Ido Gendel](http://www.idogendel.com/),
its name was a pun, and it discussed all sorts of topics. It was great.
Almost 10 years later I still remember it and I still think it's great.

There are two articles I especially liked.
[This](http://net.nana10.co.il/Article/?ArticleID=613745) is one of them,
and this project is a reimplementation of it. The second one is yet to come.
"""
        , """
**Originally from March 2009**

In 2009 or so I saved up enough money to buy an iPod Touch.
I started getting into Objective-C.
I'm not sure if I tried writing apps before I actually got an iPod,
but that's not important, because over the years I started learning Obj-C (and
quit soon after) about five times. Swift is what finally made it stick :)

Anyway, my first project was this:
you tap the screen and a square appers and starts growing.
When you let go, it jumps up a little and drops below the screen.
It was very fun.
Version 1.1 included fart sounds when you let the square go.

## Instructions

Use the mouse to make tiny circles or just look at the ones in the big circle.
"""
        , """
**Originally from May 2015**

I think I saw something similar to this on [bl.ocks.org](https://bl.ocks.org)
and wanted to make this. Space is nice, but this isn't physically accurate at all.

## Instructions

Hit enter to randomize.
"""
        , """
[Spirographs](https://en.wikipedia.org/wiki/Spirograph) are neat!
I got one in a Kinder Surprise in 2014.

I thought I'd have a lot to say about every project but I was wrong.
"""
        , """
Inspired by [this](http://sortvis.org), which was mentioned in
[Mike Bostock's talk about visualizing algorithms](https://bost.ocks.org/mike/algorithms/).

This was fun but also annoying.
"""
        , """
[The Useless Web](http://www.theuselessweb.com) is a beautiful website.
It brought me to [koalas to the max](http://koalastothemax.com).

This is an homage, mostly because recreating it would have been dificult.
"""
        , """
[Boids](https://en.wikipedia.org/wiki/Boids) are cool.
I don't know when I first encountered them, but they sure are cool.
"""
        , """
I like systems where local changes cause a global trend.
Among all these projects, four are examples of such systems, including this one.

At first I tried to come up with a model for trafic, but that proved to be much
harder than I expected. I did some googling and found
[this beautiful project](https://github.com/volkhin/RoadTrafficSimulator)
which uses the
[Intelligent Driver Model](https://en.wikipedia.org/wiki/Intelligent_driver_model).

This was a lot of fun.

## Instructions

Hover over the ring road to create a trafic jam.
"""
        , """
This seemed like a cool concept.
I was actually going for what I accomplished in the next day, but it took some time.

I tried several implementations but I think I like the result.

This isn't inspired by anything specific.
Nothing is as it seems and everything is random.

## Instructions

Click and drag to make shapes.
You can use the "Cross" option to make this faster.
Disconnected areas automatically get a different color. When two areas meet
they merge.

"""
        , """
In my opinion this is cooler than yesterday's project.

## Instructions

Click and drag to make shape outlines.
You can use the "Cross" option to make this faster.
Disconnected areas automatically get a different color. When two outlines meet
they merge.
"""
        , """
**Originally from May 2015**

Physics is fun.

## Instructions

Click an empty space to place a "reflector".
Click and hold an empty space to generate a wave.
The wave will progress and its intensity will decrease.
When it hits a reflector, a secondary wave will be emitted from it.
"""
        , """
This is based on
[another article](http://10tv.nana10.co.il/Article/?ArticleID=552882)
by [Ido Gendel](http://www.idogendel.com/). It was almost certainly my favorite.

This article introduced me to the concept of
[fractals](https://en.wikipedia.org/wiki/Fractal),
which was one of the reasons I wanted a degree in Math.
"""
        ]
