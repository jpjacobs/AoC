NB. AoC, 2025
{{
if. -. fexist '~addons/general/jaoc' do.
  install'github:jpjacobs/general_jaoc'
  echo 'installed general/jaoc; go set up COOKIE.txt'
end.
}}''
load'general/jaoc'
1!:44 jpath '~AoC/' NB. Set ~AoC in ~config/folders.cfg
'data25/'setup_aoc_ 2025 NB. Setup this year
NB. Notes:
NB.  - This file uses jaoc for download/upload/day organisation
NB.  - Each part of each day solves the entire problem, from the problem text up.
NB.  - The solution don't depend on any library; sometimes plot or viewmat used for visualisation.
NB.  - Recommended settings for vim: sw=2 ts=2 et fdm=marker foldmarker={{,}}

NB. Spin up threads up to # of cores & set boxdraw to ASCII (for Android)
0&T.@0^:(0>._1+([: {. 8&T.)-1&T.) ''
boxdraw_j_ 1 NB. set ascii boxes as others ugly on Android

NB. Daily solutions:
1 day {{ NB. Secret Entrance
  par =: 50,0".];._2@:(rplc&'L-R+') NB. start at 50; L=-;R=+
  NB. +/\ is fast, but +m.100/\ slow: reverse, and work from the back.
  sol =: (@:par)([: +/ 0 = +m.100/\.@:|.@:) NB. adverb = A A train; m is pre processing between parsing and solving
  NB. p2 spells out every instruction in single clicks, then does sol
  '`p1 p2'=: ] sol`((|#*) sol)
  0
}}

2 day {{ NB. Gift shop
  NB. Part 1: Find all invalid ideas consisting of a number repeated twice.
  par=: [: ([: ".;._1 '-',]);._1 ',',}:
  to=: ;@:(<@([+i.@(-~/))/"1) NB. List all items in set of ranges 
  NB. reshape repr; if error due to odd, valid, return 0.
  istwice =: ([:-:/2 _$":) :: 0"0
  p1=: +/@(#~istwice)@to@par

  NB. Part 2: Repetition is *at least* twice.
  div=: [: }. (*/ .^"1 (#: i.@(*/))@:>:)&|./@:(__&q:) NB. All divisors of y but 1; see odometer Essay.

  NB. At least twice; Parallelised and not keeping all boxed numbers.
  NB. Sum numbers where any reshape(by div) repeats once for all numbers converted to literal.
  alt =: +/@:".@:(#~[: +./ (_,.~div@#@{.) (1=#@~.@$)"1/ ])@:(":"0)
  NB. Do alt parallel per distinct # digits @ sorting
  bxpd =: (alt t. '';.1~ 1,2~:/\10 <.@^. ])@:/:~
  p2 =: +/@:;@:bxpd@to@par NB. 30% faster than p2a (only... work not equally distributed)

  NB. Older, slower versions
  alta =: ((_,.~div@#) ([: +./ 1=#@~.@$"1) ])@:":"0 NB. At least twice repeated
  NB. Correct but slow (~9s)
  p2_a=: +/@(*alta)@to@par
  NB. Improved: box per number of digits; and do factoring per length
  bxpdb =: (<;.1~ 1,2~:/\10 <.@^. ])@:/:~
  altb =: (]#~[: +./(_,.~div@#@{.) (1=#@~.@$)"1/ ])@:(":"0)
  p2b =: +/@:".@:;@:(altb&.>)@bxpd@to@par NB. 3x faster than p2_slow
0
}}

3 day {{ NB. Lobby
NB. Part 1: In each line of battery joltages, find 2 digits that together are highest 2 digit number. Sum=joltage!
NB.  combine    sel    first,last   imax after first  imax without last
NB. p1=:[:+/ (10 1+/ .*] {~ ] (] ([,1++) (i.>./)@(}.~>:)) (i.>./)@}:)"1@:par NB. reimplemented using part 2's solution.
NB. Part 2: turn on not 2, but 12 batteries in each row.
NB. Fits in int; make recursive so x is num found, noting greedy search is good, as selecting biggest digit first maximizes the total number; length of x is used to behead/curtail enough.
rec =: {{ NB. x: N to select,nums 9o far; y: remainder
  if. (#=1+{.)x do. <.(+/ .* 10^i.@-@#)@}.x return. end. NB. Finished; combine result
  NB.     argmax  y curtailed so x-1 remain
  imax =. (i.>./) (-({.x)-#x)}.y NB. Max num in curtailed array
  (x,imax{y) rec y }.~ 1+imax NB. Recurse on list of digits; shortened array
  }}"1
sol =: +/@:(rec "."+;._2) NB. x: #bats to turn on;y:list
'`p1 p2'=: 2&sol`(12&sol)
tst=:{{)n
987654321111111
811111111111119
234234234234278
818181911112111
}}
}}

4 day {{ NB. Printing department
  NB. Part 1: how many rolls (@) can be removed by having less than 4 neighbours?
  par =: '.@'i.];._2
  pad =: ([,[,~[,.~,.) NB. Add x to each side of matrix y
  NB. Per 3x3 window (padded), middle=@ and max 5 rolls (counting middle as well)
  accessible=:(1 1,:3) (4&{ *. 5>+/)@,;._3 ]
  p1=: [: +/@,@:accessible (0 pad par)

  NB. Part 2: Remove possible rolls until no change; how many removed?
  NB. Third approach is the best, using neighbour indices.
  NB. Add stop; look up shifted coords in originals
  nn =: (,#)@(i. (}.sh) +"1/~])@($#:I.@,)
  p2 =: (, -&(+/) (] (]-] *. 4>+/"1@:{)^:_ (<:>i.)@#)@nn)@par

  NB. Previous versions; for comparison.
  p2a =: (+/@,@:- (- [: accessible 0 pad ])^:_)@par
  NB. 6x Faster alternative: shift&sum
  sh=: >,{;~0 _1 1 NB. shifts, center first
  p2b =: (+/@,@:- (- [:({.*.4>+/@:}.) sh |.!.0])^:_)@:par
  NB. Relative timing:
  NB. (%"1 <./) 10&timespacex@(,&' io''''')&> 'p2a';'p2b';'p2'
  NB. fun visual:
  NB. load'viewmat'
  NB. viewmat +/ (- [: accessible 0 pad ])^:(<_) par io'' NB. colors
  NB. (3#"+i.255) viewmat ^.1++/ (- [: accessible 0 pad ])^:(<_) par io'' NB. gray log
tst=:{{)n
..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
}}
0
}}

5 day {{ NB. Cafeteria
  NB. Find sum of fresh ingredient's ID's (second part of input) falling into any range (first part of input).
  NB. I. matches range excluding first, so add _1 0 to each to correct it to be inclusive. cut by - is nop for ID's
  NB.     corr for Id        cut by - after prepend; for each line (LF before each line by adding).
  par=:[: _1 0&+"1&.>`]"0[: (<@(([:".;._1'-',]);._1);._2~LF2&E.) LF([,,~)]
  NB.  sum  or'ed in ranges between parsed boxes
  p1 =: +/@([: +./ 1 = I."1 0/)&>/@par
  NB. How many id's in total in ranges: note ranges overlap & to big to list. Do range intersection
  NB. intersect if any end of a range are in the other
  int=: 1 e. I. , I.~
  fuse =: (<./@{. , >./@{:)@:,.  NB. fuse ranges
  sbm =: (/: [:-:+/"1)           NB. sort range by mid
  NB. Merge ranges by insert (from back) _ 2$ for first
  NB. tail apd join or fuse~[int{.] fix first sort repeat
  mrg=: ((}.@],~(,:`fuse@.int {.)) _ 2&($,))/@sbm^:_
  NB.    sum   diff merged ranges
  p2 =: [: +/@:(-~/"1)@mrg 0 {:: par
tst=:{{)n
3-5
10-14
16-20
12-18

1
5
8
11
17
32
}}
0
}}

6 day {{ NB. Trash compactor
  NB. TODO: polish so same logic can be psed for p1 & p2
  NB. Part 1: sums/prod of nums in space-separated cols
  NB.  nums but-last row; ops = last without space
  par =: [:(".@}: ,&<~ ' '-.~{:) ];._2
  NB. p1:   sum do   op insert nums transp of parsed
  p1  =: ([:+/@:". ([,.'/',. ":@|:@])&>/)@:par
  NB. Part 2: Fun: numbers are written in cols
  par2=: [:(pn@}: ,&<~ ' '-.~{:) ];._2
  NB.     box num cut by all space pad 2x to separate nums
  pn  =: [: (<@".@,;.1~ [:*./"1' '&=) ' '([,,.)|:
  NB. Nearly the same; but boxed because # lists different
  p2  =: ([:+/@:". ([,.'/',. ":@>@:|:@])&>/)@:par2
  0
}}

7 day {{ NB. Laboratories
  NB. Part 1: how many times is the beam split?
  par=: [: {."(2) _2 ]\ e.&'S^';._2 NB. find char; keep odd rows as empty ones don't matter.
  NB. "and", used at split, but track count.
  andTrack=: {{r[T=: T++/r=. x *. y}}
  NB. fold verb x: new: 1=^; y state/first=beam
  NB.  straight or or / shift LR      ^ and |
  fv =: ((*.-.)~+.[:+./_1 1|.!.0"0 1 andTrack)
  p1 =: {{] F.. fv par y[T=: 0
  T
  }}
  NB. Part 2: count # paths down the grid; 40 for tst.
  NB. recursive aproach; return N; get pos x y; refer to global grid G (without empty lines)
  NB. Brilliant! just removing . from +. and *. makes it work!
  fv2 =: ((*-.)~+[:+/_1 1|.!.0"0 1 *)
  p2  =: +/@:(]F..fv2)@:par
tst=:{{)n
.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
}}
}}

8 day {{ NB. Playground
  NB. given junction box coords, whats the product of #jb for the 3 largest connected circuit after connecting 1000?
  par =: ".;._2
  NB. Fast squared Euclid distance mat
  dt =: [: ; <@([: +/@:*:@|: {. -"1 }.)\.
  co =: ( (#~ </"1)@(,~ #: i.@*:)@# /:  dt)
  mm=:{{ NB. y= par tst/io; x: connections to make
    NB. "Do nothing" also counts as connection
    grs=: <"0 i. #y [all=. <:#y
    for_p. con=. x{.co y do.
      if. +./(#grs) > id=.I. p&(+./@e.)@> grs do. NB. join
        if. 1=#grs=. ((<<<id){grs),<;id{grs do. all=.p_index break. end.
      end.
    end.
    (*/{."1 y{~con{~all);~*/ 3 ({.\:~) #@> grs
  }}
  p1 =: 0{:: 1000    mm    par
  p2 =: 1{::(+/@i.@# mm ])@par
tst=:{{)n
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
}}
}}

9 day {{ NB. Movie theater
  NB. Part 1: largest rectangle area
  par=: ".;._2
  surf =: 1 */"1@:+ [: | -"1
  p1=:[: >./@;@}: [: <@({. >./@surf }.)\. ".;._2
  NB. Part 2: input forms polygon; find largest surface rectangle as before, but must be entirely inside polygon
  NB. Approach: as for p1, but filter surfs based on wether any of the edges intersects any of the polygon

  NB. sides of rect; hor first; vert
  sides =: (2 2 2 2$'abcd'i.'abcbadcdabadcbcd'){,
  NB. hor, vert edges of input
  hv =: [: (]/.~ [: *@:| -/"2) 2 ,:/\ (,{.)
  NB. point locations based on I.  0 1&+ only interior!
  NB. 0 1 2  line intersects h/v if end points:
  NB.  +-+   v: 1 4  1 7  4 7
  NB. 3|4|5  h: 3 4  3 5  4 5
  NB.  +-+
  NB. 6 7 8
  NB. intr; return case no in base-81; x list of edges; y ranges
  intr =: ((4#3)#.I.)
  p2=:{{
    i=. ".;._2 y
    NB. make list of surfs as for p1; $= N
    s=. }: ;@:(<@({. surf }.)\.) i
    NB. make ranges for x and y for each pos rect; order by descending surf; $=Nx(x-y)x(start-end)
    r=: (0 _1 +/:~)"1 (rno=:}: ;@:(<@({. ,."1 }.)\.) i) \: s
    NB. edges of input; as x's, y's
    e=. 2 ,."1/\ (,{.) i
    NB. edges 247 & 249 are long; traversing circle.
    found=: _
    intr=. (,|."2)1 1,:"1([:(,|."1)1 ]\. ]) i. 3
    NB. TODO: Slowish 2.9s. think of:
    NB. - faster replacement for I. (maybe overkill)
    for_rect. r do.
      if. -. intr +./@:(e.!.0)~ > rect I."1"2 e do. NB. if per block; do "2~/
        found =: rect_index
        break.
      end.
    end.
    found ({\:~) s
  }}
  vis=:{{ NB. x rect found; parsed input
  require'plot'
  0 0$pd&.> (<'new'),(<;/|:y),(<;/|:0 1 3 2 0{>,{<"1]0 1+"1 x),<;._1 '.aspect 1.show'
  }}
  vv=: {{(found{r) vis par io''}}
  0
}}

10 day {{ NB. Factory
  NB. parse: separate lights/buttons/joltages
  par=: ([:li`bu`jo"0 ]<;.1~(i.@# e. i.&'[({'));._2
  NB. li=: (2-.~'.#'i.])&.>     NB. light pats: bool 0/1
  li=: (2 #:inv 2-.~'.#'i.|.)&.>     NB. light pats: bool 0/1
  bu=: (2 +/ .^ ".);._2&.>          NB. button con: boxed
  jo=: ([:".rplc&'{ } ')&.> NB. joltages  : num list
  NB. Part 1: button toggles lights in list.
  NB. fewest presses required for making light pattern
  NB. i.e. 1-D lights-out. IIRC order doesn't matter in the 2-D game, so shouldn't in the 1-D case.
  NB. indeed; xor toggle is symmetric, so pressing same button twice is nop.
  NB. possible smart solution:
  NB. sum bits ind of t in xor'd sel from num from buttons
  NB.   +/@#:tar i.~ (xob/@#~ (#&2 #:[:i.2^])@#@]) but
  NB. not sure if ok... as for second example doesn't hold. maybe xor assumption not right... or conversion button list input to num
  NB. TODO: MISMATCH ENDIANNESS LIGHTS & BUTTONS
  NB. recurse: (target,buttons) rec step,states; return step when target in states.
  xorb =: 22 b.
  exp =: {{
    't b'=. y
    seen=.''
    ct=. st=.0
    while. 1 do.
      ct =. >:ct
      NB. rem seen, uniq st xor buttons
      st =. seen -.~ ~. , st xorb/ b
      if. t e. st do. break. end. NB. target found
      'No solutions remain' assert 0<#st 
      seen=. seen, st NB. st is know not to be in seen
    end.
    ct
  }}

  NB. TODO: Check: tac, it, step whether salvagable.
  it =: 0 0 step^:({.@[ -.@:e. }.@])^:_~ ]
  NB. oneliner:
  tac =: [: +/@#: (i.~ (xorb/@#~ (#&2 #:[:i.2^])@#@]))&>/
  NB. optim rec: 0): memoize 1): remove previously encountered.
  step=: >:@{.@] , [: ~.@, }.@[ xob/ }.@]
  NB. p1 =: [: +/ tac@}:"1@:par NB. Arg... right for tst, but not for input 504 too high
  NB. parallel doesn't speed up...
  p1 =: [: +/ exp@}:"1@:par NB. also faster than tac...

  NB. Part 2: now for joltages in col 3. Simply bruteforcing is too slow.
  mp=: +/ .*
  NB. integer Gauss-Jordan; presume working with ints, so no tol. Based on ~addons/math/misc/linear.ijs
  gj =: {{
  'r c'=.$y
  rws=. i.r
  i=.j=.0
  while. (i<r)*.(j<c) do.
    k=. (i.>./)col=.| i}.j{"1 y
    if. 0~:k{col do.
      if. k do.
        y=. (<i,i+k) C. y
      end.
      NB. pivot i,j
      pcol=. j{"1 y
      y =. (([- (pcol-i=i.#y) */ %&(i&{)) j{"1]) y
      i =. >: i
    end.
    j=.>:j
  end.
  y
  }}
  NB. convert to sys & do Gauss-Jordan; x: to keep rational for non-integer solutions to BL mp B = L
  NB.    Light-Button mat  <. if same gj   append L
  tosys =: (|.@|:@#:&.>@[ <.^:(-:<.)@gj@x:@,.&> ])/@}."1
  sol =: {{ NB. takes parsed input
    y=. <@tosys y NB. No check for feasibility, will be stopped by div in any case.
    NB. remove all-zero rows (overdetermined sys); not required.
    y =. (({:@$ ~: 0 +/ .=~ ])@:(}:"1) # ])&.> y
    NB. verify correctness
    sols =. (#~ [: (=<./)+/"1)@(}:"1 rec {:"1)t.''&> y
    NB. assert. J -: JB mp B
    assert. *./;y ([: *./ {:"1@[ -:"1 }:"1@[ mp"_ 1 ])t.''&>  sols 
    NB. score 
    +/+/@{.@> sols
  }}
  NB. All posibilities of dividing indistinguishable objects over distinguishable bins.
  div =: {{ NB. y = # bins; x=number to divide
    assert. x *.&(>:&0) y
    if. x = 0 do. ,:y#0 return. end. NB. 0 to divide: all bins get 0
    if. y = 1 do. ,x    return. end. NB. 1 bin left: all objects in that bin.
    r=. 0$~0,y
    for_p. i. >:x do.
      r=. r,p,. (x-p) div <:y
    end.
  }} M.
  NB. div not enough. Should generalize to divide rhs over differently, rationally sized lhs. 
  NB. All possibilities to divide (possibly rational) rhs using 
  NB. x=rhs to obtain; y=rationally sized bins (all positive); returns coefs for each chunk
  rdiv =: {{
    if. 0=x  do. ,:0#~#y return. end. NB. 0 to divide: all bins get 0
    if. 1=#y do.
      if. (=<.) r=.x%{.y do.  r  return.  else.  0$0 return.  end.
    end. NB. 1 bin left: all objects in that bin if integer, otherwise, no good solution, remove.
    assert. *./ y>:0 NB. both chunks should be non-negative; rhs as well, but checked before.
    r=. 0$~0,#y
    for_p. x i.@>:@:<.@:% f=.{.y do. NB. for each possible coeff for f=.{.y, <. for situations like 61 rdiv 2 2
      r=. r,p,. (x-p*f) rdiv }. y
    end.
  }}
  NB. recursively solve.
  rec =: {{ NB. x: JB, y: J; return B
    if. ((<0 1)&|: -: 0 -.~ ,) x do. 
      if. -. (-: =@i.@#) x do. echo 'Done without x=id' end. 
      r=. ,:y 
    else. NB. Solved if diagonal return. NB. TODO: check: ID mat not enough?
    NB. TODO: Needed? Add check for unsolvable sys : where 1 var is negative; or all zero row in JB ahs non-neg val in J.
    NB. (alt test for idmat as first (=@i.@#) -: ({."1~ #)), should imo be always true.
    NB. Pick eq. with all positive coeff to bruteforce. 
    NB. eqn   =. (i. [:<./ -.&0) ndiv=. (*./"1>:&0 x) * y (([!&<:+)~ +/"1) x             NB. eq num to remove; possibilities for combinations... (likely need removal for rat.)
    eqn   =. (i. >./) ndiv=. (*./"1>:&0 x) * y (([!&<:+)~ +/"1) x             NB. eq num to remove; possibilities for combinations... (likely need removal for rat.)
    NB. echo 'Rec on ',(":eqn{ndiv),' combinations of ',(":+/0<eqn{x),' vars.'   NB. debug output
    varmsk=. 0<eqn{x                                                        NB. non-zero variable coefficients in eqn{x
    vals  =. y (rdiv varmsk&#)&(eqn&{) x                                    NB. values that vars can in eqn{x to satisfy equality.
    if. 0=#vals do. 0#,:varmsk return. end.
    remeq =. (<<<eqn)&{                                                     NB. remove equation eqn.
    Js    =. (remeq y) -"1 guess=. (remeq x) +/ .*"_ 1 varmsk&#inv"1 vals   NB. new RHS, i.e. Js by: fill in vars in sys, and sub from J
    nx    =. (<(<<eqn),(<<I.varmsk)){x                                      NB. remaining equations: remove eq. and vars appearing in them from JB
    'sub nsol' =. nx (;;#&>)@:(<@rec"_ 1) Js                                 NB. Solve remaining sys x,.Js; keep solutions to guessed rhs only where pos. (entirety assured by rdiv).
    pos =. *./"1 (0&<: *. (=<.)) sub
    NB. Fill in fill mask in sub-solutions, and return one with smallest count
    if. +./ pos do.                                          NB. Any valid solutions?
      NB. Should: expand sub to convert back to solutions to original system x,.y; return all possible solutions because longer ones could still be best in combination with higher levels.
      r=.(nsol#vals) (varmsk&#inv"1@[ + (-.varmsk)&#inv"1@])&(pos&#) sub
    else.
      r=.0#,:varmsk
    end. NB. Else return something rational, so filtered out at next layer back up.
  end. 
  r
  }}
  p2 =: sol@par 
tst=: {{)n
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
}}
}}

11 day {{ NB. Reactor
par =: [: |:@:; <@({.,.}.)@s:@:(' ',':'-.~]);._2
  NB. Part 1: count all paths from you to out
  NB. graph cannot have loops between you-out as otherwise infinite paths.
  p1 =: {{
    'you out'=: s: ' you out'
    'i o'  =: par y NB. In/Out nodes
    next =: o #~ i e. ] NB. find next nodes for current ones
  NB. y current node; x paths from you; returns number of paths 
    NB.  multip +/rec uniq next until no other than out
    rec=: (#/.~ +/@:$: ~.)@:next`[@.(0=[:#out-.~])"0
    1 rec you
  }}
tst=:{{)n
aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
}}
  NB. Part 2: all paths from srv via dac & fft to out
  NB. similar approach, but keep paths going down; when reaching out, check if dac&fft on path.
  NB. 1 rec srv crashes, so likely loops etc when not considering dac & fft.
  NB. first try, make paths srv->dac, then dac->fft then fft -> out + srv-> fft fft-> dac dac->out
  p2 =: {{
    'svr dac fft out'=: s: ' svr dac fft out'
    'i o'=. par y     NB. In/Out nodes
    next =: o #~ i e. ] NB. Find next nodes for current ones
    NB. TODO new approach: do next till end; keep dict with min&max depth a node is encountered; when finding way to fft/dac/out drop remove those that have min>max dest
    nodes=. ~.i,o
    'min max' =: |: (2,~#nodes)($,)10000 _1
    nod=. svr
    d =. 0
    while. (0<#nod) *. (d<5000) do. 
      up  =. nodes i. nod 
      min =: d&<.&.(up&{) min NB. min depth it appears
      max =: d&>.&.(up&{) max NB. max depth it appears
      d   =.d+1
      nod =. next nod
    end.
    ni=: nodes&i.
    NB. TODO new rec approach: ds fs rec nod; ret # con to out via nod,fft,dac. ds and fs track whether ds&fs seen.de is depth. Abort when de>max{~nodes i. fft,dac
    mddf =: max {~ nodes i. dac,fft NB. max depth dac&fft
    seeni=: 0 2$a:
    seenv=: 0$0
    rec =: {{
      if. (#seeni)>id =. seeni i. x;y do. NB. memoize
        r=. seenv{~id
        r return. end.
      if. y=out do.
        r=. *./x  NB. 1 if both dac&fft met, 0 otherwise
      else.
        nx=. x>.y=dac,fft NB. new x
        NB. keep next where, if not done, still hope for dac/fft
        nn=. nx (] #~ *./@(>. mddf >:/ min{~ni)) next y
        if. #nn do. r=. nx +/@:rec next y else. r=.0 end. 
        NB. r=. nx +/@:rec next y NB. Without depth check is half as fast due to hopeless cases. note: use timex because of memoizing!
      end.
      seeni=: seeni,x;y
      seenv=: seenv,r
      r
    }}"1 0
    0 0 rec svr NB. run the above.
  }}
tt=:{{)n
svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
}}
}}

12 day {{ NB. Christmas Tree Farm
  NB. How many of the regions can fit all presents listed
  NB. parse gifts  & regions @   split after last LF2
  par =: [: (pg&.>)`(reg&.>)"0 ] split~ 2+LF2 i:&1@:E. ]
  NB. look up;drop 1st+lst per 5 lines 
  pg  =: '.#' i. _5 }.@}:\ ];._2 NB. convert gifts to bool
  reg =: [: 0&".;._2 rplc&'x : ' NB. Keep only numbers
  NB. Do 2 simple tests:
  NB. Sure fit if gifts(3x3) will fit when stacked
  surefit  =: ((3 */@:<.@:%~ 2&{.) >: +/@:(2&}.))"1@]
  NB. Sure fail if area is smaller than total number of occupied squares in gifts.
  surefail =: */@:(2&{.)"1@] < +/@,"_1@[ +/ .*"(1) 2&}."1@]
  NB. Check: if counts of fails&fits is #regions, then all cases are decided. If not, error (and implement if needed)
  check =: 'fit&fail don''t sum'assert #@]=+&(+/)
  p1 =: (surefit (+/@[ [ check) surefail)&>/@:par
  NB. Spoiler: exact solver not needed ;).
0
}}
echo 'To run day 1-12, execute:',LF2,'   run >:i.12'
NB. vim: ts=2 sw=2 et fdm=marker foldmarker={{,}}
