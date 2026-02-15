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

NB. TODO: continue checking with day 10, part two, clarify rec.

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
  NB. Fits in int; make recursive so x is num found, noting greedy search is
  NB.   good, as selecting biggest digit first maximizes the total number; length
  NB.   of x is used to behead/curtail enough.
  rec =: {{ NB. x: N to select,nums 9o far; y: remainder
    if. (#=1+{.)x do. <.(+/ .* 10^i.@-@#)@}.x return. end. NB. Finished; combine result
    NB.     argmax  y curtailed so x-1 remain
    imax =. (i.>./) (-({.x)-#x)}.y NB. Max num in curtailed array
    (x,imax{y) rec y }.~ 1+imax    NB. Recurse on list of digits; shortened array
  }}"1
  sol =: +/@:(rec "."+;._2) NB. x: #bats to turn on;y:list
  '`p1 p2'=: 2&sol`(12&sol) NB. Solutions for parts 1&2
tst=:{{)n
987654321111111
811111111111119
234234234234278
818181911112111
}}
}}

4 day {{ NB. Printing department
  NB. Part 1: How many rolls (@) can be removed by having less than 4 neighbours?
  par =: '.@'i.];._2
  pad =: ([,[,~[,.~,.) NB. Add x to each side of matrix y
  NB. Per 3x3 window (padded), middle=@ and max 5 rolls (counting middle as well)
  accessible=:(1 1,:3) (4&{ *. 5>+/)@,;._3 ]
  p1=: [: +/@,@:accessible (0 pad par)

  NB. Part 2: Remove possible rolls until no change; how many removed?
  NB. Third approach tried is the best, using neighbour indices.
  sh=: >,{;~0 _1 1 NB. 9 shifts, center first
  NB. Add stop; look up shifted coords in originals
  nn =: (,#)@(i. (}.sh) +"1/~])@($#:I.@,)
  p2 =: (, -&(+/) (] (]-] *. 4>+/"1@:{)^:_ (<:>i.)@#)@nn)@par

  NB. Previous versions; for comparison.
  p2a =: (+/@,@:- (- [: accessible 0 pad ])^:_)@par
  NB. 6x Faster alternative: shift&sum
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
  NB. Part 1: Find sum of fresh ingredient's ID's (second part of input) falling into any range (first part of input).
  NB. I. matches range excluding first, so add _1 0 to each to correct it to be inclusive. Cut by - is nop for ID's.
  NB.     corr for Id        cut by - after prepend; for each line (LF before each line by adding).
  par=:[: _1 0&+"1&.>`]"0[: (<@(([:".;._1'-',]);._1);._2~LF2&E.) LF([,,~)]
  NB.  sum  or'ed in ranges between parsed boxes
  p1 =: +/@([: +./ 1 = I."1 0/)&>/@par

  NB. Part 2: How many id's in total in ranges: note ranges overlap & to big to list.
  NB. Intersect range x and y: if any end of a range lies in the other
  int=: 1 e. I. , I.~
  fuse =: (<./@{. , >./@{:)@:,.  NB. Fuse ranges x and y
  sbm =: (/: [:-:+/"1)           NB. Sort ranges by mid-point
  NB. Merge ranges by insert (from back) _ 2$ for first
  NB.   tail   apd join or fuse~[int{.] fix first sort repeat
  mrg=: ((}.@] ,~  (,:`fuse@.int {.)) _ 2&($,))/@sbm^:_
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
  NB. Part 1: Sums/prod of nums in space-separated cols
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
  NB. Part 1: How many times is the beam split?
  par=: [: {."(2) _2 ]\ e.&'S^';._2 NB. Find char; keep odd rows as empty ones don't matter.
  NB. "and", but track count; used at split.
  andTrack=: {{r[T=: T++/r=. x *. y}}
  NB. fold verb x: new, with 1 indicating a ^; y state/first=beam
  NB.  straight or  or/ shift LR      ^ and |
  fv =: ((*.-.)~+.[:+./_1 1|.!.0"0 1 andTrack)
  p1 =: {{
    ] F.. fv par y[T=: 0
    T NB. Needs extra line, otherwise used as parsed, i.e. 0.
  }}
  NB. Part 2: count # paths down the grid; 40 for tst.
  NB. Brilliant! just removing . from +. and *. makes the same fold verb work!
  fv2 =: ((*-.)~+[:+/_1 1|.!.0"0 1 *)
  p2  =: +/@:(]F..fv2)@:par NB. Slightly different as now number of paths, not number of splits is asked.
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
  NB. Part 1: Given junction box coords, what's the product of #jb for the 3 largest
  NB.   connected circuit after connecting the nearest 1000 ones?
  NB. Part 2 (solution adjusted to take care of part 2 too): When joining until all are
  NB.   a single circuit, what's the product of x's of the last junction boxes joined?
  par =: ".;._2
  NB. Fast squared Euclidean distances of each pair, avoiding diagonal and flipped pairs.
  dt =: [: ; <@([: +/@:*:@|: {. -"1 }.)\.
  co =: ( (#~ </"1)@(,~ #: i.@*:)@# /:  dt) NB. Connection id's by increasing distance.
  mmd =:{{ NB. Merge min distance; y: par tst/io; x: # connections to make
    cir=. <"0 i. #y          NB. Circuits initialised as boxed integers.
    NB. "Do nothing" also counts as connection, so just join first co, even if in same circ.
    for_p. con=. x{.co y do. NB. For all connections to consider
      if. +./(#cir) > id=.I. p&(+./@e.)@> cir do. NB. Join if connection ends lie in circuit
        NB. Remove found ones, and add joined circuit; if 1 remaining: Return part 2 done.
        if. 1=#cir=. ((<<<id){cir),<;id{cir do. */{."1 y{~con{~p_index return. end.
      end.
    end.
    */ 3 ({.\:~) #@> cir NB. Return art 1's result.
  }}
  p1 =: 1000 mmd par
  p2 =: _    mmd par
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
  NB. Part 1: Find largest rectangle area, given a list of corners to serve as
  NB.   diagonal corners.
  NB. Surface is prod of 1 + abs diff of coords.
  surf =: 1 */"1@:+ [: | -"1
  NB. Notes: drop __ resulting from >./'' for last surface;
  NB.        no repeated pairs considered: ;@:(<@:({. v }.)\.)
  NB.    max    upper triang max surf's  conv numbers per line
  p1=:[: >./@;@}: [: <@({. >./@surf }.)\. ".;._2
  NB. Part 2: input forms polygon; find largest surface rectangle as before,
  NB. but must be entirely inside polygon defined by considering list of points
  NB. as edges.  Approach: as for p1, but filter surfaces based on whether any
  NB. of the edges intersects any of the polygon

  NB. Point locations based on I.  0 1&+ only interior!
  NB. 0 1 2  line intersects hor/vert edge if end points:
  NB.  +-+   v: 1 4  1 7  4 7
  NB. 3|4|5  h: 3 4  3 5  4 5
  NB.  +-+
  NB. 6 7 8
  p2=:{{
    i=. ".;._2 y NB. Parse input
    NB. Make list of surfs as for p1; $= N
    s=. }: ;@:(<@({. surf }.)\.) i
    NB. Make ranges for x a`nd y for each pos rect; order by descending surf;
    NB. $=Nx(x-y)x(start-end). Note: left global for visualisation below, as
    NB. well as "found".
    r=: (0 _1 +/:~)"1 (rno=:}: ;@:(<@({. ,."1 }.)\.) i) \: s
    found=: _
    NB. Edges of input; as x's, y's
    e=. 2 ,."1/\ (,{.) i
    NB. Cases for I. on x's and y's endpoints where edge intersects range (i.e. rectangle)
    intr=. (,|."2)1 1,:"1([:(,|."1)1 ]\. ]) i. 3
    NB. TODO: Slowish 2.9s. think of:
    NB. - avoid nested rank
    NB. - faster replacement for I. (maybe overkill)
    NB. - Avoid multi-dim lookup.
    for_rect. r do.
      if. -. intr +./@:(e.!.0)~ > rect I."1"2 e do. NB. If not intersected stop. SLOW.
        found =: rect_index
        break.
      end.
    end.
    found ({\:~) s NB. Return corresponding surface.
  }}
  vis=:{{ NB. Visualise x rect found; y parsed input
  require'plot'
  0 0$pd&.> (<'new'),(<;/|:y),(<;/|:0 1 3 2 0{>,{<"1]0 1+"1 x),<;._1 '.aspect 1.show'
  }}
  vv=: {{(found{r) vis ".;._2 io''}} NB. Just run vv'' after running p2.
  0
}}

10 day {{ NB. Factory
  NB. Part 1: Each button turns on some lights (parentheses). What is the
  NB. fewest number of presses required to reach the pattern on the left?
  NB. Parse: separate lights/buttons/joltages
  par=: ([:li`bu`jo"0 ]<;.1~(i.@# e. i.&'[({'));._2
  li=: (2 #:inv 2-.~'.#'i.|.)&.> NB. Light pats        : bool 0/1
  bu=: (2 +/ .^ ".);._2&.>       NB. Button connections: boxed
  jo=: ([:".rplc&'{ } ')&.>      NB. Joltages          : num list
  NB. Toggle is xor, which is symmetric, so pressing same button twice is nop.
  NB. Recurse: (target,buttons) rec step,states; return step when target in states.
  xorb =: 22 b. NB. Binary xor.
  tog =: {{     NB. Explicit: y: targets and buttons
    't b'=. y
    seen=.''    NB. Seen combos.
    ct=. st=.0  NB. Iteration count and current state
    while. ct =. >:ct do.
      NB. rem seen, uniq st xor buttons
      st =. seen -.~ ~. , st xorb/ b NB. Update state
      if. t e. st do. break. end.    NB. Target found; break loop
      'No solutions remain' assert 0<#st NB. Bug catcher
      seen=. seen, st NB. st is know not to be in seen
    end.
    ct NB. How many iterations?
  }}
  NB. parallel doesn't speed p1 up...
  p1 =: [: +/ tog@}:"1@:par NB. sum of toggle counts for each problem

  NB. Part 2: Now for joltages in col 3. Simply bruteforcing as for p1 is too slow.
  mp=: +/ .*
  NB. Integer Gauss-Jordan elimination; presume working with ints/extended, so no tolerance
  NB.   Based on ~addons/math/misc/linear.ijs
  NB. y: augmented matrix; returns matrix after pivoting (can be over or underspecified)
  gj =: {{
  'r c'=.$y NB. Number of rows and columns
  rws=. i.r NB. Row indices
  i=.j=.0   NB. Pivot indices
  while. (i<r)*.(j<c) do.       NB. While pivot in bound
    k=. (i.>./)col=.| i}.j{"1 y NB. Argmax of col j, down from row i
    if. 0~:k{col do.            NB. Only if max non-zero:
      if. k do.                 NB.  If k not in the first row, make it first.
        y=. (<i,i+k) C. y
      end.
      NB. pivot i,j;
      pcol=. j{"1 y NB. pivot column to be made 0.
      y =. (([- (pcol-i=i.#y) */ %&(i&{)) j {"1 ]) y NB. Probably can be more elegant; repeated j {"1 y
      i =. >: i                 NB. Next row
    end.
    j=.>:j                      NB. Next col
  end.
  y
  }}
  NB. Convert to system & do Gauss-Jordan; "x:" to keep rational for non-integer solutions to BL mp B = L
  NB.  y: buttons and target joltages.
  NB.    Light-Button mat  <. if same gj   append L
  tosys =: (|.@|:@#:&.>@[ <.^:(-:<.)@gj@x:@,.&> ])/@}."1
  NB. Solution, using recursive verb for guessing underdetermined systems.
  sol =: {{       NB. Takes parsed input
    y=. <@tosys y NB. No check for feasibility, will be stopped by div in any case.
    NB. Remove all-zero rows (overdetermined sys); not required.
    y =. (({:@$ ~: 0 +/ .=~ ])@:(}:"1) # ])&.> y
    NB. For each y in parallel, run recursive solver rec, and keep best solutions
    sols =. (#~ [: (=<./)+/"1)@(}:"1 rec {:"1)t.''&> y
    NB. Verify correctness, just for fun (in parallel).
    assert. *./;y ([: *./ {:"1@[ -:"1 }:"1@[ mp"_ 1 ])t.''&>  sols
    NB. Score the first of each solution
    +/+/@{.@> sols
  }}
  NB. Recursively find all possibilities to divide possibly rational number in
  NB.   bins taking differently sized chunks; returns list of chunk per bin.
  NB. x=number to divide; y=bin chunk sizes (all positive)
  rdiv =: {{
    if. 0=x  do. ,:0#~#y return. end. NB. 0 to divide: all bins get 0
    if. 1=#y do.                      NB. 1 bin left.
      if. (=<.) r=.x%{.y do.  r  return.  else.  0$0 return.  end.
    end. NB. All objects in that bin if integer, otherwise, no good solution, remove.
    assert. *./ y>:0 NB. Chunks should be non-negative
    r=. 0$~0,#y      NB. initialize result list of divisions
    NB. For each possible coeff for f=.{.y, <. for situations like 61 rdiv 2 2
    for_p. x i.@>:@:<.@:% f=.{.y do.
      r=. r,p,. (x-p*f) rdiv }. y NB. add to results p & recurse for remaining number
    end.
  }} NB. TODO: could be faster if memoized (but M. won't, as y is not integer)
  NB. Recursively guess variables in underdefined systems.
  rec =: {{ NB. x: JB, y: J; return B
    if. (diag=. (<0 1)&|:x) -: 0 -.~ , x do. NB. if x is diagonal matrix return only y as possibility.
      r=. ,:y % diag
    else.
    NB. Pick equation with all positive coeff to bruteforce.
    eqn   =. (i. >./) ndiv=. (*./"1>:&0 x) * y (([!&<:+)~ +/"1) x           NB. Eq num to remove; possibilities for combinations
    varmsk=. 0<eqn{x                                                        NB. Non-zero variable coefficients in eqn{x
    vals  =. y (rdiv varmsk&#)&(eqn&{) x                                    NB. Possible values for vars in eqn{x to satisfy equality.
    if. 0=#vals do. 0#,:varmsk return. end.                                 NB. If no solutions, return empty.
    remeq =. (<<<eqn)&{                                                     NB. Remove equation eqn.
    ny    =. (remeq y) -"1 guess=. (remeq x) +/ .*"_ 1 varmsk&#inv"1 vals   NB. New RHS, i.e. ny by: fill in vars in sys, and sub from RHS
    nx    =. (<(<<eqn),(<<I.varmsk)){x                                      NB. New LHS for remaining equations: remove eq. and vars appearing in them from LHS, x.
    'sub nsol' =. nx (;;#&>)@:(<@rec"_ 1) ny                                NB. Solve remaining sys nx,.ny; keep solutions to guessed rhs only where pos. (entirety assured by rdiv).
    pos =. *./"1 (0&<: *. (=<.)) sub                                        NB. Mask for possible solutions, >:0 and integer.
    NB. Fill in fill mask in sub-solutions, and return one with smallest count
    if. +./ pos do.                                          NB. Any valid solutions?
      NB. Should: expand sub to convert back to solutions to original system x,.y; return all possible solutions because longer ones could still be best in combination with higher levels.
      NB. Not strictly necessary, but fun for verifying solution.
      r=.(nsol#vals) (varmsk&#inv"1@[ + (-.varmsk)&#inv"1@])&(pos&#) sub
    else.
      r=.0#,:varmsk
    end. NB. Else return empty, so it disappears.
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
NB. Parse graph given as parent:children
NB.    transp raz  par,ch pairs of sym after removing :
par =: [: |:@:;   <@({.,.}.)@s:@:(' ',':'-.~]);._2
  NB. Part 1: Count all paths from you to out
  NB. Graph cannot have loops between you-out as otherwise infinite paths.
  p1 =: {{
    'you out'=: s: ' you out' NB. you and out nodes
    'i o'  =: par y           NB. In/Out nodes; entire graph
    next =: o #~ i e. ]       NB. Find next nodes for current ones
    NB. y: Current node; x: Paths from you so far; returns number of paths
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
  NB. Part 2: All paths from srv via dac & fft to out
  NB. Similar approach as p1, but keep paths going down; when reaching out,
  NB. check if dac&fft on path.  1 rec srv crashes, so likely loops etc when
  NB. not considering dac & fft.  First try, make paths srv->dac, then dac->fft
  NB. then fft -> out + srv-> fft fft-> dac dac->out; too slow.
  p2 =: {{
    'svr dac fft out'=: s: ' svr dac fft out'
    'i o'=. par y
    next =: o #~ i e. ]
    NB. Early abort heuristic: do next from srv till end; keep dict with
    NB. min&max depth a node is encountered. Use these when finding way to
    NB. fft/dac/out remove those that have min>max dest
    nodes=. ~.i,o                           NB. All nodes in order
    ni=: nodes&i.                           NB. Node index verb
    'min max' =: |: (2,~#nodes)($,)10000 _1 NB. Defaults for min&max depth
    nod=. svr [ d =. 0                      NB. Start node and depth.
    while. (0<#nod) do.                     NB. While nodes left to check
      up  =. ni nod                         NB. Node indices to update:
      min =: d&<.&.(up&{) min               NB.   min depth it appears
      max =: d&>.&.(up&{) max               NB.   max depth it appears
      d   =.d+1 [ nod =. next nod           NB. Next depth and nodes.
    end.
    NB. New rec approach: ds fs rec nod; ret # con to out via nod,fft,dac. ds
    NB.   and fs track whether ds&fs seen.
    NB. Filter out where (min{node) > max{~nodes i. fft,dac.
    mddf =: max {~ nodes i. dac,fft         NB. Max depth of dac&fft
    seeni=: 0 2$a:[seenv=: 0$0              NB. Memoize seen ds fs;nodes combos
    rec =: {{ NB. x: ds fs ; y: nod
      if. (#seeni)>id =. seeni i. x;y do.   NB. memoize: if seen
        seenv{~id return. end.              NB.   return stored result
      if. y=out do.                         NB. Arrived out
        r=. *./x                            NB. 1 if both dac&fft met, else 0
      else.
        nx=. x>.y=dac,fft                   NB. New x: check for dac/fft
        NB. Keep next where, if dac/fft not seen, there is still hope...
        ny=. nx (] #~ *./@(>. mddf >:/ min{~ni)) next y
        if. #ny do. r=. nx +/@:rec ny else. r=.0 end.
        NB. r=. nx +/@:rec next y ==> Without depth check is half as fast due
        NB.   to hopeless cases. note: use timex because of memoizing!
      end.
      seeni=: seeni,x;y[seenv=: seenv,r     NB. Save result in cache.
      r
    }}"1 0
    0 0 rec svr NB. Run the above; starting with dac&fft unseen, from svr.
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
  NB. Parse gifts  & regions @   split after last LF2
  par =: [: (pg&.>)`(reg&.>)"0 ] split~ 2+LF2 i:&1@:E. ]
  NB. Look up;drop 1st+lst per 5 lines
  pg  =: '.#' i. _5 }.@}:\ ];._2 NB. convert gifts to bool
  reg =: [: 0&".;._2 rplc&'x : ' NB. Keep only numbers
  NB. Do 2 simple tests:
  NB. Sure fit if gifts(3x3) will fit when stacked as 3x3 boxes.
  surefit  =: ((3 */@:<.@:%~ 2&{.) >: +/@:(2&}.))"1@]
  NB. Sure fail if area is smaller than total number of occupied squares in gifts.
  surefail =: */@:(2&{.)"1@] < +/@,"_1@[ +/ .*"(1) 2&}."1@]
  NB. Check: if counts of fails&fits is #regions, then all cases are decided.
  NB. If not, error (and implement if needed)
  check =: 'fit&fail don''t sum'assert #@]=+&(+/)
  p1 =: (surefit (+/@[ [ check) surefail)&>/@:par
  NB. Spoiler: exact solver not needed ;).
0
}}
echo 'To run day 1-12, execute:',LF2,'   run >:i.12'
NB. vim: ts=2 sw=2 et fdm=marker foldmarker={{,}}
