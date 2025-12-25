NB. AoC, 2025
{{
if. -. fexist '~addons/general/jaoc' do.
  install'github:jpjacobs/general_jaoc'
  echo 'installed general/jaoc; go set up COOKIE.txt'
end.
}}''
NB. load'general/jaoc'
load '~J/general_jaoc/jaoc.ijs'
1!:44 jpath '~AoC/'
NB. Setup this year
'data25/'setup_aoc_ 2025
NB. Notes:
NB.  - This file uses jaoc for download/upload/day organisation
NB.  - Each part of each day solves the entire problem, from the problem text up.
NB.  - The solution don't depend on any library; sometimes plot or viewmat used for visualisation.
NB.  - Recommended settings for vim: sw=2 ts=2 et fdm=marker foldmarker={{,}}

NB. Spin up threads up to # of cores & set boxdraw to ASCII (for Android)
0&T.@0^:(0>._1+([: {. 8&T.)-1&T.) ''
boxdraw_j_ 1

NB. Daily solutions:
1 day {{ NB. Secret Entrance
  par =: 50,0".];._2@:(rplc&'L-R+') NB. start at 50; L=-;R=+
  NB. +/\ is fast, but +m.100/\ slow; reverse, and work from the back.
  sol =: (@:par)([: +/ 0 = +m.100/\.@:|.@:) NB. adverb = A A train
  NB. p2 spells out every instruction in single clicks, then do sol
  '`p1 p2'=: ] sol`((|#*) sol)
0
}}
2 day {{ NB. Gift shop
NB. Part 1: find all invalid ideas consisting of a number repeated twice
  par=: [: ([: ".;._1 '-',]);._1 ',',}:
  to=: ;@:(<@([+i.@(-~/))/"1)
  istwice =: ([:-:/2 _$":) :: 0"0
  p1=: +/@(#~istwice)@to@par
  NB. Part 2: repetition at least twice
  div=: [: }. (*/ .^"1 (#: i.@(*/))@:>:)&|./@:(__&q:) NB. all divisors of y but 1; see odometer Essay.
  alt =: ((_,.~div@#) ([: +./ 1=#@~.@$"1) ])@:":"0 NB. at least twice repeated
  NB. correct but slow (~9s)
  p2_slow=: +/@(*alt)@to@par
  NB. improved fun: box per number of digits; and do factoring per length
  bxpd =: (<;.1~ 1,2~:/\10 <.@^. ])@:/:~
  alte =: (]#~[: +./ (_,.~div@#@{.) (1=#@~.@$)"1/ ])@:(":"0)
  p2a =: +/@:".@:;@:(alte&.>)@bxpd@to@par NB. 3x faster

  NB. parallelise and don't keep all boxed numbers.
  alto =: +/@:".@:(#~[: +./ (_,.~div@#@{.) (1=#@~.@$)"1/ ])@:(":"0)
  bxpdo =: (alto t. '';.1~ 1,2~:/\10 <.@^. ])@:/:~
  p2 =: +/@:;@:bxpdo@to@par NB. 30% faster than p2a (only... work not equally distributed}
0
}}
3 day {{ NB. Lobby
NB. Part 1: in each line of battery joltages, find 2 digits that together are highest 2 digit number. Sum=joltage!
NB.  combine    sel    first,last   imax after first  imax without last
NB. p1=:[:+/ (10 1+/ .*] {~ ] (] ([,1++) (i.>./)@(}.~>:)) (i.>./)@}:)"1@:par
NB. Part 2: turn on not 2 but 12 batteries in each row.
NB. fits in int; make recursive so x is num found; len used to behead/curtail enough. x: N bats,nums collected; y: remaining array to search in
rec =: {{
  if. (#=1+{.)x do. <.(+/ .* 10^i.@-@#)@}.x return. end. NB. finished; compine result
  imax =. (i.>./) (-({.x)-#x)}.y NB. max num in curtailed array
  (x,imax{y) rec y }.~ 1+imax NB. recurse on list of digits; shortened array
  }}"1
sol =: +/@:(rec "."+;._2)
'`p1 p2'=: 2&sol`(12&sol)
tst=:{{)n
987654321111111
811111111111119
234234234234278
818181911112111
}}
0
}}
4 day {{ NB. Printing department
par =: '.@'i.];._2
NB. pad=: |:@([,,~)^:2
pad =: 0&([,[,~[,.~,.)
NB. per 3x3 window (padded), middle=@ and max 5 rolls (counting middle as well)
accessible=:(1 1,:3) (4&{ *. 5>+/)@,;._3 ]
p1=: [: +/@,@:accessible (0 pad par)
NB. Part 2, remove possible rolls until no change; how many removed?
p2a=: (+/@,@:- (- [: accessible 0 pad ])^:_)@par
NB. 6x Faster alternative: shift&sum
sh=: >,{;~0 _1 1
p2b =: (+/@,@:- (- [:({.*.4>+/@:}.) sh |.!.0])^:_)@:par
NB. try third approach, neighbour inds
nn =: (,#)@(i. (}.sh) +"1/~])@($#:I.@,)
p2 =: (, -&(+/) (] (]-] *. 4>+/"1@:{)^:_ (<:>i.)@#)@nn)@par
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

0
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
0
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
NB. bu=: <@".;._2&.>          NB. button con: boxed
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
xob =: 22 b.
it =: 0 0 step^:({.@[ -.@:e. }.@])^:_~ ]
exp =: {{
't b'=. y
seen=.''
ct=. st=.0
NB. echo 'looking for ',":t
while. 1 do.
  ct =. >:ct
  NB. rem seen, uniq st xor buttons
  NB. TODO: Double check, input 1 ends up with empty state after seeing 64 vals
  st =. seen -.~ ~. , st xob/ b
  if. t e. st do. break. end. NB. target found
  NB. if. 0=100|ct do.  echo ct, t, st ,&$ seen end.
  if. 0=#st do. 1+'4' end.
  seen=. seen, st NB. st is know not to be in seen
end.
NB. echo 'found t at ',": ct,t
ct
}}
NB. oneliner:
tac =: [: +/@#: (i.~ (xob/@#~ (#&2 #:[:i.2^])@#@]))&>/

NB. optim rec: 0): memoize 1): remove previously encountered.
step=: >:@{.@] , [: ~.@, }.@[ xob/ }.@]
NB. p1 =: [: +/ tac@}:"1@:par NB. Arg... right for tst, but not for input 504 too high
NB. parallel doesn't speed up...
p1 =: [: +/ exp@}:"1@:par NB. also faster than tac...
NB. Part 2: now for joltages
NB. exp2 works for tst; too slows for all.
exp2 =: {{
'b t'=: y
NB. max in joltages is 253 and max in b is 1 so digit always fits a byte; encode as literal LATER.
NB. sort so more impact done first? (\: +/"1)
b=: |."1 #: b
SEEN =: 0{.b NB. store seen st's as string
SEENV =: 0$0 NB. store values for seen elements when known.
BEST =: _
echo 'looking for ',":t
rec  =: {{ NB. x it; y remainder
  NB. echo (x#'*'),' ',":(ai t),_,ai y
  if. *./0=y do. NB. arrived!
    echo'found at ',":x,BEST,y
    BEST=:BEST<.x
    0 return.
    NB.   #it > best or any of y > t
  elseif. (x >: BEST)+. +./0>y do. NB. no hope
    _ return. end. NB. return dist to sol
  st  =. (/: +/"1) y -"1 b NB. greedy: smallest remainder first
  acc =. _
  for_s. st do.
    if. (#SEEN)=i=. SEEN i.!.0 s do. NB. new s?
      vv=. (x+1) rec s  NB. recurse to get value
      SEEN  =: SEEN ,s  NB. mark seen
      SEENV =: SEENV,vv NB. store val
    else.
      vv=.i{SEENV
    end.
    acc=.acc<.vv NB. 1+ counts this iteration.
  end.
  1+acc
}}
0 rec t
}}
mp=: +/ .*
rnd =: [: <. 0.5&+

exp3 =: {{
NB. TODO: attempt joint solution of all problems; asof 1239 buttons only 493 are unique. Problem for tracking value though, as dist to sol is different for each target.
'b tt'=: y
NB. max in joltages is 253 and max in b is 1 so digit always fits a byte; encode as literal LATER.
NB. sort so more impact done first? (\: +/"1)
bb =: |."1 #: b
SEEN =: 0$~0,#b NB. store seen st's as string
BEST =: _
NB. take out steps that can only be done by 1 item in b.
NB. repeat until no 1=+/#:b
off=: 0
echo bb
while. ({:$bb)>j =.1 i.~ +/ bb do. NB. j=col in bb
  echo 'red'
  i=: 1 i.~ j{"1 bb  NB. i=row in bb
  off=: off+c=.j{tt  NB. how many times to do this step?
  tt=: (<<<j){tt-c*i{bb  NB. remove j{t; sub other 1's in b too
  bb =: (<<<i){ bb {~"1 <<<j NB. remove row i and col j from b
end.
b=: 256x&#.  bb NB. back to numbers!
t=: 256x&#: inv tt NB. TODO check endianness

NB. set guestimate of coeff TODO + coeff as y for rec
NB. no need for precision far off target, as +/coef is the number of clicks. Round down so we can (hopefully) work up.
init =: <. (tt % +/ bb) <./@:(-.&0)@:*"1 bb
echo 'looking for ',":t
echo 'init guess ',": init +/ .* b
ct=: 0
rec  =: {{ NB. x new val ~ y: coeffs for b
  if. t = x do. NB. arrived!
    BEST=:BEST<.r=.+/y
    echo'found at ',":r
    r return.
    NB.   #it > best or any of y over or under flow
  elseif. (BEST<:+/y) +. tt ([+./@:< ]#:~256#~#@[) x do. NB. no hope
    _ return. end. NB. return dist to sol
  st  =. SEEN -.!.0~ (+"1/ =@:i.@:#) y NB. New coeffs
  st  =. (#~ ([: *./"1 >:&0)) st
  rs  =. st +/ .* b NB. do here to bundle comput.
  acc =. _
  for_r. rs do.
      SEEN  =: SEEN , r_index{st NB. mark seen
      vv=. r rec r_index{st  NB. recurse to get value
      acc=. acc<.vv
  end.
}}
off + b (+/ .* rec ]) 0#~#init
}}
NB. TODO: Works for tst; but keeps returning to same sol in entire input...
NB. other idea: bb=: |: ai b ; tt=: ai t; bb mp xx =tt => use (%<./) bb %. tt as first guess and explore around
NB. requires rewrite so state is xx
NB. explore e.g. sim anneal
NB. rather: <.((|: mp %.@:(mp|:)) bb) mp tt moore penrose inverse.
pp =: [: +/ ]@:(exp2@}."1)@:par NB. TODO: Boooom.
NB. rewrite with memoizing+recursion
NB. Integer matrix algebra approach with backtracking: sort eqs up so as much utm as possible. The pivot & combine. recurse on lhs;rhs;sol where sol is proposed solution; return sol found
ip =: {{
'r c'=. x
col =. c {"1 y
y - (col-r=i.#y) */ y %&(r&{) col
}}
NB. integer Gauss-Jordan; presume working with ints, so no tol. Based on ~addons/math/misc/linear.ijs
igj =: {{
echo=.]
'r c'=.$y
i=.j=.0
rws=. i.r
while. (i<r)*.(j<c) do.
  echo'it: i,j= ',":i,j
  echo y
  echo 'col ',": col=.| i}.j{"1 y
  echo 'k= ',": k=. (i.>./)col
  if. 0~:k{col do.
    if. k do.
      echo 'sorted y, k= ',":k
      echo y=. (<i,i+k) C. y
    end. NB. sort
    NB. pivot i,j
    echo 'pivot i,j= ',": i,j
    echo 'where;factor'
    pcol=. j{"1 y
    where=. ]-i=i.@#@[ NB. pcol - 1 where row i
    factor=. %&(i&{)   NB. row i % el@(i,j)
    NB. TODO: multiply by (]%*.@-.&0) pcol
    echo y=. y ([-where*/factor) j{"1 y
    i =. >: i
  end.
  j=.>:j
end.
y
}}
NB. convert to sys & do Gauss-Jordan; x: to keep rational for non-integer solutions to BL mp B = L
NB.    Light-Button mat  <. if same gj   append L
tosys =: (|.@|:@#:&.>@[ <.^:(-:<.)@igj@x:@,.&> ])/@}."1

tst=: {{)n
[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
}}
0
}}
NB. temporary storage
echo run 10
NB. cocurrent'd21'[load'~A/2024.ijs'
NB. vim: ts=2 sw=2 et fdm=marker foldmarker={{,}}
