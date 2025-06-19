NB. AoC, 2024
{{
if. -. fexist '~addons/general/jaoc' do.
  install'github:jpjacobs/general_jaoc'
  echo 'installed general/jaoc; go set up COOKIE.txt'
end.
}}''
load'general/jaoc'
1!:44 jpath '~AoC/'
NB. Setup this year
'data24/'setup_aoc_ 2024
NB. Notes:
NB.  - This file uses jaoc for download/upload/day organisation
NB.  - Each part of each day solves the entire problem, from the problem text up.
NB.  - The solution don't depend on any library; sometimes plot or viewmat used for visualisation.
NB.  - Recommended settings for vim: sw=2 ts=2 et fdm=marker foldmarker={{,}}

NB. Spin up threads up to # of cores
0&T.@0^:(0>._1+([: {. 8&T.)-1&T.) ''
boxdraw_j_ 1

NB. Daily solutions:
1 day {{ NB. Historian Hysteria
NB. sum  |diff sorted  columns parsed
p1=: +/@:(|@:-&(/:~)  )/@:|:@:(".;._2)
NB. sum L * count in R columns parsed
p2=: +/@:([*+/@(="0 1))/@:|:@:(".;._2)
0
}}
2 day {{ NB. Red Nosed Reports
NB. safe if adjacent diffs all 1 <: |@dif <: 4 and monotone
safe=: 2 (-~/\ +.&([: *./ 0&< *. <&4) -/\) ]
p1=: [: +/safe@".;._2 NB. count safe reports
p2=: [: +/([: +./ 1 safe\. ])@".;._2 NB. count reports where at most 1 unsafe
0}}
3 day {{ NB. Mull It Over
NB. Character mapping
NB. codes                                characters               other
M =: 0 1 2 3 4 5 5 5 5 5 5 5 5 5 5 6(a.i.'mul(,0123456789)') }256#7
S =: +.@:".&> }. <;._2 {{)n
NB. m   u   l   (   ,  num  ) other <-in / v-- states
   1j0 0j0 0j0 0j0 0j0 0j0 0j0 0j0 NB. 0 not started
   1j0 2j0 0j0 0j0 0j0 0j0 0j0 0j0 NB. 1 m
   0j0 0j0 3j0 0j0 0j0 0j0 0j0 0j0 NB. 2 u
   0j0 0j0 0j0 4j0 0j0 0j0 0j0 0j0 NB. 3 l
   0j0 0j0 0j0 0j0 0j0 5j1 0j0 0j0 NB. 4 (
   0j0 0j0 0j0 0j0 6j0 5j0 0j0 0j0 NB. 5 num1
   0j0 0j0 0j0 0j0 0j0 6j0 0j3 0j0 NB. 6 num2
}}
NB.    sum   mul    rows boxed nums cut by fsm
p1=: [: +/ [: */ [: |:@:(".&>) (0;S;M) ;: }:
NB. Part 2: do() enables and don't() disables
NB. rather than extend states, filter input
tst=:'xmul(2,4)&mul[3,7]!^don''t()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))'
NB. enab fold clipsum    loc do() - loc don't()
msk=: 1 ] F:. (0>.1<.+) 'do()'&E. - 'don''t()'&E.
p2=: p1@(#~ msk)
0
}}
4 day {{ NB. Ceres Search
NB. matches on a line where straight or reversed
matches =: ('XMAS'&E."1 +/@:,@:+.'SAMX'&E."1)
NB.   rows cols    diags other diags
box=: <"1, <"1@:|:, </., </.@:|.
p1=: +/@:(matches@>)@:box@:(];._2)
NB. Part 2: find MAS in an X
pos=: (|."1@|.,.~'A',.~])'MS'{~#:i.4 NB. all possible arrangements of XMAS
NB.    sum   step sz   possible? locations in windows make matrix
p2=: [:+/@:, (1 1,:3) ([: e.&pos 0 2 4 6 8({,)]);._3  ];._2
0
}}
5 day {{ NB. Print Queue
NB. Parse rules&prints   box chunks ending in LF2
par=: [:  rules`print"+ (LF2&E. <@:,&LF;._2 ])@:(,&LF)
  rules=: ((".;._2)@rplc&'|,')&.> NB. str->num
  print=: ([:}.<@".;._2)&.> NB. just to num & box each line; discard empty box (due to LF2 split)
NB. look up rules in prints i0 <: i1 (note, prints are unique)
NB. p1 =: +/@:( ((score * ordered) >)"_ 0&>/)@par
p1 =:  [: +/     (score * ordered) S:0     / @par
  NB. is ordered?: x:rules (2 cols); y: 1 line of prints
  ordered =: *./@(<:/)@:(i.~ |:@filt ])
  NB. filter results of lookup; removing results with page not in list
  NB. x: print list; y: lookup result (2 cols)
  filt =: [ #~ *./"1@(< #)
  score=: ({~2%~1-~#)@]
NB. Part 2: fix the ordering of the incorrect ones; get score of only those
NB. after fixing
p2 =: [: +/ (score@fix`0:@.ordered)S:0/@par
NB.  fix: find first rule not complied with; find first put before second; iterate till all rules complied with
    fix =: filt2 (sel swap ])^:(+./@:bad)^:_. ]
    filt2 =: [ #~ *./@:|:@:(e."0 1) NB. Filter to get applicable rules
    bad =: >/@:|:@:i.~       NB. Mask for infringed rules
    sel =: ([ {~ bad i. 1:)  NB. Select first bad filter
    swap=: i.~ {`(|.@[)`]} ] NB. Swap locations of x in y
0
}}
6 day {{ NB. Guard Gallivant
NB. Part 1: how long before the guard leaves the area.
NB. Guard walks straight, turning right at obstruction #
par=: ];._2
NB. note: hardcoded '^' facing initially up. no clue whether <v> also occur...
NB. note: F: apparently changes x and y for v with respect to F:.
NB. Need to add initial location to fold result too!
p1   =:{{ NB. explicit because oob needs size of y.
  check =: [ _2 Z: oob
  oob =: [: +./ 0 0&> +. ($y)&<:
  turn  =: (,:(4 2$_1 0 0 1 1 0 0 _1)&([ {~ 4|1+i.))/@]
  NB. step: y: (pos,:vel) ; x: binary grid: 1=obstruct
  NB. _2 Z: 1 terminating immediately is a bit of a euphemism; hence :: 0: needed
  step=: [: (+/,:{:)  ]`turn@.(<@check@:(+/)@] { ::0: [)
  #@:~. ((_1 0,:~$#:'^'i.&1@:=,) ({.@[ , {. F: step~) 2|'.#'&i.) y
}}@:par
NB. Part 2: Find all positions that, when blocked, cause the guard to loop.
p2=:{{
  NB. URDL chosen so dir when blocked is 4|1+orig dir; or (i-3){gr
  NB. Create graph; Nx4 (URDL order) pairs of pos,dir; so Nx4x2
  sh =. 4 2 $_1 0 0 1 1 0 0 _1 NB. URDL order
  NB. Fully connected graph, each pos, dir pair occupies 1 el in list
  NB. keep 2D for easy accounting of blocks
  grs =. 4 (i.@[ +"1 *) (i. sh +"1/~]) (#:i.@:(*/))$y NB. conn spaces
  NB. OOB should result in error so no <.4**/y or adding that val.
  blocks =. , (i.4) +/ 4*'#'I.@:=,y NB. ind of blocks in each dir
  NB. lookup table for replacement directions for block destinations
  NB. index into row in case mask as in comment
  lu =.".;._2  {{)n
  0 1 2 3 NB. 0 0 0 0
  0 1 2 0 NB. 0 0 0 1
  0 1 3 3 NB. 0 0 1 0
  0 1 0 0 NB. 0 0 1 1
  0 2 2 3 NB. 0 1 0 0
  0 2 2 0 NB. 0 1 0 1
  0 3 3 3 NB. 0 1 1 0
  0 0 0 0 NB. 0 1 1 1
  1 1 2 3 NB. 1 0 0 0
  1 1 2 1 NB. 1 0 0 1
  1 1 3 3 NB. 1 0 1 0
  1 1 1 1 NB. 1 0 1 1
  2 2 2 3 NB. 1 1 0 0
  2 2 2 2 NB. 1 1 0 1
  3 3 3 3 NB. 1 1 1 0
  0 0 0 0 NB. 1 1 1 1  don't care, can't arrive anyhow.
}}
  NB. blocks ab gr: add blocks to graph to the right. blocks should be all blocks in graph.
  NB.  gr swap dir   index in lookup
  ab =. ] {"1~ lu {~ #.@:e.~
  gr =. blocks ab grs
  NB. Find "normal" path to locate positions to block & simplify graph
  NB.       apd if new    next@last err? => oob
  follow =. (] ,~^:(-.@:e.)~ ({~ {:))^:(#@[ > {:@])^:_ NB. _ = oob.
  st =. 4*'^' i.&1@:=  ,y  NB. Starting pos, going up
  NB. follow, until either oob (_) or loop
  NB. First div 4 so can take  unique positions.
  route =. ~. 4<.@:%~ }. }: (,gr) follow st
  NB. for each possible blocking pos (route) add to gr as before
  loops =. ; st ({:@:follow~ gr ,@:ab~ (blocks , (i.4)+]))t.''"0 ]4*route
  loops +/@:< #,gr
}}@par
NB. Inconsistent answers were due to a function used in parallel assigning a global
0
}}
7 day {{ NB. Bridge Repair
NB. Part 1: Fill in the missing operators
NB. EDA: 2 operators; ~500k equations to verify for bruteforce (p1)
NB. Far less unique values than numbers; e.g. only 409 unique in first two pos ~ 25%
NB. part 1 bruteforce because fast enough,reverse numbers > J order
NB.        bx val rev num split@ ' 'without : each line
par =: ([: <@({.,|.@}.) [: <;._1 ' ',':'-.~]);._2
NB. apply boxed verbs to numbers by appending, ravel, raze; exec
apl =: ".@:;@:}:@:,@:,."1
NB. apl =: [: ; {{".y}}@:;@:}:@:,@:,.t.''"1 NB. {{}} because of bug; slower than other apl version.
NB. create all ops possibilities
NB.    open cart prod sel =(+|*) rot  = + * n-2 NB. = as last pad; removed in apl.
NB. ops =: [: >@:,@:{ [: {&(=`+`*)&.> 1|.(0;1 2)#~2,2-~#
ops =: [: >@:,@:{ [: {&(<;._1 ' = + *')&.> 1|.(0;1 2)#~2,2-~#
NB. mask for calibration values to be counted
msk =: (+./@:apl ops)@> NB. also slower if parallelised (req {{".y}})
NB.    test values    mp   mask after par
p1  =: (".@>@({.S:1) +/ .* msk)@:par
tst =: {{)n
190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
}}
NB. Part 2: Oh no, a third operation! concatenation
NB. c=: ([+>.&.(10&^.)@>:@[*]) M. NB. c for concat; no good.
NB. problem with 0's disappearing when handled as numbers
NB.
c=: ,~   &.> M.
p=: +&.".&.> M.
t=: *&.".&.> M.
e=: -:&>
NB. new par makes string with 3 spaces between numbers
par2 =: ([: <@([: dtb@; [:,&'   '&.> {.,|.@}.) [: <;._1 ' ',':'-.~]);._2
apl2 =: {{
  ops  =. >@:,@:{ {&x&.> (0;(i.&.<: #x))#~1,2-~# y NB. gerund of ops
  if. 4=#x do. ops=. (#~ ({.c`'')&e."1) ops end.   NB. only keep if has c
  0 ([1&Z:) F.. ((+. y&{{y/x}})~) ops NB. try ops until found
  }}
p2  =: {{
  y=. par y
  r1=. (".@>@{. S: 1 y) +/ .* m1=. e`p`t&apl2@> y   NB. already good for p1
  r2=. (".@>@{. S: 1   +/ .* [: ; e`p`t`c&apl2 t.''@>) (-.m1)#y NB. test remainder with c
  NB. r2=. (".@>@{. S: 1   +/ .* e`p`t`c&apl2@>) (-.m1)#y NB. test remainder with c
  r1+r2
}}
NB. possibility for faster: use symbols instead of boxed strings; cache ops results
0
}}
8 day {{ NB. Resonant Colinearity
NB. Part 1: antinodes appear at ..#..o..o..#..; how many unique antinode positions?
NB. Parse map per symbol & convert to coords
par =: ([: (<@}.@$ #:&.> <@I.@,"2) ('.'-.~~.@,) =/ ])
NB. an y : antinodes for coords in y
NB.   rem coords list x+-delta for each pair
an  =: (-.~       [:,/([+-)"1/~) NB.  -.~ because c1 - c1 leads to antenna location which is not an antinode
lim =: ([ #~ [: *./"1 (>: {.)*.(<: {:))~
NB. Part 1; assumes field is square
p1  =: [: #@((0,<:@{.@$) lim [: ~.@:; an &.>@:par) ];._2
NB. Part 2: instead of only at delta separation, all points on line defined by two antenna's. Quick&dirty: min distance 2 antenna's can be different is 1, so do 0 to 50 *delta in an; take out removal of antenna locations sprinkle in a rank and reduce once more, and done.
anl =:  ([: ,/^:2 ([ +"1 (i.51)*/-)"1/~)
p2  =: [: #@((0,<:@{.@$) lim [: ~.@:; anl&.>@:par) ];._2
tst=:{{)n
............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
}}
0
}}
9 day {{ NB. Disk Fragmenter
NB. Part 1: Given a list of RLE file,space pairs, move blocks of last file to first free blocks encountered until done. Files have ID numbers by their order on disk.
NB. 10000 files;9999 spaces; for part 1 simply simulate
par =: "."0 @: }: NB. remove LF; char to num
mem =: (# _1}:@,@:,.~i.@>.@-:@#)    NB. memory with file ind; _1=spc
ind =: _1 (i.&1 , i:&0)@:= ]        NB. first space, last file block
upd =: (ind (|.@:{)`[`]}^:(</@[) ]) NB. swap if last block > 1st spc
score=: i.@# +/ .* ] * _1&~:        NB. score
NB. slow steps is ind (3.3s) and upd (1.5s). Others negligible.
p1  =: score@:(upd^:_)@mem@:par
tst =: '2333133121414131402 '
NB. Part 2 move entire files by descending identifier. If not possiblefile stays put. Try working out perm in condensed form, apply to result of i. in mem and then score
vis=: '.0123456789#'{~(_1+i.11)i.] NB. Visualization of mem
p2 =:{{
  'fil spc' =. <@(]`}:"1) |: _2]\ par y NB. size of and space after file i
  ids       =. i.#fil              NB. File id's
  NB. id,fil,spc -> insert id,fil,spcorig-spcnew
  for_f. i.-#fil do.               NB. try moving f, large id's first
    NB. both spc and fil per file index; so order spc for lookup
    pos=. (e=.f{fil) (i.&1)@:<: (ids{spc) NB. first space large enough
    if. pos >: ids i. f do.
      continue.
    end. NB. only move towards front
    prv=. <:&.(ids&i.) f NB. Previous file in orig, to expand space
    NB. update new prev, old, cur:0,sz+spc before&after,remaining after f
    if. 2=#loc=.~.((pos{ids),prv,f) do. NB. move while in same order
      spc=. spc loc}~ 0,+/(f,prv){spc NB. space=left space+right space
    else.
      spc=. spc loc}~ 0,(e++/(f,prv){spc),((pos{ids){spc)-e
      ids=. (>:pos) ({.,f,f-.~}.) ids NB. update id's
    end.
  end.
  NB. Instantiate memory and score
  score (fil ,@:,.&(ids&{) spc) #  _1 ,@:,.~ ids
}}
0
}}
10 day {{ NB. Hoof It
NB. Part 1: Given map of heights find, per 0, how many different 9's can be reached with orthogonal steps What's the sum?
par =: _1&"."0 ;._2
NB. contruct graph
sh =: 4 2$0 1 1 0 0 _1 _1 0 NB. RULD
tst =: {{)n
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
}}
p1 =: {{
  y =. par y
  val =: 100,~ ,y NB. heights + nonsense value; linear
  gr =: (i. sh +"1/~ ]) ($ #: _1 I.@:~: ,) y NB. RULD connected graph
  gr2 =: val (] >. #@] * 1 ~: }:@[ -~ {~) gr      NB. rem non 1+ cons
  non =: #gr2 NB. non-neighbour = #gr by construction using i. above
  heads =: 0 I.@:= , y NB. 0's to start from (196 in my input)
  NB. for each 0, follow until we have a 9
  follow =: ([: ~. non -.~ [: ,/ {&gr2`]@.(9={&val))
  NB. +/;@:(#@(follow^:_) t. ''"0) heads NB. slower as non-threaded is ridiculously fast.
  +/#@(follow^:_)"0 heads
}}
NB. Part 2: Find sum of number of all paths, rather than only destinations. This simply requires not pruning duplicate locations in follow
NB. TODO: Deduplicate shared code.
p2 =: {{
  y =. par y
  val =: 100,~ ,y NB. heights + nonsense value; linear
  gr =: (i. sh +"1/~ ]) ($ #: _1 I.@:~: ,) y NB. RULD connected graph
  gr2 =: val (] >. #@] * 1 ~: }:@[ -~ {~) gr      NB. rem non 1+ cons
  non =: #gr2 NB. non-neighbour = #gr by construction using i. above
  heads =: 0 I.@:= , y NB. 0's to start from (196 in my input)
  NB. for each 0, follow until we have a 9; Removed [: ~. from part 1
  follow =: (non -.~ [: ,/ {&gr2`]@.(9={&val))
  +/;@:(#@(follow^:_) t. ''"0) heads NB. slower as non-threaded is ridiculously fast.
  NB. +/#@(follow^:_)"0 heads
}}
0
}}
11 day {{ NB. Plutonian Pebbles
NB. Part 1: depending on value: 1:, split or *&2024
par =: ".@}:
cond=: 1 i.~ (0&=,0=2|10 >.@:^.>:) NB. 0 if 0; 1 if even #dig; 2 else
spl =: (-@-:@# ]\])&.":            NB. split=group by half length&.":
NB. Good use for M. : 15x faster
step=: ;@:(<@(1:`spl`(*&2024)@.cond M."0)) NB. agenda + memoize
p1 =: [: # step^:25@:par
NB. Part 2: same, but for 75 steps. Previous approach predictably breaks down.
NB. remedy: recursion instead of iteration; don't compose entire array, but return count
cond2=: 1 i.~ (0>[),(0&=,0=2|10 >.@:^.>:)@]NB. 0 if end recurse; 1 if 0; 2 if even #dig; 3 else
NB. Recursion verb: x: current level (for stopping in time); y: list of numbers
NB.   sum  dec it cnt  0->1 ; spl even ; *2024  when cond  memoize
rec=: [: +/ (<:@[ #@]`($: 1:)`($: spl)`($: *&2024)@.cond2 ])M. "0 NB. note M."0 faster than "0 M.; without never finishes.
p2 =: 75 rec par@]
0
}}
12 day {{ NB. Garden Groups
NB. Part 1: Find cost of fence for garden per contiguous plant type
NB. cost =: area * perimeter
par=: ];._2
pad=: |:@([,,~)^:2
NB. Contribution to perimeter with ;._3=#ULRD neigh different from center
peri =: (1 1,:3) ([:+/4&{ ~: 1 3 5 7&{)@,;._3 '.'&pad
NB. Area via flow iteration by plant type
sh =: 5 2$0 0 0 1 1 0 0 _1 _1 0 NB. self+RULD shifts
NB.     no .  *   max self,neighs by shift converge ids for non-.
flow =: (*@{. * [:>./{.>."2}.)@:(sh |.!.0/])^:_ @:(*>:@i.@$)
NB. get indices for entire field
NB. max~type  field id   binarise (by unique plant types)
ind  =: (>./)@:(flow"2)@:(="_ 0 ~.@,) NB. faster than t.''
NB. sum  ind  area*perimeter
p1=: +/@(ind ((#*+/)/.)&, peri)@:par
NB. Part 2: Not the perimeter counts, but number of sides
{{)n 000 001 010 011 100 101 110 111
..  .#  #.  ##  ..  .#  #.  ## careful not to double count
.#  .#  .#  .#  ##  ##  ##  ##
 1   0   1   0   0   1   0   0 corner on bot right block in the middle
 0       2           5
Do for 4 corners
}}
NB. kernel to be executed on 3x3 win~number of corners of center block
NB.       sum 0 2 5 at 4 corners (inds of borders, in CW order)
ker  =: [: +/ 0 2 5 e.~ #.@:((0 2 8 6 (],.[,.1|.]) 3 1 5 7)&{)
NB. convolve 3x3  kernel same ,       after pad
corners =: (1 1,:3) ker@(= 4&{)@,;._3 '.'&pad
p2=: +/@(ind ((#*+/)/.)&, corners)@:par
0
}}
13 day {{ NB. Claw Contraption
NB. Part 1: given 2 buttons each with x-y amounts, what's the least amount of presses to gain most prices?
NB. note: only positive incr, no neg.
NB. Parsing a little complicated; 3 lines per machine-> 320x3x2
par =: _ 3 2 ($,) [: ((".@:#~ e.&'0123456789 ');._2~LF2&E.) LF,~]
NB. 3 1 +/ .*8400 5400 %.(94 22,:34 67)
sol =: [: +/ 3 1 +/ .*~ (#~ (-:<.)"1)@:(({:%.|:@}:)"2)
p1  =: sol@:par
NB. Part 2; whoops, prizes lie 10000000000000 further
inc =: 0 0 10000000000000x+"2 ]
p2  =: sol@:inc@: par
tst=: {{)n
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
}}
0
}}
14 day {{ NB. Restroom Redoubt
NB. Part 1: Given robot locations and velocities, where are they after 100 steps?
NB. Parse to Nx2x2 $ (x/y, p/v, N)
par =: [: |: _ 2 2 ($,) [: 0&".;._2 ', 'rplc~ (#~ e.&(Num_j_,',- ',LF))
NB. Aim for P2 saying: ow, it was not 100 but 100 billion steps.
dims =: 101 103
NB. give modular arithmetic a swing; modular update: u is field size, x=#steps and y is 2xN array for p and v
NB.        p   + #steps * v , bothe mod u
upd =: {{ {.@] + m. u [ * m. u {:@] }}
fp  =: (({.dims) upd)`(({:dims) upd)"2 NB. use diff mods for X and Y
NB.  steps mul per quad=rem any 0     sig rot  center    final pos
p1  =: 100 */@:(#/.~)@:(#~ [:-.0&e."1)@:*@:|:@(-&(-:@<:dims))@:fp par
NB. Part 2: Unexpectedly: when do most of the robots form a christmas tree?
NB. number of robots with neighbours; Tree requires them to be clustered, so look for time with max number of robots with neighbours
sh =: 4 2$0 1 1 0 0 _1 _1 0 NB. shifts in 4 dir
NB.      sum  any neigh># find neigh-ind in inds (y)
nn =: (# +/@:(+./"1)@:> (i. sh +"1/~ ])) NB. # of robots with neighbours
NB.         argmax   10000 s   nn  find pos  parsed
p2    =:([: (i. >./) (i.10000) ;@:(nn@:|:@:fp t.''"0 _) par) NB. threaded 3.8x faster!
NB. p2    =:([: (i. >./) (i.10000) nn@:|:@:fp"0 _ par)
tree  =: p2 |:@:fp par
NB. use: viewmat verif io''[load'viewmat'
NB.        |: field of '.'  insert # at pos@tree time
verif =: [:|:(101 103$'.') '#'"1@]`(<@])`[} tree
0
}}
15 day {{ NB. Warehouse Woes
NB. Part 1: Given field and directions, find locations of boxes after making steps. Boxes can be shoved no matter how many as long as not against the wall.
par=: [: one`two"0 (<;._2~LF2&E.)@(,&LF)
two=: ('>v<^' i. LF -.~ ])&.>
one=: ([: ];._2 ,&LF)&.>
NB. 50x50 field and 20000 moves
sh=: 4 2 $ 0 1 1 0 0 _1 _1 0 NB. RDLU shifts = >v<^
NB. Approach: give each box an id, get coords, make neighbour graph. eoa indicates space. Amend where wall to indicate self to stop chain.
cg =:(0 I.@:=[) [`[`]}"1 (i.!.0 sh +"1/ ])@] NB. Create graph from coords y and types x; 4 x #y
NB. Find chain (ending in space or block) from pos in gr in dir; returns inds.
chain =: {~ :: ] ^: a: NB. x: i{gr; y: starting ind
NB. Update; x: dir; y: robot pos; returns 0 (state in global vars gr;co)
upd =: {{
  mv =. co i.!.0 nrc=. (x{sh)+y NB. Ind of box at mv pos of robot
  NB. No wall or box in direction: just step
  if. mv=eoa do. nrc return. end.
  c =. eoa-.~(x{gr) chain mv     NB. List of ro/boxes to move
  if. ty{~{:c do.                NB. Last is not wall
    co =: ((x{sh)+"1 c{co) c} co NB. Update pos of robot+boxes
    gr =: ty cg co               NB. Recreate graph
    nrc
  else. y end.                   NB. Wall: robot keeps pos
}}
p1=: {{
  'f i'=. par y            NB. Field & instructions
  ty =: 'O' = '.@' -.~ , f NB. 0 wall; 1 box
  rc =. ($ #: '@' i.~ ,) f NB. Coords of robot in field
  NB. Graph and coords of any non-space.
  gr =: ty cg co=: ($#: '.@' I.@:-.@:e.~ ,) f
  eoa=: #ty NB. Set indicator value for space (used in upd)
  NB. Find chain from robot pos; if last = space: move, if last is block, stop.
  rc ] F.. upd i NB. Fold upd over instructions
  NB. Weigth coord of boxes where type=1, i.e. box.
  +/ 100 1 +/ .*~  co (#~ 1=]) ty
}}
NB. Part 2: The same, but all units in are double in width (but for robot, and step length).
NB. Part 1's reasoning holds, though needs some adaption for:
NB. - 2 wide blocks: keep coord, but add 1 in checking
NB. - Consequently, split graph in u/d (2 neigh) and LR (1 neigh)
NB. - Robot only 1 wide: take out of gr/ty/co and use coords directly.
shlr=: 2 2 $ 0 2 0 _2 NB. R L shifts = ><
NB. Use chain as before for LR, tree for UD.
cglr =:(0 I.@:=[) [`[`]}"1 (i. shlr +"1/ ])@] NB. Create graph from coords y and types x; 4 x #y
shud=: 2 3 2 $ 1 _1 1 0  1 1  _1 _1 _1 0  _1 1 NB. UL U UR DL D DR shifts = v^
cgud =:(0 I.@:=[) [`[`]}"1 (i. shud +"1/ ])@] NB. Create graph from coords y and types x; 4 x #y
NB. Tree search of graph direction x (2xN) of index y
tree =: {{
  eoa  =. {:@$ x
  keep =. y
  while. #y do.
    y =. eoa-.~ ~.x,@:({~"1) y  NB. Look up, keep unique, rem spc
NB. Self-ref = wall so return 1st wall to signal.
    if. *#bl =. keep ([-.-.) y do.  {.bl return.
    else. keep =. keep,y end.  NB. Update keep, continue.
  end.
  keep
}}

NB. Update x: dir; y: robot pos; returns 0 (state in global vars gr;co)
upd2 =: {{
  mv =. ((],0 1+"1]) co) i.!.0 nrc=. (x{sh)+y NB. Ind of box at mv pos of robot
  NB. No wall or box in direction: just step
  if. mv=2*eoa do. nrc return. end.
  if. selgr['selgr selopt'=. 2 (|,<.@%~) x do.
    c =. eoa-.~(selopt{grud) tree  (#co)|mv NB. List of ro/boxes to move
  else.
    c =. eoa-.~(selopt{grlr) chain (#co)|mv NB. List of ro/boxes to move
  end.
  if. ty{~{:c do. NB. Last is not wall
    co =: ((x{sh)+"1 c{co) c} co NB. Update pos of robot+boxes
    grlr =: ty cglr co NB. Recreate LR graph
    grud =: ty cgud co NB. Recreate UD graph
    nrc                NB. Return new robot position
  else. y end.         NB. Wall encountered, robot keeps pos 
}}
p2=: {{
  'f i'=. par y NB. Field & instructions
  ty =: 'O' = '.@' -.~ , f     NB. Types: 0 wall; 1 box
  rc =. 1 2*($ #: '@' i.~ ,) f NB. Coords of robot in field
  NB. Graphs and coords of any non-space
  'grlr grud' =: ty (cglr;cgud) co=: 1 2 *"1 ($#: '.@' I.@:-.@:e.~ ,) f
  eoa =: #ty NB. Set indicator value for space (used in upd)
  NB. Find chain from robot pos; if last = space: move, if last is block, stop.
  rc ] F.. upd2 i NB. Fold upd2 over instructions
  NB. Weigth coord of boxes (based on type)
  +/ 100 1 +/ .*~  co (#~ 1=]) ty
}}
NB. Visualisation helper when given co as argument
vis=:  ('@','#[]'{~(,+:)@[)`(<@(0&{::,(,0 1&+"1)@:(1&{::))@])`('.'$~1 2+>./@;@])}
tst=: {{)n
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
}}
tst2=:{{)n
#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^
}}
lrg=:{{)n
##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
}}
0
}}
16 day {{ NB. Reindeer Maze
NB. Part 1: Find lowest scoring path, with steps being 1 and turns being 1000 points.
NB. Begs for a recursive approach; but turned out to crash JAndroid...
NB. Graph of spaces is not enough, as extra cost for turns is to be takeo into account. So I'll split the graph in 4 parts (one for each direction) having connections only to the same spot in two neighbouring planes (i.e. the same spot in the original graph, but facing different directions).
sh=: 4 2 $ 0 1 1 0 0 _1 _1 0 NB. shifts >v<^
p1=: {{
  NB. Get positions of S, E and spaces in field
  'S E sp' =. 'SE.' <@($@] #: (I.@:= ,))"0 _ ];._2 y
  NB. Generate original graph for all spaces;start=0 end=1
  gro =. (i. sh +"1/]) coo=. S,E,sp
  NB. Upgrade graph to separate directions in different planes
  NB.     gr update "end" for full gr;gr+ i.4  x N
  upgr =. (] (4*#@[)`([:I.#@[=[)`]}"1 ] + (i.4) * {:@$) gro
  NB. Now directions entirely separated. Add connections between directions:
  NB.   join  add   rotd cross to~:dir
  gr =. ,/@:((,"0 1) 0|:(1&|. ,: 3&|.)@i.@$) upgr
  co =. ,/ 4#,:coo NB. coordinates for full graph
  isE=. e.&(co I.@:(-:"1) {. E) NB. is (c-i) entry E?
  eoa=.#gr  NB. End-of-array, i.e. no actual neighbor
  min=. _   NB. Minimum found so far
  cur=. 0 0 NB. At S in east direction; cost, ind
  while. #cur do.
    'cost ind'=. |:cur
    NB.      replic x3 +st or rotate repl # new inds
    cost =. cost ((3#[) +1 1000 1000$~#@]) ind=. , gr{~ind
    NB. Prune E, hopeless and non-neighbours
    Es   =. isE ind NB. reached the end
    min  =. min <. Es <./@:# cost NB. Keep best score
    NB. Keep where not E, eoa or score already > min.
    keep =. Es +: (eoa=ind) +. (min <: cost)
    NB.  first per ind sort; cost and ind where keph.
    cur  =. ({./.~ {:"1) cost (,./:[)&(keep&#) ind
    NB. cur =. ind (,~ {.@/:~)/..&(keep&#) cost NB. Slower!
  end.
  min
}}
NB. Part 2: Find best seating position: How many squares are part of any best route?
NB. After trying to extend my previous approach, it turns out too slow/heavy, so use good old Dijkstra...
p2 =: {{
  NB. Get positions of S, E and spaces in field
  'S E sp' =. 'SE.' <@($@] #: (I.@:= ,))"0 _ ];._2 y
  NB. Generate graph for all spaces;start=0 end=1
  np=. {:$ gro =. (i. sh +"1/]) coo=.S,E,sp
  NB. Upgrade graph to separate directions in different planes
  NB.     gr update "end" for full gr;gr+ i.4  x N
  upgr =. (] (4*#@[)`([:I.#@[=[)`]}"1 ] + (i.4) * {:@$) gro
  NB. Now directions entirely separated. Add connections between directions:
  NB.   join  add   rotd cross to~:dir
  gr =. ,/@:((,"0 1) 0|:(1&|. ,: 3&|.)@i.@$) upgr
  co =. ,/ 4#,:coo NB. Coordinates for full graph
  Es =. (co I.@:(-:"1) {. E) NB. all nodes corresponding to E
  dist=. _ $~ #co  NB. Shortest distance from start
  prev=. a:$~ #co  NB. Shortest distance neighbours
  q =. i.#co       NB. Queue has all nodes
  dist=. 0 (0}) dist NB. Only S has weight 0
  while. #q do.   NB. While q not empty...
    nod =. q([{~(i.<./)@:{)dist NB. Find node with least dist
    q   =. q -. nod             NB. Dequeue nod
    NB. Weight/node for each edge having dest still in queue
    'w nn'=. |: (e.&q # 1 1000 1000,.]) nod{gr
    if. -.*#w do. continue. end. NB. Drop dead-ends
    old  =. nn{dist          NB. Old dist for comparison
    new  =. old<.w+nod{dist  NB. New dist for neighs found
    dist =. new nn}dist      NB. Upgrade distances
    NB. If new better, replace current prev; if new equal, add nod; if new worse, keep current prev; make right verb for each
    NB.      add  keep repl     =0; >1; <_1
    ops  =. ((,~&.>)`]`(<@[)) {~ new *@- old
    NB. prev =. (nod ops"0 ])&.(nn&{) prev NB. loooot slower!
    prev =. (nod ops"0 nn { prev) nn}prev
  end.  NB. At this point, the result of p1 is <./Es{dist
  NB. Walk all paths back to source (which has empty prev)
  bestxbyy=. ( [#~ (=<./)@:{) NB. x where y is minimal
  NB. 'prev' is not 100% correct, due to arrival directions not being recorded. So when walking back, check whether difference in distance matches the step taken (straight or turn), and keep only valid steps to add. Due to graph construction, we know that if the nodes are in the same np-sized block, the are in the same direction
  NB.   diff in dist =   turn if not in same dir.
  valid=. -&({&dist) = 1+999 * np ({.~:}.)@:<.@:%~ ,
  NB. Single step (assumes, due to huge difference in turns, that all paths have the same length). Fun fact: 96 different optimal paths are found. If this were much much bigger, should have used recursion instead.
  NB.          box append   valid   prev nodes to each path
  step=.;@:(([:< ] ,"1 0 ((valid#]) {::&prev@])@:{:)"1)
  NB. np| converts back to spatial pos; walk until last col all zero's (at S); start with a single path at lowest distance E.
  #~.np|,step^:(0 +./@:~: {:"1)^:_ ,:,: {. Es bestxbyy dist
}}
tst=:{{)n
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
}}
0
}}
17 day {{ NB. Chronospatial Computer
NB. Part 1: Execute 3 bit opcode, val
NB. parse into program and A B C ip
par=: [: (>@{: ,&< 0,~[:;3{.]) <@".;._2@:(#~ e.&(LF,',',Num_j_))
'`xor div pt'=:22 b.`(<.@%)`(32 b.&1@]) NB. xor, int div, 2^
NB. access to state (on the left)
'`A B C ip'=: 0&{@[ `(1&{@[)`(2&{@[)`(3&{@[)
ob =: 4&}.@[ NB. output buffer comes after ABCI
NB. Values, dyad, x=A B C, y=opcode valcode; returns value
combo=: {.@] , 0:`1:`2:`3:`A`B`C@.({:@])
iip =: 2+ip NB. increment instruction pointer
fetch =: (0 1+ip) {  ] NB. fetch current instruction
NB. Ops; dyad, x=A B C, y=value; returns new A B C ip ob
NB. oc:0  1   2   3   4   5   6   7
op =: adv`bxl`bst`jnz`bxc`out`bdv`cdv@.({.@])
adv =: [ (((A div pt), B, C, iip,ob){:) combo NB. A div 2^comb
bdv =: [ ((A, (A div pt) , C,iip,ob){:) combo NB. same B
cdv =: [ ((A, B, (A div pt), iip,ob){:) combo NB. same C
bxl =:    (A, (B xor ]), C,iip,ob){: NB. xor B with lit
bxc =:    (A, (B xor C), C,iip,ob){: NB. xor B with C
bst =: [ ((A, (8 | ]), C,iip,ob){:) combo NB. mod 8 of B
jnz =:    (A,B,C,(iip`(])@.(0~:A)),ob){: NB. jump
NB. out =:    (A,B,C,iip,ob,7 (17 b.) {:@combo) NB. output
out =:    (A,B,C,iip,ob,8 | {:@combo) NB. output
p1  =: [:}.@;[:(','<@,":)"(0) 4 }. ([op fetch)~ ::]^:_&>/@:par
NB. Fold based bruteforce too slow, given high number A.
NB. Hand-"decompiled" version of my program
{{)n
i prog op  arg J
0:2 4  bst A   B =. 8 | A
1:1 3  bxl 3   B =. B xor 3
2:7 5  cdv B   C =. A div 2^B
3:0 3  adv 3   A =. A div 2^3
4:1 5  bxl 5   B =. B xor 5
5:4 4  bxc C   B =. B xor C
6:5 5  out B   ob=. ob,8|B
7:3 0  jnz 0   goto 0 if A~:0 => while. A do. ... end.
}}
check=:{{ NB. y is initial value for A.
  'A B C'=. y, 0 0
  ob =. ''             NB. output buffer
  while. A do.         NB. 7: jnz 0=restart while A
    B =. 3 xor 8|A     NB. 0 1 only uses last 3 bits of A!
    C =. A div pt B    NB. 2 replace with shift
    A =. A div 8       NB. 3
    B =. C xor B xor 5 NB. 4 5
    ob =. ob , 8|B     NB. 6
  end.
}} NB. Surprisingly, this is *exactly* as fast as p1 io''
NB. Every it/output consumes 3 bits of A. so should be between <:2x^3*16 17; decidedly too much to bruteforce (unless very lucky)
NB. approach: The program has dependent loops, depending on A, and based thereon overwrites B and C before they are first used in each loop. Next loop's A is A shifted to the right 3 bits. So build up A in reverse by adding all 8 possible sets of 3 bits and shifting it to the left.
NB. Need to keep *all* multiple possible solutions as not all lead to valid solution in next loops. So work on list of candidates, leading to matrices of ac, c, oc.
p2 =: {{
  co =: 0{::par y
  a =. 0
  b =. 3 xor i.8 NB. b candidates always the same
  b5=. 5 xor b   NB. same for b xor 5 in befor last line.
  for_ov. |.co do. NB. walk back!
    ac =: 8 (i.@[ +/ *) a NB. add candidate bits to A found
    c=. ac div pt b   NB. c candidates
    oc=. 8|c xor b5   NB. find outputs for each candidate
    a =. ; ac <@:#~"1 oc = ov  NB. update A to the found candidate
  end.
  NB. Thought I found a bug in AoC, as 2nd smallest value of what I found was accepted, rather than smallest
  NB. found 236539226447407; but 236539226447469 correct for AoC
  NB. However was due to int->float by 2^. replacing by logical shift (32 b.) solves the issue. Could probably have used float16 instead as well (e.g. a=: 0fq)
  {./:~a
}}
NB. note: don't use tst for p2 as it does not repeat.
tst=: {{)n
Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
}}
tst2=:{{)n
Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
}}
0
}}
18 day {{ NB. RAM Run
NB. Part 1: Shortest path throug memory after 1024 bytes have fallen. Forseable part two: too many blocks falling, run while you can, with changing field.
par=:".;._2 NB. convert to coords
NB. dijkstra; x=field y=state = dist/prev/q
NB. if need to change, remove blocked paths from q.
NB. could optimise (array-ify; work on all nodes at dist i) as each step has weight 1, but not worth the effort.
st =:{{
  'dist q'=.y NB. distances,  queue (as mask)
  nod  =. (i.<./)(6e3*-.q)>.dist NB. Find node with least dist
  q    =. 0 nod} q               NB. Dequeue nod
  NB. Nodes for each dest still in queue
  nn   =. (#~ {&q) nod{x
  dist =. (nn (<.>:)&({&dist) nod) nn}dist NB. Upgrade dists
  dist,:q
}}
sh =: 4 2$(,-)0 1 1 0 NB. RDLU shifts
NB. graph; x: how many blocks to drop; y: list of block coords
NB.  ind  shifted   first x rem all coords range of input
gr =: (i. sh +"1/~ ])@({. -.~ ( #:[:i. */)@:(1+>./)@])

NB. init to len+1 for non-neigh, first dist to 0 (start), last not queued as not a real node
in =: 0 (<0 0,:1 _1)} 6e3 1 #"0~ 1+#@[
NB. dist end @1kb drop  step till end reached  init  parsed
p1 =: _2 { {.@(1024&gr ([ st^:(6e3=_2{ {.@])^:_. in)])@:(|."1)@:par
NB. Part 2: What is the first block that will block the way to the exit?
NB. binary search over 1024-#par io'' should take 12 it at most. x: fixed info; y:A,B(inclusive); u verb returning 0 or 1 selecting A-mid or mid-B
NB. trick to get an adv taking middle tine: part bound conj:
NB. C (A(]:]:B)) => A C B
bs =: (([(]:]:mid@])) { nr@])^:(1<-~/@])^:_. ({.@:)
mid=: <.@-:@:(+/)        NB. midpoint between 2 num in y
nr =: 2 ]\ {. , mid , {: NB. 2 new ranges to pick from.
NB. core: x:# blocks to drop; y: parsed input
NB. returns exit clear (1) or not (0)
core=: 6e3 ~: _2 { {.@(gr( [ st^:(6e3=_2{ {.@])^:_ in) ])
NB.  pick #bl rng bin search par; split par for easy output.
p2 =: ({~ (1024,#) core~bs~ ".)@:(];._2)

tst=: {{)n
5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
}}
0
}}
19 day {{ NB. Linen Layout
NB. Part one: given available towels (~sylables) find how many of the designs (~words) can be made.
NB. Parse tow&desi    <rem space  block sep by 2 LF's
par=: [: part`pard"0@(<@(' '-.~]);._2~ LF2&E.) LF,~]
  part=: ([:s:@(\:#&>)[:<;._2',',~])&.> NB. towels (asc len)
  pard=:<;._1&.>                        NB. designs
NB. Posible? x:towels (boxed); y: designs (open)
pos =: {{
  if. -.*#y do. 1 return. end. NB. if none left: good.
  NB. Assumption max towel length = 8
  if. +./ in=.|.(x e.~ s:@<)\ 8{. y do.
    ls =. in# (1+i._8) NB. lengths of towels found
    NB. Fold so sequentially try longest prefixes first (hence sort in part and |. for in), and break when 1 possible found
    NB. ffwd  val  stop if 1  pos y beheaded by towel lengths
    1 ] F.. ((] [: ([ 1&Z:) x pos y }.~ ])~) ls
  else. 0 return. end. NB. not possible.
}}
NB. rewritten for using symbols. ~10x faster!
NB. t. 0 : 2x faster!
p1 =: [: +/ [: ; (pos  >)t.0"_ 0&>/@:par
NB. Part 2: Sum of # ways each design can be made.
NB. num counts number of ways y. Uses globals T, M, K, V for towels, mutex, cache keys & values.
num =: {{
  if. -.*#y do. 1 return. end. NB. if none left: 1 possible
  NB. y in cache? (K querried once) return cached
  if. >/ ci =. K (#@[,i.) ys=. s:<y do. V{~{:ci return. end.
  NB. |. removed, need to search all anyhow.
  if. +./ in=. (T e.~ s:@<)\ 8{. y do.
    ls=. in#(1+i.8)        NB. lengths of towels found
    r=.+/ ls num@:}."0 _ y NB. #ways for each beheaded y
    if. -. 11 T. M do.  NB. If mut lock; else skip
      if. ys -.@e. K do.   NB. If y still not in cache, add.
        K=:K,ys [ V=:V,r   NB.   if yes; skip
      end.
      13 T. M              NB. Release lock
    else.
      echo'no lock'
    end.
    r return.              NB. Return r
  else. 0 return. end. NB. not possible.
}}
NB. Tried num as for pos but need to go cache, too slow!
p2=: {{ NB. Filtering d with part 1 is slower!
  'T D'=. par y NB. Global so less passing around
  NB. Thought of initialising K to T, but wrong, as some towels are combinations of others... so initialise empty
  K=: s:''[V=:0$0 NB. Keys;vals stored chunks encountered
  V=: 0$0         NB. corresponding values
  M=: 10 T. 0     NB. mutex for updating
  NB. +/ ; num t.0@> d NB. bugs ahead! Inconsistent results with j 9.6 Anyhow does not seem much faster...
  +/ num@> d
}}
tst=:{{)n
r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
}}
0
}}
20 day {{ NB. Race Condition
NB. Part 1: Cheating maze solution: 1 cheat of at most 2 squares. How many cheats would give at least 100 units difference?
shc=: 2*sh=: 4 2$(,-) 0 1 1 0 NB. shifts, cheat & regular
NB. Dijkstra; x=field y=state = dist/prev/q
NB. If need to change, remove blocked paths from q.
NB. Could optimise (array-ify; work on all nodes at dist i) as each step has weight 1, but not worth the effort.
st =:{{
  'dist q'=.y NB. Distances,  queue (as mask)
  nod  =. (i.<./) dist ([ >. #@[ *-.@]) q NB. Find node with least dist
  q    =. 0 nod} q NB. Dequeue nod
  NB. Nodes for each dest still in queue
  nn   =. (#~ {&q) nod{x
  dist =. (nn (<.>:)&({&dist) nod) nn}dist NB. Upgrade dists
  dist;q NB. ; faaar faster than using ,:
}}
init=: (0:`[`(#~@])} ; 0{.!.1~-@]) >:@#
pb=:{{ NB. Solution for both, x: cheat shifts; y: io''
  NB. Determine cheating shift weights, as normal moves take 1, but cheats take more
  w =. +/"1@:| x NB. cheating step weights
  NB. Coords; S E, and then all other locations
  co =. ;'SE.' <@($@] #: (I.@:=,))"0 _ ];._2 y
  NB. Full graph as usual
  gr =. (i. sh +"1/~ ]) co NB. regular  graph
  gc =. (i. x  +"1/~ ]) co NB. cheating graph
  NB. As all points have valid cheats, do Dijkstra until convergence, giving min dist to all points. All points are also valid cheat destinations.
  NB. vis:  viewmat (dtS/E) (<co)} (-'#'=];._2 io'')
  dts=. 0{:: 0 (] st^:_ init) gr NB. dist to S of each point
  NB. Only single track, so dstE doesn't need dijkstra
  dte=. ((>./-])@:}: , {:) dts
  NB. Piece together dts, dte, gc and w, to find best cheats.
  NB. dts is pre-cheat; dte is post-cheat, w=cheat dist
  (_100+1{dts) +/@:>: , (}:dts) + w +"1 gc{dte
}}
p1=: shc&pb
NB. Part 2: Now, 20 ps cheats -> 20 wide diamond
NB. For once, I actually wrote something that is reusable for part 2 with minor adjustment:)
shC =: (0 0 , sh) -.~ (#~ 20>:+/"1@:|) ,/,."0 1~i:20
p2=: shC&pb
tst=:{{)n
###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
}}
0
}}

NB. temporary storage
echo run 20
NB. cocurrent'd20'[load'~A/2024.ijs'
NB. vim: ts=2 sw=2 et fdm=marker foldmarker={{,}}
