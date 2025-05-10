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
3 day {{ NB. Mull it over
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
  NB. blocks ab gr: add blocks to graph to the right. blocks should be all blocks in graph.
  NB.  gr swap dir   index in lookup
  ab =. ] {"1~ lu {~ #.@:e.~ 
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
NB. temporary storage
echo run 14
NB. vim: ts=2 sw=2 et fdm=marker foldmarker={{,}}
