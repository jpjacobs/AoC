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
mem =: (# _1}:@,@:,.~i.@>.@-:@#)  NB. memory with file ind; _1=spc
NB. recursive update; prevalloc , (i.&_1 
ind =: _1 (i.&1 , i:&0)@:= ]
upd =: (ind (|.@:{)`[`]}^:(</@[) ]) 
NB. memup =: upd mem
score=: i.@# +/ .* ] * _1&~:
p1=: score@:(upd^:_)@mem@:par
tst=: '2333133121414131402 '
0
}}
NB. temporary storage
echo run 9
NB. vim: ts=2 sw=2 et fdm=marker foldmarker={{,}}
