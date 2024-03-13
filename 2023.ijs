NB. AoC, 2023
if. -. fexist '~addons/general/jaoc' do.
  install'github:jpjacobs/general_jaoc'
  echo 'installed general/jaoc; go set up COOKIE.txt'
end.
load'general/jaoc'
NB. setup this year 
setup_aoc_ 2023
boxdraw_j_ 1
NB. TODO jaoc:
NB. - if split off utility library next year:
NB.   * connected component analysis (day 4)
NB.   * interval arrithmetics (split, merge, ... day 5)

NB. spin up threads up to # of cores 
0&T.@0^:(0>._1+([: {. 8&T.)-1&T.) '' 
1 day {{ NB. Trebuchet?!
p1=: [: +/ ".@({.,{:)@:(#~e.&Num_j_);._2
NB. easy. but simple replace doesn't work, since it replaces in the order of the replacement values, rather than occurrance in th  string
num=: ;:'zero one two three four five six seven eight nine'
fix_old=:{{ NB. previous version.
NB. replace first occurring number until done
  while. #r=.($#:I.@,)|: num (E.~>)~"0 1 y do. NB. pos,num pairs
    'l d'=.{.r
    NB.  digit@loc DO NOT remove extra length from y
    y=. ({.":d) l} y
  end.
  y
}}
tt=: {{)n
two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
}}
NB. underdocumented cornercase: eighthree => 83 and sevenine => 79
NB. more J like fix using composite item; the verb looks up numbers, and replaces those positions with the corresponding numbers.
NB. fix =: [: ([: i:&1"1@|: 1,num (E.~>)~"0 1{.)}@|:,"0 1&Num_j_
fix =: [: ([: i:&1"1@|: 1,num E.S:0 {.)}@|:,"0 1&Num_j_
p2  =: p1@fix
0
}}
2 day {{ NB. Cube Conundrum
tt=:{{)n
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
}}
NB. 100 games, seq numbered 1-100; parse to box per game, with each game having boxed turns
par =: <@:(_2 ]\&.> [: ". }.&.;:@:rplc&', ');._2@:r1
r1  =:rplc&(('Game';''),;:'red 0 green 1 blue 2')
NB. neg limitations for summing, see where neg
lim =: _2]\_12 0 _13 1 _14 2
NB. possible: sum of head keyed by tail <:0 each game
pos =: ([: *./ ([: ([:*./0>:{:"1 +//. {."1) lim,])&>)&>
p1  =: +/@:>:@I.@:pos@par
NB. power:*/ min number of blocks required per game; assumes no duplicate colours per turn.
pow =: ([: */@({:"1 >.//. {."1) ;)&>
p2  =: +/@:pow@par
NB. parsing most difficult; 
0
}}
3 day {{ NB. Gear ratios
  NB. analysis: 0 not present; 1 is present; each island max 1 symbol
tt=:{{)n
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
}}
NB. given field find which numbers are not adjacent to a symbol
pre =: {{
  lmat  =: , mat   =. '.',.~];._2 y               NB. matrix, extraxol for breaking numbers on edges  
  ind   =. ($mat)#: lind=:I.@, (e.'.'-.~~.@,) mat NB. non-dots
  sh9   =. _1+3 3#:i.9 NB. 9 neighbourhood
  neigh =. [: |:@(,#) sh9 (]i.+"1/~) ] NB. neighbours, takes ind
  NB. takes max for connected components; _1 for not found; discard not found afterwards
  cc    =. }: (neigh (>./@:{)^:_ (_1,~i.@#)) ind NB. connected components, takes ind
  NB. vis: viewmat (}:cc) ind} ($mat)$0
  NB. per cc: lookup, convert to words then to num; symbols will generate empty boxes; result is sum of numbers or 0 if no symbol
  NB. wrong:20151230, likely non-contiguous digits considered as one number; or numbers diagonally close
  NB. +/((+/@;*a:&e.)@:(".&.>)@:;:@:{&sym/. i.@#) cc
  NB. box runs of same type and consecutive indices
  NB.   frets for or different type or step ~: 1
  br=: (<;.1~1,(2 ~:/\ Num_j_ e.~ {&lmat)+.1~:2-~/\])
  cc
}}
miss=:{{  NB. y=cc, global lind & lmat
  NB. Per cc: sum*has sym  ; do after lookup after boxing runs of inds & types (note that ".'$' produces empty) 
  y (+/@;*a:&e.)@:(".@:{&lmat&.>)@br/. lind
}}
p1=: +/@miss@pre
gearratios =: {{ NB. y=cc, global lind & lmat
NB. find gears, having 2 numbers and 1 *
  NB. per cc: box after lookup per run after boxing runs
  islands=. y <@:({&lmat&.>)@br/. lind
  NB.  keep if has *    and len=3
  gears=. (#~ ('*'e.;)&> *. 3=#&>) islands
  NB. do get * between num (TAO of J)
  ([:".@; _1|./:~)&> gears
}}
p2=: +/@gearratios@pre 
0
}}
4 day {{ NB. Scratchcards
tt=:{{)n
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
}}
NB. part 1: score 1 2 4 8...
NB. parse to list of #co-occuring numbers
par=: ([: +/@:e.&>/ [: <@".;._1'|',9&}.);._2
p1 =: +/@(* * 2&^@:<:)@par
NB. Fold multiple fwd, start with extra 1 in state to keep initial card in count. {. keeps current card count for each card. Win reduces #state at every iteration to keep track of where copies*ones should be added.
p2=: +/@:({. F:. win~ 1$~1+#)@par
NB. dyad: x: single #match; y: state
NB.   prev +copies* ones for len x
win=: }.@] + {.@] * (> i.@<:@#)
NB. Alternative solution; recursive:
NB.  30% faster, 10x fatter (likely due to passing matches entirely)
pp2=: (rec 1$~#)@par
NB. recursive sol; x: matches; y: state
rec=: {.@] + (}.@[ rec (}.@]+{.@] * {.@[ > i.@<:@#@]))`0:@.(0=#@]) 
0
}}
5 day {{ NB. If you give a seed a fertilizer
pseed=: [: ". 7 }. {.
pmap =: [: (([: <@|: "."1);._1~ +./@:e.&Alpha_j_"1) }.
NB. extract seeds and maps; removing empty lines
par  =: [: (pseed ,&< pmap) ];._2@(#~ LF2 -.@E.])
tose=: ([,_1++)/ NB. start len to start end
NB. verified no overlapping ranges
NB. put matrix in 3 rows: diff,start source, end source
dse =: ({.-1&{),1&{ ([,:_1++) {:
NB. map for use with F. x: dse matrix; y values
map=: ]+(0,~{.@[) {~ (({:@[>:])i.&1@:*.1&{@[<:])"_ 0
NB. find smallest value in orig seeds
p1 =: (0&{:: ([: {.@/:~@([#~e.~)] F.. ((map~>)~)) [: dse&.> 1&{::)@par
NB. This sucks: in part 2 seeds are ranges too large to bruteforce. Need to use assumptions checked before: source ranges and dest ranges don't overlap themselves.
tst=:{{)n
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
}}
stdrg =: (/:}."1)@|:@dse&.>
NB. source ranges and backward ranges are:
NB. - almost all adjacent, complete on 0-big
NB. - non overlapping
NB. so work backward, recursively for each range
NB. inverse ranges, so reverse mapping

invrg=: [: |. cr@(/: 1&{"1)@(-@:({."1),.({.+}.)"1)&.>
p2=:{{ NB. x seeds;y boxed dse mats
  SEEDS =: /:~_2 ({.,_1++/)\ x
  NB. lowest location seed
  ls=. (0,big) rec invrg stdrg y NB. Call with wide range
  NB. forward transform this seed ls to location
  ls ([: {.@/:~@([#~e.~)] F.. ((map~>)~)) dse&.> y
}}&>/@par

NB. recursion y: ranges from last to first; x: probe range
NB. since lowest ranges in output domain are always tried first, first match is guaranteed to be smallest value corresponding to a seed.
rec =: {{
  NB. echo (tab=.(#y)#' '),":x
  if. -.*#y do. NB. no more layers
    ovl=.(#~ {."1<:{:"1) SEEDS (>.&{.,<.&{:)"1 x
    NB. echo tab,'ovl ',":ovl
    {.@{.^:(*@#) ovl return. NB. min overlap of seeds & x=lower of first
  end.
  NB. dive deeper: split x by {. x & transform
  NB. assumes complete range and values between 0 and (2^32)-1
  NB. cr required not to loose parts of ranges in noop-zones
  NB.   keep valid    prb diff+ overlapping range      ranges
  new =.(#~ {."1<:{:"1)x({.@]+"1((>.&{.,:<.&{:)}.))&.|: >{.y
  for_nr. new do.
    if. #r=. nr rec }.y do. r return. end.
  end.
  0 2$0 NB. ran out of ranges, return empty
}}
big=: <:32(33 b.)1 NB. pseudo-inf
NB. create new array of ranges, including the non-range zones
cr =: {{ NB. in: |:dse ; out de array of completed ranges
  d=. {."1 y NB. diffs
  i=. }."1 y NB. induces
  NB. I. takes ENDs of intervals
  na=. 0,~2 (1<{.@{:-{:@{.)\ i NB.not adjacent; where to insert end. append 0 because last never pair.
  if. +./na do.
    where=.1+(i.+/na)+in=.I.na
    what =. 1 _1+"1(([:;/1 0,.~])"1(,.>:)in){i
    i=. what where} i#~1 j. na
    d=. d#~1 j. na
  end.
  NB. add missing and out-of-bounds value 0 in d
  if. 0  <ff=.{.{."1 i do. 'i d'=. ((0  , <:ff), i);(0,d) end.
  if. big>ll=.{:{:"1 i do. 'i d'=. ((big,~>:ll),~i);(d,0) end.
  d,.i
}}
NB. merging TODO: excise: not needed, but useful example
merge =: {{
c=.0
NB. check how to phrase; dimension-wise
while.1 do.
  if. (#rem)>ind=.1 i.~(,.~0 2)-.@e.~(rem=.y{~<<<c)I."1]_1.5 1.5+c{y do. NB. found one:merge
    y=. ((c{y) (<.&{.,>.&{:) ind{rem) ind}rem
    c=.0
  else. c=.c+1 end.
  if. (c=#y)+.1=#y do. break. end.
end.
y
}}
0
}}
6 day {{ NB. Wait for it
par=: ".@(10&}.);._2
p1 =: [: */ ir@poly"1@|:@par
eps=: (,-)_1e_10 NB. needed because draft doesn't count
NB.      1+ round towards center
ir =: [: (1+<.@{.->.@{:) eps+1{::p. NB. integer roots
NB. x^2-T.x+d = 0
poly =: 1 ,~ (,-)~/
p2 =: [: */ ir@poly"1@|:@(([:".' '-.~10}.]);._2)
0
}}
7 day {{ NB. Camel Cards
cards=:'23456789TJQKA'
NB. sort once on cards; J's sort (later by type) is stable.
NB.   sort by cards  bid        ,.~  card no's
NB. pre-part 2: par=:[: (/: }:"1) [: (([:".6&}."1) ,.~ cards i. 5{."1]) ];._2
par=:[: (/: }:"1) [ (([:".6&}."1@]) ,.~ [ i. 5{."1]) ];._2@]
NB. highest:  H,pair, 2 pairs       3oak,full house,   4oak,5oak card counts
typ1=:[:i:&1@(1,2&e.,(2=#@(#~ 2&=)),3&e.,(3 2 *./@e.]),4&e.,5&e.)#/.~
NB. pre-part 2: p1 =: ({:"1 +/ .* 1+[:/:@/: typ@:}:"1)@par
p1 =: cards ({:"1 +/ .* 1+[:/:@/: typ1@:}:"1)@par ]
cp2=: 'J' ([, -.~) cards
NB. reimplement typ to take into account jokers
p2 =: cp2   ({:"1 +/ .* 1+[:/:@/: typ2@:}:"1)@par ]
NB. note: pairs, double pairs, 3oak, 4oak with only J is always less than using them as joker. 5 J's could make sense.
nj=: 0 +/@:=]
pair =: *@[ +. 2&e.@]
two  =: 2<:(+#@(#~2&=))
three=: [:+./3<:+
full =: 2=#@]
four =: [:+./4<:+
five =: (5=[)+.[:+./5<:+
typ2 =:nj i:&1@(1,pair,two,three,full,four,five) [: #/.~ 0 -.~]
0 
}}
8 day {{ NB. Haunted Wasteland
NB. parse: returns 0 1 instructions (LR); num lookup list, sorted AAA-ZZZ
par=: [:(('LR'i.{.),&<(/:{."1)@:;:@:(-.&'=(),'"1)@(2&}.))];._2
p1=:{{
  'ins st'=. par y
  st=.({."1 i. }."1) st
  'cur ct'=. 0
  while. cur~:<:#st do.
    ct=.ct+1[cur=.(cur{st){~ ins{~ct|~#ins
  end.
}}
NB. that's wild... start at all nodes ending in A until all end up in node ending in Z. (both 6 occurrences)
NB. likely too much to simulate together. Find per starting point looplength
p2 =: {{
  'ins st'=. par y               NB. instructions & states
  'is id'=:<@I.'AZ'=/{:&>{."1 st NB. ind of source/dest
  st=.({."1 i. }."1) st          NB. convert states to ints
  ll=. 0$~#is                    NB. cycle lengths for starts
  NB. assumes one end in each cycle, verified.
  ends=. 0$~#ll                  NB. times when at end
  L=. #ins                       NB. instruction cycle length
  for_start. is do.          NB. all start loctions
    'cur ct'=. start,0       NB. cur node & step count
    firsti=.''               NB. keep node at ins start
    while. 1 do.
      if. 0=L|ct do.         NB. first ins, check for loop
        firsti=. firsti, cur
        if. (<:#firsti) > firsti i. cur do. 
          NB. note: repetition is always of el 1 of firsti
          break. end.        NB. loop is round, break!
      end.
      cur =.(cur{st){~ ins{~L|ct NB. take step
      if. cur e. id do.          NB. end for loop found
        ends=. ct start_index} ends NB. save time for end
      end.
      ct=.ct+1
    end.
    ll  =.ll start_index}~ct NB. store loop length
  end.
  NB. note: for length, work on -L, re-add later; ll contains initial startup of length #ins, so as experimented, loops start before end of first loop through ins, so discrding this length ensures we're in loop regime. 
  ends=. L-~ ends NB. discard exit nodes, not needed
  ll  =. L-~ ll
  NB. odd fact: ll -:&(-/~) ends
  NB. heuristic: start with all at pos 0, start adding 1{ends to all, figuring out how many times to do until the next cycle in the list also reaches the end. Repeat until 1 cycle remains. Note: order of x and y critical so steps added become larger, instead of loop lengths becoming larger!
  NB. comb takes end, len for both x and y
  NB. returns end, len for combination of both
  NB.           l0  +until e1=l1|]          e0  , lcm len's
  combcyc =: {{({:y)+^:(({.x)~:({:x)|])^:_ {.y}},*.&{:
  NB. fold version: 1+L({. ] F.. combcyc }.) ends,.ll
  1+L+{. combcyc/ ends,.ll
}}
0
}}
9 day {{ NB. Mirage maintenance
NB. Figure out polynomial: correct, but ill-posed, so requires x: and makes it go slow (2+ seconds per part :/)
pfit =: ] %.[: ^/~i.@# NB. adapted from JPhrases 9C
p1=: [: +/ (pfit p.   #)@:x:@:(0&".);._2
p2=: [: +/ (pfit p. _1:)@:x:@:(0&".);._2
NB. not working per line, fitting all together, barely has an effect on run time
0
}}
10 day {{ NB. Pipe Maze
NB. great another maze searcher
NB. for each index, find connected neighbours, with */@$ being fill.
NB.  self  R    D    L       U
neigh =:{{ NB. y: literal, pipe field
  sh4=. 0 0, 0 1, 1 0, 0 _1,: _1 0
  NB. connections only where fitting
  NB. learn connection rules by example: all possible connections:
ex=. ];._2 {{)n
.....................
.F-7.F7.--.|.L7.|.FJ.
.|.|.LJ....|..S-S-S..
.L-J...FS7...FJ.|.L7.
.......J.LSJ.........
}} NB. this is silly of J. with indent doesn't work...
  NB. corresponding neighbour patterns
  pat=.(('.'([,~-.~),){~ (i.!.0 sh4 +"1/])@($#:[:I.'.'~:,)) ex
  NB. connected syms turn per direction into pairs of self, neigh of connected occurences
  NB. note: number of unique and number of dots the same: no boxing
  NB. permute axes to have shift directions first to facilitate checking later.
  NB.     sort nub;rem having .      perm  self,. neigh 
  cs  =. ([: /:~@~.(#~ 0='.'&e."1))"_1]0 2|:({. ,."0 1 |:) pat
  NB. is connected checks whether neighbouring characters are neighbours; takes literal array: self, RDLU shifts
  isc =. cs&(e."(_1)~ 0 2|:{.,."0 1|:)
  lin =. '.'([,~-.~),y
  ind =. (<:@#lin),.~(i.!.0 sh4 +"1/])coord=.($#:[:I.'.'~:,) y
  lin;(lin (] (*+(<:#lin)*-.@]) isc@:{~) ind);coord
}}
NB. part 1: find max dist from S in loop
NB. start all at */@$f set S loc to 0, iterate: 1 + <./ {~ until no change
NB. tst
tst=:{{)n
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
}}
tst2=:{{)n
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
}}
p1=: {{ NB. a bit overkill, but at least ready for part 2 to be ' how many loops are there?
  'll nn'=. 2{.neigh y=. ];._2 y
  init =. 0 (ll i. 'S')}(#~)mm=:# ll
  >./ (#~ mm&>) nn ({.<.1+<./@}.)@:{^:_ init
}}
NB. surprise: entirely different part 2: what's the enclosed area?
NB. adapt s.t. loop becomes 1, then, for each row, figure out included square, subtract +/,loop. Need to use actual letters, because simple 0 1 won't work because of horizontal edges.
p2=: {{ 
  'll nn co'=. neigh y=. ];._2 y
  init =. 1 (ll i. 'S')}0(#~)mm=.# ll
  loop =. (}: nn +./@:{^:_ init) co} ($y)$0
  NB. infer automatically S which symbol to replace S with
  neighS=.I.'.'~:}.{.ll{~(|:nn)#~'S'=ll NB. RDLU neighbours of S
  repS =. 'F-J7|L'{~((#~ </"1) >,/{,~<i. 4) i. neighS NB. replacement
  NB. keep only loop and replace S with correct function
  y=. repS (<($#:1 i.~'S'=,)y)} (-.loop)}y,:'.'
  NB. state transitions Assume only loop ~: '.'
  NB.     0   1   2   3   4   5   6
  NB.     .   -   |   F   L   J   7  
  s=. ,: 0j0  0  3j1 1j1 2j1  0   0  NB. 0 outside
  s=. s,  0  1j0  0   0   0  3j0 6j0 NB. 1 edge up in
  s=. s, 0j0 2j0  0   0   0  6j0 3j0 NB. 2 edge down in
  s=. s, 3j0  0  6j0 4j0 5j0  0   0  NB. 3 edge in=inside
  s=. s,  0  4j0  0   0   0  6j0 3j0 NB. 4 edge up out
  s=. s,  0  5j0  0   0   0  3j0 6j0 NB. 5 edge down out
  s=. s, 0j3  0  3j0 1j0 2j0  0   0  NB. 6 edge out
  NB. s=. s, 7j0  0  6j0 4j0 5j0  0   0  NB. 7 inside is same as edge in; removed
  fsm=.2;+.s
  NB. code 2 returns start,len pairs of included squares
  NB. sum per row sums of lengths of words, and subtract edges
  (+/,loop) -~ +/ +/@:({:"1)@(fsm&;:)"1 '.-|FLJ7'i. y
}}
0
}}
11 day {{ NB. Cosmic Expansion
NB. part 1: expand empty cosmos rows and columns; then find the sum of the shortest L1 paths between each unordered pair of galaxies.
par =: '#'=];._2  NB. turn into binary matrix; y= io''
loc =: ($#:I.@,) NB. locations of galaxies, y=binary matrix
emp =: (|: <@(-.~ i.)"1 0 >./) NB. empty rows/cols, y=coordinates
exp =: ([: |: |: ([+([:+/>)&>) emp) NB. expand; y=coords
NB. distance triangle sum: y: coords
dts =: [: +/@,@(*</~@i.@#) +/"1@:|@:(-"1/~) NB. 10x faster than +/@:|@:-"1/~
p1  =: dts@exp@loc@par
NB. good guess for my part 1 implementation: now spaces are 1e6 times larger, instead of 2 times larger.
exp2 =: ([: |: |: ([+(1e6-1)*([:+/>)&>) emp) NB. expand; y=coords
p2   =: dts@exp2@loc@par
0
}}
12 day {{ NB. Hot Springs
NB. input '.#' for ok/broken springs and list of numbers of runs of broken springs. Fill in ? such that list works. First try: recurrent approach, filling in first ? and seeing whether list still matches; if not backtrack.
NB. keep .#? and turn list in nums
par =: ([: (('.'([,,~)])&.>)`(".&.>)"0 [: <;._1 ' ',]);._2
NB. check: 
NB. chk =: +/@:=&'#';.1~ 2 (~:*.'#'=])/\ '.', ]
NB. makes rec more than DOUBLE as fast
 chk =: ([: {:"1 (2;(+. 0j0 1j1,:0j3 1j0));:'.#'&i.) 
pchk =: chk@sel
NB. select til last . (because run of # can be incomplete) after selecting until before last ?
sel  =: ({.~ #|i:&'.')@({.~ i.&'?')
NB. peq: prefix equal?
peq  =: -:/@(<.&# {."1 ,:) 
rec  =: {{ NB. y=list, x=row of springs
NB. fq= first question mark.
  if. (#x)=fq=. x i. '?' do. NB. done; check entire list
    y-:chk x
  else. 
    ca=. '.#' fq}"0 1 x NB. candidates for next ?
    select.  y (peq pchk)"1 ca
    case. 0 0 do. 0
    case. 1 0 do. ({.ca) rec y
    case. 0 1 do. ({:ca) rec y
    case. 1 1 do. 
      ('.' fq} x) +&(rec&y) ('#' fq}x) NB. this is bruteforce.
    end.
  end.
}} M.
p1=:[: +/@:> rec&>/ t. 0"1@par
tst=: {{)n
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
}}
NB. part 2: spring rows are folded; repeat each row 5 times, interspersed with ?
unfold=: (}:@$~ 5*#)@(,&'?')&.>`(($~5*#)&.>)"0
NB. pp2=:[: +/@:> rec&>/@:unfold t. 0"1@par
NB. probably good, but toooooo slooooow, even with partial checks :(. TODO figure out smarter way.
parf =: ([: (('.'([,,~)])@:(}:@$~ 5*#)@(,&'?')&.>)`(($~5*#)@:".&.>)"0 [: <;._1 ' ',]);._2
NB. temp: generate templates for pattern (y)
temp =: ([: #:@i. 2^#) {"1&.|: ,:&'?' 
runs =: '.'([,,~) '#' #~ ] NB. run of y #
NB. x find y: find possible positions for x in y
find =: I.@(+./)@(temp@[ E."1 ])
NB. recurse per run, rather than single ?, x: row, y: list
me=: ]
rec2 =: {{
  NB. impossible due to too little damaged, or separating good springs
  r   =. runs {.y
  NB. all groups are accounted for in list, so first # must be after or in the candidate runs TODO: minimal efficiency increase: adapt find to look only up to # + len cadidate.
  fd  =. <: x i. '#' NB. first defectuous -1 (because '.'={.pat)
  pos =. fd (>: #]) r find x NB. find and ensure no missed groups
  me x;y
  me r
  me pos
  ct=. 0
  ny  =. }.y
  for_p. pos do.
    NB. discard up to candidate position+length, add starting dot
    me '   ',nx=. '.',x}.~p+#r
    NB. trailing ### logic likely borked.
    if. (0=#ny) do. NB. no groups left: done if no # remaining
      if. '#' -.@:e. nx do. ct=.ct+1 [me'   finished' end.
      me '   imp' continue.
    end.NB. done
    NB. not enough #'s for groups or . to separate.
    if. +:/((+/,>:@#)ny) <: ('.#',.'?') +/@e.~"1 nx do. NB. not added to ct [me'imp'
      me '   imp'
      continue. end. 
    NB. recurse
    ct=. ct + nx rec2 ny
  end.
  ct
}} 
pp2=:[: +/@:> rec2&>/ t. 0"1@parf NB. seems to be correct but still so slow it doesn't even finish the first of parf io''; likely need to exploit repetive nature of rows...
ppp=: {{
  pi=. (-x) ]\ parf y NB. batches of x
  ct=. 0
  for_i. pi do.
    echo 'batch ',(":>:i_index),' of ',(":#pi)
    ct=. ct + +/>rec2&>/t. 0"1 i
  end.
  ct
}}
NB. third try: fully fsm based: emit runs of #, up to first ?
NB. design choice: 
NB. -s0 emit only runs of ##; problems: no clue what happened if .?; (minor): runs fsm twice over ... before ?
NB. -s1 emit both runs of . and #: avoids above, but length counts slightly more complicated.
s0=: 0j0 1j1 2j0,0j3 1j0 2j3,:0j6 NB. ok,damaged and ? encountered
NB. bunch ? with 'any other'
s1=: ".;._2 {{)n
1j1 2j1 3j6 NB. start
1j0 2j2 3j3 NB. .
1j2 2j0 3j3 NB. #
3j6 3j6 3j6 NB. ? or other: end immediately
}}
NB. coded outputs:
NB. 4: . ended because of .#
NB. 6: # ended because of #.
NB. 5/8: ./# ended because of ? (or generally; any non .#)
NB. 3/7: ./# ended because of input end.
fsm=: 4;(+.s1);<;:'.#'
fsmdd=:'.',(1;(+. 0j0 1j1,2j0 1j0,:0j3 1j2);<;:'.')&;:
p2 =: {{
  p=. 0
  for_b. (-@(8<.#) <\ ])parf y do.
    NB. pass each worker a row and a locale for caching
    p=.p++/> ('d12wl',"_ 0 (#>b){.'012345678789') wl t. 0"1 >b
  end.
  coerase <"1('d12wl',"_ 0 (#>b){.'012345678789')
 p
}}
NB. after many different caching setups: per row caching is faaar faster, and parallelisable.
wl=:{{
  cocurrent x
  coinsert'd12'
  NB. set up hashmap for locale
  keys=: 0 20$0 NB. s:<''
  vals=: 0$0
  rec4"1 y
}}

NB. fsm ijrd state tossing too complicated for my brain. rec4 still uses fsm, but alters row string for filling in ?
NB. finishes before Universe heat death, but still slow (38s, better than it used to be)
rec4=:{{
NB. x: locale; y: row, list
  'r l'=.y
  NB. r=. fsmdd^:('..'+./@:E.]) r NB. remove redundant dots: cutr, but 1 second slower.
  NB. key=.s:<r,a.{~l NB. initial run far slower (400+ sec), subsequent once faster (30s). Likely because all strings saved. MD5 and SHA1 similar on my phone, CRC32 wrong, likely collisions.
  key=._1 (128!:6) r,a.{~l
  if. (#keys)>ind=.keys i. key do.
      vv=.ind{vals
  else.
NB. 4: . ended because of .# 4 or 6 are never last.
NB. 6: # ended because of #.
NB. 5/8: ./# ended because of ? (or generally; any non .#)
NB. 3/7: ./# ended because of input end.
    NB. run FSM, check exit codes
    code=. {:{: g=. fsm;:r
    nn=.#dam =.(1&{"1 #~ 6={:"1) g NB. complete damaged runs 
   NB. echo x;y;g;dam;nn
    if. code e. 5 8 do. NB. end due to ?
      fq=. +/}:{:g NB. next after first question mark is after last run
      newl=.nn}.l
      if.  (-. dam -:nn {. l)do. NB. prefix doesn't match, or spurious #'s after partial when y is empty
        vv =. 0
      elseif. (0=# newl)*.'#' e. r }.~ >:fq do.
        vv=.0 
      else. NB. keep incomplete part when code=8
        if. code=5 do.
          newr=.'.',.'.#',"0 1(>:fq)}.r NB. recurse
        else. 
          sp=. {.{:g
          newr=. '.',. sp}."1 '.#'fq}"0 1 r NB. recurse
        end.
        vv=. +/ rec4"1 newr;"1 _ newl NB. recurse
      end.
    else. NB. finished with code 3/7, check l-:runs
      vv=. l-:(1&{"1 #~ 6 7 e.~ {:"1) g NB.  end.
    end.
    vals=:vals, vv
    keys=:keys, key
  end.
vv
}}
0
}}
13 day {{ NB. Point of Incidence
NB. find reflection rows / cols
tst=:{{)n
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
}}
par=:[: (([:<];._1);._2~ LF2&E.) LF([,,~)]
NB. find reflection starting at top & bottom
NB. idea is that a reflection occurs wheb a pre- or postfix is a palindrome. }. and }: to remove the trivial length 1 palindrome
T=:        [: -:@(>./)@}. (#*]-:|.)\
B=:# ([|-) [: -:@(>./)@}: (#*]-:|.)\.
NB. find top or bottom reflection.
NB. solution too good, also catches odd reflections, leading to halve rows... hook filters these out. Uses given that there is a single line per field.
refl=:[:(*(=<.)) (] B@[^:(0=]) T) NB. @i.~: initially thought to work on selfie. forgot to add it eventually, which oddly worked. With selfie a fraction slower.
p1 =: [: +/@(refl@|:&> + 100* refl&>) par
NB. Part 2: there's one smudge on each mirror that creates a new reflection line. find it, and summarize only new lines
NB.  sum 2x multipliers of fixed reflection line
p2 =: +/^:2@(100 1*"1])@:(fix&>)@par
NB. can selects candidate rows for swapping
NB. ({. ... }.}\. to consider only rows not considered yet.
can=:(i.@# (,.+)&.> ([:<{.;@([: (<@I."1#~1=+/"1)~:"1) }.)\.)
NB. J905 only! tacit adv, swapping .# at m in y
swap=: (&{)('.#'&([{~-.@i.)&.)
NB. fix smudge, on mirror y; returns new reflection line
fix =: {{
  NB. candidates are the sole differences with any of the other rows
  candTB=. <   "1; can    y
  candLR=. <@|."1; can |: y
  orig  =. (refl , refl@|:) y
  for_c. candTB,candLR do.
    NB. cannot use refl for same, because it assumes only one reflection line present. Fixing the smudge could cause multiple reflection lines. this was the failure case of my code...
    nn =. orig ([:(#~]=<.)]-.0,[)&.> ((T,B);(T,B)@|:) c swap y 
    if. +./#&> nn do.
      {.@(1&{.)&> nn return. NB. assumes only 1 valid swap candidate
    end.
  end.
  echo 'none found' NB. shouldn't happen
}}
0
}}
14 day {{ NB. Parabolic reflector dish
NB. rolling rocks O, # fixed rocks tilting eolls them in a certain direction.
NB. part 1: calculate load of all rocks when tilted north, with rocks weighting 1 at the south edge, and #y at the north edge;
tst=:{{)n
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
}}
NB. padded par; pad to have an edge to roll to
ppar=: '#'([,.~[,.[,,~) ];._2
NB. idea: key by col; use I. to see where they end up
NB. returns fixed and rolling rocks in field y.
rocks=:'#O' <@($@]#:I.@(=,))"0 _ ]
NB. Tilt north operation: x: axis (U/D tilt=0), sense(0=D/R); y boxed rocks (expected sorted)
tilt =: {{
  'ax se'=. m     NB. axis and tilt sense
  oax    =. -. ax NB. cross fingers and hope p2 is 2D as well.
  NB. commwnts below relate to 0 0-:ax,se, butcode applies to all
  fb =. (oax&{"1 <   /. ax&{"1) x NB. box row nb per column
  NB. prefix fake points for ensuring all points accounted for
  rb =. (oax&{"1 <@}./. ax&{"1) (,.~i=.i.#fb),y
  if. se do. NB. up/left
    rbnew=. fb ([ ({~ +1+[:;<@i.@#/.~@]) <:@I. )&.> rb
  else.      NB. down/right
    rbnew=. fb ([ ({~ -1+[:;<@i.@#/.~@])    I. )&.> rb
  end.
  NB. return both fixed and rbnew; if ax=0: append col num
  ;i ,.&.>~`(,.&.>)@.ax rbnew
}}
vis  =: (('#O'#~#&>@])`(;@])`('.'$~1+>./@;@])}]) 
score=: [: +/ ({:@[ -&:({."1) ]) NB. no 1 corr because padding
p1=: [: ([ score 0 1 tilt)&>/ rocks@ppar 
NB. part 2: score after 1e9 cycles of ULDR
NB. likely cycle after a while
cyc=: [ 1 0 tilt [ 0 0 tilt [ 1 1 tilt 0 1 tilt
NB. enc=: [:s:@<a.{~, NB. encoding as sym not faster
p2=: {{
  'F R'=. rocks ppar y
  found=. 0$,:R NB. cycle detection
  new=.R
  while. (#found) = ind=. found i.!.0 new=. /:~ F cyc new do.
    found=. found, new
  end.
  NB. found loop, with length:
  len=. (#found)-ind
  rem=. <:len|1e9 - #found NB. <: because while has done next cyc 
  F score F cyc^:rem new
}}
NB. not the fastest; could reduce some work keeping F boxed per rows and cols but would make structure far less intuitive.
0
}}
15 day {{ NB. Lens Library
hash=: ] F.. (256|17*a.&i.@[+])
p1=: [: +/ [: 0&hash;._1 ',',}:
NB. part 2: initial lens configuration; for each step:
NB. - find box num by hash y
NB. - - >  remove lens y from box
NB. - =m put in new lens with focal length m, labeled y
NB. Looks like folding through list again. state=256boxes ; syms ; focal lengths
NB. note: name length not uniform: use syms
NB. parse instructions to: sym op: 0:- else =
NB. if not for bug in 0&". on '-', that could have been used instead of rplc...
par=: [: (s:@{. ; ".@>@{:)@;:;._1 ',',rplc&('-';' 0')@}:
NB. initial state from parsed input: boxes,syms,hashes, focal len
NB.   tbus assumes symbols to be unique, i.e. no two same labelled lenses with different focal length
init =: (256$a:); (;0&hash&>@(s:^:_1);0$~#)@~.@;@:({."1)
NB. min: do min operation in x on state y
min =: {{
  i=. x (2&{::@] {~ 1&{::@] i. >@{.@[) y
  NB. remove sym from box at i in box array (THANK YOU J905)
  -.&(>{.x)&.>&.(i&{)&.>&.(0&{) y
}}
NB. eq: set refraction for lens {.x and add to box if not present
eq =: {{
  i=.(2{::y){~l=.(1{::y)i.>{.x NB. l is ind in syms; i in boxes
  NB. add sym in box i in boxes   set FL@ l in focal lengths
  ~.@,&(>{.x)&.>&.(i&{)&.>&.(0&{) (>{:x)&(l})&.>&.(3&{)y
}}
op =: min`eq@.(*@>@{:@[) NB. if 0: min, else eq
NB. post: extract info from state
NB.     sum   box no   *   FL ind*slot no perbox sym i. labels 
post=:[:+/@;(1+i.256) *&.> ({:({~*1+i.@#@])L:0 (1&{ i.L:0 >@{.)) 
p2=: [: post@(init ] F.. op ]) par NB. faaar faster than post F..
0
}}
16 day {{ NB. The Floor Will Be Lava
NB. laser enters top left, find energised points. try recursive approach: each point returning ~.self,children
NB. recurse: gets y=direction,:point; x step uses globals FIELD&SEEN
tst=:{{)n
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
}}
rec=: {{
  NB. loop det: if current (coord, direction) seen, don't go again
  select. nt=.FIELD { ::'#'~ <np=.+/y NB. new type; new point
  case. '/' do.
    ny=.,: (-@|.{.y),:np     NB. refl /
  case. '\' do.
    ny=.,: (  |.{.y),:np     NB. refl \
  case. '-' do.
    ny=.((,:-)@|.`,:@.(0={.){.y),:"1 np
  case. '|' do.
    ny=.((,:-)@|.`,:@.(0={:){.y),:"1 np
  case. '.';'#' do. ny=.,:({.y),:np          NB. carry on
  end.
  for_nxt. ny do. NB. :: triggers if out of bounds
    if. (0+./@:>{:nxt) +. *SEEN {~ :: _ sc=.<((0 1,1 0,0 _1,:_1 0)&(i.!.0)@{. , {:) nxt do.
      continue.
    else.
      SEEN=: x sc} SEEN NB. record if not
      (>:x) rec nxt
    end.
  end.
  0
}}
step=: {{ NB. dir/loc of current, return array of next points
  NB. loop det: if current coord, direction seen, don't go again
  y=.2 2$y
  select. nt=.FIELD { ::'#'~ <np=.+/y NB. new type; new point
  case. '/' do.
    ny=.,: (-@|.{.y),np     NB. refl /
  case. '\' do.
    ny=.,: (  |.{.y),np     NB. refl \
  case. '-' do.
    ny=.((,:-)@|.`,:@.(0={.){.y),"1 np
  case. '|' do.
    ny=.((,:-)@|.`,:@.(0={:){.y),"1 np
  case. '.';'#' do.
    ny=.,:({.y),np          NB. carry on
  end.
  sel=.1#~#ny   NB. selected?
  scs=.0 3$0 NB. seen-coord-selected
  for_nxt. ny do. NB. :: triggers if out of bounds
    sel=. sel nxt_index}~ (0+./@:>_2{.nxt) +: *SEEN {~ :: 1 <sc=.((0 1,1 0,0 _1,:_1 0)&(i.!.0)@(2&{.) , _2&{.) nxt
    scs=. sc,~^:sel scs
  end.
  SEEN=: 1 (<scs)} SEEN NB. record if not
  sel#ny
}}
vis =: [: viewmat './\-|#'i.('#'"1@]`]`{{FIELD[x,y}} }])
shootrec =: {{
  FIELD=:];._2 y
  SEEN=: 0$~4,$FIELD
  1 rec x
  +/,+./*SEEN
}}
shoot =: {{
  FIELD=:];._2 y
  SEEN=: 0$~4,$FIELD
  curr=. ,:x
  while. # curr do.
    curr=. ;@:(<@step"_1) curr
  end.
  +/,+./*SEEN
}}
NB. p1 =: (0 1,:0 _1)&shootrec NB. stack error
p1 =: (0 1 0 _1)&shoot
NB. TODO recursion fails if called by day (borderline stack limit?), but not when called directly...
NB. part 2: max energised squares for each possible edge entrance
NB. adapt shoot to keep track of # squares following, then we can reuse SEEN and the results in FOL stiring for each pos and direction the number of following active squares. shoot should also store the ancestor tree, to be referred in curr, so that when current ends, step can trace back to the point where no outstandimg siblings are present. Could backfire, since loops broken by using SEEN wiuld not have been entered at the same point between different starting positions
p2 =: {{
  'R C'=.$];._2 y
  NB. all edge entrances, counterclockwise
  edges =:,/(0 1,_1 0,0 _1,:1 0) ,"1 (_1,.~i.R),(R,.i.C),(C,.~i.-R),:(_1,.i.-C)
  best=.0
  wsn=. 'd16w',"_ 0 '0123456789'
  for_i. (-nt=.1 T. '') <\ edges do.
    results=.''
    NB. echo'batch ',":i_index
    for_t. >i do.
      cocurrent wsn{~t_index
      coinsert'd16'
      results=.results, t shoot t.0 y NB. 3x faster with t.
    end.
    NB. echo results,.&:>i
    best=.best >. >./>results
  end.
  <:best NB. TODO: debug off-by-one error, likely boundary cond in shoot at setting of sel
}}
0
}}
17 day {{ NB. Clumsy Crucible
NB. minimize heatloss, go max 3 steps straight, and no 180's. each square indicates heat loss incurred when entered. As allowable states depend on previous choices, use recursion, not dijkstra etc.   
NB. encode as rows: neighbours,num for each point in index 
NB.     val apd   list   step -win   D R U L by win #el pad sides  inds of shape num mat
par=: [: (, ,.~ [: ,/ (1 1,:3 3) (7 5 1 3{,);._3 (*/([,[,~[,.[,.~i.@])])@$) "."0;._2

traverse =:{{ NB. adapted to abort branche when seen already
  nn=: par y  NB. Neighbour info
  seen=: 1e9$~ (4,n) ,~*/@$];._2 y NB. keeps lowest loss for each ind,prev dir, count tripple.
  NB. for homogeneity of coding below, hand code first two states. this s.t. valid direction can be used.
  NB. current states: point,prev dir,times dir,acc
  NB. note that 0 encodes 1 in the # steps
  'i d c a' =: |: (2{.{.nn),.0 1,.0,.({:"1 {~ 2&{.@{.)nn
  NB. 'i d c a' =. ,. 0 0 _1 0 NB. _1 to count dirs right
  ct=.0
  last=.<:na=:{.{:nn NB. value indicating non-applicable
  cans=. }:"1 nn
  vals=. {:"1 nn
  while. *#i do.
    can =. i{cans NB. Candidate indices
    NB. original wording for p1:
    NB.    non-neigh       180 deg and long runs
    NB. fil =. (na~:can) *.((c~:2),.0) (<(i.#i),"0(d,._2+d))} 1$~$can
    NB. general wording: rotate conditions s.t. 0 ends upeat cur. direction:
    fil =. (na~:can) *.(-d)|."0 1 u c 
    NB. compose new versions
    i     =. fil #&, can
    a     =. (i{vals) + (pc =. +/"1 fil) # a
    'd c' =. (4|I.,fil) ([;(pc#c)(]*+)=) (pc#d)
    srta  =. a{~ srt =: /: a
    NB. filter idca where seen not better. sorted so smallest loss kept amongst each i-d-c triple.
    fil2     =. (~:!.0 *. srta < seen {~ <) srtidc=. srt{i,.d,.c
    'i d c a'=. fil2&#"1 srta,~|: srtidc
    seen     =: a (<i,.d,.c)} seen NB. remaining are necessarily better due to previous filtering, so store in seen.
    ct=. ct+1
  end.
  <./,last {seen
}}
p1=: (1 0 1,~"_ 0 <&2)    traverse 3
p2=: (>&2(],.[,.0,.[)<&9) traverse 10
tst=:{{)n
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
}}
vis=: {{ viewmat 1e9 (~: (*+-.@[*[:>./ #&,) ]) <./"1],/"2]141 141 $ seen }}
0
}}
18 day {{ NB. Lavaduct Lagoon
NB. input is direction, length, and color (ignore for part 1). what'sthe circumscribed surface
dirs=: 4 2$1 0 0 1 _1 0 0 _1 NB. DRUL
par=: ([: ; ((dirs,@:{~'DRUL'&i.)&.>)`(".&.>)"0)@(0 1&{)@;:;._2
area=: {{
  edge=.0 0 ] F:. (]+(}:*{:)@[) y
  NB. jphrases 9F.m29: edges not counted, so returns 42 < 62 on test. Need to classify edge: straight units loose half a unit, outside corners 3/4 and inside corners 1/4. i.e. area++/9 5 24 * 3 1 2%4
  NB. outer  inner  straight; . outside; # inside; -+ grid
  NB. ..|..  ..|##  ..|..
  NB. ..|..  ..|##  ..|..
  NB. --+--  --+--  --+--
  NB. ..|##  ##|##  ##|##
  NB. ..|##  ##|##  ##|##
  NB. area sign indicates sense: + ccw, - cw
  sar=. ([: -: [: +/ 2: -/ .*\ ]) edge 
  NB. area sum 1/4 inner(1) or outer(3) corner    + 1/2 straight
  NB. could depend on area sign for corner in/out though.
  (|sar)+(+/1r4*4|(- _1&|.)dirs i. 2{."1 y)+1r2*(+/-#)2{"1 y
}}
p1=: area@par
NB. oops, have to decode hex instead
NB.        dfh  5 first, dirs   DRUL  last  disc   til first num 
par2=: ([: (x:@dfh@(5&{.),~[:x:dirs{~'1032'i.{:) (}:@}.~ 2+i.&'(')) ;._2
p2=: area@par2
tst=:{{)n
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
}}
0
}}
19 day {{ NB. Aplenty
NB. convert name{cond:dest,dest} to verbs
conv=: {{
NB. toupper needed for avoiding stomping over (i.e. occluding) J's stdlib
  'nm bod'=: ( ({.~ ,&< ((','<;._1@,])@}.~>:)) i.&'{') }: toupper rplc&('A';'XXXACC';'R';'XXXREJ') y
  NB. check special case: no conditions: no need to wrap non-cond in else. ... end.
  NB. replace xXmM because of their special j meaning.
  bod=. ([: <;._1 ':'&,)&> bod NB. split cond and consequence
  ifs=.,(((<'if. '){.!.(<' elseif. ')~#),.{."1,.(<' do. '),.(<' y '),.~{:"1)}:bod
  noncon=. (' y',~])&>`(' end. ',~ ' else. ',' y ',~>)@.(1<#bod) {.{: bod
  funt =: '''X M A S''=.y',LF,(,_5 (LF,~;)\ ifs),noncon
  (nm)=:3 : funt NB. global on purpose.
  0 0$0
}}
par =: [: (<@:(<;._2@,&LF) ;._2~ LF2&E. ) LF ,~]
NB. p1: init; parse
p1=:{{
  NB. R A implementations
  XXXREJ=:{{ 0 0$0[reject=: reject,y}}
  XXXACC=:{{ 0 0$0[accept=: accept,y}}
  accept=: reject=:0 4$0
  'fun val'=:par y
  conv&> fun
  vals =: ".@(#~ e.&',0123456789')&> }. val NB. }. discards empty first box introduced by parsing
  IN"1 vals NB. in defined by conv
  +/,accept
}}
NB. great. part 2 doesn't jive with part 1's setup... back to the drafting board!
NB. how many combinations would be valid when each attribute e. >:i.4000?
NB. recursive approach working on intervals
p2 =:{{
  NB. R A implementations
  fun=:0{::par y
  'sym con dest'=: |: conv2&> fun NB. set globals for lookup
  (s:<'in') rec 4 2$1 4000 NB. recurse on in with full range
}}
conv2=: {{
  NB. split workflow name and body. }: discards last }
  'nm bod'=: ( ({.~ ,&< ((','<;._1@,])@}.~>:)) i.&'{') }: y 
  bod=. ([: <;._1 ':'&,)&> bod NB. split cond and destination
  NB. convert conditions to matrix
  con=. (('xmas' i.{.),('<>'i.1{]),(2".@}.]))&> {."1}:bod
  NB. make symbols of destinations
  dest=. s: ({.@{: ,~ {:"1@}:) bod 
  (s:<nm) ; (<con) ; <<dest
}}
rec=:{{
  NB. break if leaf node(A,R) return size of current interval
  if. 2>ret=.(s:' R A') i. x do. ret**/1+-~/"1 y return. end.
  +/ (sym i. x) ((dest{::~[) rec"_1 (con{::~[) applywf ]) y
}}
NB. returns split ranges x: workflow (con); y: coord range
applywf =: ] F.. (}:@],[ applyrule {:@])~ ,:

NB. x rule, in which split point is last, y range
ltrg=:({:@[ (({.@] ,  <:@[),:(, {:)) 0&{@[ {])
gtrg=:({:@[ (({:@] ,~ >:@[),:(,~{.)) 0&{@[ {])
NB. x rule, y range; returns 2 ranges
applyrule =:{{
  rgs=.x ltrg`gtrg@.(1&{@[) y
  rgs ({.x)}"1 _ y 
}}
tst=:{{)n
px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
}}
0
}}
20 day {{ NB. Pulse Propagation
NB. 1000 button presses send low pulses;
NB. % flips when L, sends new state, H ignored, no pulse
NB. & remembers prev state per input, when pulse arrives, updates and if *./mem send L, else H.
par =: {{ NB. parse input to op;indices of incoming;ind of out
  srty=. \:~];._2 y
  NB. note, not all syms have outgoing connections, they also have no type. }. so same syms as next line
  symstot =. ~.s:<;._1;<@('-,'rplc~',',-.&(' >')@}.)"1 srty
  'kind syms'=. <@:;`(<@s:)"1|:([:(}.;~'%&b'i.{.)' ->'&taketo)"1 srty
  symdiff =. symstot-.syms
  kind    =. kind,3#~nd=.#symdiff
  syms    =. syms,symdiff
  NB. out has 58 which should not happen
  out =. (nd#a:),~([:<@(syms i. s:) [:<;._1',',' '-.~'-> '&takeafter)"1 srty
  in  =. (<@({:"1 #~ ~:/"1)/.~ {."1) (/:{."1);((,~@],,.)&.> i.@#) out
  kind ; syms ; in ,&< out
}}
init =: {{ NB. set globals and initialise memories
  'KIND SYMS IN OUT'=:par y
  MEM =: 0$~,~#KIND
}}
NB. pulses represented by Dest Source HL tripple
NB. flip flop: y=pulse,ind uses globals MEM IN OUT
ff=:{{
  'd s p'=.y NB. dest source pulse (source not used)
  if. -.p do.
    MEM=: -.&.((<d,0)&{) MEM NB. flip mem
    OUT ((d,.~[),.{.@])&(d&{::) MEM NB. return new pulse
  else.
    0 3$0
  end.
}}
conj=:{{
  'd s p'=.y
  MEM=: p (<d,s)} MEM NB. update memory d at pos s
  val=. -.*./MEM{~(<d;d{IN)
  (d,.~d{::OUT),.val  NB. return pulse
}}
nop=: (0 3$0)"_
p1 =: {{
  init y
  bc =: ((0{::OUT),"0 _] 0 0)"1 NB. defined here so no recalc of out.
  pp =: (ff`conj`bc`nop)@.(KIND{~{.) NB. process pulse
  ct =. 0 0 NB. counts for H/L pulses
  for. i. 1000 do. NB. limit to 1000 for part 1 only
    pulses=. ,:0 1000 0
    ct=. ct+1 0 NB. 1 for initial button L 
    while. #pulses do.
      pulses=. ;@:(<@pp"1) pulses
      NB. pulses=: ;@:(pp t.0"1) pulses
      ct=. ct+ +/(,.~ -.){:"1 pulses
    end.
  end.
  */ct
}}
NB. Adapting p1 to solve problem 2 doesn't find pulse before 1e6 presses
NB. Second approach, after inspection of graph: check high inputs to last conjunction, and hope they immediately go into a cycle.
p2 =: {{
  init y
  bc =: ((0{::OUT),"0 _] 0 0)"1 NB. so no recalc of out.
  pp =: (ff`conj`bc`nop)@.(KIND{~{.) NB. process pulse
  rx=.(SYMS i. s:<'rx') NB. special return node (should be last)
  it=.0
  NB. last node before rx is &7 having parents 1 3 4 5
  NB. assume similar setup for other inputs
  concmemi =. <,{&IN&>^:(1 2) <rx     NB. memory indexes for conc
  conc     =. {.0{::>concmemi         NB. concentrator index
  found    =. a: $~ {: #&> > concmemi NB. found high pulses 
  NB. find lengths of cycles giving 1 Low (assume that it hits a loop immediately)
  while. (2><./#&>found) do. NB. until 2 low found for all
    pulses=. ,:0 1000 0    NB. new button press (dest,src,level)
    while. #pulses do.
      t=. conc e. {."1 pulses NB. test if conc receives pulses
      pulses=. ;@:(<@pp"1) pulses
      NB. pulses=: ;@:(pp t.0"1) pulses NB. parallel version
      if. t do. NB. conc was target
        hit   =. I. {. concmemi {MEM
        found =. ([: ~. ,&it)&.>&.(hit&{) found
      end.
    end.
    it=.it+1
  end.
  startlen =. (-~/\)"1 > found 
  NB. from day 9; TODO slow, fix. modular primitives?
  combcyc =. {{({:y)+^:(({.x)~:({:x)|])^:_ {.y}},*.&{:
  1+{. combcyc/ startlen
}}
tst=:{{)n
broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
}}
0
}}
21 day {{ NB. Step Counter
NB. starting from S how many squares can be reached in 64 steps
NB. coordinated of garden plots, starting with start (0)
par =: [: ; 'S.' ([:<$@] #:[I.@:=,@])"0 _ ];._2
sh4 =: <. +. 0j1^i.4  NB. shift in 4 cardinal directions
NB. v neighbours with extra non-neighbour row
neigh =: [: (],>./@,)@|: ] i.!.0 sh4 +"1/ ]
p1 =: #@(0 ({:@{:@[ -.~ [: ~.@, {~)^:64~ neigh)@par
NB. part 2: plots repeat infinitely and not 64 but 26501365 steps
NB. - avoid flipflopping neighbours already seen
NB. - use same approach, but add edge connections, and keep plot offsets
NB. return plot shape (head) and coordinates as par above (}.)
par2 =: [: ($,[: ; 'S.' ([:<$@] #:[I.@:=,@])"0 _]) ];._2
NB. takes output of par2 and return neighbours wrapping around
NB.          apd non-n tr co index sh mod shifted coords 
neigh2 =: [: (],>./@,)@|: }. i.!.0 {. |"1 sh4 +"1/ }.
NB. interesting, but too slow (minutes for 2000 iterations). Needs to exploit better plot multiplicity.
pp2_too_slow =: {{
  nn  =. neigh par y NB. non-wrap neighbours
  NB. wrapping neighbours with wrapping directions
  nn2 =. neigh2 par2 y
  wo  =. ( (dirs=.<.+.0j1^i.4)*"_ 1 nn~:]) nn2 NB. wrap offsets
  nonn=. {:{:nn2                         NB. non-neigh index
  cur=. ,:0 0 0    NB. currently reachable gardens
  seo =. cur;0 3$0 NB. seen ind in odd/even iterations
  cts =. 0 0 
  ret =: (<,0)
  for_i. >: i. x do.
    NB. next indices and offsets TODO joint nn2/wi faster?
    'nexti nexto'=. ([:,/ ({."1 cur)&{)&.> nn2;wo
    cur =. nexti ,. nexto =. nexto + 4#}."1 cur
    NB. remove non-neigh and seen i
    NB. TODO ditch done offset, i.e. not having neighbours in cur
    NB.     non-neigh not-or(non-unique or seen)
    msk =. (nexti=nonn) +: cur (-.@~:@[ +. e.!.0) seo{::~2|i
    cur =. msk # cur
    NB. add to seen i
    seo =. ,&cur&.>&.((2|i)&{) seo
    if. 0 [1 e.~ 100|i do.
      echo 'cull: ',":echo i, #&> seo
      NB. TODO: messes up counts
      msk=.  e.&(~.,/dirs+"1/}."1 cur)@:(}."1) (sel=.-.2|i){:: seo
      cts=. (+&(+/-.msk))&.(sel&{) cts
      seo=. (msk&#)&.>&.(sel&{) seo
      echo #&> seo
    end.
    ret=:ret,(2|i){seo
  end.
  ({&cts+#@:{::&seo)2|x
}}
vis=: (('.#S'i.];._2 io '') 3:`]`[} {)
NB. recursive approach with global checklist
p2_rec=: {{
  nn   =: neigh2 par2 y
  nnw=: nn ,"0 1 (<.+.0j1^i.4) *"_ 1 nn ~: neigh  par  y NB. nn+wrap indicators
  nonn=: {:{:nn  NB. non-neigh index
  seen=: (2|x)|.1 3 ;&($&0) 0 3 NB. GLOBAL seen in even&odd steps
  x rec 0 0 0
  NB. pick right one from seen
  #0 {::seen
}}
rec=: {{
  if. x=0 do. return. 1 end. NB. ran out of steps
  new=. (seen{::~nxt=.-.2|x)-.!.0~(#~ nonn~:{."1)(0,}.y)+"1({.y){nnw
  seen=: (,&new)&.>&.(nxt&{) seen
  (<:x) rec"1 new
}}
NB. p2=: 26501365&pp2
tst=:{{)n
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
}}
0
}}
echo run 21
NB. vim: ts=2 sw=2 et fdm=marker foldmarker={{,}}
