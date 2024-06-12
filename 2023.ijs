NB. AoC, 2023
{{
if. -. fexist '~addons/general/jaoc' do.
  install'github:jpjacobs/general_jaoc'
  echo 'installed general/jaoc; go set up COOKIE.txt'
end.
}}''
load'general/jaoc'
NB. setup this year 
setup_aoc_ 2023
boxdraw_j_ 1
NB. Notes:
NB.  - This file uses jaoc for download/upload/day organisation
NB.  - Each part of each day solves the entire problem, from the problem text up.
NB.  - Recommended settings for vim: sw=2 ts=2 et fdm=marker foldmarker={{,}}
NB. TODO jaoc:
NB. - if split off utility library next year:
NB.   * connected component analysis (day 3,4)
NB.   * interval arrithmetics (split, merge, ... day 5)
NB. TODO: revision WIP: done till day 19
NB. TODO: rewrite day 12 to use numbered locales instead; try using locales as cache again, with enourmous hashtable sizes (left arg 11 to 
NB. TODO: add expected results to test inputs.
NB. TODO: report/enquire bugs days
NB.    4: j9.6 Beta10 fails, likely in fold, while no problem for j9.5
NB.   12
NB.   15: ([: post ] F.. ) faster than (post F.. ); bug in 0".'-' == _ instead of 0
NB.   16: Making SEEN sparse makes method fail

NB. spin up threads up to # of cores 
0&T.@0^:(0>._1+([: {. 8&T.)-1&T.) '' 
NB. Daily solutions:
1 day {{ NB. Trebuchet?!
p1=: [: +/ ".@({.,{:)@:(#~e.&Num_j_);._2
NB. easy. but simple replace doesn't work, since it replaces in the order of the replacement values, rather than occurrance in th  string
num=: ;:'zero one two three four five six seven eight nine'
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
NB. parsing most difficult was most difficult for this day. 
NB. 100 games, seq numbered 1-100; parse to box per game, with each game having boxed turns
par =: <@:(_2 ]\&.> [: ". }.&.;:@:rplc&', ');._2@:r1
r1  =:rplc&(('Game';''),;:'red 0 green 1 blue 2') NB. remove Game, replace colors by nums
NB. neg limitations for summing, see where neg
lim =: _2]\_12 0 _13 1 _14 2
NB. possible: sum of head keyed by tail <:0 each game
pos =: ([: *./ ([: ([:*./0>:{:"1 +//. {."1) lim,])&>)&>
p1  =: +/@:>:@I.@:pos@par
NB. power:*/ min number of blocks required per game; assumes no duplicate colours per turn.
pow =: ([: */@({:"1 >.//. {."1) ;)&>
p2  =: +/@:pow@par
0
}}
3 day {{ NB. Gear Ratios
NB. Analysis of data: 0 not present; 1 is present; each island max 1 symbol
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
NB. pre finds connected components (9-connected) and sets globals:
NB. LMAT (linearised matrix), LIND (linearised indices of non-dots) and BR (V) boxes runs of same type (i.e., numbers).
pre =: {{
  LMAT  =: , mat  =. '.',.~];._2 y                NB. (linearized) matrix, extra . col for breaking numbers on edges  
  ind   =. ($mat)#: LIND=:I.@, (e.'.'-.~~.@,) mat NB. non-dots 
  sh9   =. _1+3 3#:i.9                            NB. 9 neighbourhood shifts (noun)
  neigh =. [: |:@(,#) sh9 (]i.+"1/~) ]            NB. neighbours, takes ind (verb)
  NB. takes max for connected components; _1 for not found; discard not found afterwards
  cc    =. }: (neigh (>./@:{)^:_ (_1,~i.@#)) ind  NB. connected components, takes start from non-dots ind; drop last, not-found node index
  NB. box runs (verb) of same type and consecutive indices
  NB. frets for: different type          or 1~: step (i.e. not adjacent(
  BR=: (<;.1~1,(2 ~:/\ Num_j_ e.~ {&LMAT)+.1~:2-~/\])
  cc NB. return connected components.
}}
  NB. visualization in the above: viewmat (}:cc) ind} ($mat)$0
NB. per cc: lookup, convert to words then to num; symbols will generate empty boxes; 
parts=:{{  NB. y=cc, global LIND & LMAT
  NB. Per cc: sum*has sym  ; do after lookup after boxing runs of inds & types (note that ".'$' produces empty) result is sum of numbers or 0 if no symbol.
  y (+/@;*a:&e.)@:(".@:{&LMAT&.>)@BR/. LIND
}}
NB. P1: find sum of part numbers
p1=: +/@parts@pre
gearratios =: {{ NB. y=cc, global LIND & LMAT
NB. find gears, having 2 numbers and 1 *
  NB. per cc: box after lookup per run after boxing runs
  islands=. y <@:({&LMAT&.>)@BR/. LIND
  NB.    keep if has *  and len=3
  gears=. (#~ ('*'e.;)&> *. 3=#&>) islands
  NB. Get * between num by sorting (TAO of J)
  ([:".@; _1|./:~)&> gears
}}
NB. sum of gear ratios
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
NB. Part 1: card score is 2^<:#co-occuring numbers 
NB. parse to list of #co-occuring numbers
par=: ([: +/@:e.&>/ [: <@".;._1'|',9&}.);._2
p1 =: +/@(* * 2^<:)@par NB. * * because 0's should remain 0, not 1 (=2^0)
NB. Part 2 uses Fold multiple fwd, start with extra 1 in state to keep initial card in count. {. keeps current card count for each card. Win reduces #state at every iteration to keep track of where copies*ones should be added.
p2 =: +/@:({. F:. win~ 1$~1+#)@par
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
5 day {{ NB. If You Give A Seed A Fertilizer
pseed=: [: ". 7 }. {.                                  NB. parse seeds
pmap =: [: (([: <@|: "."1);._1~ +./@:e.&Alpha_j_"1) }. NB. parse map
NB. extract seeds and maps; removing empty lines
par  =: [: (pseed ,&< pmap) ];._2@(#~ LF2 -.@E.])
tose=: ([,_1++)/                                       NB. start,len to start,end
NB. verified in data: no overlapping ranges
NB. put matrix in 3 rows: diff,start source, end source
dse =: ({.-1&{),1&{ ([,:_1++) {:
NB. map for use with F.. x: dse matrix; y values to be mapped
map=: ]+(0,~{.@[) {~ (({:@[>:])i.&1@:*.1&{@[<:])"_ 0
NB. find smallest value in orig seeds
NB.    seeds    min keep orig;seeds mapped by all converted maps
p1 =: (0&{:: ([: <./@([#~e.~) ] F.. ((map~>)~)) [: dse&.> 1&{::)@par
NB. Part 2: seeds indicate ranges, find lowest seed in original RANGES.
NB. This sucks: in part 2 seeds are ranges too large to bruteforce. Need to use assumptions checked before: source ranges *and* dest ranges don't overlap themselves.
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
NB. Standardise range: convert to diff/start/end; sort by start/end 
stdrg =: (/:}."1)@|:@dse&.>
NB. source ranges and backward ranges are:
NB. - almost all adjacent, complete on 0-big
NB. - non overlapping
NB. so work backward, recursively for each range
NB. inverse ranges, so reverse mapping
invrg=: [: |. cr@(/: 1&{"1)@(-@:({."1),.({.+}.)"1)&.>
p2=:{{ NB. x seeds;y boxed dse mats
  SEEDS =: /:~_2 ({.,_1++/)\ x
  NB. lowest location seed, starting from entire range.
  ls=. (0,big) rec invrg stdrg y
  NB. forward transform this seed ls to location; as before for p1
  ls ([: <./@([#~e.~)] F.. ((map~>)~)) dse&.> y
}}&>/@par
NB. recursion verb: y: ranges from last to first; x: probe range
NB. since lowest ranges in output domain are always tried first, first match is guaranteed to be smallest value corresponding to a seed.
rec =: {{
  if. -.*#y do. NB. no more layers
    ovl=.(#~ {."1<:{:"1) SEEDS (>.&{.,<.&{:)"1 x NB. find overlap
    {.@{.^:(*@#) ovl return. NB. min overlap of seeds & x=lower of first
  end.
  NB. dive deeper: split probe x by {. x & transform into new ranges.
  NB. assumes complete range and values between 0 and (2^32)-1
  NB.   keep valid    prb diff+ overlapping range      ranges
  new =.(#~ {."1<:{:"1)x({.@]+"1((>.&{.,:<.&{:)}.))&.|: >{.y
  NB. sequential approach important! explore depth first, on the low side
  for_nr. new do. NB. for each new range
    if. #r=. nr rec }.y do. r return. end.
  end.
  0 2$0 NB. ran out of ranges, return empty
}}
big=: <:32(33 b.)1 NB. pseudo-inf
NB. create new array of ranges, *including the non-range zones*
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
0
}}
6 day {{ NB. Wait for it
par=: ".@(10&}.);._2         NB. parse; dropping 10 characters
p1 =: [: */ ir@poly"1@|:@par 
NB. convert to polynomial x^2-T.x+d = 0 to be solved
poly =: 1 ,~ (,-)~/
eps=: (,-)_1e_10 NB. needed because draft doesn't count
NB.      1+ round towards center
ir =: [: (1+<.@{.->.@{:) eps+1{::p. NB. integer roots
NB. the same, but remove spaces from inputs
p2 =: [: */ ir@poly"1@|:@(([:".' '-.~10}.]);._2)
0
}}
7 day {{ NB. Camel Cards
cards1=:'23456789TJQKA'
NB. sort once on cards; J's sort (later by type) is stable.
NB. par was adapted to take card symbols on the left to work with part 2.
NB.     sort by cards  bid          ,.~  card no's
par=:[: (/: }:"1) [ (([:".6&}."1@]) ,.~ [ i. 5{."1]) ];._2@]
NB. return index of hand, highest first
NB. highest:  H,pair, 2 pairs       3oak,full house,   4oak,5oak card counts
hand1=:[:i:&1@(1,2&e.,(2=#@(#~ 2&=)),3&e.,(3 2 *./@e.]),4&e.,5&e.)#/.~
NB.          bid  times 1+rank    hand  cards
p1 =: cards1 ({:"1 +/ .* 1+[:/:@/: hand1@:}:"1)@par ]
NB. part 2: jokers can be any card, yet are lowest in order.
cards2=: 'J' ([, -.~) cards1  NB. modified card order
NB. reimplement hand to take into account jokers
p2 =: cards2 ({:"1 +/ .* 1+[:/:@/: hand2@:}:"1)@par ]
NB. note: pairs, double pairs, 3oak, 4oak with only J is always less than using them as joker. 5 J's could make sense.
hand2 =:nj i:&1@(1,pair,two,three,full,four,five) [: #/.~ 0 -.~]
NB. verbs below take: x: number of jokers; y card counts without jokers
nj=: 0 +/@:=]            NB. number of jokers in a hand
pair =: *@[ +. 2&e.@]    NB. is pair
two  =: 2<:(+#@(#~2&=))  NB. is two pairs
three=: [:+./3<:+        NB. is three
full =: 2=#@]            NB. is full hous
four =: [:+./4<:+        NB. is four of a kind
five =: (5=[)+.[:+./5<:+ NB. is five of a kind
0 
}}
8 day {{ NB. Haunted Wasteland
NB. Parse: returns 0 1 instructions (LR); boxed node labels, sorted AAA-ZZZ; self-L-R
par=: [:(('LR'i.{.),&<(/:{."1)@:;:@:(-.&'=(),'"1)@(2&}.))];._2
p1=:{{
  'ins st'=. par y      NB. parse input
  st=.({."1 i. }."1) st NB. convert to integers
  'cur ct'=. 0          NB. initialise starting node, step count
  while. cur~:<:#st do. NB. do steps per one until ZZZ (<:#st) reached
    ct=.ct+1[cur=.(cur{st){~ ins{~ct|~#ins
  end.
}}
NB. That's wild... start at all nodes ending in A until all end up in node ending in Z. (both 6 occurrences)
NB. Likely too much to simulate together. Find per starting point looplength
NB. Initial approach:
pp2 =: {{
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
  ends=. L-~ ends
  ll  =. L-~ ll
  NB. echo ,.&.> ends;ll;L
  NB. odd fact: ll -:&(-/~) ends
  NB. heuristic: start with all at pos 0, start adding 1{ends to all, figuring out how many times to do until the next cycle in the list also reaches the end. Repeat until 1 cycle remains. Note: order of x and y critical so steps added become larger, instead of loop lengths becoming larger!
  NB. comb takes end, len for both x and y
  NB. returns end, len for combination of both
  NB.           l0  +until e1=l1|]          e0  , lcm len's
  combcyc =: {{({:y)+^:(({.x)~:({:x)|])^:_ {.y}},*.&{:
  
  NB. fold version: 1+L({. ] F.. combcyc }.) ends,.ll
  1+L+{. combcyc/ ends,.ll
}}
p2 =: {{ 
NB. changes w.r.t previous version:
NB. - replace combcyc by simple *./L,ll apparently enough (verified with ll's of previous correct version)
NB. - remove ends as not needed 
NB. - step by entire ins length, instead of 1: a lot simpler, and checks don't need conditionals (7.5x faster)
NB. - check loop length for all starts at once: funnily enough a little slower than doing the same for each start seperately, but too lazy to revert
  'ins st'=. par y               NB. instructions & states
  'is id'=.<@I.'AZ'=/{:&>{."1 st NB. ind of source/dest
  st=.({."1 i. }."1) st          NB. convert states to ints
  ll=. 0$0                       NB. cycle lengths for starts
  NB. assumes one end in each cycle, verified.
  L=. #ins=.|. ins               NB. instruction cycle length
  ct=. 0       NB. cur node & step count
  firsti=. 6 0$0                 NB. keep node at ins start
  while. #is do.                 NB. reuse is as current state
    firsti=. firsti,.is          NB. keep is at each iteration, for checking whether we're back
    if. +./ msk=.firsti(<:@:#@:[ > i.)"1 0 is do. NB. found a loop in one of the active is's
      NB. remove masked cycles from firsti & is
      firsti =. (nm=.-.msk)#firsti
      is     =. (nm)#is
      ll     =. ll,ct#~+/msk     NB. store loop length
    end.
    is =. is ({ {&st)/@,~"0 1 ins NB. take L steps (space!)
    NB. is =. is ] F.. ({ {&st)"0 1 |. ins  NB. fold waaay slower than / above
    ct =. ct+L
  end.
  ll  =. L-~ ll NB. first round is no cycle
  *./L, ll      NB. all cycles and instructions should have wrapped
}}
0
}}
9 day {{ NB. Mirage Maintenance
NB. First attempt: Figure out polynomial: Neat, concise and correct, but ill-posed, so requires x: and makes it go slow (2+ seconds per part :/)
pfit =: ] %.[: ^/~i.@# NB. adapted from JPhrases 9C
pp1  =: [: +/ (pfit p.   #)@:x:@:(0&".);._2
pp2  =: [: +/ (pfit p. _1:)@:x:@:(0&".);._2
NB. Fitting all polynomials together, rather than one at a time barely has an effect on run time
NB. Recursive reimplementation below is 1000x faster (litterally)
NB. x: kept ends of previous layer; y: current layer
NB.      prev +[apd x $: delta] or last if only 0's
sol1 =: ({:@[ + (,{:) $: 2-~/\])`({:@[)@.((,0)-:~.@])
NB.           x    x  (and - in p2)  <=== differences
sol2 =: ({:@[ - (,{.) $: 2-~/\])`({:@[)@.((,0)-:~.@])
NB. adv: $: refers to largest tacit; split surrounding off, and allow reuse for part 2
NB.    init  numbers   lines  sum
mir =: (0&) (@:(0&".)) (;._2)(+/@:)
NB.    +/@:(((0&)]:)@:(0&".);._2) NB. alternative mir as modifier train
p1=: sol1 mir
NB. part 2: - required because of 0-result at the end due to choices
p2=: -@:sol2 mir
0
}}
10 day {{ NB. Pipe Maze
NB. Great another maze searcher!
NB. For each index, find connected neighbours, with */@$ being fill.
NB. Find linearised symbols, connected neighbours of those, coordinates (for part 2).
neigh =:{{ NB. y: literal, pipe field
  NB.  self  R    D    L       U
  sh4=. 0 0, 0 1, 1 0, 0 _1,: _1 0
  NB. Connections only where fitting e.g. 'L-' : yes, '|F' : no.
  NB. Learn connection rules by example: all possible connections (all neighbouring syms connected):
ex=. ];._2 {{)n
.....................
.F-7.F7.--.|.L7.|.FJ.
.|.|.LJ....|..S-S-S..
.L-J...FS7...FJ.|.L7.
.......J.LSJ.........
}} NB. this is silly of J. with indent doesn't work...
  NB. corresponding neighbour patterns
  pat=.(('.'([,~-.~),){~ (i.!.0 sh4 +"1/])@($#:[:I.'.'~:,)) ex
  NB. Connected syms turn per direction into pairs of self, neigh of connected occurences.
  NB. Note: number of unique and number of dots the same: no boxing required.
  NB. Permute axes to have shift directions first to facilitate checking later.
  NB.     sort nub;rem having .      perm  self,. neigh 
  cs  =. ([: /:~@~.(#~ 0='.'&e."1))"_1]0 2|:({. ,."0 1 |:) pat
  NB. is connected: checks whether neighbouring characters are effectively neighbours; takes literal array: self, RDLU shifts
  isc =. cs&(e."(_1)~ 0 2|:{.,."0 1|:)
  lin =. '.'([,~-.~),y NB. linearised no-dot syms
  ind =. (<:@#lin),.~(i.!.0 sh4 +"1/])coord=.($#:[:I.'.'~:,) y
  NB. Return lin, list of actual neigh; coords of each entry in lin.
  lin;(lin (] (*+(<:#lin)*-.@]) isc@:{~) ind);coord
}}
NB. Part 1: Find max dist from S in loop.
NB. Start all dists at */@$f and set S loc to 0, iterate: 1 + <./ {~ until no change.
p1=: {{ NB. a bit overkill, but at least ready for part 2 to be 'how many loops are there?'
  NB. Linear version of field and neighbours for each
  'll nn'=. 2{.neigh ];._2 y
  NB. Initialise distances of pipes to S as max, only 0 where symbol = S
  init =. 0 (ll i. 'S')}(#~) mm=:# ll
  NB. flood field from S; discard any non-connected component; take max.
  >./ (#~ mm&>) nn ({.<.1+<./@}.)@:{^:_ init
  NB. Alternative -:<:#(|:nn) ([: ~.@, {~)^:_ ll i. 'S' just as slow.
}}
NB. Part 2: Surprise: entirely different part 2: what's the enclosed area?
NB. Adapt p1 s.t. loop becomes 1, then, for each row, figure out included square, subtract +/,loop. Need to use actual letters, because simple 0 1 won't work because of horizontal edges.
p2=: {{
  NB. Linear version of field, neighbours for eachand coordinates of lin
  'll nn co'=. neigh y=. ];._2 y
  NB. Initialise distances of pipes to S, only 0 where = S
  init =. 1 (ll i. 'S')}0(#~)mm=.# ll
  NB. Find connected pipes; Reconstruct field
  loop =. (}: nn +./@:{^:_ init) co} ($y)$0
  NB. Infer automatically which symbol to replace S with (simpifies parsing with ;: below)
  neighS=.I.'.'~:}.{.ll{~(|:nn)#~'S'=ll NB. RDLU neighbours of S
  repS =. 'F-J7|L'{~((#~ </"1) >,/{,~<i. 4) i. neighS NB. replacement for S
  NB. Keep only loop in symbol-field and replace S with correct symbol for connectivity.
  y=. repS (<($#:1 i.~'S'=,)y)} (-.loop)}y,:'.'
  NB. FSM for cutting rows into included parts (edges included, could have implemented excluding them)
  NB. State transitions. Assumes only loop ~: '.'; new_state j. operation_code
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
  fsm=.(2;+.s)&;:
  NB. Function code 2 returns start,len pairs of included squares; including edges.
  NB. Sum per row sums of lengths of words, and subtract edges.
  (+/,loop) -~ +/ +/@:({:"1)@fsm"1 '.-|FLJ7'i. y
}}
NB. tst
tst=:{{)n
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
}} NB. expect 8 for p1, 1 for p2
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
}} NB. expect 23 for p1, 4 for p2
0
}}
11 day {{ NB. Cosmic Expansion
NB. Part 1: Expand empty cosmos rows and columns; then find the sum of the shortest L1 paths between each unordered pair of galaxies. Working in coordinates rather than pixel space for fear of part 2.
par =: '#'=];._2 NB. Turn into binary matrix; y= io''
loc =: ($#:I.@,) NB. Locations of galaxies, y=binary matrix
emp =: (|: <@(-.~ i.)"1 0 >./) NB. Empty rows/cols, y=coordinates
ex1 =: ([: |: |: ([+([:+/>)&>) emp) NB. Expand; y=coords
NB. Distance triangle sum: y= coords
dts =: [: +/@,@(*</~@i.@#) +/"1@:|@:(-"1/~) NB. 10x faster than +/@:|@:-"1/~
sol =: dts@]:@loc@par NB. Adverb applying dts, loc and par around "expand" verbs.
p1  =: ex1 sol
NB. Good guess for my part 1 implementation: now spaces are 1e6 times larger, instead of 2 times larger.
ex2 =: ([: |: |: ([+(1e6-1)*([:+/>)&>) emp) NB. expand; y=coords
p2   =: ex2 sol
0
}}
12 day {{ NB. Hot Springs
NB. Input: '.#' for ok/broken springs and list of numbers of runs of broken springs. Fill in ? such that list matches. First try: recurrent approach, filling in first ? and seeing whether list still matches; if not backtrack.
NB. Keep .#? and turn list in nums
par =: ([: (('.'([,,~)])&.>)`(".&.>)"0 [: <;._1 ' ',]);._2
NB. Check whether list still matches springs; version 1
NB. chk =: +/@:=&'#';.1~ 2 (~:*.'#'=])/\ '.', ]
NB. Version 2: FSM ;: makes rec more than DOUBLE as fast
chk =: ([: {:"1 (2;(+. 0j0 1j1,:0j3 1j0));:'.#'&i.) 
NB. Partial check up to last . (because run of # can be incomplete) after selecting until before last ?
pchk=: chk@({.~ #|i:&'.')@({.~ i.&'?')
NB. peq: prefix equal?
peq =: -:/@(<.&# {."1 ,:) 
NB. Recursively fill ? until no left; count possibilities coming back.
rec=:{{ NB. y=list, x=row of springs
NB. fq= first question mark.
  if. (#x)=fq=. x i. '?' do. NB. done; check entire list
    y-:chk x
  else. 
    ca=. '.#' fq}"0 1 x NB. candidates for next ?
    select.  y (peq pchk)"1 ca
    case. 0 0 do. 0            NB. wrong, exit 
    case. 1 0 do. ({.ca) rec y NB. recurse left
    case. 0 1 do. ({:ca) rec y NB. recurse right
    case. 1 1 do. +&(rec&y)/ca NB. recurse both
    end.
  end.
}}
p1=:[: +/@:> rec&>/ t. 0"1@par NB. recurse on each input
tst=: {{)n
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
}}
NB. Part 2: Spring rows are folded; repeat each row 5 times, interspersed with ?
NB. I didn't keep all attempts, since there were too many
parf =: ([: (('.'([,,~)])@:(}:@$~ 5*#)@(,&'?')&.>)`(($~5*#)@:".&.>)"0 [: <;._1 ' ',]);._2
NB. third try: fully fsm based: emit runs of #, up to first ?
NB. design choice: state transitions emit both runs of . and #: avoids above, but length counts slightly more complicated.
NB. bunch ? with 'any other'
st=: ".;._2 {{)n
1j1 2j1 3j6 NB. start
1j0 2j2 3j3 NB. .
1j2 2j0 3j3 NB. #
3j6 3j6 3j6 NB. ? or other: end immediately
}}
NB. Above st with function code 4 produces as coded outputs for the following cases
NB. 4: . ended because of .#
NB. 6: # ended because of #.
NB. 5/8: ./# ended because of ? (or generally; any non .#)
NB. 3/7: ./# ended because of input end.
fsm=: (4;(+.st);<;:'.#')&;:
NB. Part 2 solution: batch rows per (max) # processors, in each, do caching recursive solution.
NB.  hash per row is good balance between storage and speed
p2 =: {{
  p=. 0
  for_b. (-@(({.8 T.'') <.#) <\ ]) parf y do.
    NB. pass each worker a row and a locale for caching
    p=.p++/> ('d12wl',"_ 0 (#>b){.'012345678789') wl t. 0"1 >b
  end.
  coerase <"1('d12wl',"_ 0 (#>b){.'012345678789') NB. clean up
 p
}}
NB. after many different caching setups: per row caching is faaar faster, and easily parallelisable.
wl=:{{
  cocurrent x   NB. enter worker locale, with name from x
  coinsert'd12' NB. insert this day's definitions
  keys=: 0 20$0 NB. set up hashmap for locale
  vals=: 0$0
  rec2"1 y      NB. start recursion, returns number of possibilities for this line y
}}

NB. In theory, one could do the below without having to alter the string,
NB. by using ijrd to restart the FSM after the question mark, and continuing. Below does not, for simplicity.
NB. I tried using symbols for caching, rather than MD5, but slower, especially on initial run.
NB. CRC32 does not work, probably because of collisions
NB. Finishes before Universe heat death, but still slow (38s on 8 cores, better than it used to be)
NB. Tried using locales as caches themselves, but hopelessly slow.
rec2=:{{ NB. y: row, list
  'r l'=.y                           NB. row of springs, list of broken runs
  key=._1 (128!:6) r,a.{~l           NB. get binary MD5 of row, list as key
  if. (#keys)>ind=.keys i. key do.
      vv=.ind{vals
  else.
    NB. run FSM, check exit codes
    NB. 4  : .   ended because of .# ; 4 or 6 are never last.
    NB. 6  : #   ended because of #.
    NB. 5/8: ./# ended because of ? (or generally; any non .#)
    NB. 3/7: ./# ended because of input end.
    code=. {:{: g =. fsm r           NB. fsm returns index,length,coded row/col
    nn=.#dam =.(1&{"1 #~ 6={:"1) g   NB. complete damaged runs, and length
    if. code e. 5 8 do.              NB. end due to ?
      fq=. +/}:{:g                   NB. first question mark is after last run (start+len)
      newl=.nn}.l                    NB. drop complete damaged runs from l
      if.  (-. dam -:nn {. l)do.     NB. prefix doesn't match, or spurious #'s after partial when y is empty
        vv =. 0                      
      elseif. (0=# newl)*.'#' e. r }.~ >:fq do. NB. damaged list is empty, yet # remain after question mark
        vv =. 0 
      else.                          
        if. code=5 do.               NB. 5 : run of .'s ended at ?
          newr=.'.',.'.#',"0 1(>:fq)}.r NB. 2 new rows of springs
        else.                        NB. 8 : run of #'s ended at ?
          sp=. {.{:g                 NB. recurse keep incomplete part
          newr=. '.',. sp}."1 '.#'fq}"0 1 r NB. 2 new rows of springs
        end.
        vv=. +/ newr rec2@;"1 _ newl NB. recurse
      end.
    else. NB. finished with code 3/7, end of string, check l-:runs
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
NB. Find reflections by rows / cols
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
par=:[: (([:<];._1);._2~ LF2&E.) LF([,,~)] NB. add extra LF, then split by double LF's
NB. Find reflection starting at top & bottom
NB. The idea is that a reflection occurs when a pre- or postfix is a palindrome. }. and }: to remove the trivial length 1 palindrome
T=:        [: -:@(>./)@}. (#*]-:|.)\
B=:# ([|-) [: -:@(>./)@}: (#*]-:|.)\.
NB. Find top or bottom reflection.
NB. Solution too good, also catches odd reflections, leading to halve rows...
NB. The hook filters these out. Uses given that there is a single line per field.
refl=:[:(*(=<.)) (] B@[^:(0=]) T) NB. @i.~: initially thought to work on selfie. forgot to add it eventually, which oddly worked. With selfie a fraction slower.
p1 =: [: +/@(refl@|:&> + 100* refl&>) par

NB. Part 2: There's one smudge on each mirror that creates a new reflection line. Find it, and summarize only new lines
NB.  Sum 2x multipliers of fixed reflection line
p2 =: +/^:2@(100 1*"1])@:(fix&>)@par
NB. can selects candidate rows for swapping
NB. ({. ... }.)\. to consider only rows not considered yet.
can=:(i.@# (,.+)&.> ([:<{.;@([: (<@I."1#~1=+/"1)~:"1) }.)\.)
NB. J905 only (inverse of m&{)! tacit adv, swapping .# at m in y
swap=: (&{)('.#'&([{~-.@i.)&.)
NB. fix smudge, on mirror y; returns new reflection line
fix =: {{
  NB. candidates are the sole differences with any of the other rows
  candTB=. <   "1; can    y
  candLR=. <@|."1; can |: y
  orig  =. (refl , refl@|:) y
  for_c. candTB,candLR do.
    NB. Cannot use refl for same, because it assumes only one reflection line is present.
    NB. Fixing the smudge could cause multiple reflection lines. this was the failure case of my code...
    nn =. orig ([:(#~]=<.)]-.0,[)&.> ((T,B);(T,B)@|:) c swap y 
    if. +./#&> nn do.
      {.@(1&{.)&> nn return. NB. assumes only 1 valid swap candidate
    end.
  end.
  echo 'none found' NB. shouldn't happen
}}
0
}}
14 day {{ NB. Parabolic Reflector dish
NB. Given rolling rocks O, # fixed rocks, tilting the platform rolls them in a certain direction.
NB. Part 1: Calculate the load of all rocks when tilted north, with rocks weighting 1 at the south edge, and #y at the north edge;
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
NB. Padded par; pad platform to have an edge to roll against.
ppar=: '#'([,.~[,.[,,~) ];._2
NB. Idea: key by col; use I. to see where they end up.
NB. Returns boxed fixed and rolling rock coords in field y.
rocks=:'#O' <@($@]#:I.@(=,))"0 _ ]
NB. Tilt operation: m: axis (U-D=0/L-R=1), sense(0=D/R); x,y fixed,rolling rock indices(expected sorted)
tilt =: {{
  'ax se'=. m     NB. Axis and tilt sense
  oax    =. -. ax NB. Cross fingers and hope p2 is 2D as well.
  NB. Comments below relate to 0 0-:ax,se, but code applies to all
  NB. Fixed rocks: box row numbers per column, no prefix required due to padding with #
  fr =. (oax&{"1 <   /. ax&{"1) x 
  NB. Prefix fake points for ensuring all points accounted for, then discard
  rr =. (oax&{"1 <@}./. ax&{"1) (,.~i=.i.#fr),y
  NB. Find new locations for rolling rocks
  if. se do. NB. up/left rolling
    NB.  I. works with left open intervals ]a, b]
    NB.    fixed rock + i.count of intervals each col
    rbnew=. fr ([ ({~ +1+[:;<@i.@#/.~@]) <:@I. )&.> rr
  else.      NB. down/right
    rbnew=. fr ([ ({~ -1+[:;<@i.@#/.~@])    I. )&.> rr
  end.
  NB. return both fixed and rbnew; if ax=0: append col num
  ;i ,.&.>~`(,.&.>)@.ax rbnew
}}
NB. Visualisation of field, given boxed coords of fixed and rolling rocks
vis  =: (('#O'#~#&>@])`(;@])`('.'$~1+>./@;@])}]) 
score=: [: +/ ({:@[ -&:({."1) ]) NB. no 1-base correction because of padding
p1=: [: ([ score 0 1 tilt)&>/ rocks@ppar 
NB. Part 2: score after 1e9 cycles of ULDR
NB. single cycle
cyc=: [ 1 0 tilt [ 0 0 tilt [ 1 1 tilt 0 1 tilt
p2=: {{
  'F R'=. rocks ppar y
  found=. 0$,:R NB. cycle detection
  new=.R
  while. (#found) = ind=. found i.!.0 new=. /:~ F cyc new do.
    found=. found, new
  end.
  NB. Found loop, with length:
  len=. (#found)-ind
  rem=. <:len|1e9 - #found NB. <: because while has done next cyc 
  F score F cyc^:rem new
}}
NB. Not the fastest; I could reduce some work keeping F boxed per rows and cols but would make structure far less intuitive.
0
}}
15 day {{ NB. Lens Library
NB. Part 1: really simple with forward fold
hash=: ] F.. (256|17*a.&i.@[+])
p1=: [: +/ [: 0&hash;._1 ',',}:
NB. Part 2: initial lens configuration; for each step:
NB. - find box num by hash y,
NB. - -  > remove lens y from box,
NB. - =m > put in new lens with focal length m, labeled y.
NB. Looks like folding through list again. State=256boxes ; syms ; focal lengths
NB. Note: name length not uniform: use syms
NB. Parse instructions to: sym op: 0:- else =
NB. If not for bug in 0&". on '-', that could have been used instead of rplc... TODO bug
par=: [: (s:@{. ; ".@>@{:)@;:;._1 ',',rplc&('-';' 0')@}:
NB. Initial state from parsed input: boxes,syms,hashes,focal len
NB.   This assumes symbols to be unique, i.e. no two same labelled lenses with different focal length
init =: (256$a:); (;0&hash&>@(s:^:_1);0$~#)@~.@;@:({."1)
NB. min: Do min operation in x on state y.
min =: {{
  NB.       hashes  {~ syms    i. {. operation (result par)
  boxi=. x (2&{::@] {~ 1&{::@] i. >@{.@[) y
  NB. Remove sym from box at i in box array (THANK YOU J905)
  -.&(>{.x)&.>&.(boxi&{)&.>&.(0&{) y
}}
NB. eq: Set refraction for lens {.x and add to box if not present.
eq =: {{
  boxi=.(2{::y){~symi=.(1{::y)i.>{.x NB. indices; same as for "min".
  NB. add sym in box i in boxes   set FL@symi in focal lengths
  ~.@,&(>{.x)&.>&.(boxi&{)&.>&.(0&{) (>{:x)&(symi})&.>&.(3&{)y
}}
op =: min`eq@.(*@>@{:@[) NB. if 0: min, else eq
NB. post: Extract power from state: (box+1)*(slot+1)*FL
NB.     sum   box no   *   FL ind*slot no perbox sym i. labels 
post=:[:+/@;(1+i.256) *&.> ({:({~*1+i.@#@])L:0 (1&{ i.L:0 >@{.)) 
p2=: [: post@(init ] F.. op ]) par NB. TODO: bug? faaar faster than post F..
0
}}
16 day {{ NB. The Floor Will Be Lava
NB. A laser beam enters top left, find energised points. Try recursive approach: each point returning ~.self,children
NB. Recursion attempt worked, but not on full input (likely stack error related crash); had to rewrite explicitly.
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
prep=:{{ NB. split off for p2; FIELD & CAND same for different calls.
  FIELD =:'/\-|'i.];._2 y     NB. Field
  NB. Verb using FIELD for getting next cand in step.
  CAND  =: +/ fsl`bsl`min`bar`nop@.(FIELD{ :: 4~<@[) ]
  0 0$0
  }}
NB. Laser beam through field y; x= pos,dir
laser =: {{
  NB. TODO: Raise J bug: sparse doesn't work.
  SEEN  =: 0$~4,$FIELD NB. Indicates whether loc seen in direction (4 directions)
  NB. SEEN  =: 1 $. (4,$FIELD);0 1;0 NB. Indicates whether loc seen in direction (4 directions)
  NB. SEEN  =: 1 $. (4,$FIELD) NB. Indicates whether loc seen in direction (4 directions)
  NB. Take step until no active points, starting at x. step discards point once seen in a specific direction
  ;@:(<@step"_1)^:(*@#)^:_ ,:x
  +/,+./SEEN
}}
NB. For reference only; had an off-by-one error for part 2 that disappeared in the tacit translation. Was +-16s, now 6.6s
stepe=: {{ NB. y dir/loc of current, return array of next points. Uses globals FIELD & SEEN for loop detection: if current coord, direction seen, stop.
  y=.2 2$y
  select. nt=.FIELD { ::'#'~ <np=.+/y NB. new type; new point; ::'#' for catching out-of-bounds
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
  sel=.1#~#ny               NB. selected?
  scs=.0 3$0                NB. seen-coord-selected
  for_nxt. ny do.           NB. :: triggers if out of bounds
    sel=. sel nxt_index}~ (0+./@:>_2{.nxt) +: *SEEN {~ :: 1 <sc=.((0 1,1 0,0 _1,:_1 0)&(i.!.0)@(2&{.) , _2&{.) nxt
    scs=. sc,~^:sel scs
  end.
  SEEN=: 1 (<scs)} SEEN     NB. record if not
  sel#ny
}}
NB. Tile-laser interaction: x: new pt; y: cur pos,:dir 
fsl=: [:,:(,~ -@|.@{.)             NB. tile = '/'
bsl=: [:,:(,~   |.@{.)             NB. tile = '\'
min=: ,"1~ (,:-)@|.`,:@.(0={.)@:{. NB. tile = '-'
bar=: ,"1~ (,:-)@|.`,:@.(0={:)@:{. NB. tile = '|'
nop=: [:,:(,~{.)                   NB. tile e. '.#'
NB. Generate coords into seen for new y's
seencoord=:((,-)0 1,:1 0)&i.@:(2&{."1) ,. _2&{."1
NB. Single step for all active points.
step=: {{ NB. y=dir/loc of current, return array of next points. Uses globals CAND & SEEN for next position candidates and loop detection: if candidate coord, direction seen, remove candidate.
  ny=. CAND 2 2$y
  NB. find seen coordinates dir-ind ,. pos
  sc=.seencoord ny NB. taken out for speed!
  NB. Find sc to be kept (unseen and in range)
  NB. first part to exclude negative candidates; slower when taken out as adverb as done for seencoord...
  sel=. ny ((0 +./"1@:> _2&{."1)@[ +: {&SEEN :: 1 @]) <sc
  SEEN=: 1 (<sel#sc)} SEEN NB. record selected as seen
  sel#ny
}}
p1 =: (0 1 0 _1)&laser@:prep
NB. Part 2: max energised squares for each possible edge entrance. Probably smarter ways possible; this is pretty bruteforce.
p2 =: {{
  prep y NB. defines FIELD and CAND for step
  'R C'=.$FIELD
  NB. all edge entrances, counterclockwise
  edges =:,/(0 1,_1 0,0 _1,:1 0) ,"1 (_1,.~i.R),(R,.i.C),(C,.~i.-R),:(_1,.i.-C)
  best=.0
  wsn=. cocreate@''"+ i. nn=.>:1 T. '' NB. numbered locales slightly faster & less fat.
  for_i. wsn do. NB. insert d16 into workspace locales
    cocurrent i
    coinsert 'd16'
  end.
  for_i. (-nn) <\ edges do. NB. batch all edges by nn
    res=.''
    for_t. >i do.
      cocurrent t_index{wsn    NB. set worker locale to access SEEN
      res=.res, t laser t.'' y NB. 3x faster with t.
    end.
    best=.best >. >./>res      NB. update best from best and max of res
  end.
  coerase wsn                  NB. clean-up used locales
  best
}}
0
}}
17 day {{ NB. Clumsy Crucible
NB. Minimize heatloss, go max 3 steps straight, and no 180 deg turns. Each square indicates heat loss incurred when entered. As allowable states depend on previous choices, use recursion, not Dijkstra etc.   
NB. Encode each point as row of (neighbours,heat loss)
NB.     val apd  list step-win    D R U L by win #el pad sides  inds of shape num mat
par=: [: (, ,.~ [: ,/ (1 1,:3 3) (7 5 1 3{,);._3 (*/([,[,~[,.[,.~i.@])])@$) "."0;._2
NB. Traverse from top left to bottom ; u=direction rule; n=steps; y=input text.
traverse =:{{
  nn=: par y  NB. Neighbour info
  NB. Keeps lowest loss for each (ind,prev dir,count)
  seen=: 1e9$~ (4,n) ,~ {.{:nn NB. no neigh=#squares
  NB. For homogeneity of coding below, hand code first two states s.t. valid direction can be used.
  NB. States: point i,prev dir d,count steps dir c,acc a .
  NB. Note that 0 encodes 1 in the # steps.
  'i d c a' =: |: (2{.{.nn),.0 1,.0,.({:"1 {~ 2&{.@{.)nn
  last=.<:na=:{.{:nn NB. Value for last & non-applicable
  neig=. }:"1 nn     NB. Split candidates and heat loss
  heatloss=. {:"1 nn
  while. *#i do.
    can =. i{neig NB. Candidate indices
    NB. Rotate conditions s.t. 0 ends up at cur. direction; use with u for conditions.
    NB. fil = Boolean mask for allowable candidates
    fil =. (na~:can) *.(-d)|."0 1 u c 
    NB. Compose new versions of i,a,d,c
    i     =. fil #&, can
    a     =. (i{heatloss) + (pc =. +/"1 fil) # a
    'd c' =. (4|I.,fil) ([;(pc#c)(]*+)=) (pc#d)
    srta  =. a{~ srt =: /: a
    NB. Filter idca where seen not better. Sorted so smallest loss kept amongst each i-d-c triple, even if non-unique
    NB.       unique and sorted<seen sorted inds in seen
    fil2 =. (~:!.0*.srta<seen{~<) srtidc=. srt{i,.d,.c
    NB. Apply filter
    'i d c a'=. fil2&#"1 srta,~|: srtidc
    seen     =: a (<i,.d,.c)} seen NB. Remaining are necessarily better due to previous filtering, so store in seen.
  end.
  <./,last {seen
}}
NB. Original wording for fil line above written for p1:
NB.      non-neigh       180 deg and long runs
NB. fil =. (na~:can) *.((c~:2),.0) (<(i.#i),"0(d,._2+d))} 1$~$can
NB. Part 1: max 3 steps in same dir., no 180 deg turns
NB. Allowable direction, verb taking cur. dir step count
NB. note that c=0 counts for 1 (to save a dim in seen).
NB.   L B R    straight            v--- max step count
p1=: (1 0 1,~"_ 0 <&2)    traverse 3 
NB. Part 2: straight: minimum 4 blocks, max 10 blocks; still no 180 U-turns.
NB.   min F  L  B  R max           v--- max step count
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
NB. nice vis: $vis'' also fun to look at where p1 and p2 distances differ, or also 'surface'plot vis''
vis=: {{ ([viewmat)1e9 (~: (*+-.@[*[:>./ #&,) ]) <./"1],/"2]141 141 $ seen }}
0
}}
18 day {{ NB. Lavaduct Lagoon
NB. Input is direction, length, and color (ignore for part 1). What's the circumscribed surface?
dirs=: 4 2$1 0 0 1 _1 0 0 _1 NB. DRUL
par=: ([: ; ((dirs,@:{~'DRUL'&i.)&.>)`(".&.>)"0)@(0 1&{)@;:;._2
area=: {{
  NB. Fold steps to get edge coordinates
  edge=.0 0 ] F:. (]+(}:*{:)@[) y
  NB. Jphrases 9F.m29: edges not counted, so returns 42 < 62 on test. Need to classify edge: straight units loose half a unit, outside corners 3/4 and inside corners 1/4. i.e. for test: area++/9 5 24 * 3 1 2%4
  NB. outer  inner  straight; . outside; # inside; -+ grid
  NB. ..|..  ..|##  ..|..
  NB. ..|..  ..|##  ..|..
  NB. --+--  --+--  --+--
  NB. ..|##  ##|##  ##|##
  NB. ..|##  ##|##  ##|##
  NB. Signed area; area sign indicates sense: + ccw, - cw
  sar=. ([: -: [: +/ 2: -/ .*\ ]) edge 
  NB. Area sum 1/4 inner(1) or outer(3) corner    + 1/2 straight
  (|sar)+(+/1r4*4|(- _1&|.)dirs i. 2{."1 y)+1r2*(+/-#)2{"1 y
  NB. Could depend on area sign for corner in/out though, not sure.
}}
p1=: area@par
NB. Oops, have to decode hex instead and apply those as lengths; x: because big num
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
NB. Part 1: Given a list of linked conditions and a set of tuples, what is the sum of the values of the tuples given that are eventually accepted?
NB. Convert name{cond:dest,dest} to explicit verbs
conv=: {{
  NB. toupper needed for avoiding stomping over (i.e. occluding) J's stdlib, and steer clear from x,y,m,n,u,v having special meanings.
  NB. Function names and corresponding bodies.
  'nm bod'=: ( ({.~ ,&< ((','<;._1@,])@}.~>:)) i.&'{') }: toupper rplc&('A';'XXXACC';'R';'XXXREJ') y
  bod=. ([: <;._1 ':'&,)&> bod NB. split cond and consequence
  NB. Wrap all but last body part in if. elseif. ...
  ifs=.,(((<'if. '){.!.(<' elseif. ')~#),.{."1,.(<' do. '),.(<' y '),.~{:"1)}:bod
  NB. Check special case: no conditions: no need to wrap non-cond in else. ... end.
  noncon=. (' y',~])&>`(' end. ',~ ' else. ',' y ',~>)@.(1<#bod) {.{: bod
  NB. Function text with var assignment, ifs & noncon.
  funt =: '''X M A S''=.y',LF,(,_5 (LF,~;)\ ifs),noncon
  NB. Assign new function to name nm
  (nm)=:3 : funt NB. global on purpose.
  0 0$0
}}
NB. Parsing: one box per line for each part of input.
par =: [: (<@:(<;._2@,&LF) ;._2~ LF2&E. ) LF ,~]
p1=:{{
  NB. R A implementations; removing reject halves execution time
  XXXREJ=: 0: NB. removed: {{ 0 0$0[reject=: reject,y}}
  XXXACC=:{{ 0 0$0[accept=: accept,y}}
  accept=: 0 4$0   NB. Accepted tuples
  'fun val'=:par y NB. Parse input
  conv&> fun       NB. Convert functions to verbs
  vals =: ".@(#~ e.&',0123456789')&> }. val NB. }. discards empty first box introduced by parsing
  IN"1 vals        NB. IN Defined by conv
  +/,accept        NB. Sum all values of accepted
}}
NB. Great. Part 2 doesn't jive with part 1's setup... back to the drafting board!
NB. How many combinations would be valid when each attribute e. >:i.4000?
NB. Recursive approach working on intervals instead.
p2 =:{{
  NB. Keep only function declarations
  fun=:0{::par y
  NB. Set globals for lookup: symbols, their conditions, destinations corresponding to the conditions
  'sym con dest'=: |: conv2&> fun
  NB. Recurse on IN with full range
  (s:<'in') rec 4 2$1 4000
}}
NB. Convert a workflow to a symbolic name, conditions and corresponding destinations.
conv2=: {{
  NB. Split workflow name and body. }: discards last }
  'nm bod'=: ( ({.~ ,&< ((','<;._1@,])@}.~>:)) i.&'{') }: y 
  bod=. ([: <;._1 ':'&,)&> bod NB. split cond and destination
  NB. Convert conditions to matrix: var ind, op <>,val
  con=. (('xmas' i.{.),('<>'i.1{]),(2".@}.]))&> {."1}:bod
  dest=. s: ({.@{: ,~ {:"1@}:) bod NB. Make symbols of destinations
  (s:<nm) ; (<con) ; <<dest        NB. Compose return values
}}
NB. Recursion: return how many values match
rec=:{{ NB. x: sym of function; y: ranges for each var x,m,a,s
  NB. Break if leaf node(A,R) return size of current interval
  if. 2>ret=.(s:' R A') i. x do. ret**/1+-~/"1 y return. end.
  NB. Verified: ranges never become empty, so removed second early exit
  NB. Recurse:  cur dest   rec items cur cond applied ranges
  +/ (sym i. x) ((dest{::~[) rec"_1 (con{::~[) applywf ]) y
}}
NB. Returns split ranges x: workflow (con); y: coord range
NB.              apply each rule in x to ranges in y.
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
NB. m: it; x: starting point(s)(centered around middle); y: input
one=: {{ NB. x: its at which count is needed; y: field
  nn  =. neigh par y NB. non-wrap neighbours
  NB. coords of indices into as in par.
  inds =. ; 'S.' <@($@]#:(I.@:=,))"0 _ ];._2 y
  nonn =. {:{:nn     NB. non-neigh index
  cur =. (i.!.0 -:@:(>./)) inds NB. start=center
  seo =. cur;0$0 NB. seen ind in odd/even iterations
  ret =. 0$0
  for_i. >: i. >./x do.
    cur =. , cur{nn
    NB. remove non-neigh and seen i
    NB.     (non-neigh or non-unique) not-or seen
    cur =. cur ([#~(=&nonn +. -.@~:)@[ +: e.!.0) seo{::~2|i
    NB. add to seen i
    seo =. ,&cur&.>&.((2|i)&{) seo
    if. i e. x do.
      ret =. ret,#(2|i){::seo NB. add to debug-return for each step.
    end.
  end.
  ret
}}
{{)n 'block setup rad=3,2 and 1'
   N     North
  DFD    D=NW/NE, F=Full   N
 DFFFD                    DFD    N
WFFFFFE  West/Full/East  WFFFE  WFE
 DFFFD   D=SW/SE          DFD    S
  DFD                      S
   S     South
   #4xdiag (D)= radius - 1, #4xcorner = 4, #full = 1+4*rad*-:rad
   so system to solve is:
   [ 1  1 0 ]   [C]   
   [ 1  5 1 ] . [F] = (65 +1 2 3*131) one 7 nup io''
   [ 1 13 2 ]   [D]
}}
p2=: 26501365&{{
  NB. x nit; y: input (assumed to have diamond-shaped corridors and cross connections)
  hf  =: <:-:<: sz =: #];._2 y NB. assume square
  rad =: x (]%~(--:@<:)) sz NB. radius in tiles after center one NSEW
  NB. counting visited squares for sizes 3x3, 5x5, 7x7 allows solving system to find numbers of visited fields in 4 corners(c), full (f) fields and 4 diagonals (d):
  cfd =: (1 1 0,1 5 1,:1 13 2)%.~ (65+131*1 2 3) one 7 nup y
  NB. extrapolate for full number of iterations, knowing counts of corners, full and diagonal fields.
  cfd +/ .*&x: num=:1,((1++:@(*<:)),<:)rad
}}
NB. repeat tile y x times; transpose inspired by kronecker product, see Essays.
nup=: [: , LF,.~ [:,/"2 [:,/ 0 2 1 3 |: ,~@[ >@$ [:<];._2@]
0
}}
22 day {{ NB. Sand Slabs
NB. exploration: ~1.2k blocks, max len=5; so can split in cubes
NB. parse input to cubes
sortz=: /: <./@{:"1&> NB. sort blocks ascending by lowest z values
par=: [: sortz ([:(<./<@:|:@:+(**/[:i.1+>./)@:(-~/)) _3]\ ".@rplc&', ~,');._2
NB. drops blocks boxed blocks in y (assumed sorted)until they can, returning the modified blocks (in the same order)
dropbl=:{{
  NB. y: boxed blocks, expected sorted by lowest z cube
  field =. 0 2$0
  height=. 0$0
  for_bb. y do. 
    b=.>bb
    NB. find block b in field, and corresponding max height
    mxh   =. >./ (height,0) {~ ind=. field i.!.0 xy=.}:"1 b
    NB. add new xy coords; nu = new and unique xy coords
    field =. field,~. xy #~ new=. ind = #field 
    NB. update existing heights, append new ones
    bh =.mxh+1+(-<./){:"1 b NB. new block heights for each cube
    newh   =. xy >.//.&(new&#) bh      NB. new heigts
    updh   =. xy >.//.&((-.new)&#) bh  NB. heights for updating
    updi   =. ~. ind #~ -. new         NB. and their indices
    height =. newh,~ updh updi} height NB. update heights
    NB. update b_index box in y to new height
    y =.(bh,.~}:"1)&.>&.(bb_index&{) y
  end.
}}
NB. find how many bricks can be disintegrated without making any bricks fall further; do outfix on each cube, see whether remains the same (safe) or changes when attempting to drop further.
p1=: [: +/@:> 1 ( -:   dropbl) t. ''\. dropbl@:par
NB. chain reaction: figure out sum of fallen blocks for each removed block
p2=: [: +/@:> 1 (+/@:~:dropbl) t. ''\. dropbl@:par
 NB. expect 5
tst=:{{)n
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
}}
   NB. expect 2
tst2 =:{{)n
0 0 1~1 0 1
0 1 1~0 1 2
0 0 5~0 0 5
0 0 4~0 1 4
}}
0
}}
23 day {{ NB. A Long Walk
NB. longest possible walk from top to bottom via . and >, but onlyusing going in arrow direction, and using each tile once.
NB. viewmat '.# ><^v'i.];._2 io'' 
NB. from the looks of it, paths are very tortuous, but not branching often: process to directed graph with distances as edge weightoNB. make new neigh function as needs to account for:
NB. - directional neighbours
NB. - path length between nodes
sh4 =: <. +. 0j1^i.4  NB. shift in 4 cardinal directions
NB. y: input, returns tile types and indices; .v>^< same as sh4
par =: [: (('.v>^<' i.'#'-.~,) ,. ($#:'#'I.@:~:,)) ];._2
NB. find neighbours with right connections (. or slope in good dir}. 
NB.      Non-neigh apd |:  types   non-neig<.+ big*  .  +: v > ^ <  neigh ind self shift4@:coords    
neigh =: (5,~{."1);[: ({:@{: ,~ |:) (5,~{."1) (<:@#@[<.]+#@[*(0&= +: 1 2 3 4&=)@:{~) (i. sh4 +"1/])@:(}."1)
NB. e.g. walk entire graph: $0 (] ~.@, ,@:{~)^:_~ NN
NB. adapt to stop at crossing: y starting ind, prev ind (to be avoided).
cr =: ''&$: : {{
  while. 1 do.
    msk=.([: *./"1 >&0 ) KI {~ l=.{&NN y
    if. (2=+/ msk) +. (NON-1)={:y do. NB. 2 because NON always shows up, and has 4 neigh with kind=5; also stop if last reached
    ({:,<:@#)  y return. NB. return crossing id and step count
  else.
    new=. x-.~ y ~.@, ,l
    if. new-:y do. NON,__ return. else. y=.new end.
  end.
end.
NON,__
}}
NB. simplify graph by removing 'tubes' ? maybe not even needed.
NB. recursively walk our graph; if not junction, take next, recurse only at junction. 
NB. x: list of seen junctions, y: point uses global NN, NON,LAST
rec =: {{
  'y nn'=. x cr prev=. y
  if. y>:(NON-1) do. nn return. end.
  echo prev,_,y,nn,_,l=. NON-.~ NN {~ y 
  nn + >./(x,y) rec"1 0 l return.
}}
p1 =: {{
  'KI NN'=:neigh par y
  NON=: {:{:NN
  <:'' rec 0
}}
tst=:{{)n
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
}}
NB. same as neigh above, but consider only 5 not connected
neigh2 =: (5,~{."1);[: ({:@{: ,~ |:) (5,~{."1) (<:@#@[<.]+#@[*(5&=)@:{~) (i. sh4 +"1/])@:(}."1)
recgr =: {{
  'y nn'=. x cr prev=. y
  if. y>:(NON-1) do. ,:prev,y,nn return. end.
  l=. NON-.~ NN {~ y 
  NB. echo prev,_,x,_,y,nn,_,l
  NB. almost... spurious 0 row when end reached; still
  (({:x),y,nn), ~.(x,y) ;@:(<@:recgr"1 0) l return.
}}
p2 =: {{ NB. works for tst, too slow on full. check where stuck. cut bad branches.
  'KI NN'=:neigh2 IN=: par y
  NON=: {:{:NN
  numn =. 4-+/"1 NON=NN
  NB. 'KI NN'=:neigh par y NB. TRY: use neigh2 for hub detect, neigh for walking so edges directed. still missing and more complicated, not assured all links are well behaved,e.g. <..> could happen...
  'S D'=: I. 1=numn NB. start,dest
  H    =: S,D,~I. 2<numn NB. hubs, crossings+S,D
  NB. direct iteration of full adjacency matrix too slow for io''.
  NB. start from all hubs, check where runners cross
  st=.1[ cur=. ,.~H NB. step count, current active
  unseen =. 0 H}1$~#NN NB. unseen nodes
  newnn=. 0 3$0 NB. prev, cur, rel time, abs time
  while. (#cur) do.
   NB. echo'--- ',":st
NB.    echo cur
    NB.     filt nonneigh unique next  pr. hub  ,.~ nxt step.
    cur =. (#~ ~:&NON@:({."1)) ((4#{:"1),.~ NN ,@:{~ {."1) prev=.cur

    keep=. unseen{~{."1 cur 
    cur=.keep#cur
    if. 0=#cur do. break. end. NB. finished.
    cc=. {."1 cur
    NB. add connections to newnn before updating hubs in cur. cross if: same cc; cc neighbours
    NB.      swap duplicates     hubs h~ pairs of selfie-antiselfie cc
    same  =. (~: {"_1 (,:|.)"1) ({:"1 {~ [:(#~~:/"1) (i.~,.i:~)@:({."1)) cur NB. +:st
    
    cross =. ({:"1 cur){~cc i. (] ;@:(<@(,. ]#~e.&cc)"0 1) {&NN) cc NB. 1++:st prob: returns $0 result if #cur is empty.
    newnn =. newnn, cross (, ,. (+:st)+1 0#~,&#) same
    NB. if. 0<same>.&#cross do. 1+'a'[echo st;same;cross end.
    unseen =. 0 (cc)}unseen NB. seen where in cur
    st=.st+1
  end.
  newnn
  NB. re-index; indices can be glanced from newnn
  'S D L' =:gr =:({: ,~ (~.@, i.])@:}:) |: /:~ newnn
  NB. iterative solution not trivial; recurse
  NB. (i.>:{:S) recmax 0 0
  NB. Non-recursive approach TODO: optim (binary mask instead of int list)/ parallel.
  nonrec''
  }}
nonrec=: {{
  cur =. ,:0 0 NB. Length, visited nodes
  LD =.L,.D
  last =. {:S
  ct=:0
  BEST=: 0
  while. (#cur)*. ct<100 do.
    echo ct,BEST,$cur
    NB. problem: does not prohibit visiting twice & graph is undirected
    NB. each state should track visited nodes and exclude those from selection
    new =. LD (<@#~) S=/~ 1{"1 cur NB. LD for last path el 
    cur =. (({.,/:~@}.)@}."1 (,~ >./)/.. {."1) ;new (((}.@],"1~ +&.({."1) ,. {:"1@[) #~ {:"1@[ -.@e. }.@])&.> <"1) cur
    
    NB. filter done&hopeless;
    done =. (last e.}.)"1 cur
    BEST =: BEST >. >./ done # {."1 cur
    nohope =. BEST < ({."1 + (L +/@#~ ( S&e. +: D&e.)@}.)@}."1 ) cur
    cur =. cur #~ -. (done +: nohope)
    echo 'D,NH: ',":  done ,&(+/@:-.) nohope
    ct=:1+ct
  end.
  BEST
}}
recmax=: {{ NB. x: mask available nodes; y: current node
'c l'=.y NB. current node&length so far
NB. done if no x or c; no available nodes
if. (x +.&(0=#) c) +.-.+./nn=.((S=c)*. D e.x) do.
  if. BEST<l do.
    echo 'B ',":BEST=: l
  end.
  l return.
end.
NB. abort hopeless branch TODO: Tighten: L contains both directions, of which only one can be used
if. BEST>l++/L#~ D *.&(e.&x)S do. __ return. end.
  nn
  >./ (x-.c) recmax"1 (\:{:"1) (nn#D),.l+nn#L
}}
NB. not correct: 3720, 3695, 6563 and is <9430
    NB. problem was too big for simple recursion, and for Floyd-Warschall kind of approaches
    NB. previous approach, starting from S and running till D would be complicated: time calculation was complicated and links were missed because when two crawlers (cur) meet midway, they both stop, and the link is not registered. Find nodes in cur that are each others neighbours, and link their hubs; afterwards they can be deleted like any other node.
0
}}
24 day {{ NB. Never Tell Me The Odds
par=: ([: (0".-.&',');._1 '@',]);._2
NB. given, list of starting positions and velocities in 3D space.
NB. part 1, disregarding all Z components, how many trajectories intersect in the future at intersection points with coordinates between 2e10 and 4e10?
NB. solve lin sys: x crosstime y returning crossing times: A*t+B = C*s+D => (D-B) %. (A,.-B) set to _1 if error so parallel lines caught by future filter.
NB.          (D-B)  %. (_1 if err)  (A,.-B)
crosstime =: (-~&{. %. :: (_1 _1"_) (,.-)&{:)
future    =: ([: *./0&<)"1@] NB. both in future?
crosspt   =: [:+/(*1,.{.)    NB. use time for left pt 
lims      =: (2e14&<: *./@:*. <:&4e14)@:crosspt
NB. intersect in 2d
NB.          mat rg and future t cross    2D    mats
int2d     =: ([ (lims  *.future) crosstime)&:(}:"1)"2
p1        =: +/@(( {. +/@:int2d }.)\.)@par
NB. testing sample code (different limits)
limst     =: (7&<:    *./@:*. <:&27)  @:crosspt
int2dt    =: ([ (limst , future) crosstime)&:(}:"1)"2
pt1       =: (( {. <@:int2dt }.)\.)@par
tst=:{{)n
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
}}
NB. Fut: 1 1 1 0 ; ? ? 0 ; ? 0; 0
NB. Lim: 1 1 0 ? ; 0 0 ? ; 0 ?; ?
NB. Part 2: where and at what velocity to throw stone to hit all hail stones at integer positions? Now with Z? Sum start location coords
NB. 3d lines can be:
NB. - intersecting: ct -: ct&:(}:"1)
NB. - parallel : _ _ _ _ -: ct,ct&:(}:"1)
NB. - none of the two: ct ~: ct&:(}:"1)
NB. new version of crosstime returning _ _ for parallel lines
ct =:  (-~&{. %. :: (_ _"_) (,.-)&{:)
ctt=: (ct ,: ct&:(}:"1))
plane =: (_ _ -: {:"2 )
NB. verify solution x with input y
verif=: -:/@:|:@:(crosstime"2) par
NB. Find lines forming planes; crossing these planes should give single line;
NB. quadratic opt.: xT Q x + cT x = 0 st Ex +b<:0
NB. amoeba? Newton Raphson? LMA ? nope!
NB. cp=: (1&|.@[ * _1&|.@]) - (_1&|.@[*1&|.@])
cp=:  1 |. ([ * 1 |. ]) - ] * 1 |. [
NB. visualise cross product, taking fretted strings
NB. e.g. ' x y z' cpvis ' a b c'
cpvis=:((1&|.@[(,&boxopen"0)_1&|.@]) ,&< (_1&|.@[(,&boxopen"0)1&|.@]))&(<;._1)
{{)n 
p0+ v0*ti = pi + vi ti => (p0-pi) + (v0-vi)*ti
ti is scalar so (p0-pi) x (v0-vi) = AxB = 3$0 (cross)
= p0 x (v0-vi) - pi x (v0 - vi) 
= p0 x v0 - p0 x vi - pi x v0 + pi x vi
 ... facepalm ... looking it up.
}}
NB.  Not my idea (reddit aoc 2024 day 24, DrCleverName):
NB. implement eq for pairs i,j of 3 hailstones:
NB.       mat +/ .* P = vec
NB. (Vi-Vj)x(Pi-Pj).P = (Vi-Vj).Pi x Pj
NB. P = ((Vi-Vj)+/ .*Pi x Pj) %. (Vi-Vj)x(Pi-Pj)
mat =: 1 cp~/@:(-/)\. ]
vec =: 1 (([:-/{:"2)+/ .*([:cp/{."2))\. ]
NB.    sum vec\mat    ext pick 3  parse
p2  =: +/@(vec %. mat)@x:@({~3?#)@par
NB. would probably also work with > 3, but 1 ...\. needs changes to keep pairs.
0
}}
25 day {{ NB. Snowverload
NB. input specifies connections between components; find3 connections to cut to divide graph in two parts. */ of sizes of components
NB. make list of edges, adding both ways, then removing them again, just to make sure
par =: [: (#~</"1)@~.@; <@([: ({.(,.,,.~)}.)@s:' ',-.&':');._2
NB. https://en.m.wikipedia.org/wiki/Stoer-Wagner_algorithm
NB. x=starting node (def 0) y=edges (single direction, list of neighbours,. weights) No neg weights, no directivity.
NB. x weights (Def=1); edges as start-end-weight
sw =: (0&$:) : {{
  'vnames E' =. (~.@, ([;i.)]) 0{::y
  EW  =. E,.1{:: y
  V   =. i. #vnames
  gr  =. V   NB. group index for each vertex in V
  gwc =. _  NB. min weight of cut-of-phase's
  gmc =. 0 $~ 1+#V NB. min cut: cut vertex + groups
  for_ph. i.<:#V do. 
    NB. pick group having specified node
    'A vrem'=. (x{gr) ({;({~ <^:3)~) ~.gr NB. arbitrary 
    NB. Mincutphase
    while. (#vrem) do. NB. any v remaining?
      new=.{.@:\:/|:A ([ ( ({.@-.~}:)"_ 1 (,+/@:({:"1))/..]) EW([#~}:"1@[~:/"1@:e.])]) vrem
      if. 0=#new do. 1+'1' end. 
      'A vrem' =. (A,new);(vrem-.new)
    end.
    NB. last two are s-t; consider as cut; lookup needed, because groups will change.
    NB. merge last element of A into previous element of A
    NB. find size of cut s-t (t with respect to A
    NB. TODO: likely possible to simplify
    wc =. {:{: (}: ([ ( ({.@-.~}:)"_ 1 (,+/@:({:"1))/..]) EW([#~}:"1@[~:/"1@:e.])]) {:) A
    echo 'phase, weight',":ph_index,wc
    if. gwc > wc do. NB. keep if better than prev
      echo 'better than ',":gwc
      gwc =. wc
      gmc =. gr,~{:A
      if. gwc=3 do. break. end.
    end.
    NB.        s   I. gr=t      srt  s-t 
    gr =. gr {.@]`(I.@:={:)`[} ts=./:~ _2{. A
    NB. update E 
    EW =. ts join EW
  end.
NB. return vertices looked up in vnames
 ((vnames #~ (}.gmc)&e.)&.> (~.@}. (];-.) {.) gmc),<gwc
}}
NB. take edge list (Nx3) of graph as y, points to be joined as x
NB. first will be kept, others replaced
join =: {{
  y =. y (#~ *:/"1) (}:"1 y) e. x NB. remove edges between merged nodes
  NB. rename  nodes in (}.x) to {.x
  y=. x ({:"1@] ,.~ [($@] $ [ {.@]`(I.@:e. }.)`[}~ ,@]) }:"1@]) y
  NB. sum unique edges
  (/:~@}:"1 (,+/)/.. {:"1) y
}}
NB.    weight  edges  -1 on nodes for ease
swt =: |.({:"1 ; }:"1) (_1 _1 0+".);._2 {{)n
1 2 2
1 5 3
2 3 3
2 5 2
2 6 2
3 4 4
3 7 2
4 7 2
4 8 2
5 6 3
6 7 1
7 8 3
}}
p1=: 2 */@:(#&>)@:{. 0 sw 1 ,&<~ par
NB. TODO: profile, make faster. solution found at it 794, but takes like 10 min. 
NB. TODO: debug sw (groups don't quite work for larger values of x), put join, sw in graph toolbox for next year
0
}}
NB. echo run 
NB. vim: ts=2 sw=2 et fdm=marker foldmarker={{,}}
