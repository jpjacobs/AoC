NB. All aoc solutions
days=: >:i.25
res =: 0 3$a:
NB. conj to execute requested solutions (not taking any arg), timing them
dod =: {{
if. n e. days do.
  tic=. 6!:1''
  dr=. u ''[y NB. y
  res=:res, dr,<(6!:1'')-tic
end.
}}

{{ NB. Day 1
NB. find two/three numbers that sum to 2020
i1=. ".;._2 freads '2020-1.txt'
NB. strategy: make addition table/cube, find coordinates of 2020, multiply them
a1=. */@:({~ ($#:(i.&2020)@,)@:(+/~))
b1=. */@:({~ ($#:(i.&2020)@,)@:(+/ [: ~. +/~))
(a1;b1)i1
}} dod 1 ''
{{ NB. Day 2
NB. How many valid passwords n-m L: w
NB. filter non-alphanum and box words
i2 =. ;: ;._2 '- : ' stringreplace freads '2020-2.txt'
between=: (1 = _0.5 0.5&+@".@[ I. ]) NB. x= range, literal, y=number
NB. valid if number of occurrances of L in w is between n and m
a2 =. [: +/ (0&{:: between ([:{.1&{::) +/@:= 2&{::)"1
NB. valid if pw at exactly one of the indexes is L
b2 =. [: +/ (([: {.1&{::) ~:/@:= ([: <:@". 0&{::) { (2&{::))"1
(a2;b2) i2
}} dod 2 ''
{{ NB. Day 3
NB. tobogan ride: how many trees on a given slope from 0,0
i3 =. '.#'&i.;._2 freads '2020-3.txt'
NB. sum index bin mat with rows,. width mod step * rownum
ntrees =. +/@:(] ({~<"1) [ (i.@{.@] ,. {:@] | [ * i.@{.@]) $@])
a3=. 3&ntrees
NB. product of ntrees for (down, right): 1 1, 1 3, 1 5, 1 7, 2 1
NB. inds takes x: step in dir 0; y: size of field
xx=. [ * {.@] i.@>.@% [
inds =. ] (],. {:@[ | i.@#@]) xx
ntreei =. +/@:(] ({~<"1) [ inds $@])
b3=. ((2 ntreei]) * [: */@:x: 1 3 5 7 ntrees"0 _ ]) 
(a3;b3) i3
}} dod 3 ''
{{ NB. Day 4
i4 =. freads '2020-4.txt'
NB. passport field validation:
f=. s: ' byr iyr eyr hgt hcl ecl pid'
NB. find colons in each LFLF seperated record
NB. take 3 preceding letters, find missing fields, if 0: complete
a4=. [: +/ [: ((LF,LF)&E. (0 = f #@-. ] s:@:{~ _3 _2 _1 +/~ ':' I.@:=]) ;. _1 ]) (LF,])

NB. validation verbs for fields below
bn=: (1 = _0.5 0.5&+@[ I. ".@]) NB. range comprises string y?
NB.   byr            iyr            eyr
vl=: (1920 2002&bn)`(2010 2020&bn)`(2020 2030&bn)
NB.   hgt in cm or inch
vl=:vl`((150 193 bn _2&}.)`(59 76 bn _2&}.)`0:@.(('cm';'in') i. [: < _2&{.))
NB.   hcl in #abcdef
vl=:vl`(('#'={.)*.('0123456789abcdef'*./@e.~}.))
NB.   ecl in amb blu brn gry grn hzl oth.
vl=:vl`(' amb blu brn gry grn hzl oth' e.~&s: ' ',])
NB. pid 9 digit number; ignore fields not found (and thus cid)
vl=:vl`((9=#)*.(Num_j_ *./@e.~ ]))`1:
NB. clean: strip redundant space, replace LF by ' ', replace space by :
cl=. (' :'&stringreplace)@deb@dltb@((' ',~LF)&stringreplace)
NB. turn record in 2xn boxes
fmt=. [: |: _2 ]\ [: <;._1 ':',cl
gaa=. {{ NB. verb to apply gerund x to y per box
NB. echo ":x;y
, x`:6 >y}}"0 
NB. Valid if: all fields present and all valid
val=. (0=f #@:-.s:@{.) , (vl {~ f (i. s:) {.) ,@:gaa {:
b4=. [: +/ [: ((LF,LF)&E. (*./@val@:fmt) ;. _1 ]) (LF,LF,}:)

(a4;b4) i4
NB. a not correct: 93, 181.
}} dod 4 ''
{{ NB. Day 5 boardingpases: binary partitioning
i5=. ];._2 freads '2020-5.txt'
NB. col rows first attempt
NB. sid1=: ('LR'&i.@:(_3&{.) (+8&*)&:#. 'FB'&i.@:(7&{.))"1
NB. far shorter: L=0, R=1; F=0; B=1
sid=. ([: #. 2|'LRFB'i.])"1
a5=. [: >./ sid
NB. missing chair between two non-missing ones
b5=. (1 0 1  >:@:(i.&1)@:E. (i. 1024) e. sid)
(a5;b5) i5
}} dod 5 ''
{{ NB. Day 6, questionaire
i6=. freads '2020-6.txt'
NB. Part A: how many questions yes for anyone in group
spl=. [:((LF,LF)&E. <;._1]) LF,LF,] NB. split groups
a6=. [: +/ #@~.@:(#~ e.&(a.{~97+i.26)) every@:spl
NB. Part B: how many yes for everyone in group
is=. (#~-.@~:)@, NB. intersection of x and y NB. alt: [-.-.
NB. }: deletes trailing new line that would ruin result because
NB. it would generate an empty personal result, removing 6
NB. overlapping questions for the last group.
b6=. [: +/ ([: (#&:>) [: is&.>/ <;._1)&>@spl@}:
(a6;b6) i6
NB. a: 6742 too low; b: 3446 < x <3500
}} dod 6 ''
{{NB. Day 7 Matroesjka bags
i7=: freads '2020-7.txt'
NB. How many kinds of bags can finally contain the shinygolden bag of Santa Claus
NB. split on contains and , and remove extraneous words, colors to symbols
spl=: (e.&(;:',contain') ;@(([: < (_2}.]) , _2 <@s:@<@;@{. ])@}:  ;._1)])@;:@(','&,)
NB. Now every line is made up as: parent col;n;child col;...
cols=: <@((s:<'noother')-.~])@;@(#~ 2 < 3!:0&>)@spl;._2 NB. only lists of colors
NB. work from the bottom up: lookup possible containers for prev. layer.
ir =: }. ,. {. NB. inverse record: x lines of child,parent
NB. given parent-child list, make child-parent list
NB. added top level bags as referring to self for ending iteration
cp =: ([ ,"1 {:~.@-.{.)@|:@;@:(ir&.>)@cols
NB. iterate: look up parent of all found bags until no more change and add to list (roots refer to self and therefore do not change). keep sorted and nub to avoid sorting from stopping convergence.
NB. decrement because shinygold bag part is also in the list
a7 =: [: <:@# (s:' shinygold') (([: /:~@~. ],{:@[ {~ {.@[ I.@:e. ])^:_)~ cp

NB. How many bags should the shiny gold bag contain in total?
NB. recursive function approach:
NB. colors with counts: first parent, then (num,child) pairs
colsc=:[: (#~ ((<s:<'noother') -.@e. ])&>) <@spl;._2
b7=. {{
  NB. split parents (n symbols), children and colors (both n boxes)
  'par chd num'=.(>@:{.&> ; ([: > _2 {:\ }.)&.>,&<(_2".@;@{.\ }.)&.>) colsc y
  NB. recursive count,counting childs by dot product of numbers and contained bags
  count=. ({&num (+/ .* >:@$:)&:> {&chd)@:(par&i.)`(0"0)@.(-.@e.&par)"0
  count@:((s:<'shinygold')"_) y
}}
(a7;b7) i7
}} dod 7 ''
{{ NB. Day 8 boot code execution
i8=: ];._2 freads '2020-8.txt'
NB. execute until done
coclass 'gameboy'
ld=:{{                  NB. load prog & reset visited instructions
  vis=: 0 #~ >: # pr=:y NB. visited, 1 more for last iteration
  i=: 0                 NB. instruction pointer
  reg=: 0               NB. register
}} 
NB. create gameboy instance, load prog if given.
create=: {{if. #y do. ld y end. }} 
NB. instruction names should NOT be changed since prog is
NB. executed calling them directly
jmp =: {{ NB. jump to instruction, registers visited instructions
  vis=: (>:i{vis) i}vis
  i=: i + y 
  i NB. return i to keep ^:_ in run going
}}
nop =: jmp@1:              NB. advance to next instruction 
acc =: {{nop reg=: reg+y}} NB. accumulate and go to next instr.
step=: {{".i{pr}}          NB. run current line
NB. return reg after iterating step while pointer in bounds and not instruction not executed yet.
run =: {{reg}}@:(step^:{{(i<#pr)*. (i >: 0)*.1>i{vis}}^:_)
cocurrent 'base'

g=: i8 conew 'gameboy'
a8=: run__g
NB. part B: find nop or jmp to be changed to cause prog to jump
NB. to instruction #pr 
NB. swap jmps for nops in y
swap=: [: ,"2 {{ select. 3{.y case. 'nop' do. 'jmp',3}.y case. 'jmp' do. 'nop',3}.y end.}}"1
NB. for each line containing (E.) nop or jmp, execute swapped version 
b8=: {{
  gg=. conew 'gameboy'
  NB. all swp,jmp locs
  swl =: I. +./ (2 3$'nopjmp') +./@:E."1/ y
  NB. swapped input (nop<-> jmp) for each occurrence
  swi =: swap i8
  NB. run over all swap locations
  for_loc. swl do.
    ld__gg (loc{swi) loc}i8
    ret=: run__gg ''
    NB. whenever run returns, pr repeats or ip out of [0,<:#pr]
    NB. if ip is the instruction after the bootcode, we've found
    NB. the error
    if. i__gg=#pr__gg do. break. end.
  end.
  ret
}}
(a8;b8) i8
}} dod 8 ''
{{ NB. Day 9 xmas cipher
NB. Find first number from index 25 that is not a sum of 2 different preceding numbers
i9=. (".@,&'x');._2 freads '2020-9.txt'
NB. sums of different pairs of first x items of y
sums=.(+/"1@:~.@:(#~ ~:/"1)@:(/:~"1)@:(,/)@:>@:{@,~@:<@:{.)
NB. correct if
NB. could be wildly more efficient, since recalculates all sums for each 25 infix, but good enough.
a9=. {~ 25+0 i.~ 26 & (({: e. <:@# ([sums{.) ])\ )
r9=. a9 i9
b9=. r9&{{
for_ss. i.&.(<:@<:) #y do.
  tot=. ss +/\ y
  ind=. x (i.&1)@:= tot 
  if. ind<#tot do.
    (<./+>./) (ind+i.ss){y
    return.
  end.
end.}}
NB. r9 to not repeat execution of a9
r9 ;b9 i9
}} dod 9 ''
{{ NB. Day 10: power adapter problems
NB. power adapter outputs number with input > output-3
NB. device input joltage= highest adapter output +3
i10 =: ".;._2 freads '2020-10.txt'
NB. silly try: sort count differences; happens to be correct
a10 =: */@(2&{.)@([: #/.~ 1 3, 2 -~/\ /:~)

NB. B: how many unique ways can they be combined and all used?
com  =: 0, /:~ , 3+>./       NB. complete data with device input
NB. connection graph connects inputs with possible outputs
con  =: [: (0&< *. <:&3) -/~ 
NB. turns out sorting per number *is* a topological sort:
assert *./ </ ($ |:@:#: I.@,) |: con com i10

b10=: {{
adj=. <@I. con com y NB. adjacency list
NB. reverse depth first traversal counting paths, starting from last node ("device")
NB. path to 0 is 1 per definition
NB. debugging version
NB. ctposdb=:([: +/ [: $:"0 ((echo@;&({&(com y))]]) {::&adj))`(1x"_)@.(0=])
ctpos=.([: +/ [: $:"0 {::&adj)`(1x"_)@.(0=])M.
ctpos <:$adj
}}
NB. start finding possibilities 
(a10;b10) i10
}} dod 10 ''
{{ NB. Day 11 life on a grid
i11=: ];._2 freads '2020-11.txt'
NB. test input
t11=: ];._2 ] {{)n
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
}}
pad   =: 0&([, [,~ [ ,. ,.~ ) :. ([: }."1@}. }:"1@}:)"2 NB. 0 padding and unpadding
NB. shift returning central square and 8 neighbours
shift =: (,@{@,~&.< 0 1 _1)&(|.!.0)"1 _&.pad
NB. alive if none around, dead if more than four neigbours, 5 in total.
rule  =: {. + (0=+/) - ({.*. 4<:+/@}.)
NB. 'L'=empty=0, '#'=occupied=1; '.'=no seat=2. Seats are filtered
NB. by 2&~:@] (0 1 2-> 1 1 0) taken pos filtered by 2&| (0 1 2->0
NB. 1 0)
NB. * hook to account for grid of seats (no seat is zeroed out irrespective of neighbours)
a11=: [: +/@, (2&~:@] (* rule@:shift)^:_ (2&|))@('L#.'&i.)

NB. entirely different:
NB. number to consider is first seat seen in any (8) direction
NB. free seats obscure taken seats behind them.
b11 =: {{
NB. to matrix of indices indicating seats (!starting from seat 1)
mi=: ($$(* +/\)@,)@('.L'&i.)
NB. indexed seats to their coordinates
tc=: ($#:I.@:,@:*)
NB. dup8 duplicates coordinates 8 times, for each direction
dup8=: 8&(-@[ ]\ #"2) NB. global version; res: nx8x2
NB. directions in which to seek neighbours (removed 0,0)
dirs=: }. ,@{@,~&.< 0 _1 1

NB. dir +"1 co +"1/ dir NB. dir: 8x2, co nx2 no dup8 needed
NB. inbound (nx8) *"1 2 dirs (8x2) checks whether current coordinate + dirs are within bounds
NB. ib1 variation of inbound for first non-conditional iteration on nx2 (i.e. the seat positions  no dirs initially added)
NB. ib2 works on nx8x2, i.e. the neighbours' positions.
ib1=: [: (0 0&<: *./@:*. <:&($y))"1 dirs +"1/~ ]
ib2=: [: (0 0&<: *./@:*. <:&($y))"1 dirs +"2   ]

NB. global iterations (i.e. all seats at the same time) for finding visible neighbours of each seat
itg=: (] +"1 dirs *"2 1 (ib2 *. -.@:(e."1 _~)))^:_ (] +"1 dirs *"2 1 ib1)

NB.              note: -----v 1 based indices in result
neigh=: [: (-."1 0 >:@i.@#) (,@] {~ $ #. itg@tc)@pad
NB.          ^ replace (by pad) self by 0
NB. now that all set: iterate over states, look up number of seats in view, apply ruleb (to y nx8 binary list indicating occupied neighbors and x current state)
NB. ruleb: current state + if empty& 0 neigh, -if full& >: 5.
ruleb=: [ + [ ((+: 0&~:) - (*. >:&5)) (+/"1)@]
NB. pad array with 0 for 1 based indices, extra 0's don't influence ruleb
+/ (neigh (] ruleb [ { 0 , ])^:_ (0$~>./@,) )@mi y
}}
(a11;b11) i11
}} dod 11 ''
{{ NB. Day 12 navigation
i12=. ];._2 freads '2020-12.txt'

 NB. manhattan distance: simple sum of all NESW directions + running sum (sum of directions * dist)
nav=. {{
P=. 1 2$0 0
H=. 0
for_step. y do.
  select. {.step
  case. 'N' do. inc=. 0 1
  case. 'S' do. inc=. 0 _1
  case. 'E' do. inc=. 1 0
  case. 'W' do. inc=. _1 0
  case. 'L' do. inc=.0 0[H=. 2p1|H+1r180p1*".}.step
  case. 'R' do. inc=.0 0[H=. 2p1|H-1r180p1*".}.step
  case. 'F' do. inc=. (*|@*)2 1 o. H
  end.
  P=. P, ({:P)+inc*".}.step
end.
P
}}
a12=. +/@:|@:{:@nav
rotmat=: 2 2$1 _1 1 1*2 1 1 2 o.]
navb=. {{
P=. 1 2$0 0x NB. ship location
W=. 10 1   NB. waypoint relative to ship
for_step. y do.
  par=. ".}.step
  select. {.step
  case. 'N' do. W=.W+0  1*par
  case. 'S' do. W=.W+0 _1*par
  case. 'E' do. W=.W+ 1 0*par
  case. 'W' do. W=.W+_1 0*par
  case. 'L' do. W=. (rotmat 1r180p1 * par) +/ .* W
  case. 'R' do. W=. (rotmat 1r180p1 *-par) +/ .* W
  case. 'F' do. P=. P, ({:P)+W*par
  end.
end.
P
}}
b12=. +/@:|@:{:@navb
(a12;b12) i12
}} dod 12 ''
{{ NB. Day 13 shuttle bus arrivals
i13=. freads '2020-13.txt'
NB. prepare: turn input into simple list.
prep=. [: ; <@".@('x , '&stringreplace);._2
NB. determine waiting times 
awt=. (}.([,.([-|)){.) NB. M: y=prepped inp; returns id,.wait
NB. multiply result after selecting id,wait where wait is min
a13=. */@:({~ 1 i.~ [:(=<./){:"1)@awt@prep
NB. subsequent departures
prepb=: ((#~ ,: I.@]) *)@}.@;@:(<@".;._2)@('x0'&stringreplace)
NB. find modulo and offset for each departure
mo=: ({. ,. {. | -@{:)
NB. not needed... check solution in x with modulo,: offset in y
ch=: [: *./ ({:@] = (|~ {.) |:)
NB. Let's try iterative sieving: start with list sorted by modulo high-low, iterate: combine current modulo and offset with next modulo and offset by generating a list of candidates satisfying both. When found, set current modulo to product, and treat next on the list, until the list is empty.
b13=: {{
  todo=. (\: {."1) x: mo prepb y
  'm1 o1'=. {. todo NB. current mod and offset
  todo=. }. todo    NB. to be treated
  whilst. #todo do.
    'm2 o2'=. {.todo NB. next pair of modulo and offset to treat
    NB. find first number where new offset mod m2 equals o2 in
    NB. list of candidates starting from offset.
    o1 =. ({~ 1 i.~ o2=m2|])o1+m1*i. m2
    m1 =. m1*.m2 NB. accumulate moduli
    todo=.}.todo NB. remove done pair from todo
  end.
o1
}}
(a13;b13)i13
}} dod 13 ''
{{ NB. day 14: masked 36bit memory initialisation.
i14=: freads '2020-14.txt'
NB. A: written using bitmask, what's the sum of all memory
NB. 36 bit addresses, so lookup table with binary addresses to be
NB. updated. Split mask in f1 and f0 for use with <. and >.
a14=:{{
mav=: 0 37$0x NB. mem address-value pairs
mask=: {{f0 * f1 >. (36#2)#: x: y}}
NB. mem is function taking address, val as y
NB. prepends to mem and use nubsieve to keep most recent
NB. Don't rename mem as lines are executed with ". calling mem
mem=: {{mav=: (#~ ~:@:({."1)) mav,~({. , mask@{:) y}}
inst=: ];._2 ']x[ ' stringreplace y-.'='
for_line. inst do.
  if. 'a'=1{line do. NB. set masks f0 and f1 as extended
    'f1 f0'=:('1'&= ; ~:&'0') 6}.line
  elseif. 'e'=1{line do. NB. set mem
    ".'x',~dtb line
  else. echo 'unexpected input'
  end.
end.
+/ #. }."1 mav
}}
b14=:{{
mavb=: 0 2$0x NB. mem address-value pairs
NB. mask where X should make comination of all possibilities
NB. where 0 no change, where 1 pin to 1.
mask=: {{#. >@,@{ (0x;1x;0x 1x) {~(+:f=2)>.(f=1)>.(36#2)#:y}}
NB. mem is function taking address, val as y
NB. prepends to mem and use nubsieve to keep most recent
mem=: {{mavb=:  mavb,(mask@{. ,. {:) y}}
inst=: ];._2 ']x[ ' stringreplace y-.'='
for_line. inst do.
  if. 'a'=1{line do. NB. set masks f0 and f1 as extended
    f=:'01X' i. 6}.line
  elseif. 'e'=1{line do. NB. set mem
    ".'x',~dtb line
  else. echo 'unexpected input'
  end.
end.
+/ #. (}."1 #~ ~:@:({."1)) |. mavb
}}
(a14;b14) i14
}} dod 14 ''
{{ NB. day 15 elves game
i15=: ". '6,3,15,13,1,0'
NB. a= 2020'th number in the series
NB. l =: (],(  (~:*<:@-~) #  )~ (}:i:{:))
NB.          i      #l
NB. note: if not found: then index by i: (in shortened list) = #l
a15=:[: {:  (],](<:@-~ #)~ (}:i:{:))^:(2020-#)
bbb15=:[: {:  (],](<:@-~ #)~ (}:i:{:))@]^:(-#)
NB. b=30,000,000'th number: too many for keeping list.
bb15=: {{ NB. better than above, but still too slow
a=. x $0 NB. _1 cannot be generated
a=. y (i.#y)}a
for_i. i.&.(-&(#y)) x do. NB. i is 'length' and next index
  up=. ((<:i) ([-{.i:{) a)
  a=. up  i}a
end.
{:a
}}
b15=: {{ NB. slow, but feasible, runs in ~2 min
  a=. x $_1 NB. index=number, value=turn used
  a=. (i.<:#y) (}:y)}a NB. all but last number
  last=. {:y NB. last number
  for_i.  i.@<:&.(-&(<:#y)) x do. NB. i index to store
    NB. faster than when no if, and multiplication with condition
    if. _1 = lastused=. last{a do. 
      a=. i last}a
      last=. 0
    else.
      a=. i last}a
      last=. i-lastused
    end.
  NB.   if. 0=1000000|i do. NB. reporting takes extra time
  NB.     echo i
  NB.   end.
  end.
  last
}}
(a15;30000000&b15) i15
}} dod 15 ''
{{  NB. day 16: ticket translation
i16=.<;._2 freads'2020-16.txt'
NB. A: sum invalid fields in all tickets that satisfy no single rule.

a16=.{{
NB. parse rules, and join all ranges
rules=. 20{.y
to=. {{ m+i.>:n-m}} NB. conj so applied before or (,)
or=. ,
NB. valid ranges, merged
rg=. ~. ; ".@(('-';' to ')stringreplace])@}.@(':'dropto]) each rules
NB. parse tickets
t=. ".&.> 25}.y
+/ +/@(#~ -.@e.&rg)&> t NB. sum values not in allowed vals.
}}

b16=:{{
NB. parse rules, and join all ranges
rules=. 20{.y
to=. {{ m+i.>:n-m}} NB. conj so applied before ,
or=. ,
NB. fieldnames
fn =. ':'&taketo &.> rules
NB. valid ranges, merged
rgs=. ".@(('-';' to ')stringreplace])@}.@(':'dropto]) each rules
rg=. ~.;rgs NB. joint rules

NB. parse tickets
t=. ".&.> 25}.y
NB. discard entirely invalid tickets (remaining array rectangular)
t=.<"1|: > (#~ *./@(e.&rg)&>) t NB. sum values not in allowed vals.
NB. find field permutation that can fit the data
mat=. t *./@e.&>/ rgs NB. contains costs: 0 if fitting
NB. matrix to permutation: Hungarian algo: overkill
NB. assign rules (and thus names) to fields
NB. full generic case overkill, since there is one field that can have only 1 rule, and rest likely to follow since:
NB. (i. # rgs)-: <:/:~ +/mat

NB. consider tickets fixed, and find permutation of rules
ptf=. (/: +/"1 mat)&{ NB. perm to put ticket #ields in order of increasing possibilities
pfn=. >((i.#fn) \:"1 +/)&.ptf mat NB. perm of fn and rgs 
NB. test correctness
NB. (pfn { rgs) *./@e.~&> <"1|:>t
myt=. ".>22{y NB. my ticket
NB. product of values for fields of my ticket where departure is in the permuted names
*/x: myt#~'departure' +./@:E."1 >pfn{fn 
}}
(a16;b16) i16
}} dod 16 ''
{{ NB. day 17 conway cubes
i17=. ,:'.#'i. ];._2 freads '2020-17.txt'
NB. how many active cells after 6 turns?
NB. pad if needed along any dimension
padc=. (1|.i.@#@$) ([|:padx@])^:(#@$@]) ] NB. after 3 it's back in orig orien
NB. conditional padding on first axis (if 1's present on a side)
padx=. [: 0&,^:(1 e. ,@{.) ,&0^:(1 e. ,@{:)
NB. pad uncond
pad=. ((1|.i.@#@$) ([|: 0 , 0 ,~ ])^:(#@$@]) ])

NB. rule: left selfs XxYxZ, right: neighbours: XxYxZx26
NB. rules: 1 to 1 if 2 or 3 neigh; 0 to 1 if 3 neigh; else 0
rule=. [ ((-.@[ *. 3&=@]) +.( *. (=&2 +. =&3))) +/"1@]
self=. 13&{"1
nb=. (<<<13)&{"1
it=. ([: (self rule nb)  (1 1 1,:3 3 3) ,;._3 pad)@:padc 

a17=. [: +/@, it^:6
NB. B: same, in 4 D:
selfb=. 40&{"1
nbb=. (<<<40)&{"1
itb=. ([: (selfb rule nbb)  (1 1 1 1,:3 3 3 3) ,;._3 pad)@:padc 
b17=.[: +/@, itb^:6@,:
(a17;b17)i17
}}dod 17 ''
{{ NB. Day 18 left-to-right maths
i18=. ];._2 freads '2020-18.txt'
NB. left-to-right exactly opposite to J, so perform on flipped
NB. input, taking care of inversed parentheses
a18=.[: +/@:x:@:". '())(' |.&.;:@stringreplace"1 ]
NB. Part B: + has precendence:use conj to force precedence over *
plus=. {{m+n}}
b18=. a18@:(rplc&('+';'plus')"1)
(a18;b18)i18
}} dod 18 ''
{{ NB. Day 19: message decoding rules
i19=: ];._2 freads '2020-19.txt'
NB. load and sort rules s.t. rule num = row num
rules=: ((2}.':'&dropto)"1 /: ".@(':'&taketo)"1)  dtb"1] 138{.i19 
msgs =: <@dtb"1] 139}.i19
NB. test input for part b, adapted so works with contiguous array
NB. of rules, requiring consecutive numbering
t  =: ];._2 (freads'2020-t19.txt') rplc'31';'29';'42';'30'
NB. test rulee
rt =: ((2}.':'&dropto)"1 /: ".@(':'&taketo)"1) dtb"1] 31{.t
NB. test messages
mt =: <@dtb"1] 32}.t

NB. replace number(str, y) with rule (x), returns parenthesised
NB. regex
rep=: <@[ ('(',')',~ '"'-.~ (dtb@{~ ".))&.> ]

NB. br: build regex for rules (y) using replacement ft u.
br=.{{
  num=.rxcomp '[0-9][0-9]*' NB. compiled regex for numbers
  reg=. dtb {.y             NB. starting regex is rule 0: {.y
  NB. while there are numbers:
  while. #matches=: num&rxmatches reg do.
    nums=. matches rxfrom reg              NB. extract numbers
    NB. replace numbers with rules using rep
    reg=. (y rep nums) matches rxmerge reg
  end.
  NB. remove (some) unnecessary parentheses
  ('(a)';'a';'(b)';'b') rplc~^:_ ' '-.~reg
}}

NB. attempt: build regex-monster from rules, check which messages
NB. fit entire regex
a19=: +/@( rxcomp@br@[ rxeq every ])
NB. B: replaced rules 8 and 11 are self-referent
NB. try recursive patters with named capture groups
NB. http://www.pcre.org/current/doc/html/pcre2pattern.html#SEC25
NB. add special case for 8 and 11

NB. modified build regex starting from handcrafted version for
NB. rule 0 for simplicity
brb=.{{NB. adverb: m is test or not
  num=.rxcomp '[0-9][0-9]*' NB. compiled regex for numbers
  if. m do.
    NB. changed starting regex for (changed) test input.
    reg=. '\A(?<AA>30|30(?&AA))(?<BB>30 29|30(?&BB)29)\z'
  else. 
    NB. recursive starting regex for rules 0: 8 11
    reg=. '\A(?<AA>42|42(?&AA))(?<BB>42 31|42(?&BB)31)\z'
  end.
  NB. while numbers are present in regex, replace them by rules
  while. #matches=. num&rxmatches reg do. 
    nums=. matches rxfrom reg
    reg=. (y rep nums) matches rxmerge reg
  end.
  ('(a)';'a';'(b)';'b') rplc~^:_ ' '-.~reg
}}
b19=: +/@( rxcomp@(0 brb)@[ rxeq every ])
rules (a19;b19) msgs
}} dod 19 ''
{{ NB. Day 20 tile alignment
i20=.(LF,LF)(E.<@(];._2);.2]) LF,freads'2020-20.txt'
'ids tiles'=.(([: ".@}: ' 'dropto 1{])&>;('.#'i.2}.])&>) i20
NB. A: multiply id's of corners
NB. make edge id's by #. on each edge and flipped edges(8).
NB. borders match no other edges, so corner tiles have 4 unique
NB. edges

NB. sig tiles: return nx8 edge matrix as numbers,order L,T,R,B
NB. 2|.[:|. to ensure that edges are in the correct
NB. order(clockwise from left edge) when a tile is transposed:
NB. original: 0 1 2 3 to transposed 1 0 3 2
sig=: ([:#.@(,2|.[:|.|."1)|.@:({."1),{.,{:"1,:|.@{:)"2 

NB. corners have 2 sides (4 sigs due to flip) that match others
a20=.[: */@:x: (#~ (4= [+/@:e. ~.@,@-.~)"1 _~@:sig)

NB. part B: complete puzzle, find sea monsters:
NB.                   # 
NB. #    ##    ##    ###
NB.  #  #  #  #  #  #   
monster=.' #'i.];._2 {{)n
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   
}}
NB. find how many ones outside seamonsters, after needed flip
NB. and/or rotation of the entire map
NB. for tile edges: 2 4$"1 sig, restrict to only one side of tile
NB. tile composition: ({.,#)/.~([ +/@:e. ~.@,@-.~)"1 _~@:sig tiles
NB. 4 corners, 10 edges, 100 inside tiles
NB. start with 1 corner in fixed orientation
NB. fit edges and other corners, requiring edge to remain outside.
b20=:monster&{{
  NB. y: tiles assumptions: tile square, grid square, edges unique
  NB. edges per tiles std/flip
  edges=. 2 4$"1 sig y NB. edge signatures
  NB. inner tile edges (matched) are pairwise unique:
  assert. (# (=+:) #@~.)e=:(] #~&, ([ e. ~.@,@-.~)"_1 _~) edges
  NB. this makes matching trivial, because there's only 1
  NB. possible match for each piece.
  NB. from the size of e(1056), it's also clear the tiles are
  NB. layed out 12x12:
  NB. number of matched edges (#e) is 2*(4*12)-~4*144 only if
  NB. 12x12.
  side=. %: #y

  NB. non-borders to be matched in each tile [binary mask] for sig
  nonborder=. ([ e. ~.@,@-.~)"_1 _~ edges
  rot =. |.@|:@]^:(4|[) "0 2 NB. rotate tile y x times to the left
  flip=. |:@]^:["0 2         NB. flip tile y if x
  NB. set up arrays to keep result
  NB. ========================================
  NB. sel: indexes of selected tiles, init index of first corner,
  NB. having 2 non-borders (x2=4)
  sel  =. 4 i.~ +/@,"_1 nonborder
  NB. required rotations (+ is left) for selected tile
  NB. initially s.t. found corner is top left, i.e. left and top
  NB. nonborders
  rota =. ,:(0 0 i.&1@:E. ,~) {. sel { nonborder
  NB. edge(of the new tile) to be matched in next iteration
  em   =. ,: rota |. {. sel {edges
  fl   =. 0 NB. define first orientation of first corner as up.
  edges =. _1 sel}edges NB. zap for avoiding future matches
  NB. sig, flip, rot all work
  for_t. }. i. #y  do. NB. }. because first corner already found
    NB. if first tile of row: find bottom of first of prev row
    if. 0=side|t do. find=. 3{(-side){em
    NB. else: find right edge of last of current row
    else.            find=. 2{  _1   {em
    end.
    NB. update find to find mirror image of edge represented as
    NB. base-2 number
    find=. |.&.((2#~1{$y)&#:) find
    NB. new tile id, flip, edge from found edge in signatures
    'newti newfl newedi'=. ($#:i.&1@,)find=edges
    NB. get required rotation, taking into account 0=side|t ==> 1
    NB. rotation less because matching edge is top edge instead
    NB. of left
    newrot =. newedi - (0=side|t)
    newedge=.(<newti, newfl){edges
    NB. add to result arrays
    sel  =. sel ,newti  NB. add to selected tiles
    fl   =. fl  ,newfl  NB. add flip required or not to flips
    rota =. rota,newrot NB. add rotation to rota
    em   =. em  ,newrot|.newedge NB. add rotated edges to edges to be matched

    NB. zap edges of found tile avoiding further matches
    edges=. _1 newti}edges
  end.
  assert. y =&# ~. sel NB. all tiles used
  NB. verbs to piece together puzzle:
  NB. ========================================
  NB. A B  compose nxHxW converts y to AH x BW image 
  compose=. ([: ,/&:> ,.&.:>/"1)@($ <"2)
  NB. remove borders of tiles
  rembord=. (}.@}:"1 @: }.@}:)"2
  NB. recompose entire image by: compose; rotation; flip; select;
  NB. remborder using found parameters
  image  =.(,~side) compose rota rot fl flip sel{rembord y

  findmon=.((1 1,:$x) ((+/,x)=[:+/@, x*]);._3 ])"2
  NB. count monsters in all rotated flipped versions
  count =. >./ +/@,@:findmon ,/0 1 2 3 rot/ 0 1 flip image  
  NB. count sea minus monsters (assumes no overlapping monsters)
  (+/,image)-count*+/,x
}}
ids (a20;b20@]) tiles
}} dod 20 '' 
{{ NB. Day 21
i21=. <@s:@;: ;._2 '(),'-.~freads'2020-21.txt'
i=.     (s:' contains')&taketo &.> i21 NB. ingredients per food
a=. }.@((s:' contains')&dropto)&.> i21 NB. allergens per food
NB. set intersection
sis=. [ -. -. NB. in set notation x and y sets: x/ x/y 
NB. nub in which box: monad:y is allergens per food indicate which of the
NB. nub of allergens razed is in which food (#~.;a)x(#a) array 
niwb=. ((e.>)"0/~ ~.@;)
NB. dyad pos: x ingredients per food, y allergens per food
NB. possible ingredients for each allergen is the intersection between all
NB. sets of ingredients (foods) where the allergen is contained in.
pos=. sis&.>/@:#~"1 niwb
NB. solveit: solve iteratively assigning values, x,y=a, posible ingredients
solveit =. {{
  x=. ~.;x  NB. unique allergens
  NB. start point: allergen wit 1 pos
  found=. x ,:&((ind=:(1 i.~ #&> y))&{::) y
  an=. (<<<ind){x
  po=. (<<<ind){y -.&.> ind{y NB. remove recognized allergen from pos lists
  while. y >&# {.found do.    NB. as long as not all allergens matched
    found=. found ,.  an ,:&((ind=:(1 i.~ #&> po))&{::) po NB. start point
    an=. (<<<ind){an
    po=. (<<<ind){po -.&.>  ind{po
  end.
  found
}}
a21=. ;@[ #@-. [: {: ] solveit pos
NB. sorted, comma separated list of allergen containing ingredients
NB. note that symbols are sorted alphabetically by /:
b21=. [: }:[: ;@:(,&','&.>) 5 s: [: ({: /: {.) ] solveit pos
i (a21;b21) a
}} dod 21 ''
{{ NB. Day 22 combat cardgame
i22 =. _25 <\ ". (<<<0 26 27){];._2 freads'2020-22.txt' NB. P1 ,: P2
win1=. ({.&>      ,~ [:}.0{::]); (}.&>@{:) NB. player 1 wins
win2=. (|.@:({.&>),~ [:}.1{::]);~(}.&>@{.) NB. player 2 wins
wpl =. ;@#~ 0~:#&>                         NB. select, raze winning player  
score=:  +/@:* >:@i.@-@#                   NB. score winning deck
round=. win1`win2@.(<&{.&>/)^:(0 -.@e. #&>)NB. one round
a22  =. [: score@wpl round^:_ NB. whole game:rounds till done

NB. Part B: recursive combat:
NB. if both players have at least as many additonal cards as on the first,
NB. recurse into playing a new game
NB. recursion step: show winner of last round of game on selected cards
NB. adapt winning condintion: winner is non empty one,or first if both
NB. non-empty. both empty shouldn't happen, but 1 would win.
b22 =. {{
  records=: 2000$s:a:   NB. array of symbols representing game states
  outcome=: 2000   $ 0  NB. outcomes of decided games
  NB. Implemented below to update outcome only when game known so that it
  NB. can be cut short. Stores signed score: <0 means player 0 won, >0
  NB. means player 1 won.
  recnxt =: 0           NB. next index for writing records
  rc=:{{ NB. recursive combat
    NB. dyad: x:pl 0, y pl 1. returns deckswith trailing frets.
    rnds=. ''           NB. indices into records of rnds played in cur game 
    while. x *.&# y do. NB. until out of cards or break, do rounds
      NB. make hash and find in records; if round known globally, get score
      if. (#records)>i=.records i. hash=. s:<": x,0,y do.
        NB. if not registered yet due to recursion, sc will be 0
        NB. indicating a win for x
        sc=. i{outcome
        NB. if round in this game (repeat): end game, x wins
        if. i e. rnds do.
          sc=. -score x
          break.
        elseif. 0=*sc do. NB. score hasn't been computed yet
          NB. Should not produce: all subgames have less cards
          NB. so cannot be the same. same round in same game
          NB. should have ended above
          echo 'score not set, but hash found'
        else.
          rnds=. rnds,i NB. add found round to current game
          break.
        end.
      else. NB. not found so run appropriate action 
        NB. recurse if both have more cards than their first.
        if. x *.&(#>{.) y do.
          NB. new deck for subgame for players x and y
          sx=.({.{.}.)x 
          sy=.({.{.}.)y
          if. sx ([e.~>./@,) sy do. NB. if x has highest card, he will win.
            won=. _1 NB. if endless, x wins, but x never gives highest card.
          else.
            won=. * sx rc sy NB. sub-game with sub-decks
          end.
        else. NB. normal, non-recursive game
          won=. * x -~&{. y
        end.
        NB. do card exchange based on who won
        if. won<0 do. NB. neg -> x won
          x=. y ({.@[ ,~ 1|.]) x
          y=. }. y
        else. NB. pos -> y won
          y=. x ({.@[ ,~ 1|.]) y
          x=. }. x
        end.
        NB. do round accounting
        records =: hash recnxt}records NB. register hash in record
        rnds    =. rnds, recnxt     NB. register index in rnds of this game
        recnxt  =: >:recnxt         NB. prepare for next round...
        NB. if needed: extend cache arrays
        if. recnxt=#records do.
          records=: records,2000   $s:a:
          outcome=: outcome,2000   $ 0
        end.
        if. x +.&(0=#) y do.        NB. unless there's a winner, continue
          NB. x or y is empty anyhow. - for convention
          sc=. score y,-x
          break.
        end.
      end.
    end.
    NB. game done, register final score for all rounds, s.t. any round will
    NB. cause entire game to be ended irrespective of which roundeof the
    NB. game is foound.
    outcome=: sc rnds}outcome NB. register score for all rounds
    sc
  }}
  | rc&>/ y NB. score of recursive combat between boxed list of decks
}}
(a22;b22)i22
}} dod 22 ''
{{ NB. Day 23 crab cup game
NB. take 3 from current (+), put them to the right of the current number. if within 3, decrement (wrapping) until not
i23=. <:"."0 deb }: freads'2020-23.txt' NB. decrement for easy perms
perm=. ". ;._2 {{)n
4 1 2 3 5 6 7 8 0 NB.put after next cup (a#ter current cannot happen)
4 5 1 2 3 6 7 8 0
4 5 6 1 2 3 7 8 0
4 5 6 7 1 2 3 8 0
4 5 6 7 8 1 2 3 0
}}
NB. destination s.t. num not in picked: monad y: cups from current
dest =. (9|<:)@{. ([([{~6|I.)~ (i. _9) -. ]) 3{.}.
NB. one turn
turn =. {~ perm {~ _4+(i. dest) 
NB. how to collect the cups
collect =. }.@(|.~1 i.~]) NB. rotate so 1 is first, behead
NB. convert to base 10 num the collected cups after 100 turns (accounting for decrement.
a23=.  10 #: inv [: collect@:>: turn^:100  

NB. Part B: 1e6 cups, get two first after 1, after 10e 6 turns
NB. try linked list with cupnum being index, and next val
NB. newll: new linked list given input (y) and length (x) s. t. next=curr{ll
newll=: [: (1&|. /: ])  ([`(i.@#@[)`]} i.)~
NB. million mod decrement
mmd=: 1000000|<:@]
b23=:{{ NB. do all rounds, note, SLOW, takes ~3 min.
  NB. next: helper adverb: 
  NB. ll n next y : gets n next items from ll starting at y
  next=. ((] , ({~ {:))^:)(}.@)
  NB. nextfour=. 4 next NB. funnily enough, this actually slows down by 40s
  len=. 1e6
  ll=. len newll y                   NB. initialise linked list
  cur=. {. y                         NB. set current element
  for_i. i. 1e7 do.                  NB. do 10 milion rounds  
    NB. echo ^:(0=100000|]) i
    next4=. ll 4 next cur          NB. 4 elements after cur
    NB. next dest: mod decr while dest in pick , start from mod decr cur
    d=. (len|<:)^:((}:next4) e.~ ])^:_ len|<: cur
    NB. have to update at ("where"): 
    NB.     current cup   ; tail of picked cups; destination
    NB.   = cur           ;  2{next4           ; mmd current cup
    where =. cur          , (2{next4)          ,d
    NB. to values ("what") :
    NB.     val after pick; next after dest    ; first of pick
    NB.   = 3{next4       ; next dest          ; 0{next4
    what  =.({:next4)     , (d{ll)             , {.next4
    ll    =. what where} ll            NB. update linked list
    cur   =. cur{ll                    NB. set new current
  end.
  */ >: x: ll 2 next 0
}}
NB. santa's diagnostic little helper: rec: reconstructs actual list
NB. see https://code.jsoftware.com/wiki/Doc/Articles/Play131
NB. works because circular linked list with all el connected
rec=:([ (i.~ |. ]) ;@C.@])
(a23;b23)i23
}} dod 23 ''
{{ NB. Day 24 hex tile pattern on infinite hex grid
i24=: freads'2020-24.txt'
NB. flip tiles in input, indicated with e ne nw w sw se from ref (0 0).
NB. count how many black.
NB. strategy: parse; convert to complex num, sum, (2|#)/.~
NB. parse using fsm: classes any;'ns';'ew'
NB. state table
st=: +. }. ".;._2 {{)n
'any ns  ew   ']0
 0j0 2j1 1j1 NB. initial: nothing gotten, j=_1
 0j3 2j2 1j2 NB. got e/w
 0j0 2j1 1j0 NB. got n/s
}}
map=: <(,~ a.<@-.;) 'ns';'ew'   NB. character classes
parse=: (0;st;map)&;:           NB. verb for parsing a line
ccl=: (*|@*)&.+.                NB. clean near-0 from cplx num
dirs=: ccl ^ 0j1r3p1*i.6        NB. (noun) cplx hex dirs, cleaned.
dn=: <;._1 ' w nw ne e se sw'   NB. (noun) dir names, in order of dirs
NB. monad, for line, return tile position
pos=: [: ccl [: +/ dirs {~ dn i. parse
a24=: [: +/ [: (2|#)/.~ pos;._2 NB. find odd flip pos, sum

NB. Life on an hexagonal grid white=0, black=1
NB. for each life tile get neighbors=: tile+dirs
NB. a) decide for tile (1 2 e.~ +/ neigh)
NB. b) decide for non-existent (0) neighbors: if appears twice: 1
b24=:{{ NB. slow ~ 4 min, 
NB. List of initially active tiles, take those whose positions appeared an
NB. odd number of times
act =. ((2|#)/.~ # ~.) pos;._2 y
for_t. i. 100 do.
  NB. echo t,#act
  neigh=. ccl act +/ dirs                    NB. neighbours of active cells 
  live =. act #~neigh (1 2 e.~ +/@:e."1) act NB. stays alive  ; slow
  birth=. (~. #~(2=#)/.~) , neigh            NB. birth of cell; slow
  act  =. ~. live,birth
end.
#act
}}
(a24;b24)i24
}} dod 24 ''
{{ NB. Day 25 entry card encryption
i25=: ".;._2 freads'2020-25.txt' NB. public keys
NB. dpk =: (20201227 |sub * ])^:dloop [sub=:7
NB. cpk =: (20201227 |sub * ])^:cloop [sub=:7
NB. ek1 =: (20201227 |dpk * ])^:cloop
NB. ek2 =: (20201227 |cpk * ])^:dloop
NB. ek1=ek2
NB. fermat little theorem: if 1=a+.p => 1=p|a^<:p
a25=:{{
mm=: 20201227
'kc kd'=: y
NB. bruteforce computation of all elements in group
'lc ld'=: y i.~ pow7=. (mm|7*])^:(<mm) 1
NB. ek = (mm|kc^ld)= (mm|7^lc*ld) NB. expand kc
NB. ek = mm|7^((<:mm)|lc*ld)      NB. from Fermat's littl Theorem
NB. powers of 7 saved before
((<:mm)|lc *&x: ld) { pow7
}}
b25=:0:
(a25;b25)i25
}} dod 25 ''

{{ NB. Day dd
idd=: freads'2020-dd.txt'
add=:0:
bdd=:0:
(add;bdd)idd
}} dod dd

echo 'AoC 2020 summary'
echo (<;._1 '|Day|Part 1|Part 2|Time'),((<"0 days),.res),(<;._1 '|||Total time'),+/&.:>{:"1 res
NB. vim: ts=2 sw=2 et fdm=marker foldmarker={{,}}
