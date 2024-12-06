NB. AoC, 2024
{{
if. -. fexist '~addons/general/jaoc' do.
  install'github:jpjacobs/general_jaoc'
  echo 'installed general/jaoc; go set up COOKIE.txt'
end.
}}''
load'general/jaoc'
1!:44 jpath '~J/AoC/'
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
M=: 0 1 2 3 4 5 5 5 5 5 5 5 5 5 5 6(a.i.'mul(,0123456789)') }256#7
S =: +.@:".&> }. <;._2 {{)n NB. state transitions row j: code
NB. m   u   l   (   ,  num  ) other
   1j0 0j0 0j0 0j0 0j0 0j0 0j0 0j0 NB. 0 not started
   1j0 2j0 0j0 0j0 0j0 0j0 0j0 0j0 NB. 1 m
   0j0 0j0 3j0 0j0 0j0 0j0 0j0 0j0 NB. 2 u
   0j0 0j0 0j0 4j0 0j0 0j0 0j0 0j0 NB. 3 l
   0j0 0j0 0j0 0j0 0j0 5j1 0j0 0j0 NB. 4 (
   0j0 0j0 0j0 0j0 6j0 5j0 0j0 0j0 NB. 5 num1
   0j0 0j0 0j0 0j0 0j0 6j0 0j3 0j0 NB. 6 num2
}}
NB.    sum  mule rows boxed nums cut by fsm
p1=: [: +/ [:*/[:|:@:(".&>) (0;S;M) fsm }:
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
p1 =: +/@:( ((score * ordered) >)"_ 0&>/)@par
  NB. is ordered?: x:rules (2 cols); y: 1 line of prints
  ordered =: *./@:(<:/)@:(] |:@filt i.~)
  NB. filter results of lookup; removing results with page not in list
  NB. x: print list; y: lookup result (2 cols)
  filt =: ] #~ [: *./"1 ] < #@[
  score=: ({~2%~1-~#)@]
NB. Part 2: fix the ordering of the incorrect ones; get score of only those
NB. after fixing
NB.  pp2 =: +/@:( ([ score@fix ]#~(-.@ordered)>)"_ 0)&>/)@par
NB.    fix=: 
NB.    NB. move item at x0 to x1 in y
NB.    mv =: 
0
}}
NB. temporary storage
pad=: '.' |:@:([,,~)^:2 ] NB. pad all sides 
echo run 5
NB. vim: ts=2 sw=2 et fdm=marker foldmarker={{,}}
