move: 0
  dac 9f+t
  lac move i
1:dac 2f
  lac 9f+t
  jms lchar
  dac 9f+t+1
  jms dchar
2: 0
  lac 9f+t+1
  sad o777
  jmp 3f
  lac o400000
  add 9f+t
  dac 9f+t
  lac o400000
  add 2b
  jmp 1b

3:isz move
  lac 2b
  jmp move i
t=t+1   "source address
t=t+1   "source character

lchar:0
  dac junk
  ral
  lac junk i
  snl
  lrs 9
  and o777
  jmp lchar i

dchar:0
  lmq
  lac dchar i
  dac junk
  spa
  jmp 1f
  llss 9
  lac o777
  jmp .+2
1:0777000
  and junk i
  omq
  dac junk i
  isz dchar
  jmp dchar i


"     gets designated character from input
jget:0
1:lac j
  jms cbetween; add jmin; add jmax
  jmp jmore
  cma
  add jmin
  cma
  add jbot
  jms lchar
  jms class; add ignore

  jmp jget i
  lac j
  add o400000
  dac j
  jmp 1b

" read more input - filthy code, enough to make disk &
" terminal input work.  Theae only deliver full count
" except at eof or 1 word

jmore:
and o377700
  dac jmin
  add ljsiz
  dac 9f+t
  lac jmax
  jms cbetween; add jmin; add 9f+t
  lac jmin
  dac jmax
  dac 1f
  cma
  add jmin
  cma
  add jbot
  dac 2f
  lac input
  sys seek
1:0;0
  -1
  dac 2f i
  lac input
  sys read
2:0;jsiz
  sna
  lac d1
  add jmax
  dac jmax
  jmp jget+1
t=t+1

"     gets next character from input

getj:0
  lac j
  jms jget
  dac junk
  lac j
  add o400000
  dac j
  lac junk
  jmp getj i

" compare two strings - assume both left justified

comp:0
  dac 9f+t
  lac comp i
  dac 9f+t+1
  isz comp
1:lac 9f+t i
  sad 9f+t+1 i

  jmp 3f
  and 9f+t+1 i   "do both start with eof?
  spa
2:isz comp
  jmp comp i
3:and o600600  "is there an eof?
  sza
  jmp 2b
  isz 9f+t
  isz 9f+t+1
  jmp 1b
t=t+1   "address of string 1
t=t+1  "address of string 2


obuild:0
  lmq
  lac owrite
  add obot
  dac 2f
  lacq
1:jms move
2:0
  cma
  add obot
  cma
  dac owrite
  jms cbetween; add d0; add omax
   skp
  jmp obuild i

  lac lochunk
  jms oflush
  lac obot
  dac 2b
  add lochunk
  jmp 1b

oflush:0
  dac 2f
  lac obot
  dac 1f
  lac output
  sys write
1:0
2:0
  jmp oflush i

" outputs octal string from sesignated value

octal:
  isz ii
  lac ii i
  dac 2f
  lac 1f
  jms twoktab
  lac 2f i

1:gf geoctal x
2:0

geoctal:
  lac ii
  dac 8
  lac 8 i
  jms putoct
  jmp ggoon


" converts word in ac into ocatl on output stream

putoct:0
  dac 9f+t
  lac 7f
  jms obuild
  dzm 9f+t+2
  -6
  dac 9f+t+1

1:lac 9f+t
  cll
  lrs 15
  add o60
  dac 8f+1
  lls 18
  dac 9f+t
  lac 9f+t+2   "have nonzero digits been seen?
  sza
  jmp 2f
  lac 8f+1   "no,is this nonzero?
  sad o60
  jmp 3f   "no
2:lac 8f
  jms obuild
  law
  dac 9f+t+2
3:isz 9f+t+1
  jmp 1b
  jmp putoct i
t=t+1   "value to convert
t=t+1   "digit count
t=t+1   "nonzero digit flag
7: .+1; 060777
8:0400000 .+1;0;end


eof:
  lac j
  dac jsav
  jms jget
  sad o777
  jmp goon
  jmp backup
class:0
  dac junk1
  lrss 7
  sza
  jmp 2f
  lls 3
  xct class i
  isz class
  dac junk

  cla
  llss 4
  add l.llss
 dac 1f
  lac junk i
1:llss
  spa
2:isz class
  lac junk1
  jmp class i
