"     recognition stack frame advance

advance:0
  lac frame
  dac 8
  lac advance i
  dac 8 i
  lac ii
  dac 8 i
  lac ignore
  dac 8 i
  lac j
  dac 8 i
  lac k
  dac 8 i
  lac frame
  dac nframe i
  lac nframe
  dac frame
  add dffrmsz
  dac nframe
  dac nframe
  jms between; add rbot; add rtop
  jms halt
  isz advance
  jmp advance i

retreat:
  dzm junk
  lac gflag
  sza
  jmp 1f
  jms bundlep
  dac junk
1:lac frame
  dac nframe
  lac frame i
  dac frame
  dac 8
  lac 8 i
  dac 3f   "retrun address
  lac 8 i
  dac ii
  lac 8 i
  dac ignore
  lac fflag
  sna
  jmp 2f
  lac 8 i   "restore j and k on failure
  dac j
  lac 8 i
  dac k
2:lac junk
  sna
  jmp 3f
  dac nframe i   "stass reslts
  isz nframe
3:jmp

" bundle up results and return single pointer to them in ac
" return 0 if no results

bundlep:0
  lac fflag
  sza
  jmp 2f   "no results on failure
  jms nframe0
  dac 9f+t
  cma
  tad nframe
  cma
  dac 9f+t+1
  sma
  jmp 2f
  sad m1
  jmp 3f   "only one result, no bundling necessary
  lac 9f+t
  tad m1
  dac 8

1:lac 8 i
  jms kput
  isz  9f+t+1
  jmp 1b
  lac k   "make up result pointer
  add l.gk
  jmp bundlep i

2:cla
  jmp bundlep i
3:lac 9f+t i
  jmp bundlep i
t=t+1   "where to find results
t=t+1   "negative of result count

"     the main interpreter loop
" locate original value of nframe for present stack level.
nframe0:0
  jms s1get; add d.ii
  dac junk
  lac junk i
  dac junk
  lac junk i
  and opmask
  sad l.rw
  jmp 1f
  lac refrsz
  jmp 2f
1:lac junk i
  and o17777
2:add frame
  jmp nframe0 i

halt:0
  lac 1f
  jms obuild
  lac halt
  jms putoct
  lac onenl
  jms obuild
  xct rstack+1
1:.+1;012077;end

rinterp:
  las   "trace check
  and d5
  sna
  jmp .+3
  lac bugr
  jms bug

  lac fflag
  ral
  lac ii i
  and opmask
  sad l.ra
  jmp rera
  sad l.rb
  jmp rerb
  szl
  jmp retreat
  lrs 14
  and o17
  add rbranch
  dac .+1
  jmp

rbranch:
  jmp .+1 i
  reno
  rerx
  rerc
  regc
  rerf
  rerw
  rera
  reuu
  rero
  rerm
  rers
  rerv
  reuu
  reuu
  reuu
  reuu

reuu:
  jms halt

rerb:
  cml
rera:
  dzm fflag
  snl
  jmp goon
  jms aget
  dac ii
  jmp rinterp

backup:
  lac jsav
  dac j
nuts:
  law

  dac fflag

reno:
goon:
  lac ii i
  isz ii
  and exitmask
  sza
 jmp retreat
  jmp rinterp


rerw:
  jms aget
  add frame
  dac nframe
  jmp goon

rerc:
  jms advance; jmp goon
  jms aget
  dac ii
  jmp rinterp


gegf:
rerf:
  jms aget
  add ljmp
  dac .+1
  jmp

rerx:
  lac j
  dac jsav
  jms aget
1:dac 9f+t
  jms lchar
  sad o777
  jmp goon
  dac 9f+t+1
  jms getj
sad 9f+t+1
  jmp 2f
  jmp backup
2:lac 9f+t
  add o400000
  jmp 1b
t=t+1   "address of next comparison char
t=t+1   "character itself

aget:0
  lac ii i
  and o17777
  jmp aget i

regc:
  lac ii i
  and o757777
  xor exitmask
  dac nframe i

  isz nframe
  jmp goon

kput:0
  isz k
  dac junk1
  lac k
  jms between; add d0; add kmax
  jms halt
  add kbot
  dac junk
  lac junk1
  dac junk i
  jmp kput i

s0get:0
  lac frame
  xct s0get i
  dac junk
  lac junk i
  isz s0get
  jmp s0get i

s1get:0
  lac frame i
  xct s1get i
  dac junk
  lac junk i
  isz s1get
  jmp s1get i

s0put:0
  lmq
  lac frame
  xct s0put i
  dac junk
  lacq
  dac junk i
  isz s0put
  jmp s0put i
"     here is the generaion interpreter
"     the k table cant move while its active

geno:
ggoon:
  lac ii i
  isz ii
  and exitmask
  sza
  jmp retreat
ginterp:
  las   "trace check
  and d6
  sna
  jmp .+3
  lac bugg
  jms bug

  lac ii i
  lrss 14
  and o7

  add gbranch
  dac .+1
  jmp

gbranch:
  jmp .+1 i
  geno
 gegx
  geuu
  gegc
  gegf
  gegk
  gegp
  gegq

geuu:
  jms halt

gegx:
  lac ii i
  and o417777
  jms obuild
  jmp ggoon

gegq:
  jms advance; jmp ggoon
  lac env
  add d.ii
  dac junk
  jms aget
  add junk i
  dac junk
  lac junk i
  dac ii
  lac env
  add d.env
  dac junk
  lac junk i
  dac env
  jmp ginterp

gegp:
  jms advance; jmp ggoon
  lac env
  add d.ii
  dac junk
  lac frame i
  dac env
  jms aget
cma
  add junk i
  dac ii
  jmp ginterp

gegk:
  lac ii i
  jms aget
  add kbot
  dac ii
  jms s0put; add d.ii
  lac frame

  dac env
  jmp ginterp

gegc:
  jms advance; jmp ggoon
  jms aget
  dac ii
  jmp ginterp

bug:0
  dac 1f+2
  lac onenl
  jms obuild
  lac ii
  jms putoct
  lac ii i
  lrs 14
  and o17
  add 1f+2
  dac 1f+2
  lac 1f+2 i
  dac 1f+2
  lac 1f
  jms obuild
  lac ii i
  jms putoct
  las
  and d4
  sza
  jms halt
  jmp bug i

1:0400000 .+1; 040; 0; 040777
