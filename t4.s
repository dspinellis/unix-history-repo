"     put symbuf symbol into table

table:
  lac equwrite
  dac equ
  jms between; add d0; add equmax
  jms halt
  add delta
  dac equwrite
  lac equ
  add equbot
  tad m1
  dac 8
  lac symwrite
  dac 8 i
  add symbot
  dac 2f
  lac mdelta
1:dzm 8 i
  tad d1
  spa
  jmp 1b
  lac sbbot
  jms move
2:0
  add o400000
  cma
add symbot
  cma
  add o400000
  and o17777
  dac symwrite
  jms between; add d0; add symmax
  jms halt
  jmp goon

"     find occurrence of symbuf symbol in equtab

prev:
  lac equ
  jmp find+1
find:
  lac equwrite
  dac 9f+t
  lac o777
  jms sbput
  lac sbbot
  dac 2f
1:lac 9f+t
  tad mdelta
  dac 9f+t
  spa
  jmp nuts
  add equbot
  dac junk
  lac junk i
  add symbot
  jms comp
2:0
  jmp 1b
  lac 9f+t
  dac equ

  jmp goon
t=t+1 "next equtab location to test

sbput:0
  lmq
  lac sbwrite
  add sbbot
  dac 1f
  lacq
  jms dchar
1:0
  lac sbwrite
  add o400000
  dac sbwrite
  jms cbetween; add d0; add sbmax
  jms halt
  jmp sbput i

getname:
  lac equ
  add equbot
  dac 9f+t
  lac 1f
  jms twoktab; lac 9f+t i

1:gf .+1 x
  lac ii
  dac 8
  lac 8 i
  add symbot
  and o17777
  jms obuild
  jmp ggoon
t=t+1   "equtable entry

" puts double word entries in ktab and gives
"pointer to first as result

twoktab:0
  jms kput
  lac l.gk
  add k
dac nframe i
  isz nframe
  xct twoktab i
  jms kput
  jmp goon
