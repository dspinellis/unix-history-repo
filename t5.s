char:
  lac j
  dac jsav
 isz ii
  jms ctest
 jmp backup
  jmp goon

string:
  isz ii
  jms ctest
  jmp goon
  jmp string+1

ctest:0
  jms jget
  jms class; add ii i
  jmp ctest i
  jms sbput
  lac j
  add o400000
  dac j
  isz ctest
  jmp ctest i

mark:
  jms jget
  dzm sbwrite
  jmp goon

parsedo:
  isz ii
  jms advance; jmp 3f
  jms advance; jmp 1f
  jms aget
  dac ii
  jmp rinterp

1:lac frame
  add refrsz
  dac ii
  sad nframe
  jmp retreat
  dac gflag
  lac gefrsz
  dac dffrmsz
  jms advance; jmp 2f
  jmp ginterp

2:lac refrsz
  dac dffrmsz
  add frame
  dac nframe
  dzm gflag
  jmp retreat

3:jms s0get; add d.k
  dac k
  jmp goon

bundle:
  jms bundlep

  dac 9f+t
  sna
  jmp goon
  jms nframe0
  dac nframe
  lac 9f+t
  dac nframe i
  isz nframe
  jmp goon
t=t+1

"   jms between;add a; add b; skip if a<=ac<b

between:0
  dac 9f+t
  cma
  xct between i
  isz between
  sma
  jmp 1f
  lac 9f+t
  cma
  xct between i
  isz between
  sma
1:isz between
  lac 9f+t
  jmp between i
t=t+0   "shared with next temporary



"  jms cbetween; add a; add b; skip if a<=ac<b where ac
"   contains a character address

cbetween:0
  dac 9f+t
  cma
  xct cbetween i
  isz cbetween
  ral
  sma
  jmp 1f
  lac 9f+t
  cma
  xct cbetween i
  isz cbetween
  ral
  sma
1:isz cbetween
  lac 9f+t
  jmp cbetween i
t=t+1   "ac contents
