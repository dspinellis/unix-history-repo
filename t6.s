rerm:
  jms aget
  jmp 1f
rers:
  jms aget
  add frame
1:dac holdlv
  lac holdlv i
  jmp 2f
rerv:
  jms aget
  lls 6
  lrs 6
2:dac nframe i
  isz nframe
  jmp goon

rero:
  jms decnf
  dac rand1
  jms aget
  and o77
  add obranch
  dac 2f
  cma
  tad unary
  spa
  jmp 1f
  jms decnf
  dac rand2
1:lac rand1
2:xct
  jmp result

obranch:
  xct .+1
  opr        ;op=0400000+1
  jmp rorel  ;le=op;op=op+1
  jmp rorel  ;ne=op;op=op+1
  jmp rorel  ;lt=op;op=op+1
  jmp rorel  ;ge=op;op=op+1
  jmp rorel  ;eq=op;op=op+1
  jmp rorel  ;gt=op;op=op+1
  tad rand2  ;ad=op;op=op+1
  jms sub    ;sb=op;op=op+1
  and rand2  ;an=op;op=op+1
  jmp roor   ;or=op;op=op+1
  xor rand2  ;xo=op;op=op+1
  jmp rosr   ;sr=op;op=op+1
  jmp rosl   ;sl=op;op=op+1
  jmp romn   ;mn=op;op=op+1
  jmp romx   ;mx=op;op=op+1
  lac rand2  ;as=op;op=op+1

  opr        ;pl=op;op=op+1
  jmp romi   ;mi=op;op=op+1
  cma        ;cm=op;op=op+1
  jmp roindir;indir=op;op=op+1
  lac holdlv ;addr=op;op=op+1
unary:xct obranch+1+pl

rorel:      "<= 001

  jms sub   "!= 010
  sna       "<  011
  jmp 2f    ">= 100
  spa       "=  101
  jmp 1f    ">  110
  lac d1   "a>b, code 001
  jmp 3f
1:lac d2   "a<b, code 100
2:add d2   "a=b, code 010
3:and ii i
  sza
  -1
  cma
  jmp result

sub:0
  cma
  tad rand2
  cma
  jmp sub i

roor:
  lmq
  lac rand2
  omq
  jmp result

rosr:
  lac rand2
  add l.lrs
  cll
  jmp 1f
rosl:
  lac rand2
  add l.lls
  clq
1:dac .+2
  lac rand1
  0
  jmp result

romn:
  jms sub
  cma
  jmp .+2
romx:
  jms sub
  ral
  lac rand1
  szl
  lac rand2
  jmp result

romi:
  cma
  tad d1
  jmp result

roindir:
  dac holdlv
  lac holdlv i

  jmp result

result:
  dac junk
  dac nframe i
  isz nframe
  lac ii i
  and stbit
  sna
  jmp exprtest
  lac junk
  dac holdlv i
  lac ii i
  and fibit
  sza
  jms decnf
  jmp goon

exprtest:
  lac ii i
  and fibit
  sna
  jmp goon
  jms decnf
  lac nframe i
  sza
  -1
  cma
  dac fflag
  jmp goon

decnf:0
  -1
  tad nframe
  dac nframe
  lac nframe i
  jmp decnf i
