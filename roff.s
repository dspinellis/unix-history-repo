" ** 11-56-91.pdf page 9
" roff

   lac i 017777
   sad d4
   sys exit
   lac 017777
   tad d1
   dac fname
   jms nextfile
   lac obufp
   dac otal

" main i/o loop
1:
   jms readline
   jmp 3f
   lac rawchar
   sad cc
   jmp 2f
   jms text
   jmp 1b
2:
   jms control
   jmp 1b
3:
   jms break
   jms eject
   lac otal
   sma
   jmp 1f
   cla
   jms putsc; otal
1:
   -1
   tad obufp
   cma
   tad otal
   dac 1f
   lac output
   sys write; obuf; 1: 0
   sys exit

" read line routine
readline: 0
   law rawchar-1
   dac 8
1:
   jms getchar
   dac i 8
   sad o12
   skp
   jmp 1b
   isz readline
   jmp i readline

" read character routine
getchar: 0
   lac ital
   sad eibufp
   skp
   jmp 1f
   -64
" ** 11-56-91.pdf page 10
   dac 3f
   law ibuf-1
   dac 15
2:
   dzm i 15
   isz 3f
   jmp 2b
   lac input
   sys read; ibuf; 64
   sna
   jms nextfile
   lac ibufp
   dac ital
1:
   jms getsc; ital
   sza
   jmp i getchar
   jmp getchar+1
3: 0

putchar: 0
   jms putsc; otal
   lac otal
   sad eobufp
   skp
   jmp i putchar
   lac output
   sys write; obuf; 64
   lac obufp
   dac otal
   jmp i putchar

laci: 0
   dac 1f
   lac i 1f
   jmp i laci
1: 0

nextfile: 0
   lac i 017777
   sad d4
   jmp i readline
   tad dm4
   dac i 017777
   lac fname
   tad d4
   dac fname
   lac input
   sys close
   sys open; fname: 0; 0
   sma
   jmp 1f
   lac fname
   dac 2f
   lac d1
   sys write; 2: 0; 4
   lac d1
   sys write; 2f; 2
   sys exit
2: 040077;012
1:
" ** 11-56-91.pdf page 11
   dac input
   lac eibufp
   dac ital
   jmp i nextfile

getsc: 0
lac i getsc
dac sctalp
isz getsc
lac i sctalp
dac sctal
add o400000
dac i sctalp
ral
lac i sctal
szl
lrss 9
and o177
jmp i getsc

putsc: 0
and o177
lmq
lac i putsc
dac sctalp
isz putsc
lac i sctalp
dac sctal
add o400000
dac i sctalp
sma cla
jmp 1f
llss 27
dac i sctal
lrss 9
jmp i putsc
1:
lac i sctal
omq
dac i sctal
lacq
jmp i putsc

" control card decoder
control: 0
   law 2f-1
   dac 8
   -ncase
   dac c
   lac rawchar+1
   alss 9
   xor rawchar+2
1:
   sad i 8
   jmp i 8
   isz 8
   isz c
   jmp 1b
   jmp i control
2:
   ncase = 0
" ** 11-56-91.pdf page 12
   <ad>; jmp casead; ncase = ncase+1
   <bp>; jmp casebp; ncase = ncase+1
   <br>; jmp casebr; ncase = ncase+1
   <cc>; jmp casecc; ncase = ncase+1
   <ce>; jmp casece; ncase = ncase+1
   <ds>; jmp caseds; ncase = ncase+1
   <fi>; jmp casefi; ncase = ncase+1
   <in>; jmp casein; ncase = ncase+1
   <li>; jmp caseli; ncase = ncase+1
   <ll>; jmp casell; ncase = ncase+1
   <ls>; jmp casels; ncase = ncase+1
   <na>; jmp casena; ncase = ncase+1
   <ne>; jmp casene; ncase = ncase+1
   <nf>; jmp casenf; ncase = ncase+1
   <pl>; jmp casepl; ncase = ncase+1
   <sp>; jmp casesp; ncase = ncase+1
   <ss>; jmp casess; ncase = ncase+1
   <ti>; jmp caseti; ncase = ncase+1
   <ul>; jmp caseul; ncase = ncase+1
   <un>; jmp caseun; ncase = ncase+1

" control cases
casead:
   jms break
   -1
   dac ad
   jmp i control

casebp:
   jms break
   jms eject
   jmp i control

casebr:
   jms break
   jmp i control

casecc:
   jms skipcont
   lac i 8
   sad o12
   jmp i control
   dac cc
   jmp i control

casece:
   jms break
   jms number; d0
   spa
   cla
   dac ce
   jms need; ce
   jmp i control

caseds:
   jms break
   lac d2
   dac ls
   jmp i control

casefi:
" ** 11-56-91.pdf page 13
   jms break
   -1
   dac fi
   jmp i control

casein:
   jms number; in
   cma
   tad d1
   sma
   cla
   dac in
   dac un
   jmp i control

caseli:
   jms number; d0
   cma
   tad d1
   sma
   jmp i control
   dac 2f
1:
   jms readline
   jmp i control
   jms text
   isz 2f
   jmp 1b
   jmp i control
2: 0

casell:
   jms number; ll
   spa
   cla
   dac ll
   jmp i control

casels:
   jms number; d0
   sza; spa
   lac d1
   dac ls
   jmp i control

casena:
   jms break
   dzm ad
   jmp i control

casene:
   jms number; d0
   spa
   cla
   dac c
   jms need; c
   jmp i control

casenf:
   jms break
   dzm fi
" ** 11-56-91.pdf page 14
   jmp i control

casepl:
   jms number; pl
   spa
   cla
   dac pl
   jms topbot
   jmp i control

casesp:
   jms break
   jms number; d0
   cma
   tad d1
   sma
   jmp i control
   dac c
1:
   jms nline
   isz c
   jmp 1b
   jmp i control

casess:
   jms break
   lac d1
   dac ls
   jmp i control

caseti:
   jms break
   jms number; in
   spa
   cla
   dac un
   jmp i control

caseul:
   jms number; d0
   spa
   cla
   dac ul
   jmp i control

caseun:
   jms number; d0
   tad in
   sma
   cla
   dac un
   jmp i control

" selected short routines
skipcont: 0
   law rawchar-1
   dac 8
1:
   lac i 8
   sad o40
   jmp 1f
" ** 11-56-91.pdf page 15
   sad o12
   jmp 2f
   jmp 1b
1:
   lac i 8
   sad o12
   jmp 2f
   sad o40
   jmp 1b
2:
   -1
   tad 8
   dac 8
   jmp i skipcont

break: 0
   lac nc
   sna
   jmp i break
   -2
   tad ls
   cma
   sma
   jmp 2f
   dac c
1:
   jms nline
   isz c
   jmp 1b
2:
   lac nl
   sad bl
   jms eject
   lac nl
   sza
   jmp 2f
   -5
   dac c
1:
   lac o55
   jms putchar
   isz c
   jmp 1b
   lac ma1
   dac c
1:
   jms newline
   isz c
   jmp 1b
2:
   law char-1
   dac 8
   lac un
   sma
   jmp 1f
   dac c
2:
   lac o40
   jms putchar
   isz c
   jmp 2b
" ** 11-56-91.pdf page 16
1:
   lac i 8
   jms putchar
   isz nc
   jmp 1b
   jms newline
   dzm nwd
   dzm ne
   lac in
   dac un
   jmp i break

newline: 0
   lac o12
   jms putchar
   isz nl
   jmp i newline

nline: 0
   lac nl
   sna
   jmp i nline
   sad bl
   jmp i nline
   jms newline
   jmp i nline

number: 0
   dzm num
   dzm sign
   -1
   dac any
   jms skipcont
1:
   lac i 8
   sad o12
   jmp 3f
   sad o53
   jmp 2f
   sad o55
   jmp 2f
   tad om72
   sma
   jmp 1b
   tad o12
   spa
   jmp 1b
   dac any
   lac num
   cll; mul; 10
   lacq
   tad any
   dac num
   jmp 1b
2:
   dac sign
   jmp 1b
3:
   lac any
   sma
   jmp 1f
" ** 11-56-91.pdf page 17
   lac d1
   isz number
   jmp i number
1:
   lac sign
   sza
   jmp 1f
   lac num
   isz number
   jmp i number
1:
   sad o53
   jmp 1f
   lac i number
   jms laci
   cma
   tad num
   cma
   isz number
   jmp i number
1:
   lac i number
   jms laci
   tad num
   isz number
   jmp i number

eject: 0
   lac pl
   sna
   jmp i eject
   lac nl
   sna
   jmp i eject
1:
   sad pl
   jmp 1f
   jms newline
   lac nl
   jmp 1b
1:
   dzm nl
   jmp i eject

storechar: 0
   lmq
   lac nc
   sza
   jmp 1f
   law char-1
   dac 10
1:
   lacq
   dac i 10
   jms width
   cma
   tad d1
   tad ne
   dac ne
   -1
   tad nc
" ** 11-56-91.pdf page 18
   dac nc
   jmp i storechar

getword: 0
   law word-1
   dac 8
		" *** hand written box/arrow to move after 1:
   dzm wne
   dzm wch
1:
   lac i 11
   sad o12
   jmp i getword
2:
   dac i 8
   lmq
   jms width
   cma
   tad d1
   tad wne
   dac wne
   -1
   tad wch
   dac wch
   lacq
   sad o40
   jmp 1b
   lac word
   sad o40
   jmp 1f
   lac o40
   dac word
   lacq
   jmp 2b
1:
   lac i 11
   sad o12
   jmp 1f
   sad o40
   jmp 1f
   dac i 8
   jms width
   cma
   tad d1
   tad wne
   dac wne
   -1
   tad wch
   dac wch
   jmp 1b
1:
   -1
   tad 11
   dac 11
   isz getword
   jmp i getword

need: 0
   lac ls
   dac 1f
   lac i need
   jms laci
" ** 11-56-91.pdf page 19
   cll; mul; 1: 0
   lacq
   tad nl
   cma
   tad bl
   spa
   jms eject
   isz need
   jmp i need

" text line routine
text: 0
   -1
   tad ul
   sma
   jms undline
   -1
   tad ce
   sma
   jms center
   law rawchar-1
   dac 11
   lac rawchar
   sad o12
   jmp 1f-1
   sad o40
   jmp 1f-1
   lac fi
   sza
   jmp 2f
   skp
   jms break
1:
   lac i 11
   sad o12
   jmp 1f
   jms storechar
   jmp 1b
1:
   lac nc
   sna
   jms nline
   jms break
   jmp i text
2:
   jms getword; jmp i text
   lac wne
   tad ne
   tad un
   tad ll
   spa
   jms adjust
   law word-1
   dac 8
   lac nwd
   sza
   jmp 3f
1:
   lac i 8
   sad o40
   skp
" ** 11-56-91.pdf page 20
   jmp 3f+1
   isz wch
   jmp 1b
3:
   lac i 8
   jms storechar
   isz wch
   jmp 3b
   isz nwd
   jmp 2b

" adjust routine
adjust: 0
   lac nwd
   sna
   jmp i adjust
   law char-1
   dac 8
   law tchar-1
   dac 9
   dzm ndiv
   dzm nrem
   lac ad
   sna
   jmp 1f
   -1
   tad nwd
   sna
   jmp 1f
   dac 2f
   lac ll
   tad ne
   tad un
   spa
   jmp 1f
   cll; idiv; 2: 0
   dac nrem
   lacq
   dac ndiv
1:
   lac i 8
   sad o40
   jms fill
   dac i 9
   isz nc
   jmp 1b
   lac o12
   dac i 9
   law tchar-1
   dac 8
2:
   lac i 8
   sad o12
   jmp 2f
   jms storechar
   jmp 2b
2:
   jms break
   jmp i adjust

fill: 0
" ** 11-56-91.pdf page 21
   lac nrem
   sna
   jmp 2f
   tad dm1
   dac nrem
   lac d1
2:
   tad ndiv
   cma
   dac c
   lac o40
2:
   dac i 9
   isz c
   jmp 2b
2:
   isz nc
   lac i 8
   sad o40
   skp
   jmp i fill
   dac i 9
   jmp 2b

" more routines
topbot: 0
   lac pl
   sza
   jmp 1f
   dzm bl
   jmp i topbot
1:
   -11
   tad pl
   spa
   jmp 1f
   dac bl
   cma
   tad d1
   tad nl
   spa
   jmp i topbot
   lac bl
   dac nl
   jmp i topbot
1:
   lac d55
   dac bl
   dac nl
   tad d11
   dac pl
   jmp i topbot

undline: 0
   dac ul
   law rawchar-1
   dac 8
   law tchar-1
   dac 9
1:
   lac i 8
" ** 11-56-91.pdf page 22
   dac i 9
   sad o12
   jmp 1f
   sad o40
   jmp 1b
   lac o10
   dac i 9
   lac o137
   dac i 9
   jmp 1b
1:
   law tchar-1
   dac 8
   law rawchar-1
   dac 9
1:
   lac i 8
   dac i 9
   sad o12
   jmp i undline
   jmp 1b

center: 0
   dac ce
   law rawchar-1
   dac 8
   law tchar-1
   dac 9
   dzm wne
1:
   lac i 8
   dac i 9
   sad o12
   jmp 1f
   jms width
   tad wne
   dac wne
   jmp 1b
1:
   -1
   tad wne
   spa
   jmp i center
   cma
   tad ll
   tad in
   lrss 1
   cma
   tad d1
   sma
   -1
   dac c
   law tchar-1
   dac 8
   law rawchar-1
   dac 9
   lac o40
1:
   dac i 9
   isz c
   jmp 1b
" ** 11-56-91.pdf page 23
1:
   lac i 8
   dac i 9
   sad o12
   jmp i center
   jmp 1b

width: 0
   sad o10
   jmp 1f
   lac d1
   jmp i width
1:
   -1
   jmp i width

eibufp: ibuf+64
ibufp: ibuf
eobufp: obuf+64
obufp: obuf
input: 0
output: 1
ls: 1
ce: 0
in: 0
un: 0
ul: 0
ma1: -5
bl: 55
ll: 50
nwd: 0
nl: 0
nc: 0
ne: 0
pl: 66
ad: -1
fi: -1
cc: .>

o12: 012
o40: 040
o177: 0177
o53: 053
om72: -072
o55: 055
o400000: 0400000
o10: 010
d11: 11
d55: 55
o137: 0137
d1: 1
d2: 2
d4: 4
dm4: -4
d0: 0
dm1: -1

c: .=.+1
nrem: .=.+1
ndiv: .=.+1
num: .=.+1
" ** 11-56-91.pdf page 24
any: .=.+1
ital: .=.+1
otal: .=.+1
sctal: .=.+1
sctalp: .=.+1
sign: .=.+1
wch: .=.+1
wne: .=.+1
word: .=.+300
char: .=.+300
tchar: .=.+300
rawchar: .=.+300
ibuf: .=.+64
obuf: .=.+64
