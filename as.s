" as

   jms init1

assm1:
   lac eofflg
   sza
   jmp assm2
   lac passno
   sza
   jmp finis
   jms init2

assm2:
   jms gchar
   sad d4
   jmp assm1
   sad d5
   jmp assm1
   lac char
   dac savchr
   jms gpair
   lac rator
   jms betwen; d1; d6
   jmp assm3
   jms expr
   lac passno
   sza
   jms process
   isz dot+1
   nop
   lac dot+1
   and o17777
   sad dot+1
   jmp assm1
   jms error; >>
   dzm dot+1
   jmp assm1

assm3:
   lac rand
   sad d2
   jmp assm4
   sza
   jmp assm6
   lac rator
   sza
   jmp assm6
   lac rand+1
   jms betwen; dm1; d10
   jmp assm6
   dac name
   tad fbxp
   dac lvrand
   lac i lvrand
   dac name+1
   isz i lvrand
   lac o146
   dac name+2
   dzm name+3
   jms tlookup
   -1
   dac fbflg
assm4:
   lac rand+1
   tad d4
   dac lvrand
   lac rator
   sza
   jmp assm5
   lac dot
   dac r
   lac dot+1
   dac r+1
   jmp 1f

assm5:
   jms gpair
   jms expr
1:
   lac r
   dac i lvrand
   isz lvrand
   lac r+1
   dac i lvrand
   lac fbflg
   sna
   jmp assm1
   dzm fbflg
   dzm name+1
   lac o142
   dac name+2
   jms lookup
   jmp assm4

assm6:
   jms error; x>
   jmp assm1

init1: 0
   lac d1
   sys write; 1f; 2f-1f
   dzm passno
   lac o56040
   dac dot-4
   lac o56056
   dac cmflx-4
   lac o40040
   dac dot-3
   dac dot-2
   dac dot-1
   dac cmflx-3
   dac cmflx-2
   dac cmflx-1
   dzm iof
   jms init
   jmp i init1
1:
   0111012
2:

init2: 0
   lac d1
   dac passno
   sys write; 1f; 2f-1f
   jms init
   lac o17
   sys creat; 2f
   dac bfo
   sys open; 2f; 0
   dac bfi
   dzm bufadd
   jms copyz; buf; 64
   jmp i init2
1:
   0111111;012000
2:
   0141056;0157165;0164040;040040

init: 0
   lac i 017777
   dac narg
   lac 017777
   tad d1
   dac fname
   -1
   dac eofflg
   jms nextfil
   jms ioinit
   dzm savchr
   dzm comflg
   lac d1
   dac dot
   dzm dot+1
   dzm cmflx
   lac d4096
   dac cmflx+1
   dzm fbflg
   jms copyz; fbxp: fbx; 10
   jmp i init

finis:
   lac iof
   sys close
   jms bufwr
   lac bfi
   sys close
   lac bfo
   sys close
   -1
   tad namsiz
   cma
   rcl
   dac char
   rcl
   tad char
   dac 1f
   lac o17
   sys creat; n.out
   dac bfi
   sys write; namlst; 1: 0
   lac bfi
   sys close
   sys exit
n.out:
   0156056;0157165;0164040;040040

process: 0
   lac dot+1
   dac lvrand
   lac dot
   sad d3
   jmp proc4
   sza
   jmp proc1
   -1
   tad cmflx+1
   cma
   tad lvrand
   dac lvrand

proc1:
   lac lvrand
   spa
   jmp proc4
   and o17700
   sad bufadd
   jmp proc2
   jms bufwr
   jms copyz; buf; 64
   lac lvrand
   and o17700
   dac bufadd
   dac 1f
   lac bfi
   sys seek; 1: 0; 0
   spa
   jmp proc2
   lac bfi
   sys read; buf; 64

proc2:
   lac lvrand
   and o77
   jms betwen; dm1; maxsto
   dac maxsto
   tad bufp
   dac lvrand
   lac r
   sna
   jmp proc3
   sad d3
   jmp proc5
   lac cmflx+1
   tad r+1
   dac r+1

proc3:
   lac r+1
   dac i lvrand
   jmp i process

proc4:
   jms error; .>
   lac d1
   dac dot
   dzm dot+1
   jmp skip

proc5:
   jms error; u>
   jmp proc3

bufwr: 0
   lac bfo
   sys seek; bufadd: 0; 0
   isz maxsto
   lac bfo
   sys write; bufp: buf; maxsto: -1
   -1
   dac maxsto
   jmp i bufwr

:number: 0
   dac 3f
   lac d1000
   dac 2f
1:
   lac 3f
   cll
   idiv; 2: 0
   dac 3f
   lacq
   tad o60
   dac i 8
   lac 2b
   cll
   idiv; 10
   lacq
   dac 2b
   sza
   jmp 1b
   jmp i number
3: 0

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

sctalp: 0
sctal: 0

betwen: 0
   dac 2f
   lac i betwen
   dac 3f
   isz betwen
   lac i 3f
   cma
   tad 2f
   spa
   jmp 1f
   lac i betwen
   dac 3f
   isz betwen
   lac i 3f
   cma
   tad d1
   tad 2f
   spa
1:
   isz betwen
   lac 2f
   jmp i betwen
2: 0
3: 0

copyz: 0
   -1
   tad i copyz
   dac 8
   isz copyz
   lac i copyz
   cma
   tad d1
   dac 2f
   isz copyz
1:
   dzm i 8
   isz 2f
   jmp 1b
   jmp i copyz
2: 0

error: 0
   lac passno
   sza
   jmp 1f
   isz error
   jmp i error
1:
   -1
   tad mesp
   dac 8
   lac i error
   dac i 8
   lac o40
   dac i 8
   lac rator
   sad d5
   jmp 1f
   lac savchr
   sad o12
   jmp 1f
   lac lineno
   jmp 2f
1:
   -1
   tad lineno
2:
   jms number
   lac o12
   dac i 8
   -2
   tad mesp
   cma
   tad 8
   dac 1f
   lac d1
   sys write; mesp: mes; 1: 0
   isz error
   jmp i error

skip:
   lac rator
   sad d5
   jmp assm1
1:
   jms gchar
   sad d5
   jmp assm1
   jmp 1b

ioinit: 0
   jms copyz; iobuf; 64
   lac iof
   sys read; iobufp: iobuf; 64
   sna
   jms nextfil
   lac iobufp
   dac tal
   -129
   dac talc
   jmp i ioinit

nextfil: 0
   lac d1
   dac lineno
   lac iof
   sza
   sys close
nf1:
   lac narg
   sad d4
   skp
   jmp 1f
   dzm eofflg
   jmp i nextfil
1:
   tad dm4
   dac narg
   lac fname
   tad d4
   dac fname
   sys open; fname: 0; 0
   dac iof
   sma
   lac passno
   sna
   jmp nextfil i
   lac fname
   dac 1f
   lac d1
   sys write; 1; 0; 4
   lac iof
   sma
   jmp 1f
   lac d1
   sys write; emes; 2
   sys exit
1:
   lac d1
   sys write; emes+1; 1
   jmp i nextfil
emes:
   040077;012000

gchar: 0
   lac savchr
   dzm savchr
   sza
   jmp gch3
   lac eofflg
   sza
   jmp 1f
   lac o12
   jmp gch3
1:
   isz talc
   skp
   jms ioinit
   jms getsc; tal
   sna
   jmp gchar+1
   sad o177
   jmp gchar+1
   sad o12
   skp
   jmp 1f
   dzm comflg
   isz lineno
1:
   sad o42
   dac comflg
   dac char
   lac comflg
   sza
   jmp gchar+1
   lac char

gch3:
   dac char
   jms betwen; d0; o200
   cla
   tad lactab
   dac .+1
   lac 0
   jmp i gchar

gsymb: 0
   jms gchar
   dac rator
   tad jmpsw1
   dac 1f
   lac char
   sad o74
   jmp lqot
   dac namc
   jms gchar
   lac char
   sad o76
   jmp rqot
   dac savchr
   lac namc
   dac char
1:
   jmp 0

jmpsw1:
   jmp .+1
   jmp i gsymb
   jmp i gsymb
   jmp i gsymb
   jmp i gsymb
   jmp gs1
   jmp i gsymb
   jmp gs2
   jmp gs3

badchr:
   jms error; g>
1:
   jms gchar
   lac char
   sad o12
   skp
   jmp 1b
   dac savchr
   jmp gsymb+1

lqot:
   jms gchar
   lac o40
   dac savchr
   lac char
   alss 9
   jmp 1f

rqot:
   lac namc
1:
   dac rand+1
   lac d7
   dac rator
   jmp i gsymb

gs1:
   jms gchar
   sad d4
   jmp gs1
   lac char
   dac savchr
   jmp i gsymb

gs2:
   lac namep
   dac tal1
   -7
   dac tal1c
   lac char
   jms putsc; tal1

gnam1:
   jms gchar
   jms betwen; d5; d8
   jmp gnam3
   lac char
   jms putsc; tal1
   isz tal1c
   jmp gnam1

gnam2:
   jms gchar
   jms betwen; d5; d8
   skp
   jmp gnam2
   lac char
   dac savchr
   jms lookup
   jmp i gsymb

gnam3:
   lac char
   dac savchr
1:
   lac o40
   jms putsc; tal1
   isz tal1c
   jmp 1b
   jms lookup
   jmp i gsymb

gs3:
   dzm rand+1
   lac char
   sad o60
   jmp 1f
   lac d10
   jmp 2f
1:
   lac d8
2:
   dac num2

num1:
   lac rand+1
   cll
   mul
num2: 0
   lacq
   tad char
   tad dm48
   dac rand+1
   jms gchar
   sad d7
   jmp num1
   lac char
   dac savchr
   lac rand+1
   jms betwen; dm1; d10
   jmp i gsymb
   dac name
   tad fbxp
   dac name+1
   lac i name+1
   dac name+1
   lac savchr
   sad o146
   jmp 1f
   sad o142
   skp
   jmp i gsymb
   dzm name+1
1:
   dac name+2
   dzm name+3
   lac d6
   dac rator
   jms lookup
   dzm savchr
   jmp i gsymb

tlookup: 0
      jmp 1f
lookup: 0
      dzm tlookup
1:
   -1
   tad namlstp
   dac 8
   lac namsiz
   dac namc
lu1:
   lac i 8
   sad name
   jmp 1f
   lac d5
lu2:
   tad 8
   dac 8
   isz namc
   jmp lu1
      lac tlookup
      sna
      jmp 2f
      lac fnamep
      dac rand+1
      jmp i tlookup
2:
   lac name
   dac i 8
   lac 8
   dac rand+1
   lac name+1
   dac i 8
   lac name+2
   dac i 8
   lac name+3
   dac i 8
   lac d3
   dac i 8
   dzm i 8
   -1
   tad namsiz
   dac namsiz
   jmp i lookup
1:
   lac i 8
   sad name+1
   jmp 1f
   lac d4
   jmp lu2
1:
   lac i 8
   sad name+2
   jmp 1f
   lac d3
   jmp lu2
1:
   lac i 8
   sad name+3
   jmp 1f
   lac d2
   jmp lu2
1:
   -3
   tad 8
   dac rand+1
      lac tlookup
      sza
      jmp i tlookup
   jmp i lookup
namep: name

gpair: 0
   jms gsymb
   lac rator
   sad d4
   jmp gpair+1
   jms betwen; dm1; d6
   jmp gp1
   dzm rand
   dzm rand+1
   jmp i gpair
gp1:
   sad d7
   lac d4
   tad dm4
   dac rand
   jms gsymb
   lac rator
   sad d4
   jmp gp2
   jms betwen; dm1; d6
   skp
   jmp i gpair
   jms error; x>
   jmp skip
gp2:
   jms gchar
   jms betwen; d5; d8
   jmp gp3
   lac char
   dac savchr
   jmp i gpair
gp3:
   lac char
   dac savchr
   jms gsymb
   jmp i gpair

expr: 0
   jms grand
   -1
   dac srand
exp5:
   lac rand
   dac r
   lac rand+1
   dac r+1
exp1:
   lac rator
   jms betwen; d1; d5
   jmp exp3
   dac orator
   jms gpair
   jms grand
   lac orator
   sad d4
   jmp exp2
   jms oper; rand
   jmp exp1
exp2:
   jms pickup
   lac r
   dac srand
   lac r+1
   dac srand+1
   jmp exp5
exp3:
   sad d5
   jmp exp4
   jms error; x>
   jmp skip
exp4:
   jms pickup
   jmp i expr

pickup: 0
   lac srand
   spa
   jmp i pickup
   lac d4
   jms oper; srand
   jmp i pickup

grand: 0
   lac rand
   sad d2
   skp
   jmp i grand
   lac rand+1
   tad d4
   dac rand+1
   lac i rand+1
   dac rand
   isz rand+1
   lac i rand+1
   dac rand+1
   jmp i grand

oper: 0
   tad opsw
   dac oper1
   -1
   tad i oper
   dac 8
   isz oper
   lac r
   sad d3
   jmp oper2
   lac i 8
   sad d3
   jmp oper2
oper1:
   jmp 0
opsw:
   jmp .-1
   jmp oplus
   jmp ominus
   tad r
   dac r
   lac r+1
   lmq
   lac i 8
   omq
   jmp oret
oplus:
   tad r
   dac r
   lac r+1
   tad i 8
   jmp oret
ominus:
   cma
   tad d1
   tad r
   dac r
   -1
   tad i 8
   cma
   tad r+1
oret:
   dac r+1
   lac r
   jms betwen; dm1; d2
   skp
   jmp i oper
   jms error; r>
   lac d1
   dac r
   jmp i oper
oper2:
   dac r
   dzm r+1
   jmp i oper

d0: 0
d1: 1
d4096: 4096
d2: 2
d3: 3
d4: 4
d5: 5
d6: 6
d7: 7
d8: 8
o12: d10: 10
dm1: -1
o40: 040
o60: 060
dm48: -48
o400000: 0400000
o177: 0177
dm4: -4
o200: 0200
o42: 042
o142: 0142
o40040: 040040
o56056: 056056
o56040: 056040
o146: 0146
o17777: 017777
d1000: 1000
o17: 017
o17700: 017700
o77: 077
o74: 074
o76: 076

namsiz: -2
namlstp: namlst
fnamep: fakename
lactab: lac .+1
8;8;8;8;8;8;8;8
8;4;5;8;8;8;8;8
8;8;8;8;8;8;8;8
8;8;8;8;8;8;8;8
4;8;8;8;8;8;8;8
8;8;6;2;4;3;6;8
7;7;7;7;7;7;7;7
7;7;0;5;8;1;8;8
8;6;6;6;6;6;6;6
6;6;6;6;6;6;6;6
6;6;6;6;6;6;6;6
6;6;6;8;8;8;8;8
8;6;6;6;6;6;6;6
6;6;6;6;6;6;6;6
6;6;6;6;6;6;6;6
6;6;6;8;8;8;8;8

fbflg: .=.+1
tal: .=.+1
talc: .=.+1
tal1: .=.+1
tal1c: .=.+1
narg: .=.+1
lvrand: .=.+1
eofflg: .=.+1
namc: .=.+1
passno: .=.+1
char: .=.+1
savchr: .=.+1
comflg: .=.+1
rator: .=.+1
orator: .=.+1
rand: .=.+2
srand: .=.+2
r: .=.+2
name: .=.+4
buf: .=.+64
iobuf: .=.+64
fbx: .=.+10
mes: .=.+20
iof: .=.+1
bfi: .=.+1
bfo: .=.+1
lineno: .=.+1

fakename: .=.+6
namlst:
.=.+4
dot:
.=.+6
cmflx:
