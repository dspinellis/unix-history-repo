" ttt1.s

t = 0
   lac 017777
   tad d1
   dac 9f+t
   lac 9f+t i
   sad qdt
   skp
   jmp loop
   dac dspflg
   law sbuf-1
   dac blk1
   dac blk2
   jms dsboard
   law dbuf
   sys capt
   law 16
   sys sysloc
   tad d1
   dac lpadr
   law 13
   sys sysloc
   dac pbadr
   dzm lpadr i
loop:
   jms move
   jms must; jmp loop
   lac imaxin
   dac maxin
   law stack-1
   dac 10
   jms try; jmp unwind
   jms heur; jmp loop

unwind:
   lac 12
   dac 9f+t
1:
   lac 9f+t i
   jms mark; 1
   -1
   tad 9f+t
   dac 9f+t
   lac 9f+t i
   spa
   jmp done
   jms mark; 512
   -1
   tad 9f+t
   dac 9f+t
   lac dspflg
   sna
   jmp 1b
   jms getpb
   sza
   jmp .-2
   jms getpb
   sna
   jmp .-2
   jmp 1b
t = t+1

move: 0
   lac dspflg
   sza
   jmp dspmove
   jms messg; m>;o>;v>;e>;0
   dzm 9f+t
1:
   jms getc
   sad o12
   jmp 1f
   tad om60
   spa
   jmp move+1
   dac 9f+t+1
   and o3
   sad 9f+t+1
   skp
   jmp move+1
   lac 9f+t
   alss 2
   xor 9f+t+1
   dac 9f+t
   jmp 1b
1:
   lac 9f+t
   spa
   jmp move+1
   and o77
   sad 9f+t
   skp
   jmp move+1
   tad boardp
   dac 9f+t
   lac 9f+t i
   sza
   jmp move+1
   lac 9f+t
   jms mark; 512
   jmp move i

dspmove:
   jms getpb
   sza
   jmp pbhit
   lac lpadr i
   sna
   jmp dspmove

lphit:
   lmq
   lac blink
   sna
   jmp 1f
   cma
   tad boardp
   cma
   alss 4
   tad sbufp
   dac 9f+t
   lac noblink
   dac blk1 i
   dac blk2 i
   dac 9f+t i
1:
   dzm lpadr i
   lacq
   cma
   tad sbufp
   cma
   lrss 4
   dac 9f+t
   and o77
   sad 9f+t
   skp
   jmp dspmove
   tad boardp
   dac blink
   lac 9f+t
   alss 4
   tad sbufp
   dac 9f+t
   lac blinkpar
   dac 9f+t i
   jmp dspmove

pbhit:
   lac blink
   sna
   jmp dspmove
   jms mark; 512
   dzm blink
   jmp move i
t = t+2

must: 0

" check for 3g.4g.4b

   law line-1
   dac 8
   -76
   dac 9f+t
   dzm 9f+t+1
   dzm 9f+t+2
1:
   cla
   xct 8 i
   xct 8 i
   xct 8 i
   xct 8 i
   sad o4
   jmp done
   sad o4000
   jmp done
   sad o3
   dac 9f+t+2
   sad o3000
   skp
   jmp 2f
   -4
   tad 8
   dac 9
   cla
   xct 9 i
   sza cla
   jmp .-2
   -1
   tad 9
   dac 9
   lac 9 i
   dac 9f+t+1
2:
   isz 9f+t
   jmp 1b
   lac 9f+t+2
   sza
   jmp 1f
   lac 9f+t+1
   sna
   jmp 1f
   jms mark; 1
   jmp must i
1:
   isz must
   jmp must i
t = t+3

done:
   lac dspflg
   sza
   jmp 1f
   jms messg; e>;x>;i>;t>;0
   sys exit
1:
   law sbuf-1
   dac blk1
   dac blk2
   jms dsboard
   law line-1
   dac 8
   -76
   dac 9f+t
1:
   cla
   xct 8 i
   xct 8 i
   xct 8 i
   xct 8 i
   sad o4
   jmp 1f
   sad o4000
   jmp 1f
   isz 9f+t
   jmp 1b
   sys exit
1:
   -4
   tad 8
   dac 9
1:
   lac 9 i
   cma
   tad boardp
   cma
   alss 4
   tad sbufp
   dac 9f+t
   lac blinkpar
   dac 9f+t i
   lac 8
   sad 9
   skp
   jmp 1b
   jms getpb
   sza
   jmp .-2
   jms getpb
   sna
   jmp .-2
   sys exit
t = t+1

mark: 0
   dac 9f+t
   lac mark i
   dac 9f+t i
   isz mark
   lac dspflg
   sna
   jmp 1f
   lac blk1
   dac blk2
   lac 9f+t
   cma
   tad boardp
   cma
   alss 4
   tad sbufp
   dac blk1
   jms dsboard
   jmp mark i
1:
   lac 9f+t i
   sad d1
   skp
   jmp mark i
   lac 9f+t
   cma
   tad boardp
   cma
   dac 9f+t
   lrs 4
   and o3
   tad o60
   dac 0f
   lac 9f+t
   lrs 2
   and o3
   tad o60
   dac 0f+1
   lac 9f+t
   and o3
   tad o60
   dac 0f+2
   jms messg; 0:0;0;0;0
   jmp mark i
t = t+1

try: 0

" check 3g or 3b

   law line-1
   dac 9
   -76
   dac 9f+t+3
   dzm 9f+t+4
1:
   cla
   xct 9 i
   xct 9 i
   xct 9 i
   xct 9 i
   sad o3000
   dac 9f+t+4
   sad o3
   skp
   jmp 2f
   lac 10
   dac 12
   -1
   dac 12 i
   -4
   tad 9
   dac 9
   cla
   xct 9 i
   sza cla
   jmp .-2
   -1
   tad 9
   dac 9
   lac 9 i
   dac 12 i
   lac dspflg
   sza
   jmp try i
   jms messg; i>;040;w>;i>;n>;0
   jmp try i
2:
   isz 9f+t+3
   jmp 1b
   lac 9f+t+4
   sna
   jmp 1f
   isz try
   jmp try i

" save

1:
   isz maxin
   jmp 1f
   -1
   dac maxin
   isz try
   jmp try i
1:
   lac try
   dac 10 i
   lac 8
   dac 10 i
   lac 9f+t
   dac 10 i
   lac 9f+t+1
   dac 10 i
   lac 9f+t+2
   dac 10 i

" check 2g

   law line-1
   dac 8
   -76
   dac 9f+t
1:
   cla
   xct 8 i
   xct 8 i
   xct 8 i
   xct 8 i
   sad o2
   skp
   jmp 2f
   -4
   tad 8
   dac 9
   cla
   xct 9 i
   sza cla
   jmp .-2
   lac 9
   dac 9f+t+1
   lac 9f+t+1 i
   dac 9f+t+1
   cla
   xct 9 i
   sza cla
   jmp .-2
   lac 9
   dac 9f+t+2
   lac 9f+t+2 i
   dac 9f+t+2

" recurse

   lac d1
   dac 9f+t+1 i
   lac o1000
   dac 9f+t+2 i
   jms try; jmp prnt
   lac d1
   dac 9f+t+2 i
   lac o1000
   dac 9f+t+1 i
   jms try; jmp prnt
   dzm 9f+t+1 i
   dzm 9f+t+2 i
2:
   isz 9f+t
   jmp 1b

" restore
   
   -5
   tad 10
   dac 10
   dac 9
   lac 9 i
   dac try
   lac 9 i
   dac 8
   lac 9 i
   dac 9f+t
   lac 9 i
   dac 9f+t+1
   lac 9 i
   dac 9f+t+2
   isz try
   jmp try i

prnt:
   lac 9f+t+1 i
   sad d1
   jmp 1f
   lac 9f+t+1
   dac 12 i
   lac 9f+t+2
   jmp 2f
1:
   lac 9f+t+2
   dac 12 i
   lac 9f+t+1
2:
   dac 12 i
   dzm 9f+t+1 i
   dzm 9f+t+2 i
   -5
   tad 10
   dac 10
   dac 9
   lac 9 i
   dac try
   lac 9 i
   dac 8
   lac 9 i
   dac 9f+t
   lac 9 i
   dac 9f+t+1
   lac 9 i
   dac 9f+t+2
   jmp try i
t = t+5

heur: 0
   jms addpri
   -2
   tad force
   dac lforce
   -1000
   dac lpri
   -64
   dac 9f+t
   lac boardp
   dac 9f+t+1
1:
   lac 9f+t+1 i
   sza
   jmp 3f
   lac d1
   dac 9f+t+1 i
   jms addpri
   lac force
   sad lforce
   jmp 4f
   lac pri
   cma
   tad lpri
   sma cma
   jmp 3f-1
   sza
   jmp 2f
   isz prob
   -1
   cll; idiv; prob:..
   lacq
   lrss 6
   dac force
   sys time
   lacq
   tad rand
   cll; mul; 37111
   lacq
   dac rand
   cll; lrs 6
   cma
   tad force
   spa
   jmp 3f-1
   jmp 2f+2
2:
   lac d1
   dac prob
   lac pri
   dac lpri
   lac 9f+t+1
   dac lmov
   dzm 9f+t+1 i
3:
   isz 9f+t+1
   isz 9f+t
   jmp 1b
   lac lmov
   jms mark; 1
   jmp heur i
4:
   lac 9f+t+1
   jms mark; 1
   jmp heur i
t = t+2
   
addpri: 0
   clq
   law line-1
   dac 8
   -76
   dac 9f+t
   dzm force
1:
   cla
   xct 8 i
   xct 8 i
   xct 8 i
   xct 8 i
   sad o2000
   isz force
   dac 9f+t+1
   rtr;rtr;rtr;rar
   xor 9f+t+1
   and o77
   tad 2f
   dac .+2
   lacq
   tad ..
   lmq
   isz 9f+t
   jmp 1b
   law plane-1
   dac 8
   -18
   dac 9f+t
1:
   -16
   dac 9f+t+1
   cla
0:
   xct 8 i
   isz 9f+t+1
   jmp 0b
pstrat:
   jms 3f; 04002; 1
   jms 3f; 03001; 15
   jms 3f; 04001; 20
   jms 3f; 1; 1
   isz 9f+t
   jmp 1b
   lacq
   dac pri
   jmp addpri i
2: tad pritab
3: 0
   sad 3b i
   jmp 1f
   isz 3b
   isz 3b
   jmp 3b i
1:
   isz 3b
   lacq
   tad 3b i
   lmq
   cla
   isz 3b
   jmp 3b i
t = t+2

dsboard: 0
   -64
   dac 9f+t
   dzm 9f+t+2
   law board-1
   dac 8
   law sbuf-1
   dac 11

8:
   lac noblink
   dac 11 i
   lac 9f+t+2
   and o3
   alss 6
   dac 9f+t+3
   alss 1
   tad 9f+t+3
   dac 9f+t+3
   lac 9f+t+2
   and o14
   alss 4
   tad 9f+t+3
   xor setxv
   dac 11 i
   lac 9f+t+2
   and o74
   alss 4
   xor setyv
   dac 11 i
   lac 8 i
   sna
   jmp 4f
   sad d1
   jmp 3f
   law ex-1
   jmp 2f
3: law oh-1
   jmp 2f
4: law dot-1
2: dac 9
   -12
   dac 9f+t+1
1:
   lac 9 i
   dac 11 i
   isz 9f+t+1
   jmp 1b
   lac noblink
   dac 11 i
   isz 9f+t+2
   isz 9f+t
   jmp 8b
   -1
   dac 11 i
   lac blinkpar
   dac blk1 i
   dac blk2 i
   jmp dsboard i
t = t+4

getc: 0
   cla
   sys read; 9f+t; 1
   sna spa
   sys save
   lac 9f+t
   lrss 9
   jmp getc i

messg: 0
   -1
   tad messg
   dac 9
1:
   lac 9 i
   sna
   jmp 1f
   dac 9f+t
   lac d1
   sys write; 9f+t; 1
   jmp 1b
1:
   lac d1
   sys write; o12; 1
   jmp 9 i
t = t+1

getpb: 0
   sys time
   lac pbadr i
   and o2000
   sza
   sys exit
   lac pbadr i
   jmp getpb i
