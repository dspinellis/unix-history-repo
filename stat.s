" stat

arg = 017777

   lac arg
   tad d1
   dac name

loop:
   lac arg i
   sad d4
   sys exit
   tad dm4
   dac arg i
   lac name
   tad d4
   dac name

   law statbuf
   sys status; dotdot; name: ..
   spa
   jmp error
   law obuf-1
   dac 12
   lac ii
   lmq
   -3
   jms octal
   lac iflags
   lmq
   -2
   jms octal
   lac iuid
   lmq
   -2
   jms octal
   -1
   tad inlks
   cma
   lmq
   -2
   jms octal
   lac isize
   lmq
   -5
   jms octal
   lac o12
   dac obuf+18
   lac d1
   sys write; obuf; 19
   jmp loop

error:
   lac name
   dac 1f
   lac d1
   sys write; 1:..; 4
   lac d1
   sys write; 1f; 2
   jmp loop
1: 040077; 012

octal: 0
   dac c
   law tbuf-1
   dac 8
   lac o40
   dac 8 i
1:
   lacq
   and o7
   tad o60
   dac 8 i
   lrs 3
   isz c
   jmp 1b
   lac 8
   dac c
1:
   lac c i
   dac 12 i
   sad o40
   jmp octal i
   -1
   tad c
   dac c
   jmp 1b

dotdot:
   056056; 040040; 040040; 040040

d1: 1
d2: 2
d3: 3
d8: 8
d14: 14
d12: 12
o12: 012
o60: 060
o40: 040
o7: 7
d4: 4
dm4: -4

c: .=.+1

statbuf:
   iflags: .=.+1
   .=.+7
   iuid: .=.+1
   inlks: .=.+1
   isize: .=.+1
   .=.+1
   ii: .=.+1

obuf: .=.+19
tbuf: .=.+10
