" ald

   law 17
   sys sysloc
   dac crread
   tad d1
   dac crchar

   law 4
   sys sysloc
   tad d14
   dac systime

loop:
   jms holcard
   lac o12
   dac buf+4
   lac d1
   sys write; buf; 5
   law 017
   sys creat; buf
   spa
   jmp ferror
   dac fo
   dzm noc
   law obuf
   dac opt
   dzm seq

cloop:
   jms bincard
   lac buf
   and o700
   sad o500
   skp
   jmp notbin

   -48
   dac c1
   lac buf+3
   dac sum
   dzm buf+3
   law buf-1
   dac 10
   cla
1:
   add 10 i
   isz c1
   jmp 1b
   sad sum
   skp
   jmp badcksum

   lac buf+1
   sad seq
   skp
   jmp badseq

   -1
   tad buf+2
   cma
   dac c1
   law buf+3
   dac 10
1:
   lac 10 i
   jms putword
   isz c1
   jmp 1b

   isz seq
   lac buf
   sma
   jmp cloop
   lac noc
   sna
   jmp 1f
   dac 0f
   lac fo
   sys write; obuf; 0;..
1:
   lac fo
   sys close
   sys exit

holcard: 0
   jms rawcard
   lac 1f
   dac buf
   lac 1f+1
   dac buf+1
   lac 1f+2
   dac buf+2
   lac 1f+3
   dac buf+3
   jmp holcard i
1: <xx>;040040;040040;040040

bincard: 0
   jms rawcard
   -24
   dac c
   law tbuf-1
   dac 8
   law buf-1
   dac 9
1:
   lac 8 i
   alss 6
   dac 1f
   lac 8 i
   dac 1f+1
   lac 8 i
   dac 1f+2
   lac 1f+1
   lrss 6
   xor 1f
   dac 9 i
   lac 1f+1
   alss 12
   xor 1f+2
   dac 9 i
   isz c
   jmp 1b
   jmp bincard i
1: 0;0;0

rawcard: 0
   lac systime i
   tad wtime
   dac tmtime
   -80
   dac c
   law tbuf-1
   dac 8
   crsb
1:
   dzm crread i
2:
   lac systime i
   cma
   tad tmtime
   spa
   jmp timeout
   lac crread i
   sna
   jmp 2b
   lac crchar i
   dac 8 i
   isz c
   jmp 1b
   law
   dac 1f
   isz 1f
   jmp .-1
   jmp rawcard i
1: 0

badcksum:
   lac d1
   sys write; m1; m1s
   jms wait
   jmp cloop

badseq:
   lac d1
   sys write; m2; m2s
   jms wait
   jmp cloop

notbin:
   lac d1
   sys write; m3; m3s
   jms wait
   cmp cloop

timeout:
   lac d1
   sys write; m4; m4s
   jms wait
   jmp rawcard+1

m1:
  <ba>;<d 040; <ch>;<ec>;<ks>;<um>; 012
m1s = .-m1

m2:
   <ba>;<d 040; <se>;<qu>;<en>;<ce>; 012
m2s = .-m2

m3:
   <no>;<t 040; <bi>;<na>;<ry>; 012
m3s = .-m3

m4:
  <ti>;<me>;<ou>;<t 012
m4s = .-m4

wait: 0
   las
   dac 2f
1:
   las
   sad 2f
   jmp 1b
   and d1
   sna
   jmp wait i
   sys save
2: 0

putword: 0
   dac opt i
   isz opt
   isz noc
   lac noc
   sad d2048
   skp
   jmp putword i
   lac fo
   sys write; obuf; 2048
   dzm noc
   law obuf
   dac opt
   jmp putword i
   jmp putword i

d1: 1
d2048: 2048
d14: 14
o500: 0500
o700: 0700
o12: 012
wtime: 300
c: .=.+1
c1: .=.+1
buf: .=.+100
tbuf: .=.+80
fo: .=.+1
seq: .=.+1
sum: .=.+1
obuf: .=.+2048
noc: .=.+1
opt: .=.+1
systime: .=.+1
crread: .=.+1
crchar: .=.+1
tmtime: .=.+1

crsb = 0706744