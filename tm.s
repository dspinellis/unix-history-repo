" tm

   sys time
   cll
   div
   216000
   dac t1
   lacq
   cll
   idiv
   10
   tad o60
   dac buf+1
   lacq
   cll; idiv; 10
   tad o60
   dac buf
   lacq
   sna
   jmp 1f
   tad o60
   alss 9
   xor buf
   dac buf
1:
   lac t1
   cll
   idiv
   6
   lacq
   cll
   idiv
   10
   tad o56060
   dac buf+6
   lacq
   cll
   idiv
   10
   tad o60
   dac buf+5
   lacq
   cll
   idiv
   6
   tad o72060
   dac buf+4
   lacq
   cll
   idiv
   10
   tad o60
   dac buf+3
   lacq
   cll
   idiv
   6
   tad o72060
   dac buf+2
   lac d1
   sys write; buf; 8
   sys exit

d1: 1
d13: 13
d2: 2
o60: 060
o56060: 056060
o72060: 072060
t1: 0
t2: 0
buf: .=.+7; 012
