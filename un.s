" un

   sys open; n.out; 0
   spa
   jmp error
   sys read; buf; 3072
   cll; idiv; 6
   lacq; cma; tad d1
   dac c1
1:
   lac t1
   tad d4
   dac t2
   lac i t2
   sad d3
   skp
   jmp 2f
   lac d1
   sys write; t1: buf; 4
   lac d1
   sys write; mes+1; 1
2:
   lac t1
   tad d6
   dac t1
   isz c1
   jmp 1b

   sys exit

error:
   lac d1
   sys write; mes; 2
   sys exit

mes:
   077; 012
n.out:
   <n.>;<ou>;<t 040;040040

d1: 1
d6: 6
d3: 3
c1: 0
t2: 0
d4: 4

buf:
