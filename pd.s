" ** 11-56-91.pdf page 1
" pd
	
   sys open; dotdot; 0
   spa; jms error
   dac df
   law dir-1
   dac 8
1:
   lac df
   sys read; tbuf; 8
   spa; jms error
   sna
   jmp 1f
   lac tbuf
   sna
   jmp 1b
   -8
   dac c1
   law tbuf-1
   dac 9
2:
   lac 9 i
   dac 8 i
   isz c1
   jmp 2b
   jmp 1b

1:
   lac df
   sys close
   law 017
   sys creat; dotdot
   spa; jms error
   dac df
   law dir-2
   cma
   tad 8
   dac .+4
   lac df
   sys write; dir; ..
   spa; jms error
   lac df
   sys close
   sys exit

error: 0
   -1
   tad error
   hlt
   sys save

dotdot:
   056056; 040040; 040040; 040040

c1: .=.+1
df: .=.+1
tbuf: .=.+8
dir:
