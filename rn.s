" ** 11-56-91.pdf page 8
" rn

   lac 017777
   tad d1
   dac name2
loop:	
   lac 017777 i
   sad d4
   sys exit
   sad d8
   jmp unbal
   tad dm8
   dac 017777 i
   lac name2
   tad d4
   dac name1
   tad d4
   dac name2
   sys unlink; name2: 0
   lac name2
   dac 1f
   sys rename; name1: 0; 1: 0
   sma
   jmp loop
   lac name1
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; mes; 1
   lac name2
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; mes; 2
   jmp loop
mes:
   040000;077012
unbal:
   lac name2
   tad d4
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; mes; 2
   sys exit

d1: 1
d4: 4
d8: 8
dm8: -8
