" ** 11-56-91.pdf page 7
" rm

   lac 017777
   tad d1
   dac 2f
1:
   lac 017777 i
   sad d4
   sys exit
   tad dm4
   dac 017777 i
   lac 2f
   tad d4
   dac 2f
   sys unlink; 2: 0
   sma
   jmp 1b
   lac 2b
   dac 2f
   lac d1
   sys write; 2: 0; 4
   lac d1
   sys write; 1f; 2f-1f
   jmp 1b
1:
   040077;012000
2:

d1: 1
d4: 4
dm4: -4

