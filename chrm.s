" chrm

   lac 017777
   tad d5
   dac 1f
   dac 2f
   lac 017777 i
   sad d4
   sys exit
   tad dm4
   dac 017777 i
   sys chdir; dd
   sys chdir; 1;0
1:
   lac 017777 i
   sad d4
   sys exit
   tad dm4
   dac 017777 i
   lac 2f
   tad d4
   dac 2f
   sys unlink; 2:0
   sma
   jmp 1b
   lac 2b
   dac 2f
   lac d1
   sys write; 2:0; 4
   lac d1
   sys write; 1f; 2
   jmp 1b
1:
   040077;012000

dd:
   <dd>;040040;040040;040040
d1: 1
d4: 4
d5: 5
dm4: -4