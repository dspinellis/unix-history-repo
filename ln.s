" ln

   lac 017777 i
   sad d4
   jmp error
   lac 017777
   tad d1
   dac dirn
   lac dirn i
   sad qli
   jmp clink
   lac 017777
   tad d5
   dac dirn
   dac name

loop:
   lac 017777 i
   sad d8
   sys exit
   tad dm4
   dac 017777 i
   lac name
   tad d4
   dac name
   dac name1
   sys link; dirn: 0; name: 0; name1: 0
   sma
   jmp loop
   lac name
   dac 1f
   lac d1
   sys write; 1: 0; 4
   lac d1
   sys write; errmes; 2
   jmp loop

clink:
   lac 017777 i
   sad d8
   jmp arg1
   sad d12
   jmp arg2
   sad d16
   jmp arg3

error:
   lac d1
   sys write; errmes+1; 1
   sys exit

arg1:
   lac 017777
   tad d5
   dac larg+1
   dac larg+2
   jmp dlink
arg2:
   lac 017777
   tad d5
   dac larg
   tad d4
   dac larg+1
   dac larg+2
   jmp dlink
arg3:
   lac 017777
   tad d5
   dac larg
   tad d4
   dac larg+1
   tad d4
   dac larg+2
dlink:
   sys link; larg: defdir;0;0
   sma
   sys exit
   lac larg
   dac 1f
   lac larg+1
   dac 2f
   lac larg+2
   dac 3f
   lac d1
   sys write; 1:..; 4
   lac d1
   sys write; 2:..; 4
   lac d1
   sys write; 3:..; 4
   lac d1
   sys write; errmes; 2
   sys exit

errmes:
   040; 077012
d1: 1
qli: <li>
d12: 12
d16: 16
defdir: <sy>;<st>;<em>;040040
d4: 4
d8: 8
dm4: -4
d5: 5
