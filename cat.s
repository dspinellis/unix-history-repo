" cat

   lac 017777 i
   sad d4
   jmp nofiles
   lac 017777
   tad d1
   tad d4
   dac name

loop:
   sys open; name: 0; 0
   spa
   jmp badfile
   dac fi

1:
   jms getc
   sad o4
   jmp 1f
   jms putc
   jmp 1b

1:
   lac fi
   sys close

loop1:
   -4
   tad 017777 i
   dac 017777 i
   sad d4
   jmp done
   lac name
   tad d4
   dac name
   jmp loop

badfile:
   lac name
   dac 1f
   lac d8
   sys write; 1:0; 4
   lac d8
   sys write; 1f; 2
   jmp loop1

1: 040;077012
nofiles:
   lac d8
   sys write; 1f; 5
   sys exit

1: <no>; 040;  <fi>;<le>;<s 012

done:
   lac noc
   sns
   sys exit
   and d1
   sna cla
   jmp 1f
   jms putc
   jmp done
1:
   lac noc
   rcr
   dac 1f
   lac fo
   sys write; iopt+1; 1:..
   sys exit

getc: 0
   lac ipt
   sad eipt
   jmp 1f
   dac 2f
   add o400000
   dac ipt
   ral
   lac 2f i
   szl
   lrss 9
   and o177
   sna
   jmp getc+1
   jmp getc i
1:
   lac fi
   sys read; iipt+1; 64
   sna
   jmp 1f
   tad iipt
   dac eipt
   lac iipt
   dac ipt
   jmp getc+1
1:
   lac o4
   jmp getc i

putc: 0
   and o177
   dac 2f+1
   lac opt
   dac 2f
   add o400000
   dac opt
   spa
   jmp 1f
   lac 2f i
   xor 2f+1
   jmp 3f
1:
   lac 2f+1
   alss 9
3:
   dac 2f i
   isz noc
   lac noc
   sad d128
   skp
   jmp putc i
   lac fo
   sys write; iopt+1; 64
   lac iopt
   dac opt
   dzm noc
   jmp putc i
2: 0;0
ipt: 0
eipt: 0
iipt: .+1; .=.+64
fi: 0
opt: .+2
iopt: .+1; .=.+64
noc: 0
fo: 1

d1: 1
o4:d4: 4
d8: 8
o400000: 0400000
o177: 0177
d128: 128
