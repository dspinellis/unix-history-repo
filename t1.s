t=0
main:
  lac 017777 i
  sad d4
  jmp 2f
  sad d8
  jmp 1f
  law 9
  tad 017777
  dac 0f
  law 017
  sys creat;0:0
  dac output
1:law 5
  tad 017777
  dac 0f
  sys open;0:0;0
  dac input
  spa
  sys exit
2:
  jms advance; jmp 1f
  jmp rinterp

0:lac 2f
  jms obuild
1:lac owrite
  spa
  jmp 0b
  jms oflush
  las
  spa
  sys exit
  sys save
2:o777


"special machine language code

"puts out and octal strin g from symtab entry

symoct:
  lac equ
  add equbot
  dac 9f+t
  lac 1f
  jms twoktab; lac 9f+t i
1:gf  .+1 x
  lac ii
  dac 8
  lac 8 i
  add symbot
  dac 9f+t

2:lac 9f+t i
  jms putoct
  lac onenl
  jms obuild
  lac 9f+t i
  and o600600
  sza    "skip unless word contained eof (0777)
  jmp ggoon
  
  isz 9f+t
  jmp 2b
t=t+1
o600600:0600600
