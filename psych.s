" ** 11-56-91.pdf page 2
" psych

   lac d1
   sys close
   lac d13 "pushbuttons
   sys sysloc
   dac .pbp
   tad d1
   dac .pbp1
restart:
   fld; arg1		"[   g = arg1						- scan markup]
   fst; g		"[   fact = arg2					- scan markup]
   fld; arg2		"[   d = 1.0						- scan markup]
   fst; fact		"[   z = 0						- scan markup]
   fld; fp01		"[   oldx = 0						- scan markup]
   fst; d		"[   oldy = 0						- scan markup]
   jms capture		"[loop:							- scan markup]
   -100			"[   b = z - 110.					- scan markup]
   dac j		"[      ----------					- scan markup]
   fld; fp0		"[       radians					- scan markup]
   fst; z		"[   z = z + g						- scan markup]
   fst; oldx		"[   g = -g * fact					- scan markup]
   fst; oldy		"[   fact = 1 / fact					- scan markup]
   lac o400000		"[   xx = 500. * d * sin(b + pi/2)			- scan markup]
   dac i 11		"[   ni()						- scan markup]
   dac i 11		"[   yy = 500. * d * sin(b)				- scan markup]
   lac setx		"[   ni()						- scan markup]
   dac i 10		"[   goto loop	" an arrow points up to loop: above	- scan markup]
   lac sety
   dac i 10

loop:
   sys time
   fld; z
   fad; fm110
   fdv; radians
   fst; b
   fld; z
   fad; g
   fst; z
   fld; g
   fng
   fmp; fact
   fst; g
   fld; fp1
   fdv; fact
   fst; fact
   fld; b
   fad; fpid2
   sin
   fmp; d
   fmp; fp500
   fst; xx
   jms in
   jmp done
   fld; b
   sin
   fmp; d
   fmp; fp500
   fst; yy
   jms in
   jmp done
" ** 11-56-91.pdf page 3
   lac o400000
   dac i 11
   dac i 11
   fld; oldx
   fng
   fad; xx
   fix
   spa
   xor o775777
   tad vecx
   dac i 10
   fld; oldy
   fng
   fad; yy
   fix
   spa
   xor o775777
   tad vecy
   dac i 10
   fld; xx
   fst; oldx
   fld; yy
   fst; oldy
   isz j
   skp
   jmp done
   fld; g
   fdv; fp90000
   fad; d
   fst; d
   jmp loop

done:
   -1000
   dac 9f
   sys time
   isz 9f
   jmp .-2
   skp
9f:0
   dzm char
   lac auto
   sza
   jmp doauto
1:
   sys time " swap
   lac i .pbp
   sna
   jmp 1b
   spa ral
   jmp 1f
   spa ral
   jmp 2f
   spa ral
   jmp 3f
   spa ral
   jmp 4f
   spa ral
   jmp 5f
   spa ral
   jmp 6f
" ** 11-56-91.pdf page 4
   spa ral
   jmp 7f
   jms release
   sys exit
7:
   cla
   sys read; tmp; 1
   lac tmp
   sad o141000
   skp
   jmp 0f
   dac auto
   dzm i .pbp1
   jmp doauto
0:
   sad o12000
   jmp restart
   dzm g
9:
   cla
   sys read; char; 1
   lac char
   sad o12000
   jmp 9f
   lrss 9
   dac char
   lac g
   alss 3
   tad char
   tad om60
   dac g
   jmp 9b
9:
   lac tmp
   sad o61000
   jmp 9f
   lac g
   dac arg2
   jmp restart
9:
   lac g
   dac arg1
   jmp restart
5:
   isz fp500
   nop
   jmp restart
6:
   -1
   tad fp500
   dac fp500
   jmp restart
4:
   lac o400000
   dac char
3:
   fld; arg1
   fmp; fm056
   lac char
   spa
   fng
" ** 11-56-91.pdf page 5
   fad; arg1
   fst; arg1
   jmp restart
2:
   lac o400000
   dac char
1:
   fld; arg2
   fmp; fm056
   lac char
   spa
   fng
   fad; arg2
   fst; arg2
   jmp restart

doauto:
   lac i .pbp1
   sma
   jmp 1f
   dzm auto
   jmp done
1:
   sys time
   omq
   tad 0
   tad stick
   cll
   mul
   78625
   lls 9
   dac stick
   dzm aexp
   and o177777
   xor o200000
   dac ams
   fad; fmhalf
   fmp; fp128
   lac stick
   and d1
   sna
   jmp 1f
   fst; arg1
   jmp restart
1:
   fst; arg2
   jmp restart

in: 0
   fix
   tad d500
   spa
   jmp i in
   tad dm1000
   spa
   isz in
   jmp i in

capture: 0
   law buf-1
   dac 10
" ** 11-56-91.pdf page 6
   dac 11
   lac o400000
   dac i 11
   law buf
   sys capt
   jmp i capture

release: 0
   sys rele
   jmp i release

char: 0
stick: 0
xx: 0; 0; 0
oldx: 0; 0; 0
yy: 0; 0; 0
oldy: 0;0 0; 0
j: 0
auto: 0

setx: 0142000 +512
sety: 0146000 +512
vecx: 0100000
vecy: 0124000
o775777: 0775777
o177777: 0177777
o12000: 012000
o61000: 061000
om60: -060
d13: 13
d500: 500
o141000: 0141000
fp128:8; 0240000; 0
fmhalf: 0; 0600000; 0
dm1000: -1000

fact: 0;0;0
g: 0;0;0
z: 0;0;0
b: 0;0;0
d: 0;0;0

radians: 6;0345136;0
fm110: 7;0734000;0
arg1: 7;0234167;0
arg2: 1;0275531;0
fp0: 0;0;0
fp01: -6;0243656;0
fm056: -4;0745301;0
fp500: 11;0372000;0
fp90000: 021;0257620;0
   .pbp: .=.+1
   .pbp1: 0
buf:
