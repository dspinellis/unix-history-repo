'pl132
.pn 1
.de hd
'sp2
.tl '1/15/73''AS (I)'
'sp2
'in16
..
.de im
'sp10
'in 46
..
.de fo
'sp
.tl ''- A% -''
'bp
..
.wh 0 hd
.wh 61 im
.wh -6 fo
.br
.nf
.ti-5
Special variables:
.sp
.li 2
.
..
.sp
.ti -5
Register:
.sp
r0
r1
r2
r3
r4
r5
sp
pc
fr0
fr1
fr2
fr3
fr4
fr5
.sp
.ti -5
System calls:
.sp
exit
fork
read
write
open
close
wait
creat
link
unlink
exec
chdir
time
makdir
chmod
chown
break
stat
seek
tell
mount
umount
setuid
getuid
stime
quit
intr
fstat
cemt
mdate
stty
gtty
ilgins
nice
.sp
.ti -5
Double operand:
.sp
mov	src,dst
movb	   "
cmp	   "
cmpb	   "
bit	   "
bitb	   "
bic	   "
bicb	   "
bis	   "
bisb	   "
add	   "
sub	   "
.sp
.ti -5
Branch:
.sp
br
bne
beq
bge
blt
bgt
ble
bpl
bmi
bhi
blos
bvc
bvs
bhis
bec	(= bcc)
bcc
blo
bcs
bes	(= bcs)
.sp
.ne 5
.ti-5
Single operand:
.sp
clr	dst
clrb	 "
com	 "
comb	 "
inc	 "
incb	 "
dec	 "
decb	 "
neg	 "
negb	 "
adc	 "
adcb	 "
sbc	 "
sbcb	 "
ror	 "
rorb	 "
rol	 "
rolb	 "
asr	 "
asrb	 "
asl	 "
aslb	 "
jmp	 "
swab	 "
tst	src
tstb	src
.sp
.ti -5
Miscellaneous:
.sp
jsr	r,dst
rts	r
sys	exp	(= trap)
.sp
.ti -5
Flag-setting:
.sp
clc
clv
clz
cln
sec
sev
sez
sen
.sp
.ti -5
Floating point ops:
.sp
cfcc
setf
setd
seti
setl
clrf	fdst
negf	fdst
absf	fdst
tstf	fsrc
movf	fsrc,fr	(= ldf)
movf	fr,fdst	(= stf)
movif	src,fr	(= ldcif)
movfi	fr,dst	(= stcfi)
movof	fsrc,fr	(= ldcdf)
movfo	fr,fdst	(= stcfd)
addf	fsrc,fr
subf	fsrc,fr
mulf	fscr,fr
divf	fsrc,fr
cmpf	fsrc,fr
modf	fsrc,fr
.sp
.ti -5
11/45 operations
.sp
ash	src,r
ashc	src,r
mul	src,r
div	src,r
xor	r,dst
sxt	dst
mark	exp
sob	r,exp
.sp
.ti -5
Specials
.sp
.li 9
.byte
.even
.if
.endif
.globl
.text
.data
.bss
.comm
