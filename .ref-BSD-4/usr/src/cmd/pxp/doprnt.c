	# C library -- conversions

.globl	__doprnt
.globl	__strout

#define flags r10
#define literb 0
#define liter 1
#define ndfndb 0
#define ndfnd 1
#define ljustb 1
#define ljust 2
#define zfillb 2
#define zfill 4
#define precb 3
#define prec 8
#define psignb 4
#define psign 16
#define gflagb 5
#define gflag 32
#define width r9
#define ndigit r8
#define fdesc -4(fp)
#define exp -8(fp)
#define sign -9(fp)
	.set one,010			# 1.0 in floating immediate
	.set ch.zer,'0			# cpp doesn't like single appostrophes

	.align	1
__doprnt:
	.word	0xfc0			# uses r11-r6
	subl2 $128,sp
	movl 4(ap),r11			# addr of format string
	movl 12(ap),fdesc		# output FILE ptr
	movl 8(ap),ap			# addr of first arg
loop:
	movl r11,r0			# current point in format
	bicl2 $liter,flags		# no literal characters yet
L1:	movb (r11)+,width		# next character of format
	beql L2				# end of format string
	cmpb width,$'%
	beql L2				# warning character
	bisl2 $liter,flags		# literal character
	jbr L1
L2:	blbc flags,L3			# bbc $literb,flags,L3 # no literals in format
	pushl fdesc			# file pointer
	pushl $0			# no left/right adjust
	pushl r0			# addr
	subl3 r0,r11,r1			# length
	subl3 $1,r1,-(sp)		# % or null not part of literal
	calls $4,__strout		# dump the literal
L3:
	blbs width,L4			# % is odd; end of format?
	ret				# yes

	# htab overlaps last 16 characters of ftab
ftab:	.byte	 0, 0, 0,'c,'d,'e,'f,'g, 0, 0, 0,'+,'l,'-,'.,'o
htab:	.byte	'0,'1,'2,'3,'4,'5,'6,'7,'8,'9,'a,'b,'c,'d,'e,'f

L4:	movl sp,r5			# reset output buffer pointer
	clrq r9				# width; flags ljustb,ndfndb,zfillb
L4a:	movzbl (r11)+,r0		# supposed format
	extzv $0,$5,r0,r1		 # bottom 5 bits
L4b:	cmpb r0,ftab[r1]		# good enough?
	jneq L6				# no
L4c:	casel r1,$3,$22			# yes
L5:	.word charac-L5			# c
	.word decimal-L5		# d
	.word scien-L5			# e
	.word float-L5			# f
	.word general-L5		# g
	.word L6-L5			# h
	.word L6-L5			# i
	.word L6-L5			# j
	.word plus-L5			# +
	.word longorunsg-L5		# l
	.word minus-L5			# -
	.word dot-L5			# .
	.word octal-L5			# o
	.word gnum0-L5			# 0
	.word gnum-L5			# 1
	.word gnum-L5			# 2
	.word gnum-L5			# 3
	.word gnum-L5			# 4
	.word gnum-L5			# 5
	.word gnum-L5			# 6
	.word gnum-L5			# 7
	.word gnum-L5			# 8
	.word gnum-L5			# 9

L6:	jbcs $5,r0,L4b			# capitals same as small
	cmpb r0,$'s
	jeql string
	cmpb r0,$'x
	jeql hex
	cmpb r0,$'u
	jeql unsigned
	cmpb r0,$'r
	jeql remote
	movzbl -1(r11),r0		# orginal "format" character
	cmpb r0,$'*
	jeql indir
L9:	movb r0,(r5)+			# print the unfound character
	jbr prbuf

nulstr:
	.byte '(,'n,'u,'l,'l,'),0

string:
	movl ndigit,r0
	jbs $precb,flags,L20		# max length was specified
	mnegl $1,r0			# default max length
L20:	movl (ap)+,r2			# addr first byte
	bneq L21
	movab nulstr,r2
L21:	locc $0,r0,(r2)			# find the zero at the end
	movl r1,r5			# addr last byte +1
	movl r2,r1			# addr first byte
	jbr prstr


longorunsg:
	movb (r11)+,r0
	cmpb r0,$'o
	jeql loct
	cmpb r0,$'x
	jeql lhex
	cmpb r0,$'d
	jeql long
	cmpb r0,$'u
	jeql lunsigned
	decl r11
	jbr unsigned

loct:
octal:
	movl $30,r2			# init position
	movl $3,r3			# field width
	movl $10,r4			# result length -1
	jbr L10

lhex:
hex:
	movl $28,r2			# init position
	movl $4,r3			# field width
	movl $7,r4			# result length -1
L10:	mnegl r3,r6			# increment
	clrl r1
	movl (ap)+,r0			# fetch arg
L11:	extzv r2,r3,r0,r1		# pull out a digit
	movb htab[r1],(r5)+		# convert to character
L12:	acbl $0,r6,r2,L11		# continue until done
	clrb (r5)			# flag end
	skpc $'0,r4,(sp)		# skip over leading zeroes
	jbr prstr

patdec:					# editpc pattern for decimal printing
	.byte 0xA9			# eo$float 9
	.byte 0x01			# eo$end_float
	.byte 0x91			# eo$move 1
	.byte 0				# eo$end

long:
decimal:
	cvtlp (ap)+,$10,(sp)		# 10 digits max
L14:	editpc $10,(sp),patdec,8(sp)	# ascii at 8(sp); r5=end+1
	skpc $' ,$10,8(sp)		# skip leading blanks; r1=first

prstr:			# r1=addr first byte; r5=addr last byte +1
	cvtbl $' ,-(sp)			# blank fill
	jbc $zfillb,flags,L15
	cvtbl $'0,(sp)			# zero fill
L15:	pushl fdesc			# FILE
	subl2 r1,r5			# r5=actual length=end+1-first
	subl3 r5,width,r0		# if >0, how much to fill
	bgeq L24
	clrl r0				# no fill
L24:	jbs $ljustb,flags,L25
	mnegl r0,r0
L25:	pushl r0			# fill count
	pushl r1			# addr first byte
	pushl r5			# length
	calls $5,__strout
	jbr	loop

pone:	.byte	0x1C			# packed 1
	
unsigned:
lunsigned:
	extzv $1,$31,(ap),r0		# right shift logical 1 bit
	cvtlp r0,$10,(sp)		# convert [n/2] to packed
	movp $10,(sp),8(sp)		# copy packed
	addp4 $10,8(sp),$10,(sp)	# 2*[n/2] in packed, at (sp)
	blbc (ap)+,L14			# n was even
	addp4 $1,pone,$10,(sp)		# n was odd
	jbr L14

charac:
	movl $4,r0			# chars per word
L18:	movb (ap)+,(r5)+		# transfer char
	bneq L19
	decl r5				# omit null characters
L19:	sobgtr r0,L18

prbuf:
	movl sp,r1			# addr first byte
	jbr prstr

plus:	bisl2 $psign,flags		# always print sign for floats
	jbr L4a
minus:	bisl2 $ljust,flags		# left justification, please
	jbr L4a
gnum0:	jbs $ndfndb,flags,gnum
	jbs $precb,flags,gnump		# ignore when reading precision
	bisl2 $zfill,flags		# leading zero fill, please
gnum:	jbs $precb,flags,gnump
	moval (width)[width],width	# width *= 5;
	movaw -ch.zer(r0)[width],width	# width = 2*witdh + r0 - '0';
	jbr gnumd
gnump:	moval (ndigit)[ndigit],ndigit	# ndigit *= 5;
	movaw -ch.zer(r0)[ndigit],ndigit # ndigit = 2*ndigit + r0 - '0';
gnumd:	bisl2 $ndfnd,flags		# digit seen
	jbr L4a
dot:	clrl ndigit			# start on the precision
	bisl2 $prec,flags
	bicl2 $ndfnd,flags
	jbr L4a
indir:	movl (ap)+,ndigit		# width specified by parameter
	jbr gnumd
remote:	movl (ap)+,ap
	movl (ap)+,r11
	jbr loop

float:
	bsbw fltcvt
fltg:	jbs $ndfndb,flags,float1
	movl $6,ndigit			# default # digits to right of decpt.
float1:	addl3 exp,ndigit,r7
	movl r7,r6			# for later "underflow" checking
	bgeq fxplrd
	clrl r7				# poor programmer planning
fxplrd:	cmpl r7,$31			# expressible in packed decimal?
	bleq fnarro			# yes
	movl $31,r7
fnarro:	subl3 $17,r7,r0			# where to round
	ashp r0,$17,(sp),$5,r7,16(sp)	# do it
	bvc fnovfl
	# band-aid for microcode error (spurious overflow)
	clrl r0				# assume even length result
	jlbc r7,fleven			# right
	movl $4,r0			# odd length result
fleven:	cmpv r0,$4,16(sp),$0		# top digit zero iff true overflow
	bneq fnovfl
	# end band-aid
	aobleq $0,r6,fnovfl		# if "underflow" then jump
	movl r7,r0
	incl exp
	incl r7
	ashp r0,$1,pone,$0,r7,16(sp)
	ashl $-1,r7,r0			# displ to last byte
	bisb2 sign,16(sp)[r0]		# insert sign
fnovfl:
	movc3 $4,patsci,(sp)
	clrl r6				# # digits moved so far
	movl exp,r0
	bleq fexpng
	bsbb patmov			# digits to left of decpt.
fexpng:	tstl ndigit
	jeql fnodp
	movc3 $2,fpatdp,(r3)
	tstl exp
	bgeq fxppos
	addl3 exp,ndigit,r6
	bgeq  flfakl
	clrl r6				# it's all fill
flfakl:	subl3 r6,$31,r6			# fake length for patmov
flfill:	movc3 $2,fpatzf,(r3)		# zero fill to right of dec.pt
fxppos:	movl ndigit,r0
	bsbb patmov
fnodp:	sobgeq r6,fledit		# must move at least 1 digit
	movl $31,r6			# none moved; fake it
	aobleq $1,ndigit,flfill		# with a one-character zero fill
fledit:	editpc r7,16(sp),(sp),32(sp)
	jbr prflt

patexp:	.byte	0x03			# eo$set_signif
	.byte	0x44,'e			# eo$insert 'e
	.byte	0x42,'+			# eo$load_plus '+
	.byte	0x04			# eo$store_sign
	.byte	0x92			# eo$move 2
	.byte	0			# eo$end
patsci:	.byte	0x42,'+			# eo$load_plus '+
	.byte	0x03			# eo$set_signif
	.byte	0x04			# eo$store_sign
	.byte	0x91			# eo$move 1
fpatdp:	.byte	0x44,'.			# eo$insert '.
fpatzf:	.byte	0x40,'0			# eo$load_fill '0

	# construct pattern at (r3) to move  r0  digits in  editpc;
	#  r6  digits already moved for this number
patmov:
	movb $0x90,r2			# eo$move
	subl3 r6,$31,r1			# # digits remaining in packed
	addl2 r0,r6
	cmpl r0,r1			# enough digits remaining?
	bleq patsml			# yes
	tstl exp			# zero 'fill'; before or after rest?
	bgeq pataft			# after
	pushl r1			# # digits remaining
	movb $0x80,r2			# eo$fill
	subl3 $31,r6,r0			# number of fill bytes
	bsbb patsml			# recursion!
	movl (sp)+,r0
	movb $0x90,r2			# eo$move
	jbr patsml
pataft:	movl r1,r0			# last of the 31
	bsbb patsml			# recursion!
	subl3 $31,r6,r0			# number of fill bytes
	movb $0x80,r2			# eo$fill
patsml:	tstl r0
	bleq patzer			# DEC doesn't like repetition counts of 0
	mnegl $15,r1			# 15 digits at a time
	subl2 r1,r0			# counteract acbl
	jbr pattst
patmlp:	bisb3 r2,$15,(r3)+		# 15
pattst:	acbl $16,r1,r0,patmlp		# until <= 15 left
	bisb3 r2,r0,(r3)+		# rest
patzer:	clrb (r3)			# eo$end
	rsb

scien:
	bsbw fltcvt			# get packed digits
scig:	incl ndigit
	jbs $ndfndb,flags,L23
	movl $7,ndigit
L23:	subl3 $17,ndigit,r0		# rounding position
	ashp r0,$17,(sp),$5,ndigit,16(sp) # shift and round
	bvc snovfl
	# band-aid for microcode error (spurious overflow)
	clrl r0				# assume even length result
	jlbc ndigit,sceven		# right
	movl $4,r0			# odd length result
sceven:	cmpv r0,$4,16(sp),$0		# top digit zero iff true overflow
	bneq snovfl
	# end band-aid
	incl exp			# rounding overflowed to 100...
	subl3 $1,ndigit,r0
	ashp r0,$1,pone,$0,ndigit,16(sp)
	ashl $-1,ndigit,r0		# displ to last byte
	bisb2 sign,16(sp)[r0]		# insert sign
snovfl:
	jbc $gflagb,flags,enotg		# not %g format
 # find trailing zeroes in packed number
	ashl $-1,ndigit,r0
	addl2 r3,r0			# addr of l.s.digit and sign
	movl $4,r1			# bit position of digit
	movl ndigit,r7			# current length of packed
	jbr gtz
gtz1:	xorl2 $4,r1			# position of next digit
	bneq gtz			# same byte
	decl r0				# different byte
gtz:	cmpv r1,$4,(r0),$0		# a trailing zero?
	jneq gntz
	sobgtr r7,gtz1
	incl r7
gntz:					# r7: minimum width of fraction
	cmpl exp,$-4
	jleq eg				# small exponents use %e
	subl3 r7,exp,r0
	cmpl $5,r0
	jleq eg				# so do (w+5) <= exp
	tstl r0				# rest use %f
	jleq fg				# did we trim too many trailing zeroes?
	movl exp,r7			# yes
fg:	subl3 ndigit,r7,r0
	ashp r0,ndigit,16(sp),$0,r7,(sp)
	movp r7,(sp),16(sp)
	subl3 exp,r7,ndigit		# correct ndigit for %f
	jbr fnovfl
eg:	subl3 ndigit,r7,r0
	ashp r0,ndigit,16(sp),$0,r7,(sp)
	movp r7,(sp),16(sp)
	movl r7,ndigit			# packed number has been trimmed
enotg:
	movc3 $7,patsci,(sp)
	movl $1,r6			# 1P
	subl3 $1,ndigit,r0		# digits after dec.pt
	bsbw patmov
	editpc ndigit,16(sp),(sp),32(sp)	# 32(sp)->result, r5->(end+1)
	decl exp			# compensate: 1 digit left of dec.pt
	cvtlp exp,$2,(sp)		# exponent
	editpc $2,(sp),patexp,(r5)
prflt:	movab 32(sp),r1
	jbs $psignb,flags,prflt1
	cmpb (r1)+,$'+
	beql prflt1
	decl r1
prflt1:	skpc $' ,$63,(r1)
	jbr prstr

general:
	jbcs $gflagb,flags,scien
	jbr scien			# safety net

	# convert double-floating at (ap) to 17-digit packed at (sp),
	# set 'sign' and 'exp', advance ap.
fltcvt:
	clrb sign
	movd (ap)+,r5
	jeql fzero
	bgtr fpos
	mnegd r5,r5
	incb sign
fpos:
	extzv $7,$8,r5,r2		# exponent of 2
	movaw -0600(r2)[r2],r2		# unbias and mult by 3
	bgeq epos
	subl2 $9,r2
epos:	divl2 $10,r2
	bsbb expten
	cmpd r0,r5
	bgtr ceil
	incl r2
ceil:	movl r2,exp
	mnegl r2,r2
	cmpl r2,$29			# 10^(29+9) is all we can handle
	bleq getman
	muld2 ten16,r5
	subl2 $16,r2
getman:	addl2 $9,r2			# -ceil(log10(x)) + 9
	bsbb expten
	emodd r0,r4,r5,r0,r5		# (r0+r4)*r5; r0=int, r5=frac
fz1:	cvtlp r0,$9,16(sp)		# leading 9 digits
	ashp $8,$9,16(sp),$0,$17,4(sp)	# as top 9 of 17
	emodd ten8,$0,r5,r0,r5
	cvtlp r0,$8,16(sp)		# trailing 8 digits
	addp4 $8,16(sp),$17,4(sp)	# combine leading and trailing
	bisb2 sign,12(sp)		# and insert sign
	rsb
fzero:	clrl r0
	movl $1,exp		# 0.000e+00 and 0.000 rather than 0.000e-01 and .000
	jbr fz1

	# return 10^r2 as a double float in r0||r1 and 8 extra bits of precision in r4
	# preserve r2, r5||r6
expten:
	movd $one,r0			# begin computing 10^exp10
	clrl r4				# bit counter
	movad ten1,r3			# table address
	tstl r2
	bgeq e10lp
	mnegl r2,r2			# get absolute value
	jbss $6,r2,e10lp		# flag as negative
e10lp:	jbc r4,r2,el1			# want this power?
	muld2 (r3),r0			# yes
el1:	addl2 $8,r3			# advance to next power
	aobleq $5,r4,e10lp		# through 10^32
	jbcc $6,r2,el2			# correct for negative exponent
	divd3 r0,$one,r0		# by taking reciprocal
	mnegl r2,r2
el2:	clrl r4				# 8 extra bits of precision
	rsb

	# powers of ten
	.align	3
ten1:	.word	0x4220,0,0,0
ten2:	.word	0x43c8,0,0,0
ten4:	.word	0x471c,0x4000,0,0
ten8:	.word	0x4dbe,0xbc20,0,0
ten16:	.word	0x5b0e,0x1bc9,0xbf04,0
ten32:	.word	0x759d,0xc5ad,0xa82b,0x70b6
