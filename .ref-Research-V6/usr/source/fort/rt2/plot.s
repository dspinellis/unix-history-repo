/
/

/ move, line

sct = 2
/ fortran vt0 interfaces

/ erase screen

erase.:	temp
	.+2
	jsr	pc,setfil
	mov	f,r0
	sys	write; erase; 2
	jmp	retrn

/	move(x, y)
/
/	sets current x,y to arguments
/

.globl	move.
.globl	line.
.globl	erase.
.globl	frame.
.globl	arc.
.globl	circle.

.globl	temp
.globl	retrn

frame.:	temp
	.+2
	jsr	pc,setfil
	mov	2(r3),r0
	movb	2(r0),frame+1
	mov	f,r0
	sys	write; frame; 2
	jmp	retrn

move.:	temp
	.+2
	jsr	pc,garg
	mov	r0,x
	mov	r1,y
	jmp	retrn

/	line(x, y)
/
/	plot vector from current x,y
/	to arguments, setting current
/	x,y when done to arguments
/

line.:	temp
	.+2
	jsr	pc,garg
	mov	r0,x1
	mov	r1,y1
	movb	$3,command
	mov	f,r0
	sys	write; command; 9
	mov	x1,x
	mov	y1,y
	jmp	retrn

/ call circle(x, y, radius)
/ circle's center at x, y

circle.:temp
	.+2
	jsr	pc,garg
	mov	r0,x
	mov	r1,y
	mov	6(r3),r0
	mov	2(r0),r0
	ash	$2,r0
	mov	r0,x1
	movb	$5,command
	mov	f,r0
	sys	write; command; 7
	jmp	retrn

/ call arc(xo, yo, x1, y1, x2, y2)
/ plot circle arc: origin xo,yo;
/ counterclockwise from x1,y1 to x2,y2

arc.:	temp
	.+2
	jsr	pc,garg
	mov	r0,x
	mov	r1,y
	cmp	(r3)+,(r3)+
	jsr	pc,garg
	mov	r0,x1
	mov	r1,y1
	cmp	(r3)+,(r3)+
	jsr	pc,garg
	mov	r0,x2
	mov	r1,y2
	movb	$6,command
	mov	f,r0
	sys	write; command; 13.
	sub	$8.,r3
	jmp	retrn

setfil:
	tst	f
	bne	1f
	sys	creat; vt; 17
	bes	9f
	mov	r0,f
1:
	rts	pc

garg:
	jsr	pc,setfil
	mov	2(r3),r0
	mov	2(r0),r0
	mov	4(r3),r1
	mov	2(r1),r1
	ash	$2,r0
	ash	$2,r1
	sub	$2048.,r0
	sub	$2048.,r1
	rts	pc
9:
	mov	$1,r0
	sys	write; bmes; emes-bmes
	sys	exit

frame:	.byte	4,0
erase:	.byte	1,1
	.byte	0
vt:	</dev/vt0\0>
bmes:	<VT unwritable.\n>; emes:
	.bss
	.=.+1
command:.=.+1
x:	.=.+2
y:	.=.+2
x1:	.=.+2
y1:	.=.+2
x2:	.=.+2
y2:	.=.+2
f:	.=.+2
