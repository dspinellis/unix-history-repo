/
/

/ f31 - pass3 dispatcher
/
/	main scan loop for pass3
/	picks up executable statements
/

.globl	pass3
.globl	ifstmt

.globl	isagn
.globl	signon
.globl	signoff
.globl	getline
.globl	lookup
.globl	sasgn
.globl	error
.globl	perror
.globl	doend
.globl	sform
.globl	sdata
.globl	sdo
.globl	sassi
.globl	scall
.globl	scont
.globl	sretu
.globl	sgoto
.globl	sif
.globl	spaus
.globl	sstop
.globl	sread
.globl	sprin
.globl	swrit
.globl	srewi
.globl	sback
.globl	sendf
.globl	blocks
.globl	blockp
.globl	code
.globl	ptemp
.globl	dotabp

pass3:
	jsr	r5,signon; 3
	br	2f

scan3:
	jsr	r5,getline
2:
	mov	$blocks,blockp
	mov	$line,r1
	mov	r1,r2
	jsr	r5,lookup; fmttab
		br 2f
	mov	r0,-(sp)
	jsr	r5,isagn
		br 1f
	mov	(sp)+,r0
	jsr	r5,*fmtlst(r0)
	br	scan3
1:
	tst	(sp)+
2:
	mov	efno,r0
	beq	1f
	jsr	r5,ptemp; 's; efno; line
	jsr	r5,code
		<.%d:\n\0>; .even
		r0
1:
	jsr	r5,isagn
		br 1f
	mov	r1,r2
	jsr	r5,lookup; stmtab
		br 2f
	mov	r2,r1
	jsr	r5,*sublst(r0)
	br	3f
1:
	jsr	r5,sasgn
	br	3f
2:
	jsr	r5,error; 101.
3:
	cmp	progt,$6
	bne	1f
	jsr	r5,error; 50. / execut in block data
1:
	jsr	r5,perror
	mov	efno,r0
	beq	scan3
	jsr	r5,doend
	br	scan3

ifstmt:
	mov	$blocks,blockp
	jsr	r5,isagn
		br 1f
	mov	r1,r2
	jsr	r5,lookup; stmtab1
		br 2f
	mov	r2,r1
	cmp	r0,$4		/ don't allow end
	beq	2f
	jmp	*sublst1(r0)
1:
	jmp	sasgn
2:
	jsr	r5,error; 101.
	rts	r5

send:
	tst	dotabp
	beq	1f
	clr	r0
	jsr	r5,doend
	br	send
1:
	tst	progt
	bne	1f
	jsr	r5,sstop
	br	2f
1:
	cmp	progt,$6		/ block data
	beq	2f
	jsr	r5,sretu
2:
	jsr	r5,perror
	jsr	r5,signoff; 3

sublst:
	sdo
sublst1:
	sif
	sendf
	send
	sassi
	scall
	scont
	sretu
	sgoto
	spaus
	sstop
	sread
	sprin
	swrit
	srewi
	sback
fmtlst:
	sform
	sdata
stmtab:
	<do\0>
stmtab1:
	<if(\0>
	<endfile\0>
	<end\0>		/ keep in this spot!!!
	<assign\0>
	<call\0>
	<continue\0>
	<return\0>
	<goto\0>
	<pause\0>
	<stop\0>
	<read\0>
	<print\0>
	<write\0>
	<rewind\0>
	<backspace\0>
	<\0>

fmttab:
	<format\0>
	<data\0>
	<\0>
	.even

