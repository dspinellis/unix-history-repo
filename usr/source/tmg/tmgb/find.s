i=r3
.globl lptr,rptr,sptr
.globl succ,fail
.globl iget
.globl find,enter
.globl seekchar,getword,getchar
.globl putword,putchar,alterword
.globl getcstr,rewcstr
.globl length,rewind

index=0
tablep=2
temp=4
which=6
framel=10

find:
	mov	pc,-(sp)	/which(sp)
	br	1f
enter:
	clr	-(sp)
1:
	jsr	pc,rewcstr
	jsr	pc,getcstr
	bne	1f
	tst	(sp)+
	jmp	fail
1:
	clr	-(sp)	/temp(sp)
	jsr	pc,iget
	mov	(r0),-(sp)	/tablep(sp)
	clr	-(sp)	/index(sp)

right:
	add	$rptr,index(sp)
	br	1f
left:
	add	$lptr,index(sp)
1:			/get index of next entry
	mov	tablep(sp),r1
	mov	index(sp),r0
	jsr	pc,seekchar
	jsr	pc,getword
	tst	r0
	beq	nomore
	mov	r0,index(sp)
	add	$sptr,r0
	jsr	pc,seekchar
	jsr	pc,rewcstr
1:			/comparison loop
	mov	tablep(sp),r1
	jsr	pc,getchar
	mov	r0,-(sp)
	jsr	pc,getcstr
	cmp	r0,(sp)+
	bgt	right
	blt	left
	tst	r0
	beq	found
	br	1b

nomore:		/not in table
	tst	which(sp)
	beq	1f
	tst	(i)+	/exit from find
	add	$framel,sp
	jmp	fail
1:
	mov	tablep(sp),r1
	jsr	pc,length
	mov	r0,temp(sp)
	clr	r0
	jsr	pc,putword	/scratch word
	jsr	pc,putword	/left pointer
	jsr	pc,putword	/right
	mov	index(sp),r0
	jsr	pc,seekchar
	mov	temp(sp),r0
	mov	r0,index(sp)
	jsr	pc,alterword
	jsr	pc,rewcstr
1:			/copy loop
	jsr	pc,getcstr
	mov	tablep(sp),r1
	jsr	pc,putchar
	tst	r0
	bne	1b

found:
	jsr	pc,iget
	mov	index(sp),(r0)
	add	$framel,sp
	jmp	succ
