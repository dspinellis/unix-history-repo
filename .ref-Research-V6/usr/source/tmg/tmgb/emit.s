f=r5
g=r4
.globl ofile
.globl dogen
.globl succ
.globl g1,k
.globl emit

emit:
	mov	f,-(sp)
	add	$g1,(sp)
	mov	g,r1
1:			/look for a translation
	cmp	(sp),r1
	bge	3f	/none at all
	bit	-(r1),$100001	
	beq	1b
			/move it to end of stak
	mov	(r1)+,r0
1:
	cmp	g,r1
	ble	1f
	mov	(r1),-2(r1)
	mov	r0,(r1)+
	br	1b
1:
	mov	ofile,r0
	jsr	pc,dogen
	mov	g,r1
1:
	cmp	(sp),r1	/try to find prev value of k
	bge	1f	/nothing more in this stack frame
	tst	-(r1)
	bge	1b	/this isnt a k pointer (negative)
	mov	(r1),k(f)
	br	3f
1:
	mov	f,r0	/go back to prev stack frame
	mov	k(r0),k(f)
3:
	tst	(sp)+
	jmp	succ
