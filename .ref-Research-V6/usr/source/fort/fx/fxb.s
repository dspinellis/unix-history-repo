/
/

/ fxb -- get integer constant or label

.globl	geticon
.globl	getlab

.globl	getsym
.globl	geti
.globl	ptemp

geticon:
	jsr	r5,getsym
	cmp	r0,$2
	bne	1f
	cmp	r3,$intcon
	bne	1f
	jsr	r5,geti
	tst	(r5)+
1:
	rts	r5

getlab:
	jsr	r5,geticon
		br 1f
	mov	r0,temp
	jsr	r5,ptemp; 'r; temp; line
	tst	(r5)+
1:
	rts	r5

