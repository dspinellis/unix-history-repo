i = r3
.globl putcall,kput,obuild
.globl seekchar
.globl  getchar
.globl	outb, outw, outt
.globl	cfile, lfile, flush
.globl iget
.globl sptr
.globl generate,succ
.globl getnam

getnam:
	mov	$1f+1,r0
	jsr	pc,putcall
	jsr	pc,iget
	mov	(r0),r0
	jsr	pc,kput
	jsr	pc,iget
	mov	(r0),r0
	jsr	pc,kput
	jmp	succ
1:
	cmp	cfile,lfile
	beq	1f
	jsr	pc,flush
	mov	cfile,lfile
1:
	mov	(i)+,r1
	mov	r1,-(sp)
	mov	(i)+,r0
	add	$sptr,r0
	jsr	pc,seekchar
2:
	mov	(sp),r1
	mov	outw,r2
1:
	jsr	pc,getchar
	tst	r0
	beq	1f
	movb	r0,outb(r2)
	inc	r2
	mov	r2,outw
	cmp	r2,$outt
	blt	1b
/
	jsr	pc,flush
	br	2b
1:
	tst	(sp)+
	jmp	generate
