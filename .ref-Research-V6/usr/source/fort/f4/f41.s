/
/

/ f41 -- everything else

.globl	pass4

.globl	signon
.globl	flush
.globl	dope
.globl	formts
.globl	globls
.globl	constn
.globl	fopen
.globl	bsss
.globl	tfil2

pass4:
	setd
	jsr	r5,signon; 4
	mov	ibuf,r0
	sys	close
	jsr	r5,flush; obuf
	jsr	r5,flush; tbuf

	jsr	r5,dope
	jsr	r5,formts
	jsr	r5,constn
	jsr	r5,bsss
	jsr	r5,globls		/ uses r4 from bsss
	jsr	r5,flush; obuf
	sys	unlink; tfil2
	clr	r0
	tst	nerror
	beq	1f
	sys	seek; 0; 2
	mov	$1,r0		/ syntax errors
1:
	sys	exit

