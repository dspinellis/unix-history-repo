/
/

/ f21 -- storage allocation
/

.globl	pass2

.globl	signon
.globl	signoff
.globl	calloc
.globl	salloc
.globl	equiv
.globl	entry

pass2:
	jsr	r5,signon; 2
	mov	$errb,errp
	jsr	r5,calloc
	jsr	r5,equiv
	jsr	r5,salloc
	jsr	r5,entry
	jsr	r5,signoff; 2

