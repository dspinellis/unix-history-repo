/
/

/ catch runtime errors:
/ if (ierror(errno)) goto place
/ on call, returns 0
/ on occurrence of error, returns non-zero

.globl	ierror.
.globl	erret

.globl	rerr
.globl	retrn

ierror.:zero
	.+2
	mov	2(r3),r0
	mov	2(r0),r1
	mov	$errbuf,r0
1:
	cmp	r0,cerrp
	blo	2f
	cmp	cerrp,$eerbuf
	blo	3f
	jsr	r5,rerr; 120.
3:
	add	$8,cerrp
	br	1f
2:
	cmp	r1,(r0)
	beq	1f
	add	$8,r0
	br	1b
1:
	mov	r1,(r0)+
	mov	r3,(r0)+
	mov	2(sp),(r0)+
	mov	4(sp),(r0)+
	jmp	retrn

.data
erret:
	jmp	9f
.text
9:
	mov	$errbuf,r0
	cmp	(r0),r1
	beq	1f
	add	$8,r0
	cmp	r0,cerrp
	blo	1b
	rts	pc
1:
	add	$8,r0
	mov	-(r0),-(sp)
	mov	-(r0),-(sp)
	mov	$one,-(sp)
	mov	-(r0),r3
	jmp	retrn

.data
one:	77777; 177777
zero:	0; 0

cerrp:	errbuf

.bss
errbuf:	.=.+50
eerbuf:
