.globl	rewind
.globl	create
.globl	fsfile
.globl	w, r, a, l
/
/
/	routine to rewind read pointer of string
/	pointed to by r1
/
/	mov	...,r1
/	jsr	pc,rewind
/
rewind:
	mov	a(r1),r(r1)
	rts	pc
/
/	routine to rewind write pointer of string
/	pointed to by r1
/
/	mov	...,r1
/	jsr	pc,create
/
create:
	mov	a(r1),w(r1)
	mov	a(r1),r(r1)
	rts	pc
/
/
/	routine to copy read pointer of string to end of string
/
/	mov	...,r1
/	jsr	pc,fsfile
/
fsfile:
	mov	w(r1),r(r1)
	rts	pc
