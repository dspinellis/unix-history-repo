/
/ getname
/
/ takes the width of a string in r3
/ returns a pointer to a file structure in r0
/
/ there should be a string on the stack
/ of length the contents of r3 on top of
/ a pointer to the file variable
/
/ a new file structure is allocated if needed
/ temporary names are generated, and given
/ names are blank trimmed
/
/ if a new file buffer is allocated, the address
/ is stored throught the file variable pointer
/
/
getname:
	mov	(sp)+,reta
	mov	sp,r2
	mov	r3,r1
	inc	r1
	bic	$1,r1
	add	sp,r1
	mov	(r1)+,r0
	mov	r1,newsp
	tst	(r0)
	bne	gotone
/
/ dont have a file struct
/ so alloc a new one
/
	mov	r0,-(sp)
	mov	(lc),r0
	bne	1f
	mov	$1,r0
1:
	add	$518.+14.,r0
	mov	r0,-(sp)
	jsr	pc,_alloc
	tst	(sp)+
	mov	r0,518.(r0)
	add	$518.+12.,r0
	mov	(lc)+,(r0)+
	bne	1f
	bis	$FTEXT,FUNIT(r0)
	mov	$1,FSIZE(r0)
1:
	mov	(sp),FLEV(r0)
	mov	$fchain-FCHAIN,-(sp)	/ get head of file chain
	mov	fchain,r1		/ contents of head of file chain
1:
	bne	2f			/ end of the chain?
	mov	(sp)+,r1		/ get last chain entry
	mov	r0,FCHAIN(r1)		/ and stuff the current file in
	clr	FCHAIN(r0)		/ last entry
	br	3f
2:
	cmp	FLEV(r1),2(sp)		/ are we past it yet?
	beq	3f
	bhi	2f
	mov	r1,(sp)			/ advance the chain
	mov	FCHAIN(r1),r1
	br	1b
2:
	mov	r1,FCHAIN(r0)
	mov	(sp)+,r1		/ address of last entry
	mov	r0,FCHAIN(r1)		/ stuff in the current entry
3:
	mov	r0,*(sp)+
	br	2f
/
/ have a previous buffer
/ close the associated file
/
gotone:
	tst	(lc)+
	mov	(r0),r1
	mov	FUNIT(r1),r0
	bgt	3f		/ from name from prog hedr
	bic	$100000,FUNIT(r1)
	br	1f
3:
	bit	$FWRITE,FUNIT(r1)
	beq	6f
	mov	r0,-(sp)
	mov	r1,-(sp)
	mov	FBUF(r1),-(sp)
	jsr	pc,_fflush
	tst	(sp)+
	mov	(sp)+,r1
	mov	(sp)+,r0
6:
	bic	$!17,r0
	sys	close
	bes	9f
	bit	$TEMP,FUNIT(r1)
	beq	1f
	tst	r3
	beq	1f
	mov	r0,-(sp)
	mov	r1,-(sp)
	jsr	pc,cleanup
	mov	(sp)+,r1
	mov	(sp)+,r0
1:
	bic	$![TEMP+FTEXT],FUNIT(r1)
	mov	FBUF(r1),r0
	clr	(r0)+		/ clear the getc/putc buffer
	clr	(r0)+
	clr	(r0)+
	mov	r1,r0
/
/ get the filename to the
/ buffer (not necess. easy)
/
2:
	tst	r3
	bne	2f
/
/ no name given
/
	tst	FNAME(r0)
	beq	1f
/
/ no name given and had
/ a prev name so use it
/ again
/
	mov	newsp,sp
	jmp	*reta
9:
	mov	$ECLOSE,_perrno
	error	ECLOSE
/
/ no name given and no
/ prev name so generate
/ a new one of the form
/ "tmp.x"
/
1:
	bis	$TEMP,FUNIT(r0)
	inc	nextu
	movb	nextu,tnam+4
	mov	$tnam,r2
	mov	$6.,-(sp)
	br	alname
/
/ name given
/ strip trailing blanks
/
2:
	bic	$TEMP,FUNIT(r0)
	add	r3,r2
	clrb	(r2)
1:
	cmpb	-1(r2),$' 
	bne	1f
	clrb	-(r2)
	dec	r3
	bne	1b
1:
	sub	r3,r2
	inc	r3
	mov	r3,-(sp)
/
/ save the new name
/ in dynamic space
/
alname:
	mov	r0,r3
	jsr	pc,_alloc
	mov	r0,r1
1:
	movb	(r2)+,(r0)+
	bne	1b
	mov	r1,r2
/
/ free previous file name
/ (if any)
/
	mov	FNAME(r3),r0
	beq	1f
	mov	r0,-(sp)
	jsr	pc,_free
	tst	(sp)+
/
/ put the new name
/ into the structure
/
1:
	mov	r2,FNAME(r3)
	mov	r2,PFNAME(r3)
	mov	r3,r0
	mov	newsp,sp
	jmp	*reta
.bss
reta:	.=.+2
newsp:	.=.+2
.text
	.globl	cleanup
cleanup:
	mov	r2,-(sp)
	mov	4(sp),r2
	mov	PFNAME(r2),0f
	sys	indir;8f
.data
8:	sys	unlink;0: ..
.text
	bec	1f
	mov	PFNAME(r2),_file
	mov	$EREMOVE,_perrno
	error	EREMOVE
1:
	mov	PFNAME(r2),-(sp)
	jsr	pc,_free
	tst	(sp)+
	mov	(sp)+,r2
	rts	pc
