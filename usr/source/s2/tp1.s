/ tap1 -- dec-disk lod/dmp

.globl	_localtime, _end
namep = 0
mode = 2
uid = 4; gid = 5
size0 = 7
size1 = 8
time0 = 10.
time1 = 12.
tapea = 14.
dirsiz = 16.
mdirent = 496.

	mov	(sp),rnarg
	mov	(sp)+,narg
	mov	$cmr,command
	incb	flu
	tst	(sp)+
	cmp	narg,$2
	bge	1f
	mov	$2,narg
	br	3f
1:
	mov	(sp)+,r0
	mov	sp,parg
1:
	movb	(r0)+,r1
	beq	3f
	mov	$swtab,r2
2:
	cmp	r1,(r2)+
	beq	2f
	tst	(r2)+
	bne	2b
	br	useerr
2:
	jsr	pc,*(r2)+
	br	1b
3:
	jsr	pc,optap
	mov	$_end,r4 / string pointer
	jsr	pc,setb
	jmp	*command

optap:
	tstb	flm
	bne	2f
	mov	$578.,tapsiz
	mov	$192.,ndirent
	sys	open; tc; 2
	br	3f
2:
	mov	$-1,tapsiz
	mov	$mdirent,ndirent
	cmp	command,$cmr
	beq	2f
	sys	open; mt; 0
	br	3f
2:
	sys	open; mt; 1
3:
	bes	1f
	mov	r0,fio
	mov	ndirent,r1
	ash	$-3,r1
	mov	r1,ndentd8
	mov	ndirent,r1
	mul	$dirsiz,r1
	add	$dir,r1
	mov	r1,edir
	rts	pc
1:
	jsr	r5,mesg
		<Tape open error\n\0>; .even
	jmp	done

setcom:
	cmp	command,$cmr
	bne	useerr
	mov	(r5)+,command
	rts	r5

noflag:
	mov	(r5)+,r0
	beq	1f
	tstb	(r0)
	beq	noflag
	br	useerr
1:
	rts	r5

useerr:
	jsr	r5,mesg
		<Bad usage\n\0>; .even
	jmp	done

swtab:
	'0; dcof
	'1; dcof
	'2; dcof
	'3; dcof
	'4; dcof
	'5; dcof
	'6; dcof
	'7; dcof
	'c; dcc
	'd; dcd
	'f; dcf
	'i; dci
	'm; dcm
	'r; dcr
	't; dct
	'u; dcu
	'v; dcv
	'w; dcw
	'x; dcx
	 0; 0

dcof:
	movb	r1,tcx
	movb	r1,mtx
	rts	pc

dcc:
	incb	flc
	rts	pc

dcf:
	incb	flf
	rts	pc

dcd:
	jsr	r5,setcom; cmd
	rts	pc

dci:
	incb	fli
	rts	pc

dcm:
	incb	flm
	rts	pc

dcu:
	incb	flu
	jsr	r5,setcom; cmr
	rts	pc

dcr:
	clrb	flu
	jsr	r5,setcom; cmr
	rts	pc

dct:
	jsr	r5,setcom; cmt
	rts	pc

dcv:
	incb	flv
	rts	pc

dcw:
	incb	flw
	rts	pc

dcx:
	jsr	r5,setcom; cmx
	rts	pc

cmd:
	jsr	r5,noflag; flm; flc; flf; 0
	cmp	narg,$2
	bgt	1f
	jmp	useerr
1:
	jsr	pc,rddir
	jsr	r5,gettape; delete
	jsr	pc,wrdir
	br	check

cmr:
	jsr	r5,noflag; 0
	tstb	flc
	bne	1f
	tstb	flm
	bne	1f
	jsr	pc,rddir
	br	2f
1:
	jsr	pc,clrdir
2:
	jsr	pc,getfiles
	jsr	pc,update
	br	check

cmt:
	jsr	r5,noflag; flc; flf; flw; 0
	jsr	pc,rddir
	tstb	flv
	beq	1f
	jsr	r5,mesg
	<   mode    uid gid tapa    size   date    time name\n\0>; .even
1:
	jsr	r5,gettape; taboc
	br	check1

cmx:
	jsr	r5,noflag; flc; flf; 0
	jsr	pc,rddir
	jsr	r5,gettape; xtract
	br	done

check:

check1:
	jsr	pc,usage

done:
	jsr	r5,mesg
		<END\n\0>; .even
	sys	exit

encode:
	mov	r2,-(sp)
	mov	r4,(r1)
	mov	(r5)+,r2
1:
	movb	(r2),(r4)+
	jsr	pc,setb
	tstb	(r2)+
	bne	1b
	mov	(sp)+,r2
	rts	r5

decode:
	mov	r2,-(sp)
	mov	r1,-(sp)
	mov	(r1),r1
	mov	(r5)+,r2
1:
	movb	(r1)+,(r2)+
	bne	1b
	mov	(sp)+,r1
	mov	(sp)+,r2
	rts	r5

setb:
	mov	r0,-(sp)
	mov	r4,r0
	add	$513.,r0
	cmp	r0,sp
	bhis	2f
	bic	$777,r0
	cmp	r0,0f
	beq	1f
	mov	r0,0f
	sys	break; 0:..
	bes	2f
1:
	mov	(sp)+,r0
	rts	pc

2:
	jsr	r5,mesg
		<Out of core\n\0>; .even
	jmp	done
