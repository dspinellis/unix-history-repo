/
/

/ bas0 -- basic

scope = 1
.globl	main
.globl	sin, cos, log, exp, atan, pow, sqrt
.globl	rand, srand
.globl	fptrap
.globl fopen, getc

indir =	0  /for  indirect sys calls. (not in as)
one = 40200

main:
	mov	$1,prfile /initial print file
	sys	signal; 4; fptrap
	setd
	sys	time
	mov	r1,r0
	mov	r0,randx
	jsr	pc,srand
	sys	signal; 2; intrup
	mov	sp,gsp
	clr	seeka
	mov	$'a,r1
1:
	movb	r1,tmpf+8
	sys	stat; tmpf; line
	bes	1f
	inc	r1
	cmp	r1,$'z
	blos	1b
	br	2f
1:
	sys	creat; tmpf; 600
	bes	2f
	mov	r0,tfo
	sys	open; tmpf; 0
	bec	1f
2:
	mov	$3f,r0
	jsr	pc,print
	sys	exit
3:
	<Tmp file?\n\0>; .even
1:
	mov	r0,tfi

	mov	gsp,sp
	cmp	(sp),$2  /is there a file argument
	blt	noarg
	mov	4(sp),r0
	mov	$argname,r1
1:
	movb	(r0)+,(r1)+
	bne	1b
aftered: / after edit
	mov	$argname,r0
	jsr	r5,fopen; iobuf
	bes	1f
noarg:
	jsr	pc,isymtab
	br	loop
1:
	mov	$1f,r0
	jsr	pc,print
	br	loop
1:
	<Cannot open file\n\0>; .even

intrup:
	sys	signal; 2; intrup
	mov	$'\n,r0
	jsr	r5,xputc
	jsr	r5,error
		<ready\n\0>; .even

loop:
	mov	gsp,sp
	clr	lineno
	jsr	pc,rdline
	mov	$line,r3
1:
	movb	(r3),r0
	jsr	pc,digit
		br 1f
	jsr	r5,atoi
	cmp	r0,$' /
	beq	3f
	cmp	r0,$'	 /tab
	bne	1f
3:
	mov	$lintab,r3
	mov	r1,r0
	bgt	2f
	jsr	pc,serror
2:
	cmp	r0,(r3)
	beq	2f
	tst	(r3)
	beq	2f
	add	$6,r3
	br	2b
2:
	cmp	r3,$elintab-12.
	blo	2f
	jsr	r5,error
		<too many lines\n\0>; .even
2:
	mov	r0,(r3)+
	mov	seeka,(r3)+
	mov	tfo,r0
	mov	seeka,seekx
	sys	indir; sysseek
	mov	$line,r0
	jsr	pc,size
	inc	r0
	add	r0,seeka
	mov	r0,wlen
	mov	tfo,r0
	mov	$line,wbuf
	sys	indir;syswrit
	br	loop
1:
	mov	$line,r3
	jsr	pc,singstat
	br	loop

nextc:
	movb	(r3)+,r0
	rts	r5

size:
	clr	-(sp)
1:
	inc	(sp)
	cmpb	(r0),$'\n
	beq	1f
	cmpb	(r0),$0
	beq	1f
	inc	r0
	br	1b
1:
	mov	(sp)+,r0
	rts	pc

rdline:  / read input (file or tty) to carr. ret.
	mov	$line,r1
1:
	jsr	r5,getc; iobuf
	bes	2f
	tst	r0
	beq	2f
	cmp	r1,$line+99.
	bhis	2f			/ bad check, but a check
	movb	r0,(r1)+
	cmpb	r0,$'\n
	bne	1b
	clrb	(r1)
	rts	pc
2:
	mov	fi,r0
	beq	1f
	sys	close
	clr	fi
	br	1b
1:
	jmp	_done

error:
	tst	fi
	beq	1f
	sys	close
	clr	fi
1:
	tst	lineno
	beq	1f
	jsr	pc,nextlin
		br 1f
	mov	$line,r0
	jsr	pc,print
1:
	mov	r5,r0
	jsr	pc,print
	jmp	loop

serror:
	dec	r3
	tst	fi
	beq	1f
	sys	close
	clr	fi
1:
	mov	$line,r1
1:
	cmp	r1,r3
	bne	2f
	mov	$'_,r0
	jsr	r5,xputc
	mov	$10,r0
	jsr	r5,xputc
2:
	movb	(r1),r0
	jsr	r5,xputc
	cmpb	(r1)+,$'\n
	bne	1b
	jmp	loop

print:
	mov	r0,wbuf
	jsr	pc,size
	mov	r0,wlen
	mov	prfile,r0
	sys	indir; syswrit
	rts	pc

digit:
	cmp	r0,$'0
	blo	1f
	cmp	r0,$'9
	bhi	1f
	add	$2,(sp)
1:
	rts	pc

alpha:
	cmp	r0,$'a
	blo	1f
	cmp	r0,$'z
	bhi	1f
	add	$2,(sp)
1:
	cmp	r0,$'A
	blo	1f
	cmp	r0,$'Z
	bhi	1f
	add	$2,(sp)
1:
	rts	pc

name:
	mov	$nameb,r1
	clr	(r1)
	clr	2(r1)
1:
	cmp	r1,$nameb+4
	bhis	2f
	movb	r0,(r1)+
2:
	movb	(r3)+,r0
	jsr	pc,alpha
		br 2f
	br	1b
2:
	jsr	pc,digit
		br 2f
	br	1b
2:
	mov	$resnam,r1
1:
	cmp	nameb,(r1)
	bne	2f
	cmp	nameb+2,2(r1)
	bne	2f
	sub	$resnam,r1
	asr	r1
	add	$2,(sp)
	rts	pc
2:
	add	$4,r1
	cmp	r1,$eresnam
	blo	1b
	mov	$symtab,r1
1:
	tst	(r1)
	beq	1f
	cmp	nameb,(r1)
	bne	2f
	cmp	nameb+2,2(r1)
	bne	2f
	rts	pc
2:
	add	$14.,r1
	br	1b
1:
	cmp	r1,$esymtab-28.
	blo	1f
	jsr	r5,error
		<out of symbol space\n\0>; .even
1:
	mov	nameb,(r1)
	mov	nameb+2,2(r1)
	clr	4(r1)
	clr	14.(r1)
	rts	pc

skip:
	cmp	r0,$' /
	beq	1f
	cmp	r0,$'	  / tab
	bne	2f
1:
	movb	(r3)+,r0
	br	skip
2:
	rts	pc

xputc:
.if scope  / for plotting
	tstb	drflg
	beq	1f
	jsr	pc,drput
	rts	r5
1:
.endif
	mov	r0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	r5

nextlin:
	clr	-(sp)
	mov	$lintab,r1
1:
	tst	(r1)
	beq	1f
	cmp	lineno,(r1)
	bhi	2f
	mov	(sp),r0
	beq	3f
	cmp	(r0),(r1)
	blos	2f
3:
	mov	r1,(sp)
2:
	add	$6,r1
	br	1b
1:
	mov	(sp)+,r1
	beq	1f
	mov	(r1)+,lineno
	mov	(r1)+,seekx
	mov	tfi,r0
	sys	indir; sysseek
	mov	tfi,r0
	sys	read; line; 100.
	add	$2,(sp)
1:
	rts	pc

getloc:
	mov	$lintab,r1
1:
	tst	(r1)
	beq	1f
	cmp	r0,(r1)
	beq	2f
	add	$6,r1
	br	1b
1:
	jsr	r5,error
		<label not found\n\0>; .even
2:
	rts	pc

isymtab:
	mov	$symtab,r0
	mov	$symtnam,r1
	clrf	fr0
	movf	$one,fr1
1:
	mov	(r1)+,(r0)+
	mov	(r1)+,(r0)+
	mov	$1,(r0)+
	subf	r1,r0
	movf	r0,(r0)+
	cmp	r1,$esymtnam
	blo	1b
	clr	(r0)+
	rts	pc

/
/

/ bas1 -- compile
/
/    convention:	jsr pc,subrout /test
/				br failside
/			succeed ...

compile:
	clr	forp
	mov	$iflev,ifp /added for if..else..fi
	mov	$space,r4
	tst	lineno
	beq	1f
	rts	pc
1:
	jsr	pc,nextlin
		br 1f
	mov	lineno,r0
	jsr	pc,getloc
	mov	r4,4(r1)
	jsr	pc,statement
		br .+2
	inc	lineno
	cmp	r4,$espace+20  / out of code space?
	blo	1b
	jsr	r5,error
		<out of code space\n\0>; .even
1:
	tst	forp
	jne	forer
	cmp	ifp,$iflev
	jne	fier   /hanging if..fi
	mov	$loop,(r4)+
	rts	pc

singstat:
	clr	forp
	mov	$iflev,ifp
	mov	$exline,r4
	jsr	pc,statement
		br 1f
	cmp	-2(r4),$_asgn
	beq	1f
	mov	$_print,(r4)+
	mov	$_nline,(r4)+
1:
	tst	forp
	jne	forer
	cmp	r4,$eexline
	blo	1f
	jsr	r5,error
		<out of code space\n\0>; .even
1:
	mov	$loop,(r4)+
	mov	r4,exprloc
	mov	$exline,r4
	jmp	execute

statement:
	mov	$line,r3
	movb	(r3)+,r0
	jsr	pc,digit
		br stat1
	dec	r3
	jsr	r5,atoi
	cmp	r0,$' /
	beq	1f
	cmp	r0,$'	 /tab
	beq	1f
	mov	$line,r3
	movb	(r3)+,r0
	br	stat1
1:
	mov	$_line,(r4)+
	mov	r1,(r4)+

stat1:
	jsr	pc,skip
	cmp	r0,$'\n
	bne	.+4
	rts	pc
	mov	r3,-(sp)
	jsr	pc,alpha
		br 1f
	jsr	pc,name
		br 1f
	tst	(sp)+
	jsr	pc,skip
	dec	r3
	jmp	*2f(r1)
2:
	stlist
	stdone
	stdone
	strun
	stprint
	stprompt   / prompt is like print except for cr
	stif
	stgoto
	streturn
	stfor
	stnext
	stoctl
	stsave
	stdump
	stfi
	stelse
	stedit
	stcomment
.if scope    / for plotting on tektronix
	stdisp
	stdraw
	steras
.endif

1:
	mov	(sp)+,r3
	dec	r3
	jsr	pc,expr
	cmp	r0,$'\n
	jne	joe
	add	$2,(sp)
	rts	pc

stsave:
	mov	$_save,func
	br	1f

stlist:
	mov	$_list,func
1:
	cmp	r0,$'\n
	bne	1f
	clrf	r0
	jsr	pc,const
	movif	$77777,r0
	jsr	pc,const
	br	2f
1:
	jsr	pc,expr
	cmp	r0,$'\n
	bne	1f
	mov	$_dup,(r4)+
	br	2f
1:
	dec	r3
	jsr	pc,expr
	cmp	r0,$'\n
	jne	joe
2:
	mov	func,(r4)+
	rts	pc

stdone:
	cmp	r0,$'\n
	jne	joe
	mov	$_done,(r4)+
	rts	pc

strun:
	cmp	r0,$'\n
	jne	joe
	mov	$_run,(r4)+
	rts	pc


stprompt:
	clr	-(sp)
	br	stpr2

stdump:
	cmp	r0,$'\n
	jne	joe
	mov	$_dump,(r4)+
	rts	pc

stprint:
	mov	pc,-(sp)
stpr2:
	movb	(r3)+,r0
	jsr	pc,skip
1:
	cmp	r0,$'\n
	beq	2f
	cmp	r0,$'"
	beq	1f
	dec	r3
	jsr	pc,expr
	mov	$_print,(r4)+
	br	1b
1:
	mov	$_ascii,(r4)+
1:
	movb	(r3)+,(r4)
	cmpb	(r4),$'"
	beq	1f
	cmpb	(r4)+,$'\n
	bne	1b
	jbr	joe
1:
	add	$2,r4
	bic	$1,r4
	br	stpr2
2:
	tst	(sp)+
	beq	1f
	mov	$_nline,(r4)+
1:
	rts	pc

stif:
	jsr	pc,expr
	mov	$_if,(r4)+
	mov	r4,*ifp
	add	$2,ifp
	tst	(r4)+
	jsr	pc,skip
	cmp	r0,$'\n   / if ... fi
	beq	1f
	jsr	pc,stat1
		br  .+2
stfi:
	sub	$2,ifp
	cmp	ifp,$iflev
	jlo	fier
	mov	*ifp,r1  /for jump around if
	mov	r4,(r1)
1:
	rts	pc

fier:
	jsr	r5,error; <if...else...fi imbalance\n\0>; .even

stelse:
	mov	$_tra,(r4)+  /jump around else side
	mov	r4+,-(sp) / save hole
	tst	(r4)+
	sub	$2,ifp
	cmp	ifp,$iflev
	jlo	fier
	mov	*ifp,r1
	mov	r4,(r1)  /fill in jump to else
	mov	(sp)+,*ifp /save hole for fi
	add	$2,ifp
	rts	pc

stedit:  / enter the regular editor <ed>
	sys fork
	br	newpr
	mov	$lintab,r0  / zero out line table during edit
1:
	cmp	r0,$elintab  /done
	beq	1f
	mov	$0,(r0)+
	br	1b
1:
	sys	unlink; tmpf
	sys	wait
	jmp	aftered / start over
newpr:
	sys	exec; ed; edarg
	sys	exit
ed:	</bin/ed\0> ; .even
ednm:	<-\n>
 .even
edarg:	ednm; argname; 0

stcomment:  /comment line
	cmp	r0,$'\n
	beq	1f
	movb	(r3)+,r0
	br	stcomment
1:
	rts	pc
stgoto:
	jsr	pc,expr
	mov	$_goto,(r4)+
	rts	pc

streturn:
	cmp	r0,$'\n
	beq	1f
	jsr	pc,expr
	cmp	r0,$'\n
	bne	joe
	br	2f
1:
	clrf	r0
	jsr	pc,const
2:
	mov	$_return,(r4)+
	rts	pc

joe:
	jsr	pc,serror

stfor:
	mov	r4,-(sp)
	jsr	pc,e2
	mov	r4,-(sp)
	cmp	r0,$'=
	bne	joe
	tst	val
	bne	joe
	jsr	pc,expr
	mov	forp,(r4)+	/ overlay w _asgn
	mov	r4,forp
	cmp	(r4)+,(r4)+	/ _tra ..
	mov	(sp)+,r0
	mov	(sp)+,r1
1:
	mov	(r1)+,(r4)+
	cmp	r1,r0
	blo	1b
	mov	$_fori,(r4)+
	mov	forp,r1
	mov	$_tra,(r1)+
	mov	r4,(r1)+
	dec	r3
	jsr	pc,expr
	mov	$_lesseq,(r4)+
	mov	$_if,(r4)+
	mov	forp,(r4)+
	mov	r4,forp
	cmp	r0,$'\n
	beq	1f
	jsr	pc,stat1
		br .+2
	br	stnext
1:
	rts	pc

forer:
	jsr	r5,error; <for/next imbalance\n\0>; .even

stnext:
	mov	forp,r1
	beq	forer
	mov	-(r1),r0
	mov	-(r0),forp
	mov	$_ptra,(r4)+
	mov	$_asgn,(r0)+
	cmp	(r0)+,(r0)+
	mov	r0,(r4)+
	mov	r4,(r1)+
	rts	pc

stoctl:
	jsr	pc,expr
	mov	$_octal,(r4)+
	rts	pc

.if scope  / for plotting
stdisp:
	mov	$_sdisp,(r4)+
	jsr	pc,stprint
	mov	$_fdisp,(r4)+
	rts	pc
stdraw:
	jsr	pc,expr
	dec	r3
	jsr	pc,expr
	cmp	r0,$'\n
	bne	1f
	movf	$one,r0
	jsr	pc,const
	br	2f
1:
	dec	r3
	jsr	pc,expr
2:
	mov	$_draw,(r4)+
	rts	pc

steras:
	mov	$_erase,(r4)+
	rts	pc
.endif

/
/

/ bas2 -- expression evaluation

expr:
	jsr	pc,e1
	jsr	pc,rval
	rts	pc

/ assignment right to left
e1:
	jsr	pc,e2
	cmp	r0,$'=
	beq	1f
	jsr	pc,rval
	rts	pc
1:
	tst	val
	beq	1f
	jsr	pc,serror
1:
	jsr	pc,e1
	jsr	r5,op; _asgn
	rts	pc

/ and or left to right
e2:
	jsr	pc,e3
1:
	cmp	r0,$'&
	beq	2f
	cmp	r0,$'|
	beq	3f
	rts	pc
2:
	jsr	pc,rval
	jsr	pc,e3
	jsr	r5,op; _and
	br	1b
3:
	jsr	pc,rval
	jsr	pc,e3
	jsr	r5,op; _or
	br	1b

/ relation extended relation
e3:
	jsr	pc,e4
	jsr	pc,e3a
		rts pc
	clr	-(sp)
1:
	mov	r0,-(sp)
	jsr	pc,e4
	jsr	pc,rval
	mov	(sp)+,(r4)+
	jsr	pc,e3a
		br 1f
	mov	$_extr,(r4)+
	inc	(sp)
	br	1b
1:
	dec	(sp)
	blt	1f
	mov	$_and,(r4)+
	br	1b
1:
	tst	(sp)+
	rts	pc

/ relational operator
e3a:
	cmp	r0,$'>
	beq	1f
	cmp	r0,$'<
	beq	2f
	cmp	r0,$'=
	beq	3f
	rts	pc
1:
	mov	$_great,r0
	cmpb	(r3),$'=
	bne	1f
	inc	r3
	mov	$_greateq,r0
	br	1f
2:
	cmpb	(r3),$'>
	bne	2f
	inc	r3
	mov	$_noteq,r0
	br	1f
2:
	mov	$_less,r0
	cmpb	(r3),$'=
	bne	1f
	inc	r3
	mov	$_lesseq,r0
	br	1f
3:
	cmpb	(r3),$'=
	beq	2f
	rts	pc
2:
	inc	r3
	mov	$_equal,r0
1:
	jsr	pc,rval
	add	$2,(sp)
	rts	pc

/ add subtract
e4:
	jsr	pc,e5
1:
	cmp	r0,$'+
	beq	2f
	cmp	r0,$'-
	beq	3f
	rts	pc
2:
	jsr	pc,rval
	jsr	pc,e5
	jsr	r5,op; _add
	br	1b
3:
	jsr	pc,rval
	jsr	pc,e5
	jsr	r5,op; _sub
	br	1b

/ multiply divide
e5:
	jsr	pc,e6
1:
	cmp	r0,$'*
	beq	2f
	cmp	r0,$'/
	beq	3f
	rts	pc
2:
	jsr	pc,rval
	jsr	pc,e6
	jsr	r5,op; _mult
	br	1b
3:
	jsr	pc,rval
	jsr	pc,e6
	jsr	r5,op; _divid
	br	1b

/ exponential
e6:
	jsr	pc,e6a
1:
	cmp	r0,$'^
	beq	2f
	rts	pc
2:
	jsr	pc,rval
	jsr	pc,e6a
	jsr	r5,op; _expon
	br	1b

e6a:
	movb	(r3)+,r0
	jsr	pc,skip
	cmp	r0,$'_
	bne	1f
	jsr	pc,e6a
	jsr	r5,op; _neg
	rts	pc
1:
	dec	r3
	jsr	pc,e7
	rts	pc
/ end of unary -

/ primary
e7:
	movb	(r3)+,r0
	jsr	pc,skip
	mov	$1,val
	cmp	r0,$'(
	bne	1f
	jsr	pc,e1
	cmp	r0,$')
	bne	2f
	movb	(r3)+,r0
	br	e7a
2:
	jsr	pc,serror
1:
	cmp	r0,$'.
	beq	2f
	jsr	pc,digit
		br 1f
2:
	dec	r3
	jsr	r5,atof; nextc
	jsr	pc,const
	br	e7a
1:
	jsr	pc,alpha
		br jim
	jsr	pc,name
		br 2f
	jsr	r5,error; <reserved name\n\0>; .even
2:
/ try to fix illegal symbol bug:
	cmp	r4,$eexline
	bhis	jim

	mov	$_lval,(r4)+
	mov	r1,(r4)+
	clr	val
	br	e7a
jim:
	jsr	pc,serror

e7a:
	jsr	pc,skip
	cmp	r0,$'(
	bne	1f
	jsr	pc,rval
	jsr	r5,rlist; _funct
	cmp	r0,$')
	bne	jim
	movb	(r3)+,r0
	br	e7a
1:
	cmp	r0,$'[
	bne	1f
	tst	val
	beq	2f
	jsr	pc,serror
2:
	jsr	r5,rlist; _subscr
	clr	val
	cmp	r0,$']
	bne	jim
	movb	(r3)+,r0
	br	e7a
1:
	rts	pc

op:
	jsr	pc,rval
	mov	(r5)+,(r4)+
	rts	r5

rval:
	tst	val
	bne	1f
	mov	$_rval,(r4)+
	inc	val
1:
	rts	pc

const:
	mov	r0,-(sp)
	movf	r1,-(sp)
	tstf	r0
	cfcc
	bne	1f
	mov	$_con0,(r4)+
	br	2f
1:
	cmpf	$one,r0
	cfcc
	bne	1f
	mov	$_con1,(r4)+
	br	2f
1:
	movfi	r0,r0
	movif	r0,r1
	cmpf	r0,r1
	cfcc
	bne	1f
	mov	$_intcon,(r4)+
	mov	r0,(r4)+
	br	2f
1:
	mov	$_const,(r4)+
	movf	r0,(r4)+
2:
	movf	(sp)+,r1
	mov	(sp)+,r0
	rts	pc

rlist:
	clr	-(sp)
	cmpb	(r3),$')
	bne	1f
	movb	(r3)+,r0
	br	2f
1:
	inc	(sp)
	jsr	pc,expr
	cmp	r0,$',
	beq	1b
2:
	mov	(r5)+,(r4)+
	mov	(sp)+,(r4)+
	rts	r5

/
/
/ bas3 -- execution

execute:
	mov	$estack,r3
	mov	r3,sstack
	jmp	*(r4)+

_if:
	tstf	(r3)+
	cfcc
	beq	_tra
	tst	(r4)+
	jmp	*(r4)+

_ptra:
	mov	sstack,r3

_tra:
	mov	(r4)+,r4
	jmp	*(r4)+

_funct:
	mov	r4,-(r3)
	mov	sstack,-(r3)
	mov	r3,sstack
	inc	sublev
	clr	r0
	jsr	pc,arg
	tstf	r0
	cfcc
	bge	1f
	jmp	builtin

_goto:
	movf	(r3),r0
1:
	movfi	r0,-(sp)
	jsr	pc,compile
	mov	(sp)+,r0
	jsr	pc,getloc
	mov	4(r1),r4
	jmp	*(r4)+

_run:
	jsr	pc,isymtab
	mov	randx,r0
	jsr	pc,srand
	jsr	pc,compile
	mov	$space,r4
	jmp	*(r4)+

_save:    / _save is a _list to the file named on the bas command
	sys	creat; argname; 666
	bes	1f
	mov	r0,prfile
	br	2f
1:
	mov	1f,r0
	mov	$1,prfile
	jsr	pc,print
	br	_done
1:	<Cannot create b.out\n\0>; .even

_list:
	mov	$1,prfile
2:
	movf	(r3)+,r0
	movfi	r0,-(sp)
/ probably vistigal?? 	mov	r3,0f
	movf	(r3),r0
	movfi	r0,lineno
1:
	jsr	pc,nextlin
		br 1f
	cmp	lineno,(sp)
	bhi	1f
	mov	$line,r0
	jsr	pc,print
	inc	lineno
	br	1b
1:
	cmp	$1,prfile
	beq	1f
	mov	prfile,r0
	sys	close
	mov	$1,prfile
1:
	tst	(sp)+
	jmp	*(r4)+

_done:
	sys	unlink; tmpf
	sys	exit

.if scope  / for plotting
_sdisp:
	mov	$2,r0
	jsr	pc,drput
	jsr	pc,drxy
	mov	$1,r0
	jsr	pc,drput
	mov	$3,r0
	jsr	pc,drput
	incb	drflg
	jmp	*(r4)+

_fdisp:
	clr	r0
	jsr	pc,drput
	clrb	drflg
	jmp	*(r4)+

_draw:
	movf	(r3)+,r2
	movf	(r3)+,r1
	movf	(r3)+,r0
	jsr	r5,draw
	jmp	*(r4)+

_erase:
	mov	$1,r0
	jsr	pc,drput
	mov	$1,r0
	jsr	pc,drput
	jmp	*(r4)+
.endif

_print:
	movf	(r3)+,r0
	jsr	r5,ftoa; xputc
	jmp	*(r4)+

_octal:
	movf	(r3)+,r0
	jsr	r5,ftoo; xputc
	jmp	*(r4)+

_nline:
	mov	$'\n,r0
	jsr	r5,xputc
	jmp	*(r4)+

_ascii:
	movb	(r4)+,r0
	cmp	r0,$'"
	beq	1f
	jsr	r5,xputc
	br	_ascii
1:
	inc	r4
	bic	$1,r4
	jmp	*(r4)+

_line:
	mov	sstack,r3
	cmp	r3,$stack+20.
	bhi	1f
	jsr	r5,error
		<out of space\n\0>; .even
1:
	mov	(r4)+,lineno
	jmp	*(r4)+

_or:
	tstf	(r3)+
	cfcc
	bne	stone
	tstf	(r3)
	cfcc
	bne	stone
	br	stzero

_and:
	tstf	(r3)+
	cfcc
	beq	stzero
	tstf	(r3)
	cfcc
	beq	stzero
	br	stone

_great:
	jsr	pc,bool
	bgt	stone
	br	stzero

_greateq:
	jsr	pc,bool
	bge	stone
	br	stzero

_less:
	jsr	pc,bool
	blt	stone
	br	stzero

_lesseq:
	jsr	pc,bool
	ble	stone
	br	stzero

_noteq:
	jsr	pc,bool
	bne	stone
	br	stzero

_equal:
	jsr	pc,bool
	beq	stone

stzero:
	clrf	r0
	br	advanc

stone:
	movf	$one,r0
	br	advanc

_extr:
	movf	r1,r0		/ dup for _and in extended rel
	br	subadv

_asgn:
	movf	(r3)+,r0
	mov	(r3)+,r0
	add	$4,r0
	bis	$1,(r0)+
	movf	r0,(r0)
	br	subadv

_add:
	movf	(r3)+,r0
	addf	(r3),r0
	br	advanc

_sub:
	movf	(r3)+,r0
	negf	r0
	addf	(r3),r0
	br	advanc

_mult:
	movf	(r3)+,r0
	mulf	(r3),r0
	br	advanc

_divid:
	movf	(r3)+,r1
	movf	(r3),r0
	divf	r1,r0
	br	advanc

_expon:
	movf	(r3)+,fr1
	movf	(r3),fr0
	jsr	pc,pow
	bec	advanc
	jsr	r5,error
		<Bad exponentiation\n\0>; .even

_neg:  / unary -
	negf	r0
	jbr	advanc
/ end of _neg

_intcon:
	movif	(r4)+,r0
	jbr	subadv

_con0:
	clrf	r0
	jbr	subadv

_con1:
	movf	$one,r0
	jbr	subadv

_const:
	movf	(r4)+,r0

subadv:
	movf	r0,-(r3)
	jmp	*(r4)+

advanc:
	movf	r0,(r3)
	jmp	*(r4)+

_rval:
	jsr	pc,getlv
	br	subadv

_fori:
	jsr	pc,getlv
	addf	$one,r0
	movf	r0,(r0)
	br	subadv

_lval:
	mov	(r4)+,-(r3)
	jmp	*(r4)+

_dup:
	movf	(r3),r0
	br	subadv

_return:
	dec	sublev
	bge	1f
	jsr	r5,error
		<bad return\n\0>; .even
1:
	movf	(r3),r0
	mov	sstack,r3
	mov	(r3)+,sstack
	mov	(r3)+,r4
	mov	(r4)+,r0
1:
	dec	r0
	blt	advanc
	add	$8,r3
	br	1b

_subscr:
	mov	(r4),r1
	mpy	$8.,r1
	add	r1,r3
	mov	r3,-(sp)
	mov	(r3),r0
	mov	(r4)+,-(sp)
1:
	dec	(sp)
	blt	1f
	movf	-(r3),r0
	movfi	r0,r2
	com	r2
	blt	2f
	jsr	r5,error
		<subscript out of range\n\0>; .even
2:
	mov	r0,r1
	mov	4(r0),r0
	bic	$1,r0
2:
	beq	2f
	cmp	r2,(r0)+
	bne	3f
	tst	-(r0)
	br	1b
3:
	mov	(r0),r0
	br	2b
2:
	mov	$symtab,r0
2:
	tst	(r0)
	beq	2f
	add	$14.,r0
	br	2b
2:
	cmp	r0,$esymtab-28.
	blo	2f
	jsr	r5,error
		<out of symbol space\n\0>; .even
2:
	cmp	(r1)+,(r1)+
	mov	r0,-(sp)
	clr	14.(r0)
	mov	r2,(r0)+
	mov	(r1),r2
	bic	$1,r2
	mov	r2,(r0)+
	clr	(r0)+
	mov	(sp)+,r0
	bic	$!1,(r1)
	bis	r0,(r1)
	br	1b
1:
	tst	(sp)+
	mov	(sp)+,r3
	mov	r0,(r3)
	jmp	*(r4)+

bool:
	movf	(r3)+,r1	/ r1 used in extended rel
	cmpf	(r3),r1
	cfcc
	rts	pc

getlv:
	mov	(r3)+,r0
	add	$4,r0
	bit	$1,(r0)+
	bne	1f
	jsr	r5,error;<used before set\n\0>; .even
1:
	movf	(r0),r0
	rts	pc

/
/

/ bas4 -- builtin functions

builtin:
	dec	sublev
	mov	(r3)+,sstack
	mov	(r3)+,r4
	movfi	r0,r0
	com	r0
	asl	r0
	cmp	r0,$2f-1f
	bhis	2f
	jmp	*1f(r0)
1:
	fnarg
	fnexp
	fnlog
	fnsin
	fncos
	fnatan
	fnrand
	fnexpr
	fnint
	fnabs
	fnsqr
2:
	mov	$-1,r0
	jsr	pc,getloc		/ label not found diagnostic

fnarg:
	cmp	(r4)+,$1
	bne	narg
	movf	(r3),r0
	movfi	r0,r0
	jsr	pc,arg
	br	fnadvanc

fnexp:
	jsr	r5,fnfn; exp
	br	fnadvanc

fnlog:
	jsr	r5,fnfn; log
	bec	fnadvanc
	jsr	r5,error
		<Bad log\n\0>; .even

fnsin:
	jsr	r5,fnfn; sin
	bec	fnadvanc
	jsr	r5,error
		<Bad sine\n\0>; .even

fncos:
	jsr	r5,fnfn; cos
	bec	fnadvanc
	jsr	r5,error
		<Bad cosine\n\0>; .even

fnatan:
	jsr	r5,fnfn; atan
	bec	fnadvanc
	jsr	r5,error
		<Bad arctangent\n\0>; .even

fnrand:
	tst	(r4)+
	bne	narg
	jsr	pc,rand
	movif	r0,r0
	divf	$44000,r0
	jmp	advanc

fnexpr:
	tst	(r4)+
	bne	narg
	mov	r3,-(sp)
	mov	r4,-(sp)
	jsr	pc,rdline
	mov	exprloc,r4
	mov	$line,r3
	jsr	pc,expr
	mov	$_tra,(r4)+
	mov	(sp)+,(r4)+
	mov	(sp)+,r3
	mov	exprloc,r4
	add	$8,r3
	jmp	*(r4)+

fnint:
	cmp	(r4)+,$1
	bne	narg
	movf	(r3),r0
	modf	$one,r0
	movf	r1,r0
	br	fnadvanc

fnabs:
	cmp	(r4)+,$1
	bne	narg
	movf	(r3),r0
	cfcc
	bge	fnadvanc
	negf	r0
	br	fnadvanc

fnsqr:
	jsr	r5,fnfn; sqrt
	bec	fnadvanc
	jsr	r5,error
	<Bad square root arg\n\0>; .even
fnadvanc:
	add	$8,r3
	jmp	advanc

narg:
	jsr	r5,error
		<arg count\n\0>; .even

arg:
	tst	sublev
	beq	1f
	mov	sstack,r1
	sub	*2(r1),r0
	bhi	1f
2:
	inc	r0
	bgt	2f
	add	$8,r1
	br	2b
2:
	movf	4(r1),r0
	rts	pc
1:
	jsr	r5,error
		<bad arg\n\0>; .even

fnfn:
	cmp	(r4)+,$1
	bne	narg
	movf	(r3),r0
	jsr	pc,*(r5)+
	rts	r5

.if scope / for plotting
draw:
	tstf	r2
	cfcc
	bne	1f
	movf	r0,drx
	movf	r1,dry
	rts	r5
1:
	movf	r0,-(sp)
	movf	r1,-(sp)
	mov	$3,r0
	jsr	pc,drput
	jsr	pc,drxy
	movf	(sp)+,r0
	movf	r0,dry
	movf	(sp)+,r0
	movf	r0,drx
	jsr	pc,drxy
	rts	r5

drxy:
	movf	drx,r0
	jsr	pc,drco
	movf	dry,r0

drco:
	tstf	r0
	cfcc
	bge	1f
	clrf	r0
1:
	cmpf	$40200,r0		/ 1.0
	cfcc
	bgt	1f
	movf	$40177,r0		/ 1.0-eps
1:
	subf	$40000,r0		/ .5
	mulf	$43200,r0		/ 4096
	movfi	r0,r0
	mov	r0,-(sp)
	jsr	pc,drput
	mov	(sp)+,r0
	swab	r0

drput:
	movb	r0,ch
	mov	drfo,r0
	bne	1f
	sys	open; vt; 1
	bec	2f
	4
2:
	mov	r0,drfo
1:
	sys	write; ch; 1
	rts	pc

.endif
/ bas4 -- old library routines
atoi:
	clr	r1
	jsr	r5,nextc
	clr	-(sp)
	cmp	r0,$'-
	bne	2f
	inc	(sp)
1:
	jsr	r5,nextc
2:
	sub	$'0,r0
	cmp	r0,$9
	bhi	1f
	mpy	$10.,r1
	bcs	3f / >32k
	add	r0,r1
	bcs	3f / >32k
	br	1b
1:
	add	$'0,r0
	tst	(sp)+
	beq	1f
	neg	r1
1:
	rts	r5
3:
	tst	(sp)+
	mov	$'.,r0  / faking overflow
	br	1b

ldfps = 170100^tst
stfps = 170200^tst
atof:
	stfps	-(sp)
	ldfps	$200
	movf	fr1,-(sp)
	mov	r1,-(sp)
	mov	r2,-(sp)
	clr	-(sp)
	clrf	fr0
	clr	r2
	jsr	r5,*(r5)
	cmpb	r0,$'-
	bne	2f
	inc	(sp)
1:
	jsr	r5,*(r5)
2:
	sub	$'0,r0
	cmp	r0,$9.
	bhi	2f
	jsr	pc,dig
		br	1b
	inc	r2
	br	1b
2:
	cmpb	r0,$'.-'0
	bne	2f
1:
	jsr	r5,*(r5)
	sub	$'0,r0
	cmp	r0,$9.
	bhi	2f
	jsr	pc,dig
		dec r2
	br	1b
2:
	cmpb	r0,$'e-'0
	bne	1f
	jsr	r5,atoi
	sub	$'0,r0
	add	r1,r2
1:
	movf	$one,fr1
	mov	r2,-(sp)
	beq	2f
	bgt	1f
	neg	r2
1:
	cmp	r2,$38.
	blos	1f
	clrf	fr0
	tst	(sp)+
	bmi	out
	movf	$huge,fr0
	br	out
1:
	mulf	$ten,fr1
	sob	r2,1b
2:
	tst	(sp)+
	bge	1f
	divf	fr1,fr0
	br	2f
1:
	mulf	fr1,fr0
	cfcc
	bvc	2f
	movf	$huge,fr0
2:
out:
	tst	(sp)+
	beq	1f
	negf	fr0
1:
	add	$'0,r0
	mov	(sp)+,r2
	mov	(sp)+,r1
	movf	(sp)+,fr1
	ldfps	(sp)+
	tst	(r5)+
	rts	r5

dig:
	cmpf	$big,fr0
	cfcc
	blt	1f
	mulf	$ten,fr0
	movif	r0,fr1
	addf	fr1,fr0
	rts	pc
1:
	add	$2,(sp)
	rts	pc

one	= 40200
ten	= 41040
big	= 56200
huge	= 77777

.globl	_ndigits
.globl ecvt
.globl fcvt

ftoa:
	jsr	pc,ecvt
	mov	r0,bufptr
	tstb	r1
	beq	1f
	mov	$'-,r0
	jsr	r5,*(r5)
1:
	cmp	r3,$-2
	blt	econ
	cmp	r2,$-5
	ble	econ
	cmp	r2,$6
	bgt	econ
	jsr	pc,cout
	tst	(r5)+
	rts	r5

econ:
	mov	r2,-(sp)
	mov	$1,r2
	jsr	pc,cout
	mov	$'e,r0
	jsr	r5,*(r5)
	mov	(sp)+,r0
	dec	r0
	jmp	itoa

cout:
	mov	bufptr,r1
	add	_ndigits,r1
	mov	r2,-(sp)
	add	bufptr,r2
1:
	cmp	r1,r2
	blos	1f
	cmpb	-(r1),$'0
	beq	1b
	inc	r1
1:
	mov	(sp)+,r2
	bge	2f
	mov	$'.,r0
	jsr	r5,*(r5)
1:
	mov	$'0,r0
	jsr	r5,*(r5)
	inc	r2
	blt	1b
	dec	r2
2:
	mov	r2,-(sp)
	mov	bufptr,r2
1:
	cmp	r2,r1
	bhis	1f
	tst	(sp)
	bne	2f
	mov	$'.,r0
	jsr	r5,*(r5)
2:
	dec	(sp)
	movb	(r2)+,r0
	jsr	r5,*(r5)
	br	1b
1:
	tst	(sp)+
	rts	pc

.bss
bufptr:	.=.+2
.text

ftoo:
	stfps	-(sp)
	ldfps	$200
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	$buf,r1
	movf	fr0,(r1)+
	mov	$buf,r2
	br	2f
1:
	cmp	r2,r1
	bhis	1f
	mov	$';,r0
	jsr	r5,*(r5)
2:
	mov	(r2)+,r0
	jsr	pc,oct
	br	1b
1:
	mov	$'\n,r0
	jsr	pc,*(r5)+
	ldfps	(sp)+
	rts	r5

oct:
	mov	r0,x+2
	setl
	movif	x,fr0
	mulf	$small,fr0
	seti
	mov	$6.,-(sp)
1:
	modf	$eight,fr0
	movfi	fr1,r0
	add	$'0,r0
	jsr	r5,*(r5)
	dec	(sp)
	bne	1b
	tst	(sp)+
	rts	pc

eight	= 41000
small	= 33600
.bss
buf:	.=.+8
x:	.=.+4
.text

itoa:
	mov	r1,-(sp)
	mov	r0,r1
	bge	1f
	neg	r1
	mov	$'-,r0
	jsr	r5,*(r5)
1:
	jsr	pc,1f
	mov	(sp)+,r1
	tst	(r5)+
	rts	r5

1:
	clr	r0
	dvd	$10.,r0
	mov	r1,-(sp)
	mov	r0,r1
	beq	1f
	jsr	pc,1b
1:
	mov	(sp)+,r0
	add	$'0,r0
	jsr	r5,*(r5)
	rts	pc
/ bas -- BASIC
/ new command "dump" which dumps symbol table values by name
/		R. Haight
/
_dump:
	mov	r4,-(sp)
	mov	$11.*14.+symtab-14.,r4
1:
	add	$14.,r4
	tst	(r4)
	beq	1f
	bit	$1,4(r4)
	beq	1b
	jsr	pc,dmp1
	mov	$'=,r0
	jsr	r5,xputc
	movf	6(r4),r0
	jsr	r5,ftoa; xputc
	mov	$'\n,r0
	jsr	r5,xputc
	br	1b
1:
	mov	(sp)+,r4
	jmp	*(r4)+

dmp1:
	tst	(r4)
	blt	1f
	mov	(r4),nameb
	mov	2(r4),nameb+2
	mov	$nameb,r0
	jsr	pc,print
	rts	pc
1:
	mov	r4,-(sp)
	mov	$symtab-14.,r4
1:
	add	$14.,r4
	tst	(r4)
	beq	1f
	mov	4(r4),r0
	bic	$1,r0
2:
	beq	1b
	cmp	r0,(sp)
	beq	2f
	mov	2(r0),r0
	br	2b
2:
	jsr	pc,dmp1
	mov	$'[,r0
	jsr	r5,xputc
	mov	*(sp),r0
	com	r0
	movif	r0,r0
	jsr	r5,ftoa; xputc
	mov	$'],r0
	jsr	r5,xputc
1:
	mov	(sp)+,r4
	rts	pc
/
/

/ basx -- data

one = 40200

.data

_ndigits:10.
tmpf:	</tmp/btma\0>
argname: <b.out\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0>
vt:	</dev/vt0\0>
.even
pname:	<\0\0\0\0\0\0>
	.even

resnam:
	<list>
	<done>
	<q\0\0\0>
	<run\0>
	<prin>
	<prom>   / prompt is like print without \n (cr)
	<if\0\0>
	<goto>
	<retu>
	<for\0>
	<next>
	<octa>
	<save>
	<dump>
	<fi\0\0>
	<else>
	<edit>
	<comm>  / comment
.if scope / for plotting
	<disp>
	<draw>
	<eras>
.endif
eresnam:

symtnam:
	<arg\0>
	<exp\0>
	<log\0>
	<sin\0>
	<cos\0>
	<atn\0>
	<rnd\0>
	<expr>
	<int\0>
	<abs\0>
	<sqr\0>
esymtnam:

/ indirect sys calls:
sysseek:	sys	seek; seekx: 0; 0
syswrit:	sys	write; wbuf: 0; wlen: 0
sysread:	sys	read; rbuf: 0; rlen: 0
sysopen:	sys	open; ofile: 0 ; omode: 0
syscreat:	sys	creat; cfile: 0; cmode: 0
.bss
drx:	.=.+8
dry:	.=.+8
drfo:	.=.+2
ch:	.=.+2
drflg:	.=.+2
randx:	.=.+2
gsp:	.=.+2
forp:	.=.+2
exprloc:.=.+2
sstack:	.=.+2
sublev:	.=.+2
val:	.=.+2
splimit:	.=.+2  / statement size limit
iflev:	.=.+20.  / nested if compile stack: 10 deep
ifp:	.=.+2    / current pointer to iflev
line:	.=.+100.
prfile:	.=.+2   / output from _list or _save
tfi:	.=.+2  / input file
func:	.=.+2   / alternate functions, eg: _list or _save
seeka:	.=.+2   / seek offset 1
lineno:	.=.+2
nameb:	.=.+4
tfo:	.=.+2
symtab:	.=.+2800.; esymtab: / symbol=7wds; symtab for 200
space:	.=.+8000.; espace: / code space
exline:	.=.+1000.; eexline: / line execute space
lintab:	.=.+1800.; elintab: / 3wds per statement = 300 stmts
stack:	.=.+800.; estack:

iobuf: fi: .=.+518.  / should be aquired??
