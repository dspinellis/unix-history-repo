/ tmg
/ main program and parsing rule interpreter
/
tracing = 1
f = r5
g = r4
i = r3

sef=sec^sec; clf=clc^clc; bfs=bcs^bcs; bfc=bcc^bcc	/fail indicator

.globl flush,obuild,putch,iget,kput
.globl generate
.globl cfile,dfile,ofile,input
.globl main,succ,fail,errcom,pbundle,parse,diag
.globl alt,salt,stop,goto
.globl tables,start,end
.globl stkb,stke
.globl ktab
.globl trswitch,trace
.globl x,si,j,k,n,g1,env

/ begin here
/ get arguments from shell
/ arg1 is input file
/ arg2 is output file (standard output if missing)

main:
	dec	(sp)
	beq	3f
	mov	4(sp),0f
	sys	open;0:0;0
	bes	1f
	mov	r0,input
	dec	(sp)
	beq	3f
	mov	6(sp),0f
	sys	creat;0:0;666
	bes	1f
	mov	r0,ofile

/ set up tables
/ initialize stack, for definitions see tmgc.s
/ go interpret beginning at "start"
/ finish up
3:
	mov	$stkb,f
	clr	j(f)
	clr	k(f)
	clr	n(f)
	mov	f,g
	add	$g1,g
	mov	$start,r0
	jsr	pc,adv
	jsr	pc,flush
1:
	sys	unlink;1f
	sys	exit
1:
		<alloc.d\0>;.even
/ fatal processor error
/write a two letter message on diagnostic file
/ get a dump

errcom:
	mov	dfile,cfile
	jsr	pc,obuild
	mov	$1f,r0
	jsr	pc,obuild
	jsr	pc,flush
stop:
	4
1:	<--fatal\n\0>;.even

/ all functions that succeed come here
/ test the exit indicator, and leave the rule if on

succ:
	inc	succc
	bit	$1,x(f)
	bne	sret
contin:
	inc	continc
    .if tracing
	tst	trswitch
	beq	1f
	mov	$'r,r0
	jsr	pc,trace
1:
    .endif
/ get interpreted instruction
/ save its exit bit (bit 0) on stack
/ distinguish type of instruction by ranges of value

	jsr	pc,iget
	mov	r0,x(f)
	bic	$1,r0
.if ..
	cmp	r0,$..
	blo	1f
.endif
	cmp	r0,$start
	blo	2f
	cmp	r0,$end
	blo	3f
	cmp	r0,$tables
	blo	2f

/ bad address
1:
	jsr	r0,errcom
		<bad address in parsing\0>;.even

/ machine coded function
2:
	jmp	(r0)

/ tmg-coded rule, execute and test its success
/ bfc = branch on fail clear
3:
	jsr	pc,adv
	bfc	succ

/ all functions and rules that fail come here
/ if exit bit is on do a fail return
/ if following instruction is an alternate (recognized literally)
/ do a goto, if a success alternate, do a nop
/ otherwise do a fail return

fail:
	inc	failc
	bit	$1,x(f)
	bne	fret
	jsr	pc,iget
	mov	r0,x(f)
	bic	$1,r0
	cmp	r0,$alt
	beq	salt
	cmp	r0,$salt
	bne	fret

alt:
	tst	(i)+
	br	succ

salt:
	jsr	pc,iget
	mov	r0,i
	br	contin

goto:
	br	salt

/ do a success return
/ bundle translations delivered to this rule,
/ pop stack frame
/ restore  interpreted instruction counter (i)
/ update input cursor (j) for invoking rule
/ update high water mark (k) in ktable
/ if there was a translation delivered, add to stack frame
/ clear the fail flag

sret:
	mov	f,r0
	add	$g1,r0
	jsr	pc,pbundle
	mov	f,g
	mov	(f),f
	mov	si(f),i
	mov	j(g),j(f)
	mov	k(g),k(f)
	tst	r0
	beq	1f
	mov	r0,(g)+
1:
	clf
	rts	pc

/ do a fail return
/ pop stack
/ do not update j or k
/ restore interpreted instruction counter

fret:
	mov	f,g
	mov	(f),f
	mov	si(f),i
	sef
	rts	pc

/ diag and parse builtins
/ set current file to diagnostic or output
/ save and restore ktable water mark around parse-translate
/ also current file and next frame pointer (g)
/ execute parsing rule

diag:
	mov	dfile,r1
	br	1f
parse:
	mov	ofile,r1
1:
	mov	cfile,-(sp)
	mov	r1,cfile
	mov	k(f),-(sp)
	mov	g,-(sp)
	jsr	pc,iget
	jsr	pc,adv
	bfs	1f
/ rule succeeded
/ if it delivered translation, put it in ktable and set
/ instruction counter for
/ translation generator to point there
/ go generate
	cmp	g,(sp)+
	ble	2f
	mov	-(g),r0
	jsr	pc,kput
	mov	k(f),i
	neg	i
	add	$ktab,i
	mov	f,-(sp)
	mov	g,f
	clr	x(f)
	jsr	pc,generate
	mov	(sp)+,f
	mov	si(f),i
2:
	mov	(sp)+,k(f)
	mov	(sp)+,cfile
	jmp	succ
1:
	mov	(sp)+,g
	mov	(sp)+,k(f)
	mov	(sp)+,cfile
	br	fail

/ advance stack frame to invoke a parsing rule
/ copy  corsor, watr mark, ignored class to new frame
/ set intial frame length to default (g1)
/ check end of stack
/ r0,r1 are new i,environment

adv:
	inc	advc
	mov	f,(g)
	mov	i,si(f)
	mov	j(f),j(g)
	mov	k(f),k(g)
	mov	n(f),n(g)
	mov	g,f
	add	$g1,g
	cmp	g,$stke
	bhis	1f
	mov	r0,i
	mov	r1,env(f)
	jmp	contin
1:
	jsr	r0,errcom
		<stack overflow\0>;.even

/pbundle entered with pointer to earliest element of bunlde
/to reduce from the top of stack in r0
/exit with pointer to bundle in r0, or zero if bundle is empty

pbundle:
	cmp	r0,g
	blo	1f
	clr	r0	/empty bundle
	rts	pc
1:
	mov	r0,-(sp)
	mov	r0,r1
	mov	(r1)+,r0
	cmp	r1,g
	beq	2f		/trivial bundle
1:
	mov	r1,-(sp)
	jsr	pc,kput
	mov	(sp)+,r1
	mov	(r1)+,r0
	cmp	r1,g
	blos	1b
	mov	k(f),r0
2:
	mov	(sp)+,g
	rts	pc

/ tmg translation rule interpreter (generator)
/ see tmgc.s for definitions

tracing = 1
f = r5
.globl x,si,ek,ep,ek.fs,ep.fs,fs
.globl trswitch,trace
.globl start,end,tables,ktab,ktat
.globl errcom
.globl generate,.tp
i = r3

/ if exit bit is on pop stack frame restore inst counter and return

generate:
bit	$1,x(f)
	beq	gcontin
	sub	$fs,f
	mov	si(f),i
	rts	pc
gcontin:
    .if tracing
	tst	trswitch
	beq	1f
	mov	$'g,r0
	jsr	pc,trace
1:
    .endif 
/ get interpreted instruction, decode by range of values

	mov	(i)+,r0
	mov	r0,x(f)
	bic	$1,r0
.if ..
	cmp	r0,$..
	blo	badadr
.endif
	cmp	r0,$start
	blo	gf
	cmp	r0,$end
	blo	gc
	cmp	r0,$tables
	blo	gf
	neg	r0
	cmp	r0,$ktat
	blo	gk
badadr:
	jsr	r0,errcom
		<bad address in translation\0>;.even

/ builtin  translation function
gf:
	jmp	(r0)

/ tmg-coded translation subroutine
/ execute it in current environment
gc:
	mov	i,si(f)
	mov	r0,i
	mov	ek(f),ek.fs(f)
	mov	ep(f),ep.fs(f)
	add	$fs,f
	jsr	pc,gcontin
	br	generate

/ delivered compound translation
/ instruction counter is in ktable
/ set the k environment for understanding 1, 2 ...
/ to designate this frame
gk:
	mov	f,ek(f)
	add	$ktab,r0
	mov	r0,i
	br	gcontin

/ execute rule called for by 1 2 ...
/ found relative to instruction counter in the k environment
/ this frame becomes th p environment for
/ any parameters passed with this invocation
/ e.g. for 1(x) see also .tq
.tp:
	movb	(i)+,r0
	movb	(i)+,r2
	inc	r0
	asl	r0
	mov	i,si(f)
	mov	f,ep.fs(f)
	mov	ek(f),r1
	mov	si(r1),i
	sub	r0,i
	add	$fs,f
	mov	f,ek(f)
	asl	r2
	beq	2f
/element is 1.1, 1.2, .. 2.1,...
	mov	(i),i
	neg	i
	bge	1f
	jsr	r0,errcom
		<not a bundle\0>;.even
1:
	cmp	i,$ktat
	bhis	badadr
	add	$ktab,i
	sub	r2,i
2:
	jsr	pc,gcontin
	br	generate

/ tmg output routines/ and iget
f = r5
i = r3
.globl env,si
.globl errcom
.globl cfile,lfile
.globl putch,obuild,iget,flush
.globl outb,outt,outw
.globl start

/ adds 1 or 2 characters in r0 to output

putch:
	clr	-(sp)
	mov	r0,-(sp)
	mov	sp,r0
	jsr	pc,obuild
	add	$4,sp
	rts	pc

/ r0 points to string to put out  on current output file (cfile)
/ string terminated by 0
/ if last file differed from current file, flush output buffer first
/ in any case flush output buffer when its write pointer (outw)
/ reaches its top (outt)

obuild:
	cmp	cfile,lfile
	beq	1f
	mov	r0,-(sp)
	jsr	pc,flush
	mov	(sp)+,r0
	mov	cfile,lfile
1:
	mov	outw,r1
1:
	tstb	(r0)
	beq	1f
	movb	(r0)+,outb(r1)
	inc	r1
	mov	r1,outw
	cmp	r1,$outt
	blt	1b
	mov	r0,-(sp)
	jsr	pc,flush
	mov	(sp)+,r0
	br	obuild
1:
	rts	pc

/ copy output buffer onto last output file and clear buffer

flush:
	mov	outw,0f
	mov	lfile,r0
	sys	write;outb;0:0
	clr	outw
	rts	pc


/ get interpreted instruction for a parsing rule
/ negative instruction is a pointer to a parameter in this
/ stack fromae, fetch that instead
/ put environment pointer in r1

iget:
	mov	f,r1
	mov	(i)+,r0
	bge	2f
	mov	r0,-(sp)	/save the exit bit 
	bic	$-2,(sp)
	bic	(sp),r0
1:			/chase parameter
	mov	env(r1),r1
	add	si(r1),r0
	mov	(r0),r0
	blt	1b
	mov	env(r1),r1
	bis	(sp)+,r0
2:
	rts	pc
/there followeth the driving tables
start:

.data
succc:	0
continc:	0
failc:	0
advc:	0
.text
