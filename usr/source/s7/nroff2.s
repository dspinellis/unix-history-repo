/
/

/ nroff2

casead:
	inc	ad
	jsr	pc,skipcont
	tst	nlflg
	bne	1f
	jsr	pc,getchar
	cmp	r0,$'r	/right adj, left ragged
	bne	0f
	mov	$2,jfomod
	br	1f
0:
	cmp	r0,$'l	/left adj, right ragged
	bne	0f
	clr	jfomod
	clr	ad
	br	1f
0:
	cmp	r0,$'c	/centered adj
	bne	0f
	mov	$1,jfomod
	br	1f
0:
	clr	jfomod	/left and right adj
	inc	ad
1:
	rts	pc

casebr:
	jsr	pc,rbreak
	rts	pc

casecc:
	clr	-(sp)
	br	0f
casec2:
	mov	pc,-(sp)
0:
	jsr	pc,skipcont
	jsr	pc,getchar
	cmp	r0,$'\n
	beq	2f
	tst	(sp)
	bne	1f
	mov	r0,cc
	br	2f
1:
	mov	r0,c2
2:
	mov	r0,ch
	tst	(sp)+
	rts	pc

casece:
	jsr	r5,number; 0
	jsr	pc,min
	mov	r0,-(sp)
	jsr	pc,rbreak
	mov	(sp)+,ce
	rts	pc

casefi:
	jsr	pc,rbreak
	inc	fi
	rts	pc

casein:
	jsr	pc,skipcont
	tst	nlflg
	beq	1f
	mov	in1,r0
	br	2f
1:
	jsr	r5,number1; in
	jsr	pc,min
2:
	mov	r0,-(sp)
	jsr	pc,rbreak
	mov	in,in1
	mov	(sp)+,in
	tst	nc
	bne	1f
	mov	in,un
	jsr	pc,setnel
1:
	rts	pc

caseli:
	jsr	r5,number; 0
	jsr	pc,min
	mov	r0,lit
	mov	frame,litlev
	cmp	nl,$-1
	bne	0f
	jsr	pc,newln1
0:
	rts	pc

casell:
	jsr	pc,skipcont
	tst	nlflg
	beq	1f
	mov	ll1,r0
	br	2f
1:
	jsr	r5,number1; ll
	jsr	pc,min
2:
	mov	ll,ll1
	mov	r0,ll
2:
	jsr	pc,setnel
	rts	pc

casels:
	jsr	pc,skipcont
	tst	nlflg
	beq	1f
	mov	ls1,r0
	br	2f
1:
	jsr	r5,number1; ls
	dec	r0
	jsr	pc,min
	inc	r0
2:
	mov	r0,-(sp)
/	jsr	pc,rbreak
	mov	ls,ls1
	mov	(sp)+,ls
	rts	pc

casens:
	inc	nls
	rts	pc

casers:
	clr	nls
	rts	pc

casena:
	clr	ad
	rts	pc

casene:
	jsr	r5,number; 0
	jsr	pc,min
	jsr	pc,need2
	rts	pc

casenf:
	jsr	pc,rbreak
	clr	fi
	rts	pc

casepn:
	jsr	pc,skipcont
	bne	1f
	jsr	r5,number1; pn
	jsr	pc,min
	mov	r0,npn
1:
	rts	pc
casebp:
	mov	ilistp,-(sp)
	clr	-(sp)
	jsr	pc,skipcont
	bne	1f
	jsr	r5,number1; pn
	jsr	pc,min
	mov	r0,(sp)
1:
	jsr	pc,rbreak
	mov	(sp)+,r0
	beq	0f
	mov	r0,npn
0:
	bne	2f
	tst	nls
	bne	3f
2:
	mov	(sp),r0
	jsr	pc,eject
3:
	tst	(sp)+
	rts	pc

casepl:
	jsr	pc,skipcont
	tst	nlflg
	bne	1f
	jsr	r5,number1; pl
	mov	r0,pl
	rts	pc
1:
	mov	$66.,pl
	rts	pc

casesp:
	mov	pc,-(sp)
	br	0f
casesp1:
	clr	-(sp)
0:
	mov	r0,-(sp)
	tst	nb
	bne	0f
	jsr	pc,rbreak
0:
	tst	nls
	bne	2f
	clr	r0
	jsr	pc,findt
	tst	nb
	bne	1f
	tst	trap
	bne	2f
1:
	tst	2(sp)
	beq	1f
	jsr	r5,number;0
	mov	r0,(sp)
1:
	cmp	r1,(sp)
	bge	1f
	mov	r1,(sp)
1:
	mov	(sp),r0
	ble	3f
	jsr	r5,nlines; newline
2:
	cmp	(sp)+,(sp)+
	rts	pc
3:
	add	nl,r0
	cmp	(sp)+,(sp)+
	br	casert1

casert:
	mov	markline,r2
	jsr	pc,skipcont
	bne	0f
	jsr	r5,number1;nl
casert1:
	mov	r0,r2
0:
	tst	r2
	blt	2f
	cmp	r2,nl
	bge	2f
	mov	nl,r1
	mov	r2,nl
	sub	r2,r1
	mov	r1,r2
1:
	mov	$036,r0
	jsr	pc,putchar
	dec	r2
	bgt	1b
	mov	$015,r0
	jsr	pc,putchar
2:
	rts	pc

casesv:
	clr	r0
	jsr	pc,findt
	jsr	r5,number; 0
	cmp	r1,r0
	bge	1f
	mov	r0,sv
	rts	pc
1:
	jsr	r5,nlines; newline
	rts	pc

caseos:
	tst	sv
	beq	2f
	clr	r0
	jsr	pc,findt
	cmp	r1,sv
	bge	1f
	rts	pc
1:
	mov	sv,r0
	clr	sv
	jsr	r5,nlines; newline
2:
	rts	pc

casetr:
	jsr	pc,skipcont
1:
	jsr	pc,getchar
	cmp	r0,$'\n
	beq	1f
	mov	r0,r1
	jsr	pc,getchar
	cmp	r0,$'\n
	bne	2f
	mov	$' ,r0
2:
	movb	r0,trtab(r1)
	br	1b
1:
	rts	pc

caseta:
	clr	-(sp)
	mov	$tabtab,r1
1:
	jsr	pc,getchar
	tst	nlflg
	bne	1f
	cmpb	r0,$'+
	bne	0f
	inc	(sp)
	br	2f
0:
	cmpb	r0,$'-
	beq	2f
	cmpb	r0,$'0
	blo	1b
	cmpb	r0,$'9
	bhi	1b
2:
	mov	r0,ch
	jsr	pc,atoi
	beq	1f
	jsr	pc,min
	dec	r0
	ble	1f
	cmp	r1,$tabtab
	beq	0f
	tst	(sp)
	bne	3f
	cmp	r0,-2(r1)
	bgt	0f
3:
	add	-2(r1),r0
0:
	clr	(sp)
	mov	r0,(r1)+
	cmp	r1,$etabtab
	blo	1b
	tst	-(r1)
1:
	tst	(sp)+
	clr	(r1)
	rts	pc

caseti:
	jsr	r5,number; in
	jsr	pc,min
	mov	r0,-(sp)
	jsr	pc,rbreak
	mov	(sp)+,un1
	jsr	pc,setnel
	rts	pc

caseul:
	jsr	r5,number; 0
	jsr	pc,min
	mov	r0,ul
	rts	pc

casetl:
	jsr	pc,header
	rts	pc

caselt:
	jsr	pc,skipcont
	tst	nlflg
	beq	1f
	mov	llh1,r0
	br	2f
1:
	jsr	r5,number1; llh
	jsr	pc,min
2:
	mov	llh,llh1
	mov	r0,llh
	rts	pc


casehc:
	jsr	pc,skipcont
	jsr	pc,getchar
	cmp	r0,$'\n
	bne	1f
	movb	$200,r0
1:
	mov	r0,ohc
	rts	pc

casetc:
	jsr	pc,skipcont
	jsr	pc,getchar
	cmp	r0,$'\n
	bne	1f
	mov	$014,r0
1:
	mov	r0,tabc
	rts	pc

caselc:
	jsr	pc,skipcont
	jsr	pc,getchar
	cmp	r0,$'\n
	bne	1f
	mov	$'.,r0
1:
	mov	r0,dotc
	rts	pc

casenc:
	jsr	pc,skipcont
	jsr	pc,getchar
	cmp	r0,$'\n
	bne	1f
	mov	$034,r0
1:
	mov	r0,numc
	rts	pc

casehy:
	mov	$1,hyf
	jsr	pc,skipcont
	bne	1f
	jsr	pc,atoi
	beq	1f
	jsr	pc,min
	mov	r0,hyf
1:
	rts	pc

casenh:
	clr	hyf
	rts	pc

casenp:
	jsr	pc,skipcont
	tst	nlflg
	beq	2f
	clr	ni
	mov	$1,nms
	mov	$1,ndf
	rts	pc
casenm:
	clr	numbmod
	clr	nn
	jsr	pc,skipcont
	tst	nlflg
	beq	1f
	rts	pc
1:
	inc	numbmod
	jsr	r5,number1; lnumber
	jsr	pc,min
	mov	r0,lnumber
	jsr	pc,skipcont
2:
	jsr	r5,3f; ndf
	tst	ndf
	bne	1f
	inc	ndf
1:
	jsr	pc,skipcont
	jsr	r5,3f; nms
	jsr	pc,skipcont
	jsr	r5,3f; ni
	rts	pc
3:
	tst	nlflg
	bne	4f
	jsr	pc,atoi
	beq	4f
	jsr	pc,min
	mov	r0,*(r5)
4:
	tst	(r5)+
	rts	r5

casenn:
	jsr	r5,number; 0
	jsr	pc,min
	mov	r0,nn
	rts	pc

casear:
	clr	ro
	rts	pc

caseroc:
	mov	$2,ro
	rts	pc
casero:
	mov	$1,ro
	rts	pc

casenx:
	jsr	pc,skipcont
	jsr	r5,getname; nextf
	inc	nx
	jsr	pc,nextfile
	inc	nlflg
	clr	ip
	clr	ap
	clr	nspace
	clr	pendt
	mov	$ilist,ilistp
	rts	pc

casepo:
	jsr	pc,skipcont
	tst	nlflg
	beq	1f
	mov	po1,r0
	br	2f
1:
	jsr	r5,number1; po
	jsr	pc,min
2:
	mov	r0,-(sp)
/	jsr	pc,rbreak
	mov	po,po1
	mov	(sp)+,po
	rts	pc

caseig:
	inc	skp
	jsr	pc,copyb
	rts	pc

casern:
/	inc	lgf
	jsr	pc,skipcont
	jsr	r5,getname;bname
	beq	2f
	jsr	pc,findmn
	beq	2f
	mov	oldptr,-(sp)
/	inc	lgf
	jsr	pc,skipcont
	jsr	r5,getname;bname
	beq	1f
	jsr	pc,findmn
	jsr	pc,clrold
	mov	(sp),r1
	tst	-(r1)
	mov	(r1),(sp)
	bic	$77777,(sp)
	mov	bname,(r1)
	bis	(sp),(r1)
1:
	tst	(sp)+
2:
	rts	pc

caserm:
/	inc	lgf
	jsr	pc,skipcont
	jsr	r5,getname;bname
	beq	1f
	jsr	pc,findmn
	jsr	pc,clrold
1:
	rts	pc

caseas:
	inc	app
caseds:
	inc	ds
	br	casede
caseam:
	inc	app
casede:
	tst	op
	beq	1f
	jsr	pc,wbfl
1:
/	inc	lgf
	jsr	pc,skipcont
	jsr	r5,getname; bname
	bne	1f
	clr	r1
	br	6f
1:
	clr	skp
	jsr	pc,finds
	beq	7f
	tst	ds
	beq	0f
	tst	skp
	bne	5f
	jsr	pc,copys
	br	5f
0:
	jsr	pc,copyb
5:
	jsr	pc,wbfl
	jsr	pc,clrold
	tst	newptr
	beq	0f
	bis	$100000,bname
	mov	bname,*newptr
0:
	mov	r1,-(sp)
	mov	apptr,r1
	beq	0f
	mov	$004,r0
	jsr	pc,wbt
0:
	mov	(sp)+,r1
6:
	clr	app
	clr	ds
	rts	pc
7:
	tst	macerr
	bne	0f
	jsr	r5,stringfl; 8f
0:
	inc	macerr
	cmp	macerr,$1
	jeq	edone
	jmp	done2
/	br	6b
8: <Too many string/macro names.\n\0>
	.even

findmn:
	mov	$contab,r1
1:
	mov	(r1)+,r0
	bic	$100000,r0
	cmp	bname,r0
	beq	2f
	cmp	(r1)+,$-1
	bne	1b
	clr	r1
2:
	mov	r1,oldptr
	rts	pc

finds:
	jsr	pc,findmn
	clr	newptr
	clr	apptr
	clr	aplnk
	tst	app
	beq	0f
	tst	oldptr
	bne	5f
0:
	mov	$contab,r1
1:
	tst	(r1)+
	beq	2f
	cmp	(r1)+,$-1
	bne	1b
1:
	inc	skp
	clr	r1
	rts	pc
2:
	jsr	pc,alloc
	beq	1b
	tst	oldptr
	bne	3f
4:
	tst	-(r1)
	bis	$100000,bname
	mov	bname,(r1)+
	mov	nextb,(r1)
	br	6f
3:
	tst	diflg
	bne	4b
	mov	nextb,(r1)
	tst	-(r1)
	mov	r1,newptr
	br	6f
5:
	tst	-(r1)
	bmi	1f
	clr	app
	br	0b
1:
	tst	(r1)+
	clr	oldptr
	mov	ip,-(sp)
	mov	(r1),ip
1:
	jsr	pc,rbf
	tst	r0
	bne	1b
	mov	ip,r1
	mov	r1,apptr
	tst	diflg
	bne	0f
	jsr	pc,incoff
0:
	mov	r1,nextb
	mov	(sp)+,ip
6:
	clr	app
	mov	nextb,r1
	rts	pc

clrold:
	mov	oldptr,r0
	beq	1f
	mov	(r0),-(sp)
	clr	(r0)
	tst	-(r0)
	bmi	0f
	clr	(sp)
0:
	clr	(r0)
	mov	(sp)+,r0
	beq	1f
	jsr	pc,free
1:
	rts	pc

caseda:
	inc	app
casedi:
/	inc	lgf
	clr	ditf
	jsr	pc,skipcont
	jsr	r5,getname; bname
	beq	1f
	tst	op
	bne	3f
	inc	diflg
	jsr	pc,finds
	beq	3f
	mov	r1,op
	jsr	pc,clrold
/	mov	blss,sblss
/	mov	alss,salss
/	clr	blss
/	clr	alss
	clr	dnl
	br	3f
1:
	tst	op
	beq	3f
	clr	r0
	jsr	pc,pchar1
	jsr	pc,wbfl
/	mov	sblss,blss
/	mov	salss,alss
	clr	op
3:
	clr	app
	clr	diflg
	rts	pc

caseev:
	jsr	pc,skipcont
	tst	nlflg
	beq	2f
	cmp	evp,$elist
	ble	5f
1:
	sub	$2,evp
	mov	*evp,-(sp)
	br	3f
2:
	jsr	pc,atoi
	beq	6f
	cmp	r0,nev
	bgt	6f
	tst	r0
	blt	6f
	cmp	evp,$eliste
	bgt	6f
	mov	r0,-(sp)
	mov	ev,*evp
	add	$2,evp
3:
	cmp	(sp),ev
	beq	4f
	mov	$1024.,r3
	mpy	ev,r3
	mov	r3,8f
	mov	$1024.,r3
	mpy	(sp),r3
	mov	r3,9f
	mov	ibf,r0
	sys	0;7f
.data
7:	sys	seek; 8:.. ; 0
.text
	mov	ibf,r0
	sys	write; block; 1024.
	mov	ibf1,r0
	sys	0;7f
.data
7:	sys	seek; 9:.. ; 0
.text
	mov	blocksize,0f
	mov	ibf1,r0
	sys	0;7f
.data
7:	sys	read; block; 0:..
.text
4:
	mov	(sp)+,ev
5:
	rts	pc
6:
	jsr	r5,string;9f
	rts	pc
9: <Cannot do "ev".\n\0>
.even

caseif:
	clr	-(sp)
	jsr	pc,skipcont
	jsr	pc,getchar
	cmp	r0,$'!
	bne	1f
	inc	(sp)
	br	2f
1:
	mov	r0,ch
2:
	jsr	pc,atoi
	beq	1f
	tst	r0
	bgt	5f /true
	br	6f /false
1:
	jsr	pc,getchar
	cmp	r0,$'e
	bne	1f
	bit	$1,pn
	bne	6f
	br	5f
1:
	cmp	r0,$'o
	bne	1f
	bit	$1,pn
	beq	6f
	br	5f
1:
	cmpb	r0,$'n
	beq	5f
1:
	tst	(sp)+
	rts	pc
5:
	tst	(sp)
	bne	1b
2:
	clr	column
	jsr	pc,getchar
	bmi	0f
	cmpb	r0,$' /
	beq	2b
0:
	mov	r0,ch
	inc	nflush
	br	1b
6:
	tst	(sp)
	beq	1b
	br	2b

casewh:
	clr	-(sp)
	jsr	pc,skipcont
	jsr	pc,getchar
	cmp	r0,$'x
	bne	1f
	mov	$-1,r0
	jsr	pc,findt
	add	nl,r1
	sub	dnl,r1
	mov	r1,r0
	br	2f
1:
	mov	r0,ch
	jsr	pc,atoi
	beq	4f
2:
	mov	r0,(sp)
	jsr	pc,skipcont
	jsr	r5,getname; bname
	tstb	bname
	bne	1f
	clr	bname
1:
	mov	(sp),r0
	jsr	pc,findn
	tst	r1
	beq	1f
	mov	bname,[mlist-nlist](r1)
	br	4f
1:
	mov	$mlist,r1
2:
	tst	(r1)+
	beq	3f
	cmp	r1,$mliste
	bgt	4f
	br	2b
3:
	mov	bname,-2(r1)
	mov	(sp),[nlist-mlist-2](r1)
4:
	tst	(sp)+
	rts	pc

findn:
	mov	$nlist,r1
1:
	cmp	(r1),r0
	beq	3f
2:
	tst	(r1)+
	cmp	r1,$nliste
	bne	1b
	clr	r1
	rts	pc
3:
	tst	[mlist-nlist](r1)
	beq	2b
	rts	pc

findm:
	mov	$mlist,r1
1:
	cmp	(r1),bname
	beq	3f
2:
	tst	(r1)+
	cmp	r1,$mliste
	bne	1b
	clr	r1
	rts	pc
3:
	sub	$[mlist-nlist],r1
	rts	pc

casech:
	clr	-(sp)
	jsr	pc,skipcont
	jsr	pc,atoi
	beq	2f
	jsr	pc,findn
	br	3f
2:
	jsr	r5,getname; bname
	tstb	bname
	beq	1f
	jsr	pc,findm
3:
	tst	r1
	beq	1f
	mov	r1,(sp)
	jsr	pc,skipcont
	jsr	pc,atoi
	beq	2f
	mov	r0,*(sp)
1:
	tst	(sp)+
	rts	pc
2:
	jsr	pc,getchar
	tst	nlflg
	bne	1b
	mov	*(sp),r0
	beq	1b
	bgt	0f
	add	pl,r0
	inc	r0
0:
	sub	nl,r0
	ble	1b
	dec	r0
	ble	1b
	cmp	dnl,r0
	bge	0f
	mov	dnl,r0
0:
	sub	r0,*(sp)
	br	1b

casemk:
	jsr	pc,skipcont
	beq	1f
	mov	nl,markline
	rts	pc
1:
	jsr	r5,getname; bname
	mov	bname,r0
	beq	2f
	jsr	pc,findr
	beq	2f
	mov	nl,(r1)
2:
	rts	pc

casetm:
	inc	copyf
	jsr	pc,skipcont
	mov	$bname,r1
1:
	jsr	pc,getchar
	bmi	1b
	movb	r0,(r1)+
	tst	nlflg
	bne	2f
	cmp	r1,$ename
	blo	1b
	movb	$'\n,-1(r1)
2:
	clrb	(r1)
	jsr	r5,stringfl; bname
	clr	copyf
	rts	pc

caserd:
	jsr	pc,skipcont
	jsr	r5,getname; bname
	tst	iflg
	bne	1f
	tst	quiet
	bne	2f
	tstb	bname
	beq	5f
	jsr	r5,string; bname
	jsr	r5,string; 3f
1:
	jsr	pc,collect
	inc	tty
	jsr	pc,pushi
	beq	6f
	mov	$-1,ip
	rts	pc
2:
	bic	$10,ttym
	mov	ttyid,r0
	sys	stty; ttys
	jsr	pc,flush
5:
	jsr	r5,string;4f
	br	1b
6:
	jmp	rdtty2
3:	<: \0>
4:	<\0> /bell
	.even

caseaf:
	jsr	pc,skipcont
	bne	3f
	jsr	r5,getname;bname
	mov	bname,r0
	beq	3f
	jsr	pc,findr
	jsr	pc,skipcont
	jsr	pc,getchar
	mov	$4f,r2
1:
	cmpb	r0,(r2)+
	beq	2f
	inc	r2
	tstb	(r2)
	bne	1b
2:
	movb	(r2),[flist-vlist](r1)
3:
	rts	pc
4:
.byte '1,1
.byte 'i,2
.byte 'I,3
.byte 'a,4
.byte 'A,5
.byte 0,0

casenr:
	jsr	pc,skipcont
	bne	5f
	jsr	r5,getname; bname
	mov	bname,r0
	jsr	pc,findr
	mov	r1,0f
	beq	5f
	jsr	pc,skipcont
	bne	5f
	jmp	7f
.data
7:	jsr	r5,number1; 0:..
	jmp	8f
.text
8:
	bne	1f
	clr	r0
1:
	mov	r0,*0b
	jsr	pc,skipcont
	bne	5f
	jsr	pc,atoi
	beq	5f
	mov	r0,[inclist-vlist](r1)
5:
	rts	pc

casefl:
	jsr	pc,rbreak
	jsr	pc,flush
	rts	pc

casexh:
/	tst	x.5
/	bne	1f
	inc	x.5
	rts	pc
/1:
/	clr	x.5
/	rts	pc

caseso:
	jsr	pc,skipcont
	tst	nlflg
	bne	1f
	jsr	r5,getname; nextf
	tstb	nextf
	beq	1f
	sys	open; nextf; 0
	bes	1f
	mov	ifp,r1
	cmp	r1,$ifle
	bhi	1f
	mov	r0,-(sp)
	jsr	pc,flushi
	mov	ifile,(r1)
	mov	(sp)+,ifile
	mov	ioff,[offl-ifl](r1)
	add	$2,ifp
	clr	ioff
	inc	nx
	inc	nflush
	tst	(r1)
	bne	1f
	jsr	pc,savebuf
1:
	rts	pc

caseeo:
	clr	r0
	br	1f
caseec:
	jsr	pc,skipcont
	jsr	pc,getchar
	cmpb	r0,$'\n
	bne	1f
	movb	$'\\,r0
1:
	movb	r0,eschar
	movb	r0,eschar+1
	rts	pc

casest:
	4

caseem:
	clr	em
	jsr	pc,skipcont
	bne	1f
	jsr	r5,getname;bname
	beq	1f
	mov	bname,em
1:
	rts	pc

casefc:
	mov	$4,fc
	mov	$' ,padc
	jsr	pc,skipcont
	bne	1f
	jsr	pc,getchar
	bmi	1f
	movb	r0,fc
	jsr	pc,skipcont
	bne	1f
	mov	ch,r0
	bmi	1f
	cmpb	r0,fc
	beq	1f
	movb	r0,padc
1:
	rts	pc

casepi:
	tst	toolate
	bne	1f
	jsr	pc,skipcont
	jsr	r5,getname;bname
	beq	1f
	sys	42. /pipe
	jes	place
	mov	r0,pipin
	mov	r1,outid
	sys	fork
	br	2f
	jes	place
	mov	$1,r0
	sys	close
	mov	pipin,r0
	sys	close
1:
	inc	toolate
	rts	pc
2:
	clr	r0
	sys	close
	mov	pipin,r0
	sys	41. /dup
	mov	outid,r0
	sys	close
	sys	exec;bname;args
	jsr	r5,string;9f
	sys	exit
args: bname;0
9: <exec failed\n\0>
.even
