/
/

/ roff2 -- runoff

casead:
	jsr	pc,rbreak
	inc	ad
	rts	pc

	rts	pc

casebr:
	jsr	pc,rbreak
	rts	pc

casecc:
	jsr	pc,skipcont
	jsr	pc,getchar
	cmp	r0,$'\n
	beq	1f
	movb	r0,cc
1:
	mov	r0,ch
	rts	pc

casece:
	jsr	pc,rbreak
	jsr	r5,number; 0
	jsr	pc,min
	mov	r0,ce
	jsr	pc,need
	rts	pc

caseds:
	jsr	pc,rbreak
	mov	$2,ls
	rts	pc

casefi:
	jsr	pc,rbreak
	inc	fi
	rts	pc

casein:
	jsr	pc,rbreak
	jsr	r5,number; in
	jsr	pc,min
	mov	r0,in
	mov	r0,un
	rts	pc

caseix:
	jsr	r5,number; in
	jsr	pc,min
	mov	r0,in
	rts	pc

caseli:
	jsr	r5,number; 0
	mov	r0,-(sp)
1:
	dec	(sp)
	blt	1f
	jsr	pc,flushi
	clr	nlflg
	jsr	pc,text
	br	1b
1:
	tst	(sp)+
	rts	pc

casell:
	jsr	r5,number; ll
	jsr	pc,min
	mov	r0,ll
	rts	pc

casels:
	jsr	pc,rbreak
	jsr	pc,skipcont
	jsr	pc,getchar
	cmp	r0,$'\n
	bne	1f
	mov	ls1,ls
	rts	pc
1:
	mov	r0,ch
	jsr	r5,number1; ls
	dec	r0
	jsr	pc,min
	inc	r0
	mov	r0,ls
	mov	r0,ls1
	rts	pc

casena:
	jsr	pc,rbreak
	clr	ad
	rts	pc

casene:
	jsr	r5,number; 0
	jsr	pc,min
	jsr	pc,need
	rts	pc

casenf:
	jsr	pc,rbreak
	clr	fi
	rts	pc

casepa:
casebp:
	jsr	pc,rbreak
	jsr	pc,eject
	jsr	pc,skipcont
	tst	nlflg
	bne	1f
	jsr	r5,number; pn
	jsr	pc,min
	mov	r0,pn
1:
	rts	pc

casebl:
	jsr	pc,rbreak
	jsr	r5,number; 0
	jsr	pc,min
	mov	r0,-(sp)
	jsr	pc,need2
1:
	dec	(sp)
	blt	1f
	mov	$' ,r0
	jsr	pc,storeline
	jsr	pc,rbreak
	br	1b
1:
	tst	(sp)+
	rts	pc

casepl:
	jsr	r5,number; pl
	mov	r0,pl
	jsr	pc,topbot
	rts	pc

casesk:
	jsr	r5,number; 0
	jsr	pc,min
	mov	r0,skip
	rts	pc

casesp:
	jsr	pc,rbreak
	jsr	r5,number; 0
	jsr	r5,nlines; nline
	rts	pc

casess:
	jsr	pc,rbreak
	mov	$1,ls
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
	mov	$tabtab,r1
1:
	jsr	r5,number; 0
	jsr	pc,min
	dec	r0
	ble	1f
	movb	r0,(r1)+
	br	1b
1:
	clrb	(r1)+
	rts	pc

caseti:
	jsr	pc,rbreak
	jsr	r5,number; in
	jsr	pc,min
	mov	r0,un
	rts	pc

caseul:
	jsr	r5,number; 0
	jsr	pc,min
	mov	r0,ul
	rts	pc

caseun:
	jsr	r5,number; 0
	sub	in,r0
	neg	r0
	jsr	pc,min
	mov	r0,un
	rts	pc

casehx:
	tst	hx
	beq	1f
	clr	hx
	br	2f
1:
	inc	hx
2:
	jsr	pc,topbot
	rts	pc

casehe:
	jsr	r5,headin; ehead
	mov	ehead,ohead
	rts	pc
casefo:
	jsr	r5,headin; efoot
	mov	efoot,ofoot
	rts	pc

caseeh:
	jsr	r5,headin; ehead
	rts	pc

caseoh:
	jsr	r5,headin; ohead
	rts	pc

caseef:
	jsr	r5,headin; efoot
	rts	pc

caseof:
	jsr	r5,headin; ofoot
	rts	pc

casem1:
	jsr	r5,number; ma1
	jsr	pc,min
	mov	r0,ma1
	br	1f

casem2:
	jsr	r5,number; ma2
	jsr	pc,min
	mov	r0,ma2
	br	1f

casem3:
	jsr	r5,number; ma3
	jsr	pc,min
	mov	r0,ma3
	br	1f

casem4:
	jsr	r5,number; ma4
	jsr	pc,min
	mov	r0,ma4
1:
	jsr	pc,topbot
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
	mov	$' ,r0
1:
	mov	r0,tabc
	rts	pc

casehy:
	jsr	r5,number; 0
	mov	r0,hyf
	rts	pc

casen1:
	jsr	pc,rbreak
	mov	$1,numbmod
	br	1f
casen2:
	jsr	pc,rbreak
	mov	$2,numbmod
1:
	clr	nn
	jsr	r5,number; 0
	tst	r0
	ble	1f
	mov	r0,lnumber
	rts	pc
1:
	clr	numbmod
	rts	pc

casenn:
	jsr	r5,number; 0
	jsr	pc,min
	mov	r0,nn
	rts	pc

caseni:
	jsr	r5,number; ni
	jsr	pc,min
	mov	r0,ni
	rts	pc

casejo:
	jsr	r5,number; 0
	mov	r0,jfomod
	rts	pc

casear:
	clr	ro
	rts	pc

casero:
	inc	ro
	rts	pc

casenx:
	jsr	pc,skipcont
	jsr	r5,getname; nextf
	inc	nx
	jsr	pc,nextfile
	inc	nlflg
	clr	ip
	mov	$ilist,ilistp
	rts	pc

casepo:
	jsr	pc,rbreak
	jsr	r5,number; po
	jsr	pc,min
	mov	r0,po
	rts	pc

casede:
	tst	ip
	bne	5f
	jsr	pc,skipcont
	jsr	r5,getname; bname
	clr	skp
	mov	$contab,r1
	clr	-(sp)
1:
	mov	(r1)+,(sp)
	beq	2f
	bic	$100000,(sp)
	cmp	bname,(sp)
	bne	3f
2:
	bis	$100000,bname
	mov	nextb,(r1)
	mov	bname,-(r1)
	br	4f
3:
	cmp	(r1)+,$-1
	bne	1b
	inc	skp
4:
	tst	(r1)+
	jsr	pc,copyb
	tst	(sp)+
5:
	rts	pc

caseig:
	inc	skp
	jsr	pc,copyb
	rts	pc

casemk:
	jsr	pc,rbreak
	mov	$002,r0	/stx
	jsr	pc,putchar
	rts	pc

