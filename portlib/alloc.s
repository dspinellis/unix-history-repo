/	storage allocator for use with C
/
/
/
/	hand-tooled from C compilation to modify save-return
/	so that it can be called from within the C save when
/	running with coroutines
/
/#
//*
/ *	C storage allocator
/ *	(circular first fit strategy)
/ */
/#define BUSY 01
/
/char *allocs[2] {		/*initial empty arena*/
/	&allocs[1],
/	&allocs[0]
/};
/struct { int word; };
/char **allocp &allocs[1];	/*current search pointer*/
/char **alloct &allocs[1];	/*top of arena (last cell)*/
/
/alloc(nbytes)
/{
/	register int nwords;
/	register char **p, **q;
/	static char *t;
/
/	allocs[0].word =| BUSY;	/*static initialization*/
/	allocs[1].word =| BUSY;
/
/	nwords = (nbytes+(2*sizeof(p)-1))/sizeof(p);
/	for(p=allocp;;) {
/		do {
/			if((p->word&BUSY)==0) {
/				while(((q = *p)->word&BUSY)==0)
/					*p = *q;
/				if(q >= &p[nwords])
/					goto found;
/			}
/			q = p;
/			p = p->word & ~BUSY;
/		} while(q>=allocp || p<allocp);
/		if((t = sbrk(sizeof(p)*nwords)) == -1)
/			return(-1);
/		*alloct = t;
/		if(t != alloct + 1)
/			alloct->word =| BUSY;
/		alloct = (*t = &t[nwords]-1);
/		*alloct = allocs;
/		alloct->word =| BUSY;
/	}
/found:
/	allocp = &p[nwords];
/	if(q>allocp)
/		*allocp = *p;
/	*p = allocp.word|BUSY;
/	return(p+1);
/}
/
/free(p)
/char **p;
/{
/	allocp = p-1;
/	allocp->word =& ~BUSY;
/}
.data
.globl	_allocs
_allocs:
2+_allocs
_allocs
.data
.globl	_allocp
_allocp:
2+_allocs
.data
.globl	_alloct
_alloct:
2+_allocs
.text
.globl	_alloc
.globl	_getvec
_getvec:
_alloc:
~~alloc:
.bss
L2:.=.+2
.text
~p=r3
~q=r2
~t=L2
~nbytes=4
~nwords=r4
mov	r5,-(sp)
mov	sp,r5
mov	r4,-(sp)
mov	r3,-(sp)
mov	r2,-(sp)
bis	$1,_allocs
bis	$1,2+_allocs
mov	4(r5),r4
add	$3,r4
asr	r4
mov	_allocp,r3
jbr	L7
L12:mov	L2,*_alloct
mov	_alloct,r0
add	$2,r0
cmp	L2,r0
jeq	L13
bis	$1,*_alloct
L13:
mov	r4,r0
dec	r0
asl	r0
add	L2,r0
mov	r0,*L2
mov	r0,_alloct
mov	$_allocs,*_alloct
bis	$1,*_alloct
L7:bit	$1,(r3)
jeq	L9
L8:mov	r3,r2
mov	(r3),r3
bic	$1,r3
cmp	_allocp,r2
jlos	L7
cmp	_allocp,r3
jhi	L7
mov	r4,r0
asl	r0
mov	r0,(sp)
.globl	_sbrk
jsr	pc,*$_sbrk
mov	r0,L2
cmp	$-1,r0
jne	L12
L1:
mov	(sp)+,r2
mov	(sp)+,r3
mov	(sp)+,r4
mov	(sp)+,r5
rts	pc
L20001:mov	(r2),(r3)
L9:mov	(r3),r2
bit	$1,(r2)
jeq	L20001
mov	r4,r0
asl	r0
add	r3,r0
cmp	r2,r0
jlo	L8
mov	r4,r0
asl	r0
add	r3,r0
mov	r0,_allocp
cmp	r0,r2
jhis	L14
mov	(r3),(r0)
L14:
bis	$1,r0
mov	r0,(r3)
mov	r3,r0
add	$2,r0
jbr	L1
.globl	_free
.globl	_cfree,_relvec
_cfree:
_relvec:
_free:
~~free:
~p=4
mov	r5,-(sp)
mov	sp,r5
mov	4(r5),r0
add	$-2,r0
mov	r0,_allocp
bic	$1,*_allocp
mov	(sp)+,r5
rts	pc
.globl
.data
