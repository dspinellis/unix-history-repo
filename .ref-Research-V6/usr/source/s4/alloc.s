/	storage allocator for use with C
/
/
/
/	hand-tooled from C compilation to modify save-return
/	so that it can be called from within the C save
/	when running with coroutines
/
/#
//*
/ *	C storage allocator
/ *	(circular first fit strategy)
/ */
/#define BLOK 512
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
/	static char **t;
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
/		if((*alloct=t=sbrk(BLOK*sizeof(p))) == -1)
/			return(-1);
/		if(t!=alloct+1)
/			alloct->word =| BUSY;
/		alloct = (*t = &t[BLOK]-1);
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
.globl	_allocs
.data
_allocs=.
2+_allocs
_allocs
.globl	_allocp
.data
_allocp=.
2+_allocs
.globl	_alloct
.data
_alloct=.
2+_allocs
.globl	_alloc

.globl	_sbrk
.text
_alloc:
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
jbr	L6
L7:mov	r3,r2
mov	(r3),r3
bic	$!177776,r3
cmp	r2,_allocp
jhis	L6
cmp	r3,_allocp
jlo	L6
mov	$2000,-(sp)
jsr	pc,*$_sbrk
tst	(sp)+
mov	r0,t
mov	r0,*_alloct
cmp	$177777,r0
jeq	L11
mov	_alloct,r0
add	$2,r0
cmp	t,r0
jeq	L12
bis	$1,*_alloct
L12:mov	t,r0
add	$1776,r0
mov	r0,*t
mov	r0,_alloct
mov	$_allocs,*_alloct
bis	$1,*_alloct
L6:bit	$1,(r3)
jeq	L8
jbr	L7
L20001:mov	(r2),(r3)
L8:mov	(r3),r2
bit	$1,(r2)
jeq	L20001
mov	r4,r0
asl	r0
add	r3,r0
cmp	r2,r0
jlo	L7
mov	r4,r0
asl	r0
add	r3,r0
mov	r0,_allocp
cmp	r2,r0
jlos	L13
mov	(r3),*_allocp
L13:mov	_allocp,r0
bis	$1,r0
mov	r0,(r3)
mov	r3,r0
add	$2,r0
L11:
mov	(sp)+,r2
mov	(sp)+,r3
mov	(sp)+,r4
mov	(sp)+,r5
rts	pc
.globl	_free
.text
_free:
mov	r5,-(sp)
mov	sp,r5
mov	4(r5),r0
add	$177776,r0
mov	r0,_allocp
bic	$!177776,*_allocp
mov	(sp)+,r5
rts	pc
.bss
t:	.=.+2
