#define ASSERT(p) if(!(p))botch("p");else
botch(s)
char *s;
{
	printf("assertion botched: %s\n",s);
	abort();
}
/*	C storage allocator
 *	circular first-fit strategy
 *	works with noncontiguous, but monotonically linked, arena
 *	each block is preceded by a ptr to the (pointer of) 
 *	the next following block
 *	blocks are exact number of words long; BUSY
 *	bit in ptr is 1 for busy, 0 for idle
 *	gaps in arena are merely noted as busy blocks
 *	last block of arena (pointed to by alloct) is empty and
 *	has a pointer to first
 *	idle blocks are coalesced during space search
*/
/*	all these defines must be powers of 2 */
#define WORD sizeof(struct store)
#define BLOCK 1024
#define BUSY 1
#define NULL 0
#define testbusy(p) ((int)(p)&BUSY)
#define setbusy(p) ((int)(p)+BUSY)
#define clearbusy(p) ((int)(p)&~BUSY)

struct store { struct store *ptr; };

struct store allocs[] = {	/*initial arena*/
	setbusy(&allocs[1].ptr),
	setbusy(&allocs[0].ptr)
};
struct store *allocp = &allocs[1];	/*search ptr*/
struct store *alloct = &allocs[1];	/*arena top*/
struct store *allocx;		/*for benefit of realloc*/
struct store *sbrk();

struct store *
malloc(nbytes)
unsigned nbytes;
{
	register struct store *p, *q;
	register nw;
	static temp;	/*coroutines assume no auto*/

	nw = (nbytes+2*WORD-1)/WORD;
	ASSERT(allocp>allocs && allocp<=alloct);
	for(p=allocp; ; ) {
		for(temp=0; ; ) {
			if(!testbusy(p->ptr)) {
				while(!testbusy((q=p->ptr)->ptr)) {
					ASSERT(q>p&&q<alloct);
					p->ptr = q->ptr;
				}
				if(q>=p+nw && p+nw>=p)
					goto found;
			}
			q = p;
			p = clearbusy(p->ptr);
			if(p>q)
				ASSERT(p<=alloct);
			else if(q!=alloct || p!=allocs) {
				write(2,"corrupt arena\n",14);
				exit(0175);
			} else if(++temp>1)
				break;
		}
		temp = (nw+BLOCK/WORD)&~(BLOCK/WORD-1);
		q = sbrk(0);
		if(q+temp < q)
			return(NULL);
		q = sbrk(temp*WORD);
		if((int)q == -1)
			return(NULL);
		ASSERT(q>alloct);
		alloct->ptr = q;
		if(q!=alloct+1)
			alloct->ptr = setbusy(alloct->ptr);
		alloct = q->ptr = q+temp-1;
		alloct->ptr = setbusy(allocs);
	}
found:
	allocp = p + nw;
	ASSERT(allocp<=alloct);
	if(q>allocp) {
		allocx = allocp->ptr;
		allocp->ptr = p->ptr;
	}
	p->ptr = setbusy(allocp);
	return(p+1);
}
/*	freeing strategy tuned for LIFO allocation
*/
free(p)
register struct store *p;
{
	ASSERT(p>clearbusy(allocs[1].ptr)&&p<=alloct);
	allocp = --p;
	ASSERT(testbusy(p->ptr));
	p->ptr = clearbusy(p->ptr);
	ASSERT(p->ptr > allocp && p->ptr <= alloct);
}

struct { unsigned tag:4, vtype:4;};

prbusy()
{
	register struct store *p, *q;

	ASSERT(allocp>allocs && allocp<=alloct);
	for(p=allocs; ; ) {
			if(testbusy(p->ptr))
				{
				printf("busy 0%o, tag %d, type %d, length %d\n",
					p, p[1].tag, p[1].vtype,
					clearbusy(p->ptr) - (int) p - 2 );
				}
			q = p;
			p = clearbusy(p->ptr);
			if(p>q)
				ASSERT(p<=alloct);
			else if(q!=alloct || p!=allocs)
				{
				write(2,"corrupt arena\n",14);
				exit(0175);
				}
			else return;
	}
}
