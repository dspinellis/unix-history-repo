#include "defs"

#define NHISTO 50
int histo[NHISTO];

int mem[MEMSIZE];
unsigned int nmemused	= 0;
unsigned int nmemavail	= 0;
long int totalloc	= 0;
long int totfreed	= 0;

int nexpblocks	= 0;
ptr expblocks	= 0;
int nexcblocks	= 0;
ptr excblocks	= 0;
ptr chains	= 0;

ptr alloc(), calloc(), malloc();

ptr intalloc(n)
int n;
{
int *p;

/*debug*/ if(n>sizeof(struct genblock)) fatal1("intalloc(%d)", n);
if( (p = calloc(1,n)) == NULL)
	{
	if(memdump)
		prmem();
	fatal1("Line %d:  Cannot allocate memory", yylineno);
	}

return(p);
}




ptr calloc(m,n)
int m, n;
{
return(alloc(m*n));
}



ptr malloc(m)
int m;
{
return(alloc(m));
}



/* Very stupid memory allocator.  Stores a count word before
   each block; negative if idle, positive if busy.
   Looks for a block big enough for current request, and splits it
   if necessary.  Does not coalesce, always starts at bottom of memory.
   Checks validity of all count words it encounters.
*/


ptr alloc(k)
register int k;
{
int *p;
register int i, j;

k = (k + sizeof(int)-1) / sizeof(int);
if(k <=0) fprintf(diagfile, "alloc(%d words)\n", k);
else if(k >= NHISTO) ++histo[0];
else ++histo[k];
totalloc += k;
if(k > 256) fprintf(diagfile, "calloc(%d words)\n", k);

/* look for a large enough slot */
if(nmemavail > k)
    for(i=0 ; i<nmemused ; )
	{
	j = mem[i];
	if(j>256)
		{
		fprintf(diagfile, "Bad count word %d\n", j);
		goto die;
		}
	if(j>=0 ||  (j = -j)<k)
		i += (j+1);
	else	{
		if(j > 256)
			{
			fprintf(diagfile, "Bad count word %d\n", j);
			goto die;
			}
		mem[i] = k;
		if(j > k)
			mem[i+k+1] = -(j-k-1);
		for(j = i+k ; j>i ; --j)
			mem[j] = 0;
		nmemavail -= (k+1);
		return(mem + i+1);
		}
	}

/* otherwise try to advance the fence */
mem[nmemused] = k;
p = mem + nmemused + 1;
nmemused += (k+1);
if(nmemused >= MEMSIZE)
	{
	die:
/*debug*/	fprintf(diagfile, "Highwater mark %d words. ", nmemused);
/*debug*/	fprintf(diagfile, "%ld words left over\n", totalloc-totfreed);
/*	prmem();	*/
	fatal1("Line %d:  out of memory", yylineno);
	}
return(p);
}



cfree(p)
ptr p;
{
if(p==0)
	fatal("cfree(0)");
free(p);
}




free(p)
register unsigned int *p;
{
if(p<=mem || p>mem+nmemused)
	{
	fprintf(diagfile, "attempt to free an unallocated block,  ");
	goto bad;
	}
if(p[-1]>256 || p[-1]<0)
	{
	fprintf(diagfile, "attempted to free a block of length %u\n",p[-1]);
  bad:	fprintf(diagfile, "location %o    ", p);
	fprintf(diagfile, "mem=%o   lastused=%o\n", mem, mem+nmemused);
/*	if(p[-1]>256 || p[-1]<0)	*/
		fatal("");
	}
totfreed += p[-1];
nmemavail += p[-1]+1;
p[-1] = - p[-1];
;
}


prhisto()
{
int i;
fprintf(diagfile, "allocation histogram:\n%4d big blocks\n",histo[0]);
for(i=1;i<NHISTO;++i)
	if(histo[i]>0) fprintf(diagfile, "%4d %2d-word blocks\n", histo[i],i);
}





ptr allexpblock()
{
ptr p;

if(expblocks)
	{
	p = expblocks;
	expblocks = expblocks->leftp;
	zeroout(p, sizeof(struct exprblock));
	--nexpblocks;
	return(p);
	}
else	return( ALLOC(exprblock) );
}




frexpblock(p)
register ptr p;
{
if ( p[-1] != sizeof(struct exprblock)/sizeof(int) )
	badtag("frexpblock", p->tag);
if(nexpblocks < EXPRPOOL)
	{
	p->leftp = expblocks;
	p->tag = 0;
	expblocks = p;
	++nexpblocks;
	}
else	cfree(p);
}




ptr allexcblock()
{
ptr p;

if(excblocks)
	{
	p = excblocks;
	excblocks = excblocks->leftp;
	zeroout(p, sizeof(struct execblock));
	--nexcblocks;
	return(p);
	}
else	return( ALLOC(execblock) );
}




frexcblock(p)
register ptr p;
{
if( p[-1] != sizeof(struct execblock)/sizeof(int) )
	fatal1("invalid frexcblock block of size %d", p[-1]);
if(nexcblocks < EXECPOOL)
	{
	p->leftp = excblocks;
	p->tag = 0;
	excblocks = p;
	++nexcblocks;
	}
else	cfree(p);
}



zeroout(p,n)
register int *p;
int n;
{
register int *pn;

pn = p + (n + sizeof(int)-1)/sizeof(int);

while(p < pn)
	*p++ = 0;
}




frchain(p0)
register chainp *p0;
{
register ptr p;

if(p0==0 || *p0==0) return;

for(p = *p0 ; p->nextp ; p = p->nextp)
	p->datap = 0;

p->datap = 0;
p->nextp = chains;
chains = *p0;
*p0 = 0;
}


chainp mkchain(p,q)
ptr p, q;
{
register chainp r;

if(chains)
	{
	r = chains;
	chains = chains->nextp;
	}
else	r = ALLOC(chain);
r->datap = p;
r->nextp = q;
return(r);
}




prmem()
{
register int i,j;

fprintf(diagfile, "Memory dump:\n");

for(i=0 ; i<nmemused ; )
	{
	j = mem[i];
	fprintf(diagfile, "Loc %6o = Word %5d   ", mem+i, i);
	if(j<0)
		fprintf(diagfile, "Idle block length %4d   ", j = -j);
	else	fprintf(diagfile, "Busy block length %4d   ", j);
	fprintf(diagfile, "tag %3d", mem[i+1].tag);
	if(mem[i+1].tag==TNAME && mem[i+1].sthead!=0)
		fprintf(diagfile, "   varname %s", mem[i+1].sthead->namep);
	else if(j==2)
		fprintf(diagfile, "  chain %o %o", mem[i+1], mem[i+2]);
	else if (mem[i+1].tag > TIOSTAT)
		{
		char *s, *sn;
		s = & mem[i+1];
		sn = s + 12;
		fprintf(diagfile, "  \"");
		while(*s!= '\0' && s<sn)
			putc(*s++, diagfile);
		}
	fprintf(diagfile, "\n");

	i += j+1;
	}
}
