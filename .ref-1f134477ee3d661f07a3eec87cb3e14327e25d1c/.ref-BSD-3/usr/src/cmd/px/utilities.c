#include	"stdio.h"
#include	"h00vars.h"
#include	"h01errs.h"
#include	"h02opcs.h"

/*
 * allocate a block of storage on the heap
 */
char	*palloc(need)

long	need;

{
extern	 char *malloc();
register char *memblk, *ptr;

memblk = malloc(need);
if (memblk == 0)
	error(EOUTOFMEM);
if (memblk == (char *)(-1))
	error(ETRASHHEAP);
for(ptr=memblk; ptr<memblk+need; ptr++)
	*ptr = 0;
return(memblk);
}



/*
 * Free a block of storage on the stack
 */
pfree(ptr)

char	*ptr;

{
extern	long free();

if (ptr == 0)
	error(ENILPTR);
else if (free(ptr) == -1)
	error(ETRASHHEAP);
}

/*
 * Constant set constructor (yechh!)
 */

#define	sets	int short

sets	*pcttot(uprbp, lwrb, n, av)

int	uprbp, lwrb, n;
sets	av;

{
register int l, h;
register sets *set, *ap;

	ap = &av;
	set = &ap[2 * n];
	while(--n >= 0) {
		if ((l = *ap++ - lwrb) < 0 || l > uprbp ||
		    (h = *ap++ - lwrb) < 0 || h > uprbp)
			error(ECTTOT);
		while (l <= h) {
			set[l >> 4] = set[l >> 4] | 1 << (l & 017);
			l++;
		}
	}
	return(set);
}

char	pd_date[] = {
	8, 9, 10, 4, 5, 6, 10, 22, 23, 10, 0
};

char *ctime();

pdattim(op, alfap)
register char *alfap;
{
	register char *ap, *cp, *dp;
	long a;
	int i;

	time(&a);
	cp = ctime(&a);
	ap = alfap;
	if (op == O_DATE)
		for (dp = pd_date; *dp; *ap++ = cp[*dp++]);
	else
		for (cp = cp + 10, i = 10; i; *ap++ = *cp++, i--);
}

psexit(code)

long	code;
{

pmflush();
if (mode == PIX && nodump == 0) {
	fputs("Execution terminated",stderr);
	if (code)
		fputs(" abnormally",stderr);
	fputc('.',stderr);
	fputc('\n',stderr);
	}
stats();
exit(code);
}
