/* Copyright (c) 1979 Regents of the University of California */

static char sccsid[] = "@(#)utilities.c 4.2 10/20/80";

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

long	_mask[] = {	
		    0xffffffff , 0xfffffffe , 0xfffffffc , 0xfffffff8 ,
		    0xfffffff0 , 0xffffffe0 , 0xffffffc0 , 0xffffff80 ,
		    0xffffff00 , 0xfffffe00 , 0xfffffc00 , 0xfffff800 ,
		    0xfffff000 , 0xffffe000 , 0xffffc000 , 0xffff8000 ,
		    0xffff0000 , 0xfffe0000 , 0xfffc0000 , 0xfff80000 ,
		    0xfff00000 , 0xffe00000 , 0xffc00000 , 0xff800000 ,
		    0xff000000 , 0xfe000000 , 0xfc000000 , 0xf8000000 ,
		    0xf0000000 , 0xe0000000 , 0xc0000000 , 0x80000000 ,
		    0x00000000
		 };
/*
 * Constant set constructor
 */

long *
cttot(result, lowerbnd, upperbnd, paircnt, singcnt, data)

	long	*result;	/* pointer to final set */
	int	lowerbnd;	/* lower bound of set */
	int	upperbnd;	/* upper - lower of set */
	int	paircnt;	/* number of pairs to construct */
	int	singcnt;	/* number of singles to construct */
	int	data;		/* paircnt plus singcnt sets of data */
{
	register int	lower;
	register int	lowerdiv;
	register int	lowermod;
	register int	upper;
	int		upperdiv;
	int		uppermod;
	register int	*dataptr;
	register long	*lp;
	long		*limit;
	long		temp;
	long		cnt;

	limit = &result[(upperbnd + 1 + BITSPERLONG - 1) / BITSPERLONG];
	for (lp = result; lp < limit; )
		*lp++ = 0;
	dataptr = &data;
	for (cnt = 0; cnt < paircnt; cnt++) {
		lower = *dataptr++ - lowerbnd;
		if (lower < 0 || lower > upperbnd) {
			error(ECTLWR);
			return;
		}
		upper = *dataptr++ - lowerbnd;
		if (upper < 0 || upper > upperbnd) {
			error(ECTUPR);
			return;
		}
		if (lower > upper) {
			continue;
		}
		lowerdiv = lower / BITSPERLONG;
		lowermod = lower % BITSPERLONG;
		upperdiv = upper / BITSPERLONG;
		uppermod = upper % BITSPERLONG;
		temp = _mask [lowermod];
		if ( lowerdiv == upperdiv ) {
			temp &= ~_mask[ uppermod + 1 ];
		}
		result[ lowerdiv ] |= temp;
		limit = &result[ upperdiv-1 ];
		for ( lp = &result[ lowerdiv+1 ] ; lp <= limit ; lp++ ) {
			*lp |= ~0;
		}
		if ( lowerdiv != upperdiv ) {
			result[ upperdiv ] |= ~_mask[ uppermod + 1 ];
		}
	}
	for (cnt = 0; cnt < singcnt; cnt++) {
		lower = *dataptr++ - lowerbnd;
		if (lower < 0 || lower > upperbnd) {
			error(ECTSNG);
			return;
		}
		lowerdiv = lower / BITSPERLONG;
		lowermod = lower % BITSPERLONG;
		result[ lowerdiv ] |= ( 1 << lowermod );
	}
	return(result);
}

inct(element, paircnt, singcnt, data)

	register int	element;	/* element to find */
	int		paircnt;	/* number of pairs to check */
	int		singcnt;	/* number of singles to check */
	int		data;		/* paircnt plus singcnt bounds */
{
	register int	*dataptr;
	register int	cnt;

	dataptr = &data;
	for (cnt = 0; cnt < paircnt; cnt++) {
		if (element < *dataptr++) {
			dataptr++;
			continue;
		}
		if (element <= *dataptr++) {
			return TRUE;
		}
	}
	for (cnt = 0; cnt < singcnt; cnt++) {
		if (element == *dataptr++) {
			return TRUE;
		}
	}
	return FALSE;
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
if (mode == PIX) {
	fputs("Execution terminated",stderr);
	if (code)
		fputs(" abnormally",stderr);
	fputc('.',stderr);
	fputc('\n',stderr);
	}
stats();
exit(code);
}
