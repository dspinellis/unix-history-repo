/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * slssort() sorts list slslist according to comparison function compar().
 * compar() is to be called with two arguments and must return an integer
 * greater than, equal to, or less than 0, depending on the lexicographic
 * relationship between the two arguments. Returns integer YES if
 * successful, otherwise NO if out of memory.
 */
#include "null.h"
#include "slslist.h"
#include "yesno.h"

static int (*sscmp)();			/* string compare function */

slssort(compar, slslist)
	int (*compar)();		/* compare two strings */
	SLSLIST *slslist;		/* pointer to list head block */
{
	char *malloc();			/* memory allocator */
	int bpi;			/* block pointer array index */
	int comparb();			/* compare 2 list blocks */
	SLSBLK **bp;			/* pointer to block pointer array */
	SLSBLK *curblk;			/* current list block */

	if (slslist->nk <= 0)
		return(YES);
	else if ((bp = (SLSBLK **) malloc((unsigned)slslist->nk*sizeof(SLSBLK *)))==NULL)
		{
		warn("out of memory");
		return(NO);
		}
	for (bpi=0, curblk=slslist->head; curblk != NULL; bpi++, curblk=curblk->next)
		bp[bpi] = curblk;

	sscmp = compar;
	qsort((char *) bp, slslist->nk, sizeof(SLSBLK *), comparb);

	for (bpi=0, curblk=slslist->head=bp[bpi++]; bpi < slslist->nk; bpi++)
		curblk = curblk->next = bp[bpi];
	curblk->next = NULL;
	slslist->tail = curblk;
	
	free((char *) bp);
	return(YES);
}



/*
 * comparb() compares key strings in 2 list blocks. Returns whatever
 * sscmp() returns. sscmp() is a string compare function.
 */

static int
comparb(b1, b2)
	SLSBLK **b1;			/* block pointer */
	SLSBLK **b2;			/* block pointer */
{
	return(sscmp((*b1)->key, (*b2)->key));
}
