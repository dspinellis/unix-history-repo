/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include "null.h"
#include "pdlist.h"
#include "yesno.h"

/*
 * pdinit() returns a pointer to the head block of a new project directory
 * list, or dies if out of memory.
 */
PDLIST *
pdinit()
{
	char *malloc();			/* memory allocator */
	PDLIST *pdlist;			/* pointer to list head block */

	if ((pdlist = (PDLIST *) malloc(sizeof(PDLIST))) == NULL)
		nomorecore();
	pdlist->nd = 0;
	pdlist->head = NULL;
	return(pdlist);
}



/*
 * pdprepend() saves null-terminated project directory pathnames
 * somewhere and inserts a pointer to the directory at the head of list
 * pdlist. Returns a pointer to the pathname, or dies if out of
 * memory.
 */
char *
pdprepend(ppathname, pathname, project, pdlist)
	char *ppathname;		/* project directory project pathname */
	char *pathname;			/* project directory regular pathname */
	char *project;			/* project directory's project */
	PDLIST *pdlist;			/* pointer to list head block */
{
	char *malloc();			/* memory allocator */
	char *strcpy();			/* string copy */
	int strlen();			/* string length */
	PDBLK *pdbptr;			/* pointer to list block */

	if (pdlist == NULL)
		return(NULL);
	if ((pdbptr = (PDBLK *) malloc(sizeof(PDBLK))) == NULL ||
	    (pdbptr->ppath = malloc((unsigned)(strlen(ppathname)+1))) == NULL ||
	    (pdbptr->rpath = malloc((unsigned)(strlen(pathname)+1))) == NULL)
		nomorecore();
	strcpy(pdbptr->rpath, pathname);
	strcpy(pdbptr->ppath, ppathname);
	pdbptr->project = project;
	pdbptr->next = pdlist->head;
	pdlist->head = pdbptr;
	pdlist->nd++;
	return(pdbptr->ppath);
}



/*
 * pdrm() removes a project directory list.
 */
void
pdrm(pdlist)
	PDLIST *pdlist;			/* pointer to list head block */
{
	PDBLK *nxtblk;			/* next list block */

	while (pdlist->head != NULL)
		{
		nxtblk = pdlist->head->next;
		free(pdlist->head->ppath);
		free(pdlist->head->rpath);
		free((char *) pdlist->head);
		pdlist->head = nxtblk;
		}
	free((char *) pdlist);
}



/*
 * pdsort() sorts list pdlist according to comparison function compar().
 * compar() is to be called with two arguments and must return an integer
 * greater than, equal to, or less than 0, depending on the lexicographic
 * relationship between the two arguments.
 */

static int (*sscmp)();			/* string compare function */

void
pdsort(compar, pdlist)
	int (*compar)();		/* compare two strings */
	PDLIST *pdlist;			/* pointer to list head block */
{
	char *malloc();			/* memory allocator */
	int bpi;			/* block pointer array index */
	int comparb();			/* compare 2 list blocks */
	PDBLK **bp;			/* pointer to block pointer array */
	PDBLK *curblk;			/* current list block */

	if (pdlist->nd <= 0)
		return;
	else if ((bp = (PDBLK **) malloc((unsigned)pdlist->nd*sizeof(PDBLK *)))==NULL)
		nomorecore();
	for (bpi=0, curblk=pdlist->head; curblk != NULL; bpi++, curblk=curblk->next)
		bp[bpi] = curblk;

	sscmp = compar;
	qsort((char *) bp, pdlist->nd, sizeof(PDBLK *), comparb);

	for (bpi=0, curblk=pdlist->head=bp[bpi++]; bpi < pdlist->nd; bpi++)
		curblk = curblk->next = bp[bpi];
	curblk->next = NULL;
	
	free((char *) bp);
}



/*
 * comparb() compares project directory pathnames in 2 list blocks.
 * Returns whatever sscmp() returns. sscmp() is a string compare function.
 */
static int
comparb(b1, b2)
	PDBLK **b1;			/* block pointer */
	PDBLK **b2;			/* block pointer */
{
	return(sscmp((*b1)->ppath, (*b2)->ppath));
}
