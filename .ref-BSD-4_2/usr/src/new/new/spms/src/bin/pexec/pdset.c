/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "hash.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "pdset.h"
#include "pdlist.h"
#include "pdtyp.h"
#include "pld.h"
#include "slist.h"
#include "slslist.h"
#include "spms.h"
#include "truefalse.h"
#include "yesno.h"

#define INCRPDIRS	 50		/* amount to increase pdir ptr array */
#define MAXPDIRS 	100		/* initial size of proj dir ptr array */

extern char *PGN;			/* program name */
extern PDTYP PDIRTYP;			/* project directory type labels list */

static int Ipdirs;			/* attribute block array index */
static int Maxpdirs = MAXPDIRS;		/* maximum no. of project dirs */
static int Ntypes;			/* number of unique type labels */
static PDSET **Pdarray;			/* project dir attribute block array */
static short *Maptyp;			/* unique type label mapping array */
static TYPES *Typstat;			/* type label statistics */

/*
 * add_pdset() adds a project directory to the set of project directories
 * which satisfy a boolean project directory type label expression. Only
 * those type labels which satisfy the boolean expression are included with
 * the project directory. To determine which type labels qualify, the
 * postfix expression is scanned from right to left. Type labels within a
 * negated expression are ignored. For example, in the postfix equivalent
 * of expression "src & !(cmd | lib)" (that is: src cmd lib | ! &),
 * even if both "src" and "cmd" are found, "src" is the only type label
 * which could qualify.
 */
void
add_pdset(postfix, ppathname, pathname, project)
	register PDTYP *postfix;	/* postfix expression struct */
	char *ppathname;		/* project directory project pathname */
	char *pathname;			/* project directory pathname */
	char *project;			/* project directory's project */
{
	register int i;			/* postfix expression index */
	register int opcount;		/* count of expected operands */
	char *pdtcpy();			/* copy project directory type label */
	char *realloc();		/* reallocate memory block */
	char type[TYPESIZE];		/* project dir type label buffer */
	PDSET *savepdir();		/* save pdir attribute blk somewhere */
	void savetype();		/* save type label */

	if (Ipdirs >= Maxpdirs)
		{
		Maxpdirs += INCRPDIRS;
		if ((Pdarray = (PDSET **) realloc((char *)Pdarray,
			       (unsigned)Maxpdirs*sizeof(PDSET *))) == NULL)
			nomorecore();
		}
	Pdarray[Ipdirs] = savepdir(ppathname, pathname, project);

	opcount = 0;
	for (i = (postfix->pfxsize)-1; i >= 0;)
		{
		switch ((postfix->pfx)[i].p_class)
			{
			case B_ID:
				if ((postfix->pfx)[i].p_sw == TRUE)
					{
					pdtcpy(type, (postfix->pfx)[i].p_label);
					savetype(type, i);
					}
				break;
			case B_OR:
			case B_AND:
				if ((postfix->pfx)[i].p_sw == FALSE)
					opcount += 2;
				break;
			case B_NOT:
				/* always skip !subexpr */
				opcount += 1;
				break;
			}
		/* skip false subexpression */
		for (--i; opcount > 0; i--)
			switch ((postfix->pfx)[i].p_class)
				{
				case B_ID:
					opcount -= 1;
					break;
				case B_OR:
				case B_AND:
					opcount += 1;
					break;
				case B_NOT:
					break;
				}
		}
	Ipdirs++;
}



/*
 * build_pdset() builds a set of project directories which satisfy a boolean
 * project directory type label expression.
 */
build_pdset(ppathname, pathname)
	char *ppathname;		/* project root dir project pathname */
	char *pathname;			/* regular project root dir pathname */
{
	extern int ERRSTATUS;		/* pexec error status */
	extern SLIST *ENVLIST;		/* project environment variable list */
	char ppathbuf[PPATHSIZE];	/* project pathname buffer */
	char *ppathcat();		/* project pathname concatenation */
	char *pv;			/* PROJECT environment variable */
	char *slprepend();		/* prepend key */
	char *slsprepend();		/* prepend key+string */
	int closepdb();			/* close database */
	int errpdb();			/* print database error message */
	int pdtmatch();			/* match project dir type label expr */
	int status = 0;			/* return status */
	PATH *pd;			/* pathname struct pointer */
	PATH *readpld();		/* read project link directory entry */
	PDB *openpdb();			/* open database */
	PDB *pldp;			/* project link directory stream */
	SLSBLK *pblk;			/* project list block */
	SLSLIST *plist;			/* project list */
	SLSLIST *slsinit();		/* initialize list */
	void add_pdset();		/* add to set of project dirs */
	void slsrm();			/* remove list item */

	if ((pv = slprepend(pathname, ENVLIST)) == NULL)
		pxexit();
	plist = slsinit();

	/* read PLDNAME project link directory */
	if ((pldp = openpdb(PLDNAME, pathname, "r")) == NULL)
		return(errpdb((PDB *) NULL));
	while ((pd = readpld(pldp)) != NULL)
		{
		if (EQUAL(pd->p_alias, PARENTPROJECT))
			continue;

		if (EQUAL(pd->p_alias, CURPROJECT))
			{
			if (pdtmatch(&PDIRTYP, pd->p_type) == YES)
				add_pdset(&PDIRTYP, ppathname, pd->p_path, pv);
			}
		else if (pd->p_mode == P_IFPROOT)
			{
			if (slsprepend(pd->p_alias, pd->p_path, plist) == NULL)
				pxexit();
			}
		else if (pdtmatch(&PDIRTYP, pd->p_type) == YES)
			{
			ppathcat(ppathbuf, ppathname, pd->p_alias);
			add_pdset(&PDIRTYP, ppathbuf, pd->p_path, pv);
			}
		}
	if (closepdb(pldp) != 0)
		status = ERRSTATUS;

	/* build project directory type label tree for subprojects */
	for (pblk = plist->head; pblk != NULL; pblk = pblk->next)
		{
		ppathcat(ppathbuf, ppathname, pblk->key);
		status |= build_pdset(ppathbuf, pblk->string);
		}
	slsrm(CNULL, plist);

	return(status);
}



/*
 * check_pdset() detects conflicting type label priorities by
 * checking that project directories are sorted into ascending
 * order according to priority. An error message is printed and
 * 1 returned on conflict, otherwise zero.
 */
check_pdset()
{
	register int iPd;		/* proj dir block array index */
	register int ityp;		/* type label block index */
	register int lastpr;		/* previous type label priority */
	register int prior;		/* project dir type label priority */
	register PDSET **ptrPd;		/* Pdarray block array pointer */
	int nsortyp;			/* no. type label categories to sort */

	nsortyp = 0;
	for (ityp = 0; ityp < Ntypes; ityp++)
		if (Typstat[ityp].t_sort)
			nsortyp++;
	if (nsortyp < 2)
		return(0);

	for (ityp = 0; ityp < Ntypes; ityp++)
		if (Typstat[ityp].t_sort)
			{
			for (ptrPd=Pdarray, iPd=Ipdirs; iPd > 0; ptrPd++, iPd--)
				if ((*ptrPd)->typblk[ityp].t_exist)
					{
					lastpr = (*ptrPd)->typblk[ityp].t_prior;
					break;
					}
			for (ptrPd++, iPd--; iPd > 0; ptrPd++, iPd--)
				{
				if ((*ptrPd)->typblk[ityp].t_exist)
					{
					prior = (*ptrPd)->typblk[ityp].t_prior;
					if (prior < lastpr)
						goto conflict;
					lastpr = prior;
					}
				}
			}
	return(0);
conflict:
	fprintf(stderr, "%s:", PGN);
	for (ityp = Ntypes-1; ityp >= 0; ityp--)
		if (Typstat[ityp].t_sort)
			fprintf(stderr, (nsortyp-- > 1) ? " %s," :
				" %s: conflicting type label priorities\n",
				Typstat[ityp].t_name);
	return(1);
}



/*
 * debug_pdset() prints the sorted project directories together with
 * the type labels that satisfy the boolean expression.
 */
void
debug_pdset()
{	
	int iPd;			/* project dir block array index */
	int ityp;			/* type label statistics array index */

	for (iPd = 0; iPd < Ipdirs; iPd++)
		{
		fprintf(stderr, "%s:", Pdarray[iPd]->ppath);
		for (ityp = 0; ityp < Ntypes; ityp++)
			if (Pdarray[iPd]->typblk[ityp].t_exist)
				fprintf(stderr," %s.%d",Typstat[ityp].t_name,
					Pdarray[iPd]->typblk[ityp].t_prior);
		putc('\n', stderr);
		}
}



/*
 * exec_pdset() executes a set of project directories. Returns non-zero
 * error status if error.
 */
exec_pdset()
{
	extern int ERRSTATUS;		/* pexec error status */
	extern int EXECUTE;		/* execute command? */
	extern int PRINT_HEADING;	/* print headings for project dirs */
	int ch_dir();			/* change current working directory */
	int execcmd();			/* execute command in directory */
	int iPd;			/* project dir block array index */
	int status = 0;			/* return status */
	void print_title();		/* print project directory title */

	for (iPd = 0; iPd < Ipdirs; iPd++)
		{
		if (PRINT_HEADING == YES)
			print_title(Pdarray[iPd]->ppath);
		if (ch_dir(Pdarray[iPd]->rpath) == NO)
			status = ERRSTATUS;
		else if (EXECUTE == YES)
			status |= execcmd(Pdarray[iPd]->project);
		}
	return(status);
}



/*
 * init_pdset() allocates an array of pointers (Pdarray) to project
 * directory attribute blocks, and calculates the maximum number of type
 * labels to be stored with each project directory based on the number of
 * unique type labels in the boolean postfix type expression. An array
 * (Typstat) is also created to maintain statistics on each brand of
 * label.
 *
 * Hash table lookup is used in forming unique type labels and a
 * mapping array is used to map the labels from the boolean postfix
 * type label expression to the unique representation.
 */
#define UNIQTYPHASHSIZE		41

void
init_pdset()
{
	register int i;			/* postfix type expression index */
	char *malloc();			/* memory allocator */
	char *pfxcpy();			/* copy string prefix */
	char type[TYPESIZE];		/* project dir type label buffer */
	HASH *htinit();			/* initialize hash table */
	HASH *uniqtyp;			/* hash table of unique type labels */
	HASHBLK *htb;			/* hash table block pointer */
	HASHBLK *htinstall();		/* install hash table entry */
	HASHBLK *htlookup();		/* find hash table entry */
	int nid;			/* no. of ids in boolean expression */

	/* project directory attribute block pointer array */
	if ((Pdarray = (PDSET **) malloc((unsigned)Maxpdirs*sizeof(PDSET *))) == NULL)
		nomorecore();

	/* create postfix expression -> unique type label mapping array */
	if ((Maptyp = (short *) malloc((unsigned)PDIRTYP.pfxsize*sizeof(short))) == NULL)
		nomorecore();

	/* create type label statistics array (estimate size first) */
	nid = 0;
	for (i = (PDIRTYP.pfxsize)-1; i >= 0; i--)
		if ((PDIRTYP.pfx)[i].p_class == B_ID)
			nid++;
	if ((Typstat = (TYPES *) malloc((unsigned)nid*sizeof(TYPES))) == NULL)
		nomorecore();

	/* unique type label determination */
	uniqtyp = htinit(UNIQTYPHASHSIZE);
	for (i = (PDIRTYP.pfxsize)-1; i >= 0; i--)
		{
		if ((PDIRTYP.pfx)[i].p_class != B_ID)
			continue;
		pfxcpy(type, (PDIRTYP.pfx)[i].p_id);
		if ((htb = htlookup(type, uniqtyp)) != NULL)
			{
			Maptyp[i] = htb->h_val;
			}
		else	{
			if ((htb = htinstall(type,"",Ntypes,uniqtyp)) == NULL)
				nomorecore();
			Maptyp[i] = Ntypes;
			Typstat[Ntypes].t_name = htb->h_key;
			Typstat[Ntypes].t_ntl = Typstat[Ntypes].t_sort = 0;
			Ntypes++;
			}
		}
}



/*
 * pdbcmp() compares the type label priorities and project pathnames
 * for two project directories. Type label priorities override the
 * lexicographical relationship of the project pathnames. Conflicting
 * priorities are not detected. For example, a conflict occurs if the
 * first directory has type labels print.1 and update.2, whereas the
 * second directory has type labels print.2 and update.1. Returns
 * an integer less than, equal to, or greater than zero, depending on
 * the relative priorities of the type labels or lexicographical ordering
 * of the project pathnames.
 */
pdbcmp(b1, b2)
	PDSET **b1;			/* project directory block pointer */
	PDSET **b2;			/* project directory block pointer */
{
	register TYPBLK *t1;		/* type label block pointer */
	register TYPBLK *t2;		/* type label block pointer */
	register TYPES *ty;		/* type statistics array pointer */
	register int comp;		/* block comparison */
	register int ityp;		/* type label block index */
	register int ntypes;		/* number of unique type labels */
	int strcmp();			/* string comparison */

	comp = 0;
	ntypes = Ntypes;
	t1 = (*b1)->typblk;
	t2 = (*b2)->typblk;
	ty = Typstat;

	for (ityp = 0; ityp < ntypes; ityp++)
		{
		if (ty->t_sort && t1->t_exist && t2->t_exist)
			if ((comp = t1->t_prior - t2->t_prior) != 0)
				return(comp);
		t1++, t2++, ty++;
		}
	return(strcmp((*b1)->ppath, (*b2)->ppath));
}



/*
 * savepdir() saves a block of project directory attributes somewhere
 * and returns a pointer to the somewhere, or dies if out of memory.
 */
PDSET *
savepdir(ppathname, pathname, project)
	char *ppathname;		/* project directory project pathname */
	char *pathname;			/* project directory regular pathname */
	char *project;			/* project directory's project */
{
	char *calloc();			/* initialize memory to zero */
	char *malloc();			/* memory allocator */
	char *strcpy();			/* string copy */
	int strlen();			/* string length */
	PDSET *pdbptr;			/* pointer to proj directory block */

	if ((pdbptr = (PDSET *) malloc(sizeof(PDSET))) == NULL ||
	    (pdbptr->ppath = malloc((unsigned)(strlen(ppathname)+1))) == NULL ||
	    (pdbptr->rpath = malloc((unsigned)(strlen(pathname)+1))) == NULL ||
	    (pdbptr->typblk = (TYPBLK *) calloc((unsigned)Ntypes,sizeof(TYPBLK))) == NULL)
		nomorecore();
	strcpy(pdbptr->rpath, pathname);
	strcpy(pdbptr->ppath, ppathname);
	pdbptr->project = project;
	return(pdbptr);
}



/*
 * savetype() records the priorities of the type labels attached to each
 * directory, and also the total number of each type label.
 */
void
savetype(type, idx)
	char *type;			/* project dir type label */
	int idx;			/* boolean type expression id index */
{
	register char *ptyptr;		/* pointer to type label priority */
	register int priority;		/* type label priority */
	register int uniqid;		/* unique type label number */
	char *index();			/* first occurrence of character */
	int atoi();			/* string to decimal integer */

	uniqid = Maptyp[idx];
	ptyptr = index(type, '.');
	priority = (ptyptr == NULL) ? 0 : atoi(++ptyptr);
	if (Typstat[uniqid].t_ntl == 0)
		{
		Typstat[uniqid].t_itlp = priority;
		}
	else	{
		if (priority != Typstat[uniqid].t_itlp)
			Typstat[uniqid].t_sort = 1;
		}
	Typstat[uniqid].t_ntl++;

	Pdarray[Ipdirs]->typblk[uniqid].t_exist++;
	Pdarray[Ipdirs]->typblk[uniqid].t_prior = priority;
}



/*
 * sort_pdset() sorts the set of project directories alpahabetically
 * and by type label priorities.
 */
void
sort_pdset()
{
	int pdbcmp();			/* compare project dir blocks */

	qsort((char *)Pdarray, Ipdirs, sizeof(PDSET *), pdbcmp);
}
