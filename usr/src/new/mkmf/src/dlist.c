/* $Header: dlist.c,v 1.1 85/04/23 13:56:29 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "Mkmf.h"
#include "dlist.h"
#include "hash.h"
#include "null.h"
#include "slist.h"
#include "yesno.h"

static HASHBLK *printtag = NULL;	/* include files already printed */
static int COLUMN;			/* last column printed */

/*
 * dlappend() adds a dependency chain block to the end of a list of
 * dependency chain blocks. Each dependency chain block consists of a pointer
 * to a source file name block contained in a singly-linked list, and a pointer
 * to the head of the dependent list of included files. Returns a pointer to
 * the dependency chain block, or a null pointer if out of memory.
 */
DLBLK *
dlappend(srctyp, srcblk, incblk, dlist)
	int srctyp;			/* source file type */
	SLBLK *srcblk;			/* pointer to the source file block */
	INCBLK *incblk;			/* included file dependency chain */
	DLIST *dlist;			/* pointer to list head block */
{
	char *malloc();			/* memory allocator */
	DLBLK *dblk;			/* pointer to dependency list block */

	if (dlist == NULL)
		return(NULL);
	if ((dblk = (DLBLK *) malloc(sizeof(DLBLK))) == NULL)
		{
		warn("out of memory");
		return(NULL);
		}
	dblk->d_src = srcblk;
	dblk->d_type = srctyp;
	dblk->d_incl = incblk;
	dblk->d_next = NULL;
	if (dlist->d_tail == NULL)
		{
		dlist->d_head = dlist->d_tail = dblk;
		}
	else	{
		dlist->d_tail = dlist->d_tail->d_next = dblk;
		}
	return(dblk);
}



/*
 * dlinit() returns a pointer to the head block of a dependency list, or
 * null pointer if out of memory.
 */
DLIST *
dlinit()
{
	char *malloc();			/* memory allocator */
	DLIST *dlist;			/* pointer to list head block */

	if ((dlist = (DLIST *) malloc(sizeof(DLIST))) == NULL)
		{
		warn("out of memory");
		return(NULL);
		}
	dlist->d_head = dlist->d_tail = NULL;
	return(dlist);
}



/*
 * dlprint() appends the object-include file dependencies to the end of
 * a makefile. Transitive closure is checked by making suring that an
 * object-include file dependency is not generated if the source file is
 * included in another file.
 */
void
dlprint(dlist, ofp)
	DLIST *dlist;			/* dependency list */
	FILE *ofp;			/* output stream */
{
	DLBLK *dblk;			/* pointer to dependency list block */
	HASHBLK *lookupinclude();	/* look up include name in hash table */
	INCBLK *iblk;			/* cur. include file hash table blk */
	void putinchain();		/* output nested subinclude filenames */
	void putobjd();			/* output object file name */
	void rmprinttag();		/* remove "already printed" tags */

	fprintf(ofp, "%s\n", DEPENDMARK);
	for (dblk=dlist->d_head; dblk != NULL; dblk=dblk->d_next)
		{
		if (lookupinclude(dblk->d_src->key, dblk->d_type) == NULL)
			{
			putobjd(dblk->d_src, ofp);
			for (iblk=dblk->d_incl; iblk != NULL; iblk=iblk->i_next)
				{
				putinchain(iblk->i_hblk, ofp);
				}
			fprintf(ofp, "\n");
			rmprinttag();
			}
		}
}



/*
 * putinchain() outputs a chain of nested include file names. It changes
 * the sign of each chain block h_val field as it traverses the chain to
 * detect looping.
 */
static void
putinchain(htb, ofp)
	HASHBLK *htb;			/* hash table blk including chain */
	FILE *ofp;			/* output stream */
{
	INCBLK *iblk;			/* cur. include file hash table blk */
	void putinclude();		/* output include file pathname */
	void putinchain();		/* output nested subinclude file names*/

	putinclude(htb, ofp);
	htb->h_val = -htb->h_val;
	for (iblk=htb->h_sub; iblk != NULL; iblk=iblk->i_next)
		{
		if (iblk->i_hblk->h_val < 0)
			{
			if (iblk->i_loop == NO)
				{
				warn2("recursive include nesting of \"%s\" in \"%s\"",
				      iblk->i_hblk->h_def, htb->h_def);
				iblk->i_loop = YES;
				}
			continue;
			}
		putinchain(iblk->i_hblk, ofp);
		}
	htb->h_val = -htb->h_val;
}



#define MAXLINE		80
#define	TABSIZE		8

/*
 * putinclude() writes an include file pathname to stream ofp if
 * if it has not already been written on the current dependency line.
 * and adds the hash block containing the file pathname to the
 * "already printed" printtag list. The last block on the list
 * points back onto itself rather than at NULL so that the non-NULL
 * tag will indicate that the filename has already been seen.
 */
static void
putinclude(htb, ofp)
	HASHBLK *htb;			/* include file hash block */
	FILE *ofp;			/* output stream */
{
	if (htb->h_tag == NULL)
		{
		COLUMN += htb->h_val + 1;
		if (COLUMN >= (MAXLINE - 2))
			{
			fprintf(ofp, " \\\n\t%s", htb->h_def);
			COLUMN = htb->h_val + TABSIZE;
			}
		else	{
			fprintf(ofp, " %s", htb->h_def);
			}
		/* add to "already printed" filenames */
		htb->h_tag = (printtag == NULL) ? htb :printtag;
		printtag = htb;
		}
}



/*
 * putobjd() writes an object file dependency name.
 */
static void
putobjd(srcblk, ofp)
	SLBLK *srcblk;			/* source file name list block */
	FILE *ofp;			/* output stream */
{
	extern char OBJSFX[];		/* object file name suffix */
	int strlen();			/* string length */
	void putobj();			/* output object file name */

	COLUMN = strlen(srcblk->key) + strlen(OBJSFX) + 1;
	putobj(srcblk->key, ofp);
	fprintf(ofp, ":");
}



/*
 * rmprinttag() removes the chain of tags indicating that an include
 * file dependency has already been printed for the current source file.
 */
static void
rmprinttag()
{
	register HASHBLK *curhtb;	/* current hash table block */
	register HASHBLK *nxthtb;	/* next hash table block */

	for (curhtb = printtag; curhtb != NULL; curhtb = nxthtb)
		{
		nxthtb = curhtb->h_tag;
		curhtb->h_tag = NULL;
		}
	printtag = NULL;
}
