/*
 * Copyright (c) 1985, 1991 Peter J. Nicklin.
 * Copyright (c) 1991 Version Technology.
 * All Rights Reserved.
 *
 * $License: VT.1.1 $
 * Redistribution and use in source and binary forms,  with or without
 * modification,  are permitted provided that the following conditions
 * are met:  (1) Redistributions of source code must retain the  above
 * copyright  notice,  this  list  of  conditions  and  the  following
 * disclaimer.  (2) Redistributions in binary form must reproduce  the
 * above  copyright notice,  this list of conditions and the following
 * disclaimer in the  documentation  and/or other  materials  provided
 * with  the  distribution.  (3) All advertising materials  mentioning
 * features or  use  of  this  software  must  display  the  following
 * acknowledgement:  ``This  product  includes  software  developed by
 * Version Technology.''  Neither the name of Version  Technology  nor
 * the  name  of  Peter J. Nicklin  may  be used to endorse or promote
 * products derived from this software without specific prior  written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY VERSION TECHNOLOGY ``AS IS''  AND  ANY
 * EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO, THE
 * IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL  VERSION  TECHNOLOGY  BE
 * LIABLE  FOR ANY DIRECT,  INDIRECT,  INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR  CONSEQUENTIAL DAMAGES   (INCLUDING,   BUT   NOT   LIMITED   TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF  LIABILITY,  WHETHER  IN  CONTRACT,  STRICT LIABILITY,  OR  TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING  IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE,  EVEN  IF  ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 * Report problems and direct questions to nicklin@netcom.com
 *
 * $Header: dlist.c,v 4.3 91/11/25 19:44:59 nicklin Exp $
 *
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
static void putinchain();		/* output nested subinclude filenames */
static void putinclude();		/* output include file pathname */
static void putobjd();			/* output object file name */
static void rmprinttag();		/* remove "already printed" tags */

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
		nocore();
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
		nocore();
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

	if (dlist->d_head != NULL)
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
	int putobj();			/* output object file name */

	COLUMN = putobj(srcblk->key, ofp) + 1;
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
