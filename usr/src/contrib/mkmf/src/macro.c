/*
 * Copyright (c) 1983, 1985, 1991, 1993 Peter J. Nicklin.
 * Copyright (c) 1991, 1993 Version Technology.
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
 * $Header: macro.c,v 4.8 93/05/25 21:49:09 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <stdio.h>
#include "Mkmf.h"
#include "hash.h"
#include "macro.h"
#include "null.h"
#include "slist.h"
#include "stringx.h"
#include "yesno.h"

extern char IOBUF[];			/* I/O buffer line */
extern HASH *MDEFTABLE;			/* macro definition table */

/*
 * findmacro() searchs a line for a macro definition. A macro definition
 * takes the form "name op= value" where "name" is any sequence of characters
 * excluding the '=' character or whitespace. "op" may be one of the '+', '?',
 * ':', or '!' characters which have special meanings for Berkeley make(1).
 * findmacro() returns the name, or NULL if not found.
 */
char *
findmacro(macroname, bp)
	char *macroname;		/* macro name receiving buffer */
	register char *bp;		/* buffer pointer */
{
	register char *mp;		/* macro name pointer */

	while (WHITESPACE(*bp))
		bp++;

	for (mp = macroname; *bp && *bp != '='; bp++, mp++)
		{
		if (WHITESPACE(*bp))
			{
			break;
			}
		else if ((*bp == '+' ||
			  *bp == '?' ||
			  *bp == ':' ||
			  *bp == '!') && bp[1] == '=')
			{
			break;
			}
		*mp = *bp;
		}

	*mp = '\0';

	while (WHITESPACE(*bp))
		bp++;

	if (*bp == '+' || *bp == '?' || *bp == ':' || *bp == '!')
		bp++;

	return (*bp == '=' && mp > macroname) ? macroname : NULL;
}
	


/*
 * getmacro() loads the body of a macro definition into mdefbuf and returns
 * a pointer to mdefbuf. If the macro definition continues on more than
 * one line further lines are fetched from the input stream.
 */
char *
getmacro(mdefbuf, stream)
	char *mdefbuf;			/* receiving macro definition buffer */
	FILE *stream;			/* input stream */
{
	extern short CONTINUE;		/* does the line continue? */
	char *getlin();			/* get a line from input stream */
	register char *bp;		/* buffer pointer */
	register char *mp;		/* macro definition buffer pointer */

	bp = IOBUF;
	mp = mdefbuf;
	while (*bp++ != '=')
		continue;
	if (WHITESPACE(*bp))
		bp++;
	while (*bp != '\0')
		*mp++ = *bp++;
	while (CONTINUE == YES)
		{
		*mp++ = ' ';
		if (getlin(stream) == NULL)
			break;
		bp = IOBUF;
		while (*bp != '\0')
			*mp++ = *bp++;
		}
	*mp = '\0';
	return(mdefbuf);
}



/*
 * putmacro() prints a macro definition from the macro definition table.
 */
void
putmacro(macrovalue, stream)
	char *macrovalue;		/* value of macro definition */
	register FILE *stream;		/* output stream */
{
	register char *iop;		/* IOBUF pointer */
	register int c;			/* current character */

	iop = IOBUF;
	while ((c = *iop++) != '=')
		putc(c, stream);
	fprintf(stream, "= %s\n", macrovalue);
}



/*
 * putobjmacro() derives and prints object file names from the SRCLIST list.
 */
void
putobjmacro(stream)
	register FILE *stream;		/* output stream */
{
	extern SLIST *SRCLIST;		/* source file name list */
	register char *iop;		/* IOBUF pointer */
	register int c;			/* current character */
	char *suffix;			/* suffix pointer */
	HASHBLK *lookupinclude();	/* look up include name in hash table */
	int cnt = 0;			/* number of object filenames printed */
	int lookuptypeofinclude();	/* look up the brand of include */
	int putobj();			/* print object file name */
	int type;			/* file type */
	SLBLK *lbp;			/* list block pointer */

	iop = IOBUF;
	while ((c = *iop++) != '=')
		putc(c, stream);
	putc('=', stream);
	for (lbp = SRCLIST->head; lbp != NULL; lbp = lbp->next)
		{
		suffix = strrchr(lbp->key, '.');
		type = lookuptypeofinclude(++suffix);
		if (lookupinclude(lbp->key, type) == NULL)
			{
			cnt += 1;
			if (cnt == 1)
				{
				putc(' ', stream);
				putobj(lbp->key, stream);
				}
			else	{
				fprintf(stream, " \\\n\t\t");
				putobj(lbp->key, stream);
				}
			}
		}
	putc('\n', stream);
}



/*
 * putslmacro() copies a macro definition from a list.
 */
void
putslmacro(slist, stream)
	SLIST *slist;			/* singly-linked macro def list */
	register FILE *stream;		/* output stream */
{
	register char *iop;		/* IOBUF pointer */
	register int c;			/* current character */
	SLBLK *lbp;			/* list block pointer */

	iop = IOBUF;
	while ((c = *iop++) != '=')
		putc(c, stream);
	putc('=', stream);
	if (SLNUM(slist) > 0)
		{
		lbp = slist->head;
		fprintf(stream, " %s", lbp->key);
		}
	if (SLNUM(slist) > 1)
		for (lbp = lbp->next; lbp != NULL; lbp = lbp->next)
			fprintf(stream, " \\\n\t\t%s", lbp->key);
	putc('\n', stream);
}



/*
 * storemacro() stores a macro definition in the macro definition table.
 * Returns integer YES if a macro definition (macro=definition), otherwise
 * NO. exit(1) is called if out of memory.
 */
storemacro(macdef)
	char *macdef;			/* macro definition string */
{
	register int i;			/* macro value index */
	register int j;			/* macro name index */
	HASHBLK *htinstall();		/* install hash table entry */

	for (i = 0; macdef[i] != '='; i++)
		if (macdef[i] == '\0')
			return(NO);
	
	/* removing trailing blanks and tabs from end of macro name */
	for (j = i; j > 0; j--)
		if (!WHITESPACE(macdef[j-1]))
			break;
	macdef[j] = '\0';

	/* remove leading blanks and tabs from macro value */
	for (i++; WHITESPACE(macdef[i]); i++)
		continue;
	if (htinstall(macdef, macdef+i, VREADWRITE, MDEFTABLE) == NULL)
		exit(1);
	return(YES);
}



/*
 * storenvmacro() stores all of the environment variables as
 * though they were macros. Returns YES if successful, otherwise
 * NO.
 */
storenvmacro()
{
	extern char **environ;		/* user environment */
	register char **ep;		/* environment pointer */
	register char *value;		/* macro value pointer */
	register int i;			/* macro name index */
	char macroname[MACRONAMSIZE];	/* macro name buffer */

	if (environ)
		for (ep = environ; *ep != NULL; ep++)
			{
			value = *ep;
			for (i = 0; *value && *value != '='; i++, value++)
				macroname[i] = *value;
			if (*value == '=')
				{
				macroname[i] = '\0';
				value++;
				}

			if (htlookup(macroname, MDEFTABLE) != NULL)
				continue;

			if (htinstall(macroname, value,
				      VREADONLY, MDEFTABLE) == NULL)
				return(NO);
			}
	return(YES);
}



/*
 * storedynmacro() creates placeholders for certain macro definitions
 * that ought to be generated unless specified on the command line.
 * This prevents environment variables with the same names from being
 * loaded into the macro definition table unnecessarily.
 */
storedynmacro()
{
	if (htlookup(MHEADERS, MDEFTABLE) == NULL)
		if (htinstall(MHEADERS, "", VDYNAMIC, MDEFTABLE) == NULL)
			return(NO);

	if (htlookup(MOBJECTS, MDEFTABLE) == NULL)
		if (htinstall(MOBJECTS, "", VDYNAMIC, MDEFTABLE) == NULL)
			return(NO);

	if (htlookup(MSOURCES, MDEFTABLE) == NULL)
		if (htinstall(MSOURCES, "", VDYNAMIC, MDEFTABLE) == NULL)
			return(NO);

	if (htlookup(MEXTERNALS, MDEFTABLE) == NULL)
		if (htinstall(MEXTERNALS, "", VDYNAMIC, MDEFTABLE) == NULL)
			return(NO);
	return(YES);
}
