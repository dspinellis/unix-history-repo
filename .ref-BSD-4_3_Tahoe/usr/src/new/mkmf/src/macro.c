/* $Header: macro.c,v 1.3 85/05/02 07:53:52 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <stdio.h>
#include "Mkmf.h"
#include "hash.h"
#include "macro.h"
#include "null.h"
#include "slist.h"
#include "yesno.h"

extern char IOBUF[];			/* I/O buffer line */

/*
 * findmacro() searchs a line for a macro definition. Returns the name,
 * or null if not found.
 */
char *
findmacro(macroname, bp)
	char *macroname;		/* macro name receiving buffer */
	register char *bp;		/* buffer pointer */
{
	register char *mp;		/* macro name pointer */

	mp = macroname;
	while(isalnum(*bp))
		*mp++ = *bp++;
	*mp = '\0';
	while(*bp == ' ' || *bp == '\t')
		bp++;
	if (*bp != '=')
		return(NULL);
	return(macroname);
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
	char *rindex();			/* find last occurrence of character */
	char *suffix;			/* suffix pointer */
	HASHBLK *lookupinclude();	/* look up include name in hash table */
	int cnt = 0;			/* number of object filenames printed */
	int lookuptypeofinclude();	/* look up the brand of include */
	int type;			/* file type */
	SLBLK *lbp;			/* list block pointer */
	void putobj();			/* print object file name */

	iop = IOBUF;
	while ((c = *iop++) != '=')
		putc(c, stream);
	putc('=', stream);
	for (lbp = SRCLIST->head; lbp != NULL; lbp = lbp->next)
		{
		suffix = rindex(lbp->key, '.');
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
	extern HASH *MDEFTABLE;		/* macro definition table */
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
