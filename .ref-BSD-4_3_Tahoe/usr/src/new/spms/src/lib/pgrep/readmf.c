/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <stdio.h>
#include "macro.h"
#include "null.h"
#include "pgrep.h"
#include "slist.h"
#include "yesno.h"

#define DEPENDMARK		"###"

static char Iobuf[BUFSIZ];		/* I/O line buffer */
static short Continue;			/* does the line continue? */

/*
 * dependmark() returns 1 if the current input line begins with a string
 * marking the beginning of generated dependency lines, otherwise zero.
 */
dependmark()

{
	if (EQUAL(Iobuf, DEPENDMARK))
		return(1);
	return(0);
}



/*
 * findmacro() searchs a line for a macro definition. Returns the name,
 * or null if not found.
 */
char *
findmacro(macroname)
	char *macroname;		/* macro name receiving buffer */
{
	register char *bp;		/* buffer pointer */
	register char *mp;		/* macro name pointer */

	bp = Iobuf;
	mp = macroname;
	while (*bp == ' ')
		bp++;
	if (!isalnum(*bp))
		return(NULL);
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
 * getlin() stores a line from input stream in Iobuf. The string is terminated
 * by a newline character which is replaced by a null character. getlin()
 * returns Iobuf, or null pointer upon end of file.
 */
char *
getlin(stream)
	register FILE *stream;		/* input stream */
{
	register int c;			/* current character */
	register char *iop;		/* Iobuf pointer */

	iop = Iobuf;
	while ((c = getc(stream)) != '\n' && c != EOF)
		*iop++ = c;
	if (c == EOF && iop == Iobuf)
		return(NULL);
	if (iop != Iobuf && iop[-1] == '\\')
		{
		iop[-1] = '\0';
		Continue = YES;
		}
	else	{
		iop[0] = '\0';
		Continue = NO;
		}
	return(Iobuf);
}



/*
 * getmacro() loads the body of a macro definition into slist and returns
 * a pointer to slist. If the macro definition continues on more than
 * one line further lines are fetched from the input stream. Returns NO
 * if out of memory, otherwise YES.
 */
getmacro(slist, stream)
	register SLIST *slist;		/* singly-linked list */
	register FILE *stream;		/* input stream */
{
	register char *bp;		/* buffer pointer */
	char mdefbuf[MACRODEFSIZE];	/* macro definition buffer */
	char *getlin();			/* get a line from input stream */
	char *gettoken();		/* get next token */
	char *slappend();		/* append key */

	bp = Iobuf;
	while (*bp++ != '=')
		continue;
	while ((bp = gettoken(mdefbuf, bp)) != NULL)
		if (slappend(mdefbuf, slist) == NULL)
			return(NO);
	while (Continue == YES)
		{
		if (getlin(stream) == NULL)
			break;
		bp = Iobuf;
		while ((bp = gettoken(mdefbuf, bp)) != NULL)
			if (slappend(mdefbuf, slist) == NULL)
				return(NO);
		}
	return(YES);
}



/*
 * gettoken() copies the next token from token buffer to token. Returns a
 * pointer to the first character after the token, or null upon reaching
 * the end of the token buffer.
 */
char *
gettoken(token, tp)
	register char *token;		/* receiving token */
	register char *tp;		/* token buffer pointer */
{
	while (isspace(*tp) && *tp != '\0')
		tp++;
	if (*tp == '\0')
		{
		*token = '\0';
		return(NULL);
		}
	while (!isspace(*tp) && *tp != '\0')
		*token++ = *tp++;
	*token = '\0';
	return(tp);
}



/*
 * readmf() reads a makefile and loads file names from HEADERS and SOURCE
 * macros into a singly-linked list. Returns NO on error, otherwise YES.
 */
readmf(mfname, filelist)
	char *mfname;			/* name of makefile */
	SLIST *filelist;		/* file name list */
{
	char *findmacro();		/* is the line a macro definition? */
	char *getlin();			/* get a line from input stream */
	char macroname[MACRONAMSIZE];	/* macro name buffer */
	FILE *fopen();			/* open file */
	FILE *fp;			/* file pointer */
	int dependmark();		/* generated dependency line? */
	int getmacro();			/* get macro def from input stream */
	SLIST *headlist;		/* header file name list */
	SLIST *slinit();		/* initialize list */
	SLIST *srclist;			/* source file name list */
	void slsplice();		/* splice lists together */

	headlist = slinit();
	srclist = slinit();

	if ((fp = fopen(mfname, "r")) == NULL)
		{
		pperror(mfname);
		return(NO);
		}
	while (getlin(fp) != NULL)
		{
		if (dependmark())
			break;
		if (findmacro(macroname) != NULL)
			{
			if (EQUAL(macroname, MHEADERS))
				{
				if (getmacro(headlist, fp) == NO)
					return(NO);
				}
			else if (EQUAL(macroname, MSOURCE))
				{
				if (getmacro(srclist, fp) == NO)
					return(NO);
				}
			}
		}
	fclose(fp);

	/* splice header files to file list */
	slsplice(filelist, headlist);

	/* splice source files to file list */
	slsplice(filelist, srclist);

	return(YES);
}
