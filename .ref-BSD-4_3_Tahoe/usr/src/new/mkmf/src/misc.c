/* $Header: misc.c,v 1.6 85/06/27 08:15:57 nicklin Exp $ */

/*
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <stdio.h>
#include "Mkmf.h"
#include "hash.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "target.h"
#include "spms.h"
#include "suffix.h"
#include "system.h"
#include "yesno.h"

/*
 * answer() installs a line from stdin in the macro definition table.
 * exit(1) is called if EOF, error, or out of memory.
 */
void
answer(mdefkey, mdefval)
	char *mdefkey;			/* macro definition key */
	int mdefval;			/* macro definition value */
{
	extern HASH *MDEFTABLE;		/* macro definition table */
	char answerbuf[ANSWERBUFSIZE];	/* answer from stdin */
	char *gets();			/* get a line from stdin */
	HASHBLK *htinstall();		/* install hash table entry */

	if (gets(answerbuf) == NULL)
		exit(1);
	if (*answerbuf != '\0')
		if (htinstall(mdefkey, answerbuf, mdefval, MDEFTABLE) == NULL)
			exit(1);
}



/*
 * fastcopy() copies file to stream fp. Returns integer YES if successful,
 * otherwise NO.
 */
fastcopy(filename, ofp)
	char *filename;			/* file to be copied */
	register FILE *ofp;		/* output stream */
{
	register int ifd;		/* input file descriptor */
	register int n;			/* byte count */
	char buf[BUFSIZ];		/* I/O buffer */

	if ((ifd = OPEN(filename, 0, 0644)) == -1)
		{
		pperror("");
		return(NO);
		}
	while ((n = read(ifd, buf, BUFSIZ)) > 0)
		write(fileno(ofp), buf, n);
	close(ifd);
	return(YES);
}



/*
 * findmf() locates the makefile to be edited. The type of makefile
 * is returned in target. The pathname to the makefile is returned
 * in mfpath. Returns YES if makefile or makefile template can be
 * opened, otherwise NO.
 */
findmf(mfname, mfpath, target)
	char *mfname;			/* name of target makefile */
	char *mfpath;			/* path to target makefile */
	TARGET *target;			/* type of makefile target */
{
	extern int CFLAG;		/* makefile creation message */
	char *strcpy();			/* string copy */
	int readmf();			/* read makefile */
	int targettype;			/* type of makefile requested */
	void findmftemplate();		/* find makefile template */

	targettype = target->type;
	if (FILEXIST(mfname))
		{
		if (!FILEWRITE(mfname))
			{
			pperror(mfname);
			target->type = VERROR;
			return(NO);
			}
		if (readmf(mfname, target) == VERROR)
			return(NO);
		if (targettype == VUNKNOWN || targettype == target->type)
			{
			strcpy(mfpath, mfname);
			return(YES);
			}
		}
	target->type = (targettype == VLIBRARY) ? VLIBRARY : VPROGRAM;
	findmftemplate(mfpath, target);
	if (readmf(mfpath, target) == VERROR)
		return(NO);
	else if (CFLAG == YES)
		warn2("creating %s from template %s", mfname, mfpath);
	return(YES);
}



/*
 * findmftemplate() returns the pathname of a makefile template in mfpath.
 */
void
findmftemplate(mfpath, target)
	char *mfpath;			/* path to target makefile */
	TARGET *target;			/* type of makefile target */
{
	extern char *L_MAKEFILE;	/* library makefile template */
	extern char *P_MAKEFILE;	/* program makefile template */
	char *cwp;			/* current project pathname pointer */
	char *getcwp();			/* get current project pathname */
	char *pathcat();		/* pathname concatenation */
	char *strcpy();			/* string copy */

	cwp = getcwp();
	if (target->type == VPROGRAM)
		{
		if (*P_MAKEFILE == _RDIRC)
			strcpy(mfpath, P_MAKEFILE);
		else if (cwp == NULL ||
			 !FILEXIST(pathcat(mfpath, pathcat(mfpath, cwp, "lib"), P_MAKEFILE)))
			pathcat(mfpath, SPMSLIB, P_MAKEFILE);
		}
	else if (target->type == VLIBRARY)
		{
		if (*L_MAKEFILE == _RDIRC)
			strcpy(mfpath, L_MAKEFILE);
		else if (cwp == NULL ||
			 !FILEXIST(pathcat(mfpath, pathcat(mfpath, cwp, "lib"), L_MAKEFILE)))
			pathcat(mfpath, SPMSLIB, L_MAKEFILE);
		}
	else
		warn("unknown template request");
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
 * putobj() converts a source file name to an object file name and then
 * writes the file name to stream.
 */
void
putobj(s, stream)
	register char *s;		/* source file name */
	register FILE *stream;		/* output stream */
{
	extern char OBJSFX[];		/* object file name suffix */
	register char *dot;		/* pointer to suffix */
	char *rindex();			/* last occurrence of character */

	dot = rindex(s, '.');
	while (s != dot)
		putc(*s++, stream);
	fprintf(stream, "%s", OBJSFX);
}



/*
 * readmf() reads a makefile and loads CFLAGS, FFLAGS, and SUFFIX definitions
 * into the macro definition table if they do not already exist. Returns
 * integer VLIBRARY, VPROGRAM, or VUNKNOWN according to the type of makefile,
 * or VERROR if cannot open makefile.
 */
readmf(mfname, target)
	char *mfname;			/* name of makefile */
	TARGET *target;			/* type of makefile target */
{
	register char *bp;		/* buffer pointer */
	extern char IOBUF[];		/* I/O buffer line */
	extern HASH *MDEFTABLE;		/* macro definition table */
	char *findmacro();		/* is the line a macro definition? */
	char *findrule();		/* is the line a rule definition? */
	char *getlin();			/* get a line from input stream */
	char *getmacro();		/* get macro def from input stream */
	char macrodef[MACRODEFSIZE];	/* macro definition buffer */
	char macroname[MACRONAMSIZE];	/* macro name buffer */
	char rulename[2*SUFFIXSIZE+3];	/* transformation rule name */
	FILE *fopen();			/* open file */
	FILE *fp;			/* file pointer */
	HASHBLK *htinstall();		/* install hash table entry */
	HASHBLK *htlookup();		/* find hash table entry */
	int storerule();		/* store transformation rule */

	target->type = target->dest = VUNKNOWN;
	if ((fp = fopen(mfname, "r")) == NULL)
		{
		pperror(mfname);
		target->type = VERROR;
		return(target->type);
		}
	while (getlin(fp) != NULL)
		{
		if (EQUAL(IOBUF, DEPENDMARK))
			break;
		for (bp = IOBUF; *bp == ' '; bp++)
			continue;
		if (isalnum(*bp) && findmacro(macroname, bp) != NULL)
			{
			if (EQUAL(macroname, MPROGRAM))
				{
				target->type = VPROGRAM;
				continue;
				}
			else if (EQUAL(macroname, MLIBRARY))
				{
				target->type = VLIBRARY;
				continue;
				}
			else if (EQUAL(macroname, MDESTDIR))
				{
				target->dest = VDESTDIR;
				continue;
				}

			/* does macro definition already exist? */
			if (htlookup(macroname, MDEFTABLE) != NULL)
				continue;

			if (EQUAL(macroname, MCFLAGS) ||
			    EQUAL(macroname, MFFLAGS) ||
			    EQUAL(macroname, MPFLAGS) ||
			    EQUAL(macroname, MSUFFIX))
				{
				if (htinstall(macroname, getmacro(macrodef, fp),
					      VREADONLY, MDEFTABLE) == NULL)
					{
					fclose(fp);
					return(NO);
					}
				}
			}
		else if (*bp == '.' && findrule(rulename, bp) != NULL)
			{
			if (storerule(rulename) == NO)
				{
				fclose(fp);
				return(NO);
				}
			}
		}
	fclose(fp);
	return(target->type);
}
