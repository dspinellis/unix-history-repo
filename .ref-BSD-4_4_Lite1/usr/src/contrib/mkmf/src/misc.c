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
 * $Header: misc.c,v 4.10 93/05/25 21:49:09 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <stdio.h>
#include <sys/types.h>
#include "Mkmf.h"
#include "config.h"
#include "dir.h"
#include "hash.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "target.h"
#include "stringx.h"
#include "suffix.h"
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
		if (targettype != VUNKNOWN && targettype != target->type)
			{
			if (targettype == VPROGRAM)
				{
				warns("%s not a program makefile", mfname);
				}
			else if (targettype == VLIBRARY)
				{
				warns("%s not a library makefile", mfname);
				}
			else	{
				warns("%s: unknown makefile type", mfname);
				}
			return(NO);
			}
		strcpy(mfpath, mfname);
		}
	else	{
		target->type = (targettype == VLIBRARY) ? VLIBRARY : VPROGRAM;
		findmftemplate(mfpath, target);
		if (readmf(mfpath, target) == VERROR)
			return(NO);
		if (CFLAG == YES)
			warn2("creating %s from %s", mfname, mfpath);
		}
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
	extern char *MAKEFILE;		/* makefile template name */
	extern int FFLAG;	        /* makefile template pathname flag */
	extern int LIBOBJ;		/* load object file into library? */
	char *cwp;			/* current project pathname pointer */
	char *getcwp();			/* get current project pathname */
	char *mktname();		/* make template name */
	char *pathcat();		/* pathname concatenation */
	char tname[MAXNAMLEN+1];	/* template name */

	if (FFLAG == YES)
		{
		strcpy(mfpath, MAKEFILE);
		return;
		}
	strcpy(tname, MAKEFILE);
	if (strrchr(tname, '.') == NULL)
		switch (target->type)
			{
			case VPROGRAM:
				mktname(tname, SPROGRAM);
				break;
			case VLIBRARY:
				if (LIBOBJ == 1)
					{
					mktname(tname, SLIBRARY2);
					}
				else	{
					mktname(tname, SLIBRARY);
					}
				break;
			}

	cwp = getcwp();
	if (cwp == NULL ||
	    !FILEXIST(pathcat(mfpath, pathcat(mfpath, cwp, MKMFLIB), tname)))
		pathcat(mfpath, pathcat(mfpath, INSTALLDIR, MKMFLIB), tname);
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
 * getoption() copies the next matching option from token buffer to token.
 * Returns a pointer to the first character after the token, or null upon
 * reaching the end of the token buffer.
 */
char *
getoption(token, tp, option)
	register char *token;		/* receiving token */
	register char *tp;		/* token buffer pointer */
	register char *option;		/* option leader string */
{
	int optlen = strlen(option);	/* option length */

	if ((tp = gettoken(token, tp)) == NULL)
		return(NULL);

	/* skip non-matching options */
	while (strncmp(token, option, optlen) != 0)
		{
		if ((tp = gettoken(token, tp)) == NULL)
			{
			return(NULL);
			}
		}

	if (EQUAL(token, option))
		tp = gettoken(token, tp);		/* -X option */
	else
		(void) gettoken(token, token+optlen);	/* -Xoption */

	return(tp);
}



/*
 * getpath() copies the next pathname from path buffer to path. Returns a
 * pointer to the first character after the path, or null upon reaching
 * the end of the path buffer.
 */
char *
getpath(path, pp)
	register char *path;		/* receiving path */
	register char *pp;		/* path buffer pointer */
{
	while ((isspace(*pp) || *pp == ':') && *pp != '\0')
		pp++;
	if (*pp == '\0')
		{
		*path = '\0';
		return(NULL);
		}
	while (!(isspace(*pp) || *pp == ':') && *pp != '\0')
		*path++ = *pp++;
	*path = '\0';
	return(pp);
}



/*
 * libobj() returns NO if the definition of the OBJS macro contains regular
 * object file names, and YES if it contains $(LIBRARY)(objfile) file names
 * (indicating that each object file should be inserted into a library
 * immediately after compilation). Only the first line of the macro is
 * scanned for $(LIBRARY).
 */
int
libobj(bp)
	register char *bp;		/* buffer pointer */
{
	register char *sbp;		/* save buffer pointer */
	register char *mp;		/* macro name pointer */

	while (*bp++ != '=')
		continue;
	if (WHITESPACE(*bp))
		bp++;
	for (mp = DLIBRARY, sbp = bp; *mp == *bp && *mp != '\0'; mp++, bp++)
		continue;
	if (*mp == '\0')
		return(YES);
	for (mp = dLIBRARY, bp = sbp; *mp == *bp && *mp != '\0'; mp++, bp++)
		continue;
	if (*mp == '\0')
		return(YES);
	return(NO);
}



/*
 * mktname() concatenates a suffix to a makefile template name. Returns
 * name if successful, otherwise NULL.
 */
char *
mktname(base, suffix)
	char *base;			/* template basename */
	char *suffix;			/* template suffix */
{
	if (strlen(base) + strlen(suffix) > MAXNAMLEN)
		{
		warn("makefile template name too long");
		return(NULL);
		}
	else	{
		return(strcat(base, suffix));
		}
}



/*
 * nocore() places an "out of memory" error message on the standard error
 * output stream stderr.
 */
nocore()
{
	warn("out of memory");
}



/*
 * putobj() converts a source file name to an object file name and then
 * writes the file name to stream. Returns the length of the file name.
 */
putobj(s, stream)
	register char *s;		/* source file name */
	register FILE *stream;		/* output stream */
{
	register int baselen = 0;	/* length of object file basename */
	register char *dot;		/* pointer to suffix */
	char *pathsep;			/* pathname separator */
	static int psfxlen = 0;		/* length of object prefix & suffix */
	extern int LIBOBJ;		/* load object file into library? */
	extern char OBJSFX[];		/* object file name suffix */

	if (psfxlen == 0)
		{
		psfxlen = strlen(OBJSFX);
		if (LIBOBJ) psfxlen += strlen(DLIBRARY) + 2;
		}
	if ((pathsep = strrchr(s, _PSC)) != NULL)
		s = pathsep + 1;
	dot = strrchr(s, '.');
	if (LIBOBJ)
		{
		fprintf(stream, "%s(", DLIBRARY);
		while (s != dot)
			{
			putc(*s++, stream);
			baselen++;
			}
		fprintf(stream, "%s)", OBJSFX);
		}
	else	{
		while (s != dot)
			{
			putc(*s++, stream);
			baselen++;
			}
		fprintf(stream, "%s", OBJSFX);
		}
	return(psfxlen+baselen);
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
	extern int LIBOBJ;		/* load object file into library? */
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
	int libobj();			/* is object file loaded into library?*/
	int storerule();		/* store transformation rule */
	void purgcontinue();		/* get rid of continuation lines */

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
		if (findmacro(macroname, bp) != NULL)
			{
			if (EQUAL(macroname, MPROGRAM))
				{
				target->type = VPROGRAM;
				}
			else if (EQUAL(macroname, MLIBRARY))
				{
				target->type = VLIBRARY;
				}
			else if (EQUAL(macroname, MDESTDIR))
				{
				target->dest = VDESTDIR;
				}

			if (EQUAL(macroname, MOBJECTS))
				{
				if (libobj(bp) == YES)
					LIBOBJ = 1;
				purgcontinue;
				}

			/* does macro definition already exist? */
			if (htlookup(macroname, MDEFTABLE) != NULL)
				continue;

			if (htinstall(macroname, getmacro(macrodef, fp),
				      VREADONLY, MDEFTABLE) == NULL)
				{
				fclose(fp);
				return(NO);
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
