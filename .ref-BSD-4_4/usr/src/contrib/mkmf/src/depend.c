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
 * $Header: depend.c,v 4.10 93/05/25 21:17:49 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <stdio.h>
#include "Mkmf.h"
#include "config.h"
#include "dlist.h"
#include "hash.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "slist.h"
#include "stringx.h"
#include "yesno.h"

#define CURINCLUDE		"./"
#define TOLOWER(c)		(isupper(c) ? tolower(c) : (c))
#define ISWHITESPACE(c)		(c == ' ' || c == '\t')
#define SKIPWHITESPACE(c, f)	while ((c = getc(f))==' ' || c=='\t'); ungetc(c,f);
#define USRINCLUDE		"/usr/include/"
#define USRINCLUDECC		"/usr/include/CC/"

/*
 * Include file state
 */
#define NOTFOUND		0	/* not found anywhere */
#define EXTERNAL		1	/* not found in current directory */
#define INTERNAL		2	/* found in current directory */
#define FROMRULE		3	/* derived from tranformation rule */
#define SYSTEM			4	/* a system header file */

static char SYSINCLUDE[PATHSIZE];	/* /usr/include pathname buffer */
static char SYSINCLUDECC[PATHSIZE];	/* /usr/include/CC pathname buffer */
static int  SYSINCLUDELEN;		/* length of SYSINCLUDE */
/*
 * Include files are stored in hash tables by direct chaining (See
 * p. 134 in `The C Programming Language' by Kernighan and Ritchie).
 * Included include files are also added to a singly-linked list
 * attached to the hash table entry for the include file.
 */
static HASH *C_INCLUDETABLE   = NULL;	/* C include file hash table */
static HASH *CXX_INCLUDETABLE = NULL;	/* C++ include file hash table */
static HASH *F_INCLUDETABLE   = NULL;	/* Fortran include file hash table */
static HASH *P_INCLUDETABLE   = NULL;	/* Pascal include file hash table */
/*
 * Additional include directories are specified via the -I compiler
 * command line option. These directories are stored in singly-linked lists.
 * We also assume that the last look-up directory is "/usr/include".
 */
static SLIST *C_INCDIR;			/* C include directories */
static SLIST *CXX_INCDIR;		/* C++ include directories */
static SLIST *F_INCDIR;			/* Fortran include directories */
static SLIST *P_INCDIR;			/* Pascal include directories */

SLIST *EXTLIST;				/* external header file name list */
SLIST *SYSLIST;				/* system header file name list */

extern char *PGN;			/* program name */
extern int  SYSHDRS;			/* search system header files? */
extern HASH *MDEFTABLE;			/* macro definition table */

/*
 * addincdir() adds directories containing include files to the
 * appropriate singly-linked list. The pathnames to the directories
 * are derived from makefile macro definitions.
 */
void
addincdir()
{
	char *slappend();		/* append to singly-linked list */
	HASHBLK *htb;			/* hash table entry block */
	HASHBLK *htlookup();		/* find hash table entry */
	int cleanup();			/* remove temporary makefile and exit */
	void getI();			/* get include directory pathnames */

	/* initialize system include pathnames */
	initsysinclude();

	/* C CFLAGS macro */
	if ((htb = htlookup(MCFLAGS, MDEFTABLE)) != NULL)
		getI(htb->h_key, htb->h_def, C_INCDIR);
	if (slappend(SYSINCLUDE, C_INCDIR) == NULL)
		cleanup();
	
	/* C++ CXXFLAGS macro */
	if ((htb = htlookup(MCXXFLAGS, MDEFTABLE)) != NULL)
		getI(htb->h_key, htb->h_def, CXX_INCDIR);

	/* C++ C++FLAGS macro */
	if ((htb = htlookup(MCPXFLAGS, MDEFTABLE)) != NULL)
		getI(htb->h_key, htb->h_def, CXX_INCDIR);

	/* C++ CCFLAGS macro */
	if ((htb = htlookup(MCCFLAGS, MDEFTABLE)) != NULL)
		getI(htb->h_key, htb->h_def, CXX_INCDIR);

	if (slappend(SYSINCLUDECC, CXX_INCDIR) == NULL ||
	    slappend(SYSINCLUDE,   CXX_INCDIR) == NULL)
		cleanup();

	/* Fortran FFLAGS macro */
	if ((htb = htlookup(MFFLAGS, MDEFTABLE)) != NULL)
		getI(htb->h_key, htb->h_def, F_INCDIR);
	if (slappend(SYSINCLUDE, F_INCDIR) == NULL)
		cleanup();

	/* Pascal PFLAGS macro */
	if ((htb = htlookup(MPFLAGS, MDEFTABLE)) != NULL)
		getI(htb->h_key, htb->h_def, P_INCDIR);
	if (slappend(SYSINCLUDE, P_INCDIR) == NULL)
		cleanup();
}



/*
 * findinclude() tries to find the pathname of an include file. Returns
 * integer INTERNAL if found in the current directory, EXTERNAL if found
 * somewhere else, FROMRULE if derived from a transformation rule,
 * SYSTEM if a system header file, otherwise NOTFOUND.  The pathname
 * is copied into incpath.
 */
#define LOCALDIR(f)    (strchr(f, _PSC) == NULL)
#define ISYSINCLUDE(f) (strncmp(f, SYSINCLUDE, SYSINCLUDELEN) == 0)
#define INCLUDETYPE(f) (LOCALDIR(f) ? INTERNAL : (ISYSINCLUDE(f) ? SYSTEM : EXTERNAL))

findinclude(incpath, incname, lastname, type)
	char *incpath;			/* pathname receiving buffer */
	register char *incname;		/* include file name */
	char *lastname;			/* file that includes incname */
	int type;			/* file type */
{
	register char *pp;		/* include file path pointer */
	char *optpath();		/* optimize pathname */
	char *pathcat();		/* pathname concatenation */
	char *pathhead();		/* remove pathname tail */
	char *strpcpy();		/* string copy and update pointer */
	int applyrule();		/* apply transformation rules */
	SLBLK *slb;			/* singly-linked list block */
	SLIST *slist;			/* include directory list pointer */

	/*
	 * look for an absolute include file name
	 */
	if (*incname == '/')
		{
		strcpy(incpath, incname);
		return(FILEXIST(incpath) ? INCLUDETYPE(incpath) : NOTFOUND);
		}

	/*
	 * search the current include directory for an #include file
	 * whose name is enclosed in " ", or see if it can be generated
	 * by a transformation rule in the current working directory.
	 */
	if (*incname != '<')
		{
		if (LOCALDIR(lastname))
			{
			if (LOCALDIR(incname))
				{
				if (applyrule(incname, incpath) == YES)
					return(FROMRULE);
				}
			strcpy(incpath, incname);
			if (FILEXIST(incpath))
				return(INCLUDETYPE(incpath));
			}
		else	{
			strcpy(incpath, lastname);
			pathcat(incpath, pathhead(incpath), incname);
			optpath(incpath);
			if (FILEXIST(incpath))
				return(INCLUDETYPE(incpath));
			}
		}

	/*
	 * search directory list
	 */
	switch (type)
		{
		case INCLUDE_C:
			slist = C_INCDIR;
			break;
		case INCLUDE_CXX:
			slist = CXX_INCDIR;
			break;
		case INCLUDE_FORTRAN:
			slist = F_INCDIR;
			break;
		case INCLUDE_PASCAL:
			slist = P_INCDIR;
			break;
		}
	for (slb = slist->head; slb != NULL; slb = slb->next)
		{
		pp = strpcpy(incpath, slb->key);
		if (*incname == '<')
			{
			pp = strpcpy(pp, incname+1);
			pp[-1] = '\0';
			}
		else	{
			strcpy(pp, incname);
			}
		optpath(incpath);
		if (FILEXIST(incpath))
			return(INCLUDETYPE(incpath));
		}

	return(NOTFOUND);
}



/*
 * getI() appends include directories found via the -I compiler option to
 * a singly linked list.
 */
void
getI(mnam, mdef, slist)
	char *mnam;			/* compiler options macro name */
	char *mdef;			/* compiler options macro definition */
	SLIST *slist;			/* singly-linked list */
{
	char *getoption();		/* get next token */
	char incpath[PATHSIZE];		/* include directory pathname buffer */
	char *slappend();		/* append to singly-linked list */
	int cleanup();			/* remove temporary makefile and exit */

	while ((mdef = getoption(incpath, mdef, "-I")) != NULL)
		{
		if (*incpath == '\0')
			{
			warns("missing include directory in %s macro definition", mnam);
			break;
			}
		else	{
			strcat(incpath, PATHSEP);
			if (slappend(incpath, slist) == NULL)
				cleanup();
			}
		}
}



/*
 * getinclude() fetchs an include file name from a line of source code.
 * /usr/include '<' and '>' delimiters remain with the filename to
 * distinguish it from an include file in a local directory. Returns
 * NO if syntax error, otherwise YES.
 */
getinclude(incname, curname, lineno, ifp)
	char *curname;			/* current file name */
	char *incname;			/* include file name receiving buffer */
	int lineno;			/* current line number */
	register FILE *ifp;		/* input stream */
{
	register char *ip;		/* include file name buffer pointer */
	register int c;			/* current character */

	SKIPWHITESPACE(c, ifp);
	for (ip = incname; (c = getc(ifp)) != EOF; ip++)
		{
		*ip = c;
		if (c == '\n' || c == '\t' || c == ' ' ||
		    c == ';'  || c == ','  || c == '$' )
			{
			ungetc(c, ifp);
			break;
			}
		}
	*ip = '\0';
	if ((*incname == '<'  && ip[-1] != '>')  ||
	    (*incname == '\"' && ip[-1] != '\"') ||
	    (*incname == '\'' && ip[-1] != '\'') ||
	    (*incname == '('  && ip[-1] != ')'))
		{
		fprintf(stderr,
			"%s: \"%s\", line %d: bad include syntax for %s\n",
			PGN, curname, lineno, incname);
		return(NO);
		}
	if (*incname == '\"' || *incname == '\'' || *incname == '(')
		{
		ip[-1] = '\0';
		ip = incname + 1;
		while (*incname++ = *ip++)
			continue;
		}
	return(YES);
}



/*
 * inclink() stores a pointer to a hash table block (which contains
 * include file information) somewhere. Returns a pointer to the somewhere,
 * or calls cleanup() if out of memory.
 */
INCBLK *
inclink(htb)
	HASHBLK *htb;			/* hash table block pointer to save */
{
	char *malloc();			/* memory allocator */
	INCBLK *iblk;			/* pointer to new include chain block */
	int cleanup();			/* remove temporary makefile and exit */

	if ((iblk = (INCBLK *) malloc(sizeof(INCBLK))) == NULL)
		{
		nocore();
		cleanup();
		}
	iblk->i_loop = NO;
	iblk->i_hblk = htb;
	iblk->i_next = NULL;
	return(iblk);
}



/*
 * instalinclude() adds an include file name to the appropriate include
 * file hash table. Returns a pointer to the hash table block, or calls
 * cleanup() if out of memory.
 */
HASHBLK *
instalinclude(incname, incpath, type)
	char *incname;			/* name of include file */
	char *incpath;			/* path to include file */
	int type;			/* type of source file */
{
	HASH *htinit();			/* initialize hash table */
	HASHBLK *htb;			/* hash table block */
	HASHBLK *htinstall();		/* install hash table entry */
	int cleanup();			/* remove temporary makefile and exit */
	int ilen;			/* include path length */

	ilen = strlen(incpath);
	switch (type)
		{
		case INCLUDE_C:
			if (C_INCLUDETABLE == NULL)
				{
				C_INCLUDETABLE = htinit(INCLUDETABLESIZE);
				}
			htb = htinstall(incname, incpath, ilen, C_INCLUDETABLE);
			break;
		case INCLUDE_CXX:
			if (CXX_INCLUDETABLE == NULL)
				{
				CXX_INCLUDETABLE = htinit(INCLUDETABLESIZE);
				}
			htb = htinstall(incname, incpath, ilen, CXX_INCLUDETABLE);
			break;
		case INCLUDE_FORTRAN:
			if (F_INCLUDETABLE == NULL)
				{
				F_INCLUDETABLE = htinit(INCLUDETABLESIZE);
				}
			htb = htinstall(incname, incpath, ilen, F_INCLUDETABLE);
			break;
		case INCLUDE_PASCAL:
			if (P_INCLUDETABLE == NULL)
				{
				P_INCLUDETABLE = htinit(INCLUDETABLESIZE);
				}
			htb = htinstall(incname, incpath, ilen, P_INCLUDETABLE);
			break;
		default:
			htb = NULL;
			break;
		}
	if (htb == NULL)
		cleanup();
	return(htb);
}



/*
 * lookupinclude() returns a pointer to an include hash table block
 * corresponding to incname and type. Returns null if not found.
 */
HASHBLK *
lookupinclude(incname, type)
	char *incname;			/* name of include file */
	int type;			/* type of source file */
{
	HASH *includetable;		/* include file hash table */
	HASHBLK *htlookup();		/* find hash table entry */

	switch (type)
		{
		case INCLUDE_C:
			includetable = C_INCLUDETABLE;
			break;
		case INCLUDE_CXX:
			includetable = CXX_INCLUDETABLE;
			break;
		case INCLUDE_FORTRAN:
			includetable = F_INCLUDETABLE;
			break;
		case INCLUDE_PASCAL:
			includetable = P_INCLUDETABLE;
			break;
		default:
			includetable = NULL;
			break;
		}
	return((includetable == NULL) ? NULL : htlookup(incname, includetable));
}



/*
 * mkdepend() creates include file dependencies for object files and installs
 * them to dependency list dlp. Returns a pointer to the dependency list.
 */
DLIST *
mkdepend()
{
	extern SLIST *SRCLIST;		/* source file name list */
	char *suffix;			/* suffix pointer */
	DLBLK *dlappend();		/* append dependency list */
	DLIST *dlinit();		/* initialize dependency list */
	DLIST *dlist;			/* dependency list */
	INCBLK *ibp;			/* pointer to chain of include files */
	INCBLK *readC();		/* read C include-style files */
	INCBLK *readF();		/* read Fortran include-style files */
	INCBLK *readP();		/* read Pascal include-style files */
	int cleanup();			/* remove temporary makefile and exit */
	int lookuptypeofinclude();	/* look up the brand of include */
	int slsort();			/* sort singly-linked list */
	int type;			/* source file type */
	SLBLK *lbp;			/* list block pointer */
	SLIST *slinit();		/* initialize singly-linked list */
	void addincdir();		/* add to list of include directories */
	void rmprinttag();		/* remove "already printed" tags */

	/* initialize include file look-up lists */
	C_INCDIR   = slinit();
	CXX_INCDIR = slinit();
	F_INCDIR   = slinit();
	P_INCDIR   = slinit();

	/* add additional include directories */
	addincdir();

	/* initialize external and system header file name lists */
	EXTLIST = slinit();
	SYSLIST = slinit();

	/* initialize dependency list */
	dlist = dlinit();

	for (lbp = SRCLIST->head; lbp != NULL; lbp = lbp->next)
		{
		suffix = strrchr(lbp->key, '.');
		type = lookuptypeofinclude(++suffix);
		switch (type)
			{
			case INCLUDE_C:
			case INCLUDE_CXX:
				ibp = readC(lbp->key, 0, lbp->key, type);
				break;
			case INCLUDE_FORTRAN:
				ibp = readF(lbp->key, 0, lbp->key, type);
				break;
			case INCLUDE_PASCAL:
				ibp = readP(lbp->key, 0, lbp->key, type);
				break;
			case INCLUDE_NONE:
				ibp = NULL;
				break;
			}
		if (ibp != NULL)
			{
			if (dlappend(type, lbp, ibp, dlist) == NULL)
				cleanup();
			}
		}
	if (slsort(strcmp, EXTLIST) == NO || slsort(strcmp, SYSLIST) == NO)
		cleanup();
	return(dlist);
}



/*
 * notfound() prints a "can't find" filename error message.
 */
void
notfound(curname, lineno, incname)
	char *curname;			/* current file name */
	char *incname;			/* name of include file */
	int lineno;			/* current line number */
{
	if (PGN != NULL && *PGN != '\0')
		{
		fprintf(stderr, "%s: ", PGN);
		}
	if (*incname == '<')
		{
		fprintf(stderr, "\"%s\", line %d: can't find %s\n",
			curname, lineno, incname);
		}
	else	{
		fprintf(stderr, "\"%s\", line %d: can't find \"%s\"\n",
			curname, lineno, incname);
		}
}



/*
 * readC() searches C files for included files. Returns a pointer to
 * the chain of include files installed or found in the include file
 * hash table, or null if no include files found.
 */
INCBLK *
readC(lastfile, lastline, curname, type)
	char *lastfile;			/* parent file name */
	int lastline;			/* current line in parent file */
	char *curname;			/* current file name */
	int type;			/* file type */
{
	register char *p;		/* include string pointer */
	register FILE *ifp;		/* input file stream */
	register int c;			/* current character */
	char incname[PATHSIZE];		/* name of include file */
	char incpath[PATHSIZE];		/* path to include file */
	char *slappend();		/* append pathname to list */
	FILE *fopen();			/* open file */
	HASHBLK *ftb;			/* fromrule hash table entry block */
	HASHBLK *htb;			/* hash table entry block */
	HASHBLK *instalinclude();	/* install include name in hash table */
	HASHBLK *lookupinclude();	/* look up include in hash table */
	INCBLK *i_head = NULL;		/* head of include chain */
	INCBLK *i_tail = NULL;		/* tail of include chain */
	INCBLK *inclink();		/* link include file hash blocks */
	int cleanup();			/* remove temporary makefile and exit */
	int findinclude();		/* locate include file */
	int getinclude();		/* get include name from input line */
	int inctype;			/* origin of include file */
	int lineno = 1;			/* current line number */
	void notfound();		/* print "can't find" filename msg */

	if ((ifp = fopen(curname, "r")) == NULL)
		{
		if (lastline > 0)
			fprintf(stderr, "%s: \"%s\", line %d: ", PGN,
				lastfile, lastline);
		else
			fprintf(stderr, "%s: ", PGN);
		perror(curname);
		return(NULL);
		}
	while ((c = getc(ifp)) != EOF)
		{
		if (ISWHITESPACE(c))
			continue;
		if (c != '#')
			goto nextline;
		SKIPWHITESPACE(c, ifp);
		for (p = "include"; (c = getc(ifp)) == *p && *p != '\0' ; p++)
			continue;
		if (*p != '\0')
			goto nextline;
		ungetc(c, ifp);
		if (getinclude(incname, curname, lineno, ifp) == NO)
			goto nextline;
		if ((htb = lookupinclude(incname, type)) == NULL)
			{
			inctype = findinclude(incpath, incname, curname, type);
			if (inctype == INTERNAL)
				{
				htb = instalinclude(incname, incpath, type);
				}
			else if (inctype == SYSTEM && SYSHDRS == YES)
				{
				htb = instalinclude(incname, incpath, type);
				if (slappend(incpath, SYSLIST) == NULL)
					cleanup();
				}
			else if (inctype == SYSTEM && SYSHDRS == NO)
				{
				goto nextline;
				}
			else if (inctype == EXTERNAL)
				{
				htb = instalinclude(incname, incpath, type);
				if (slappend(incpath, EXTLIST) == NULL)
					cleanup();
				}
			else if (inctype == FROMRULE)
				{
				htb = instalinclude(incname, incname, type);
				ftb = instalinclude(incpath, incpath, type);
				}
			else	{
				notfound(curname, lineno, incname);
				goto nextline;
				}

			/* look for nested include files */
			htb->h_sub = readC(curname, lineno, incpath, type);

			if (inctype == FROMRULE)
				ftb->h_sub = htb->h_sub;
			}
		if (i_tail == NULL)
			{
			i_head = i_tail = inclink(htb);
			}
		else	{
			i_tail = i_tail->i_next = inclink(htb);
			}
nextline:	while (c != '\n' && c != EOF)
			c = getc(ifp);
		lineno++;
		}
	fclose(ifp);
	return(i_head);
}



/*
 * readF() searches Fortran files for included files. Returns a pointer
 * to the chain of include files installed or found in the include file
 * hash table, or null if no include files found.
 */
INCBLK *
readF(lastfile, lastline, curname, type)
	char *lastfile;			/* parent file name */
	int lastline;			/* current line in parent file */
	char *curname;			/* current file name */
	int type;			/* file type */
{
	register char *p;		/* include string pointer */
	register FILE *ifp;		/* input file stream */
	register int c;			/* current character */
	char incname[PATHSIZE];		/* name of include file */
	char incpath[PATHSIZE];		/* path to include file */
	char *slappend();		/* append pathname to list */
	FILE *fopen();			/* open file */
	HASHBLK *ftb;			/* fromrule hash table entry block */
	HASHBLK *htb;			/* hash table entry block */
	HASHBLK *instalinclude();	/* install include name in hash table */
	HASHBLK *lookupinclude();	/* look up include in hash table */
	INCBLK *i_head = NULL;		/* head of include chain */
	INCBLK *i_tail = NULL;		/* tail of include chain */
	INCBLK *inclink();		/* link include file hash blocks */
	int cleanup();			/* remove temporary makefile and exit */
	int findinclude();		/* locate include file */
	int getinclude();		/* get include name from input line */
	int inctype;			/* origin of include file */
	int lineno = 1;			/* current line number */
	void notfound();		/* print "can't find" filename msg */

	if ((ifp = fopen(curname, "r")) == NULL)
		{
		if (lastline > 0)
			fprintf(stderr, "%s: \"%s\", line %d: ", PGN,
				lastfile, lastline);
		else
			fprintf(stderr, "%s: ", PGN);
		perror(curname);
		return(NULL);
		}
	while ((c = getc(ifp)) != EOF)
		{
		if (c == 'c' || c == 'C' || c == '*' || c == '\n')
			goto nextline;
		while ((c = getc(ifp)) == ' ' || c == '\t' || c == '#' || c == '$')
			continue;
		for (p = "include"; *p == TOLOWER(c) && *p != '\0'; p++)
			c = getc(ifp);
		if (*p != '\0')
			goto nextline;
		ungetc(c, ifp);
		if (getinclude(incname, curname, lineno, ifp) == NO)
			goto nextline;
		if ((htb = lookupinclude(incname, type)) == NULL)
			{
			inctype = findinclude(incpath, incname, curname, type);
			if (inctype == INTERNAL)
				{
				htb = instalinclude(incname, incpath, type);
				}
			else if (inctype == SYSTEM && SYSHDRS == YES)
				{
				htb = instalinclude(incname, incpath, type);
				if (slappend(incpath, SYSLIST) == NULL)
					cleanup();
				}
			else if (inctype == SYSTEM && SYSHDRS == NO)
				{
				goto nextline;
				}
			else if (inctype == EXTERNAL)
				{
				htb = instalinclude(incname, incpath, type);
				if (slappend(incpath, EXTLIST) == NULL)
					cleanup();
				}
			else if (inctype == FROMRULE)
				{
				htb = instalinclude(incname, incname, type);
				ftb = instalinclude(incpath, incpath, type);
				}
			else	{
				notfound(curname, lineno, incname);
				goto nextline;
				}

			/* look for nested include files */
			htb->h_sub = readF(curname, lineno, incpath, type);

			if (inctype == FROMRULE)
				ftb->h_sub = htb->h_sub;
			}
		if (i_tail == NULL)
			{
			i_head = i_tail = inclink(htb);
			}
		else	{
			i_tail = i_tail->i_next = inclink(htb);
			}
nextline:	while (c != '\n' && c != EOF)
			c = getc(ifp);
		lineno++;
		}
	fclose(ifp);
	return(i_head);
}



/*
 * readP() searches Pascal files for included files. Returns a pointer
 * to the chain of include files installed or found in the include file
 * hash table, or null if no include files found.
 */
INCBLK *
readP(lastfile, lastline, curname, type)
	char *lastfile;			/* parent file name */
	int lastline;			/* current line in parent file */
	char *curname;			/* current file name */
	int type;			/* file type */
{
	register char *p;		/* include string pointer */
	register FILE *ifp;		/* input file stream */
	register int c;			/* current character */
	char incname[PATHSIZE];		/* name of include file */
	char incpath[PATHSIZE];		/* path to include file */
	char *slappend();		/* append pathname to list */
	FILE *fopen();			/* open file */
	HASHBLK *ftb;			/* fromrule hash table entry block */
	HASHBLK *htb;			/* hash table entry block */
	HASHBLK *instalinclude();	/* install include name in hash table */
	HASHBLK *lookupinclude();	/* look up include in hash table */
	INCBLK *i_head = NULL;		/* head of include chain */
	INCBLK *i_tail = NULL;		/* tail of include chain */
	INCBLK *inclink();		/* link include file hash blocks */
	int cleanup();			/* remove temporary makefile and exit */
	int findinclude();		/* locate include file */
	int getinclude();		/* get include name from input line */
	int inctype;			/* origin of include file */
	int lineno = 1;			/* current line number */
	void notfound();		/* print "can't find" filename msg */

	if ((ifp = fopen(curname, "r")) == NULL)
		{
		if (lastline > 0)
			fprintf(stderr, "%s: \"%s\", line %d: ", PGN,
				lastfile, lastline);
		else
			fprintf(stderr, "%s: ", PGN);
		perror(curname);
		return(NULL);
		}
	while ((c = getc(ifp)) != EOF)
		{
		if (ISWHITESPACE(c))
			continue;
		if (c != '#' && c != '$')
			goto nextline;
		while ((c = getc(ifp)) == ' ' || c == '\t')
			continue;
		for (p = "include"; *p == TOLOWER(c) && *p != '\0'; p++)
			c = getc(ifp);
		if (*p != '\0')
			goto nextline;
		ungetc(c, ifp);
		if (getinclude(incname, curname, lineno, ifp) == NO)
			goto nextline;
		if ((htb = lookupinclude(incname, type)) == NULL)
			{
			inctype = findinclude(incpath, incname, curname, type);
			if (inctype == INTERNAL)
				{
				htb = instalinclude(incname, incpath, type);
				}
			else if (inctype == SYSTEM && SYSHDRS == YES)
				{
				htb = instalinclude(incname, incpath, type);
				if (slappend(incpath, SYSLIST) == NULL)
					cleanup();
				}
			else if (inctype == SYSTEM && SYSHDRS == NO)
				{
				goto nextline;
				}
			else if (inctype == EXTERNAL)
				{
				htb = instalinclude(incname, incpath, type);
				if (slappend(incpath, EXTLIST) == NULL)
					cleanup();
				}
			else if (inctype == FROMRULE)
				{
				htb = instalinclude(incname, incname, type);
				ftb = instalinclude(incpath, incpath, type);
				}
			else	{
				notfound(curname, lineno, incname);
				goto nextline;
				}

			/* look for nested include files */
			htb->h_sub = readP(curname, lineno, incpath, type);

			if (inctype == FROMRULE)
				ftb->h_sub = htb->h_sub;
			}
		if (i_tail == NULL)
			{
			i_head = i_tail = inclink(htb);
			}
		else	{
			i_tail = i_tail->i_next = inclink(htb);
			}
nextline:	while (c != '\n' && c != EOF)
			c = getc(ifp);
		lineno++;
		}
	fclose(ifp);
	return(i_head);
}

initsysinclude()
{
	HASHBLK *htb;			/* hash table entry block */
	HASHBLK *htlookup();		/* find hash table entry */

#ifdef _HasCompileSysType
	if ((htb = htlookup(MCOMPILESYSTYPE, MDEFTABLE)) != NULL)
		{
	        sprintf(SYSINCLUDE, "/%s", htb->h_def);
	        sprintf(SYSINCLUDECC, "/%s", htb->h_def);
		}
#endif

        strcat(SYSINCLUDE, USRINCLUDE);
        strcat(SYSINCLUDECC, USRINCLUDECC);
	SYSINCLUDELEN = strlen(SYSINCLUDE)-1; /* length - last '/' */
}
