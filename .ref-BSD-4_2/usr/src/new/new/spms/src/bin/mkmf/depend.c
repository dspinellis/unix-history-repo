/* $Header$ */

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
#include "slist.h"
#include "system.h"
#include "yesno.h"

#define DEPENDMARK		"###"
#define USRINCLUDE		"/usr/include/"
#define TOLOWER(c)		(isupper(c) ? tolower(c) : (c))
#define SKIPWHITESPACE(c, f) while ((c = getc(f))==' ' || c=='\t'); ungetc(c,f);

/*
 * Dependency line print states
 */
#define FIRSTLINE		0
#define NEXTLINE		1

/*
 * Include file state
 */
#define NOTFOUND		0	/* not found anywhere */
#define EXTERNAL		1	/* not found in current directory */
#define INTERNAL		2	/* found in current directory */

/*
 * Include files are stored in hash tables by direct chaining (See
 * p. 134 in `The C Programming Language' by Kernighan and Ritchie).
 * Included include files are also added to a singly-linked list
 * attached to the hash table entry for the include file.
 */
static HASH *C_INCLUDETABLE;		/* C include file hash table */
static HASH *F_INCLUDETABLE;		/* Fortran include file hash table */
static HASH *P_INCLUDETABLE;		/* Pascal include file hash table */
/*
 * Additional include directories are stored in singly-linked lists.
 * At present additional directories can only be specified for  C and f77
 * compilers via the -I option. Also, we assume that "/usr/include" is
 * the default look-up directory for the C compiler.
 */
static SLIST *C_INCDIR;			/* C include directories */
static SLIST *F_INCDIR;			/* Fortran include directories */

static char *CURSRC;			/* current source filename */
static HASHBLK *printtag = NULL;	/* include files already printed */
static int COLUMN;			/* last column printed */
static int DEPSTAT;			/* dependency print state */
SLIST *EXTLIST;				/* external header file name list */

/*
 * addincdir() adds directories containing include files to the
 * appropriate singly-linked list. The pathnames to the directories
 * are derived from makefile macro definitions.
 */
void
addincdir()
{
	extern HASH *MDEFTABLE;		/* macro definition table */
	char *slappend();		/* append to singly-linked list */
	HASHBLK *htb;			/* hash table entry block */
	HASHBLK *htlookup();		/* find hash table entry */
	void getI();			/* get include directory pathnames */

	/* C files */
	if ((htb = htlookup(MCFLAGS, MDEFTABLE)) != NULL)
		getI(htb->h_key, htb->h_def, C_INCDIR);
	if (slappend(USRINCLUDE, C_INCDIR) == NULL)
		exit(1);
	
	/* Fortran files */
	if ((htb = htlookup(MFFLAGS, MDEFTABLE)) != NULL)
		getI(htb->h_key, htb->h_def, F_INCDIR);
}



/*
 * dependmark() returns 1 if the current input line begins with a string
 * marking the beginning of generated dependency lines, otherwize zero.
 */
dependmark()

{
	extern char IOBUF[];		/* I/O buffer line */

	if (EQUAL(IOBUF, DEPENDMARK))
		return(1);
	return(0);
}



/*
 * findinclude() tries to find the pathname of an include file. Returns
 * integer INTERNAL if found in the current directory, EXTERNAL if found
 * somewhere else, otherwise NOTFOUND. The pathname is copied into incpath.
 */
findinclude(incpath, incname, type)
	char *incpath;			/* pathname receiving buffer */
	register char *incname;		/* include file name */
	int type;			/* file type */
{
	register char *pp;		/* include file path pointer */
	register int quote;		/* delimiter character */
	char *strcpy();			/* string copy */
	char *strpcpy();		/* string copy and update pointer */
	SLBLK *slb;			/* singly-linked list block */
	SLIST *slist;			/* include directory list pointer */

	if (*incname == '<')		/* look in /usr/include only */
		{
		pp = strpcpy(incpath, USRINCLUDE);
		for (incname++; *incname != '>'; incname++, pp++)
			*pp = *incname;
		*pp = '\0';
		if (FILEXIST(incpath))
			return(EXTERNAL);
		}
	else	{			/* look in current directory */
		quote = (*incname == '\"') ? *incname++ : '\0';
		for (pp = strcpy(incpath, incname); *pp != quote; pp++)
			continue;
		*pp = '\0';
		if (FILEXIST(incpath))
			return(INTERNAL);

		switch (type)		/* search directory list */
			{
			case INCLUDE_C:
				slist = C_INCDIR;
				break;
			case INCLUDE_FORTRAN:
				slist = F_INCDIR;
				break;
			case INCLUDE_PASCAL:
				return(NOTFOUND);
			}
		for (slb = slist->head; slb != NULL; slb = slb->next)
			{
			pp = strpcpy(incpath, slb->key);
			for (pp = strcpy(pp, incname); *pp != quote; pp++)
				continue;
			*pp = '\0';
			if (FILEXIST(incpath))
				return(EXTERNAL);
			}
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
	char *gettoken();		/* get next token */
	char incpath[PATHSIZE];		/* include directory pathname buffer */
	char *slappend();		/* append to singly-linked list */
	char *strcat();			/* string concatenation */

	while ((mdef = gettoken(incpath, mdef)) != NULL)
		if (incpath[0] = '-' && incpath[1] == 'I')
			if (incpath[2] == '\0')	/* -I dir option */
				{
				if ((mdef = gettoken(incpath, mdef)) != NULL)
					{
					strcat(incpath, PATHSEP);
					if (slappend(incpath, slist) == NULL)
						exit(1);
					}
				else	{
					warn("missing include directory in %s%s",
					     mnam, " macro definition");
					}
				}
			else	{		/* -Idir option */
				strcat(incpath+2, PATHSEP);
				if (slappend(incpath+2, slist) == NULL)
					exit(1);
				}
}



/*
 * getinclude() fetchs an include file name from a line of source code.
 * Returns NO if syntax error, otherwise YES.
 */
getinclude(incname, ifp)
	char *incname;			/* include file name receiving buffer */
	register FILE *ifp;		/* input stream */
{
	register char *ip;		/* include file name buffer pointer */
	register int c;			/* current character */

	SKIPWHITESPACE(c, ifp);
	for (ip = incname; (c = getc(ifp)) != EOF; ip++)
		if ((*ip = c) == '\n' || c == '\t' || c == ' ' || c == ';')
			{
			ungetc(c, ifp);
			break;
			}
	*ip = '\0';
	if ((*incname == '<' && ip[-1] != '>') ||
	    (*incname == '\"' && ip[-1] != '\"') ||
	    (*incname == '\'' && ip[-1] != '\'') ||
	    (*incname == '(' && ip[-1] != ')'))
		return(NO);
	if (*incname == '\'') *incname = ip[-1] = '\"';
	return(YES);
}



/*
 * inclink() stores a pointer to a hash table block (which contains
 * include file information) somewhere. Returns a pointer to the somewhere,
 * or calls exit(1) if out of memory.
 */
INCBLK *
inclink(htb)
	HASHBLK *htb;			/* hash table block pointer to save */
{
	char *malloc();			/* memory allocator */
	INCBLK *iblk;			/* pointer to new include chain block */

	if ((iblk = (INCBLK *) malloc(sizeof(INCBLK))) == NULL)
		{
		warn("out of memory");
		exit(1);
		}
	iblk->i_hblk = htb;
	iblk->i_next = NULL;
	return(iblk);
}



/*
 * instalinclude() adds an include file name to the appropriate include
 * file hash table. Returns a pointer to the hash table block, or calls
 * exit(1) if out of memory.
 */
HASHBLK *
instalinclude(incname, incpath, type)
	char *incname;			/* name of include file */
	char *incpath;			/* path to include file */
	int type;			/* type of source file */
{
	HASHBLK *htb;			/* hash table block */
	HASHBLK *htinstall();		/* install hash table entry */
	int ilen;			/* include path length */
	int strlen();			/* string length */

	ilen = strlen(incpath);
	switch (type)
		{
		case INCLUDE_C:
			htb = htinstall(incname, incpath, ilen, C_INCLUDETABLE);
			break;
		case INCLUDE_FORTRAN:
			htb = htinstall(incname, incpath, ilen, F_INCLUDETABLE);
			break;
		case INCLUDE_PASCAL:
			htb = htinstall(incname, incpath, ilen, P_INCLUDETABLE);
			break;
		}
	if (htb == NULL)
		exit(1);
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
	HASHBLK *htb;			/* hash table block */
	HASHBLK *htlookup();		/* find hash table entry */

	switch (type)
		{
		case INCLUDE_C:
			htb = htlookup(incname, C_INCLUDETABLE);
			break;
		case INCLUDE_FORTRAN:
			htb = htlookup(incname, F_INCLUDETABLE);
			break;
		case INCLUDE_PASCAL:
			htb = htlookup(incname, P_INCLUDETABLE);
			break;
		}
	return(htb);
}



/*
 * mkdepend() creates include file dependencies for object files and writes
 * them to stream ofp.
 */
void
mkdepend(ofp)
	FILE *ofp;			/* output stream */
{
	extern SLIST *SRCLIST;		/* source file name list */
	char *rindex();			/* find last occurrence of character */
	char *suffix;			/* suffix pointer */
	HASH *htinit();			/* initialize hash table */
	INCBLK *readC();		/* read C include-style files */
	INCBLK *readF();		/* read Fortran include-style files */
	int lookuptypeofinclude();	/* look up the brand of include */
	int slsort();			/* sort singly-linked list */
	int strcmp();			/* string comparison */
	SLBLK *lbp;			/* list block pointer */
	SLIST *slinit();		/* initialize singly-linked list */
	void addincdir();		/* add to list of include directories */
	void rmprinttag();		/* remove "already printed" tags */

	/* initialize include file hash tables */
	C_INCLUDETABLE = htinit(INCLUDETABLESIZE);
	F_INCLUDETABLE = htinit(INCLUDETABLESIZE);
	P_INCLUDETABLE = htinit(INCLUDETABLESIZE);

	/* initialize include file look-up lists */
	C_INCDIR = slinit();
	F_INCDIR = slinit();

	/* add additional include directories */
	addincdir();

	/* initialize external header file name list */
	EXTLIST = slinit();

	fprintf(ofp, "%s\n", DEPENDMARK);
	for (lbp = SRCLIST->head; lbp != NULL; lbp = lbp->next)
		{
		DEPSTAT = FIRSTLINE;	
		CURSRC = lbp->key;
		suffix = rindex(lbp->key, '.');
		switch (lookuptypeofinclude(++suffix))
			{
			case INCLUDE_C:
				readC(lbp->key, 0, lbp->key, INCLUDE_C, 1, ofp);
				break;
			case INCLUDE_FORTRAN:
				readF(lbp->key, 0, lbp->key, INCLUDE_FORTRAN, 1, ofp);
				break;
			case INCLUDE_PASCAL:
				readC(lbp->key, 0, lbp->key, INCLUDE_PASCAL, 1, ofp);
				break;
			case INCLUDE_NONE:
				break;
			}
		if (DEPSTAT == NEXTLINE)
			{
			fprintf(ofp, "\n");
			rmprinttag();
			}
		}
	if (slsort(strcmp, EXTLIST) == NO)
		exit(1);
}



/*
 * putinchain() outputs a chain of nested include file names. It changes
 * the sign of each chain block h_val field as it traverses the chain to
 * detect looping.
 */
void
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
			warn("\"%s\": unreasonable include nesting in \"%s\"",
			     iblk->i_hblk->h_def, htb->h_def);
			break;
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
void
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
void
putobjd(srcfile, ofp)
	char *srcfile;			/* source file name */
	FILE *ofp;			/* output stream */
{
	extern char OBJSFX[];		/* object file name suffix */
	int strlen();			/* string length */
	void putobj();			/* output object file name */

	COLUMN = strlen(srcfile) + strlen(OBJSFX) + 1;
	putobj(srcfile, ofp);
	fprintf(ofp, ":");
	DEPSTAT = NEXTLINE;
}



/*
 * readC() reads C files and prints include file dependencies for
 * object files. Returns a pointer to the chain of include files installed
 * or found in the include file hash table, or null if no include files
 * found.
 */
INCBLK *
readC(lastfile, lastline, curname, type, nest, ofp)
	char *lastfile;			/* parent file name */
	int lastline;			/* current line in parent file */
	char *curname;			/* current file name */
	int type;			/* file type */
	int nest;			/* readC nest level */
	FILE *ofp;			/* output stream */
{
	register char *p;		/* include string pointer */
	register FILE *ifp;		/* input file stream */
	register int c;			/* current character */
	char incname[PATHSIZE];		/* name of include file */
	char incpath[PATHSIZE];		/* path to include file */
	char *slappend();		/* append pathname to list */
	FILE *fopen();			/* open file */
	HASHBLK *htb = NULL;		/* hash table entry block */
	HASHBLK *instalinclude();	/* install include name in hash table */
	HASHBLK *lookupinclude();	/* look up include in hash table */
	INCBLK *i_head = NULL;		/* head of include chain */
	INCBLK *i_tail = NULL;		/* tail of include chain */
	INCBLK *inclink();		/* link include file hash blocks */
	int findinclude();		/* locate include file */
	int getinclude();		/* get include name from input line */
	int lineno = 1;			/* current line number */
	int strlen();			/* string length */
	void putinclude();		/* output include file pathname */
	void putinchain();		/* output nested include file names */
	void putobjd();			/* output object file name */

	if ((ifp = fopen(curname, "r")) == NULL)
		{
		if (lastline > 0)
			warn("\"%s\", line %d", lastfile, lastline);
		else
			warn("");
		perror(curname);
		return(NULL);
		}
	while ((c = getc(ifp)) != EOF)
		{
		if (c != '#')
			goto nextline;
		SKIPWHITESPACE(c, ifp);
		for (p = "include"; (c = getc(ifp)) == *p && *p != '\0' ; p++)
			continue;
		if (*p != '\0')
			goto nextline;
		if (getinclude(incname, ifp) == NO)
			{
			warn("\"%s\", line %d: %s bad include syntax",
			     curname, lineno, incname);
			goto nextline;
			}
		if ((htb = lookupinclude(incname, type)) == NULL)
			{
			switch (findinclude(incpath, incname, type))
				{
				case NOTFOUND:
					warn("\"%s\", line %d: can't find %s",
					     curname, lineno, incname);
					goto nextline;
					break;
				case EXTERNAL:
					if (slappend(incpath, EXTLIST) == NULL)
						exit(1);
					break;
				}
			htb = instalinclude(incname, incpath, type);

			/* look for nested include files */
			htb->h_sub = readC(curname, lineno, incpath, type, nest+1 , ofp);
			}
		if (nest == 1)
			{
			if (DEPSTAT == FIRSTLINE)
				putobjd(CURSRC, ofp);
			putinchain(htb, ofp);
			}
		else	{
			if (i_tail == NULL)
				i_head = i_tail = inclink(htb);
			else
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
 * readF() reads Fortran-style and prints include file dependencies
 * for object files. Returns a pointer to the chain of include files
 * installed or found in the include file hash table, or null if
 * no include files found.
 */
INCBLK *
readF(lastfile, lastline, curname, type, nest, ofp)
	char *lastfile;			/* parent file name */
	int lastline;			/* current line in parent file */
	char *curname;			/* current file name */
	int type;			/* file type */
	int nest;			/* readF nest level */
	FILE *ofp;			/* output stream */
{
	register char *p;		/* include string pointer */
	register FILE *ifp;		/* input file stream */
	register int c;			/* current character */
	char incname[PATHSIZE];		/* name of include file */
	char incpath[PATHSIZE];		/* path to include file */
	char *slappend();		/* append pathname to list */
	FILE *fopen();			/* open file */
	HASHBLK *htb = NULL;		/* hash table entry block */
	HASHBLK *instalinclude();	/* install include name in hash table */
	HASHBLK *lookupinclude();	/* look up include in hash table */
	INCBLK *i_head = NULL;		/* head of include chain */
	INCBLK *i_tail = NULL;		/* tail of include chain */
	INCBLK *inclink();		/* link include file hash blocks */
	int findinclude();		/* locate include file */
	int getinclude();		/* get include name from input line */
	int lineno = 1;			/* current line number */
	int strlen();			/* string length */
	void putinclude();		/* output include file pathname */
	void putinsub();		/* output nested subinclude file names*/
	void putobjd();			/* output object file name */

	if ((ifp = fopen(curname, "r")) == NULL)
		{
		if (lastline > 0)
			warn("\"%s\", line %d", lastfile, lastline);
		else
			warn("");
		perror(curname);
		return(NULL);
		}
	while ((c = getc(ifp)) != EOF)
		{
		if (c == 'c' || c == 'C' || c == '*' || c == '\n')
			goto nextline;
		while ((c = getc(ifp)) == ' ' || c == '\t')
			continue;
		for (p = "include"; *p == TOLOWER(c) && *p != '\0'; p++)
			c = getc(ifp);
		if (*p != '\0')
			goto nextline;
		if (getinclude(incname, ifp) == NO)
			{
			warn("\"%s\", line %d: %s bad include syntax",
			     curname, lineno, incname);
			goto nextline;
			}
		if ((htb = lookupinclude(incname, type)) == NULL)
			{
			switch (findinclude(incpath, incname, type))
				{
				case NOTFOUND:
					warn("\"%s\", line %d: can't find %s",
					     curname, lineno, incname);
					goto nextline;
					break;
				case EXTERNAL:
					if (slappend(incpath, EXTLIST) == NULL)
						exit(1);
					break;
				}
			htb = instalinclude(incname, incpath, type);

			/* look for nested include files */
			htb->h_sub = readF(curname, lineno, incpath, type, nest+1, ofp);
			}
		if (nest == 1)
			{
			if (DEPSTAT == FIRSTLINE)
				putobjd(CURSRC, ofp);
			putinchain(htb, ofp);
			}
		else	{
			if (i_tail == NULL)
				i_head = i_tail = inclink(htb);
			else
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
 * rmprinttag() removes the chain of tags indicating that an include
 * file dependency has already been printed for the current source file.
 */
void
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
