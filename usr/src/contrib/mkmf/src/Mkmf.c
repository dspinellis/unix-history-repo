/*
 * Copyright (c) 1983, 1985, 1991 Peter J. Nicklin.
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
 * $Header: Mkmf.c,v 4.5 91/11/25 19:44:59 nicklin Exp $
 *
 * mkmf - makefile editor
 *
 * Author: Peter J. Nicklin
 */
#include "RELEASE.h"
#include "Mkmf.h"
#include "config.h"
#include "getarg.h"
#include "hash.h"
#include "null.h"
#include "path.h"
#include "target.h"
#include "slist.h"
#include "suffix.h"
#include "yesno.h"

char *MAKEFILE = "C";			/* default makefile template language */
char OBJSFX[SUFFIXSIZE] = ".o";		/* default object suffix */
char *PGN = "mkmf";			/* program name */
int AFLAG = NO;				/* list .source files? */
int CFLAG = YES;			/* makefile creation message flag */
int DEPEND = 1;				/* dependency analysis? */
int EFLAG = NO;				/* environ. overrides makefile macros? */
int FFLAG = NO;				/* user-specified template pathname? */
int LIBOBJ = 0;				/* load object file into library? */
int MKSYMLINK = 0;			/* make symbolic links to current dir? */
int SYSHDRS = NO;			/* search system header files? */
SLIST *HEADLIST;			/* header file name list */
SLIST *LIBLIST;				/* library pathname list */
SLIST *SRCLIST;				/* source file name list */
HASH *MDEFTABLE;			/* macro definition table */

char *DEFRULE[] =			/* default preprocessor rules */
	{
#include "defaultrul.h"
	NULL
	};

SUFFIX DEFSFX[] =			/* default suffix list */
	{
#include "defaultsfx.h"
	};

MAPINCLUDE INCKEY[] =			/* include style lookup table */
	{
#include "inckey.h"
	};

main(argc, argv)
	int argc;
	char **argv;
{
	char *mfname = NULL;		/* makefile name */
	char mfpath[PATHSIZE];		/* makefile template pathname */
	HASHBLK *htb;			/* hash table block */
	HASHBLK *htinstall();		/* install hash table entry */
	HASHBLK *htlookup();		/* find hash table entry */
	int buildliblist();		/* build list of library pathnames */
	int buildruletable();		/* build table of preprocessor rules */
	int buildsfxtable();		/* build table of suffixes */
	int buildsrclist();		/* build list of source file names */
	int findmf();			/* find makefile */
	int status = 0;			/* exit status */
	int storemacro();		/* store macro definition */
	short iflag = NO;		/* interactive flag */
	TARGET target;			/* type of makefile target */
	void answer();			/* install answer in macro def table */
	void editmf();			/* edit makefile */

	target.type = target.dest = VUNKNOWN;

	{
	register char *s;		/* option pointer */
	while (--argc > 0 && **++argv == '-')
		{
		for (s = argv[0]+1; *s != '\0'; s++)
			switch (*s)
				{
				case 'F':
					MAKEFILE = GETARG(s);
					if (MAKEFILE==NULL || *MAKEFILE=='\0')
						{
						warn("missing template name");
						status = 1;
						}
					FFLAG = YES;
					goto endfor;
				case 'L':
					LIBOBJ = 1;
					target.type = VLIBRARY;
					break;
				case 'M':
					MAKEFILE = GETARG(s);
					if (MAKEFILE==NULL || *MAKEFILE=='\0')
						{
						warn("missing template name");
						status = 1;
						}
					goto endfor;
				case 'S':
					MKSYMLINK++;
					break;
				case 'a':
					AFLAG = YES;
					break;
				case 'c':
					CFLAG = NO;
					break;
				case 'd':
					/* turn OFF dependency analysis */
					DEPEND = 0;
					break;
				case 'e':
					EFLAG = YES;
					break;
				case 'f':
					mfname = GETARG(s);
					if (mfname == NULL || *mfname == '\0')
						{
						warn("missing makefile name");
						status = 1;
						}
					goto endfor;
				case 'i':
					iflag = YES;
					break;
				case 'l':
					target.type = VLIBRARY;
					break;
				default:
					badopt(**argv, *s);
					status = 1;
					goto endfor;
				}
		endfor: continue;
		}
	}
	
	/* initialize macro definition table */
	MDEFTABLE = htinit(MDEFTABLESIZE);

	/* get command line macro definitions */
	for (; argc > 0; argc--, argv++)
		if (storemacro(*argv) == NO)
			{
			warns("%s not a macro definition", *argv);
			status = 1;
			}

	if (status == 1)
		{
		usage("[-acdeilS] [-f makefile] [-F template] [-M language]\n       [macroname=value...]");
		exit(1);
		}

	/*
	 * store macro definition placeholders that will be generated later
	 * unless specified on the command line
	 */
	if (storedynmacro() == NO)
		exit(1);


	/* environment variables override makefile macro definitions */
	if (EFLAG == YES)
		{
		if (storenvmacro() == NO)
			exit(1);
		}


	/* determine the makefile name */
	if (mfname == NULL)
		if ((htb = htlookup(MMAKEFILE, MDEFTABLE)) != NULL)
			mfname = htb->h_def;
		else if (FILEXIST("makefile"))
			mfname = "makefile";
		else if (FILEXIST("Makefile"))
			mfname = "Makefile";
		else
			mfname = "Makefile";
	if (htinstall(MMAKEFILE, mfname, VREADWRITE, MDEFTABLE) == NULL)
		exit(1);


	/* find the makefile (template) and load useful macro definitions */
	if (target.type == VUNKNOWN)
		{
		if (htlookup(MPROGRAM, MDEFTABLE) != NULL)
			target.type = VPROGRAM;
		else if (htlookup(MLIBRARY, MDEFTABLE) != NULL)
			target.type = VLIBRARY;
		}
	if (findmf(mfname, mfpath, &target) == NO)
		exit(1);
	
	/* search system header files? */
	if ((htb = htlookup(MSYSHDRS, MDEFTABLE)) != NULL)
		SYSHDRS = YES;

	/* interactive option */
	if (iflag == YES)
		{
		if (htlookup(MPROGRAM, MDEFTABLE) == NULL &&
		    htlookup(MLIBRARY, MDEFTABLE) == NULL)
			if (target.type == VPROGRAM)
				{
				printf("program name? ");
				answer(MPROGRAM, VREADWRITE);
				}
			else if (target.type == VLIBRARY)
				{
				printf("library name? ");
				answer(MLIBRARY, VREADWRITE);
				}
		if (htlookup(MDESTDIR, MDEFTABLE) == NULL && target.dest == VDESTDIR)
			{
			printf("destination directory? ");
			answer(MDESTDIR, VREADWRITE);
			}
		}

	/* load environment variables into macro definitions */
	if (EFLAG == NO)
		{
		if (storenvmacro() == NO)
			exit(1);
		}

	/* build the suffix table */
	if (buildsfxtable() == NO)
		exit(1);

	/* build the rule table */
	if (buildruletable() == NO)
		exit(1);

	/* build the source code and header file name lists */
	if (buildsrclist() == NO)
		exit(1);

	/* build the library pathname list */
	if (buildliblist() == NO)
		exit(1);

	/* edit makefile */
	editmf(mfname, mfpath);

	exit(0);
}
