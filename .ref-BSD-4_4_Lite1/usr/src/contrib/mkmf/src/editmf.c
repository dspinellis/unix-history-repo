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
 * $Header: editmf.c,v 4.6 93/05/25 21:49:09 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */
#include <ctype.h>
#include <signal.h>
#include <stdio.h>
#include "Mkmf.h"
#include "config.h"
#include "dlist.h"
#include "hash.h"
#include "macro.h"
#include "null.h"
#include "slist.h"
#include "yesno.h"

static char Mftemp[] = "mkmfXXXXXX";		/* temporary makefile */

/*
 * editmf() replaces macro definitions within a makefile.
 */
void
editmf(mfname, mfpath)
	char *mfname;			/* makefile name */
	char *mfpath;			/* makefile template pathname */
{
	extern char IOBUF[];		/* I/O buffer line */
	extern int DEPEND;		/* dependency analysis? */
	extern SLIST *EXTLIST;		/* external header file name list */
	extern SLIST *HEADLIST;		/* header file name list */
	extern SLIST *LIBLIST;		/* library pathname list */
	extern SLIST *SRCLIST;		/* source file name list */
	extern SLIST *SYSLIST;		/* system header file name list */
	extern HASH *MDEFTABLE;		/* macro definition table */
	char *findmacro();		/* is the line a macro definition? */
	char *getlin();			/* get a line from input stream */
	char *mktemp();			/* make file name */
	char mnam[MACRONAMSIZE];	/* macro name buffer */
	DLIST *dlp;			/* dependency list */
	DLIST *mkdepend();		/* generate object-include file deps */
	FILE *ifp;			/* input stream */
	FILE *mustfopen();		/* must open file or die */
	FILE *ofp;			/* output stream */
	HASHBLK *htb;			/* hash table block */
	HASHBLK *htlookup();		/* find hash table entry */
	void cleanup();			/* remove temporary makefile and exit */
	void dlprint();			/* print dependency list */
	void purgcontinue();		/* get rid of continuation lines */
	void putmacro();		/* put macro defs from table */
	void putlin();			/* put a makefile line */
	void putobjmacro();		/* put object file name macro def */
	void putslmacro();		/* put macro defs from linked list */

	ifp = mustfopen(mfpath, "r");
	mktemp("Mftemp");

	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		{
		signal(SIGINT, cleanup);
		signal(SIGHUP, cleanup);
		signal(SIGQUIT, cleanup);
		}

	ofp = mustfopen(Mftemp, "w");
	if (DEPEND)
		{
		dlp = mkdepend();
		}

	while (getlin(ifp) != NULL)
		{
		if (DEPEND && EQUAL(IOBUF, DEPENDMARK))
			break;
		if (findmacro(mnam, IOBUF) != NULL)
			{
			if (EQUAL(mnam, MHEADERS))
				{
				putslmacro(HEADLIST, ofp);
				purgcontinue(ifp);
				}
			else if (EQUAL(mnam, MOBJECTS))
				{
				putobjmacro(ofp);
				purgcontinue(ifp);
				}
			else if (EQUAL(mnam, MSOURCES))
				{
				putslmacro(SRCLIST, ofp);
				purgcontinue(ifp);
				}
			else if (EQUAL(mnam, MSYSHDRS))
				{
				putslmacro(SYSLIST, ofp);
				purgcontinue(ifp);
				}
			else if (EQUAL(mnam, MEXTERNALS))
				{
				if (DEPEND)
					{
					putslmacro(EXTLIST, ofp);
					purgcontinue(ifp);
					}
				else	{
					putlin(ofp);
					}
				}
			else if (EQUAL(mnam, MLIBLIST) && LIBLIST != NULL)
				{
				putslmacro(LIBLIST, ofp);
				purgcontinue(ifp);
				}
			else if ((htb = htlookup(mnam, MDEFTABLE)) != NULL)
				{
				if (htb->h_val == VREADWRITE)
					{
					putmacro(htb->h_def, ofp);
					purgcontinue(ifp);
					}
				else	{
					putlin(ofp);
					}
				}
			else	{
				putlin(ofp);
				}
			}
		else	{
			putlin(ofp);
			}
		}
	fclose(ifp);
	if (DEPEND)
		{
		dlprint(dlp, ofp);
		}
	fclose(ofp);

	signal(SIGINT, SIG_IGN);
	signal(SIGHUP, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);

	RENAME(Mftemp, mfname);
}



/*
 * cleanup() removes the temporary makefile and dependency file, and
 * calls exit(1).
 */
void
cleanup(sig)
	int sig;			/* value of signal causing cleanup */
{
	signal(SIGINT, cleanup);
	signal(SIGHUP, cleanup);
	signal(SIGQUIT, cleanup);

	unlink(Mftemp);
	exit(1);
}
