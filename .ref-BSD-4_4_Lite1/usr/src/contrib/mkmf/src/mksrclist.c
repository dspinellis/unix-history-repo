/*
 * Copyright (c) 1991 Peter J. Nicklin.
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
 * $Header: mksrclist.c,v 4.8 91/11/25 19:44:59 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */
#include <sys/types.h>
#include <sys/stat.h>
#include "Mkmf.h"
#include "hash.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "slist.h"
#include "stringx.h"
#include "yesno.h"

extern SLIST *SRCLIST;			/* source file name list */
extern SLIST *HEADLIST;			/* header file name list */
extern HASH *MDEFTABLE;			/* macro definition table */

static int addsrctable();		/* add file to src hash table / list */
static int addvsrctable();		/* add virtual file to hash table */
static int  hashtolist();		/* convert hash table file list */
static HASH *SRCTABLE = NULL;		/* source file hash table */

static struct stat CURDIRSTAT;		/* current directory status */

/*
 * mksrclist() composes a list of source and header files names
 * from the current directory and directories listed in VPATH.
 * Return YES if successful, otherwise NO.
 */
mksrclist(needsrc, needhdr)
	int needsrc;			/* need source file names */
	int needhdr;			/* need header file names */
{
	char *getpath();		/* get next path */
	char *vp;			/* virtual path buffer pointer */
	char vpath[PATHSIZE];		/* virtual directory path buffer */
	int addsrclist();		/* add file to header list */
	int read_dir();			/* read dir for source and headers */
	struct stat statbuf;		/* virtual path stat buffer */

	/*
	 * save inode info for current directory so that we can make sure
	 * that directories in VPATH do not include the current directory.
	 */
	if (stat(CURDIR, &CURDIRSTAT) < 0)
		{
		pperror(CURDIR);
		return(NO);
		}

	/*
	 * if VPATH exists, merge the source file names from the
	 * directories in VPATH with the source files in the current
	 * directory, otherwise build source list directly.
	 */
	if (htlookup(MVPATH, MDEFTABLE) != NULL)
		{
		vp = htdef(MDEFTABLE);		

		if ((SRCTABLE = htinit(SOURCETABLESIZE)) == NULL)
			return(NO);
	
		if (read_dir(CURDIR, addsrctable, needsrc, needhdr) == NO)
			return(NO);

		while ((vp = getpath(vpath, vp)) != NULL)
			{
			if (stat(vpath, &statbuf) < 0)
				{
				pperror(vpath);
				continue;
				}

			if (!S_ISDIR(statbuf.st_mode))
				{
				warns("(warning) %s in VPATH not a directory", vpath);
				continue;
				}

			if (CURDIRSTAT.st_dev == statbuf.st_dev &&
			    CURDIRSTAT.st_ino == statbuf.st_ino)
				{
				warns("(warning) current directory %s included in VPATH", vpath);
				continue;
				}

			if (read_dir(vpath, addvsrctable, needsrc, needhdr) == NO)
				return(NO);
			}

		/*
		 * convert hash table source file entries to
		 * singly-linked source list
		 */
		if (hashtolist(SRCTABLE, SRCLIST) == NO)
			{
			htrm(NULL, SRCTABLE);
			return(NO);
			}
		htrm(NULL, SRCTABLE);
		}
	else	{
		if (read_dir(CURDIR, addsrclist, needsrc, needhdr) == NO)
			return(NO);
		}
	return(YES);
}



/*
 * addsrclist() adds a file to a source/header file list. Returns YES if
 * successful, otherwise NO.
 */
addsrclist(dirname, filename, lswitch)
	char *dirname;			/* directory name */
	char *filename;			/* file name to add to source list */
	int lswitch;			/* source/header list switch */
{
	if (lswitch == 's')
		return((slappend(filename, SRCLIST) != NULL) ? YES : NO);
	else
		return((slappend(filename, HEADLIST) != NULL) ? YES : NO);
}



/*
 * addsrctable() adds a file to a source file hash table or a header file
 * singly-linked list. Returns YES if successful, otherwise NO.
 */
static int
addsrctable(dirname, filename, tswitch)
	char *dirname;			/* directory name */
	char *filename;			/* file name to add to source table */
	int tswitch;			/* source/header switch */
{
	if (tswitch == 's')
		{
		if (htlookup(filename, SRCTABLE) != NULL)
			{
			warn2("(warning) duplicate file %s/%s ignored",
			      dirname, filename);
			}
		else	{
			if (htinstall(filename, dirname, 0, SRCTABLE) == NULL)
				return(NO);
			}
		}	  
	else	{
		if (slappend(filename, HEADLIST) == NULL)
			return(NO);
		}
	return(YES);
}



/*
 * addvsrctable() adds a file to a source file hash table. Returns
 * YES if successful, otherwise NO.
 */
static int
addvsrctable(dirname, filename, tswitch)
	char *dirname;			/* directory name */
	char *filename;			/* file name to add to source table */
	int tswitch;			/* source/header table switch */
{
	HASH *table;			/* pointer to source/header file table */

	if (tswitch == 's')
		{
		if (htlookup(filename, SRCTABLE) != NULL)
			{
			warn2("(warning) duplicate file %s/%s ignored",
			      dirname, filename);
			}
		else	{
			if (htinstall(filename, dirname, 0, SRCTABLE) == NULL)
				return(NO);
			}
		}
	return(YES);
}



/*
 * hashtolist() converts the file entries stored in a hash table to a
 * singly-linked list of pathnames. Returns YES if successful, otherwise
 * NO.
 */
static int
hashtolist(table, list)
	HASH *table;			/* hash table */
        SLIST *list;			/* pointer to list head block */
{
	char path[PATHSIZE];		/* path to foreign source file */

	htrewind(table);
	while (htnext(table))
		{
		if (htdef(table) == NULL || EQUAL(CURDIR, htdef(table)))
			{		/* current directory */
			if (slappend(htkey(table), list) == NULL)
				return(NO);
			}
		else	{		/* VPATH directory */
			pathcat(path, htdef(table), htkey(table));
			if (slappend(path, list) == NULL)
				return(NO);
			}
		}
	return(YES);
}
