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
 * $Header: mksymlink.c,v 4.10 91/11/25 19:44:59 nicklin Exp $
 *
 * Author: Peter J. Nicklin
 */
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "Mkmf.h"
#include "config.h"
#include "hash.h"
#include "macro.h"
#include "null.h"
#include "path.h"
#include "slist.h"
#include "yesno.h"

#ifdef _HasSymLinks

#define NOT_SYMLINK	1		/* not a symbolic link */
#define UNV_SYMLINK	2		/* unverified symbolic link */
#define VER_SYMLINK	3		/* verified symbolic link */

extern int errno;			/* error indicator for system calls */
extern SLIST *SRCLIST;			/* source file name list */
extern SLIST *HEADLIST;			/* header file name list */
extern HASH *MDEFTABLE;			/* macro definition table */

static int addsrctable();		/* add file to source hash table */
static int addvsrctable();		/* add virtual file to hash table */
static int  hashtolist();		/* convert hash table file list */
static HASH *SRCTABLE = NULL;		/* source file hash table */
static HASH *HDRTABLE = NULL;		/* header file hash table */

static struct stat CURDIRSTAT;		/* current directory status */

/*
 * mksymlink() creates symbolic links from foreign directories into
 * the current directory. Return YES if successful, otherwise NO.
 *
 * Given the following state of the current directory and a virtual path:
 *
 * ./a.c -----> dangling reference
 * ./b.c        ordinary file
 * ./c.c ----->	a/c.c
 *		a/d.c
 * ./d.c ----->	b/d.c
 * ./e.c ----->	b/e.c
 * ./f.c ----->	c/f.c
 *
 * VPATH = a b
 *
 * mksymlink() needs to delete a.c, shift the link for d.c from directory b
 * to directory a, and issue a warning that f.c is linked to a file in an
 * external directory.
 */
mksymlink(needsrc, needhdr)
	int needsrc;			/* need source file names */
	int needhdr;			/* need header file names */
{
	char *getpath();		/* get next path */
	char *vp;			/* virtual path buffer pointer */
	char vpath[PATHSIZE];		/* virtual directory path buffer */
	int read_dir();			/* read dir for source and headers */
	struct stat statbuf;		/* virtual path stat buffer */

	if ((HDRTABLE = htinit(SOURCETABLESIZE)) == NULL)
		return(NO);
	if ((SRCTABLE = htinit(SOURCETABLESIZE)) == NULL)
		return(NO);

	if (stat(CURDIR, &CURDIRSTAT) < 0)
		{
		pperror(CURDIR);
		return(NO);
		}

	if (read_dir(CURDIR, addsrctable, needsrc, needhdr) == NO)
		return(NO);

	/*
	 * merge source/header files in other directories into hash table
	 * source list
	 */
	if (htlookup(MVPATH, MDEFTABLE) != NULL)
		{
		vp = htdef(MDEFTABLE);		
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
		}

	/*
	 * convert hash table source file entries to singly-linked source list
	 */
	if (hashtolist(SRCTABLE, SRCLIST) == NO)
		{
		htrm(NULL, SRCTABLE);
		return(NO);
		}
	htrm(NULL, SRCTABLE);

	/*
	 * convert hash table header file entries to singly-linked header list
	 */
	if (hashtolist(HDRTABLE, HEADLIST) == NO)
		{
		htrm(NULL, HDRTABLE);
		return(NO);
		}
	htrm(NULL, HDRTABLE);

	return(YES);
}



/*
 * addsrctable() adds a file to a source/header file hash table. Returns
 * YES if successful, otherwise NO.
 */
static int
addsrctable(dirname, filename, tswitch)
	char *dirname;			/* directory name */
	char *filename;			/* file name to add to source table */
	int tswitch;			/* source/header table switch */
{
	char symbuf[PATHSIZE];		/* symbolic link pathname buffer */

if (readlink(filename, symbuf, PATHSIZE) < 0)
		{
#ifdef _HasEnxioReadlinkReturn		/* ugly hack around apollo bug */
		if (errno == EINVAL || errno == ENXIO)
#else
                if (errno == EINVAL)
#endif
			{
			if (htinstall(filename, NULL, NOT_SYMLINK,
			   (tswitch == 's') ? SRCTABLE : HDRTABLE) == NULL)
				return(NO);
			}
		else	{
			pperror(filename);
			return(NO);
			}
		}
	else	{
		if (htinstall(filename, pathhead(symbuf), UNV_SYMLINK,
		   (tswitch == 's') ? SRCTABLE : HDRTABLE) == NULL)
			return(NO);
		}
	return(YES);
}



/*
 * addvsrctable() merges source/header files from directories in the virtual
 * path into a source or header file hash table. If a file in the current
 * directory is linked to a file in a yet unseen directory, then the link
 * is broken and reestabished to a file in the current virtual directory.
 * Returns YES if successful, otherwise NO.
 */
static int
addvsrctable(dirname, filename, tswitch)
	char *dirname;			/* directory name */
	char *filename;			/* file name to add to source table */
	int tswitch;			/* source/header table switch */
{
	char path[PATHSIZE];		/* path to foreign source file */
	HASH *table;			/* pointer to source/header file table */

	table = (tswitch == 's') ? SRCTABLE : HDRTABLE;
	if (htlookup(filename, table) != NULL)
		{
		if (htval(table) == UNV_SYMLINK)
			{
			if (!EQUAL(dirname, htdef(table)))
				{	/* link file from dirname */
				if(unlink(filename) < 0)
					{
					warns("(error) failed to remove symbolic link %s", filename);
					return(NO);
					}
				else if (symlink(pathcat(path, dirname, filename), filename) < 0)
					{
					pperror(filename);
					return(NO);
					}
				else	{
					char oldpath[PATHSIZE];	/* extra pathname buffer */
					pathcat(oldpath, htdef(table), filename);
					warn2("(warning) symbolic link %s replaced by %s", oldpath, path);
					}
				}
			htval(table) = VER_SYMLINK;
			}
		else	{
			warn2("(warning) duplicate file %s/%s ignored", htdef(table), filename);
			}
		}
	else	{
		if (htinstall(filename, NULL, VER_SYMLINK, table) == NULL)
			return(NO);
		pathcat(path, dirname, filename);
		if (symlink(path, filename) < 0)
			{
			pperror(filename);
			return(NO);
			}
		}
	return(YES);
}



/*
 * hashtolist() converts the file entries stored in a hash table to a
 * singly-linked list. Also removes dangling symbolic links, and complains
 * about rogue links that are not in VPATH. Returns YES if successful,
 * otherwise NO.
 */
static int
hashtolist(table, list)
	HASH *table;			/* hash table */
        SLIST *list;			/* pointer to list head block */
{
        extern int MKSYMLINK;           /* clobber rogue links if > 1 */

	htrewind(table);
	while (htnext(table))
		{
		if (htval(table) == UNV_SYMLINK)
			{
			if (MKSYMLINK > 1 || !FILEXIST(htkey(table)))
				{
				if(unlink(htkey(table)) < 0)
					{
					warns("Failed to remove symbolic link %s", htkey(table));
					}
				}
			else	{
				warns("%s linked to directory not in VPATH", htkey(table));
				if (slappend(htkey(table), list) == NULL)
					return(NO);
				}
			}
		else 	{
			if (slappend(htkey(table), list) == NULL)
				return(NO);
			}
		}
	return(YES);
}

#else

/*
 * mksymlink() dummy routine always returns NO.
 */
mksymlink(needsrc, needhdr)
	int needsrc;			/* need source file names */
	int needhdr;			/* need header file names */
{
	warn("symbolic links not available");
	return(NO);
}

#endif /* _HasSymLinks */
