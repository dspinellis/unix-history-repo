/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)object.h	5.3 (Berkeley) %G%
 */

/*
 * Object module definitions.
 *
 * The object module is the interface to the object file; in particular
 * it contains the routines that read symbol and line number information.
 */

char *objname;			/* name of object file */
int objsize;			/* size of object code */

struct {
	unsigned int stringsize;	/* size of the dumped string table */
	unsigned int nsyms;		/* number of symbols */
	unsigned int nfiles;		/* number of files */
	unsigned int nlines;		/* number of lines */
} nlhdr;

char *stringtab;	/* string table */
char *dotpfile;		/* name of compiled file */

int readobj();		/* read in the object file */
int objfree();		/* release storage for object file information */
