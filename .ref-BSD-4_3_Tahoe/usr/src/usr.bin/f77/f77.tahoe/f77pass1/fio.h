/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)fio.h	5.1 (Berkeley) 6/7/85
 */

#include <stdio.h>
typedef long ftnint;
typedef ftnint flag;
typedef long ftnlen;
/*external read, write*/
typedef struct
{	flag cierr;
	ftnint ciunit;
	flag ciend;
	char *cifmt;
	ftnint cirec;
} cilist;
/*internal read, write*/
typedef struct
{	flag icierr;
	char *iciunit;
	flag iciend;
	char *icifmt;
	ftnint icirlen;
	ftnint icirnum;
} icilist;
/*open*/
typedef struct
{	flag oerr;
	ftnint ounit;
	char *ofnm;
	ftnlen ofnmlen;
	char *osta;
	char *oacc;
	char *ofm;
	ftnint orl;
	char *oblnk;
} olist;
/*close*/
typedef struct
{	flag cerr;
	ftnint cunit;
	char *csta;
} cllist;
/*rewind, backspace, endfile*/
typedef struct
{	flag aerr;
	ftnint aunit;
} alist;
/*units*/
typedef struct
{	FILE *ufd;	/*0=unconnected*/
	char *ufnm;
	long uinode;
	int url;	/*0=sequential*/
	flag useek;	/*true=can backspace, use dir, ...*/
	flag ufmt;
	flag uprnt;
	flag ublnk;
	flag uend;
	flag uwrt;	/*last io was write*/
	flag uscrtch;
} unit;
typedef struct
{	flag inerr;
	ftnint inunit;
	char *infile;
	ftnlen infilen;
	ftnint	*inex;	/*parameters in standard's order*/
	ftnint	*inopen;
	ftnint	*innum;
	ftnint	*innamed;
	char	*inname;
	ftnlen	innamlen;
	char	*inacc;
	ftnlen	inacclen;
	char	*inseq;
	ftnlen	inseqlen;
	char 	*indir;
	ftnlen	indirlen;
	char	*infmt;
	ftnlen	infmtlen;
	char	*inform;
	ftnint	informlen;
	char	*inunf;
	ftnlen	inunflen;
	ftnint	*inrecl;
	ftnint	*innrec;
	char	*inblank;
	ftnlen	inblanklen;
} inlist;

extern int errno;
extern flag init;
extern cilist *elist;	/*active external io list*/
extern flag reading,external,sequential,formatted;
extern int (*getn)(),(*putn)();	/*for formatted io*/
extern FILE *cf;	/*current file*/
extern unit *curunit;	/*current unit*/
extern unit units[];
#define err(f,n,s) {if(f) errno= n; else fatal(n,s); return(n);}

/*Table sizes*/
#define MXUNIT 10

extern int recpos;	/*position in current record*/
