/*
 * fortran file i/o type definitions
 */

#include <stdio.h>

/* Logical Unit Table Size */
#define MXUNIT _NFILE

#define GLITCH '\2'	/* special quote for Stu, generated in f77pass1 */

#define LISTDIRECTED  -1
#define FORMATTED      1

#define ERROR	1
#define OK	0
#define YES	1
#define NO	0

#define STDERR	0
#define STDIN	5
#define STDOUT	6

#define WRITE	1
#define READ	2
#define SEQ	3
#define DIR	4
#define FMT	5
#define UNF	6
#define EXT	7
#define INT	8

typedef char ioflag;
typedef long ftnint;
typedef ftnint flag;
typedef long ftnlen;

typedef struct		/*external read, write*/
{	flag cierr;
	ftnint ciunit;
	flag ciend;
	char *cifmt;
	ftnint cirec;
} cilist;

typedef struct		/*internal read, write*/
{	flag icierr;
	char *iciunit;
	flag iciend;
	char *icifmt;
	ftnint icirlen;
	ftnint icirnum;
	ftnint icirec;
} icilist;

typedef struct		/*open*/
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

typedef struct		/*close*/
{	flag cerr;
	ftnint cunit;
	char *csta;
} cllist;

typedef struct		/*rewind, backspace, endfile*/
{	flag aerr;
	ftnint aunit;
} alist;

typedef struct		/*units*/
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

typedef struct		/* inquire */
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
	char	*inform;
	ftnlen	informlen;
	char	*infmt;
	ftnint	infmtlen;
	char	*inunf;
	ftnlen	inunflen;
	ftnint	*inrecl;
	ftnint	*innrec;
	char	*inblank;
	ftnlen	inblanklen;
} inlist;

typedef union
{	float pf;
	double pd;
} ufloat;

typedef union
{	short is;
	char ic;
	long il;
} uint;

