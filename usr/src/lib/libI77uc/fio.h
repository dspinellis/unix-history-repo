/*
char id_fio[] = "@(#)fio.h	1.1";
 *
 * f77 file i/o common definitions
 */

#include "fiodefs.h"

#define err(f,n,s)	{if(f) return(errno=n); else fatal(n,s);}
#define not_legal(u)	(u>=MXUNIT || u<0)
#define GET(x)		if((x=(*getn)())<0) return(x)
#define VAL(x)		(x!='\n'?x:' ')
#define PUT(x)		{if(n=(*putn)(x)) return(n);}
#define lcase(s)	((s >= 'A') && (s <= 'Z') ? s+('a'-'A') : s)

#define MAXINTLENGTH	32	/* to accomodate binary format */

long ftell();

extern int errno;
extern ioflag init;
extern icilist *svic;	/* active internal io list */
extern flag reading,external,sequential,formatted;
extern int (*getn)(),(*putn)(),(*ungetn)();	/*for formatted io*/
extern FILE *cf;	/*current file structure*/
extern unit *curunit;	/*current unit structure */
extern int lunit;	/*current logical unit*/
extern char *lfname;	/*current filename*/
extern unit units[];	/*logical units table*/
extern int recpos;		/*position in current record*/
extern ftnint recnum;		/*current record number*/
extern int reclen;		/* current record length */
extern int (*doed)(), (*doned)();
extern int (*dorevert)(), (*donewrec)(), (*doend)(), (*dotab)();
extern ioflag cblank, cplus, tab, elist, signit, errflag, endflag;
extern char *fmtbuf, *icptr, *icend, *fmtptr;
extern int scale;
extern int cursor;
extern int radix;
