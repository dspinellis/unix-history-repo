
/* basic typedefs & defines */
/* $Header: imPdefs.h,v 10.2 86/02/01 16:00:23 tony Rel $ */

typedef short		I16;
typedef	short		BDIM;
typedef	char		SDIM;
typedef	unsigned short	DISP;
typedef unsigned long	LONGDISP;
typedef unsigned short	RFC;
typedef	unsigned short	MSKWORD;
typedef unsigned short	GLINK;
typedef unsigned short	SYSCNV;
typedef unsigned char	BYTE;

#define	MSKSHFT		4
#define	MSKHIGH		15
#define	NILPTR		0
#define OOPTR		((char *)0xFFFFFFFF)
#define	NILIDX		-1
#define	TRUE		1
#define	FALSE		0

#define	not	~
#define	REG	register

#define	ADVPTR(p,n)	((char *)p+n)	/* advances pointer 'p' 'n' bytes */

#define	MLIWSIZ(w)	((w+15)>>4)
#define	MLIBSIZ(w)	((w+7)>>3)
#define	MSKWSIZ(w,h)	(MLIWSIZ(w)*h)
#define	SMWSIZ(p)	MSKWSIZ(p->sgw,p->sgh)
#define	BMWSIZ(p)	MSKWSIZ(p->bgw,p->bgh)
#define GVWSIZ(t,p)	(t==SMALL ? SGDWSIZ+SMWSIZ(((sgdims *)(p))) : \
				BGDWSIZ+BMWSIZ(((bgdims *)(p)))	\
			)
#define	GLYWSIZ(p)	(GHWSIZ+GVWSIZ(p->gsize,(char *)(p)+GHBSIZ))
#define	GPRECOMP(r,f)	(((r<<14)|(f<<7)) % GHTLEN) /* not used by imPRESS */
#define	GPREHASH(r,f)	gprehtab[(r<<7)|f]
#define	GHASHIX(rfc)	gprehtab[(rfc>>7)&0777] + (rfc&0177)

#define	FNAMLEN	8

typedef	union {			/******** DECODE PARAMETERS ***********/
	BDIM    pval;		/* parameter value		      */
	struct {
#ifdef XWIND
		char	loval;
		char	hival;
#else
		char	hival;
		char	loval;
#endif
	}	
	pval2;
	char    *pptr;		/* parameter address (w/input area)   */
} parva;			/**************************************/
parva	param[10];
#define	V(i)	(param[i].pval)
#define	v(i)	(param[i].pval2.loval)
#define P(i)	(param[i].pptr)
/* extend sign for an int */
#define EXTSIGN(n)	if (v(n)<0) param[n].pval2.hival = 0377
