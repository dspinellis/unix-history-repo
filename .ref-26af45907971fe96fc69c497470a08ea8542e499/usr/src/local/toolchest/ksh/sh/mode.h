/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)mode.h	1.1 */
/*
 *	UNIX shell
 *	S. R. Bourne
 *	Rewritten by David Korn
 *
 */



/* command tree for tretyp */
#define COMBITS	4
#define COMMSK	((1<<COMBITS)-1)
#define COMSCAN	(01<<COMBITS)
#define FPRS	(01<<COMBITS)
#define FINT	(02<<COMBITS)
#define FAMP	(04<<COMBITS)
#define FTMP	(010<<COMBITS)
#define FPIN	(020<<COMBITS)
#define FPOU	(040<<COMBITS)
#define FPCL	(0100<<COMBITS)
#define FCMD	(0200<<COMBITS)
#define	FCOMSUB	(0400<<COMBITS)

#define TCOM	0
#define TPAR	1
#define TFIL	2
#define TLST	3
#define TIF	4
#define TWH	5
#define TUN	6
#define TSW	7
#define TAND	8
#define TORF	9
#define TFORK	10
#define TFOR	11
#define TSELECT	12
#define	TTIME	13
#define TSETIO	14
#define TPROC	15


/* heap storage */
/* this node is a proforma for those that follow */
struct trenod
{
	int	tretyp;
	IOPTR	treio;
};


struct dolnod
{
	DOLPTR	dolnxt;
	int	doluse;
	char	*dolarg[1];
};

struct forknod
{
	int	forktyp;
	IOPTR	forkio;
	TREPTR	forktre;
};

struct comnod
{
	int	comtyp;
	IOPTR	comio;
	ARGPTR	comarg;
	ARGPTR	comset;
	int	comline;
};

struct ifnod
{
	int	iftyp;
	TREPTR	iftre;
	TREPTR	thtre;
	TREPTR	eltre;
};

struct whnod
{
	int	whtyp;
	TREPTR	whtre;
	TREPTR	dotre;
};

struct fornod
{
	int	fortyp;
	TREPTR	fortre;
	char *fornam;
	COMPTR	forlst;
};

struct swnod
{
	int	swtyp;
	char *swarg;
	REGPTR	swlst;
};

struct regnod
{
	ARGPTR	regptr;
	TREPTR	regcom;
	REGPTR	regnxt;
};

struct parnod
{
	int	partyp;
	TREPTR	partre;
};

struct lstnod
{
	int	lsttyp;
	TREPTR	lstlef;
	TREPTR	lstrit;
};


struct procnod
{
	int	proctyp;
	BLKPTR	proctre;
	char	*procnam;
	long	procloc;
};


#define	FORKTYPE	(sizeof(struct forknod))
#define	COMTYPE		(sizeof(struct comnod))
#define	IFTYPE		(sizeof(struct ifnod))
#define	WHTYPE		(sizeof(struct whnod))
#define	FORTYPE		(sizeof(struct fornod))
#define	SWTYPE		(sizeof(struct swnod))
#define	REGTYPE		(sizeof(struct regnod))
#define	PARTYPE		(sizeof(struct parnod))
#define	LSTTYPE		(sizeof(struct lstnod))
#define DOLTYPE		(sizeof(struct dolnod))
#define	PROCTYPE	(sizeof(struct procnod))
