/*
 * Copyright (c) 1987, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ctags.h	8.3 (Berkeley) %G%
 */

#define	bool	char

#define	YES		1
#define	NO		0
#define	EOS		'\0'

#define	ENDLINE		50		/* max length of pattern */
#define	MAXTOKEN	250		/* max size of single token */

#define	SETLINE		{++lineno;lineftell = ftell(inf);}
#define	GETC(op,exp)	((c = getc(inf)) op (int)exp)

#define	iswhite(arg)	(_wht[(unsigned)arg])	/* T if char is white */
#define	begtoken(arg)	(_btk[(unsigned)arg])	/* T if char can start token */
#define	intoken(arg)	(_itk[(unsigned)arg])	/* T if char can be in token */
#define	endtoken(arg)	(_etk[(unsigned)arg])	/* T if char ends tokens */
#define	isgood(arg)	(_gd[(unsigned)arg])	/* T if char can be after ')' */

typedef struct nd_st {			/* sorting structure */
	struct nd_st	*left,
			*right;		/* left and right sons */
	char	*entry,			/* function or type name */
		*file,			/* file name */
		*pat;			/* search pattern */
	int	lno;			/* for -x option */
	bool	been_warned;		/* set if noticed dup */
} NODE;

extern char	*curfile;		/* current input file name */
extern NODE	*head;			/* head of the sorted binary tree */
extern FILE    *inf;			/* ioptr for current input file */
extern FILE    *outf;			/* ioptr for current output file */
extern long	lineftell;		/* ftell after getc( inf ) == '\n' */
extern int	lineno;			/* line number of current line */
extern int	dflag;			/* -d: non-macro defines */
extern int	tflag;			/* -t: create tags for typedefs */
extern int	vflag;			/* -v: vgrind style index output */
extern int	wflag;			/* -w: suppress warnings */
extern int	xflag;			/* -x: cxref style output */
extern bool	_wht[], _etk[], _itk[], _btk[], _gd[];
extern char	lbuf[LINE_MAX];
extern char    *lbp;
extern char	searchar;		/* ex search character */

extern int	cicmp __P((char *));
extern void	getline __P((void));
extern void	pfnote __P((char *, int));
extern int	skip_key __P((int));
extern void	put_entries __P((NODE *));
extern void	toss_yysec __P((void));
extern void	l_entries __P((void));
extern void	y_entries __P((void));
extern int	PF_funcs __P((void));
extern void	c_entries __P((void));
extern void	skip_comment __P((void));
