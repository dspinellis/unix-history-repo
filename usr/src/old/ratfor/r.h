Original BTL Ratfor System for 4.2
/*-
 * %sccs.include.proprietary.c%
 *
 *	@(#)r.h	1.3 (Berkeley) %G%
 */

#include <stdio.h>
#include "y.tab.h"

#
#define	putbak(c)	*ip++ = c
/*	#define	getchr()	(ip>ibuf?*--ip: getc(infile[infptr]))	*/

#define	LET	1
#define	DIG	2
#define	CRAP	3
#define	COMMENT	'#'
#define	QUOTE	'"'

extern int	transfer;

#define	INDENT	3	/* indent delta */
#ifdef	gcos
#define	CONTFLD	6
#endif
#ifdef	unix
#define	CONTFLD	1
#endif
extern	int	contfld;	/* column for continuation char */
extern	int	contchar;
extern	int	dbg;
extern	int	yyval;
extern	int	*yypv;
extern	int	yylval;
extern	int	errorflag;

extern	char	comment[];	/* save input comments here */
extern	int	comptr;	/* next free slot in comment */
extern	int	printcom;	/* print comments, etc., if on */
extern	int	indent;	/* level of nesting for indenting */

extern	char	ibuf[];
extern	char	*ip;

extern	FILE	*outfil;	/* output file id */
extern	FILE	*infile[];
extern	char	*curfile[];
extern	int	infptr;
extern	int	linect[];

extern	char	fcname[];

extern	int	svargc;
extern	char	**svargv;

#define EOS 0
#define	HSHSIZ	101
struct	nlist {
	char	*name;
	char	*def;
	int	ydef;
	struct	nlist *next;
};

struct nlist	*lookup();
char	*install();
char	*malloc();
extern	char	*fcnloc;

extern	char	type[];
