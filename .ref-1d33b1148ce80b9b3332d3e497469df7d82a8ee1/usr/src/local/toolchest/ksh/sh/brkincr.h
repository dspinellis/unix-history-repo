/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)brkincr.h	1.1 */
/*
 *	UNIX shell
 *	S. R. Bourne
 *	Rewritten by David Korn
 *
 */

#if u370 || uts
#define BRKINCR		4096
#else
#define BRKINCR		((int)(0400*sizeof(char*)))
#endif	/* u370 */
#ifdef INT16
#define BRKMAX		((unsigned)(077777-BRKINCR))
#else
#define BRKMAX		(1024*BRKINCR)
#endif /* INT16 */
#define MINTRAP		0
#define MAXTRAP		NSIG+1

#define SIGBITS		8

#define SIGSLOW		1
#define SIGSET		4
#define SIGMOD		8
/*
 * SIGNOSET means that signal not set unless mentioned explicitly
 * SIGCAUGHT signals trap to fault, others to done
 */
#define SIGCAUGHT	16
#define SIGNOSET	32
#define SIGIGNORE	64
#define	SIGJOBS		128

#ifdef BSD
# ifdef BSD_4_2
#define	sighold(s)	sigblock(1<<((s)-1))
#define sigrelse(s)	sigsetmask(sigblock(0)&~(1<<((s)-1)))
# else
#define signal	sigset
# endif /* BSD_4_2 */
#else
#endif	/* BSD */
#define SIGFAIL		0200
#define SIGFLG		0200
#define TRAPSET		2



 
struct	blk		/* heap storage */
{
	BLKPTR		word;
};


extern char *brkbegin;
extern BLKPTR		blokp;			/*current search pointer*/
extern BLKPTR		bloktop;		/*top of arena (last blok)
						   initialized in addblok */

extern jmp_buf	subshell;	/* jump here for subshell */
extern jmp_buf *freturn;	/* return pointer for functions or failure */
extern jmp_buf errshell;	/* return here on failures */
