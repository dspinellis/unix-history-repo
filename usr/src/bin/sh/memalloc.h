/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Kenneth Almquist.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)memalloc.h	8.2 (Berkeley) %G%
 */

struct stackmark {
	struct stack_block *stackp;
	char *stacknxt;
	int stacknleft;
};


extern char *stacknxt;
extern int stacknleft;
extern int sstrnleft;
extern int herefd;

pointer ckmalloc __P((int));
pointer ckrealloc __P((pointer, int));
char *savestr __P((char *));
pointer stalloc __P((int));
void stunalloc __P((pointer));
void setstackmark __P((struct stackmark *));
void popstackmark __P((struct stackmark *));
void growstackblock __P((void));
void grabstackblock __P((int));
char *growstackstr __P((void));
char *makestrspace __P((void));
void ungrabstackstr __P((char *, char *));



#define stackblock() stacknxt
#define stackblocksize() stacknleft
#define STARTSTACKSTR(p)	p = stackblock(), sstrnleft = stackblocksize()
#define STPUTC(c, p)	(--sstrnleft >= 0? (*p++ = (c)) : (p = growstackstr(), *p++ = (c)))
#define CHECKSTRSPACE(n, p)	{ if (sstrnleft < n) p = makestrspace(); }
#define USTPUTC(c, p)	(--sstrnleft, *p++ = (c))
#define STACKSTRNUL(p)	(sstrnleft == 0? (p = growstackstr(), *p = '\0') : (*p = '\0'))
#define STUNPUTC(p)	(++sstrnleft, --p)
#define STTOPC(p)	p[-1]
#define STADJUST(amount, p)	(p += (amount), sstrnleft -= (amount))
#define grabstackstr(p)	stalloc(stackblocksize() - sstrnleft)

#define ckfree(p)	free((pointer)(p))
