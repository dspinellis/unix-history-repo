/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.3 (Berkeley) %G%
 */

#include <sys/cdefs.h>

/*
 * csh.c
 */
int	gethdir __P((Char *));
void	dosource __P((Char **));
void	exitstat __P((void));
void	goodbye __P((void));
void	importpath __P((Char *));
void	initdesc __P((void));
void	pintr __P((int));
void	pintr1 __P((bool));
void	printprompt __P((void));
void	process __P((bool));
void	rechist __P((void));
void	untty __P((void));

#ifdef PROF
void done __P((int));
#else
void xexit __P((int));
#endif

/*
 * dir.c
 */
void	 dinit __P((Char *));
void	 dodirs __P((Char **));
Char	*dcanon __P((Char *, Char *));
void	 dtildepr __P((Char *, Char *));
void	 dtilde __P((void));
void	 dochngd __P((Char **));
Char	*dnormalize __P((Char *));
void	 dopushd __P((Char **));
void	 dopopd __P((Char **));
struct directory;
void	 dfree __P((struct directory *));

/*
 * dol.c
 */
void	 Dfix __P((struct command *));
Char	*Dfix1 __P((Char *));
void	 heredoc __P((Char *));

/*
 * err.c
 */
void	seterror __P((int, ...));
void	stderror __P((int, ...));

/*
 * exec.c
 */
void	doexec __P((struct command *));
void	dohash __P((void));
void	dounhash __P((void));
void	execash __P((char **, struct command *));
void	hashstat __P((void));
void	xechoit __P((Char **));

/*
 * exp.c
 */
int	exp __P((Char ***));
int	exp0 __P((Char ***, bool));

/*
 * file.c
 */
#ifdef FILEC
int	tenex __P((Char *, int));
#endif

/*
 * func.c
 */
void	Setenv __P((Char *, Char *));
void	doalias __P((Char **));
void	dobreak __P((void));
void	docontin __P((void));
void	doecho __P((Char **));
void	doelse __P((void));
void	doend __P((void));
void	doeval __P((Char **));
void	doexit __P((Char **));
void	doforeach __P((Char **));
void	doglob __P((Char **));
void	dogoto __P((Char **));
void	doif __P((Char **, struct command *));
void	dolimit __P((Char **));
void	dologin __P((Char **));
void	dologout __P((void));
void	donohup __P((void));
void	doonintr __P((Char **));
void	dorepeat __P((Char **, struct command *));
void	dosetenv __P((Char **));
void	dosuspend __P((void));
void	doswbrk __P((void));
void	doswitch __P((Char **));
void	doumask __P((Char **));
void	dounlimit __P((Char **));
void	dounsetenv __P((Char **));
void	dowhile __P((Char **));
void	dozip __P((void));
void	func __P((struct command *, struct biltins *));
struct	biltins *
	isbfunc __P((struct command *));
void	prvars __P((void));
void	search __P((int, int, Char *));
int	srchx __P((Char *));
void	unalias __P((Char **));
void	wfree __P((void));

/*
 * glob.c
 */
Char	**dobackp __P((Char *, bool));
void	  Gcat __P((Char *, Char *));
Char	 *globone __P((Char *, int));
int	  Gmatch __P((Char *, Char *));
void	  ginit __P((void));
Char	**globall __P((Char **));
void	  rscan __P((Char **, void (*)()));
void	  tglob __P((Char **));
void	  trim __P((Char **));
#ifdef FILEC
int	  sortscmp __P((Char **, Char **));
#endif /* FILEC */

/*
 * hist.c
 */
void	dohist __P((Char **));
struct Hist *
	enthist __P((int, struct wordent *, bool));
void	savehist __P((struct wordent *));

/*
 * lex.c
 */
void	 addla __P((Char *));
void	 bseek __P((off_t));
void	 btoeof __P((void));
void	 copylex __P((struct wordent *, struct wordent *));
Char	*domod __P((Char *, int));
void	 freelex __P((struct wordent *));
int	 lex __P((struct wordent *));
void	 prlex __P((struct wordent *));
int	 readc __P((bool));
void	 settell __P((void));
void	 unreadc __P((int));

/*
 * misc.c
 */
int	  any __P((char *, int));
Char	**blkcat __P((Char **, Char **));
Char	**blkcpy __P((Char **, Char **));
Char	**blkend __P((Char **));
void	  blkfree __P((Char **));
int	  blklen __P((Char **));
void	  blkpr __P((Char **));
Char	**blkspl __P((Char **, Char **));
void	  closem __P((void));
Char	**copyblk __P((Char **));
int	  dcopy __P((int, int));
int	  dmove __P((int, int));
void	  donefds __P((void));
Char	  lastchr __P((Char *));
void	  lshift __P((Char **, int));
int	  number __P((Char *));
int	  prefix __P((Char *, Char *));
Char	**saveblk __P((Char **));
void	  setzero __P((char *, int));
Char	 *strip __P((Char *));
char	 *strsave __P((char *));
char	 *strspl __P((char *, char *));
void	  udvar __P((Char *));

#ifndef	NOTUSED
char	 *strstr __P((const char *, const char *));
#endif
#ifndef	SHORT_STRINGS
char	 *strend __P((char *));
#endif

/*
 * parse.c
 */
void	alias __P((struct wordent *));
void	freesyn __P((struct command *));
struct command *
	syntax __P((struct wordent *, struct wordent *, int));

/*
 * print.c
 */
void	draino __P((void));
void	flush __P((void));
void	pcsecs __P((long));
void	psecs __P((long));
int	putpure __P((int));
int	putraw __P((int));
void	xputchar __P((int));

/*
 * proc.c
 */
void	dobg __P((Char **));
void	dobg1 __P((Char **));
void	dofg __P((Char **));
void	dofg1 __P((Char **));
void	dojobs __P((Char **));
void	dokill __P((Char **));
void	donotify __P((Char **));
void	dostop __P((Char **));
void	dowait __P((void));
void	palloc __P((int, struct command *));
void	panystop __P((bool));
void	pchild __P((int));
void	pendjob __P((void));
struct process *
	pfind __P((Char *));
int	pfork __P((struct command *, int));
void	pgetty __P((int, int));
void	pjwait __P((struct process *));
void	pnote __P((void));
void	prestjob __P((void));
void	psavejob __P((void));
void	pstart __P((struct process *, int));
void	pwait __P((void));

/*
 * sem.c
 */
void	execute __P((struct command *, int, int *, int *));
void	mypipe __P((int *));

/*
 * set.c
 */
struct	varent
	*adrof1 __P((Char *, struct varent *));
void	 doset __P((Char **));
void	 dolet __P((Char **));
Char	*putn __P((int));
int	 getn __P((Char *));
Char	*value1 __P((Char *, struct varent *));
void	 set __P((Char *, Char *));
void	 set1 __P((Char *, Char **, struct varent *));
void	 setq __P((Char *, Char **, struct varent *));
void	 unset __P((Char *[]));
void	 unset1 __P((Char *[], struct varent *));
void	 unsetv __P((Char *));
void	 setNS __P((Char *));
void	 shift __P((Char **));
void	 plist __P((struct varent *));

/*
 * time.c
 */
void	donice __P((Char **));
void	dotime __P((void));
void	prusage __P((struct rusage *, struct rusage *,
	    struct timeval *, struct timeval *));
void	ruadd __P((struct rusage *, struct rusage *));
void	settimes __P((void));
void	tvadd __P((struct timeval *, struct timeval *));
void	tvsub __P((struct timeval *, struct timeval *, struct timeval *));

/*
 * tc.alloc.c
 */
#ifndef SYSMALLOC
#ifdef sun
extern int free();
#else
extern void free();
#endif
extern ptr_t malloc();
extern ptr_t realloc();
extern ptr_t calloc();
#else
extern void Free();
extern ptr_t Malloc();
extern ptr_t Realloc();
extern ptr_t Calloc();
#endif				/* SYSMALLOC */
extern void showall();

/*
 * tc.printf.h
 */
extern void xprintf();
extern void xsprintf();
extern void xvprintf();
extern void xvsprintf();

/*
 * tc.str.c:
 */
#ifdef SHORT_STRINGS
extern Char *s_strchr();
extern Char *s_strrchr();
extern Char *s_strcat();
#ifdef NOTUSED
extern Char *s_strncat();
#endif
extern Char *s_strcpy();
extern Char *s_strncpy();
extern Char *s_strspl();
extern int s_strlen();
extern int s_strcmp();
extern int s_strncmp();
extern Char *s_strsave();
extern Char *s_strend();
extern Char *s_strspl();
#ifdef NOTUSED
extern Char *s_strstr();
#endif
extern Char *str2short();
extern Char **blk2short();
extern char *short2str();
extern char *short2qstr();
extern char **short2blk();
#endif
