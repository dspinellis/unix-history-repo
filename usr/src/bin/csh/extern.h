/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)extern.h	5.1 (Berkeley) %G%
 */

/*
 * sh.c
 */
extern void rechist();
extern void goodbye();
extern void exitstat();
extern void pintr();
extern void pintr1();
extern void process();
extern void dosource();
extern int gethdir();
extern void importpath();
extern void initdesc();
extern void untty();
#ifndef PROF
extern void xexit();
#else
extern void done();
#endif
extern void printprompt();

/*
 * sh.dir.c
 */
extern void dinit();
extern void dodirs();
extern Char *dcanon();
extern void dtildepr();
extern void dtilde();
extern void dochngd();
extern Char *dnormalize();
extern void dopushd();
extern void dopopd();
extern void dfree();

/*
 * sh.dol.c
 */
extern void Dfix();
extern Char *Dfix1();
extern void heredoc();

/*
 * sh.err.c
 */
extern void seterror();
extern void stderror();

/*
 * sh.exec.c
 */
extern void doexec();
extern void execash();
extern void xechoit();
extern void dohash();
extern void dounhash();
#ifdef VFORK
extern void hashstat();
#endif

/*
 * sh.exp.c
 */
extern int exp();
extern int exp0();

/*
 * sh.time.c
 */
extern void settimes();

/*
 * sh.file.c
 */
#ifdef FILEC
extern int tenex();
#endif

/*
 * sh.func.c
 */
extern struct biltins *isbfunc();
extern void func();
extern void doonintr();
extern void donohup();
extern void dozip();
extern void prvars();
extern void doalias();
extern void unalias();
extern void dologout();
extern void dologin();
extern void doif();
extern void doelse();
extern void dogoto();
extern void doswitch();
extern void dobreak();
extern void doexit();
extern void doforeach();
extern void dowhile();
extern void doend();
extern void docontin();
extern void dorepeat();
extern void doswbrk();
extern int srchx();
extern void search();
extern void wfree();
extern void doecho();
extern void doglob();
extern void dosetenv();
extern void dounsetenv();
extern void Setenv();
extern void doumask();
extern void dolimit();
extern void dounlimit();
extern void dosuspend();
extern void doeval();

/*
 * sh.glob.c
 */
extern Char **globall();
extern void ginit();
extern void trim();
extern int Gmatch();
extern void Gcat();
extern void rscan();
extern void tglob();
extern Char *globone();
extern Char **dobackp();

#ifdef FILEC
extern int sortcmp();

#endif


/*
 * sh.hist.c
 */
extern void savehist();
extern struct Hist *enthist();
extern void dohist();

/*
 * sh.lex.c
 */
extern int lex();
extern void prlex();
extern void copylex();
extern void freelex();
extern void addla();
extern Char *domod();
extern void unreadc();
extern int readc();
extern void bseek();
#ifndef btell
extern off_t btell();
#endif
extern void btoeof();
extern void settell();

/*
 * sh.misc.c
 */
extern int any();
extern char *strsave();
extern Char **blkend();
extern void blkpr();
extern int number();
extern int blklen();
extern Char **blkcpy();
extern Char **blkcat();
extern void blkfree();
extern Char **saveblk();
extern void setzero();
#ifndef NOTUSED
extern char *strstr();
#endif
extern char *strspl();
extern Char **blkspl();
extern Char lastchr();
extern void closem();
extern void donefds();
extern int dmove();
extern int dcopy();
extern void lshift();
extern int number();
extern Char **copyblk();
#ifndef SHORT_STRINGS
extern char *strend();
#endif
extern Char *strip();
extern void udvar();
extern int prefix();

/*
 * sh.parse.c
 */
extern void alias();
extern struct command *syntax();
extern void freesyn();

/*
 * sh.print.c
 */
extern void psecs();
extern void pcsecs();
extern void xputchar();
extern int putraw();
extern int putpure();
extern void draino();
extern void flush();

/*
 * sh.proc.c
 */
extern void pchild();
extern void pnote();
extern void pwait();
extern void pjwait();
extern void dowait();
extern void palloc();
extern void psavejob();
extern void prestjob();
extern void pendjob();
extern void dojobs();
extern void dofg();
extern void dofg1();
extern void dobg();
extern void dobg1();
extern void dokill();
extern void dostop();
extern void pstart();
extern void panystop();
extern struct process *pfind();
extern void donotify();
extern int pfork();
extern void pgetty();

/*
 * sh.sem.c
 */
extern void execute();
extern void mypipe();

/*
 * sh.set.c
 */
extern void doset();
extern void dolet();
extern Char *putn();
extern int getn();
extern Char *value1();
extern struct varent *adrof1();
extern void set();
extern void set1();
extern void setq();
extern void unset();
extern void unset1();
extern void unsetv();
extern void setNS();
extern void shift();
extern void plist();

/*
 * sh.time.c
 */
extern void settimes();
extern void dotime();
extern void donice();
extern void ruadd();
extern void tvadd();
extern void tvsub();
extern void prusage();

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
