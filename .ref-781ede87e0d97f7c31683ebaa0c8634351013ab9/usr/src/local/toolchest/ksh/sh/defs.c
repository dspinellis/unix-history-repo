/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)defs.c	1.1 */
/*
 * Ksh - AT&T Bell Laboratories
 * Written by David Korn
 * This file defines all the  read/write shell global variables
 */

#include	"defs.h"
#include	"sym.h"
#include	"io.h"
#include	"history.h"
#include	"brkincr.h"
#include	"flags.h"
#include	"name.h"
#include	"jobs.h"
#include	"edit.h"
#include	"timeout.h"


struct Amemory *alias;
struct	State	st;
BLKPTR		blokp;
BLKPTR		bloktop;
NAMPTR		bltin_nodes;
char		*brkbegin;
STKPTR		brkend;
char		*comdiv;
#ifdef VSH
struct	edit	editb;
#else
# ifdef ESH
struct	edit	editb;
# endif /* ESH */
#endif	/* VSH */
jmp_buf		errshell;
int		exitval;
struct fixcmd	*fc_fix;
jmp_buf		*freturn;
ARGPTR		gchain;
#ifdef JOBS
struct jobs	jobstat;
#endif	/* JOBS */
char		*lastarg;
int		lastbase = 10; 
long		mailchk = 600;
long		timeout = TIMEOUT;
struct Amemory	*namep;
int		oldexit;
FILE		*output;
int		parent;
char		pcsadr[12];
char		pidadr[12];
long		ppid;
struct Amemory	*prnames;
int		savexit;
STKPTR		stakbas;
BLKPTR		stakbsy;
STKPTR		stakbot;
STKPTR		staktop;
FILEBLK		stdfile;
jmp_buf		subshell;
char		*sysmsg[MAXTRAP];
char		tmpout[] = "/tmp/shxxxxxx.aaa";
int		topfd;
char		*trapcom[MAXTRAP+1];
BOOL		trapnote;
BOOL		login_sh;
int		userid;
int		subflag;
int		wdnum;
ARGPTR		wdarg;
int		wdval;

#ifdef 	NOBUF
unsigned char	_sibuf[BUFSIZ];
unsigned char	_sobuf[BUFSIZ];
#endif	/* NOBUF */
