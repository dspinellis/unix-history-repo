/*
 * Copyright (c) 1980, 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)systat.h	5.4 (Berkeley) 3/12/90
 */

#include <netdb.h>
#include <nlist.h>
#include <signal.h>
#include <curses.h>
#include <math.h>

#include <sys/param.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/dkstat.h>

#include <netinet/in.h>
#include <arpa/inet.h>

struct p_times {
        short   pt_pid;
        float   pt_pctcpu;
        int     pt_uid;
        int     pt_paddr;
        struct  proc *pt_pp;
} *pt;
long    nproc, procp;
struct	proc *kprocp;

struct procs {
        int     pid;
        char    cmd[16];
} procs[200];
int     numprocs;

struct users {
        int     k_uid;
        char    k_name[16];
} known[30];
int     numknown;

struct  cmdtab {
        char    *c_name;		/* command name */
        int     (*c_refresh)();		/* display refresh */
        int     (*c_fetch)();		/* sets up data structures */
        int     (*c_label)();		/* label display */
	int	(*c_init)();		/* initialize namelist, etc. */
	WINDOW	*(*c_open)();		/* open display */
	int	(*c_close)();		/* close display */
	int	(*c_cmd)();		/* display command interpreter */
	char	c_flags;		/* see below */
};

#define	CF_INIT		0x1		/* been initialized */
#define	CF_LOADAV	0x2		/* display w/ load average */

struct	cmdtab *curcmd;
struct	cmdtab cmdtab[];
struct	cmdtab *lookup();

int     kmem, mem, swap;
int     naptime, col;

long	ntext, textp;
struct	text *xtext;

int	fscale;
double  lccpu;
double	avenrun[3];

char    *kmemf, *memf, *swapf;
int	hz, phz;
char	**dr_name;
int	dk_ndrive;
int	*dk_select;
float	*dk_mspw;
char    c, *namp, hostname[MAXHOSTNAMELEN];

int	nports;
int	nhosts;
int	protos;
#define	TCP	0x1
#define	UDP	0x2

struct  pte *usrpt;
struct  pte *Usrptma;

WINDOW  *wnd;
int	CMDLINE;

char    *malloc(), *calloc(), *strncpy();
long    getw();
