/*
 * Copyright (c) 1980, 1989 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)systat.h	5.6 (Berkeley) %G%
 */

#include <netdb.h>
#include <nlist.h>
#include <signal.h>
#include <curses.h>
#include <math.h>

#include <sys/param.h>
#include <sys/file.h>
#include <sys/dkstat.h>

#include <netinet/in.h>
#include <arpa/inet.h>

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

int     naptime, col;

long	ntext, textp;
struct	text *xtext;

double	avenrun[3];

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

WINDOW  *wnd;
int	CMDLINE;

char    *malloc(), *calloc(), *strncpy();

#define KREAD(addr, buf, len)  kvm_ckread((addr), (buf), (len))
#define NVAL(indx)  nlst[(indx)].n_value
#define NPTR(indx)  (void *)NVAL((indx))
#define NREAD(indx, buf, len) kvm_ckread(NPTR((indx)), (buf), (len))
#define LONG	(sizeof (long))
