/* $Header: /home/hyperion/mu/christos/src/sys/tcsh-6.01/RCS/tc.os.c,v 3.13 1991/12/19 22:34:14 christos Exp $ */
/*
 * tc.os.c: OS Dependent builtin functions
 */
/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include "sh.h"

RCSID("$Id: tc.os.c,v 3.13 1991/12/19 22:34:14 christos Exp $")

#include "tw.h"
#include "ed.h"
#include "ed.defns.h"		/* for the function names */

#ifdef titan
int     end;
#endif				/* titan */

/***
 *** MACH
 ***/

#ifdef MACH
/* dosetpath -- setpath built-in command
 *
 **********************************************************************
 * HISTORY
 * 08-May-88  Richard Draves (rpd) at Carnegie-Mellon University
 *	Major changes to remove artificial limits on sizes and numbers
 *	of paths.
 *
 **********************************************************************
 */

#ifdef MACH
static Char STRCPATH[] = {'C', 'P', 'A', 'T', 'H', '\0'};
static Char STRLPATH[] = {'L', 'P', 'A', 'T', 'H', '\0'};
static Char STRMPATH[] = {'M', 'P', 'A', 'T', 'H', '\0'};
static Char STREPATH[] = {'E', 'P', 'A', 'T', 'H', '\0'};
#endif				/* MACH */

static Char *syspaths[] = {STRPATH, STRCPATH, STRLPATH, STRMPATH, STREPATH, 0};
#define LOCALSYSPATH	"/usr/cs"

/*ARGSUSED*/
void
dosetpath(arglist, c)
    Char  **arglist;
    struct command *c;
{
    extern char *getenv();
    sigmask_t omask;
    Char  **pathvars, **cmdargs;
    Char  **paths;
    char  **spaths, **cpaths, **cmds;
    char   *tcp;
    unsigned int npaths, ncmds;
    int     i, sysflag;

    omask = sigsetmask(sigmask(SIGINT));

    /*
     * setpath(3) uses stdio and we want 0, 1, 2 to work...
     */
    if (!didfds) {
	(void) dcopy(SHIN, 0);
	(void) dcopy(SHOUT, 1);
	(void) dcopy(SHDIAG, 2);
	didfds = 1;
    }

    for (i = 1; arglist[i] && (arglist[i][0] != '-'); i++);
    npaths = i - 1;

    cmdargs = &arglist[i];
    for (; arglist[i]; i++);
    ncmds = i - npaths - 1;

    if (npaths) {
	sysflag = 0;
	pathvars = &arglist[1];
    }
    else {
	sysflag = 1;
	npaths = (sizeof syspaths / sizeof *syspaths) - 1;
	pathvars = syspaths;
    }

    /* note that npaths != 0 */

    spaths = (char **) xmalloc(npaths * sizeof *spaths);
    setzero((char *) spaths, npaths * sizeof *spaths);
    paths = (Char **) xmalloc((npaths + 1) * sizeof *paths);
    setzero((char *) paths, (npaths + 1) * sizeof *paths);
    cpaths = (char **) xmalloc((npaths + 1) * sizeof *cpaths);
    setzero((char *) cpaths, (npaths + 1) * sizeof *cpaths);
    cmds = (char **) xmalloc((ncmds + 1) * sizeof *cmds);
    setzero((char *) cmds, (ncmds + 1) * sizeof *cmds);
    for (i = 0; i < npaths; i++) {
	char   *val = getenv(short2str(pathvars[i]));

	if (val == NULL)
	    val = "";

	spaths[i] = xmalloc((Strlen(pathvars[i]) + strlen(val)
			     + 2) * sizeof **spaths);
	(void) strcpy(spaths[i], short2str(pathvars[i]));
	(void) strcat(spaths[i], "=");
	(void) strcat(spaths[i], val);
	cpaths[i] = spaths[i];
    }

    for (i = 0; i < ncmds; i++) {
	Char   *val = globone(cmdargs[i], G_ERROR);

	if (val == NULL)
	    goto abortpath;
	cmds[i] = xmalloc(Strlen(val) + 1);
	(void) strcpy(cmds[i], short2str(val));
    }


    if (setpath(cpaths, cmds, LOCALSYSPATH, sysflag, 1) < 0) {
abortpath:
	if (spaths) {
	    for (i = 0; i < npaths; i++)
		if (spaths[i])
		    xfree((ptr_t) spaths[i]);
	    xfree((ptr_t) spaths);
	}
	if (paths) {
	    for (i = 0; i < npaths; i++)
		if (paths[i])
		    xfree((ptr_t) paths[i]);
	    xfree((ptr_t) paths);
	}
	if (cpaths)
	    xfree((ptr_t) cpaths);
	if (cmds) {
	    for (i = 0; i < ncmds; i++)
		if (cmds[i])
		    xfree((ptr_t) cmds[i]);
	    xfree((ptr_t) cmds);
	}

	for (i = 0; i < npaths; i++) {
	    paths[i] = SAVE(cpaths[i]);
	    xfree((ptr_t) cpaths[i]);
	}
	(void) sigsetmask(omask);
	donefds();
	return;
    }

    for (i = 0; i < npaths; i++) {
	Char   *val;

	for (val = paths[i]; val && *val && *val != '='; val++);
	if (val && *val == '=') {
	    *val++ = '\0';
	    setenv(paths[i], val);
	    if (Strcmp(paths[i], STRPATH) == 0) {
		importpath(val);
		if (havhash)
		    dohash(NULL, NULL);
	    }
	    *--val = '=';
	}
    }
    (void) sigsetmask(omask);
    donefds();
}
#endif /* MACH */

/***
 *** AIX
 ***/
#ifdef TCF
/* ARGSUSED */
void
dogetxvers(v, c)
    Char  **v;
    struct command *c;
{
    char    xvers[MAXPATHLEN];

    if (getxvers(xvers, MAXPATHLEN) == -1)
	stderror(ERR_SYSTEM, "getxvers", strerror(errno));
    xprintf("%s\n", xvers);
    flush();
}

/*ARGSUSED*/
void
dosetxvers(v, c)
    Char  **v;
    struct command *c;
{
    char   *xvers;

    ++v;
    if (!*v || *v[0] == '\0')
	xvers = "";
    else
	xvers = short2str(*v);
    if (setxvers(xvers) == -1)
	stderror(ERR_SYSTEM, "setxvers", strerror(errno));
}

#include <sf.h>
#ifdef _AIXPS2
# define XC_PDP11	0x01
# define XC_23		0x02
# define XC_Z8K		0x03
# define XC_8086	0x04
# define XC_68K		0x05
# define XC_Z80		0x06
# define XC_VAX		0x07
# define XC_16032	0x08
# define XC_286		0x09
# define XC_386		0x0a
# define XC_S370	0x0b
#else
# include <sys/x.out.h>
#endif /* _AIXPS2 */

static struct xc_cpu_t {
    short   xc_id;
    char   *xc_name;
}       xcpu[] =
{
    { XC_PDP11,	"pdp11"   },
    { XC_23,	"i370"    },
    { XC_Z8K,	"z8000"   },
    { XC_8086,	"i86"	  },
    { XC_68K,	"mc68000" },
    { XC_Z80,	"x80"	  },
    { XC_VAX,	"vax"	  },
    { XC_16032,	"ns16032" },
    { XC_286,	"i286"	  },
    { XC_386,	"i386"	  },
    { XC_S370,	"xa370"	  },
    { 0,	NULL      }
};

/*
 * our local hack table, stolen from x.out.h
 */
static char *
getxcode(xcid)
    short   xcid;
{
    int     i;

    for (i = 0; xcpu[i].xc_name != NULL; i++)
	if (xcpu[i].xc_id == xcid)
	    return (xcpu[i].xc_name);
    return (NULL);
}

static short
getxid(xcname)
    char   *xcname;
{
    int     i;

    for (i = 0; xcpu[i].xc_name != NULL; i++)
	if (strcmp(xcpu[i].xc_name, xcname) == 0)
	    return (xcpu[i].xc_id);
    return ((short) -1);
}


/*ARGSUSED*/
void
dogetspath(v, c)
    Char  **v;
    struct command *c;
{
    int     i, j;
    sitepath_t p[MAXSITE];
    struct sf *st;
    static char *local = "LOCAL ";

    if ((j = getspath(p, MAXSITE)) == -1)
	stderror(ERR_SYSTEM, "getspath", strerror(errno));
    for (i = 0; i < j && (p[i] & SPATH_CPU) != NOSITE; i++) {
	if (p[i] & SPATH_CPU) {
	    if ((p[i] & SPATH_MASK) == NULLSITE)
		xprintf(local);
	    else if ((st = sfxcode((short) (p[i] & SPATH_MASK))) != NULL)
		xprintf("%s ", st->sf_ctype);
	    else {
		char   *xc = getxcode(p[i] & SPATH_MASK);

		if (xc != NULL)
		    xprintf("%s ", xc);
		else
		    xprintf("*cpu %d* ", (int) (p[i] & SPATH_MASK));
		/* 
		 * BUG in the aix code... needs that cause if
		 * sfxcode fails once it fails for ever 
		 */
		endsf();	
	    }
	}
	else {
	    if (p[i] == NULLSITE)
		xprintf(local);
	    else if ((st = sfnum(p[i])) != NULL)
		xprintf("%s ", st->sf_sname);
	    else
		xprintf("*site %d* ", (int) (p[i] & SPATH_MASK));
	}
    }
    xprintf("\n");
    flush();
}

/*ARGSUSED*/
void
dosetspath(v, c)
    Char  **v;
    struct command *c;
{
    int     i;
    short   j;
    char   *s;
    sitepath_t p[MAXSITE];
    struct sf *st;

    for (i = 0, v++; *v && *v[0] != '\0'; v++, i++) {
	s = short2str(*v);
	if (Isdigit(*s))
	    p[i] = atoi(s);
	else if (strcmp(s, "LOCAL") == 0)
	    p[i] = NULLSITE;
	else if ((st = sfctype(s)) != NULL)
	    p[i] = SPATH_CPU | st->sf_ccode;
	else if ((j = getxid(s)) != -1)
	    p[i] = SPATH_CPU | j;
	else if ((st = sfname(s)) != NULL)
	    p[i] = st->sf_id;
	else {
	    setname(s);
	    stderror(ERR_NAME | ERR_STRING, "Bad cpu/site name");
	}
	if (i == MAXSITE - 1)
	    stderror(ERR_NAME | ERR_STRING, "Site path too long");
    }
    if (setspath(p, i) == -1)
	stderror(ERR_SYSTEM, "setspath", strerror(errno));
}

/* sitename():
 *	Return the site name where the process is running
 */
char   *
sitename(pid)
    pid_t   pid;
{
    siteno_t ss;
    struct sf *st;

    if ((ss = site(pid)) == -1 || (st = sfnum(ss)) == NULL)
	return ("unknown");
    else
	return (st->sf_sname);
}

static int
migratepid(pid, new_site)
    pid_t   pid;
    siteno_t new_site;
{
    struct sf *st;
    int     need_local;

    need_local = (pid == 0) || (pid == getpid());

    if (kill3((pid_t) pid, SIGMIGRATE, new_site) < 0) {
	xprintf("%d: %s\n", pid, strerror(errno));
	return (-1);
    }

    if (need_local) {
	if ((new_site = site(0)) == -1) {
	    xprintf("site: %s\n", strerror(errno));
	    return (-1);
	}
	if ((st = sfnum(new_site)) == NULL) {
	    xprintf("%d: Site not found\n", new_site);
	    return (-1);
	}
	if (setlocal(st->sf_local, strlen(st->sf_local)) == -1) {
	    xprintf("setlocal: %s: %s\n", st->sf_local, strerror(errno));
	    return (-1);
	}
    }
    return (0);
}

/*ARGSUSED*/
void
domigrate(v, c)
    Char  **v;
    struct command *c;
{
    struct sf *st;
    char   *s;
    Char   *cp;
    struct process *pp;
    int     pid, err1 = 0;
    siteno_t new_site = 0;
    sigmask_t omask;

#ifdef BSDSIGS
    omask = sigmask(SIGCHLD);
    if (setintr)
	omask |= sigmask(SIGINT);
    omask = sigblock(omask) & ~omask;
#else
    if (setintr)
	(void) sighold(SIGINT);
    (void) sighold(SIGCHLD);
#endif /* BSDSIGS */

    ++v;
    if (*v[0] == '-') {
	/*
	 * Do the -site.
	 */
	s = short2str(&v[0][1]);
	if ((st = sfname(s)) == NULL) {
	    setname(s);
	    stderror(ERR_NAME | ERR_STRING, "Site not found");
	}
	new_site = st->sf_id;
	++v;
    }

    if (!*v || *v[0] == '\0') {
	if (migratepid(0, new_site) == -1)
	    err1++;
    }
    else {
	gflag = 0, tglob(v);
	if (gflag) {
	    v = globall(v);
	    if (v == 0)
		stderror(ERR_NAME | ERR_NOMATCH);
	}
	else {
	    v = gargv = saveblk(v);
	    trim(v);
	}

	while (v && (cp = *v)) {
	    if (*cp == '%') {
		pp = pfind(cp);
		if (kill3((pid_t) - pp->p_jobid, SIGMIGRATE, new_site) < 0) {
		    xprintf("%s: %s\n", short2str(cp), strerror(errno));
		    err1++;
		}
	    }
	    else if (!(Isdigit(*cp) || *cp == '-'))
		stderror(ERR_NAME | ERR_JOBARGS);
	    else {
		pid = atoi(short2str(cp));
		if (migratepid(pid, new_site) == -1)
		    err1++;
	    }
	    v++;
	}
	if (gargv)
	    blkfree(gargv), gargv = 0;
    }

done:
#ifdef BSDSIGS
    (void) sigsetmask(omask);
#else
    (void) sigrelse(SIGCHLD);
    if (setintr)
	(void) sigrelse(SIGINT);
#endif /* BSDSIGS */
    if (err1)
	stderror(ERR_SILENT);
}

#endif /* TCF */

/***
 *** CONVEX Warps.
 ***/

#ifdef WARP
/*
 * handle the funky warping of symlinks
 */
#include <warpdb.h>
#include <sys/warp.h>

static jmp_buf sigsys_buf;

static  sigret_t
catch_sigsys()
{
    longjmp(sigsys_buf, 1);
}


/*ARGSUSED*/
void
dowarp(v, c)
    Char  **v;
    struct command *c;
{
    int     warp, oldwarp;
    struct warpent *we;
    void    (*old_sigsys_handler) () = 0;
    char   *newwarp;

    if (setjmp(sigsys_buf)) {
	signal(SIGSYS, old_sigsys_handler);
	stderror(ERR_NAME | ERR_STRING, 
		 "You're trapped in a universe you never made");
	return;
    }
    old_sigsys_handler = signal(SIGSYS, catch_sigsys);

    warp = getwarp();

    v++;
    if (*v == 0) {		/* display warp value */
	if (warp < 0)
	    stderror(ERR_NAME | ERR_STRING, "Getwarp failed");
	we = getwarpbyvalue(warp);
	if (we)
	    printf("%s\n", we->w_name);
	else
	    printf("%d\n", warp);
    }
    else {			/* set warp value */
	oldwarp = warp;
	newwarp = short2str(*v);
	if (Isdigit(*v[0]))
	    warp = atoi(newwarp);
	else {
	    we = getwarpbyname(newwarp);
	    if (we)
		warp = we->w_value;
	    else
		warp = -1;
	}
	if ((warp < 0) || (warp >= WARP_MAXLINK))
	    stderror(ERR_NAME | ERR_STRING, "Invalid warp");
	if ((setwarp(warp) < 0) || (getwarp() != warp)) {
	    (void) setwarp(oldwarp);
	    stderror(ERR_NAME | ERR_STRING, "Setwarp failed");
	}
    }
    signal(SIGSYS, old_sigsys_handler);
    return;
}
#endif /* WARP */

/***
 *** Masscomp
 ***/
/* Added, DAS DEC-90. */
#ifdef masscomp
/*ARGSUSED*/
void
douniverse(v, c)
    register Char **v;
    struct command *c;
{
    register Char *cp = v[1];
    char    ubuf[100];

    if (cp == 0) {
	(void) getuniverse(ubuf);
	xprintf("%s\n", ubuf);
    }
    else if (*cp == '\0' || setuniverse(short2str(cp)) != 0)
	stderror(ERR_NAME | ERR_STRING, "Illegal universe");
}
#endif /* masscomp */


#ifdef _SEQUENT_
/*
 * Compute the difference in process stats.
 */
void
pr_stat_sub(p2, p1, pr)
    struct process_stats *p2, *p1, *pr;
{
    pr->ps_utime.tv_sec = p2->ps_utime.tv_sec - p1->ps_utime.tv_sec;
    pr->ps_utime.tv_usec = p2->ps_utime.tv_usec - p1->ps_utime.tv_usec;
    if (pr->ps_utime.tv_usec < 0) {
	pr->ps_utime.tv_sec -= 1;
	pr->ps_utime.tv_usec += 1000000;
    }
    pr->ps_stime.tv_sec = p2->ps_stime.tv_sec - p1->ps_stime.tv_sec;
    pr->ps_stime.tv_usec = p2->ps_stime.tv_usec - p1->ps_stime.tv_usec;
    if (pr->ps_stime.tv_usec < 0) {
	pr->ps_stime.tv_sec -= 1;
	pr->ps_stime.tv_usec += 1000000;
    }

    pr->ps_maxrss = p2->ps_maxrss - p1->ps_maxrss;
    pr->ps_pagein = p2->ps_pagein - p1->ps_pagein;
    pr->ps_reclaim = p2->ps_reclaim - p1->ps_reclaim;
    pr->ps_zerofill = p2->ps_zerofill - p1->ps_zerofill;
    pr->ps_pffincr = p2->ps_pffincr - p1->ps_pffincr;
    pr->ps_pffdecr = p2->ps_pffdecr - p1->ps_pffdecr;
    pr->ps_swap = p2->ps_swap - p1->ps_swap;
    pr->ps_syscall = p2->ps_syscall - p1->ps_syscall;
    pr->ps_volcsw = p2->ps_volcsw - p1->ps_volcsw;
    pr->ps_involcsw = p2->ps_involcsw - p1->ps_involcsw;
    pr->ps_signal = p2->ps_signal - p1->ps_signal;
    pr->ps_lread = p2->ps_lread - p1->ps_lread;
    pr->ps_lwrite = p2->ps_lwrite - p1->ps_lwrite;
    pr->ps_bread = p2->ps_bread - p1->ps_bread;
    pr->ps_bwrite = p2->ps_bwrite - p1->ps_bwrite;
    pr->ps_phread = p2->ps_phread - p1->ps_phread;
    pr->ps_phwrite = p2->ps_phwrite - p1->ps_phwrite;
}

#endif /* _SEQUENT_ */


#ifdef tcgetpgrp
int
xtcgetpgrp(fd)
    int     fd;
{
    int     pgrp;

    /* ioctl will handle setting errno correctly. */
    if (ioctl(fd, TIOCGPGRP, (ioctl_t) & pgrp) < 0)
	return (-1);
    return (pgrp);
}

/*
 * XXX: tcsetpgrp is not a macro any more cause on some systems,
 * pid_t is a short, but the ioctl() takes a pointer to int (pyr)
 * Thanks to Simon Day (simon@pharaoh.cyborg.bt.co.uk) for pointing
 * this out.
 */
int
xtcsetpgrp(fd, pgrp)
    int fd, pgrp;
{
    return ioctl(fd, TIOCSPGRP, (ioctl_t) &pgrp);
}

#endif	/* tcgetpgrp */


#ifdef YPBUGS
void
fix_yp_bugs()
{
    char   *mydomain;

    /*
     * PWP: The previous version assumed that yp domain was the same as the
     * internet name domain.  This isn't allways true. (Thanks to Mat Landau
     * <mlandau@bbn.com> for the original version of this.)
     */
    if (yp_get_default_domain(&mydomain) == 0) {	/* if we got a name */
	extern void yp_unbind();

	yp_unbind(mydomain);
    }
}

#endif /* YPBUGS */


void
osinit()
{
    extern ptr_t membot;

    membot = (ptr_t) sbrk(0);

#ifdef OREO
    set42sig();
    sigignore(SIGIO);		/* ignore SIGIO */
#endif /* OREO */

#ifdef aiws
    {
	struct sigstack inst;
	inst.ss_sp = xmalloc(4192) + 4192;
	inst.ss_onstack = 0;
	sigstack(&inst, NULL);
    }
#endif /* aiws */

#ifdef titan
    end = sbrk(0);
#endif	/* titan */

#ifdef apollo
    (void) isapad();
#endif
}

#ifdef strerror
char *
xstrerror(i)
    int i;
{
    static char errbuf[40]; /* 64 bit num */

    if (i >= 0 && i < sys_nerr) 
	return sys_errlist[i];
    else {
	xsprintf(errbuf, "Unknown Error: %d", i);
	return errbuf;
    }
}
#endif /* strerror */
    
#ifdef gethostname
#include <sys/utsname.h>

int
xgethostname(name, namlen)
    char   *name;
    int     namlen;
{
    int     i, retval;
    struct utsname uts;

    retval = uname(&uts);

#ifdef DEBUG
    xprintf("sysname:  %s\n", uts.sysname);
    xprintf("nodename: %s\n", uts.nodename);
    xprintf("release:  %s\n", uts.release);
    xprintf("version:  %s\n", uts.version);
    xprintf("machine:  %s\n", uts.machine);
#endif				/* DEBUG */
    i = strlen(uts.nodename) + 1;
    (void) strncpy(name, uts.nodename, i < namlen ? i : namlen);

    return retval;
}				/* end gethostname */

#endif				/* gethostname */


#ifdef getwd
static char *strrcpy __P((char *, char *));

/* xgetwd():
 *	Return the pathname of the current directory, or return
 *	an error message in pathname.
 */

# ifdef hp9000s500
/*
 *  From: Bernd Mohr <mohr@faui77.informatik.uni-erlangen.de>
 *  I also ported the tcsh to the HP9000 Series 500. This computer
 *  is a little bit different than the other HP 9000 computer. It has
 *  a HP Chip instead of a Motorola CPU and it is no "real" UNIX. It runs
 *  HP-UX which is emulated in top of a HP operating system. So, the last
 *  supported version of HP-UX is 5.2 on the HP9000s500. This has two
 *  consequences: it supports no job control and it has a filesystem
 *  without "." and ".." !!!
 */
static int pathsize;			/* pathname length */

char *
xgetwd(pathname)
	char *pathname;
{
	char pathbuf[MAXNAMLEN];	/* temporary pathname buffer */
	char *pnptr = &pathbuf[(sizeof pathbuf)-1]; /* pathname pointer */
	char *prepend();		/* prepend dirname to pathname */
	dev_t rdev;			/* root device number */
	DIR *dirp;			/* directory stream */
	ino_t rino;			/* root inode number */
	off_t rsize;			/* root size */
	struct direct *dir;		/* directory entry struct */
	struct stat d ,dd;		/* file status struct */

	pathsize = 0;
	*pnptr = '\0';
	stat("/.", &d);
	rdev = d.st_dev;
	rino = d.st_ino;
	rsize = d.st_size;
	for (;;) {
		stat(".", &d);
		if (d.st_ino == rino && d.st_dev == rdev && d.st_size == rsize)
			break;		/* reached root directory */
		if ((dirp = opendir("..")) == NULL) {
        		(void) xsprintf(pathname,
                        "getwd: Cannot open \"..\" (%s)", strerror(errno));
			goto fail;
		}
		if (chdir("..") < 0) {
        		(void) xsprintf(pathname,
                        "getwd: Cannot chdir to \"..\" (%s)", strerror(errno));
			goto fail;
		}
		do {
			if((dir = readdir(dirp)) == NULL) {
				closedir(dirp);
        			(void) xsprintf(pathname,
                        	"getwd: Read error in \"..\" (%s)",
				strerror(errno));
				goto fail;
			}
			stat(dir->d_name, &dd);
		} while (dd.st_ino  != d.st_ino  ||
			 dd.st_dev  != d.st_dev  ||
			 dd.st_size != d.st_size
			);
		closedir(dirp);
		pnptr = prepend("/", prepend(dir->d_name, pnptr));
	}

	if (*pnptr == '\0')		/* current dir == root dir */
		strcpy(pathname, "/");
	else {
		strcpy(pathname, pnptr);
		if (chdir(pnptr) < 0) {
        		(void) xsprintf(pathname,
                        "getwd: Cannot change back to \".\" (%s)",
			strerror(errno));
			return (NULL);
		}
	}
	return (pathname);

fail:
	chdir(prepend(".", pnptr));
	return (NULL);
}

/* prepend():
 *	 tacks a directory name onto the front of a pathname.
 */
static char *
prepend(dirname, pathname)
	register char *dirname;
	register char *pathname;
{
	register int i;			/* directory name size counter */

	for (i = 0; *dirname != '\0'; i++, dirname++)
		continue;
	if ((pathsize += i) < MAXNAMLEN)
		while (i-- > 0)
			*--pathname = *--dirname;
	return (pathname);
}

# else /* ! hp9000s500 */
char   *
xgetwd(pathname)
    char   *pathname;
{
    DIR    *dp;
    struct dirent *d;

    struct stat st_root, st_cur, st_next, st_dot;
    char    pathbuf[MAXPATHLEN], nextpathbuf[MAXPATHLEN * 2];
    char   *cur_name_add;
    char   *pathptr, *nextpathptr;

    /* find the inode of root */
    if (stat("/", &st_root) == -1) {
	(void) xsprintf(pathname,
			"getwd: Cannot stat \"/\" (%s)", strerror(errno));
	return (NULL);
    }
    pathbuf[MAXPATHLEN - 1] = '\0';
    pathptr = &pathbuf[MAXPATHLEN - 1];
    nextpathbuf[MAXPATHLEN - 1] = '\0';
    cur_name_add = nextpathptr = &nextpathbuf[MAXPATHLEN - 1];

    /* find the inode of the current directory */
    if (lstat("./", &st_cur) == -1) {
	(void) xsprintf(pathname,
			"getwd: Cannot stat \".\" (%s)", strerror(errno));
	return (NULL);
    }
    nextpathptr = strrcpy(nextpathptr, "../");

    /* Descend to root */
    for (;;) {

	/* look if we found root yet */
	if (st_cur.st_ino == st_root.st_ino &&
	    DEV_DEV_COMPARE(st_cur.st_dev, st_root.st_dev)) {
	    (void) strcpy(pathname, *pathptr != '/' ? "/" : pathptr);
	    return (pathname);
	}

	/* open the parent directory */
	if ((dp = opendir(nextpathptr)) == NULL) {
	    (void) xsprintf(pathname,
			    "getwd: Cannot open directory \"%s\" (%s)",
			    nextpathptr, strerror(errno));
	    return (NULL);
	}

	/* look in the parent for the entry with the same inode */
	for (d = readdir(dp); d != NULL; d = readdir(dp)) {
	    (void) strcpy(cur_name_add, d->d_name);
	    if (lstat(nextpathptr, &st_next) == -1) {
		(void) xsprintf(pathname, "getwd: Cannot stat \"%s\" (%s)",
				d->d_name, strerror(errno));
		return (NULL);
	    }
	    if (d->d_name[0] == '.' && d->d_name[1] == '\0')
		st_dot = st_next;

	    /* check if we found it yet */
	    if (st_next.st_ino == st_cur.st_ino &&
	    DEV_DEV_COMPARE(st_next.st_dev, st_cur.st_dev)) {
		st_cur = st_dot;
		pathptr = strrcpy(pathptr, d->d_name);
		pathptr = strrcpy(pathptr, "/");
		nextpathptr = strrcpy(nextpathptr, "../");
		*cur_name_add = '\0';
		(void) closedir(dp);
		break;
	    }
	}
	if (d == NULL) {
	    (void) xsprintf(pathname, "getwd: Cannot find \".\" in \"..\"");
	    return (NULL);
	}
    }
} /* end getwd */

/* strrcpy():
 *	Like strcpy, going backwards and returning the new pointer
 */
static char *
strrcpy(ptr, str)
    register char *ptr, *str;
{
    register int len = strlen(str);

    while (len)
	*--ptr = str[--len];

    return (ptr);
} /* end strrcpy */
# endif /* hp9000s500 */
#endif /* getwd */

#ifdef apollo
/***
 *** Domain/OS
 ***/
#undef value			/* XXX: Careful here */
#include <apollo/base.h>
#include <apollo/loader.h>
#include <apollo/error.h>


static char *
apperr(st)
    status_$t *st;
{
    static char buf[BUFSIZE];
    short e_subl, e_modl, e_codel;
    error_$string_t e_sub, e_mod, e_code;

    error_$get_text(*st, e_sub, &e_subl, e_mod, &e_modl, e_code, &e_codel);
    e_sub[e_subl] = '\0';
    e_code[e_codel] = '\0';
    e_mod[e_modl] = '\0';
    (void) xsprintf(buf, "%s (%s/%s)", e_code, e_sub, e_mod);

    return(buf);
}

static int
llib(s)
    Char *s;
{
    short len = Strlen(s);
    status_$t st;
    char *t;

    loader_$inlib(t = short2str(s), len, &st);
    if (st.all != status_$ok) 
	stderror(ERR_SYSTEM, t, apperr(&st));
}

/*ARGSUSED*/
void
doinlib(v, c)
    Char **v;
    struct command *c;
{
    setname(short2str(*v++));
    gflag = 0, tglob(v);
    if (gflag) {
	v = globall(v);
	if (v == 0)
	    stderror(ERR_NAME | ERR_NOMATCH);
    }
    else {
	v = gargv = saveblk(v);
	trim(v);
    }

    while (v && *v) 
	llib(*v++);
    if (gargv)
	blkfree(gargv), gargv = 0;
}

int
getv(v)
    Char *v;
{
    if (eq(v, STRbsd43))
	return(1);
    else if (eq(v, STRsys53))
	return(0);
    else 
	stderror(ERR_NAME | ERR_SYSTEM, short2str(v), "Invalid system type");
    /*NOTREACHED*/
    return(0);
}

/*ARGSUSED*/
void
dover(v, c)
    Char **v;
    struct command *c;
{
    Char *p;

    setname(short2str(*v++));
    if (!*v) {
	if (!(p = Getenv(STRSYSTYPE)))
	    stderror(ERR_NAME | ERR_STRING, "System type is not set");
	xprintf("%s\n", short2str(p));
    }
    else {
	Setenv(STRSYSTYPE, getv(*v) ? STRbsd43 : STRsys53);
	dohash(NULL, NULL);
    }
}

/*
 * Many thanks to rees@citi.umich.edu (Jim Rees) and
 *                mathys@ssdt-tempe.sps.mot.com (Yves Mathys)
 * For figuring out how to do this... I could have never done
 * it without their help.
 */
typedef short enum {
	name_$wdir_type,
	name_$ndir_type,
	name_$node_dir_type,
} name_$dir_type_t;

/*ARGSUSED*/
void
dorootnode(v, c)
    Char **v;
    struct command *c;
{
    name_$dir_type_t dirtype = name_$node_dir_type;
    uid_$t uid;
    status_$t st;
    char *name;
    short namelen;

    setname(short2str(*v++));

    name = short2str(*v);
    namelen = strlen(name);

    name_$resolve(name, &namelen, &uid, &st);
    if (st.all != status_$ok) 
	stderror(ERR_SYSTEM, name, apperr(&st));
    namelen = 0;
    name_$set_diru(&uid, "", &namelen, &dirtype, &st);
    if (st.all != status_$ok) 
	stderror(ERR_SYSTEM, name, apperr(&st));
    dohash(NULL, NULL);
}

int
isapad()
{
    static int res = -1;
    static status_$t st;

    if (res == -1) {
	int strm;
	if (isatty(0))
	    strm = 0;
	if (isatty(1))
	    strm = 1;
	if (isatty(2))
	    strm = 2;
	else {
	    res = 0;
	    st.all = status_$ok;
	    return(res);
	}
	res = stream_$isavt(&strm, &st);
	res = res ? 1 : 0;
    }
    else {
	if (st.all != status_$ok) 
	    stderror(ERR_SYSTEM, "stream_$isavt", apperr(&st));
    }
    return(res);
}
#endif
