/*	@(#)wrtprivf.c	4.9	(Melbourne)	82/07/17	*/

#include <stdio.h>
#include <ctype.h>
#include <sys/vlimit.h>
#include <sys/types.h>
#include <sys/quota.h>
#include <udata.h>
#include <lpdquota.h>
#include <mushmuck.h>
#include <grp.h>

static	struct	udata		udata;
static	struct	mushmuck	mm;
static	struct	lpquota		lpdq;
static	struct	dquot		dqb[16];
static	char			dqfn[16][32];

int	psval(), plval(), psecs(), pmins(), pdate(), pbits(), pflags(), pgrps(),
	psflags(), pchar(), pdquot(), praise(), pmem(), psoct(), pnolog();

static FILE *fd;

static	struct	kwtab	{
	char	*kwname;
	int	(*kwfunc)();
	char	*kwcmt;
	char	*kwptr;
} kwtab[] = {
	"shares",	psval,	"",			(char *)&mm.mm_qu.qu_shares,
	"plimit",	psval,	"max number of processes",(char *)&mm.mm_qu.qu_plim,
	"class",	pbits,	"user classes",		(char *)&mm.mm_qu.qu_class,
	"groups",	pgrps,	"system file access groups",	(char *)&udata.ud_grps,
	"umask",	psoct,	"initial umask",		(char *)&udata.ud_umask,
	"syflags",	psflags, "system flags",	(char *)&mm.mm_qu.qu_syflags,
	"flags",	pflags,	"other flags",		(char *)&udata.ud_flags[0],
	"ttyclass",	pbits,	"permitted terminal classes",		(char *)&udata.ud_ttys,
	"expires",	pdate,	"Account expiry date",		(char *)&udata.ud_expires,
	"nologins",	pnolog,	"",				(char *)0,
	"maxsimul",	pchar,	"maximum simultaneous logins",	(char *)&udata.ud_maxlogin,
	"maxlogin",	psecs,	"maximum login time (hr:min:sec)",		(char *)&mm.mm_maxuse,
	"maxlsess",	pmins,	"maximum login session (hr:min)",		(char *)&mm.mm_smax,
	"sessgap",	pmins,	"gap between sessions (hr:min)",		(char *)&mm.mm_lgap,
	"sessleft",	pmins,	"time left in current session (hr:min)",	(char *)&mm.mm_left,
	"weeklmax",	pmins,	"max week's login time (hr:min)",		(char *)&mm.mm_wmax,
	"weeklinc",	pmins,	"weekly login allowance (hr:min)",		(char *)&mm.mm_winc,
	"weeklrem",	pmins,	"weekly allowance remaining (hr:min)",	(char *)&mm.mm_wrem,
	"daylmax",	pmins,	"Max day's login time (hr:min)",		(char *)&mm.mm_dmax,
	"daylinc",	pmins,	"daily login allowance (hr:min)",		(char *)&mm.mm_dinc,
	"daylrem",	pmins,	"daily allowance remaining (hr:min)",	(char *)&mm.mm_drem,
	"prclass",	pbits,	"permitted printer classes",		(char *)&lpdq.lpq_prclass,
	"lpdwmax",	psval,	"maximum pages in a week",		(char *)&lpdq.lpq_wmax,
	"lpdwinc",	psval,	"weekly page allowance",		(char *)&lpdq.lpq_allow,
	"lpdwrem",	psval,	"pages left this week",			(char *)&lpdq.lpq_week,
	"lpddmax",	psval,	"maximum pages in a day",		(char *)&lpdq.lpq_dmax,
	"lpddinc",	psval,	"daily page allowance",		(char *)&lpdq.lpq_daily,
	"lpddrem",	psval,	"pages left today",		(char *)&lpdq.lpq_today,
	"lpdlimit",	plval,	"absolute page limit",		(char *)&lpdq.lpq_limit,
	"maxrss",	pmem,	"expected largest proc size",	(char *)&udata.ud_maxrss,
	"maxdata",	pmem,	"maximum data segment",	(char *)&udata.ud_maxdata,
	"maxstack",	pmem,	"maximum stack segment",	(char *)&udata.ud_maxstack,
	"maxcore",	pmem,	"largest core file generated",	(char *)&udata.ud_maxcore,
	"maxfile",	pmem,	"largest file allowed",	(char *)&udata.ud_maxfile,
	"maxcpu",	psecs,	"cpu time limit (hr:min:sec)",	(char *)&udata.ud_maxcpu,
	"noraise",	praise,	"can't raise maximums",		(char *)&udata.ud_raise,
	"disc",		pdquot,	"",				(char *)0,
	(char *)0
};

static	struct	syf	{
	char	*syfname;
	char	*syfcmt;
	long	syfval;
} syf[] = {
	"modtty",	"modify terminals other than login tty",	QF_MODTTY,
	"ttyfast",	"increase tty speed > 1200 baud",	QF_FASTTY,
	"nasync",	"remaining procs niced at logout",	QF_NASYNC,
	"kasync",	"remaining procs killed at logout",	QF_KASYNC,
	"noumask",	"not permitted to alter umask",	QF_UMASK,
	(char *)0
};

static	struct	flg	{
	char	*flgname;
	char	*flgcmt;
	enum	u_priv	flgval;
} flg[] = {
	"usecu",	"use 'cu' to connect to remote machine",	USE_REMOTE,
	"useuucp",	"use 'uucp' to transfer files",	USE_UUCP,
	"tutor",	"various tutor priveleges",	TUTOR,
	"supertute",	"special tutor privs (incl modify privs)",	SUPER_TUTE,
	"student",	"account is a student (misc restrictions)",	STUDENT,
	"stumail",	"students permitted to send mail to user",	GET_STU_MAIL,
	"mailstu",	"permitted to send mail to students",	SEND_STU_MAIL,
	"uucpmail",	"permitted to mail to remote machines",	UUCP_MAIL,
	"nonewgrp",	"not permitted to alter login group",	NO_NEWGRP,
	"studsu",	"students can 'su' to this account",	STUD_SU,
	(char *)0
};

static struct vnm {
	char *valname;
	long  valval[4];
} vnm[] = {
		/* char data */ /* short data */		/* long data */
	"none",		0,		0,		0,		0,
	"unlimited", INFINITY>>24, INFINITY>>16,	0,	 INFINITY,
	"all",		~0,		~0,		0,		~0,
	(char *)0
};

wrtprivf(name, mp, lp, dp, dnp, up)
char *name;
struct mushmuck *mp;
struct lpquota *lp;
struct dquot *dp;
char (*dnp)[32];
struct udata *up;
{
	register struct kwtab *k;
	char	keyw[32];
	register n;

	udata = *up;
	mm = *mp;
	lpdq = *lp;
	for (n = 0; n < 16; n++) {
		if (dnp[0][0]) {
			strcpy(dqfn[n], dnp[0]);
			dqb[n] = *dp++;
			dnp++;
		} else
			break;
	}
	if (n < 16)
		dqfn[n][0] = 0;

	if ((fd = fopen(name, "w")) != NULL) {
		for (k = kwtab; k->kwname; k++)
			(*k->kwfunc)(k);
		fclose(fd);
		fd = NULL;
		return(0);
	} else
		return(-1);
}

static
plval(k)
struct kwtab *k;
{
	if (nameval(k, *(long *)k->kwptr, sizeof(long)))
		return;
	fprintf(fd,
		"%s %ld		/* %s */\n"
		, k->kwname
		, *(long *)k->kwptr
		, k->kwcmt
	);
}

static
psval(k)
struct kwtab *k;
{
	if (nameval(k, (long)*(short *)k->kwptr, sizeof(short)))
		return;
	fprintf(fd,
		"%s %d		/* %s */\n"
		, k->kwname
		, *(short *)k->kwptr
		, k->kwcmt
	);
}

static
pchar(k)
struct kwtab *k;
{
	if (nameval(k, (long)*(char *)k->kwptr, sizeof(char)))
		return;
	fprintf(fd,
		"%s %d		/* %s */\n"
		, k->kwname
		, *(char *)k->kwptr
		, k->kwcmt
	);
}

static
psoct(k)
struct kwtab *k;
{
	if (nameval(k, (long)*(short *)k->kwptr, sizeof(short)))
		return;
	fprintf(fd,
		"%s %o		/* %s */\n"
		, k->kwname
		, *(short *)k->kwptr
		, k->kwcmt
	);
}

static
pmem(k)
struct kwtab *k;
{
	register long n;

	n = *(long *)k->kwptr;
	if (nameval(k, n, sizeof(long)))
		return;
	fprintf(fd, "%s ", k->kwname);
	if (n && (n & (1 << 20)-1) == 0)
		fprintf(fd, "%ldM", n>>20);
	else if (n && (n & (1 << 10)-1) == 0)
		fprintf(fd, "%ldK", n>>10);
	else
		fprintf(fd, "%ld", n);
	fprintf(fd, "		/* %s */\n", k->kwcmt);
}

static
psecs(k)
struct kwtab *k;
{
	register long secs;

	secs = *(long *)k->kwptr;
	if (nameval(k, secs, sizeof(long)))
		return;
	fprintf(fd,
		"%s %ld:%02ld:%02ld		/* %s */\n"
		, k->kwname
		, secs / 3600
		, (secs % 3600) / 60
		, secs % 60
		, k->kwcmt
	);
}

static
pmins(k)
struct kwtab *k;
{
	register mins;

	mins = *(short *)k->kwptr;
	if (nameval(k, (long)mins, sizeof(short)))
		return;
	fprintf(fd,
		"%s %d:%02d		/* %s */\n"
		, k->kwname
		, mins / 60
		, mins % 60
		, k->kwcmt
	);
}

static
pbits(k)
struct kwtab *k;
{
	register unsigned long bits;
	register ch = 'a';

	bits = *(long *)k->kwptr;
	fprintf(fd, "%s ", k->kwname);
	while (bits) {
		if (bits & 1)
			putc(ch, fd);
		bits >>= 1;
		ch++;
		if (ch > 'z')
			break;
	}
	fprintf(fd, "		/* %s */\n", k->kwcmt);
}

pnolog(k)
struct kwtab *k;
{
	register t_range *rp = udata.ud_logon;
	register ok = 1;
	static char *dofw[] = {
		"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", ""
	};

	if (rp->tr_low.xt_ok == rp->tr_high.xt_ok)
		return;

	fprintf(fd, "%s ", k->kwname);

	for (;;) {
		if (rp->tr_low.xt_ok != ok)
			fprintf(fd, "except ");
		ok = rp->tr_low.xt_ok;
		fprintf(fd, "%s ", dofw[rp->tr_low.xt_day]);
		if (rp->tr_low.xt_day != rp->tr_high.xt_day)
			fprintf(fd, "- %s ", dofw[rp->tr_high.xt_day]);
		if (rp->tr_low.xt_min != 0 || rp->tr_high.xt_min != 24*60)
			fprintf(fd, "%02d:%02d-%02d:%02d "
			    , rp->tr_low.xt_min/60
			    , rp->tr_low.xt_min % 60
			    , rp->tr_high.xt_min/60
			    , rp->tr_high.xt_min % 60
			);
		if (++rp >= &udata.ud_logon[4] ||
		    rp->tr_low.xt_ok == rp->tr_high.xt_ok)
			break;
		fprintf(fd, ", ");
	}
	fprintf(fd, "	/* %s */\n", k->kwcmt);
}
		

static
pdate(k)
struct kwtab *k;
{
	time_t date;
	char *ctime();

	date = *(long *)k->kwptr;
	fprintf(fd,
		"%s %.20s		/* %s */\n"
		, k->kwname
		, date ? ctime(&date)+4 : "never"
		, k->kwcmt
	);
}

static
pdquot()
{
	register char (*fs)[32];
	register struct dquot *dqp = dqb;

	for (fs = dqfn; fs[0][0]; fs++) {
		fprintf(fd,
		    "disc %.32s blks = %d blim = %d iq = %d ilim = %d\n"
			, fs[0]
			, dqp->dq_quot
			, dqp->dq_blim
			, dqp->dq_iq
			, dqp->dq_ilim
		);
		dqp++;
	}
}

static
pflags(k)
struct kwtab *k;
{
	register long f;
	register struct flg *fp;
	register n = 0;

	f = *(long *)k->kwptr;

	fprintf(fd, "flags");
	for (fp = flg; fp->flgname; fp++)
		if (f & (1 << (int)fp->flgval))
			fprintf(fd, "%s %s", n++ ? "," : "", fp->flgname);
	fprintf(fd, "	/* %s */\n", k->kwcmt);
}

static
psflags(k)
struct kwtab *k;
{
	register long f;
	register struct syf *fp;
	register n = 0;

	f = *(long *)k->kwptr;

	fprintf(fd, "syflags");
	for (fp = syf; fp->syfname; fp++)
		if (f & fp->syfval)
			fprintf(fd, "%s %s", n++ ? "," : "", fp->syfname);
	fprintf(fd, "	/* %s */\n", k->kwcmt);
}

static
praise(k)
struct kwtab *k;
{
	if (*(long *)k->kwptr)
		fprintf(fd,
		    "%s			/* %s */\n"
			, k->kwname
			, k->kwcmt
		);
}

nameval(k, val, size)
struct kwtab *k;
register long val;
register size;
{
	register struct vnm *vp;

	for (vp = vnm; vp->valname; vp++)
		if (val == vp->valval[size-1]) {
			fprintf(fd, "%s %s		/* %s */\n"
				, k->kwname
				, vp->valname
				, k->kwcmt
			);
			return(1);
		}
	return(0);
}

static
pgrps(k)
struct kwtab *k;
{
	register int n;
	register long grps;
	register struct group *gp;
	register char *sep = "";

	grps = *(long *)k->kwptr;

	fprintf(fd, "%s", k->kwname);
	setgrent();
	for (n = 128; n < 128+32; n++) {	/* much magic here */
		if (grps & 1) {
			gp = getgrgid(n);
			if (gp) {
				fprintf(fd, "%s %s", sep, gp->gr_name);
				sep = ",";
			}
		}
		grps >>= 1;	/* any sort of shift will do */
	}
	endgrent();
	fprintf(fd, "		/* %s */\n", k->kwcmt);
}
