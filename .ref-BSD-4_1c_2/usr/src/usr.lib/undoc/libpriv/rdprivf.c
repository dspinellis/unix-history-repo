/*	@(#)rdprivf.c	4.13	(Melbourne)	82/07/17	*/

#include <stdio.h>
#include <ctype.h>
#include <sys/vlimit.h>
#include <sys/types.h>
#include <sys/quota.h>
#include <udata.h>
#include <lpdquota.h>
#include <mushmuck.h>
#include <sys/stat.h>
#include <arrays.h>
#include <grp.h>

static	struct	udata		udata;
static	struct	mushmuck	mm;
static	struct	lpquota		lpdq;
static	struct	dquot		dqb, zdqb;

int	gsval(), glval(), gsecs(), gmins(), gdate(), gbits(), gflags(), ggrps(),
	gsflags(), gchar(), gdquot(), graise(), gmem(), gsoct(), gnolog();
long	number();

static FILE *fd;
static char lastqfs[32];

static	struct	kwtab	{
	char	*kwname;
	int	(*kwfunc)();
	long	kwdflt;
	char	*kwptr;
} kwtab[] = {
	"shares",	gsval,	20,		(char *)&mm.mm_qu.qu_shares,
	"plimit",	gsval,	25,		(char *)&mm.mm_qu.qu_plim,
	"class",	gbits,	0,		(char *)&mm.mm_qu.qu_class,
	"syflags",	gsflags, 0,		(char *)&mm.mm_qu.qu_syflags,
	"flags",	gflags,	0,		(char *)&udata.ud_flags[0],
	"ttyclass",	gbits,	~0,		(char *)&udata.ud_ttys,
	"maxlogin",	gsecs,	0,		(char *)&mm.mm_maxuse,
	"maxlsess",	gmins,	0,		(char *)&mm.mm_smax,
	"sessgap",	gmins,	0,		(char *)&mm.mm_lgap,
	"sessleft",	gmins,	0,		(char *)&mm.mm_left,
	"weeklmax",	gmins,	0,		(char *)&mm.mm_wmax,
	"weeklinc",	gmins,	0,		(char *)&mm.mm_winc,
	"weeklrem",	gmins,	0,		(char *)&mm.mm_wrem,
	"daylmax",	gmins,	0,		(char *)&mm.mm_dmax,
	"daylinc",	gmins,	0,		(char *)&mm.mm_dinc,
	"daylrem",	gmins,	0,		(char *)&mm.mm_drem,
	"expires",	gdate,	0,		(char *)&udata.ud_expires,
	"nologins",	gnolog,	0,		(char *)0,
	"maxsimul",	gchar,	INFINITY>>24,	(char *)&udata.ud_maxlogin,
	"disc",		gdquot,	0,		(char *)0,
	"disk",		gdquot,	0,		(char *)0,
	"prclass",	gbits,	~0,		(char *)&lpdq.lpq_prclass,
	"lpdwmax",	gsval,	0,		(char *)&lpdq.lpq_wmax,
	"lpdwinc",	gsval,	0,		(char *)&lpdq.lpq_allow,
	"lpdwrem",	gsval,	0,		(char *)&lpdq.lpq_week,
	"lpddmax",	gsval,	0,		(char *)&lpdq.lpq_dmax,
	"lpddinc",	gsval,	0,		(char *)&lpdq.lpq_daily,
	"lpddrem",	gsval,	0,		(char *)&lpdq.lpq_today,
	"lpdlimit",	glval,	0,		(char *)&lpdq.lpq_limit,
	"maxrss",	gmem,	INFINITY,	(char *)&udata.ud_maxrss,
	"maxdata",	gmem,	INFINITY,	(char *)&udata.ud_maxdata,
	"maxstack",	gmem,	512*1024,	(char *)&udata.ud_maxstack,
	"maxcore",	gmem,	INFINITY,	(char *)&udata.ud_maxcore,
	"maxfile",	gmem,	INFINITY,	(char *)&udata.ud_maxfile,
	"maxcpu",	gsecs,	INFINITY,	(char *)&udata.ud_maxcpu,
	"noraise",	graise,	0,		(char *)&udata.ud_raise,
	"umask",	gsoct,	0,		(char *)&udata.ud_umask,
	"groups",	ggrps,	0,		(char *)&udata.ud_grps,
	(char *)0
};

static	struct	syf	{
	char	*syfname;
	long	syfval;
} syf[] = {
	"modtty",	QF_MODTTY,
	"ttyfast",	QF_FASTTY,
	"nasync",	QF_NASYNC,
	"kasync",	QF_KASYNC,
	"noumask",	QF_UMASK,
	(char *)0
};

static	struct	flg	{
	char	*flgname;
	enum	u_priv	flgval;
} flg[] = {
	"usecu",	USE_REMOTE,
	"useuucp",	USE_UUCP,
	"tutor",	TUTOR,
	"supertute",	SUPER_TUTE,
	"student",	STUDENT,
	"stumail",	GET_STU_MAIL,
	"mailstu",	SEND_STU_MAIL,
	"uucpmail",	UUCP_MAIL,
	"nonewgrp",	NO_NEWGRP,
	"studsu",	STUD_SU,
	(char *)0
};

static struct vnm {
	char *valname;
	long  valval[4];
} vnm[] = {
		/* char data */ /* short data */		/* long data */
	"zero",		0,		0,		0,		0,
	"none",		0,		0,		0,		0,
	"huge",	INFINITY>>24,	INFINITY>>16,		0,	 INFINITY,
	"inf",	INFINITY>>24,	INFINITY>>16,		0,	 INFINITY,
	"infinite", INFINITY>>24, INFINITY>>16,		0,	 INFINITY,
	"unlimited", INFINITY>>24, INFINITY>>16,	0,	 INFINITY,
	"all",		~0,		~0,		0,		~0,
	"any",		~0,		~0,		0,		~0,
	(char *)0
};


rdprivf(name, mp, lp, dp, dnp, up)
char *name;
struct mushmuck *mp;
struct lpquota *lp;
struct dquot *dp;
char (*dnp)[32];
struct udata *up;
{
	register struct kwtab *k;
	char	keyw[32];
	struct	kwtab *lookup();
	register n;

	privzero(&mm, sizeof mm);
	privzero(&lpdq, sizeof lpdq);
	privzero(&udata, sizeof udata);

	for (k = kwtab; k->kwname; k++)
		if (k->kwdflt)		/* default isn't zero */
			(*k->kwfunc)(k, k->kwdflt);

	if ((fd = fopen(name, "r")) != NULL) {
		n = 1;
		while (!feof(fd)) {
			if (fscanf(fd, "%30s", keyw) == 1) {
				if (k = lookup(keyw)) {
					(*k->kwfunc)(k, 0);
					if (k->kwfunc == gdquot && dp &&
					    lastqfs[0]) {
						*dp++ = dqb;
						if (dnp) {
							strcpy(dnp, lastqfs);
							dnp++;
						}
					}
				}
			} 
			while (getc(fd) != '\n' && !feof(fd))
				;
		}
		fclose(fd);
		fd = NULL;
	} else
		n = 0;
	if (mp)
		*mp = mm;
	if (lp)
		*lp = lpdq;
	if (up)
		*up = udata;
	if (dp)
		*dp = zdqb;
	if (dnp)
		dnp[0][0] = 0;
	return(n);
}

static struct kwtab *
lookup(s)
register char *s;
{
	register struct kwtab *k;

	for (k = kwtab; k->kwname; k++)
		if (strcmp(k->kwname, s) == 0)
			return(k);
	return((struct kwtab *)0);
}

static
glval(k, x)
struct kwtab *k;
long x;
{
	if (x == 0 && (nextchar(1) == '\n' || ispunct(nextchar(1))))
		return;
	if (x == 0 && isalpha(nextchar(1)))
		x = valuename(sizeof(long));
	*(long *)k->kwptr = x ? x : number(10);
}

static
gsval(k, x)
struct kwtab *k;
long x;
{
	if (x == 0 && (nextchar(1) == '\n' || ispunct(nextchar(1))))
		return;
	if (x == 0 && isalpha(nextchar(1)))
		x = valuename(sizeof(short));
	*(short *)k->kwptr = x ? x : number(10);
}

static
gchar(k, x)
struct kwtab *k;
long x;
{
	if (x == 0 && (nextchar(1) == '\n' || ispunct(nextchar(1))))
		return;
	if (x == 0 && isalpha(nextchar(1)))
		x = valuename(sizeof(char));
	*(char *)k->kwptr = x ? x : number(10);
}

static
gsoct(k, x)
struct kwtab *k;
long x;
{
	if (x == 0 && (nextchar(1) == '\n' || ispunct(nextchar(1))))
		return;
	if (x == 0 && isalpha(nextchar(1)))
		x = valuename(sizeof(short));
	*(short *)k->kwptr = x ? x : number(8);
}

static
gmem(k, x)
struct kwtab *k;
long x;
{
	register long n;

	if (x == 0 && (nextchar(1) == '\n' || ispunct(nextchar(1))))
		return;
	if (x == 0 && isalpha(nextchar(1)))
		x = valuename(sizeof(long));
	n = x ? x : number(10);
	if (x == 0) {
		if (nextchar(1) == 'k' || nextchar(1) == 'K')
			n *= 1024;
		else if (nextchar(1) == 'm' || nextchar(1) == 'M')
			n *= 1024 * 1024;
	}
	*(long *)k->kwptr = n;
}

static
gsecs(k, x)
struct kwtab *k;
long x;
{
	register long secs;

	if (x == 0 && (nextchar(1) == '\n' || ispunct(nextchar(1))))
		return;
	if (x == 0 && isalpha(nextchar(1)))
		x = valuename(sizeof(long));
	secs = x ? x : number(10);
	if (x == 0 && nextchar(1) == ':') {
		getc(fd);
		secs = secs * 60 + number(10);
		if (nextchar(1) == ':') {
			getc(fd);
			secs = secs * 60 + number(10);
		}
	}
	*(long *)k->kwptr = secs;
}

static
gmins(k, x)
struct kwtab *k;
long x;
{
	register mins;

	if (x == 0 && (nextchar(1) == '\n' || ispunct(nextchar(1))))
		return;
	if (x == 0 && isalpha(nextchar(1)))
		x = valuename(sizeof(long));
	mins = x ? x : number(10);
	if (x == 0 && nextchar(1) == ':') {
		getc(fd);
		mins = mins * 60 + number(10);
	}
	*(short *)k->kwptr = mins;
}

static
gbits(k, x)
struct kwtab *k;
long x;
{
	register long bits;

	if ((bits = x) == 0)
		while (islower(nextchar(1)))
			bits |= 1 << (getc(fd) - 'a');
	*(long *)k->kwptr = bits;
}

static
gnolog()
{
	static char *dofw[] = {
		"sun", "mon", "tue", "wed", "thu", "fri", "sat",
		(char *)0
	};
	char line[256];
	register t_range *rp = udata.ud_logon;
	register char *p, *q;
	register c, i;
	register char **dp;
	register haddays = 0, hadtimes = 0, okflag = 1, wantmore;

	do {
		if (fgets(line, sizeof line, fd) == NULL)
			return;
		for (p = line; *p; p++)
			if (isupper(*p))
				*p = tolower(*p);
		p = line;
		wantmore = 0;

		while (*p && *p != '\n' &&
		    (*p == ',' || *p == '-' || !ispunct(*p))) {
			if (isspace(*p) || *p == '-') {
				p++;
				continue;
			}
			if (*p == ',') {
				if (haddays || hadtimes) {
					complete(rp, haddays, hadtimes, okflag);
					haddays = hadtimes = 0;
					rp++;
				}
				p++;
				wantmore++;
				continue;
			}
			wantmore = 0;
			if (rp >= &udata.ud_logon[4])
				break;
			for (q = p; isalpha(*q); q++)
				;
			if (q > p) {
				c = *q;
				*q = 0;
				if (strcmp(p, "except") == 0) {
					if (haddays || hadtimes) {
						complete(rp, haddays, hadtimes,
						    okflag);
						haddays = hadtimes = 0;
						rp++;
					}
					if (rp == udata.ud_logon) {
						complete(rp, 0, 0, okflag);
						rp++;
					}
					okflag ^= 1;
					*q = c;
					p = q;
					continue;
				}
				for (i = 0, dp = dofw; *dp; dp++, i++)
					if (strcmp(p, *dp) == 0)
						break;
				if (i >= 7)
					break;
				if (haddays > 1)
					break;
				if (haddays++)
					rp->tr_high.xt_day = i;
				else
					rp->tr_low.xt_day = i;
				*q = c;
				p = q;
				continue;
			}
			for (q = p; isdigit(*q); q++)
				;
			if (q > p) {
				if (hadtimes > 1)
					break;
				i = atoi(p) * 60;
				if (*q == ':') {
					i += atoi(q+1);
					while (isdigit(*++q))
						;
				}
				if (i < 0 || i > 24*60)
					break;
				if (hadtimes++)
					rp->tr_high.xt_min = i;
				else
					rp->tr_low.xt_min = i;
				p = q;
				continue;
			}
			break;
		}
	} while (wantmore);
	if (haddays || hadtimes) {
		complete(rp, haddays, hadtimes, okflag);
		rp++;
	}
	if (rp < &udata.ud_logon[4])
		rp->tr_low.xt_ok = rp->tr_high.xt_ok = 0;
}

static
complete(rp, hd, ht, ok)
register t_range *rp;
{
	switch (hd) {

	case 0:
	default:
		rp->tr_low.xt_day = 0;
		rp->tr_high.xt_day = 6;
		break;

	case 1:
		rp->tr_high.xt_day = rp->tr_low.xt_day;
		break;
	
	case 2:
		break;

	}

	switch (ht) {

	case 0:
	default:
		rp->tr_low.xt_min = 0;
		rp->tr_high.xt_min = 24*60;
		break;
	
	case 1:
		rp->tr_high.xt_min = rp->tr_low.xt_min;
		break;
	
	case 2:
		break;

	}

	rp->tr_low.xt_ok = ok;
	rp->tr_high.xt_ok = !ok;		/* validity check */
	ungetc('\n', fd);
}

static
gdate(k)
struct kwtab *k;
{
	register time_t date;
	register long n;
	register char *p;
	char *index();
	char line[256];

	date = 0;
	fgets(line, sizeof line, fd);
	if (p = index(line, '\n')) {
		*p = 0;
		ungetc('\n', fd);
	}
	if (p = index(line, '#'))
		*p = 0;
	if (p = index(line, ';'))
		*p = 0;
	if ((p = index(line, '/')) && p[1] == '*')
		*p = 0;
	p = line;
	while (isspace(*p))
		p++;

	if (strcmp(p, "never") == 0)		/* we don't really need this */
		goto xit;		/* date errs (eg: never) do the same */

	if (strncmp(p, "in", 2) == 0 || *p == '+') {
		p += (*p != '+') + 1;
		for (;;) {
			static struct scales {
				char *sname;
				long	scale;
			} scales[] = {
				"y",	365*24*60*60,
				"year",	365*24*60*60,
				"yr",	365*24*60*60,
				"mon",	30*24*60*60,
				"month",30*24*60*60,
				"m",	30*24*60*60,
				"week",	7*24*60*60,
				"wk",	7*24*60*60,
				"day",	24*60*60,
				"dy",	24*60*60,
				"d",	24*60*60,
				"hour",	60*60,
				"hr",	60*60,
				"h",	60*60,
				"min",	60,
				"minute",60,
				"second",1,
				"sec",	1,
				"s",	1,
				(char *)0
			};
			register struct scales *s;

			while (isspace(*p))
				p++;
			if (!isdigit(*p))
				break;
			n = atoi(p);
			while (isdigit(*p))
				p++;
			while (isspace(*p))
				p++;
			for (s = scales; s->sname; s++) {
				if (strncmp(s->sname,p,strlen(s->sname)) == 0) {
					n *= s->scale;
					p += strlen(s->sname);
					if (*p == 's')
						p++;
					break;
				}
			}
			date += n;
		}
		date += time((time_t *)0);
	} else {
		if (strncmp(p, "on", 2) == 0)
			p += 2;
		date = asctotime(p, 0);
		if (date == -1)		/* errors mean no date */
			date = 0;
	}
 xit:
	*(long *)k->kwptr = date;
}

static
gdquot()
{
	register char *p, *q;
	char line[256];
	char qfile[128];
	struct stat statb;

	privzero(&dqb, sizeof dqb);

	fgets(line, sizeof line, fd);
	p = line;
	while (isspace(*p))
		p++;
	q = p;
	while (!isspace(*p) && *p)
		p++;
	*p++ = 0;
	lastqfs[0] = 0;
	if (strlen(q) > 32)
		return;
	strcpy(lastqfs, q);
	strcpy(qfile, q);
	strcat(qfile, "/quota");
	if (stat(qfile, &statb) >= 0) {
		dqb.dq_dev = statb.st_dev;
		if (sscanf(p,
		    " blks = %hd blim = %hd iq = %hd ilim = %hd"
			, &dqb.dq_quot
			, &dqb.dq_blim
			, &dqb.dq_iq
			, &dqb.dq_ilim
		) != 4)
			lastqfs[0] = 0;
	}
	ungetc('\n', fd);
}

static
gflags(k)
struct kwtab *k;
{
	register long f = 0;
	register struct flg *fp;
	char option[60];

	if (nextchar(1) == '\n' || ispunct(nextchar(1)))
		return;
	while (fscanf(fd, " %50[abcdefghijklmnopqrstuvwxyz]", option) == 1) {
		for (fp = flg; fp->flgname; fp++)
			if(strcmp(option, fp->flgname) == 0)
				f |= 1 << (int)fp->flgval;
		if (nextchar(1) == ',') {
			getc(fd);
			continue;
		}
		break;
	}
	*(long *)k->kwptr = f;
}

static
gsflags(k)
struct kwtab *k;
{
	register long f = 0;
	register struct syf *fp;
	char option[60];

	if (nextchar(1) == '\n' || ispunct(nextchar(1)))
		return;
	while (fscanf(fd, " %50[abcdefghijklmnopqrstuvwxyz]", option) == 1) {
		for (fp = syf; fp->syfname; fp++)
			if(strcmp(option, fp->syfname) == 0)
				f |= fp->syfval;
		if (nextchar(1) == ',') {
			getc(fd);
			continue;
		}
		break;
	}
	*(long *)k->kwptr = f;
}

static
graise(k)
struct kwtab *k;
{
	*(long *)k->kwptr = 1;
}

static
ggrps(k)
struct kwtab *k;
{
	register long grps = 0;
	register struct group *gp;
	register char *p;
	char gpname[32];

	if (nextchar(1) == '\n' || ispunct(nextchar(1)))
		return;
	setgrent();
	while (fscanf(fd, " %30[abcdefghijklmnopqrstuvwxyz0123456789]",
	    gpname) == 1) {
		gp = getgrnam(gpname);
		if (gp) {
			if (gp->gr_gid >= 128 && gp->gr_gid < 128+32)
				grps |= 1 << (gp->gr_gid - 128);
		}
		if (nextchar(1) == ',') {
			getc(fd);
			continue;
		}
		break;
	}
	endgrent();
	*(long *)k->kwptr = grps;
}

static long
number(base)
register base;
{
	register long n;

	n = 0;
	nextchar(1);
	while (isdigit(nextchar(0)))
		n = n * base + getc(fd) - '0';
	return (n);
}

static long
valuename(size)
{
	register long n;
	register struct vnm *vp;
	char lbuf[60];
	register char *p = lbuf;

	n = 0;
	nextchar(1);
	while (p < last(lbuf) && isalpha(nextchar(0)))
		if (isupper(nextchar(0)))
			*p++ = tolower(getc(fd));
		else
			*p++ = getc(fd);
	*p = 0;

	for (vp = vnm; vp->valname; vp++)
		if (strcmp(lbuf, vp->valname) == 0) {
			n = vp->valval[size-1];
			break;
		}

	return(n);
}


static
nextchar(s)
register s;
{
	register c;

	while (isspace(c = getc(fd)) && c != '\n' && s)
		;
	ungetc(c, fd);
	return(c);
}
