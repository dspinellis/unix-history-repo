#ifndef lint
static char sccsid[] = "@(#)conn.c	5.3 (Berkeley) %G%";
#endif

#include "uucp.h"
#include <signal.h>
#include <setjmp.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/time.h>
#include <errno.h>
#ifdef	SYSIII
#include <termio.h>
#include <fcntl.h>
#endif
#ifndef	SYSIII
#include <sgtty.h>
#endif

#define MAXC 1000

extern jmp_buf Sjbuf;
extern int errno;

/* Parity control during login procedure */
#define	P_ZERO	0
#define	P_ONE	1
#define	P_EVEN	2
#define	P_ODD	3
char	par_tab[128];	/* must be power of two */

int next_fd = -1;	/* predicted fd to close interrupted opens */
				/* rti!trt, courtesy unc!smb */
/***
 *	alarmtr()  -  catch alarm routine for "expect".
 */
alarmtr()
{
	signal(SIGALRM, alarmtr);
	if (next_fd >= 0) {
		if (close(next_fd))
			logent("FAIL", "ACU LINE CLOSE");
		next_fd = -1;
	}
	longjmp(Sjbuf, 1);
}

/*******
 *	conn(system)
 *	char *system;
 *
 *	conn - place a telephone call to system and
 *	login, etc.
 *
 *	return codes:
 *		CF_SYSTEM: don't know system
 *		CF_TIME: wrong time to call
 *		CF_DIAL: call failed
 *		CF_NODEV: no devices available to place call
 *		CF_LOGIN: login/password dialog failed
 *
 *		>0  - file no.  -  connect ok
 *
 */

int Dcf = -1;

conn(system)
char *system;
{
	int ret, nf;
	register int fn, fnd;
	char info[MAXC], *flds[MAXC/10];
	register FILE *fsys;
	int fcode = 0;

	nf = 0;
	fnd = 0;


	fsys = fopen(SYSFILE, "r");
	ASSERT(fsys != NULL, "CAN'T OPEN", SYSFILE, 0);

	DEBUG(4, "finds %s\n", "called");
	while((nf = finds(fsys, system, info, flds)) > 0) {
		DEBUG(4, "getto %s\n", "called");
		if ((fn = getto(flds)) > 0) {
			fnd = 1;
			Dcf = fn;
			break;
		}
		fcode = (fn == FAIL ? CF_DIAL : fn);
	}
	fclose(fsys);

	if (nf <= 0)
		return(fcode ? fcode : nf);

	DEBUG(4, "login %s\n", "called");
	ret = login(nf, flds, fn);
	if (ret < 0) {
		clsacu();
		return(CF_LOGIN);
	}
	/* rti!trt:  avoid passing file to children */
	fioclex(fn);
	return(fn);
}

/***
 *	getto(flds)		connect to remote machine
 *	char *flds[];
 *
 *	return codes:
 *		>0  -  file number - ok
 *		FAIL  -  failed
 */

getto(flds)
register char *flds[];
{
	register struct condev *cd;
	int nulldev(), diropn();

	DEBUG(4, "call: no. %s ", flds[F_PHONE]);
	DEBUG(4, "for sys %s\n", flds[F_NAME]);

	CU_end = nulldev;
	for (cd = condevs; cd->CU_meth != NULL; cd++) {
		if (snccmp(cd->CU_meth, flds[F_LINE]) == SAME) {
			DEBUG(4, "Using %s to call\n", cd->CU_meth);
			return((*(cd->CU_gen))(flds));
			}
		}
	logent(flds[F_LINE], "getto: Can't find, using DIR");
	return(diropn(flds));	/* search failed, so use direct */
	}

/***
 *	clsacu()	close call unit
 *
 *	return codes:  none
 */

int (*CU_end)() = nulldev;
clsacu()
{
	(*(CU_end))(Dcf);
	if (close(Dcf) == 0) {
		DEBUG(4, "fd %d NOT CLOSED by CU_clos\n", Dcf);
		logent("clsacu", "NOT CLOSED by CU_clos");
	}
	Dcf = -1;
	CU_end = nulldev;
}

/***
 *	exphone - expand phone number for given prefix and number
 *
 *	return code - none
 */

exphone(in, out)
register char *in, *out;
{
	FILE *fn;
	char pre[MAXPH], npart[MAXPH], tpre[MAXPH], p[MAXPH];
	char buf[BUFSIZ];
	register char *s1;

	if (!isalpha(*in)) {
		strcpy(out, in);
		return;
	}

	s1=pre;
	while (isalpha(*in))
		*s1++ = *in++;
	*s1 = '\0';
	s1 = npart;
	while (*in != '\0')
		*s1++ = *in++;
	*s1 = '\0';

	tpre[0] = '\0';
	if ((fn = fopen(DIALFILE, "r")) == NULL)
		DEBUG(2, "CAN'T OPEN %s\n", DIALFILE);
	else {
		while (cfgets(buf, BUFSIZ, fn)) {
			sscanf(buf, "%s%s", p, tpre);
			if (strcmp(p, pre) == SAME)
				goto found;
			tpre[0] = '\0';
		}
		DEBUG(2, "CAN'T FIND dialcodes prefix '%s'\n", pre);
	found:;
		fclose(fn);
	}

	strcpy(out, tpre);
	strcat(out, npart);
	return;
}

/***
 *	rddev - read and decode a line from device file
 *
 *	return code - FAIL at end-of file; 0 otherwise
 */

rddev(fp, dev)
register struct Devices *dev;
FILE *fp;
{
	char *fdig();
	char buf[BUFSIZ];
	int na;

	if (!cfgets(buf, BUFSIZ, fp))
		return(FAIL);

	na = sscanf(buf, "%s%s%s%s%s", dev->D_type, dev->D_line,
	  dev->D_calldev, dev->D_class, dev->D_brand);
	ASSERT(na >= 4, "BAD DEVICE ENTRY", buf, 0);
	if (na != 5) dev->D_brand[0] = '\0';
	dev->D_speed = atoi(fdig(dev->D_class));
	return(0);
}

/***
 *	finds(fsys, sysnam, info, flds)	set system attribute vector
 *
 *	return codes:
 *		>0  -  number of arguments in vector - succeeded
 *		CF_SYSTEM  -  system name not found
 *		CF_TIME  -  wrong time to call
 */

finds(fsys, sysnam, info, flds)
char *sysnam, info[], *flds[];
FILE *fsys;
{
	char sysn[8];
	int na;
	int fcode = 0;

	/* format of fields
	 *	0 name;
	 *	1 time
	 *	2 acu/hardwired
	 *	3 speed
	 *	etc
	 */
	while (cfgets(info, MAXC, fsys) != NULL) {
		na = getargs(info, flds);
		sprintf(sysn, "%.7s", flds[F_NAME]);
		if (strcmp(sysnam, sysn) != SAME)
			continue;
		if (ifdate(flds[F_TIME]))
			/*  found a good entry  */
			return(na);
		DEBUG(2, "Wrong time ('%s') to call\n", flds[F_TIME]);
		fcode = CF_TIME;
	}
	return(fcode ? fcode : CF_SYSTEM);
}

/***
 *	login(nf, flds, dcr)		do login conversation
 *	char *flds[];
 *	int nf;
 *
 *	return codes:  0  |  FAIL
 */

login(nf, flds, fn)
register char *flds[];
int nf, fn;
{
	register char *want, *altern;
	extern char *index();
	int k, ok;

	ASSERT(nf > 4, "TOO FEW LOG FIELDS", "", nf);
	for (k = F_LOGIN; k < nf; k += 2) {
		want = flds[k];
		ok = FAIL;
		while (ok != 0) {
			altern = index(want, '-');
			if (altern != NULL)
				*altern++ = '\0';
			DEBUG(4, "wanted %s ", want);
			ok = expect(want, fn);
			DEBUG(4, "got %s\n", ok ? "?" : "that");
			if (ok == 0)
				break;
			if (altern == NULL) {
				logent("LOGIN", "FAILED");
				/* close *not* needed here. rti!trt */
				return(FAIL);
			}
			want = index(altern, '-');
			if (want != NULL)
				*want++ = '\0';
			sendthem(altern, fn);
		}
		sleep(2);
		if (k+1 < nf)
			sendthem(flds[k+1], fn);
	}
	return(0);
}


/* rti!trt: conditional table generation to support odd speeds */
/* Suggested in n44a.139 by n44!dan (Dan Ts'o) */
struct sg_spds {int sp_val, sp_name;} spds[] = {
#ifdef B50
	{  50,	 B50},
#endif
#ifdef B75
	{  75,	 B75},
#endif
#ifdef B110
	{ 110,	B110},
#endif
#ifdef B150
	{ 150,	B150},
#endif
#ifdef B200
	{ 200,	B200},
#endif
#ifdef B300
	{ 300,  B300},
#endif
#ifdef B600
	{600,	B600},
#endif
#ifdef B1200
	{1200, B1200},
#endif
#ifdef B1800
	{1800, B1800},
#endif
#ifdef B2000
	{2000, B2000},
#endif
#ifdef B2400
	{2400, B2400},
#endif
#ifdef B3600
	{3600, B3600},
#endif
#ifdef B4800
	{4800, B4800},
#endif
#ifdef B7200
	{7200, B7200},
#endif
#ifdef B9600
	{9600, B9600},
#endif
#ifdef B19200
	{19200,B19200},
#endif
	{0, 0}
};

/***
 *	fixline(tty, spwant)	set speed/echo/mode...
 *	int tty, spwant;
 *
 *	return codes:  none
 */

fixline(tty, spwant)
int tty, spwant;
{
#ifdef	SYSIII
	struct termio ttbuf;
#endif
#ifndef	SYSIII
	struct sgttyb ttbuf;
#endif
	register struct sg_spds *ps;
	int speed = -1;
	int ret;

	for (ps = spds; ps->sp_val; ps++)
		if (ps->sp_val == spwant)
			speed = ps->sp_name;
	ASSERT(speed >= 0, "BAD SPEED", "", speed);
#ifdef	SYSIII
	ioctl(tty, TCGETA, &ttbuf);
	/* ttbuf.sg_flags = (ANYP|RAW);
	ttbuf.sg_ispeed = ttbuf.sg_ospeed = speed; */
	ttbuf.c_iflag = (ushort)0;
	ttbuf.c_oflag = (ushort)0;
	ttbuf.c_cflag = (speed|CS8|HUPCL|CREAD);
	ttbuf.c_lflag = (ushort)0;
	ttbuf.c_cc[VMIN] = 6;
	ttbuf.c_cc[VTIME] = 1;
	ret = ioctl(tty, TCSETA, &ttbuf);
#endif
#ifndef	SYSIII
	ioctl(tty, TIOCGETP, &ttbuf);
	ttbuf.sg_flags = (ANYP|RAW);
	ttbuf.sg_ispeed = ttbuf.sg_ospeed = speed;
	ret = ioctl(tty, TIOCSETP, &ttbuf);
#endif
	ASSERT(ret >= 0, "RETURN FROM STTY", "", ret);
#ifndef	SYSIII
	ioctl(tty, TIOCHPCL, STBNULL);
	ioctl(tty, TIOCEXCL, STBNULL);
#endif
	return;
}


/* Bill Shannon recommends MR 2000, but that takes too much space on PDPs */
/* Actually, the 'expect' algorithm should be rewritten. */
#define MR 1000


/***
 *	expect(str, fn)	look for expected string
 *	char *str;
 *
 *	return codes:
 *		0  -  found
 *		FAIL  -  lost line or too many characters read
 *		some character  -  timed out
 */

expect(str, fn)
register char *str;
int fn;
{
	char rdvec[MR];
	register char *rp = rdvec;
	int kr;
	char nextch;

	if (strcmp(str, "\"\"") == SAME)
		return(0);
	*rp = 0;
	if (setjmp(Sjbuf)) {
		return(FAIL);
	}
	signal(SIGALRM, alarmtr);
/* change MAXCHARTIME to MAXMSGTIME, outside while loop -- brl-bmd!dpk */
	alarm(MAXMSGTIME);
	while (notin(str, rdvec)) {
		kr = read(fn, &nextch, 1);
		if (kr <= 0) {
			alarm(0);
			DEBUG(4, "lost line kr - %d\n, ", kr);
			logent("LOGIN", "LOST LINE");
			return(FAIL);
		}
		{
		int c;
		c = nextch & 0177;
		DEBUG(4, c >= 040 ? "%c" : "\\%03o", c);
		}
		if ((*rp = nextch & 0177) != '\0')
			rp++;
/* Check rdvec before null termination -- cmcl2!salkind */
		if (rp >= rdvec + MR) {
			alarm(0);
			return(FAIL);
		}
		*rp = '\0';
	}
	alarm(0);
	return(0);
}


/*
 * Determine next file descriptor that would be allocated.
 * This permits later closing of a file whose open was interrupted.
 * It is a UNIX kernel problem, but it has to be handled.
 * unc!smb (Steve Bellovin) probably first discovered it.
 */
getnextfd()
{
	close(next_fd = open("/", 0));
}

/***
 *	sendthem(str, fn)	send line of login sequence
 *	char *str;
 *
 *	return codes:  none
 */

sendthem(str, fn)
register char *str;
int fn;
{
	register char *strptr;
	int i, n, cr = 1;
	static int p_init = 0;

	/* Note: debugging authorized only for privileged users */
	DEBUG(5, "send %s\n", str);

	if (!p_init) {
		p_init++;
		bld_partab(P_EVEN);
	}

	if (prefix("BREAK", str)) {
		sscanf(&str[5], "%1d", &i);
		if (i <= 0 || i > 10)
			i = 3;
		/* send break */
		genbrk(fn, i);
		return;
	}

	if (prefix("PAUSE", str)) {
		sscanf(&str[5], "%1d", &i);
		if (i <= 0 || i > 10)
			i = 3;
		/* pause for a while */
		sleep((unsigned)i);
		return;
	}

	if (strcmp(str, "EOT") == SAME) {
		p_chwrite(fn, '\04');
		return;
	}

	/* LF, CR, and "" courtesy unc!smb */
	/* Send a '\n' */
	if (strcmp(str, "LF") == SAME)
		str = "\\n\\c";

	/* Send a '\r' */
	if (strcmp(str, "CR") == SAME)
		str = "\\r\\c";

	/* Set parity as needed */
	if (strcmp(str, "P_ZERO") == SAME) {
		bld_partab(P_ZERO);
		return;
	}
	if (strcmp(str, "P_ONE") == SAME) {
		bld_partab(P_ONE);
		return;
	}
	if (strcmp(str, "P_EVEN") == SAME) {
		bld_partab(P_EVEN);
		return;
	}
	if (strcmp(str, "P_ODD") == SAME) {
		bld_partab(P_ODD);
		return;
	}

	/* If "", just send '\r' */
	if (strcmp(str, "\"\"") != SAME)
	for (strptr = str; *strptr; strptr++) {
		if (*strptr == '\\') switch(*++strptr) {
		case 's':
			DEBUG(5, "BLANK\n", "");
			*strptr = ' ';
			break;
		case 'd':
			DEBUG(5, "DELAY\n", "");
			sleep(1);
			continue;
		case 'r':
			DEBUG(5, "RETURN\n", "");
			*strptr = '\r';
			break;
		case 'b':
			if (isdigit(*(strptr+1))) {
				i = (*++strptr - '0');
				if (i <= 0 || i > 10)
					i = 3;
			} else
				i = 3;
			/* send break */
			genbrk(fn, i);
			continue;
		case 'c':
			if (*(strptr+1) == '\0') {
			DEBUG(5, "NO CR\n", "");
				cr = 0;
				continue;
			}
			DEBUG(5, "NO CR - MIDDLE IGNORED\n", "");
			continue;
		default:
			if (isdigit(strptr[1])) {
				i = 0;
				n = 0;
				while (isdigit(strptr[1]) && ++n <= 3)
					i = i*8 + (*++strptr - '0');
				p_chwrite(fn, i);
				continue;
			}
			DEBUG(5, "BACKSLASH\n", "");
			strptr--;
		}
		p_chwrite(fn, *strptr);
	}

	/* '\n' changed to '\r'--a better default. rti!trt */
	if (cr)
		p_chwrite(fn, '\r');
	return;
}

p_chwrite(fd, c)
int fd;
int c;
{
	char t[2];

	t[0] = par_tab[c&0177];
	t[1] = '\0';
	ASSERT(write(fd, t, 1) == 1, "BAD WRITE", "", t[0]);
}

/*
 * generate parity table for use by p_chwrite.
 */
bld_partab(type)
int type;
{
	register int i, j, n;

	for (i = 0; i < sizeof(par_tab); i++) {
		n = 0;
		for (j = i&(sizeof(par_tab)-1); j; j = (j-1)&j)
			n++;
		par_tab[i] = i;
		if (type == P_ONE
		 || (type == P_EVEN && (n&01) != 0)
		 || (type == P_ODD && (n&01) == 0))
			par_tab[i] |= sizeof(par_tab);
	}
}

#define BSPEED B150

/***
 *	genbrk		send a break
 *
 *	return codes;  none
 */

genbrk(fn, bnulls)
register int fn, bnulls;
{
	register int ret;
#ifdef	SYSIII
	ret = ioctl(fn, TCSBRK, STBNULL);
	DEBUG(5, "break ioctl ret %d\n", ret);
#endif
#ifndef	SYSIII
#ifdef	TIOCSBRK
	ret = ioctl(fn, TIOCSBRK, STBNULL);
	DEBUG(5, "break ioctl ret %d\n", ret);
#ifdef	TIOCCBRK
	ret = write(fn, "\0\0\0\0\0\0\0\0\0\0\0\0", bnulls);
	ASSERT(ret > 0, "BAD WRITE genbrk", "", ret);
	sleep(1);
	ret = ioctl(fn, TIOCCBRK, STBNULL);
	DEBUG(5, "break ioctl ret %d\n", ret);
#endif
	DEBUG(4, "ioctl 1 second break\n", STBNULL);
#else
	struct sgttyb ttbuf;
	register int sospeed;

	ret = ioctl(fn, TIOCGETP, &ttbuf);
	sospeed = ttbuf.sg_ospeed;
	ttbuf.sg_ospeed = BSPEED;
	ret = ioctl(fn, TIOCSETP, &ttbuf);
	ret = write(fn, "\0\0\0\0\0\0\0\0\0\0\0\0", bnulls);
	ASSERT(ret > 0, "BAD WRITE genbrk", "", ret);
	ttbuf.sg_ospeed = sospeed;
	ret = ioctl(fn, TIOCSETP, &ttbuf);
	ret = write(fn, "@", 1);
	ASSERT(ret > 0, "BAD WRITE genbrk", "", ret);
	DEBUG(4, "sent BREAK nulls - %d\n", bnulls);
#endif
#endif
}


/***
 *	notin(sh, lg)	check for occurrence of substring "sh"
 *	char *sh, *lg;
 *
 *	return codes:
 *		0  -  found the string
 *		1  -  not in the string
 */

notin(sh, lg)
register char *sh, *lg;
{
	while (*lg != '\0') {
		/* Dave Martingale: permit wild cards in 'expect' */
		if (wprefix(sh, lg))
			return(0);
		else
			lg++;
	}
	return(1);
}


/*******
 *	ifdate(s)
 *	char *s;
 *
 *	ittvax!swatt
 *	Allow multiple date specifications separated by '|'.
 *	Calls ifadate, formerly "ifdate".
 *
 *	return codes:
 *		see ifadate
 */

ifdate(s)
char *s;
{
	register char *p;
	register int ret;

	for (p = s; p && (*p == '|' ? *++p : *p); p = index(p, '|'))
		if (ret = ifadate(p))
			return(ret);
	return(0);
}


/*******
 *	ifadate(s)
 *	char *s;
 *
 *	ifadate  -  this routine will check a string (s)
 *	like "MoTu0800-1730" to see if the present
 *	time is within the given limits.
 *	SIDE EFFECT - Retrytime is set
 *
 *	String alternatives:
 *		Wk - Mo thru Fr
 *		zero or one time means all day
 *		Any - any day
 *
 *	return codes:
 *		0  -  not within limits
 *		1  -  within limits
 */

ifadate(s)
char *s;
{
	static char *days[]={
		"Su", "Mo", "Tu", "We", "Th", "Fr", "Sa", 0
	};
	time_t clock;
	int rtime;
	int i, tl, th, tn, flag, dayok=0;
	struct tm *localtime();
	struct tm *tp;
	char *index();
	char *p;

	/*  pick up retry time for failures  */
	/*  global variable Retrytime is set here  */
	if ((p = index(s, ',')) == NULL) {
		Retrytime = RETRYTIME;
	}
	else {
		i = sscanf(p+1, "%d", &rtime);
		if (i < 1 || rtime < 5)
			rtime = 5;
		Retrytime  = rtime * 60;
	}

	time(&clock);
	tp = localtime(&clock);
	while (isalpha(*s)) {
		for (i = 0; days[i]; i++) {
			if (prefix(days[i], s))
				if (tp->tm_wday == i)
					dayok = 1;
		}

		if (prefix("Wk", s))
			if (tp->tm_wday >= 1 && tp->tm_wday <= 5)
				dayok = 1;
		if (prefix("Any", s))
			dayok = 1;
		s++;
	}

	if (dayok == 0)
		return(0);
	i = sscanf(s, "%d-%d", &tl, &th);
	tn = tp->tm_hour * 100 + tp->tm_min;
	if (i < 2)
		return(1);
	if (th < tl)
		flag = 0;  /* set up for crossover 2400 test */
	else
		flag = 1;
	if ((tn >= tl && tn <= th)
	  || (tn >= th && tn <= tl)) /* test for crossover 2400 */
		return(flag);
	else
		return(!flag);
}


/***
 *	char *
 *	lastc(s)	return pointer to last character
 *	char *s;
 *
 */

char *
lastc(s)
register char *s;
{
	while (*s != '\0') s++;
	return(s);
}


/***
 *	char *
 *	fdig(cp)	find first digit in string
 *
 *	return - pointer to first digit in string or end of string
 */

char *
fdig(cp)
register char *cp;
{
	register char *c;

	for (c = cp; *c; c++)
		if (*c >= '0' && *c <= '9')
			break;
	return(c);
}


/*
 * Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0
 * Strings are compared as if they contain all capital letters.
 */

snccmp(s1, s2)
register char *s1, *s2;
{
	char c1, c2;

	if (islower(*s1)) c1 = toupper(*s1);
	else c1 = *s1;
	if (islower(*s2)) c2 = toupper(*s2);
	else c2 = *s2;

	while (c1 == c2) {
		if (*s1++=='\0')
			return(0);
		s2++;
		if (islower(*s1)) c1 = toupper(*s1);
		else c1 = *s1;
		if (islower(*s2)) c2 = toupper(*s2);
		else c2 = *s2;
	}
	return(c1 - c2);
}
