#ifndef lint
static char sccsid[] = "@(#)conn.c	5.6 (Berkeley) %G%";
#endif

#include <signal.h>
#include "uucp.h"
#include <setjmp.h>
#include <ctype.h>
#include <errno.h>
#ifdef	USG
#include <termio.h>
#include <fcntl.h>
#endif
#ifndef	USG
#include <sgtty.h>
#endif
#ifdef BSD4_2
#include <sys/time.h>
#else
#include <time.h>
#endif

#define MAXC 1000

extern jmp_buf Sjbuf;
jmp_buf Cjbuf;
extern int errno, onesys;
extern char *sys_errlist[];
extern char MaxGrade, DefMaxGrade;

/* Parity control during login procedure */
#define	P_ZERO	0
#define	P_ONE	1
#define	P_EVEN	2
#define	P_ODD	3

#define ABORT -2

char 	*AbortOn = NULL;
char	par_tab[128];	/* must be power of two */
int	linebaudrate;	/* used for the sleep test in pk1.c */
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

/*
 *	place a telephone call to system and login, etc.
 *
 *	return codes:
 *		CF_SYSTEM: don't know system
 *		CF_TIME: wrong time to call
 *		CF_DIAL: call failed
 *		CF_NODEV: no devices available to place call
 *		CF_LOGIN: login/password dialog failed
 *
 *		>0  - file no.  -  connect ok
 */

int Dcf = -1;
char *Flds[MAXC/10];
extern int LocalOnly;

conn(system)
char *system;
{
	int ret, nf;
	register int fn = 0;
	char info[MAXC], wkpre[NAMESIZE], file[NAMESIZE];
	register FILE *fsys;
	int fcode = 0;

	nf = 0;

	fsys = fopen(SYSFILE, "r");
	ASSERT(fsys != NULL, "CAN'T OPEN", SYSFILE, 0);

	DEBUG(4, "finds (%s) called\n", system);
	while((nf = finds(fsys, system, info, Flds)) > 0) {
		if (LocalOnly) {
			if (strcmp("TCP", Flds[F_LINE])
				&& strcmp("DIR", Flds[F_LINE])
				&& strcmp("LOCAL", Flds[F_LINE]) )
					fn = CF_TIME;
		}
		sprintf(wkpre, "%c.%.*s", CMDPRE, SYSNSIZE, Rmtname);
		if (!onesys && MaxGrade != DefMaxGrade &&
			!iswrk(file, "chk", Spool, wkpre)) 
				fn = CF_TIME;
		if (fn != CF_TIME && (fn = getto(Flds)) > 0) {
			Dcf = fn;
			break;
		}
		fcode = (fn == FAIL ? CF_DIAL : fn);
	}
	fclose(fsys);

	if (nf <= 0)
		return fcode ? fcode : nf;

	DEBUG(4, "login %s\n", "called");
	ret = login(nf, Flds, fn);
	if (ret != SUCCESS) {
		clsacu();
		if (ret == ABORT)
			return CF_DIAL;
		else
			return CF_LOGIN;
	}
	/* rti!trt:  avoid passing file to children */
	fioclex(fn);
	return fn;
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

	DEBUG(4, "getto: call no. %s ", flds[F_PHONE]);
	DEBUG(4, "for sys %s\n", flds[F_NAME]);

	CU_end = nulldev;
	for (cd = condevs; cd->CU_meth != NULL; cd++) {
		if (snccmp(cd->CU_meth, flds[F_LINE]) == SAME) {
			DEBUG(4, "Using %s to call\n", cd->CU_meth);
			return (*(cd->CU_gen))(flds);
		}
	}
	DEBUG(1, "Can't find %s, assuming DIR", flds[F_LINE]);
	return diropn(flds);	/* search failed, so use direct */
}

/***
 *	clsacu()	close call unit
 *
 *	return codes:  none
 */

int (*CU_end)() = nulldev;
clsacu()
{
	/* make *sure* Dcf is no longer exclusive.
	 * Otherwise dual call-in/call-out modems could get stuck.
	 * Unfortunately, doing this here is not ideal, but it is the
	 * easiest place to put the call.
	 * Hopefully everyone honors the LCK protocol, of course
	 */
#ifndef	USG
	if (!IsTcpIp && Dcf >= 0 && ioctl(Dcf, TIOCNXCL, STBNULL) < 0)
		DEBUG(5, "clsacu ioctl %s\n", sys_errlist[errno]);
#endif
	if  (setjmp(Sjbuf))
		logent(Rmtname, "CLOSE TIMEOUT");
	else {
		signal(SIGALRM, alarmtr);
		alarm(20);
		(*(CU_end))(Dcf);
		alarm(0);
	}
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

	if (!isascii(*in) || !isalpha(*in)) {
		strcpy(out, in);
		return;
	}

	s1=pre;
	while (isascii(*in) && isalpha(*in))
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
			if (sscanf(buf, "%s%s", p, tpre) != 2)
				continue;
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
}

/*
 *	read and decode a line from device file
 *
 *	return code - FAIL at end-of file; 0 otherwise
 */

rddev(fp, dev)
register struct Devices *dev;
FILE *fp;
{
	register int na;

	if (!cfgets(dev->D_argbfr, sizeof(dev->D_argbfr), fp))
		return FAIL;
	na = getargs(dev->D_argbfr, dev->D_arg, 20);
	ASSERT(na >= 4, "BAD DEVICE ENTRY", dev->D_argbfr, 0);
	if (na == 4) {
		dev->D_brand = "";
		na++;
	}
	dev->D_speed = atoi(fdig(dev->D_class));
	dev->D_numargs = na;
	return 0;
}

/*
 *	set system attribute vector
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
		na = getargs(info, flds, MAXC/10);
		if (strncmp(sysnam, flds[F_NAME], MAXBASENAME) != SAME)
			continue;
		if (ifdate(flds[F_TIME]) != FAIL)
			/*  found a good entry  */
			return na;
		DEBUG(2, "Wrong time ('%s') to call\n", flds[F_TIME]);
		fcode = CF_TIME;
	}
	return fcode ? fcode : CF_SYSTEM;
}

/*
 *	do login conversation
 *
 *	return codes:  SUCCESS  |  FAIL
 */

login(nf, flds, fn)
register char *flds[];
int nf, fn;
{
	register char *want, *altern;
	int k, ok;

	ASSERT(nf > 4, "TOO FEW LOG FIELDS", CNULL, nf);
	if (setjmp(Cjbuf))
		return FAIL;
	AbortOn = NULL;
	for (k = F_LOGIN; k < nf; k += 2) {
		want = flds[k];
		ok = FAIL;
		while (ok != SUCCESS) {
			altern = index(want, '-');
			if (altern != NULL)
				*altern++ = '\0';
			if (strcmp(want, "ABORT") == 0) {
				AbortOn = flds[k+1];
				DEBUG(4, "ABORT ON: %s\n", AbortOn);
				goto nextfield;
			}
			DEBUG(4, "wanted: %s\n", want);
			ok = expect(want, fn);
			DEBUG(4, "got: %s\n", ok ? "?" : "that");
			if (ok == FAIL) {
				if (altern == NULL) {
					logent("LOGIN", _FAILED);
					return FAIL;
				}
				want = index(altern, '-');
				if (want != NULL)
					*want++ = '\0';
				sendthem(altern, fn);
			} else
				if (ok == ABORT) {
					logent("LOGIN ABORTED", _FAILED);
					return ABORT;
				}
		}
		sleep(1);
		if (k+1 < nf)
			sendthem(flds[k+1], fn);
nextfield: ;
	}
	return SUCCESS;
}


/* conditional table generation to support odd speeds */
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
	{19200, B19200},
#endif
#ifdef EXTA
	{19200, EXTA},
#endif
	{0, 0}
};

/*
 *	set speed/echo/mode...
 *
 *	return codes:  none
 */

fixline(tty, spwant)
int tty, spwant;
{
#ifdef	USG
	struct termio ttbuf;
#else	!USG
	struct sgttyb ttbuf;
#endif !USG
	register struct sg_spds *ps;
	int speed = -1;

	for (ps = spds; ps->sp_val; ps++)
		if (ps->sp_val == spwant)
			speed = ps->sp_name;
	ASSERT(speed >= 0, "BAD SPEED", CNULL, speed);
#ifdef	USG
	if (ioctl(tty, TCGETA, &ttbuf) < 0)
		return FAIL;
	/* ttbuf.sg_flags = (ANYP|RAW);
	ttbuf.sg_ispeed = ttbuf.sg_ospeed = speed; */
	ttbuf.c_iflag = (ushort)0;
	ttbuf.c_oflag = (ushort)0;
	ttbuf.c_cflag = (speed|CS8|HUPCL|CREAD);
	ttbuf.c_lflag = (ushort)0;
	ttbuf.c_cc[VMIN] = 6;
	ttbuf.c_cc[VTIME] = 1;
	if (ioctl(tty, TCSETA, &ttbuf) < 0)
		return FAIL;
#else	!USG
	if (ioctl(tty, TIOCGETP, &ttbuf) < 0)
		return FAIL;
	ttbuf.sg_flags = (ANYP|RAW);
	ttbuf.sg_ispeed = ttbuf.sg_ospeed = speed;
	if (ioctl(tty, TIOCSETP, &ttbuf) < 0)
		return FAIL;
#endif
#ifndef	USG
	if (ioctl(tty, TIOCHPCL, STBNULL) < 0)
		return FAIL;
	if (ioctl(tty, TIOCEXCL, STBNULL) < 0)
		return FAIL;
#endif
	linebaudrate = spwant;
	return SUCCESS;
}

#define MR 100

/*
 *	look for expected string
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
	register char *rp = rdvec, *strptr;
	int kr, cnt_char;
	char nextch;

	if (*str == '\0' || strcmp(str, "\"\"") == SAME)
		return SUCCESS;
	/* Cleanup str, convert \0xx strings to one char  */
	for (strptr = str; *strptr; strptr++) {
		if (*strptr == '\\')
			switch(*++strptr) {
			case 's':
				DEBUG(5, "BLANK\n", CNULL);
				*strptr = ' ';
				break;
			default:
				strptr--;  /* back up to backslash */
				sscanf(strptr + 1,"%o", &cnt_char);
				DEBUG(6, "BACKSLASHED %02xH\n", cnt_char);
				*strptr = (char) (cnt_char);
				strcpy(&strptr[1], &strptr[4]);
			}
	}

	*rp = 0;
	if (setjmp(Sjbuf))
		return FAIL;
	signal(SIGALRM, alarmtr);
	alarm(MAXMSGTIME);
	while (notin(str, rdvec)) {
		if(AbortOn != NULL && !notin(AbortOn, rdvec)) {
			DEBUG(1, "Call aborted on '%s'\n", AbortOn);
			alarm(0);
			return ABORT;
		}
		kr = read(fn, &nextch, 1);
		if (kr <= 0) {
			alarm(0);
			DEBUG(4, "lost line kr - %d\n, ", kr);
			logent("LOGIN", "LOST LINE");
			return FAIL;
		}
		{
		int c;
		c = nextch & 0177;
		DEBUG(4, c >= 040 ? "%c" : "\\%03o", c);
		if (c == '\n')
			DEBUG(4,"\n", CNULL);
		}
		if ((*rp = nextch & 0177) != '\0')
			rp++;
		if (rp >= rdvec + MR) {
			register char *p;
			for (p = rdvec+MR/2; p < rp; p++)
				*(p-MR/2) = *p;
			rp -= MR/2;
		}
		*rp = '\0';
	}
	alarm(0);
	return SUCCESS;
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

/*
 *	send line of login sequence
 *
 *	return codes:  none
 */
sendthem(str, fn)
register char *str;
int fn;
{
	register char *strptr;
	int i, n, cr = 1;
	register char c;
	static int p_init = 0;

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
	if (strcmp(str, "\"\"") == SAME) {
		p_chwrite(fn, '\r');
		return;
	}

	for (strptr = str; c = *strptr++;) {
		if (c == '\\') {
			switch(*strptr++) {
			case 's':
				DEBUG(5, "BLANK\n", CNULL);
				p_chwrite(fn, ' ');
				break;
			case 'd':
				DEBUG(5, "DELAY\n", CNULL);
				sleep(1);
				continue;
			case 'r':
				DEBUG(5, "RETURN\n", CNULL);
				p_chwrite(fn, '\r');
				break;
			case 'b':
				if (isdigit(*strptr)) {
					i = (*strptr++ - '0');
					if (i <= 0 || i > 10)
						i = 3;
				} else
					i = 3;
				/* send break */
				genbrk(fn, i);
				if (*strptr == '\0')
					cr = 0;
				continue;
			case 'c':
				if (*strptr == '\0') {
					DEBUG(5, "NO CR\n", CNULL);
					cr = 0;
					continue;
				}
				DEBUG(5, "NO CR - MIDDLE IGNORED\n", CNULL);
				continue;
			default:
				if (isdigit(*strptr)) {
					i = 0;
					n = 0;
					while (isdigit(*strptr) && ++n <= 3)
						i = i*8 + (*strptr++ - '0');
					p_chwrite(fn, (char)i);
					continue;
				}
				DEBUG(5, "BACKSLASH\n", CNULL);
				--strptr;
			}
		} else
			p_chwrite(fn, c);
	}

	if (cr)
		p_chwrite(fn, '\r');
	return;
}

p_chwrite(fd, c)
int fd;
char c;
{
	c = par_tab[c&0177];
	if (write(fd, &c, 1) != 1) {
		logent(sys_errlist[errno], "BAD WRITE");
		longjmp(Cjbuf, 2);
	}
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

/*
 *	send a break
 */
genbrk(fn, bnulls)
register int fn, bnulls;
{
#ifdef	USG
	if (ioctl(fn, TCSBRK, STBNULL) < 0)
		DEBUG(5, "break ioctl %s\n", sys_errlist[errno]);
#else	!USG
	struct sgttyb ttbuf;
	register int sospeed;
# ifdef	TIOCSBRK
	if (ioctl(fn, TIOCSBRK, STBNULL) < 0)
		DEBUG(5, "break ioctl %s\n", sys_errlist[errno]);
# ifdef	TIOCCBRK
	sleep(1);
	if (ioctl(fn, TIOCCBRK, STBNULL) < 0)
		DEBUG(5, "break ioctl %s\n", sys_errlist[errno]);
# endif TIOCCBRK
	DEBUG(4, "ioctl %d second break\n", bnulls );
# else !TIOCSBRK

	if (ioctl(fn, TIOCGETP, &ttbuf) < 0)
		DEBUG(5, "break ioctl %s\n", sys_errlist[errno]);
	sospeed = ttbuf.sg_ospeed;
	ttbuf.sg_ospeed = BSPEED;
	if (ioctl(fn, TIOCSETP, &ttbuf) < 0)
		DEBUG(5, "break ioctl %s\n", sys_errlist[errno]);
	if (write(fn, "\0\0\0\0\0\0\0\0\0\0\0\0", bnulls) != bnulls) {
badbreak:
		logent(sys_errlist[errno], "BAD WRITE genbrk");
		alarm(0);
		longjmp(Sjbuf, 3);
	}
	ttbuf.sg_ospeed = sospeed;
	if (ioctl(fn, TIOCSETP, &ttbuf) < 0)
		DEBUG(5, "break ioctl %s\n", sys_errlist[errno]);
	if (write(fn, "@", 1) != 1)
		goto badbreak;
	DEBUG(4, "sent BREAK nulls - %d\n", bnulls);
#endif !TIOCSBRK
#endif !USG
}

/*
 *	check for occurrence of substring "sh"
 *
 *	return codes:
 *		0  -  found the string
 *		1  -  not in the string
 */
notin(sh, lg)
register char *sh, *lg;
{
	while (*lg != '\0') {
		if (wprefix(sh, lg))
			return 0;
		else
			lg++;
	}
	return 1;
}

/*
 *	Allow multiple date specifications separated by ','.
 */
ifdate(p)
register char *p;
{
	register char *np, c;
	register int ret, g;
	int rtime, i;

	/*  pick up retry time for failures  */
	/*  global variable Retrytime is set here  */
	if ((np = index(p, ';')) == NULL) {
		Retrytime = RETRYTIME;
	} else {
		i = sscanf(np+1, "%d", &rtime);
		if (i < 1 || rtime < 0)
			rtime = 5;
		Retrytime  = rtime * 60;
	}

	ret = FAIL;
	MaxGrade = '\0';
	do {
		np = strpbrk(p, ",|");	/* prefer , but allow | for compat */
		if (np)
			*np = '\0';
		g = ifadate(p);
		DEBUG(11,"ifadate returns %o\n", g);
		if (g != FAIL) {
			ret = SUCCESS;
			if (g > MaxGrade)
				MaxGrade = g;
		}
		if (np)
			*np = ',';
		p = np + 1;
	} while (np);
	if (MaxGrade == '\0')
		MaxGrade = DefMaxGrade;
	return ret;
}

/*
 *	this routine will check a string (string)
 *	like "MoTu0800-1730" to see if the present
 *	time is within the given limits.
 *	SIDE EFFECT - Retrytime is set
 *
 *	return codes:
 *		0  -  not within limits
 *		1  -  within limits
 */

ifadate(string)
char *string;
{
	static char *days[]={
		"Su", "Mo", "Tu", "We", "Th", "Fr", "Sa", 0
	};
	time_t clock;
	register char *s = string;
	int i, tl, th, tn, dayok=0;
	struct tm *localtime();
	struct tm *tp;
	char *p, MGrade;

	if ((p = index(s, '/')) == NULL)
		MGrade = DefMaxGrade;
	else
		MGrade = p[1];

	time(&clock);
	tp = localtime(&clock);
	while (isascii(*s) && isalpha(*s)) {
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
		if (prefix("Evening", s)) {
			/* Sat or Sun */
			if (tp->tm_wday == 6 || tp->tm_wday == 0
				|| tp->tm_hour >= 17 || tp->tm_hour < 8)
					dayok = 1;
		}
		if (prefix("Night", s)) {
			if (tp->tm_wday == 6  /* Sat */
				|| tp->tm_hour >= 23 || tp->tm_hour < 8
					/* Sunday before 5pm */
				|| (tp->tm_wday == 0 && tp->tm_hour < 17))
					dayok = 1;
		}
		s++;
	}

	if (dayok == 0 && s != string)
		return FAIL;
	i = sscanf(s, "%d-%d", &tl, &th);
  	if (i < 2)
  		return MGrade;
	tn = tp->tm_hour * 100 + tp->tm_min;
  	if (th < tl) { 		/* crosses midnight */
  		if (tl <= tn || tn < th)
  			return MGrade;
  	} else

	if (i < 2)
		return MGrade;
	if (th < tl) { 	/* crosses midnight */
		if (tl <= tn || tn < th)
			return MGrade;
	} else
		if (tl <= tn && tn < th)
			return MGrade;
	return FAIL;
}

/*
 *	find first digit in string
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
	return c;
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
			return 0;
		s2++;
		if (islower(*s1)) c1 = toupper(*s1);
		else c1 = *s1;
		if (islower(*s2)) c2 = toupper(*s2);
		else c2 = *s2;
	}
	return c1 - c2;
}
/*
 * do chat script
 * occurs after local port is opened,
 * before 'dialing' the other machine.
 */
dochat(dev, flds, fd)
register struct Devices *dev;
char *flds[];
int fd;
{
	register int i;
	register char *p;
	char bfr[sizeof(dev->D_argbfr)];

	if (dev->D_numargs <= 5)
		return(0);
	DEBUG(4, "dochat called %d\n", dev->D_numargs);
	for (i = 0; i < dev->D_numargs-5; i++) {
		sprintf(bfr, dev->D_arg[D_CHAT+i], flds[F_PHONE]);
		if (strcmp(bfr, dev->D_arg[D_CHAT+i])) {
			p = malloc((unsigned)strlen(bfr)+1);
			if (p != NULL) {
				strcpy(p, bfr);
				dev->D_arg[D_CHAT+i] = p;
			}
		}
	}
	/* following is a kludge because login() arglist is a kludge */
	i = login(dev->D_numargs, &dev->D_arg[D_CHAT-5], fd);
	/*
	 * If login() last did a sendthem(), must pause so things can settle.
	 * But don't bother if chat failed.
	 */
	if (i == 0 && (dev->D_numargs&01))
		sleep(2);
	return(i);
}
