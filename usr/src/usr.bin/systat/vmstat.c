/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)vmstat.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Cursed vmstat -- from Robert Elz.
 */
#include <curses.h>
#include <signal.h>
#include <nlist.h>
/*					this is STUPID!
#include <sys/time.h>
*/
#include <ctype.h>
#include <utmp.h>
#include <sys/param.h>
#include <sys/vm.h>
#include <sys/dk.h>
#include <sys/buf.h>
#include <vaxuba/ubavar.h>
#include <vaxmba/mbavar.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <vax/pte.h>
#include <sys/namei.h>

/*
 * These constants define where the major pieces are laid out
 */
#define PROCSROW	13	/* uses 2 rows and 20 cols */
#define PROCSCOL	 0
#define NAMEIROW	20	/* uses 3 rows and 38 cols */
#define NAMEICOL	 0
#define GRAPHROW	16	/* uses 3 rows and 51 cols */
#define GRAPHCOL	 0
#define GENSTATROW	14	/* uses 8 rows and 11 cols */
#define GENSTATCOL	51
#define INTSROW		 2	/* uses all rows to bottom and 17 cols */
#define INTSCOL		63
#define STATROW		 0	/* uses 1 row and 68 cols */
#define STATCOL		 2
#define PAGEROW		 2	/* uses 11 rows and 26 cols */
#define PAGECOL		36
#define MEMROW		 2	/* uses 4 rows and 31 cols */
#define MEMCOL		 0
#define DISKROW		 7	/* uses 5 rows and 35 cols */
#define DISKCOL		 0

/*
 * Maximum number of times that the clock may no longer advance.
 */
#define LOOPMAX	10

#if DK_NDRIVE > 6
#undef DK_NDRIVE
#define	DK_NDRIVE 6
#endif

char *fread();
long time();
float cputime();
char *asctime();
struct tm *localtime();

void finish();
void docmd();
struct utmp utmp;

struct nlist name[] = {
	{ "_cp_time" },
#define X_CPTIME 0
	{ "_rate" },
#define X_RATE 1
	{ "_total" },
#define X_TOTAL 2
	{ "_avenrun" },
#define X_AVENRUN 3
	{ "_proc" },
#define X_PROC 4
	{ "_nproc" },
#define X_NPROC 5
	{ "_bootime" },
#define X_BOOTIME 6
	{ "_deficit" },
#define X_DEFICIT 7
	{ "_ubdinit" },
#define X_UBDINIT 8
	{ "_mbdinit" },
#define X_MBDINIT 9
	{ "_sum" },
#define X_SUM 10
	{ "_dk_busy" },
#define	X_DK_BUSY	11
	{ "_dk_time" },
#define	X_DK_TIME	12
	{ "_dk_xfer" },
#define	X_DK_XFER	13
	{ "_dk_wds" },
#define	X_DK_WDS	14
	{ "_tk_nin" },
#define	X_TK_NIN	15
	{ "_tk_nout" },
#define	X_TK_NOUT	16
	{ "_dk_seek" },
#define	X_DK_SEEK	17
	{ "_dk_mspw" },
#define	X_DK_MSPW	18
	{ "_hz" },
#define	X_HZ		19
	{ "_phz" },
#define	X_PHZ		20
	{ "_nchstats" },
#define	X_NCHSTATS	21
	{ "_intrnames" },
#define	X_INTRNAMES	22
	{ "_eintrnames" },
#define	X_EINTRNAMES	23
	{ "_intrcnt" },
#define	X_INTRCNT	24
	{ "_eintrcnt" },
#define	X_EINTRCNT	25
	{ 0 },
};

struct Info {
	long time[CPUSTATES];
	struct vmmeter Rate;
	struct vmtotal Total;
	struct vmmeter Sum;
	struct forkstat Forkstat;
	long	dk_time[DK_NDRIVE];
	long	dk_wds[DK_NDRIVE];
	long	dk_seek[DK_NDRIVE];
	long	dk_xfer[DK_NDRIVE];
	float	dk_mspw[DK_NDRIVE];
	int	dk_busy;
	long	tk_nin;
	long	tk_nout;
	struct	nchstats nchstats;
	long	nchcount;
	long	*intrcnt;
} s, s1, s2, z;


#define total s.Total
#define sum s.Sum
#define sumold s1.Sum
#define rate s.Rate
#define	nchtotal s.nchstats
#define	oldnchtotal s1.nchstats
#define oldrate s1.Rate
#define X(fld)	{t=s.fld[i]; s.fld[i]-=s1.fld[i]; if(state==TIME) s1.fld[i]=t;}
#define Y(fld)	{t = s.fld; s.fld -= s1.fld; if(state == TIME) s1.fld = t;}
#define Z(fld)	{t = s.nchstats.fld; s.nchstats.fld -= s1.nchstats.fld; \
	if(state == TIME) s1.nchstats.fld = t;}

int ut;
int kmem;
int deficit;
double avenrun[3];
long c;
char buf[26];
time_t t;
time_t bootime;
double etime;
int secs;
int delay = 5;
int hz;
int phz;
float hertz;
int nintr;
long *intrloc;
char **intrname;

char dr_name[DK_NDRIVE][10];
enum state { BOOT, TIME, RUN, STOP } state = TIME;
enum { NONE, SOME } dr_state[DK_NDRIVE];

char cpuchar[CPUSTATES] = { '=' , '>', '-', ' ' };
char cpuorder[CPUSTATES] = { CP_SYS, CP_USER, CP_NICE, CP_IDLE };

main(argc,argv)
	int argc;
	char **argv;
{
	time_t now, lastime, starttime;
	int i, l, c, loopcnt;
	int psiz;
	int interv;
	int hits;
	float f1, f2;
	int inttotal, nextintsrow;
	char *intrnamebuf, *cp, *calloc(), *malloc();

	if (argc > 1)
		switch (c = argv[1][0] == '-' ? argv[1][1] : argv[1][0]) {

		case 't':
			state = TIME;
			break;

		case 'r':
			state = RUN;
			break;

		case 'b':
			state = BOOT;
			break;

			  case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			delay = c - '0';
			break;

		}
	if ((kmem = open("/dev/kmem",0)) < 0) {
		fprintf(stderr, "No /dev/kmem \n");
		exit(1);
	}

	if ((ut = open("/etc/utmp", 0)) < 0) {
		fprintf(stderr, "No utmp\n");
		exit(1);
	}

	nlist("/vmunix",name);
	if (name[0].n_type == 0) {
		fprintf(stderr, "No namelist\n");
		exit(1);
	}
	lseek(kmem, (long)name[X_HZ].n_value, 0);
	read(kmem, &hz, sizeof hz);
	lseek(kmem, (long)name[X_PHZ].n_value, 0);
	read(kmem, &phz, sizeof phz);
	hertz = phz ? phz : hz;
	lseek(kmem, (long)name[X_DK_MSPW].n_value, 0);
	read(kmem, s.dk_mspw, sizeof s.dk_mspw);
	for (i = 0; i < DK_NDRIVE; i++)
		if (s.dk_mspw[i] == 0.0)
			dr_state[i] = NONE;
		else
			dr_state[i] = SOME;
	read_names();

	nintr = (name[X_EINTRCNT].n_value -
		name[X_INTRCNT].n_value) / sizeof(long);
	intrloc = (long *) calloc(nintr, sizeof(long));
	intrname = (char **) calloc(nintr, sizeof(long));
	intrnamebuf = malloc(name[X_EINTRNAMES].n_value -
		name[X_INTRNAMES].n_value);
	if (intrnamebuf == NULL || intrname == NULL || intrloc == NULL) {
		fprintf(stderr, "vsta: out of memory\n");
		exit(1);
	}
	lseek(kmem, (long)name[X_INTRNAMES].n_value, 0);
	read(kmem, intrnamebuf, name[X_EINTRNAMES].n_value -
		name[X_INTRNAMES].n_value);
	for (cp = intrnamebuf, i = 0; i < nintr; i++) {
		intrname[i] = cp;
		cp += strlen(cp) + 1;
	}
	nextintsrow = INTSROW + 2;
	allocinfo(&s);
	allocinfo(&s1);
	allocinfo(&s2);
	allocinfo(&z);

	time(&lastime);
	starttime = lastime;
	getinfo(&s2, RUN);
	switch (state) {
	case RUN:
		copyinfo(&s2, &s1);
		break;
	case TIME:
		getinfo(&s1, TIME);
		break;
	case BOOT:
		copyinfo(&z, &s1);
		break;
	default:
		fprintf(stderr, "vsta: bad state %d\n", state);
		exit(3);
		break;
	}
	lseek(kmem, (long)name[X_BOOTIME].n_value, 0);
	read(kmem, &bootime, sizeof bootime);

	initscr();
	signal(SIGINT, finish);
	noecho();
	crmode();
	layout();

	for (loopcnt = 0; loopcnt < LOOPMAX; loopcnt++) {
		while (state == STOP)
			waittty(delay*10);
		time(&now);
		strcpy(buf, ctime(&now));
		buf[16] = '\0';
		getinfo(&s, state);
		for (i = 0; i < DK_NDRIVE; i++) {
			X(dk_xfer); X(dk_seek); X(dk_wds); X(dk_time);
		}
		Y(tk_nin); Y(tk_nout);
		etime = 0;
		for(i = 0; i < CPUSTATES; i++) {
			X(time);
			etime += s.time[i];
		}
		if (etime < 5.0)	/* < 5 ticks - ignore this trash */
			continue;
		loopcnt = 0;
		etime /= hertz;
		inttotal = 0;
		for (i = 0; i < nintr; i++) {
			if (s.intrcnt[i] == 0)
				continue;
			if (intrloc[i] == 0) {
				if (nextintsrow == LINES)
					continue;
				intrloc[i] = nextintsrow++;
				mvprintw(intrloc[i], INTSCOL + 9, "%-8.8s",
					intrname[i]);
			}
			X(intrcnt);
			l = (int)((float)s.intrcnt[i]/etime + 0.5);
			inttotal += l;
			putint(l, intrloc[i], INTSCOL, 8);
		}
		putint(inttotal, INTSROW + 1, INTSCOL, 8);
		Z(ncs_goodhits); Z(ncs_badhits); Z(ncs_miss);
		Z(ncs_long); Z(ncs_pass2); Z(ncs_2passes);
		s.nchcount = nchtotal.ncs_goodhits + nchtotal.ncs_badhits +
		    nchtotal.ncs_miss + nchtotal.ncs_long;
		if (state == TIME)
			s1.nchcount = s.nchcount;

		psiz = 0;
		f2 = 0.0;
		for (c = 0; c < CPUSTATES; c++) {
			i = cpuorder[c];
			f1 = cputime(i);
			f2 += f1;
			l = (int) ((f2 + 1.0) / 2.0) - psiz;
			if (c == 0)
				putfloat(f1, GRAPHROW, GRAPHCOL + 1, 5, 1, 0);
			else
				putfloat(f1, GRAPHROW, GRAPHCOL + 12 * c,
					5, 1, 0);
			move(GRAPHROW + 2, psiz);
			psiz += l;
			while (l-- > 0)
				addch(cpuchar[c]);
		}

		putint(ucount(), STATROW, STATCOL, 3);
		putfloat(avenrun[0], STATROW, STATCOL + 17, 6, 2, 0);
		putfloat(avenrun[1], STATROW, STATCOL + 23, 6, 2, 0);
		putfloat(avenrun[2], STATROW, STATCOL + 29, 6, 2, 0);
		mvaddstr(STATROW, STATCOL + 53, buf);
		putint(total.t_arm/2, MEMROW + 2, MEMCOL + 4, 5);
		putint(total.t_armtxt/2, MEMROW + 2, MEMCOL + 9, 5);
		putint(total.t_avm/2, MEMROW + 2, MEMCOL + 14, 5);
		putint(total.t_avmtxt/2, MEMROW + 2, MEMCOL + 19, 5);
		putint(total.t_rm/2, MEMROW + 3, MEMCOL + 4, 5);
		putint(total.t_rmtxt/2, MEMROW + 3, MEMCOL + 9, 5);
		putint(total.t_vm/2, MEMROW + 3, MEMCOL + 14, 5);
		putint(total.t_vmtxt/2, MEMROW + 3, MEMCOL + 19, 5);
		putint(total.t_free/2, MEMROW + 2, MEMCOL + 26, 5);
		putint(total.t_rq, PROCSROW + 1, PROCSCOL + 5, 3);
		putint(total.t_pw, PROCSROW + 1, PROCSCOL + 8, 3);
		putint(total.t_dw, PROCSROW + 1, PROCSCOL + 11, 3);
		putint(total.t_sl, PROCSROW + 1, PROCSCOL + 14, 3);
		putint(total.t_sw, PROCSROW + 1, PROCSCOL + 17, 3);
		putrate(rate.v_swtch, oldrate.v_swtch, 
			GENSTATROW, GENSTATCOL, 7);
		putrate(rate.v_trap, oldrate.v_trap, 
			GENSTATROW + 1, GENSTATCOL, 7);
		putrate(rate.v_syscall, oldrate.v_syscall, 
			GENSTATROW + 2, GENSTATCOL, 7);
		putrate(rate.v_intr, oldrate.v_intr, 
			GENSTATROW + 3, GENSTATCOL, 7);
		putrate(rate.v_pdma, oldrate.v_pdma, 
			GENSTATROW + 4, GENSTATCOL, 7);
		putrate(rate.v_faults, oldrate.v_faults, 
			GENSTATROW + 5, GENSTATCOL, 7);
		putrate(rate.v_scan, oldrate.v_scan, 
			GENSTATROW + 6, GENSTATCOL, 7);
		putrate(rate.v_rev, oldrate.v_rev, 
			GENSTATROW + 7, GENSTATCOL, 7);
		putrate(rate.v_pgin, oldrate.v_pgin, PAGEROW + 2,
			PAGECOL + 5, 5);
		putrate(rate.v_pgout, oldrate.v_pgout, PAGEROW + 2,
			PAGECOL + 10, 5);
		putrate(rate.v_swpin, oldrate.v_swpin, PAGEROW + 2,
			PAGECOL + 15, 5);
		putrate(rate.v_swpout, oldrate.v_swpout, PAGEROW + 2,
			PAGECOL + 20, 5);
		putrate(rate.v_pgpgin, oldrate.v_pgpgin, PAGEROW + 3,
			PAGECOL + 5, 5);
		putrate(rate.v_pgpgout, oldrate.v_pgpgout, PAGEROW + 3,
			PAGECOL + 10, 5);
		putrate(rate.v_pswpin, oldrate.v_pswpin, PAGEROW + 3,
			PAGECOL + 15, 5);
		putrate(rate.v_pswpout, oldrate.v_pswpout, PAGEROW + 3,
			PAGECOL + 20, 5);
		putrate(rate.v_pgrec, oldrate.v_pgrec, PAGEROW + 6, PAGECOL, 3);
		putrate(rate.v_intrans, oldrate.v_intrans, PAGEROW + 6,
			PAGECOL + 4, 2);
		putrate(rate.v_xsfrec, oldrate.v_xsfrec, PAGEROW + 6,
			PAGECOL + 7, 3);
		putrate(rate.v_xifrec, oldrate.v_xifrec, PAGEROW + 6,
			PAGECOL + 11, 3);
		putrate(rate.v_pgfrec, oldrate.v_pgfrec, PAGEROW + 6,
			PAGECOL + 15, 3);
		putrate(rate.v_dfree, oldrate.v_dfree, PAGEROW + 6,
			PAGECOL + 19, 3);
		putrate(rate.v_seqfree, oldrate.v_seqfree, PAGEROW + 6,
			PAGECOL + 23, 3);
		putrate(rate.v_zfod, oldrate.v_zfod, PAGEROW + 8, PAGECOL, 8);
		putrate(rate.v_nzfod, oldrate.v_nzfod, PAGEROW + 9, PAGECOL, 8);
		putrate(rate.v_exfod, oldrate.v_exfod, PAGEROW + 8,
			PAGECOL + 14, 8);
		putrate(rate.v_nexfod, oldrate.v_nexfod, PAGEROW + 9,
			PAGECOL + 14, 8);
		putfloat (
			rate.v_nzfod == 0 ?
				0.0
			: state != RUN ?
				( 100.0 * rate.v_zfod / rate.v_nzfod )
			: rate.v_nzfod == oldrate.v_nzfod ?
				0.0
			:
				( 100.0 * (rate.v_zfod-oldrate.v_zfod)
				/ (rate.v_nzfod-oldrate.v_nzfod) )
			, PAGEROW + 10
			, PAGECOL
			, 8
			, 2
			, 1
		);
		putfloat (
			rate.v_nexfod == 0 ?
				0.0
			: state != RUN ?
				( 100.0 * rate.v_exfod / rate.v_nexfod )
			: rate.v_nexfod == oldrate.v_nexfod ?
				0.0
			:
				( 100.0 * (rate.v_exfod-oldrate.v_exfod)
				/ (rate.v_nexfod-oldrate.v_nexfod) )
			, PAGEROW + 10
			, PAGECOL + 14
			, 8
			, 2
			, 1
		);
		c = 1;
		for (i = 0; i < DK_NDRIVE; i++)
			if (dr_state[i] == SOME)
				dinfo(i, c++);

		putint(s.nchcount, NAMEIROW + 2, NAMEICOL, 9);
		putint(nchtotal.ncs_goodhits, NAMEIROW + 2, NAMEICOL + 9, 9);
#define nz(x)	((x) ? (x) : 1)
		putfloat(nchtotal.ncs_goodhits * 100.0 / nz(s.nchcount),
		   NAMEIROW + 2, NAMEICOL + 19, 4, 0, 1);
		putint(nchtotal.ncs_pass2, NAMEIROW + 2, NAMEICOL + 23, 9);
		putfloat(nchtotal.ncs_pass2 * 100.0 / nz(s.nchcount),
		   NAMEIROW + 2, NAMEICOL + 34, 4, 0, 1);
#undef nz
		move(LINES-1,0);
		refresh();
		waittty(delay);
	}
	clear();
	mvprintw(2, 10, "THE SYSTEM CLOCK HAS DIED!");
	refresh();
	sleep(5);
	finish();
}

/* calculate number of users on the system */
ucount()
{
	register int nusers = 0;

	while (read(ut, &utmp, sizeof(utmp)))
		if (utmp.ut_name[0] != '\0')
			nusers++;

	lseek(ut, 0L, 0);
	return(nusers);
}

float
cputime(indx)
int indx;
{
	double t;
	register i;

	t = 0;
	for (i = 0; i < CPUSTATES; i++)
		t += s.time[i];
	if (t == 0.0)
		t = 1.0;
	return(s.time[indx] * 100.0 / t);
}

void
finish()
{
	mvcur(0, COLS-1, LINES-1, 0);
	endwin();
	exit(0);

}

waittty(period)
{
	int inbits = 1 << 0;
	struct timeval tv;

	tv.tv_usec = 0;
	tv.tv_sec = period;

	select(32, &inbits, 0, 0, &tv);
	if (inbits & (1 << 0))
		docmd();
}

void
docmd()
{
	int c;
	static enum state oldstate;

	c = getchar() & 0177;

	switch ( c ) {
		  case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		delay = c - '0';
		state = TIME;
		break;
	case 'r':
		copyinfo(&s2, &s1);
		state = RUN;
		break;
	case 'b':
		state = BOOT;
		copyinfo(&z, &s1);
		break;
	case 't':
		state = TIME;
		break;
	case 'l': case 'l'&037:
		layout();
		if (state == STOP)
			state = oldstate;
		break;
	case 'z':
		if (state == RUN)
			getinfo(&s1, RUN);
		else if (state == STOP)
			state = oldstate;
		break;
	case 'q': case 0177:
		finish();
		/* NOTREACHED */
	case ' ': case '0':
		if (state == STOP) {
			state = oldstate;
			break;
		}
		oldstate = state;
		state = STOP;
		break;

	default:
		if (state == STOP)
			state = oldstate;
	}
}

layout()
{
	register i, j;

	clear();
	mvprintw(STATROW, STATCOL + 4, "users    Load");
	mvprintw(MEMROW, MEMCOL, "Mem     REAL    VIRTUAL ");
	mvprintw(MEMROW + 1, MEMCOL, "      Tot Text  Tot Text");
	mvprintw(MEMROW + 2, MEMCOL, "Act");
	mvprintw(MEMROW + 3, MEMCOL, "All");

	mvprintw(MEMROW + 1, MEMCOL + 27, "Free");

	mvprintw(PAGEROW, PAGECOL, "        PAGING    SWAPING ");
	mvprintw(PAGEROW + 1, PAGECOL, "        in  out   in  out ");
	mvprintw(PAGEROW + 2, PAGECOL, "count");
	mvprintw(PAGEROW + 3, PAGECOL, "pages");

	mvprintw(INTSROW, INTSCOL, " Interrupts");
	mvprintw(INTSROW + 1, INTSCOL + 9, "total");

	mvprintw(GENSTATROW, GENSTATCOL + 8, "Csw");
	mvprintw(GENSTATROW + 1, GENSTATCOL + 8, "Trp");
	mvprintw(GENSTATROW + 2, GENSTATCOL + 8, "Sys");
	mvprintw(GENSTATROW + 3, GENSTATCOL + 8, "Int");
	mvprintw(GENSTATROW + 4, GENSTATCOL + 8, "Pdm");
	mvprintw(GENSTATROW + 5, GENSTATCOL + 8, "Flt");
	mvprintw(GENSTATROW + 6, GENSTATCOL + 8, "Scn");
	mvprintw(GENSTATROW + 7, GENSTATCOL + 8, "Rev");

	mvprintw(PAGEROW + 5, PAGECOL, "Rec It F/S F/F RFL Fre SFr");

	mvprintw(PAGEROW + 8, PAGECOL + 9, " zf");
	mvprintw(PAGEROW + 9, PAGECOL + 9, "nzf");
	mvprintw(PAGEROW + 10, PAGECOL + 9, "%%zf");
	mvprintw(PAGEROW + 8, PAGECOL + 23, " xf");
	mvprintw(PAGEROW + 9, PAGECOL + 23, "nxf");
	mvprintw(PAGEROW + 10, PAGECOL + 23, "%%xf");

	mvprintw(GRAPHROW, GRAPHCOL,
		"    . %% Sys    . %% User    . %% Nice    . %% Idle");
	mvprintw(PROCSROW, PROCSCOL, "Procs  r  p  d  s  w");
	mvprintw(GRAPHROW + 1, GRAPHCOL,
		"|    |    |    |    |    |    |    |    |    |    |");

	mvprintw(NAMEIROW, NAMEICOL, "Namei         Sys-cache     Proc-cache");
	mvprintw(NAMEIROW + 1, NAMEICOL,
		"    Calls     hits    %%     hits     %%");
	mvprintw(DISKROW, DISKCOL, "Discs");
	mvprintw(DISKROW + 1, DISKCOL, "seeks");
	mvprintw(DISKROW + 2, DISKCOL, "xfers");
	mvprintw(DISKROW + 3, DISKCOL, " blks");
	mvprintw(DISKROW + 4, DISKCOL, " msps");
	j = 0;
	for (i = 0; i < DK_NDRIVE; i++)
		if (dr_state[i] == SOME) {
			mvprintw(DISKROW, DISKCOL + 5 + 5 * j,
				"  %3.3s", dr_name[j]);
			j++;
		}
	for (i = 0; i < nintr; i++) {
		if (intrloc[i] == 0)
			continue;
		mvprintw(intrloc[i], INTSCOL + 9, "%-8.8s", intrname[i]);
	}
}

putrate(r, or, l, c, w)
{
	if (state != TIME) {
		if (state == RUN)
			r -= or;
		putint((int)((float)r/etime + 0.5), l, c, w);
	} else
		putint(r, l, c, w);
}

putint(n, l, c, w)
{
	char b[128];

	move(l, c);
	if (n == 0) {
		while (w-- > 0)
			addch(' ');
		return;
	}
	sprintf(b, "%*d", w, n);
	if (strlen(b) > w) {
		while (w-- > 0)
			addch('*');
		return;
	}
	addstr(b);
}

putfloat(f, l, c, w, d, nz)
float f;
{
	char b[128];

	move(l, c);
	if (nz && f == 0.0) {
		while (w-- > 0)
			addch(' ');
		return;
	}
	sprintf(b, "%*.*f", w, d, f);
	if (strlen(b) > w) {
		while (w-- > 0)
			addch('*');
		return;
	}
	addstr(b);
}

/*
 * Read the drive names out of kmem.
 * ARGH ARGH ARGH ARGH !!!!!!!!!!!!
 */

#define steal(where, var) lseek(kmem, where, 0); read(kmem, &var, sizeof var);
read_names()
{
	struct mba_device mdev;
	register struct mba_device *mp;
	struct mba_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;
	struct uba_device udev, *up;
	struct uba_driver udrv;

	mp = (struct mba_device *) name[X_MBDINIT].n_value;
	up = (struct uba_device *) name[X_UBDINIT].n_value;
	if (up == 0)
	{
		fprintf(stderr, "vsta: Disk init info not in namelist\n");
		exit(1);
	}
	if (mp)
	while(1)
	{
		steal(mp++, mdev);
		if (mdev.mi_driver == 0)
			break;
		if (mdev.mi_dk < 0 || mdev.mi_alive == 0)
			continue;
		steal(mdev.mi_driver, mdrv);
		steal(mdrv.md_dname, two_char);
		sprintf(dr_name[mdev.mi_dk], "%c%c%d", cp[0], cp[1], mdev.mi_unit);
	}
	while(1)
	{
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		sprintf(dr_name[udev.ui_dk], "%c%c%d", cp[0], cp[1], udev.ui_unit);
	}
}

getinfo(s, st)
	struct Info *s;
	enum state st;
{
	lseek(kmem, (long)name[X_CPTIME].n_value,0);
	read(kmem, s->time, sizeof s->time);
	if (st != TIME) {
		lseek(kmem, (long)name[X_SUM].n_value, 0);
		read(kmem, &s->Rate, sizeof &s->Rate);
	} else {
		lseek(kmem, (long)name[X_RATE].n_value,0);
		read(kmem, &s->Rate, sizeof s->Rate);
	}
	lseek(kmem, (long)name[X_DEFICIT].n_value,0);
	read(kmem, deficit, sizeof deficit);
	lseek( kmem, (long)name[X_AVENRUN].n_value, 0 );
	read( kmem, avenrun, sizeof(avenrun) );
	lseek(kmem, (long)name[X_TOTAL].n_value,0);
	read(kmem, &s->Total, sizeof s->Total);
	lseek(kmem, (long)name[X_DK_BUSY].n_value, 0);
 	read(kmem, &s->dk_busy, sizeof s->dk_busy);
 	lseek(kmem, (long)name[X_DK_TIME].n_value, 0);
 	read(kmem, s->dk_time, sizeof s->dk_time);
 	lseek(kmem, (long)name[X_DK_XFER].n_value, 0);
 	read(kmem, s->dk_xfer, sizeof s->dk_xfer);
 	lseek(kmem, (long)name[X_DK_WDS].n_value, 0);
 	read(kmem, s->dk_wds, sizeof s->dk_wds);
 	lseek(kmem, (long)name[X_TK_NIN].n_value, 0);
 	read(kmem, &s->tk_nin, sizeof s->tk_nin);
 	lseek(kmem, (long)name[X_TK_NOUT].n_value, 0);
 	read(kmem, &s->tk_nout, sizeof s->tk_nout);
	lseek(kmem, (long)name[X_DK_SEEK].n_value, 0);
	read(kmem, s->dk_seek, sizeof s->dk_seek);
	lseek(kmem, (long)name[X_NCHSTATS].n_value, 0);
	read(kmem, &s->nchstats, sizeof s->nchstats);
	lseek(kmem, (long)name[X_INTRCNT].n_value, 0);
	read(kmem, s->intrcnt, nintr * sizeof (long));
}

allocinfo(s)
	struct Info *s;
{

	s->intrcnt = (long *) malloc(nintr * sizeof(long));
	if (s->intrcnt == NULL) {
		fprintf(stderr, "vsta: out of memory\n");
		exit(2);
	}
}

copyinfo(from, to)
	struct Info *from, *to;
{
	register int i, *fip = from->intrcnt, *tip = to->intrcnt;

	*to = *from;
	to->intrcnt = tip;
	for (i = 0; i < nintr; i++)
		*tip++ = *fip++;
}

dinfo(dn, c)
{
	double words, atime, itime, xtime;

	c = DISKCOL + c * 5;
	atime = s.dk_time[dn];
	atime /= 60.0;
	words = s.dk_wds[dn]*32.0;	/* number of words transferred */
	xtime = s.dk_mspw[dn]*words;	/* transfer time */
	itime = atime - xtime;		/* time not transferring */
	if (xtime < 0)
		itime += xtime, xtime = 0;
	if (itime < 0)
		xtime += itime, itime = 0;
	putint((int)((float)s.dk_seek[dn]/etime+0.5), DISKROW + 1, c, 5);
	putint((int)((float)s.dk_xfer[dn]/etime+0.5), DISKROW + 2, c, 5);
	putint((int)(words/etime/512.0 + 0.5), DISKROW + 3, c, 5);
	if (s.dk_seek[dn])
		putfloat(itime*1000.0/s.dk_seek[dn], DISKROW + 4, c, 5, 1, 1);
	else
		putint(0, DISKROW + 4, c, 5);
}
