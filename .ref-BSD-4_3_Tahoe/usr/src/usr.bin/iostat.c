#ifndef lint
static	char *sccsid = "@(#)iostat.c	4.15 (Berkeley) 87/01/12";
#endif

/*
 * iostat
 */
#include <stdio.h>
#include <ctype.h>
#include <nlist.h>
#include <signal.h>

#include <sys/types.h>
#include <sys/file.h>
#include <sys/buf.h>
#include <sys/dkstat.h>

struct nlist nl[] = {
	{ "_dk_busy" },
#define	X_DK_BUSY	0
	{ "_dk_time" },
#define	X_DK_TIME	1
	{ "_dk_xfer" },
#define	X_DK_XFER	2
	{ "_dk_wds" },
#define	X_DK_WDS	3
	{ "_tk_nin" },
#define	X_TK_NIN	4
	{ "_tk_nout" },
#define	X_TK_NOUT	5
	{ "_dk_seek" },
#define	X_DK_SEEK	6
	{ "_cp_time" },
#define	X_CP_TIME	7
	{ "_dk_mspw" },
#define	X_DK_MSPW	8
	{ "_hz" },
#define	X_HZ		9
	{ "_phz" },
#define	X_PHZ		10
	{ "_dk_ndrive" },
#define	X_DK_NDRIVE	11
#ifdef vax
	{ "_mbdinit" },
#define X_MBDINIT	(X_DK_NDRIVE+1)
	{ "_ubdinit" },
#define X_UBDINIT	(X_DK_NDRIVE+2)
#endif
#ifdef tahoe
#define	X_VBDINIT	(X_DK_NDRIVE+1)
	{ "_vbdinit" },
#endif
	{ 0 },
};

char	**dr_name;
int	*dr_select;
float	*dk_mspw;
int	dk_ndrive;
int	ndrives = 0;
#ifdef vax
char	*defdrives[] = { "hp0", "hp1", "hp2",  0 };
#else
char	*defdrives[] = { 0 };
#endif

struct {
	int	dk_busy;
	long	cp_time[CPUSTATES];
	long	*dk_time;
	long	*dk_wds;
	long	*dk_seek;
	long	*dk_xfer;
	long	tk_nin;
	long	tk_nout;
} s, s1;

int	mf;
int	hz;
int	phz;
double	etime;
int	tohdr = 1;
int	printhdr();

main(argc, argv)
	char *argv[];
{
	extern char *ctime();
	register  i;
	int iter, ndrives;
	double f1, f2;
	long t;
	char *arg, **cp, name[6], buf[BUFSIZ];

	nlist("/vmunix", nl);
	if(nl[X_DK_BUSY].n_type == 0) {
		printf("dk_busy not found in /vmunix namelist\n");
		exit(1);
	}
	mf = open("/dev/kmem", 0);
	if(mf < 0) {
		printf("cannot open /dev/kmem\n");
		exit(1);
	}
	iter = 0;
	for (argc--, argv++; argc > 0 && argv[0][0] == '-'; argc--, argv++)
		;
	if (nl[X_DK_NDRIVE].n_value == 0) {
		printf("dk_ndrive undefined in system\n");
		exit(1);
	}
	lseek(mf, nl[X_DK_NDRIVE].n_value, L_SET);
	read(mf, &dk_ndrive, sizeof (dk_ndrive));
	if (dk_ndrive <= 0) {
		printf("dk_ndrive %d\n", dk_ndrive);
		exit(1);
	}
	dr_select = (int *)calloc(dk_ndrive, sizeof (int));
	dr_name = (char **)calloc(dk_ndrive, sizeof (char *));
	dk_mspw = (float *)calloc(dk_ndrive, sizeof (float));
#define	allocate(e, t) \
    s./**/e = (t *)calloc(dk_ndrive, sizeof (t)); \
    s1./**/e = (t *)calloc(dk_ndrive, sizeof (t));
	allocate(dk_time, long);
	allocate(dk_wds, long);
	allocate(dk_seek, long);
	allocate(dk_xfer, long);
	for (arg = buf, i = 0; i < dk_ndrive; i++) {
		dr_name[i] = arg;
		sprintf(dr_name[i], "dk%d", i);
		arg += strlen(dr_name[i]) + 1;
	}
	read_names();
	lseek(mf, (long)nl[X_HZ].n_value, L_SET);
	read(mf, &hz, sizeof hz);
	lseek(mf, (long)nl[X_PHZ].n_value, L_SET);
	read(mf, &phz, sizeof phz);
	if (phz)
		hz = phz;
	lseek(mf, (long)nl[X_DK_MSPW].n_value, L_SET);
	read(mf, dk_mspw, dk_ndrive*sizeof (dk_mspw));
	/*
	 * Choose drives to be displayed.  Priority
	 * goes to (in order) drives supplied as arguments,
	 * default drives.  If everything isn't filled
	 * in and there are drives not taken care of,
	 * display the first few that fit.
	 */
	ndrives = 0;
	while (argc > 0 && !isdigit(argv[0][0])) {
		for (i = 0; i < dk_ndrive; i++) {
			if (strcmp(dr_name[i], argv[0]))
				continue;
			dr_select[i] = 1;
			ndrives++;
		}
		argc--, argv++;
	}
	for (i = 0; i < dk_ndrive && ndrives < 4; i++) {
		if (dr_select[i] || dk_mspw[i] == 0.0)
			continue;
		for (cp = defdrives; *cp; cp++)
			if (strcmp(dr_name[i], *cp) == 0) {
				dr_select[i] = 1;
				ndrives++;
				break;
			}
	}
	for (i = 0; i < dk_ndrive && ndrives < 4; i++) {
		if (dr_select[i])
			continue;
		dr_select[i] = 1;
		ndrives++;
	}
	if (argc > 1)
		iter = atoi(argv[1]);
	signal(SIGCONT, printhdr);
loop:
	if (--tohdr == 0)
		printhdr();
	lseek(mf, (long)nl[X_DK_BUSY].n_value, L_SET);
 	read(mf, &s.dk_busy, sizeof s.dk_busy);
 	lseek(mf, (long)nl[X_DK_TIME].n_value, L_SET);
 	read(mf, s.dk_time, dk_ndrive*sizeof (long));
 	lseek(mf, (long)nl[X_DK_XFER].n_value, L_SET);
 	read(mf, s.dk_xfer, dk_ndrive*sizeof (long));
 	lseek(mf, (long)nl[X_DK_WDS].n_value, L_SET);
 	read(mf, s.dk_wds, dk_ndrive*sizeof (long));
	lseek(mf, (long)nl[X_DK_SEEK].n_value, L_SET);
	read(mf, s.dk_seek, dk_ndrive*sizeof (long));
 	lseek(mf, (long)nl[X_TK_NIN].n_value, L_SET);
 	read(mf, &s.tk_nin, sizeof s.tk_nin);
 	lseek(mf, (long)nl[X_TK_NOUT].n_value, L_SET);
 	read(mf, &s.tk_nout, sizeof s.tk_nout);
	lseek(mf, (long)nl[X_CP_TIME].n_value, L_SET);
	read(mf, s.cp_time, sizeof s.cp_time);
	for (i = 0; i < dk_ndrive; i++) {
		if (!dr_select[i])
			continue;
#define X(fld)	t = s.fld[i]; s.fld[i] -= s1.fld[i]; s1.fld[i] = t
		X(dk_xfer); X(dk_seek); X(dk_wds); X(dk_time);
	}
	t = s.tk_nin; s.tk_nin -= s1.tk_nin; s1.tk_nin = t;
	t = s.tk_nout; s.tk_nout -= s1.tk_nout; s1.tk_nout = t;
	etime = 0;
	for(i=0; i<CPUSTATES; i++) {
		X(cp_time);
		etime += s.cp_time[i];
	}
	if (etime == 0.0)
		etime = 1.0;
	etime /= (float) hz;
	printf("%4.0f%5.0f", s.tk_nin/etime, s.tk_nout/etime);
	for (i=0; i<dk_ndrive; i++)
		if (dr_select[i])
			stats(i);
	for (i=0; i<CPUSTATES; i++)
		stat1(i);
	printf("\n");
	fflush(stdout);
contin:
	if (--iter && argc > 0) {
		sleep(atoi(argv[0]));
		goto loop;
	}
}

printhdr()
{
	register int i;

	printf("      tty");
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			printf("          %3.3s ", dr_name[i]);
	printf("         cpu\n");
	printf(" tin tout");
	for (i = 0; i < dk_ndrive; i++)
		if (dr_select[i])
			printf(" bps tps msps ");
	printf(" us ni sy id\n");
	tohdr = 19;
}

stats(dn)
{
	register i;
	double atime, words, xtime, itime;

	if (dk_mspw[dn] == 0.0) {
		printf("%4.0f%4.0f%5.1f ", 0.0, 0.0, 0.0);
		return;
	}
	atime = s.dk_time[dn];
	atime /= (float) hz;
	words = s.dk_wds[dn]*32.0;	/* number of words transferred */
	xtime = dk_mspw[dn]*words;	/* transfer time */
	itime = atime - xtime;		/* time not transferring */
	if (xtime < 0)
		itime += xtime, xtime = 0;
	if (itime < 0)
		xtime += itime, itime = 0;
	printf("%4.0f", words/512/etime);
	printf("%4.0f", s.dk_xfer[dn]/etime);
	printf("%5.1f ",
	    s.dk_seek[dn] ? itime*1000./s.dk_seek[dn] : 0.0);
}

stat1(o)
{
	register i;
	double time;

	time = 0;
	for(i=0; i<CPUSTATES; i++)
		time += s.cp_time[i];
	if (time == 0.0)
		time = 1.0;
	printf("%3.0f", 100.*s.cp_time[o]/time);
}

#define steal(where, var) \
    lseek(mf, where, L_SET); read(mf, &var, sizeof var);

#ifdef vax
#include <vaxuba/ubavar.h>
#include <vaxmba/mbavar.h>

read_names()
{
	struct mba_device mdev;
	register struct mba_device *mp;
	struct mba_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;
	struct uba_device udev, *up;
	struct uba_driver udrv;

	mp = (struct mba_device *) nl[X_MBDINIT].n_value;
	up = (struct uba_device *) nl[X_UBDINIT].n_value;
	if (up == 0) {
		fprintf(stderr, "iostat: Disk init info not in namelist\n");
		exit(1);
	}
	if (mp) for (;;) {
		steal(mp++, mdev);
		if (mdev.mi_driver == 0)
			break;
		if (mdev.mi_dk < 0 || mdev.mi_alive == 0)
			continue;
		steal(mdev.mi_driver, mdrv);
		steal(mdrv.md_dname, two_char);
		sprintf(dr_name[mdev.mi_dk], "%c%c%d",
		    cp[0], cp[1], mdev.mi_unit);
	}
	if (up) for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		sprintf(dr_name[udev.ui_dk], "%c%c%d",
		    cp[0], cp[1], udev.ui_unit);
	}
}
#endif

#ifdef tahoe
#include <tahoevba/vbavar.h>

/*
 * Read the drive names out of kmem.
 */
read_names()
{
	struct vba_device udev, *up;
	struct vba_driver udrv;
	short two_char;
	char *cp = (char *)&two_char;

	up = (struct vba_device *) nl[X_VBDINIT].n_value;
	if (up == 0) {
		fprintf(stderr, "vmstat: Disk init info not in namelist\n");
		exit(1);
	}
	for (;;) {
		steal(up++, udev);
		if (udev.ui_driver == 0)
			break;
		if (udev.ui_dk < 0 || udev.ui_alive == 0)
			continue;
		steal(udev.ui_driver, udrv);
		steal(udrv.ud_dname, two_char);
		sprintf(dr_name[udev.ui_dk], "%c%c%d",
		     cp[0], cp[1], udev.ui_unit);
	}
}
#endif
