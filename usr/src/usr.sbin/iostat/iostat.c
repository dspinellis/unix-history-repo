static	char *sccsid = "@(#)iostat.c	4.3 (Berkeley) 3/9/81";
/*
 * iostat
 */
#include <stdio.h>
#include <nlist.h>
#include <sys/types.h>
#include <sys/buf.h>
#include <sys/ubavar.h>
#include <sys/mbavar.h>
#include <sys/dk.h>

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
	{ "_mbdinit" },
#define X_MBDINIT	9
	{ "_ubdinit" },
#define X_UBDINIT	10
	{ 0 },
};

char dr_name[DK_NDRIVE][10];

struct
{
	int	dk_busy;
	long	cp_time[CPUSTATES];
	long	dk_time[DK_NDRIVE];
	long	dk_wds[DK_NDRIVE];
	long	dk_seek[DK_NDRIVE];
	long	dk_xfer[DK_NDRIVE];
	float	dk_mspw[DK_NDRIVE];
	long	tk_nin;
	long	tk_nout;
} s, s1;

int	mf;
double	etime;

main(argc, argv)
char *argv[];
{
	extern char *ctime();
	register  i;
	int iter;
	double f1, f2;
	long t;
	int tohdr = 1;

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
	while (argc>1&&argv[1][0]=='-') {
		argc--;
		argv++;
	}
	lseek(mf, (long)nl[X_DK_MSPW].n_value, 0);
	read(mf, s.dk_mspw, sizeof s.dk_mspw);
	for (i = 0; i < DK_NDRIVE; i++)
		sprintf(dr_name[i], "dk%d", i);
	read_names();
	if(argc > 2)
		iter = atoi(argv[2]);
loop:
	if (--tohdr == 0) {
		printf("      tty");
		for (i = 0; i < DK_NDRIVE; i++)
			if (s.dk_mspw[i] != 0.0)
				printf("          %3.3s ", dr_name[i]);
		printf("         cpu\n");
		printf(" tin tout");
		for (i = 0; i < DK_NDRIVE; i++)
			if (s.dk_mspw[i] != 0.0)
				printf(" sps tps msps ");
		printf(" us ni sy id\n");
		tohdr = 19;
	}
	lseek(mf, (long)nl[X_DK_BUSY].n_value, 0);
 	read(mf, &s.dk_busy, sizeof s.dk_busy);
 	lseek(mf, (long)nl[X_DK_TIME].n_value, 0);
 	read(mf, s.dk_time, sizeof s.dk_time);
 	lseek(mf, (long)nl[X_DK_XFER].n_value, 0);
 	read(mf, s.dk_xfer, sizeof s.dk_xfer);
 	lseek(mf, (long)nl[X_DK_WDS].n_value, 0);
 	read(mf, s.dk_wds, sizeof s.dk_wds);
 	lseek(mf, (long)nl[X_TK_NIN].n_value, 0);
 	read(mf, &s.tk_nin, sizeof s.tk_nin);
 	lseek(mf, (long)nl[X_TK_NOUT].n_value, 0);
 	read(mf, &s.tk_nout, sizeof s.tk_nout);
	lseek(mf, (long)nl[X_DK_SEEK].n_value, 0);
	read(mf, s.dk_seek, sizeof s.dk_seek);
	lseek(mf, (long)nl[X_CP_TIME].n_value, 0);
	read(mf, s.cp_time, sizeof s.cp_time);
	lseek(mf, (long)nl[X_DK_MSPW].n_value, 0);
	read(mf, s.dk_mspw, sizeof s.dk_mspw);
	for (i = 0; i < DK_NDRIVE; i++) {
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
	etime /= 60.0;
	printf("%4.0f%5.0f", s.tk_nin/etime, s.tk_nout/etime);
	for (i=0; i<DK_NDRIVE; i++)
		if (s.dk_mspw[i] != 0.0)
			stats(i);
	for (i=0; i<CPUSTATES; i++)
		stat1(i);
	printf("\n");
contin:
	--iter;
	if(iter)
	if(argc > 1) {
		sleep(atoi(argv[1]));
		goto loop;
	}
}

stats(dn)
{
	register i;
	double atime, words, xtime, itime;

	if (s.dk_mspw[dn] == 0.0) {
		printf("%4.0f%4.0f%5.1f ", 0.0, 0.0, 0.0);
		return;
	}
	atime = s.dk_time[dn];
	atime /= 60.0;
	words = s.dk_wds[dn]*32.0;	/* number of words transferred */
	xtime = s.dk_mspw[dn]*words;	/* transfer time */
	itime = atime - xtime;		/* time not transferring */
/*
	printf("\ndn %d, words %8.2f, atime %6.2f, xtime %6.2f, itime %6.2f\n",
	    dn, words, atime, xtime, itime);
*/
	if (xtime < 0)
		itime += xtime, xtime = 0;
	if (itime < 0)
		xtime += itime, itime = 0;
	printf("%4.0f", s.dk_seek[dn]/etime);
	printf("%4.0f", s.dk_xfer[dn]/etime);
	printf("%5.1f ",
	    s.dk_seek[dn] ? itime*1000./s.dk_seek[dn] : 0.0);
/*
	printf("%4.1f",
	    s.dk_xfer[dn] ? xtime*1000./s.dk_xfer[dn] : 0.0);
*/
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
	printf("%3.0f", 100*s.cp_time[o]/time);
}

/*
 * Read the drive names out of kmem.
 * ARGH ARGH ARGH ARGH !!!!!!!!!!!!
 */

#define steal(where, var) lseek(mf, where, 0); read(mf, &var, sizeof var);
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
	if (mp == 0 || up == 0)
	{
		fprintf(stderr, "iostat: Disk init info not in namelist\n");
		exit(1);
	}
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
