#ifndef lint
static char sccsid[] = "@(#)iostat.c	1.3 (Lucasfilm) %G%";
#endif

/*
 * iostat
 */
#include "systat.h"

#include <sys/buf.h>
#include <sys/dk.h>
#ifdef vax
#include <vaxuba/ubavar.h>
#include <vaxmba/mbavar.h>
#endif
#ifdef sun
#include <sundev/mbvar.h>
#endif

WINDOW *
openiostat()
{
	static WINDOW *w = NULL;

        if (w == NULL)
		w = newwin(20, 70, 4, 5);
	return (w);
}

closeiostat(w)
	WINDOW *w;
{

	if (w == NULL)
		return;
	move(5, 0);
	clrtobot();
	wclear(w);
	wrefresh(w);
}

static struct nlist nlst[] = {
#define	X_DK_BUSY	0
	{ "_dk_busy" },
#define	X_DK_TIME	1
	{ "_dk_time" },
#define	X_DK_XFER	2
	{ "_dk_xfer" },
#define	X_DK_WDS	3
	{ "_dk_wds" },
#define	X_DK_SEEK	4
	{ "_dk_seek" },
#define	X_CP_TIME	5
	{ "_cp_time" },
#define	X_DK_MSPW	6
	{ "_dk_mspw" },
#define	X_HZ		7
	{ "_hz" },

#ifdef vax
#define X_MBDINIT	8
	{ "_mbdinit" },
#define X_UBDINIT	9
	{ "_ubdinit" },
#endif
#ifdef sun
#define X_MBDINIT	8
	{ "_mbdinit" },
#endif
	{ "" },
};

char dr_name[DK_NDRIVE][10];

struct {
	int	dk_busy;
	long	cp_time[CPUSTATES];
	long	dk_time[DK_NDRIVE];
	long	dk_wds[DK_NDRIVE];
	long	dk_seek[DK_NDRIVE];
	long	dk_xfer[DK_NDRIVE];
	float	dk_mspw[DK_NDRIVE];
} s, s1;

int	kmem;
int	hz;
double	etime;

initiostat()
{
	register  i;

	if (nlst[X_DK_BUSY].n_type == 0) {
		nlist("/vmunix", nlst);
		if (nlst[X_DK_BUSY].n_type == 0) {
			error("Disk init information isn't in namelist");
			return;
		}
	}
	lseek(kmem, (long)nlst[X_DK_MSPW].n_value, L_SET);
	read(kmem, s.dk_mspw, sizeof s.dk_mspw);
	for (i = 0; i < DK_NDRIVE; i++)
		sprintf(dr_name[i], "dk%d", i);
	lseek(kmem, (long)nlst[X_DK_MSPW].n_value, L_SET);
	read(kmem, s.dk_mspw, sizeof s.dk_mspw);
	lseek(kmem, (long)nlst[X_HZ].n_value, L_SET);
	read(kmem, &hz, sizeof hz);
	read_names();
}

fetchiostat()
{

	if (nlst[X_DK_BUSY].n_type == 0)
		return;
	lseek(kmem, (long)nlst[X_DK_BUSY].n_value, L_SET);
 	read(kmem, &s.dk_busy, sizeof s.dk_busy);
 	lseek(kmem, (long)nlst[X_DK_TIME].n_value, L_SET);
 	read(kmem, s.dk_time, sizeof s.dk_time);
 	lseek(kmem, (long)nlst[X_DK_XFER].n_value, L_SET);
 	read(kmem, s.dk_xfer, sizeof s.dk_xfer);
 	lseek(kmem, (long)nlst[X_DK_WDS].n_value, L_SET);
 	read(kmem, s.dk_wds, sizeof s.dk_wds);
	lseek(kmem, (long)nlst[X_DK_SEEK].n_value, L_SET);
	read(kmem, s.dk_seek, sizeof s.dk_seek);
	lseek(kmem, (long)nlst[X_CP_TIME].n_value, L_SET);
	read(kmem, s.cp_time, sizeof s.cp_time);
}

labeliostat()
{
	register int i, row;

	if (nlst[X_DK_BUSY].n_type == 0) {
		error("No dk_busy defined.");
		return;
	}
	row = 5;
	move(row, 0); clrtoeol();
	mvaddstr(row++, 10,
            "/0   /10  /20  /30  /40  /50  /60  /70  /80  /90  /100");
	mvaddstr(row++, 0, "cpu  user|"); clrtoeol();
	mvaddstr(row++, 0, "     nice|"); clrtoeol();
	mvaddstr(row++, 0, "   system|"); clrtoeol();
	mvaddstr(row++, 0, "     idle|"); clrtoeol();
	row++;
	for (i = 0; i < DK_NDRIVE; i++)
		if (s.dk_mspw[i] != 0.0) {
			mvprintw(row++, 0, "%3.3s   bps|", dr_name[i]);
			clrtoeol();
			mvaddstr(row++, 0, "      tps|"); clrtoeol();
			mvaddstr(row++, 0, "      mps|"); clrtoeol();
		}
}

showiostat()
{
	register int i;
	register long t;
	int row;

	if (nlst[X_DK_BUSY].n_type == 0)
		return;
	for (i = 0; i < DK_NDRIVE; i++) {
#define X(fld)	t = s.fld[i]; s.fld[i] -= s1.fld[i]; s1.fld[i] = t
		X(dk_xfer); X(dk_seek); X(dk_wds); X(dk_time);
	}
	etime = 0;
	for(i = 0; i < CPUSTATES; i++) {
		X(cp_time);
		etime += s.cp_time[i];
	}
	if (etime == 0.0)
		etime = 1.0;
	etime /= (float) hz;
	row = 2;
	for (i = 0; i < CPUSTATES; i++)
		stat1(row++, i);
	row++;
	for (i = 0; i < DK_NDRIVE; i++)
		if (s.dk_mspw[i] != 0.0)
			row = stats(row, i);
}

stats(row, dn)
	int row, dn;
{
	register i;
	double atime, words, xtime, itime;

	if (s.dk_mspw[dn] == 0.0) {
		wmove(wnd, row++, 5); wclrtoeol(wnd);
		wmove(wnd, row++, 5); wclrtoeol(wnd);
		wmove(wnd, row++, 5); wclrtoeol(wnd);
		return (row);
	}
	atime = s.dk_time[dn];
	atime /= (float) hz;
	words = s.dk_wds[dn]*32.0;	/* number of words transferred */
	xtime = s.dk_mspw[dn]*words;	/* transfer time */
	itime = atime - xtime;		/* time not transferring */
	if (xtime < 0)
		itime += xtime, xtime = 0;
	if (itime < 0)
		xtime += itime, itime = 0;
	wmove(wnd, row++, 5); histogram(words / 512 / etime, 60, 1.0, 'X');
	wmove(wnd, row++, 5); histogram(s.dk_xfer[dn] / etime, 60, 1.0, 'X');
	wmove(wnd, row++, 5); histogram(s.dk_seek[dn] ?
	    itime * 1000. / s.dk_seek[dn] : 0, 60, 1.0, 'X');
	return (row);
}

stat1(row, o)
	int row, o;
{
	register i;
	double time;

	time = 0;
	for (i = 0; i < CPUSTATES; i++)
		time += s.cp_time[i];
	if (time == 0.0)
		time = 1.0;
	wmove(wnd, row, 5); histogram(100*s.cp_time[o] / time, 60, 0.5, 'X');
}

histogram(val, colwidth, scale, c)
	double val;
	int colwidth;
	double scale;
	char c;
{
	char buf[10];
	register int k;
	register int v = (int)(val * scale) + 0.5;

	k = MIN(v, colwidth);
	if (v > colwidth) {
		sprintf(buf, "%4.1f", val);
		k -= strlen(buf);
		while (k--)
			waddch(wnd, c);
		waddstr(wnd, buf);
		return;
	}
	while (k--)
		waddch(wnd, c);
	wclrtoeol(wnd);
}

#define steal(where, var) \
	lseek(kmem, where, L_SET); read(kmem, &var, sizeof var);

#ifdef vax
read_names()
{
	struct mba_device mdev;
	register struct mba_device *mp;
	struct mba_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;
	struct uba_device udev, *up;
	struct uba_driver udrv;

	mp = (struct mba_device *) nlst[X_MBDINIT].n_value;
	up = (struct uba_device *) nlst[X_UBDINIT].n_value;
	if (up == 0) {
		error("Disk init info not in namelist\n");
		return;
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

#ifdef sun
read_names()
{
	struct mb_device mdev;
	register struct mb_device *mp;
	struct mb_driver mdrv;
	short two_char;
	char *cp = (char *) &two_char;

	mp = (struct mb_device *) nlst[X_MBDINIT].n_value;
	if (mp == 0) {
		error("Disk init info not in namelist\n");
		return;
	}
	for (;;) {
		steal(mp++, mdev);
		if (mdev.md_driver == 0)
			break;
		if (mdev.md_dk < 0 || mdev.md_alive == 0)
			continue;
		steal(mdev.md_driver, mdrv);
		steal(mdrv.mdr_dname, two_char);
		sprintf(dr_name[mdev.md_dk], "%c%c%d",
		    cp[0], cp[1], mdev.md_unit);
	}
}
#endif
