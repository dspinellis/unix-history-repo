#ifndef lint
static char sccsid[] = "@(#)iostat.c	1.6 (Lucasfilm) %G%";
#endif

/*
 * iostat
 */
#include "systat.h"
#include <sys/param.h>
#include <sys/buf.h>
#include <sys/file.h>
#include <nlist.h>

#define WBASEROW        4
#define WBASECOL        5

WINDOW *
openiostat()
{
        static WINDOW *w = NULL;

        if (w == NULL)
                w = newwin(LINES - 1 - WBASEROW, 0, WBASEROW, WBASECOL);
        return (w);
}

closeiostat(w)
        WINDOW *w;
{

        if (w == NULL)
                return;
        move(WBASEROW, 0);
        clrtobot();
        wclear(w);
        wrefresh(w);
}

static struct nlist nlst[] = {
#define X_DK_BUSY       0
        { "_dk_busy" },
#define X_DK_TIME       1
        { "_dk_time" },
#define X_DK_XFER       2
        { "_dk_xfer" },
#define X_DK_WDS        3
        { "_dk_wds" },
#define X_DK_SEEK       4
        { "_dk_seek" },
#define X_CP_TIME       5
        { "_cp_time" },
#ifdef vax
#define X_MBDINIT       6
        { "_mbdinit" },
#define X_UBDINIT       7
        { "_ubdinit" },
#endif
        { "" },
};

static struct {
        int     dk_busy;
        long    cp_time[CPUSTATES];
        long    dk_time[DK_NDRIVE];
        long    dk_wds[DK_NDRIVE];
        long    dk_seek[DK_NDRIVE];
        long    dk_xfer[DK_NDRIVE];
} s, s1;

static  int linesperregion;
static  double etime;
static  int numbers = 0;                /* default display bar graphs */
static  int msps = 0;                   /* default ms/seek shown */
static  int dk_select[DK_NDRIVE];

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
        if (ndrives == 0) {
                for (i = 0; i < DK_NDRIVE; i++)
                        if (dk_mspw[i] != 0.0)
                                sprintf(dr_name[i], "dk%d", i), ndrives++;
#ifdef vax
                read_names(nlst[X_MBDINIT].n_value, nlst[X_UBDINIT].n_value);
#endif
        }
        for (i = 0; i < DK_NDRIVE; i++)
                dk_select[i] = 1;
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
        int row;

        if (nlst[X_DK_BUSY].n_type == 0) {
                error("No dk_busy defined.");
                return;
        }
        row = WBASEROW + 1;
        move(row, 0); clrtobot();
        mvaddstr(row++, WBASECOL + 5, 
            "/0   /10  /20  /30  /40  /50  /60  /70  /80  /90  /100");
        mvaddstr(row++, 0, "cpu  user|");
        mvaddstr(row++, 0, "     nice|");
        mvaddstr(row++, 0, "   system|");
        mvaddstr(row++, 0, "     idle|");
        if (numbers)
                row = numlabels(row + 1);
        else
                row = barlabels(row + 1);
}

static
numlabels(row)
{
        int i, col, regions;

#define COLWIDTH        14
#define DRIVESPERLINE   ((COLS - WBASECOL) / COLWIDTH)
        regions = howmany(ndrives, DRIVESPERLINE);
        /*
         * Deduct -regions for blank line after each scrolling region.
         */
        linesperregion = (CMDLINE - row - regions) / regions;
        /*
         * Minimum region contains space for two
         * label lines and one line of statistics.
         */
        if (linesperregion < 3)
                linesperregion = 3;
        col = 0;
        for (i = 0; i < DK_NDRIVE; i++)
                if (dk_select[i] && dk_mspw[i] != 0.0) {
                        if (col + COLWIDTH >= COLS - WBASECOL) {
                                col = 0, row += linesperregion + 1;
                                if (row > CMDLINE - (linesperregion + 1))
                                        break;
                        }
                        mvwaddstr(wnd, row - WBASEROW, col + 4, dr_name[i]);
                        mvwaddstr(wnd, row + 1 - WBASEROW, col, "bps tps msps");
                        col += COLWIDTH;
                }
        if (col)
                row += linesperregion + 1;
        return (row);
}

static
barlabels(row)
        int row;
{
        int i;

        mvaddstr(row++, 10,
            "/0   /5   /10  /15  /20  /25  /30  /35  /40  /45  /50");
        linesperregion = 2 + msps;
        for (i = 0; i < DK_NDRIVE; i++)
                if (dk_select[i] && dk_mspw[i] != 0.0) {
                        if (row > CMDLINE - linesperregion)
                                break;
                        mvprintw(row++, 0, "%3.3s   bps|", dr_name[i]);
                        mvaddstr(row++, 0, "      tps|");
                        if (msps)
                                mvaddstr(row++, 0, "     msps|");
                }
        return (row);
}

showiostat()
{
        register int i, row, col;
        register long t;

        if (nlst[X_DK_BUSY].n_type == 0)
                return;
        for (i = 0; i < DK_NDRIVE; i++) {
#define X(fld)  t = s.fld[i]; s.fld[i] -= s1.fld[i]; s1.fld[i] = t
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
        if (!numbers) {
                row += 2;
                for (i = 0; i < DK_NDRIVE; i++)
                        if (dk_select[i] && dk_mspw[i] != 0.0) {
                                if (row > CMDLINE - linesperregion - WBASEROW)
                                        break;
                                row = stats(row, 10 - WBASECOL, i);
                        }
                return;
        }
        col = 0;
        wmove(wnd, row + linesperregion, 0);
        wdeleteln(wnd);
        wmove(wnd, row + 3, 0);
        winsertln(wnd);
        for (i = 0; i < DK_NDRIVE; i++)
                if (dk_select[i] && dk_mspw[i] != 0.0) {
                        if (col + COLWIDTH >= COLS - WBASECOL) {
                                col = 0, row += linesperregion + 1;
                                if (row + WBASEROW >
                                    CMDLINE - (linesperregion + 1))
                                        break;
                                wmove(wnd, row + linesperregion, 0);
                                wdeleteln(wnd);
                                wmove(wnd, row + 3, 0);
                                winsertln(wnd);
                        }
                        (void) stats(row + 3, col, i);
                        col += COLWIDTH;
                }
}

static
stats(row, col, dn)
        int row, dn;
{
        register i;
        double atime, words, xtime, itime;

        atime = s.dk_time[dn];
        atime /= (float) hz;
        words = s.dk_wds[dn]*32.0;      /* number of words transferred */
        xtime = dk_mspw[dn]*words;      /* transfer time */
        itime = atime - xtime;          /* time not transferring */
        if (xtime < 0)
                itime += xtime, xtime = 0;
        if (itime < 0)
                xtime += itime, itime = 0;
        if (numbers) {
                mvwprintw(wnd, row, col, "%3.0f%4.0f%5.1f",
                    words / 512 / etime, s.dk_xfer[dn] / etime,
                    s.dk_seek[dn] ? itime * 1000. / s.dk_seek[dn] : 0.0);
                return (row);
        }
        wmove(wnd, row++, col);
        histogram(words / 512 / etime, 50, 1.0);
        wmove(wnd, row++, col);
        histogram(s.dk_xfer[dn] / etime, 50, 1.0);
        if (msps) {
                wmove(wnd, row++, col);
                histogram(s.dk_seek[dn] ? itime * 1000. / s.dk_seek[dn] : 0,
                   50, 1.0);
        }
        return (row);
}

static
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
        wmove(wnd, row, 5);
#define CPUSCALE        0.5
        histogram(100 * s.cp_time[o] / time, 50, CPUSCALE);
}

histogram(val, colwidth, scale)
        double val;
        int colwidth;
        double scale;
{
        char buf[10];
        register int k;
        register int v = (int)(val * scale) + 0.5;

        k = MIN(v, colwidth);
        if (v > colwidth) {
                sprintf(buf, "%4.1f", val);
                k -= strlen(buf);
                while (k--)
                        waddch(wnd, 'X');
                waddstr(wnd, buf);
                return;
        }
        while (k--)
                waddch(wnd, 'X');
        wclrtoeol(wnd);
}

cmdiostat(cmd, args)
        char *cmd, *args;
{
        int i;

        if (prefix(cmd, "msps")) {
                msps = !msps;
                goto fixdisplay;
        }
        if (prefix(cmd, "numbers")) {
                numbers = 1;
                goto fixdisplay;
        }
        if (prefix(cmd, "bars")) {
                numbers = 0;
                goto fixdisplay;
        }
        if (prefix(cmd, "display")) {
                dkselect(args, 1, dk_select);
                goto fixdisplay;
        }
        if (prefix(cmd, "ignore")) {
                dkselect(args, 0, dk_select);
                goto fixdisplay;
        }
        if (prefix(cmd, "drives")) {
                move(CMDLINE, 0);
                for (i = 0; i < DK_NDRIVE; i++)
                        if (dk_mspw[i] != 0.0)
                                printw("%s ", dr_name[i]);
                return (1);
        }
        return (0);
fixdisplay:
        wclear(wnd);
        wrefresh(wnd);
        labeliostat();
        refresh();
        return (1);
}

prefix(s1, s2)
        register char *s1, *s2;
{

        while (*s1 == *s2) {
                if (*s1 == '\0')
                        return (1);
                s1++, s2++;
        }
        return (*s1 == '\0');
}
