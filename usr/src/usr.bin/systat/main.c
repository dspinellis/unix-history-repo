/*% cc -a % -lcurses -ltermlib -lm
 */
#ifndef lint
static char sccsid[] = "@(#)main.c	1.1 (Lucasfilm) %G%";
#endif

#include "systat.h"

struct nlist nlst[] = {
#define X_PROC          0
        { "_proc" },
#define X_NPROC         1
        { "_nproc" },
#define X_CCPU          2
        { "_ccpu" },
#define X_AVENRUN       3
        { "_avenrun" },
#define X_USRPTMAP      4
        { "_Usrptmap" },
#define X_USRPT         5
        { "_usrpt" },
#define X_NSWAP         6
        { "_nswap" },
#define X_SWAPMAP       7
        { "_swapmap" },
#define X_NSWAPMAP      8
        { "_nswapmap" },
#define X_DMMIN         9
        { "_dmmin" },
#define X_DMMAX         10
        { "_dmmax" },
#define X_NSWDEV        11
        { "_nswdev" },
        { "" }
};

int     kmem = -1;
int     mem = -1;
int     swap = -1;

int     die();
int     display();
int     suspend();

int     showpigs(), openpigs(), fetchpigs(), labelpigs();
int     showswap(), fetchswap(), labelswap();
int     showuser(), openuser(), fetchuser(), labeluser();
int     shownet(), opennet(), fetchnet(), labelnet();

struct  cmdtab {
        char    *c_name;
        int     (*c_refresh)();
        int     (*c_open)();
        int     (*c_fetch)();
        int     (*c_label)();
} cmdtab[] = {
        { "pigs",       showpigs,       openpigs,       fetchpigs,
          labelpigs },
        { "swap",       showswap,       openpigs,       fetchswap,
          labelswap },
#ifdef notdef
        { "user",       showuser,       openuser,       fetchuser,
          labeluser },
        { "net",        shownet,        opennet,        fetchnet,
          labelnet },
#endif
        { "" }
};
struct  cmdtab *curcmd = &cmdtab[0];

main(argc, argv)
        int argc;
        char **argv;
{
        char ch, line[80];

        nlist("/vmunix", nlst);
        (*curcmd->c_open)();
        naptime = 5;
        if (argc != 1)
                naptime = atoi(argv[1]);
        if (naptime < 5)
                naptime = 5;
        signal(SIGINT, die);
        signal(SIGQUIT, die);
        signal(SIGTERM, die);

        /* Initialize curses. */
        initscr();
        wnd = newwin(20, 70, 3, 5);

        lseek(kmem, nlst[X_CCPU].n_value, 0);
        read(kmem, &ccpu, sizeof (ccpu));
        lccpu = log(ccpu);
        (*curcmd->c_fetch)();
        gethostname(hostname, sizeof (hostname));
        labels();

        known[0].k_uid = -1;
        strcpy(known[0].k_name, "<idle>");
        numknown = 1;
        dellave = 0.0;

        signal(SIGALRM, display);
        signal(SIGTSTP, suspend);
        display();
        noecho();
        crmode();
        for (;;) {
                col = 0;
                move(22, 0);
                do {
                        refresh();
                        ch = getch() & 0177;
                        if (ch == 0177 && ferror(stdin)) {
                                clearerr(stdin);
                                continue;
                        }
                        if (ch >= 'A' && ch <= 'Z')
                                ch += 'a' - 'A';
                        if (col == 0) {
                                if (ch == CTRL(l)) {
                                        wrefresh(curscr);
                                        continue;
                                }
                                if (ch != ':')
                                        continue;
                                move(22, 0);
                                clrtoeol();
                        }
                        if (ch == _tty.sg_erase && col > 0) {
                                if (col == 1 && line[0] == ':')
                                        continue;
                                col--;
                                goto doerase;
                        }
                        if (ch == CTRL(w) && col > 0) {
                                while (--col >= 0 && isspace(line[col]))
                                        ;
                                col++;
                                while (--col >= 0 && !isspace(line[col]))
                                        if (col == 0 && line[0] == ':')
                                                break;
                                col++;
                                goto doerase;
                        }
                        if (ch == _tty.sg_kill && col > 0) {
                                col = 0;
                                if (line[0] == ':')
                                        col++;
                doerase:
                                move(22, col);
                                clrtoeol();
                                continue;
                        }
                        if (isprint(ch)) {
                                line[col] = ch;
                                mvaddch(22, col, ch);
                                col++;
                        }
                } while (col == 0 || (ch != '\r' && ch != '\n'));
                line[col] = '\0';
                command(line + 1);
        }
}

command(cmd)
        char *cmd;
{
        register char *cp;
        register struct cmdtab *p;
        char *arg;

        for (cp = cmd; *cp && !isspace(*cp); cp++)
                ;
        if (*cp)
                *cp++ = '\0';
        if (strcmp(cmd, "quit") == 0)
                die();
        if (strcmp(cmd, "status") == 0 || strcmp(cmd, "help") == 0) {
                status();
                return;
        }
        for (p = cmdtab; *p->c_name; p++)
                if (strcmp(cmd, p->c_name) == 0)
                        break;
        if (*p->c_name) {
                if (curcmd == p)
                        return;
                alarm(0);
                curcmd = p;
                wclear(wnd);
                (*p->c_label)();
                display();
                status();
                return;
        }
        if (strcmp(cmd, "stop") == 0) {
                alarm(0);
                mvaddstr(22, 0, "Refresh disabled.");
                clrtoeol();
                return;
        }
        /* commands with arguments */
        for (; *cp && isspace(*cp); cp++)
                ;
        if (strcmp(cmd, "start") == 0) {
                int x;

                if (*cp == '\0')
                        x = naptime;
                else
                        x = atoi(cp);
                if (x <= 0) {
                        mvprintw(22, 0, "%d: bad interval.", x);
                        clrtoeol();
                        return;
                }
                alarm(0);
                naptime = x;
                display();
                status();
                return;
        }
        mvprintw(22, 0, "%s: Unknown command.", cmd);
        clrtoeol();
}

status()
{

        mvprintw(22, 0, "Showing %s, refresh every %d seconds.",
          curcmd->c_name, naptime);
        clrtoeol();
}

suspend()
{
        int oldmask;

        move(22, 0);
        refresh();
        echo();
        nocrmode();
        signal(SIGTSTP, SIG_DFL);
        oldmask = sigsetmask(0);
        kill(getpid(), SIGTSTP);
        sigsetmask(oldmask);
        signal(SIGTSTP, suspend);
        crmode();
        noecho();
        move(22, col);
        wrefresh(curscr);
}

labels()
{

        mvaddstr(2, 20,
                "/0   /1   /2   /3   /4   /5   /6   /7   /8   /9   /10");
        mvwaddstr(wnd, 0, 0, "Load Average");
        (*curcmd->c_label)();
#ifdef notdef
        mvprintw(21, 25, "CPU usage on %s", hostname);
#endif
        refresh();
}

display()
{
        register int i, j;

        /* Get the load average over the last minute. */
        lseek(kmem, nlst[X_AVENRUN].n_value, L_SET);
        read(kmem, &lave, sizeof (lave));
        (*curcmd->c_fetch)();
        j = 5.0*lave + 0.5;
        dellave -= lave;
        if (dellave >= 0.0)
                c = '<';
        else {
                c = '>';
                dellave = -dellave;
        }
        if (dellave < 0.1)
                c = '|';
        dellave = lave;
        wmove(wnd, 0, 15);
        wclrtoeol(wnd);
        for (i = (j > 50)? 50 : j; i > 0; i--)
                waddch(wnd, c);
        if (j > 50)
                wprintw(wnd, " %4.1f", lave);
        (*curcmd->c_refresh)();
        wrefresh(wnd);
        move(22, col);
        refresh();
        alarm(naptime);
}

die()
{

        endwin();
        exit(0);
}
