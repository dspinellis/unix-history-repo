#ifndef lint
static char sccsid[] = "@(#)main.c	1.4 (Lucasfilm) %G%";
#endif

#include "systat.h"

static struct nlist nlst[] = {
#define X_CCPU          0
        { "_ccpu" },
#define X_AVENRUN       1
        { "_avenrun" },
        { "" }
};

int     kmem = -1;
int     mem = -1;
int     swap = -1;
int	naptime = 5;

int     die();
int     display();
int     suspend();

main(argc, argv)
        int argc;
        char **argv;
{

	argc--, argv++;
	while (argc > 0) {
		if (argv[0][0] == '-') {
			struct cmdtab *p;

			for (p = cmdtab; *p->c_name; p++)
				if (strcmp(p->c_name, &argv[0][1]) == 0)
					break;
			if (*p->c_name == 0) {
				fprintf(stderr, "%s: unknown request\n",
				    &argv[0][1]);
				exit(1);
			}
			curcmd = p;
		} else {
			naptime = atoi(argv[1]);
			if (naptime < 5)
				naptime = 5;
		}
		argc--, argv++;
	}
        nlist("/vmunix", nlst);
	if (nlst[X_CCPU].n_type == 0) {
		fprintf(stderr, "Couldn't namelist /vmunix.\n");
		exit(1);
	}
	kmemf = "/dev/kmem";
	kmem = open(kmemf, O_RDONLY);
	if (kmem < 0) {
		perror(kmemf);
		exit(1);
	}
	memf = "/dev/mem";
	mem = open(memf, O_RDONLY);
	if (mem < 0) {
		perror(memf);
		exit(1);
	}
	swapf = "/dev/drum";
	swap = open(swapf, O_RDONLY);
	if (swap < 0) {
		perror(swapf);
		exit(1);
	}
        signal(SIGINT, die);
        signal(SIGQUIT, die);
        signal(SIGTERM, die);

        /*
	 * Initialize display.  Load average appears in standard
	 * window with current display's overlapping sub-window
	 * maintained by the display routines to minimize update
	 * work by curses.
	 */
        initscr();
	wnd = (*curcmd->c_open)();
	if (wnd == NULL) {
		fprintf(stderr, "Couldn't initialize display.\n");
		die();
	}

#ifdef notdef
        gethostname(hostname, sizeof (hostname));
#endif
        lseek(kmem, nlst[X_CCPU].n_value, 0);
        read(kmem, &ccpu, sizeof (ccpu));
        lccpu = log(ccpu);
	(*curcmd->c_init)();
	curcmd->c_flags = 1;
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
	keyboard();
	/*NOTREACHED*/
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

load()
{

	lseek(kmem, nlst[X_AVENRUN].n_value, L_SET);
	read(kmem, &lave, sizeof (lave));
	mvprintw(22, 0, "%4.1f", lave);
	clrtoeol();
}

die()
{

        endwin();
        exit(0);
}

error(fmt, a1, a2, a3)
{

	mvprintw(22, 0, fmt, a1, a2, a3);
	clrtoeol();
	refresh();
}
