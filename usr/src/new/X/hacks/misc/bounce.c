#include <stdio.h>
#include <X/Xlib.h>
#include <signal.h>
#include <math.h>

#define MAXWIN 24

/* Bounce, by Steven Grady */

#define abs(x) ((x) > 0 ? x : (-x))
struct window {
	Window win;
	WindowInfo winf;
	float v;
} windows[MAXWIN], savedwins[MAXWIN];

int totwin;
float elast = 0.5, el, gravity = 2.0;
int bottom;
int beep;
int dontfix = 0;

int bye();

main(argc, argv)
int argc;
char **argv;
{
	int e, i;
	struct window *wind;
	int c;
	extern int optind;
	extern char *optarg;
	int errflg = 0;

	
	while ((c = getopt(argc, argv, "sbg:e:")) != EOF)
		switch (c) {
		case 'b':
			beep++;
			break;
		case 'g':
			gravity = (float) atoi(optarg)/ 10.0;
			if (gravity < 0.0) {
				fprintf(stderr,
					"Try to control yourself, please. \n");
				exit(1);
			}
			break;
		case 'e':
			elast = (float) atoi(optarg)/ 10.0;
			break;
		case 's':
			dontfix++;
			break;
		case '?':
			errflg++;
			break;
		}
	if (errflg) {
		fprintf(stderr, "usage: %s [-b] [-g gravity] [-e elasticity]",
				argv[0]);
		fprintf(stderr, " [-s] [display]\n");
		fprintf(stderr, "\t-b: turn on beep   -s: don't restore ");
		fprintf(stderr, "screen\n\tdefault g=20");
		fprintf(stderr, "\tdefault e=5\n");
		exit(1);
	}

	XOpenDisplay((optind < argc) ? argv[optind] : (char *) NULL);
		
	totwin = get_windows();
	signal(SIGINT, bye);
	for (i = 0; i < totwin; i++) {
		wind = &windows[i];
		el = elast * 1.2; /* Make up for some of the rounding */
		do {
			calc_windows(wind);
			disp_windows(wind);
			XSync(0);
		} while (!stopped(wind));
	}
	fix();
	exit();
/*
	if (!dontfix) {
		printf("Waiting for an interrupt..\n");
		pause();
		/* Never returns -- bye() does an exit() 
	}
*/
}

stopped(w)
struct window *w;
{

	if (abs(w->v) < (float) gravity) {
		return((int) abs(bottom - (w->winf.y + w->winf.height))
				< 10);
	} else
		return(0);
}

calc_windows(win)
struct window *win;
{
	int new_y;
	float new_v;

	new_y = win->winf.y + win->v;
	new_v = win->v + gravity;
	if (new_y+win->winf.height > bottom) {
		new_y = 2*bottom-(new_y+2*win->winf.height);
		if (beep) {
			XFeep((int) log((double) (win->v*win->v *
				sqrt((float) win->winf.height) / 30)) - 6);
		}
		new_v = -new_v*el;
		el *= 0.8;
	}
	win->winf.y = new_y;
	win->v = new_v;
}

int
get_windows()
{
	int n, i, j;
	Window *children, par;
	WindowInfo winf;

	XQueryTree(RootWindow, &par, &n, &children);
	for (i = 0, j = 0; i < n; i++) {
		XQueryWindow(children[i], &winf);
		if (winf.mapped == IsMapped) {
			windows[j].winf = winf;
			windows[j].win = children[i];
			windows[j].v = 0;
			savedwins[j] = windows[j];
			j++;
		}
	}
	bottom = DisplayHeight() - 3; /*Give allowance for a reasonable borde*/
	return(j);
}

disp_windows(win)
struct window *win;
{
	int i;

	XMoveWindow(win->win, win->winf.x, win->winf.y);
	XMapWindow(win->win);
	XFlush();
}

bye()
{
	if (!dontfix) {
		fix();
	}
	exit(0);
}

fix()
{
	int i;
	struct window sw;

	for (i = 0; i < totwin; i++) {
		sw = savedwins[i];
		XMoveWindow(sw.win, sw.winf.x, sw.winf.y);
		XMapWindow(sw.win);
		XFlush();
	}
}
