#include <X/mit-copyright.h>

/* Copyright    Massachusetts Institute of Technology    1985	*/

#ifndef lint
static char *rcsid_xset_c = "$Header: xset.c,v 10.10 86/11/19 19:49:37 jg Rel $";
#endif

#include <X/Xlib.h>
#include <sys/types.h>
#include <stdio.h>
#include <netdb.h>
#include <netinet/in.h>
#include <strings.h>

#define TRUE 1
#define FALSE 0

main(argc, argv)
    int argc;
    char **argv;
{
    char *disp = NULL;
    register char *arg;
    register int i;
    int click, repeat, lock, bell, acc, thresh, video, timeout, cycle;
    int doclick = FALSE, dorepeat = FALSE, dolock = FALSE;
    int dobell = FALSE, domouse = FALSE, dosaver = FALSE;
    int pixels[512];
    caddr_t colors[512];
    int numpixels = 0;
    Color def;

    if (argc == 1)  usage(argv[0]);

    for (i = 1; i < argc; ) {
	arg = argv[i];
	i++;
	if (strcmp(arg, "-c") == 0 || strcmp(arg, "-click") == 0) {
	    click = 0;
	    doclick = TRUE;
	} else if (strcmp(arg, "c") == 0 || strcmp(arg, "click") == 0) {
	    click = 6;
	    doclick = TRUE;
	    if (i >= argc)
		break;
	    arg = argv[i];
	    if (strcmp(arg, "on") == 0) {
		i++;
	    } else if (strcmp(arg, "off") == 0) {
		click = 0;
		i++;
	    } else if (*arg >= '0' && *arg <= '8') {
		click = atoi(arg);
		i++;
	    }
	} else if (strcmp(arg, "-b") == 0 || strcmp(arg, "-bell") == 0) {
	    bell = 0;
	    dobell = TRUE;
	} else if (strcmp(arg, "b") == 0 || strcmp(arg, "bell") == 0) {
	    bell = 6;
	    dobell = TRUE;
	    if (i >= argc)
		break;
	    arg = argv[i];
	    if (strcmp(arg, "on") == 0) {
		i++;
	    } else if (strcmp(arg, "off") == 0) {
		bell = 0;
		i++;
	    } else if (*arg >= '0' && *arg <= '7') {
		bell = atoi(arg);
		i++;
	    }
	} else if (strcmp(arg, "m") == 0 || strcmp(arg, "mouse") == 0) {
	    acc = 4;
	    thresh = 2;
	    domouse = TRUE;
	    if (i >= argc)
		break;
	    arg = argv[i];
	    if (strcmp(arg, "default") == 0) {
		i++;
	    } else if (*arg >= '0' && *arg <= '9') {
		acc = atoi(arg);
		i++;
		if (i >= argc)
			break;
		arg = argv[i];
		if (*arg >= '0' && *arg <= '9') {
			thresh = atoi(arg);
			i++;
		}
	    }
	} else if (strcmp(arg, "s") == 0 || strcmp(arg, "saver") == 0 ||
		   strcmp(arg, "v") == 0 || strcmp(arg, "video") == 0) {
	    timeout = 10;
	    cycle = 60;
	    video = *arg == 's' ? 1 : 0;
	    dosaver = TRUE;
	    if (i >= argc)
		break;
	    arg = argv[i];
	    if (strcmp(arg, "default") == 0) {
		i++;
	    } else if (*arg >= '0' && *arg <= '9') {
		timeout = atoi(arg);
		i++;
		if (i >= argc)
			break;
		arg = argv[i];
		if (*arg >= '0' && *arg <= '9') {
			cycle = atoi(arg);
			i++;
		}
	    }
	} else if (strcmp(arg, "-r") == 0 || strcmp(arg, "-repeat") == 0) {
	    repeat = FALSE;
	    dorepeat = TRUE;
	} else if (strcmp(arg, "r") == 0 || strcmp(arg, "repeat") == 0) {
	    repeat = TRUE;
	    dorepeat = TRUE;
	    if (i >= argc)
		break;
	    arg = argv[i];
	    if (strcmp(arg, "on") == 0) {
		i++;
	    } else if (strcmp(arg, "off") == 0) {
		repeat = FALSE;
		i++;
	    }
	} else if (strcmp(arg, "-l") == 0 || strcmp(arg, "-lock") == 0) {
	    lock = FALSE;
	    dolock = TRUE;
	} else if (strcmp(arg, "l") == 0 || strcmp(arg, "lock") == 0) {
	    lock = TRUE;
	    dolock = TRUE;
	    if (i >= argc)
		break;
	    arg = argv[i];
	    if (strcmp(arg, "on") == 0) {
		i++;
	    } else if (strcmp(arg, "off") == 0) {
		lock = FALSE;
		i++;
	    }
	} else if (strcmp(arg, "p") == 0 || strcmp(arg, "pixel") == 0) {
	    if (i + 1 >= argc)
		usage(argv[0]);
	    arg = argv[i];
	    if (*arg >= '0' && *arg <= '9')
		pixels[numpixels] = atoi(arg);
	    else
		usage(argv[0]);
	    i++;
	    colors[numpixels] = argv[i];
	    i++;
	    numpixels++;
	} else if (index(arg, ':')) {
	    disp = arg;
	} else
	    usage(argv[0]);
    }
    if (!doclick && !dorepeat && !dolock && !dobell && !domouse && !dosaver &&
	numpixels == 0)
    	usage(argv[0]);

    if (XOpenDisplay(disp) == NULL) {
 	fprintf(stderr, "%s: Can't open display '%s'\n",
		    argv[0], XDisplayName(disp));
	exit(1);
	}
    if (doclick) XKeyClickControl(click);
    if (dolock) {
	if (lock)  XLockToggle();
	else XLockUpDown();
	}
    if (dorepeat) {
	if (repeat) XAutoRepeatOn();
	else XAutoRepeatOff();
	}
    if (dobell) XFeepControl(bell);
    if (domouse) XMouseControl(acc, thresh);
    if (dosaver) XScreenSaver(timeout, cycle, video);
    if (DisplayCells() >= 2) {
	while (--numpixels >= 0) {
	    def.pixel = pixels[numpixels];
	    if (XParseColor(colors[numpixels], &def))
		XStoreColor(&def);
	    else
		fprintf(stderr, "%s: No such color\n", colors[numpixels]);
	}
    }
    XSync(0);
    exit(0);
}

usage(prog)
	char *prog;
{
	printf("usage: %s option [option ...] [host:vs]\n", prog);
	printf("    To turn bell off:\n");
	printf("\t-b                b off               b 0\n");
        printf("    To set bell volume:\n");
        printf("\t b [1-7]          b on\n");
        printf("    To turn keyclick off:\n");
        printf("\t-c                c off               c 0\n");
        printf("    To set keyclick volume:\n");
        printf("\t c [1-8]          c on\n");
        printf("    To turn shift-lock key off or on:\n");
        printf("\t-l                l off\n");
        printf("\t l                l on\n");
        printf("    To set mouse acceleration and threshold:\n");
        printf("\t m [acc [thr]]    m default\n");
        printf("    To set pixel colors:\n");
        printf("\t p pixel_value color_name\n");
	printf("    To turn auto-repeat off or on:\n");
        printf("\t-r                r off\n");
        printf("\t r                r on\n");
	printf("    To make the screen-saver display a pattern:\n");
        printf("\t s [timeout [cycle]]  s default\n");
	printf("    To make the screen-saver blank the video:\n");
        printf("\t v [timeout [cycle]]  v default\n");
        exit(0);
}
