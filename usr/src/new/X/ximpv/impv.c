/*
 * Copyright (c) 1985 University of Alberta *
 *
 * 'impv' impress (i.e. canon) previewer for a SUN workstation.
 * 	impv [-p##] [-s] [job# | -n name]
 * where job# is the job number of the troff/dimp processed output suitable
 * for the canon imagen printer. This can be found by executing 'ipq'. The 
 * -p## argument sets the number of pages the user will be allowed to
 * backup.  The -n name option will cause the users most recent job to
 * be displayed.The human interface for this proram leaves something to be
 * desired ( skip pages, print page number 'n', etc.).
 * The algorithm for crunching glyphs could be improved from the straight 
 * forward method used.
#ifdef COLOR
 * The -s option will cause the output to be sent to the color monitor. Troff
 * Macros .CC and .BG set the background and current color. This option is
 * intended for slide production.
#endif COLOR
 * 
 * 	This program requires that a 'file server' (used very loosely)
 * exist on the remote host UNIX system. See the code for impvserv. 
 *
 * history:
 *
 * The program was written in desparation by:
 *		Steven Sutphen
 *		University of Alberta
 * 		Department of Computing Science
 *		Edmonton, Alberta T6G 2H1
 *		ihnp4!alberta!steve
 *		November 20, 1982
 *
 *	        Revised for Sun Work Station:
 *			January 1, 1984 Ted Bentley
 *		Revised for Color SUN displays:
 *			January 1985 Martin Dubetz
 */
#ifndef lint
static char *rcsid_impv_c = "$Header: impv.c,v 10.7 86/11/19 19:27:39 jg Rel $";
#endif

#ifdef XWIND
#include <X/Xlib.h>
#include <X/Xkeyboard.h>
#include <sys/types.h>
#include <strings.h>
#define min(a,b) ((a) < (b) ? (a) : (b))
#define CHUNKSIZE 2048
#else XWIND
#include <pixrect/pixrect_hs.h>
/* pixrect_hs.h includes sys/types.h */
#include <sys/socket.h>
#endif XWIND
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>
#include <signal.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sgtty.h>

#include	"site.h"
#include	"imPdefs.h"
#include	"imPcodes.h"
#include	"impv.h"


char map8_4[256] = {	/* map 8 bits to 4bits for pixel compression */
	00, 01, 01, 01, 02, 03, 03, 03, 02, 03, 03, 03, 02, 03, 03, 03,
	04, 05, 05, 05, 06, 07, 07, 07, 06, 07, 07, 07, 06, 07, 07, 07,
	04, 05, 05, 05, 06, 07, 07, 07, 06, 07, 07, 07, 06, 07, 07, 07,
	04, 05, 05, 05, 06, 07, 07, 07, 06, 07, 07, 07, 06, 07, 07, 07,
	010, 011, 011, 011, 012, 013, 013, 013, 012, 013, 013, 013, 012, 013, 013, 013,
	014, 015, 015, 015, 016, 017, 017, 017, 016, 017, 017, 017, 016, 017, 017, 017,
	014, 015, 015, 015, 016, 017, 017, 017, 016, 017, 017, 017, 016, 017, 017, 017,
	014, 015, 015, 015, 016, 017, 017, 017, 016, 017, 017, 017, 016, 017, 017, 017,
	010, 011, 011, 011, 012, 013, 013, 013, 012, 013, 013, 013, 012, 013, 013, 013,
	014, 015, 015, 015, 016, 017, 017, 017, 016, 017, 017, 017, 016, 017, 017, 017,
	014, 015, 015, 015, 016, 017, 017, 017, 016, 017, 017, 017, 016, 017, 017, 017,
	014, 015, 015, 015, 016, 017, 017, 017, 016, 017, 017, 017, 016, 017, 017, 017,
	010, 011, 011, 011, 012, 013, 013, 013, 012, 013, 013, 013, 012, 013, 013, 013,
	014, 015, 015, 015, 016, 017, 017, 017, 016, 017, 017, 017, 016, 017, 017, 017,
	014, 015, 015, 015, 016, 017, 017, 017, 016, 017, 017, 017, 016, 017, 017, 017,
	014, 015, 015, 015, 016, 017, 017, 017, 016, 017, 017, 017, 016, 017, 017, 017, 
};

#ifdef XWIND
int Select_mask, maxplus1;
int forepix, backpix;
OpaqueFrame win;
Window	Win;		/* our window */
#define impv_width 15
#define impv_height 15
static short impv_bits[] = {
   0x0080, 0x01c0, 0x03e0, 0x06b0,
   0x0c98, 0x188c, 0x3086, 0x7fff,
   0x3086, 0x188c, 0x0c98, 0x06b0,
   0x03e0, 0x01c0, 0x0080};
#else XWIND
struct pixrect *picon, *ptube, *side;
#endif XWIND

int diameter = 1;
short pages = SAVE_PAGES;
char screen_file[32];
unsigned char in_buf[512], *in_bufp;
int ptubew, ptubeh;	/* screen window dimensions */
struct sgttyb cbreak;	/* tty mode bits */
int s = 0;			/* socket or input file descriptor */
int tty;		/* tty file descriptor */
int screens;		/* screen store file descriptor */

main(argc, argv)
char **argv;
{
	struct sockaddr_in sin;
#ifdef XWIND
	char *display = NULL;
	char *option;
	int reverse = 0;
	int bwidth = 2;
	char *fore_color;
	char *back_color;
	char *brdr_color;
	char *mous_color;
	char *geometry = NULL, def[32];
	int backmap, bdrmap, mouspix;
	Color cdef;
#else XWIND
#ifdef COLOR
	unsigned char r[216],b[216],g[216],tmp[6];
	register short i,j,k;
	int l;
	register unsigned char *z;
	unsigned char setcolor();
#endif COLOR
	extern struct pixrect *pr_open();
#endif XWIND
	int pipes[2], id_num = 0, rq;
	char string[100], buf[28];
	struct hostent *hp;
	struct servent *sp;
	struct stat stat1;
	char *prog = argv[0];

	extern get_out();

#ifdef XWIND
	if ((option = XGetDefault(argv[0], "ReverseVideo")) &&
	    strcmp(option, "on") == 0)
		reverse = 1;
	if (option = XGetDefault(argv[0], "BorderWidth"))
		bwidth = atoi(option);
	fore_color = XGetDefault(argv[0], "ForeGround");
	back_color = XGetDefault(argv[0], "BackGround");
	brdr_color = XGetDefault(argv[0], "Border");
	mous_color = XGetDefault(argv[0], "Mouse");
#else XWIND
	/* set up the keyboard input */
	(void)gtty(0, &cbreak);
	cbreak.sg_flags &= ~ECHO;
	cbreak.sg_flags |= CBREAK;
	(void)stty(0, &cbreak);
	cbreak.sg_flags &= ~CBREAK;
	cbreak.sg_flags |= ECHO;
#endif XWIND
	signal(SIGINT, get_out);
	signal(SIGQUIT, get_out);
	signal(SIGHUP, get_out);
	signal(SIGIOT, get_out);

	scr_x = MAXx / 2;		/* number of dots on a screen page */
	wide =  (7 + MAXx/2) / 8;	/* rounded up to byte for pixrect */
	scr_y = MAXy / 2;
	scr_d = 1;
	/* calculate the screen size in bytes 2::1 compression */
	scr_size = (((scr_x + 7) / 8) * scr_y);

	for (argv++; --argc; argv++) {
		if (**argv == '-'){
			switch (argv[0][1]){
#ifndef NOSPOOL
			case 'n': 
				if (pipe(pipes)) {
					fprintf(stderr, "pipe mistake\n");
					get_out();
				}
				if (fork() == 0){
					/*child*/
					(void)close(1);
					(void)dup(pipes[1]);
					(void)sprintf(string, "echo `ipq | grep %s | awk ' {print $3}'| sort -rn | head -1 `", *++argv);
					(void)system(string);
					exit(0);
				}
				/* parent */
				while (read(pipes[0], buf, 20) <= 0);
				id_num = atoi(buf);
				argv++; 
				argc--;
				break;
			case 'r': 
				rq = 1;
				break;
#endif NOSPOOL
			case 'p': 
				pages = atoi( &argv[0][2] );
				if (pages < 0) pages = 0;
				break;
#ifdef COLOR
			case 's':
				slide = 1;
				scr_x = MAXx / 3;
				scr_y = MAXy / 3;
				scr_d = 8;
				scr_size = scr_x * scr_y;
				bc.red = 0;
				bc.blue = 255;
				bc.green = 0;
				cc.red = 255;
				cc.blue = 255;
				cc.green = 255;
				break;
#endif COLOR
			default:
#ifdef XWIND
				if (strcmp(*argv, "-rv") == 0) {
					reverse = 1;
					break;
				} else if (strcmp(*argv, "-fg") == 0 && argc) {
					argv++;
					argc--;
					fore_color = *argv;
					break;
				} else if (strcmp(*argv, "-bg") == 0 && argc) {
					argv++;
					argc--;
					back_color = *argv;
					break;
				} else if (strcmp(*argv, "-bd") == 0 && argc) {
					argv++;
					argc--;
					brdr_color = *argv;
					break;
				} else if (strcmp(*argv, "-ms") == 0 && argc) {
					argv++;
					argc--;
					mous_color = *argv;
					break;
				} 
#endif XWIND
				(void)usage();
				get_out();
			}
		} 
		else {
#ifdef XWIND
			if (index(*argv, ':') != NULL)
				display = *argv;
			else if (argv[0][0] == '=') {
					geometry = argv[0];
			}

			else
#endif XWIND
#ifdef NOSPOOL
				if((s = open(*argv, O_RDONLY)) <= 0){
					fprintf(stderr,"can't open file\n");
					exit(0);
				}
#else NOSPOOL
				id_num = atoi(*argv);
#endif NOSPOOL
		}
	}
#ifdef NOSPOOL
	/* set up tty for piped input situation*/
	if(s == 0) {
		s = dup(0);
		close(0);
		tty = open("/dev/tty",O_RDONLY);
		fstat(s, &stat1);
		if( (stat1.st_mode>>12) == 02 )
			(void)usage();
	}
#else NOSPOOL
	if(id_num == 0) (void)usage();
#endif NOSPOOL

	/* get the file for screen pages */
	if (pages) {
		strcpy(screen_file, SCREEN_FILE);
		mktemp(screen_file);
		if ((screens = creat(screen_file, 0666)) <= 0){
			fprintf(stderr, "couldn't create: %s\n", screen_file);
			get_out();
		}
		(void)close(screens);
		if ((screens = open(screen_file, 2)) <= 0){
			fprintf(stderr, "couldn't reopen: %s\n", screen_file);
			get_out();
		}
	}

#ifndef NOSPOOL
	/*open up the network to get file from remote host */
	sp = getservbyname(P_SERV, "tcp");
	hp = gethostbyname(REMOTE_HOST);
	if (hp == NULL) {
		fprintf(stderr, "impv: nohost - %s\n", REMOTE_HOST);
		get_out();
	}
	bzero((char *)&sin, sizeof (sin));
	bcopy(hp->h_addr, (char *)&sin.sin_addr, hp->h_length);
	sin.sin_family = hp->h_addrtype;
	sin.sin_port = sp->s_port;
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0) {
		perror("impv: socket");
		get_out();
	}
 	(void)setsockopt(s, SOL_SOCKET, SO_KEEPALIVE, 0, 0);
	if (connect(s, (char *)&sin, sizeof (sin)) < 0) {
		perror("impv: connect");
		get_out();
	}
	/* write the ipd file name to remote_host */
	(void)sprintf(string, "P%c%03d", rq ? 'r' : 'n', id_num);
	(void)write(s, string, strlen(string));
#endif NOSPOOL

	in_bufp = in_buf;
	family[0] = font0;
	inicodes();

#ifdef XWIND
	if (!XOpenDisplay(display)) {
	    fprintf(stderr, "%s: Can't open display '%s'\n",
		 prog, XDisplayName(display));
	    exit(1);
	}
	if (reverse) {
		forepix = WhitePixel;
		backpix = BlackPixel;
		backmap = BlackPixmap;
		bdrmap = WhitePixmap;
		mouspix = WhitePixel;
	} else {
		forepix = BlackPixel;
		backpix = WhitePixel;
		backmap = WhitePixmap;
		bdrmap = BlackPixmap;
		mouspix = BlackPixel;
	}
	if (DisplayCells() > 2) {
		if (fore_color && XParseColor(fore_color, &cdef) &&
			XGetHardwareColor(&cdef))
			forepix = cdef.pixel;
		if (back_color && XParseColor(back_color, &cdef) &&
			XGetHardwareColor(&cdef)) {
			backpix = cdef.pixel;
			backmap = XMakeTile(backpix);
		}
		if (brdr_color && XParseColor(brdr_color, &cdef) &&
			XGetHardwareColor(&cdef))
			bdrmap = XMakeTile(cdef.pixel);
		if (mous_color && XParseColor(mous_color, &cdef) &&
			XGetHardwareColor(&cdef))
			mouspix = cdef.pixel;
	}
	win.bdrwidth = bwidth;
	win.border = bdrmap;
	win.background = backmap;

	sprintf(def, "=%dx%d+0+0", DisplayWidth() - (bwidth << 1),
		DisplayHeight() - (bwidth << 1));
	Win = XCreate (
		"Imagen Previewer", argv[0], geometry, def, &win, 200, 200);
	XSetResizeHint(Win, 100, 100, 1, 1);
	XMapWindow (Win);
	ptubew = win.width;
	/* crock around crock */
	if ((DisplayType() == XDEV_QDSS) && (bwidth < 4))
		ptubew &= ~7;
	if (ptubew > scr_x)
		ptubew = scr_x;
	ptubeh = win.height;
	if (ptubeh > scr_y)
		ptubeh = scr_y;

	XSelectInput (Win, KeyPressed|ButtonPressed|ExposeRegion|ExposeCopy);
	XDefineCursor(Win,
		XCreateCursor(impv_width, impv_height, impv_bits, impv_bits,
				7, 7, mouspix, backpix, GXcopy));
	XSync(0);
	Select_mask = 1 << dpyno();
	maxplus1 = 1 + dpyno();
#else XWIND
	/* set up the raster pixrect functions */
	if( slide ) {
#ifdef COLOR
		ptube = pr_open("/dev/cgone0");
		tmp[0] = 0;
		tmp[1] = 130;
		tmp[2] = 180;
		tmp[3] = 210;
		tmp[4] = 235;
		tmp[5] = 255;
		l = 0;
		for( i = 0; i < 6; i++ )
			for( j = 0; j < 6; j++ )
				for( k = 0; k < 6; k++ ) {
					r[l] = tmp[i];
					g[l] = tmp[j];
					b[l++] = tmp[k];
				}

		pr_putcolormap(ptube,0,216,r,g,b);
#endif COLOR
	} 
	else
		ptube = pr_open("/dev/fb");
	pscreen = mem_create(scr_x, scr_y, scr_d);
#ifdef COLOR
	if( slide ) {
		backcolor = setcolor(0);
		z = (unsigned char *)(mpr_d(pscreen)->md_image);
		for (l = scr_size; l--; ) *z++ = backcolor;
	} 
#endif COLOR
	picon = mem_create(46, 53, scr_d);
	side = mem_create(ptubew-scr_x, ptubeh, scr_d);
	ptubew = ptube->pr_size.x;
	ptubeh = ptube->pr_size.y;
#endif XWIND

	dofile();
	(void)write(s, string, strlen(string));
	get_out();
}

#ifdef XWIND
unsigned char rot8[] = {
	0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0, 
	0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0, 
	0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8, 
	0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8, 
	0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4, 
	0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4, 
	0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec, 
	0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc, 
	0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2, 
	0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2, 
	0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea, 
	0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa, 
	0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6, 
	0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6, 
	0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee, 
	0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe, 
	0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1, 
	0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1, 
	0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9, 
	0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9, 
	0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5, 
	0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5, 
	0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed, 
	0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd, 
	0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3, 
	0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3, 
	0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb, 
	0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb, 
	0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7, 
	0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7, 
	0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef, 
	0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
};

static short current_page = -1;
static short current_x = -1;
#endif XWIND

ppause()
{
	short  y = 0;
	short  x;
	register int c, i;
	register unsigned char *z, *scptr;
#ifdef COLOR
	unsigned char setcolor();
#endif COLOR

#ifdef XWIND
	short current_y = 0;
	int select_mask;
	XEvent event;
	scptr = pscreen;
	for (i = sizeof pscreen; i--; scptr++) *scptr = rot8[*scptr];
	winker();
	scptr = pscreen;
	winker();
#else XWIND
	scptr = (unsigned char *)(mpr_d(pscreen)->md_image);
#endif XWIND
	if (current_x < 0)
		if((current_x = (scr_x - ptubew)/2) < 0) current_x = 0;
	x = current_x;
	/*write the screen */
	while(1) {
#ifdef XWIND
		if (page == current_page && x == current_x &&
		    y < current_y && y + ptubeh > current_y) {
			int delta = current_y - y;
			XMoveArea(Win, 0, 0, 0, delta, ptubew, ptubeh - delta);
			BitsPut(x, y, 0, 0, ptubew, delta);
		} else if (page == current_page && x == current_x &&
			   y > current_y && current_y + ptubeh > y) {
			int delta = y - current_y;
			XMoveArea(Win, 0, delta, 0, 0, ptubew, ptubeh - delta);
			BitsPut(x, y + ptubeh - delta, 0, ptubeh - delta,
				ptubew, delta);
		} else if (page == current_page && y == current_y &&
		    x < current_x && x + ptubew > current_x) {
			int delta = current_x - x;
			XMoveArea(Win, 0, 0, delta, 0, ptubew - delta, ptubeh);
			BitsPut(x, y, 0, 0, delta, ptubeh);
		} else if (page == current_page && y == current_y &&
			   x > current_x && current_x + ptubew > x) {
			int delta = x - current_x;
			XMoveArea(Win, delta, 0, 0, 0, ptubew - delta, ptubeh);
			BitsPut(x + ptubew - delta, y, ptubew - delta, 0,
				delta, ptubeh);
		} else if (y != current_y || x != current_x ||
			   page != current_page)
			BitsPut(x, y, 0, 0, min(scr_x - x, ptubew),
				min(scr_y - y, ptubeh));
		current_y = y;
		current_x = x;
		current_page = page;
		XFlush();
		select_mask = Select_mask;
		if (select(maxplus1, &select_mask, NULL, NULL, 0) < 0) exit(1);
		if (!(select_mask & Select_mask)) continue;
		XPending();
		do {
			XNextEvent (&event);
			switch (event.type) {
			case ExposeWindow:
				ptubeh = ((XExposeEvent *)(&event))->height;
				ptubew = ((XExposeEvent *)(&event))->width;
			case ExposeRegion: {
				short ex = ((XExposeEvent *)(&event))->x;
				short ew = ((XExposeEvent *)(&event))->width;
				short ey = ((XExposeEvent *)(&event))->y;
				short eh = ((XExposeEvent *)(&event))->height;
				BitsPut(x + ex, y + ey, ex, ey,
					min(scr_x - (x + ex), ew),
					min(scr_y - (y + ey), eh));
				continue;
			}
			case ExposeCopy:
				continue;
			case ButtonPressed:
			{
			short detail = ((XButtonPressedEvent *) (&event))->detail;
			switch (detail & ValueMask) {
			case LeftButton: if (detail & ShiftMask)
						c = 'l';
					 else
						c = 'u';
					 break;
			case MiddleButton: if (detail & ShiftMask) {
					   if (x < scr_x - (x + ptubew))
						c = '6';
					   else
						c = '4';
					   } else {
					   if (y < scr_y - (y + ptubeh))
						c = '3';
					   else
						c = '9';
					   }
					   break;
			case RightButton: if (detail & ShiftMask)
						c = 'r';
					  else
						c = 'd';
					  break;
			}
				break;
			}
			case KeyPressed:
			switch ((((XKeyPressedEvent *) (&event))->detail) & ValueMask) {
			case KC_E5: c = '-'; break; /* prev screen */
			case KC_E6: c = '+'; break; /* next screen */
			case KC_CURSOR_UP: c = 'u'; break; /* prev screen */
			case KC_CURSOR_DOWN: c = 'd'; break; /* next screen */
			case KC_CURSOR_LEFT: c = '<'; break; /* left screen */
			case KC_CURSOR_RIGHT: c = '>'; break; /* right screen */
			case KC_KEYPAD_0: c = '0'; break; /* R0 */
			case KC_KEYPAD_PERIOD: c = '.'; break; /* R. */
			case KC_KEYPAD_COMMA: c = ','; break; /* R, */
			case KC_KEYPAD_1: c = '1'; break; /* R1 */
			case KC_KEYPAD_2: c = '2'; break; /* R2 */
			case KC_KEYPAD_3: c = '3'; break; /* R3 */
			case KC_KEYPAD_4: c = '4'; break; /* R4 */
			case KC_KEYPAD_5: c = '5'; break; /* R5 */
			case KC_KEYPAD_6: c = '6'; break; /* R6 */
			case KC_KEYPAD_7: c = '7'; break; /* R7 */
			case KC_KEYPAD_8: c = '8'; break; /* R8 */
			case KC_KEYPAD_9: c = '9'; break; /* R9 */
			case KC_KEYPAD_MINUS: c = '-'; break; /* R- */

			default:
				{
				char *string;
				int nbytes;
				string = XLookupMapping (&event, &nbytes);
				if (nbytes == 1)
					c = *string;
				else
					continue;
				}
			}
			}
#else XWIND
		/* adjust place of page on screen so it is centered */
		/* will clip the edges of imagen300 pages  */
		pr_rop(ptube, 0, 0, ptubew, ptubeh, PIX_SRC, pscreen, x, y);
		if( !slide ) {
			pr_rop(ptube, scr_x, 0, 4, ptubeh, PIX_NOT(PIX_SRC),
				side, 0, 0);
			page_icon(y);
		}
		/* have a look */
		c = getchar();
		if( c == 033 ){	/* fixup for SUN 120 keypad */
			c = getchar();
			if(c == '[') c = getchar();
			if(c == '2') c = getchar();
			if(c == '2'){
				c = getchar() + 1;
				(void) getchar();
			}
			else if(c == '1'){
				c = getchar() + 3;
				(void) getchar();
			}
		}
#endif XWIND
		switch(c){
		case 04: 
		case 03:
			return(-1);	/* quit on EOF or ^C */
		case '1': 
			y++; 
			break;
		case '7': 
			y--; 
			break;
		case 0102: 
		case '2': 
			y += 65; 
			break;
		case 0101: 
		case '8': 
			y -= 65; 
			break;
		case '3': 
			y = scr_y - ptubeh -1; 
			break;
		case '9': 
			y = 0; 
			break;
		case '4': 
			x = 0;
			break;
		case '5': 
			if((x = (scr_x - ptubew)/2) < 0) x = 0;
			break;
		case '6': 
			if((x = scr_x - ptubew) < 0) x = 0;
			break;
		case '<':
			x -= 65;
			break;
		case '>':
			x += 65;
			break;
		case 'l':
			x -= ptubew;
			break;
		case 'r':
			x += ptubew;
			break;
		case 'd':
			if (y < scr_y - 1 - ptubeh) {
				y += ptubeh;
				break;
			}
			/* falls into */
		case '.':
			if (pages && page != finish) {
				page = (page +1)%pages;
				(void)lseek(screens, scr_size * page, 0);
				(void)read(screens, scptr, scr_size);
				y = 0;
				break;
			}
			/* falls into */
		case '+':
		case ',':
			if (pages && page == finish) {
				(void)lseek(screens, scr_size * page, 0);
				(void)write(screens, scptr, scr_size);
			}
			if (pages){
				page = finish = (finish +1)%pages;
				if(start == finish) start = (start + 1)%pages;
			}
			/*blank the memory where screen image is built*/
			z=scptr;
			for (i=scr_size;i--;) *z++ = backcolor;
			return(0);
		case 'u':
			if (y > 0) {
				y -= ptubeh;
				break;
			}
			/* falls into */
		case '-':
			if (pages == 0 || page == start) {
#ifndef XWIND
			    if (y == 0)
			    fprintf(stderr, "can not back up more\n");
#endif XWIND
			    y = 0;
			}else{
				if(page == finish) {
					(void)lseek(screens, scr_size * page, 0);
					(void)write(screens, scptr, scr_size);
				}
				page = (page==0? (pages-1):(page -1))%pages;
				(void)lseek(screens, scr_size * page, 0);
				(void)read(screens, scptr, scr_size);
				if (c == '-')
					y = 0;
				else
					y = scr_y - 1 - ptubeh;
			}
			break;
		default: 
			break;
		}
		if (x < 0) x = 0;
		else if (x >= scr_x - ptubew) x = scr_x - 1 - ptubew;
		if (y < 0) y = 0;
		else if (y >= scr_y - ptubeh) y = scr_y - 1 - ptubeh;
#ifdef XWIND
		} while (QLength() > 0);
#endif
	}
}

/* get a character from the input file */
gc()
{
	static int len = 0;

	if(macro_on == TRUE) {
		/* macro in effect read from its code*/
		if(macro_length == macro_count+1) macro_on = FALSE;
		return(mp[macro_count++]);
	}
	else {
		/*read from the input file */
		if(in_bufp <= &in_buf[len-1]) {
			return(*in_bufp++);
		}
		len = read(s, in_buf, 512);
		if(len == 0) {
			fprintf(stderr,"No such job\n");
			(void)fflush(stderr);
			get_out();
		}
		else if(len < 0) {
			perror("impv: read failed");
			get_out();
		}

		in_bufp = in_buf;
		winker();
		return(*in_bufp++);
	}
}

get_out()
{
	/*reset keyboard*/
#ifdef XWIND
	XDestroyWindow (Win);
#else XWIND
	(void)stty(0, &cbreak);
	if(pscreen != NULL) pr_close(pscreen);
	if(ptube != NULL) pr_close(ptube);
	printf("\n");
#endif XWIND
	if(big || little)
	    printf("%d pixels/glyphs/lines would be off the page\n",
	big + little);
	if (screens && unlink(screen_file) < 0)
	    printf("%s not removed\n", screen_file);
	exit(0);
}

winker()
{
#ifdef XWIND
	XPixFill(Win, ptubew / 2 - 8, ptubeh - 50, 16, 16, 0, NULL, GXinvert, 1);
	XFlush();
#else XWIND
	pr_rop(ptube, 450, 780, 16, 16, PIX_NOT(PIX_SRC), NULL, 0, 0);
#endif XWIND
}

#ifdef XWIND
unsigned char outbuf[CHUNKSIZE];

BitsPut (srcx, srcy, dstx, dsty, width, height)
	int srcx, srcy, dstx, dsty, width, height;
{
	register unsigned char *data, *ptr;
	register int i, per, delta;
	int linesize;

	linesize = (scr_x + 7) >> 3;
	dstx -= (srcx & 7);
	width += (srcx & 7);
	srcx &= ~7;
	data = &pscreen[(srcy*linesize) + (srcx>>3)];

	per = BitmapSize(width, 1);
	delta = CHUNKSIZE / per;

	while (height) {
		if (height < delta)
			delta = height;
		for (ptr = outbuf, i = delta;
		     --i >= 0;
		     data += linesize, ptr += per)
			bcopy(data, ptr, per);
		XBitmapBitsPut(Win, dstx, dsty, width, delta, outbuf,
				forepix, backpix, NULL, GXcopy, AllPlanes);
		dsty += delta;
		height -= delta;
	}
}
#endif XWIND

#ifndef XWIND
page_icon(y)
int y;
{
	int h, i;

	i = 43 * ptubeh / scr_y;
	pr_rop(picon, 0 , 0, 46, 53, PIX_NOT(PIX_SRC), NULL, 0, 0);
	pr_rop(picon, 2 , 2, 42, 49, PIX_CLR, NULL, 0, 0);
	pr_rop(picon, 6 , 4, 34, 45, PIX_NOT(PIX_SRC), NULL, 0, 0);
	h = 44 * y / scr_y;
	pr_rop(picon, 5, 5+h, 36, i, PIX_NOT(PIX_SRC), picon, 5, 5+h);
	pr_rop(ptube, ptubew-46, ptubeh/2, 46, 53, PIX_SRC, picon, 0, 0);
}
#endif

#ifdef COLOR
/*
	set color
*/
unsigned char
setcolor(code)
register short int code;
{
	register unsigned char color;
	register float c1,c2;
	
	c1 = (float)(5 - code) / 255.;
	c2 = (float)code / 255.;
	color = 36 * ((int)(bc.red * c1 + .5) + (int)(cc.red * c2 + .5))
	        + 6 * ((int)(bc.green * c1 + .5) + (int)(cc.green * c2 + .5))
	        + ((int)(bc.blue * c1 + .5) + (int)(cc.blue * c2 + .5));
	return(color);
}
#endif COLOR

usage()
{
#ifdef NOSPOOL
#ifdef COLOR
		printf("usage: impv [-p#] [-s] [file]\n");
#else COLOR
#ifdef XWIND
		printf("usage: ximpv [=<geometry>] [-p#] [-rv] [-fg <color>] [-bg <color>] [-bd <color>] [-ms <color>] [host:display] [file]\n");
#else XWIND
		printf("usage: impv [-p#] [file]\n");
#endif XWIND
#endif COLOR
	 	(void)fflush(stdout);
		exit(0);
#else NOSPOOL
#ifdef COLOR
		printf("usage: impv [-p#] [-s] [-r] [idnumber | -n name]\n");
#else COLOR
		printf("usage: impv [-p#] [-r] [idnumber | -n name]\n");
#endif COLOR
		(void)fflush(stdout);
		get_out();
#endif NOSPOOL
}
