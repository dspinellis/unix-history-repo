#include <X/mit-copyright.h>

/* Copyright 1985, Massachusetts Institute of Technology */
#ifndef lint
static char            *rcsid_xfd_c = "$Header: xfd.c,v 1.1 86/06/08 19:33:55 andrew Exp $";
#endif

#include <X/Xlib.h>
#include <stdio.h>
#include <strings.h>
#include <ctype.h>

short		gray_bits[16] = {
			0xaaaa, 0x5555, 0xaaaa, 0x5555,
			0xaaaa, 0x5555, 0xaaaa, 0x5555,
			0xaaaa, 0x5555, 0xaaaa, 0x5555,
			0xaaaa, 0x5555, 0xaaaa, 0x5555
		};

char		*trueFalse[] = {"False", "True"};

char		chars[9];
int		last_line;

main(argc, argv)
int	argc;
char	**argv;
{
	Window		w;
	FontInfo	*fontInfo;
	int		width;
	char		*fontname = "vtsingle";
	register char	*option;
	char		*border_color;
	char		*back_color;
	char		*fore_color;
	int		border_width;
	int		reverse = 0;
	char		*geometry;	/* user supplied geometry spec */
	char		def[32];	/* default size */
	int		defwidth;
	int		defheight;
	char		display[128];
	register int	i;
	OpaqueFrame	window;	/* frame for the window */
	Pixmap		border_pixmap;
	int		background;	/* color of background */
	int		foreground;	/* color of graph */
	int		highlight;	/* color of text, scale */
	Color		cdef;

	if ((option = XGetDefault(argv[0], "ReverseVideo")) != NULL)
		if (strcmp(option, "on") == 0)
			reverse = 1;
	if ((option = XGetDefault(argv[0], "BorderWidth")) != NULL)
		border_width = atoi(option);
	if ((border_color = XGetDefault(argv[0], "Border")) == NULL)
		border_color = XGetDefault(argv[0], "BorderColor");
	back_color = XGetDefault(argv[0], "Background");
	fore_color = XGetDefault(argv[0], "Foreground");
	display[0] = '\0';

	for (i = 1; i < argc; i++) {	/* Parse line */
		if (argv[i][0] == '=') {
			geometry = argv[i];
			continue;
		}
		if (index(argv[i], ':') != NULL) {	/* host:display */
			(void) strncpy(display, argv[i], sizeof(display));
			continue;
		}
		if (strcmp(argv[i], "-rv") == 0 ||
		    strcmp(argv[i], "-reverse") == 0) {	/* black on white */
			reverse = 1;
			continue;
		}
		if (strcmp(argv[i], "-fw") == 0 ||
		    strcmp(argv[i], "-forward") == 0) {	/* white on black */
			reverse = 0;
			continue;
		}
		if (strcmp(argv[i], "-bw") == 0 ||
		    strcmp(argv[i], "-border") == 0) {	/* border width */
			if (++i >= argc)
				usage(argv[0]);
			border_width = atoi(argv[i]);
			continue;
		}
		if (strcmp(argv[i], "-bd") == 0 ||
		    strcmp(argv[i], "-color") == 0) {	/* border color */
			if (++i >= argc)
				usage(argv[0]);
			border_color = argv[i];
			continue;
		}
		/* foreground color */
		if (strcmp(argv[i], "-fg") == 0 ||
		    strcmp(argv[i], "-foreground") == 0) {
			if (++i >= argc)
				usage(argv[0]);
			fore_color = argv[i];
			continue;
		}
		/* background color */
		if (strcmp(argv[i], "-bg") == 0 ||
		    strcmp(argv[i], "-background") == 0) {
			if (++i >= argc)
				usage(argv[0]);
			back_color = argv[i];
			continue;
		}
		if (argv[i][0] == '-')
			usage(argv[0]);
		fontname = argv[i];
	}
	if (!XOpenDisplay(display)) {
		fprintf(stderr, "%s: Could not open display %s!\n", 
			argv[0], display);
		exit(1);
	}
	if (!(fontInfo = XOpenFont(fontname))) {
		fprintf(stderr, "%s: Could not open font %s!\n", 
			argv[0], fontname);
		exit(1);
	}
	printf("Font info:\n");
	printf("  id:		%d\n", fontInfo->id);
	printf("  height:	%d\n", fontInfo->height);
	printf("  width:	%d\n", fontInfo->width);
	printf("  baseline:	%d\n", fontInfo->baseline);
	printf("  firstchar:	%d\n", fontInfo->firstchar);
	printf("  lastchar:	%d\n", fontInfo->lastchar);
	printf("  fixed width:	");
	if (fontInfo->fixedwidth)
		printf("true\n");
	else {
		printf("false");

		for (i = fontInfo->firstchar; i <= fontInfo->lastchar; i++) {
			if (i % 4 == 0 || i == fontInfo->firstchar) {
				printf("\n\t");
			}
			if (i < 32) {
				printf("%3d ^%c  %2d  |  ", 
					i, i + 64, fontInfo->widths[i]);
			} else if (i > 160) {
				printf("%3d M%c  %2d  |  ",
					i, i - 128, fontInfo->widths[i]);
			} else if (i > 127) {
				printf("%3d M%c  %2d  |  ",
					i, i - 64, fontInfo->widths[i]);
			} else {
				printf("%3d  %c  %2d  |  ", 
					i, i, fontInfo->widths[i]);
			}
		}
		printf("\n");
	}

	last_line = (unsigned char) (fontInfo->lastchar) / 8;
	width = ComputeWidth(fontInfo);

	if (border_color && DisplayCells() > 2 &&
	    XParseColor(border_color, &cdef) && XGetHardwareColor(&cdef))
		border_pixmap = XMakeTile(cdef.pixel);
	else if (border_color && strcmp(border_color, "black") == 0)
		border_pixmap = BlackPixmap;
	else if (border_color && strcmp(border_color, "white") == 0)
		border_pixmap = WhitePixmap;
	else
		border_pixmap = XMakePixmap(XStoreBitmap(16, 16, gray_bits),
					    BlackPixel, WhitePixel);

	if (back_color && DisplayCells() > 2 &&
	    XParseColor(back_color, &cdef) && XGetHardwareColor(&cdef)) {
		background = cdef.pixel;
	} else if (back_color && strcmp(back_color, "white") == 0) {
		background = WhitePixel;
		reverse = 0;
	} else if (back_color && strcmp(back_color, "black") == 0) {
		background = BlackPixel;
		reverse = 0;
	} else
		background = BlackPixel;

	if (fore_color && DisplayCells() > 2 &&
	    XParseColor(fore_color, &cdef) && XGetHardwareColor(&cdef)) {
		foreground = cdef.pixel;
	} else if (fore_color && strcmp(fore_color, "black") == 0) {
		foreground = BlackPixel;
		reverse = 0;
	} else if (fore_color && strcmp(fore_color, "white") == 0) {
		foreground = WhitePixel;
		reverse = 0;
	} else
		foreground = WhitePixel;

	if (reverse) {
		highlight = background;
		background = foreground;
		foreground = highlight;
	}
	window.bdrwidth = border_width;
	window.border = border_pixmap;
	window.background = XMakeTile(background);

	defwidth = width + 10;
	defheight = fontInfo->height * (last_line + 1) + 10;
	(void) sprintf(def, "=%dx%d+300+300", defwidth, defheight);
	w = XCreate("Font Display", argv[0], geometry, def, &window,
		    defwidth, defheight);

	if (!w) {
		fprintf(stderr, "XCreateWindow failed\n");
		exit(1);
	}
	XSelectInput(w, ExposeWindow | ButtonPressed);
	XMapWindow(w);
	while (1) {
		XEvent                  event;
		int                     i, j;
		XWindowEvent(w, ExposeWindow | ButtonPressed, &event);
		if (event.type == ButtonPressed)
			exit(0);
		for (i = 0; i <= last_line; i++) {
			for (j = 0; j < 8; j++)
				chars[j] = (char) ((8 * i) + j);
			XText(w, 5, 5 + (i * fontInfo->height), chars, 8,
			      fontInfo->id, foreground, background);
		}
	}
}

usage(program)
char                   *program;
{
	fprintf(stderr, "usage: %s [host:display] [=geom] [-fw] ", program);
	fprintf(stderr, "[-rv] [-bw] [-bd] [-fg] [-bg] <fontname>\n");
	exit(1);
}

ComputeWidth(fontInfo)
FontInfo               *fontInfo;
{
	int                     maxwidth, i, j;
	/* Horrible hack needed for first line because line starts with \0, and
	 * XStringWidth considers \0 to terminate string */
	for (j = 1; j < 8; j++)
		chars[j] = j;
	maxwidth = XStringWidth(&chars[1], fontInfo, 0, 0);
	/* add the width of the '\0' character, if it has one */
	if (fontInfo->firstchar == '\0')
		maxwidth += (fontInfo->fixedwidth ?
			     fontInfo->width : fontInfo->widths[0]);

	/* now measure the width of remaining lines */
	for (i = 1; i <= last_line; i++) {
		int                     this_width;
		for (j = 0; j < 8; j++)
			chars[j] = (char) ((8 * i) + j);
		this_width = XStringWidth(chars, fontInfo, 0, 0);
		if (this_width > maxwidth)
			maxwidth = this_width;
	}
	return (maxwidth);
}
