/*
 * DVI previewer for X.
 *
 * Eric Cooper, CMU, September 1985.
 *
 * Code derived from dvi-imagen.c.
 *
 * Modified for X.10 by Bob Scheifler, MIT LCS, January 1986.
 *
 */
#ifndef lint
static char *dv_c = "$Header: dv.c,v 10.7 86/11/19 19:16:18 jg Rel $";
#endif 	lint

#include <sys/types.h>
#include <X/Xlib.h>
#include <stdio.h>
#include <ctype.h>
#include "dvi.h"
#include "pxl.h"

/* These are probably site dependent */
#define FONT_DIRECTORY	"/usr/lib/tex/fonts"
#define FONT_SUFFIX	".%dpxl"

#define PAPER_WIDTH	ROUNDUP(17*pixels_per_inch,shrink_factor*2)
#define PAPER_HEIGHT	ROUNDUP(11*pixels_per_inch,shrink_factor)
#define X_PAGE_OFFSET	ROUNDUP(pixels_per_inch,shrink_factor)
#define Y_PAGE_OFFSET	ROUNDUP(pixels_per_inch,shrink_factor)

#define pixel_round(x)      ((long) (conv * (double) (x) + 0.5))
#define dvi_round(x)        ((long) ((double) (x) / conv + 0.5))

#define one(fp)     num (fp, 1)
#define two(fp)     num (fp, 2)
#define stwo(fp)    snum(fp, 2)
#define four(fp)    num (fp, 4)
#define sfour(fp)   snum(fp, 4)

typedef unsigned char ubyte;

struct frame {
	long pxl_h, dvi_h, pxl_v, dvi_v, w, x, y, z;
};

struct frame *stack;
int stackp;

#define PXL_H   stack[stackp].pxl_h
#define PXL_V   stack[stackp].pxl_v
#define DVI_H   stack[stackp].dvi_h
#define DVI_V   stack[stackp].dvi_v
#define WW      stack[stackp].w
#define XX      stack[stackp].x
#define YY      stack[stackp].y
#define ZZ      stack[stackp].z

#define DBG_BITMAP	0x1
#define DBG_DVI		0x2
#define DBG_ALL		(DBG_BITMAP|DBG_DVI)

/*
 * Command line flags.
 */
int debug = 0;
int list_fonts = 0;

int pixels_per_inch = 300;
int shrink_factor = 4;

FILE *dvi_file;				/* user's file */

int font_not_found = 0;
struct font *current_font = NULL;	/* ptr into circular list of fonts */
#define MAX_OPEN_FONTS 12
int n_open_fonts = 0;			/* for LRU management of fonts */

/*
 * DVI preamble and postamble information.
 */
char job_id[300];
int total_pages, maxstack;
int current_page;
double fraction, conv;
long numerator, denominator, magnification;

/*
 * Offset in DVI file of last page, set in read_postamble().
 */
long last_page_offset;

/*
 * Table of page offsets in DVI file, indexed by page number - 1.
 * Initialized in prepare_pages().
 */
long *page_offset;

#define xdvi_width 15
#define xdvi_height 15
#define xdvi_x_hot 7
#define xdvi_y_hot 7
static short xdvi_bits[] = {
   0x0080, 0x01c0, 0x03e0, 0x06b0,
   0x0c98, 0x188c, 0x3086, 0x7fff,
   0x3086, 0x188c, 0x0c98, 0x06b0,
   0x03e0, 0x01c0, 0x0080};

Window win;
int forepix, backpix, highpix;

long screen_w, screen_h, page_w, page_h;
long min_x, max_x, min_y, max_y, base_x, base_y;
long smin_x, smax_x, smin_y, smax_y;
int redisplay = 0;

unsigned long num();
long snum();

extern char reverse_byte[];
char *malloc(), *calloc(), *index();

int GXfunc;
int backwards = 0;

main(argc, argv)
	int argc;
	char **argv;
{
	char *prog, *file;
	char *display = NULL;
	char *option;
	OpaqueFrame frame;
	int reverse = 0;
	int bwidth = 2;
	char *fore_color;
	char *back_color;
	char *high_color;
	char *brdr_color;
	char *mous_color;
	char *geometry = NULL, def[32];
	int backmap, bdrmap, mouspix;
	Color cdef;
	int indian = 1;
	if (*(char *) &indian) backwards = 0; else backwards = 1;

	prog = *argv++;
	argc--;
	if ((option = XGetDefault(prog, "ReverseVideo")) &&
	    strcmp(option, "on") == 0)
		reverse = 1;
	if (option = XGetDefault(prog, "BorderWidth"))
		bwidth = atoi(option);
	fore_color = XGetDefault(prog, "ForeGround");
	back_color = XGetDefault(prog, "BackGround");
	high_color = XGetDefault(prog, "Highlight");
	brdr_color = XGetDefault(prog, "Border");
	mous_color = XGetDefault(prog, "Mouse");
	file = NULL;
        while (argc) {
		if (strncmp(*argv, "-d", 2) == 0)
			debug = isdigit(argv[0][2]) ? atoi(&argv[0][2]) : DBG_ALL;
		else if (strcmp(*argv, "-l") == 0)
			list_fonts = 1;
		else if (strcmp(*argv, "-s") == 0 && argc > 1) {
			argv++;
			argc--;
			shrink_factor = atoi(*argv);
			if (shrink_factor <= 0) goto usage;
		} else if (strcmp(*argv, "-p") == 0 && argc > 1) {
			argv++;
			argc--;
			pixels_per_inch = atoi(*argv);
			if (pixels_per_inch <= 0) goto usage;
		} else if (strcmp(*argv, "-rv") == 0) {
			reverse = 1;
		} else if (strcmp(*argv, "-fg") == 0 && argc > 1) {
			argv++;
			argc--;
			fore_color = *argv;
		} else if (strcmp(*argv, "-bg") == 0 && argc > 1) {
			argv++;
			argc--;
			back_color = *argv;
		} else if (strcmp(*argv, "-hl") == 0 && argc > 1) {
			argv++;
			argc--;
			high_color = *argv;
		} else if (strcmp(*argv, "-bd") == 0 && argc > 1) {
			argv++;
			argc--;
			brdr_color = *argv;
		} else if (strcmp(*argv, "-ms") == 0 && argc > 1) {
			argv++;
			argc--;
			mous_color = *argv;
		} else if (**argv == '=') {
			geometry = *argv;
		} else if (**argv != '-') {
			if (index(*argv, ':') != NULL)
				display = *argv;
			else
				file = *argv;
		} else {
		    usage:
			fprintf(stderr, "Usage: xdvi [-s <shrink>] [-p <pixels>] [-l] [-rv] [-fg <color>] [-bg <color>] [-hl <color>] [-bd <color>] [-ms <color>] [=<geometry>] [host:display] dvi_file\n");
			exit(1);
		}
		argv++;
		argc--;
	}
        if (file == NULL)
		goto usage;
	if ((dvi_file = fopen(file, "r")) == NULL) {
		int n = strlen(file);
		char *dvi_name;

		if (strcmp(file + n - sizeof(".dvi") + 1, ".dvi") == 0) {
			perror(file);
			exit(1);
		}
		dvi_name = malloc((unsigned) n + sizeof(".dvi"));
		sprintf(dvi_name, "%s.dvi", file);
		if ((dvi_file = fopen(dvi_name, "r")) == NULL) {
			perror(dvi_name);
			exit(1);
		}
 	}
	process_preamble();
	find_postamble();
	read_postamble();
	prepare_pages();
	init_page();
	if (XOpenDisplay(display) == NULL) {
	    fprintf(stderr, "%s: Can't open display '%s'\n",
		    prog, XDisplayName(display));
	    exit(1);
	}
	if (reverse) {
		forepix = WhitePixel;
		highpix = WhitePixel;
		backpix = BlackPixel;
		backmap = BlackPixmap;
		bdrmap = WhitePixmap;
		mouspix = WhitePixel;
		GXfunc = GXor;
	} else {
		forepix = BlackPixel;
		highpix = BlackPixel;
		backpix = WhitePixel;
		backmap = WhitePixmap;
		bdrmap = BlackPixmap;
		mouspix = BlackPixel;
		GXfunc = GXand;
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
		if (high_color && XParseColor(high_color, &cdef) &&
			XGetHardwareColor(&cdef))
			highpix = cdef.pixel;
		if (brdr_color && XParseColor(brdr_color, &cdef) &&
			XGetHardwareColor(&cdef))
			bdrmap = XMakeTile(cdef.pixel);
		if (mous_color && XParseColor(mous_color, &cdef) &&
			XGetHardwareColor(&cdef))
			mouspix = cdef.pixel;
	}
	frame.bdrwidth = bwidth;
	frame.height = page_h;
	if (frame.height + (bwidth << 1) > DisplayHeight())
	    frame.height = DisplayHeight() - (bwidth << 1);
	frame.width = page_w;
	if (frame.width + (bwidth << 1) > DisplayWidth())
	    frame.width = DisplayWidth() - (bwidth << 1);
	frame.border = bdrmap;
	frame.background = backmap;
	frame.x = 0;
	frame.y = 0;
	sprintf(def, "=%dx%d+0+0", frame.width, frame.height);
	win = XCreate("DVI Previewer", prog, geometry, def, &frame, 50, 50);
	screen_h = frame.height;
	screen_w = frame.width;
	XMapWindow(win);
	XSelectInput(win, KeyPressed|ButtonPressed|ExposeWindow|ExposeRegion);
	XDefineCursor(win,
	    XCreateCursor(xdvi_width, xdvi_height, xdvi_bits, xdvi_bits,
			  xdvi_x_hot, xdvi_y_hot, mouspix, backpix, GXcopy));
	do_pages();
	stop_output(0);
}

/*
**      process_preamble reads the information in the preamble and stores
**      it into global variables for later use.
*/
process_preamble()
{
        ubyte   k;

        if (one(dvi_file) != PRE)
		error("xdvi: DVI file doesn't start with preamble");
	if (one(dvi_file) != 2)
		error("xdvi: Wrong version of DVI output for this program");
	numerator     = four(dvi_file);
	denominator   = four(dvi_file);
	magnification = four(dvi_file);
	fraction = (((double) numerator * magnification)
	                                 / ((double) denominator * 1000.));
	define_conv();
	k = one(dvi_file);
	fread(job_id, sizeof(char), k, dvi_file);
	job_id[k] = '\0';
}

define_conv ()
{
	conv = ((fraction * pixels_per_inch) / 100000) / (2.54 * shrink_factor);
}

/*
**      find_postamble locates the beginning of the postamble
**	and leaves the file ready to start reading at that location.
*/
find_postamble()
{
	ubyte byte;
	long offset = -4;        /* At least 4 TRAILERS */

	do {
		offset -= 1;
		fseek(dvi_file, offset, 2);
		byte = one(dvi_file);
	} while (byte == TRAILER);
	if (byte != 2)
		error("xdvi: Wrong version of DVI output for this program");
	offset -= 4;
	fseek(dvi_file, offset, 2);
	fseek(dvi_file, sfour(dvi_file), 0);
}

/*
**      read_postamble reads the information in the postamble,
**	storing it into global variables.
**      It also takes care of reading in all of the PXL files for the fonts
**      used in the job.
*/
read_postamble()
{
        ubyte   cmnd;
	int page_width, page_height;

        if (one(dvi_file) != POST)
	    error("xdvi: Postamble doesn't begin with POST");
	last_page_offset = four(dvi_file);
	if (numerator != four(dvi_file)
	          ||  denominator != four(dvi_file)
		  ||  magnification != four(dvi_file))
	    error("xdvi: Postamble doesn't match preamble");
	page_height = pixel_round(four(dvi_file));
	page_width = pixel_round(four(dvi_file));
	maxstack = two(dvi_file);
	total_pages = two(dvi_file);
	do {
	    switch(cmnd = one(dvi_file)) {
	        case FNTDEF1:
	        case FNTDEF2:
	        case FNTDEF3:
	        case FNTDEF4:
		    define_font(cmnd);
		    break;
		case POSTPOST:
		    break;
		default:
		    error("xdvi: Non-fntdef cmnd found in postamble");
	    }
	} while (cmnd != POSTPOST);
	if (font_not_found)
		error("xdvi: Not all PXL files were found");
	list_fonts = 0;
}

prepare_pages()
{
	int i;

        stack = (struct frame *) malloc((unsigned) sizeof(struct frame) * (maxstack+1));
        if (stack == NULL)
		error("xdvi: Can't allocate stack space (%d frames)", maxstack);
	page_offset = (long *) malloc((unsigned) total_pages * sizeof(long));
        if (page_offset == NULL)
		error("xdvi: Can't allocate page directory (%d pages)", total_pages);
	i = total_pages;
	page_offset[--i] = last_page_offset;
	fseek(dvi_file, last_page_offset, 0);
	/*
	 * Follow back pointers through pages in the DVI file,
	 * storing the offsets in the page_offset table.
	 */
	while (i > 0) {
		num(dvi_file, 1+4+(9*4));
		fseek(dvi_file, page_offset[--i] = four(dvi_file), 0);
	}
}

/*
**      define_font reads the rest of the fntdef command and then reads in
**      the specified PXL file, adding it to the global linked-list holding
**      all of the fonts used in the job.
*/
define_font(cmnd)
	ubyte cmnd;
{
        register struct font *fontp;
	int len;
	int unmodsize;
	float realsize;
	int size;
        long checksum;

	fontp = (struct font *) malloc((unsigned) sizeof(struct font));
	if (fontp == NULL)
		error("xdvi: Can't allocate memory for font");
	fontp->TeXnumber = num(dvi_file, cmnd - FNTDEF1 + 1);
	checksum = four(dvi_file);
	fontp->scale = four(dvi_file);
	fontp->design = four(dvi_file);
	len = one(dvi_file) + one(dvi_file);
	fontp->fontname = malloc(len + 10);	/* leave space for magnification */
	fread(fontp->fontname, sizeof(char), len, dvi_file);
	fontp->fontname[len] = '\0';
	fontp->file = NULL;
/*
**	In the actual implementation, scaled-size/design-size hasn't been
**	stored with sufficient precision, hence the messing around to find
**	its actual value.
*/
	realsize = (magnification/1000.)*((float) fontp->scale / fontp->design);
	unmodsize = (realsize * 1000) + 0.5;
	/* a real hack to correct for rounding in some cases */
	switch (unmodsize) {
	    case 1095:
		realsize = 1.095445;	/* stephalf */
		break;
	    case 1315:
		realsize = 1.314534;	/* stepihalf */
		break;
	    case 2074:
		realsize = 2.0736;	/* stepiv */
		break;
	    case 2488:
		realsize = 2.48832;	/* stepv */
		break;
	    case 2986:
		realsize = 2.985984;	/* stepiv */
		break;
	}
	/*
	 * the remaining magnification steps are represented
	 * with sufficient accuracy already
	 */
	size = (realsize * pixels_per_inch * 5) + 0.5;
	sprintf(&fontp->fontname[len], FONT_SUFFIX, size);
	if (!open_pxl_file(fontp))
		return;
	read_glyphs(fontp);
	if (current_font == NULL) {
		fontp->next = fontp;
		fontp->prev = fontp;
	} else {
		fontp->next = current_font;
		fontp->prev = current_font->prev;
		current_font->prev->next = fontp;
		current_font->prev = fontp;
	}
	current_font = fontp;
}

open_pxl_file(font)
	struct font *font;
{
	char filename[300];
	extern int errno;

	if (font->file == NULL) {
		sprintf(filename, "%s/%s",
				FONT_DIRECTORY, font->fontname);
		if (n_open_fonts == MAX_OPEN_FONTS)
			close_lru();
		font->file = fopen(filename, "r");
		if (font->file == NULL) {
			font_not_found = 1;
			printf("%s [not found]\n", font->fontname);
			return (0);
		}
		n_open_fonts += 1;
	}
	if (list_fonts)
		printf("%s\n", font->fontname);
	return (1);
}

read_pxl_bitmap(ch, g)
	ubyte ch;
	register struct glyph *g;
{
	register struct bitmap *bitmap;
	register int file_bytes_wide;
	register char *ptr;
	register int i, j;

	bitmap = &g->bitmap;

	/* in file, bitmap rows are multiples of 32 bits wide */
	file_bytes_wide = ROUNDUP(bitmap->w, BITS_PER_LONG)*BYTES_PER_LONG;
	/* width must be multiple of 16 bits for raster_op */
	bitmap->bytes_wide = ROUNDUP(bitmap->w, BITS_PER_SHORT)*BYTES_PER_SHORT;
	ptr = bitmap->bits = malloc((unsigned) bitmap->h * bitmap->bytes_wide);
	if (ptr == NULL)
		error("xdvi: Can't allocate bitmap for character %d of font %s (%d by %d)",
			ch, current_font->fontname, bitmap->h, bitmap->w);
	if (!open_pxl_file(current_font))
		error("xdvi: Can't find font file %s", current_font->fontname);
	fseek(current_font->file, g->addr, 0);
	for (i = 0; i < bitmap->h; i += 1)
		for (j = 0; j < file_bytes_wide; j += 1)
			if (j < bitmap->bytes_wide)
				*ptr++ = reverse_byte[one(current_font->file)];
			else
				one(current_font->file);
	if (shrink_factor != 1)
		shrink_bitmap(bitmap, shrink_factor, shrink_factor);
	if (debug & DBG_BITMAP)
		print_char(ch, g);
}

/*
 * Find font #n and move it to the head of the list.
 */
change_font(n)
	unsigned long n;
{
        register struct font *fontp;

	fontp = current_font;
	for (;;) {
		if (fontp->TeXnumber == n)
                        break;
		fontp = fontp->next;
		if (fontp == current_font)
			error("xdvi: Non-existent font #%d", n);
	}
	if (current_font == fontp)
		return;
	fontp->prev->next = fontp->next;
	fontp->next->prev = fontp->prev;
	fontp->next = current_font;
	fontp->prev = current_font->prev;
	current_font->prev->next = fontp;
	current_font->prev = fontp;
	current_font = fontp;
}

/*
 * Close the PXL file for the least recently used font.
 */
close_lru()
{
        register struct font *f;

	f = current_font->prev;
	for (;;) {
		if (f->file != NULL)
                        break;
		f = f->prev;
		if (f == current_font->prev)
			error("xdvi: Can't find an open PXL file to close");
	}
	fclose(f->file);
	f->file = NULL;
	n_open_fonts -= 1;
}

reset_fonts()
{
        register struct font *f;
	register struct glyph *g;

	f = current_font;
	for (;;) {
	    open_pxl_file(f);
	    for (g = &f->glyph[0]; g < &f->glyph[MAXCHARS]; g += 1) {
		if (g->bitmap.bits) free(g->bitmap.bits);
	    }
	    read_glyphs(f);
	    f = f->next;
	    if (f == current_font) break;
	}
}

read_glyphs (fontp)
        register struct font *fontp;
{
	register struct glyph *g;
        long checksum, magnify, design_size, font_dir_ptr, pxl_id_word;

	/* seek to trailer info */
	fseek(fontp->file, (long) -(5 * BYTES_PER_LONG), 2);
        checksum = four(fontp->file);
        magnify = four(fontp->file);
        design_size = four(fontp->file);
        font_dir_ptr = sfour(fontp->file) * 4;
        pxl_id_word = four(fontp->file);
#ifdef lint
	magnify = design_size = pxl_id_word = magnify;
#endif
	/* seek to font directory */
	fseek(fontp->file, font_dir_ptr, 0);
	for (g = &fontp->glyph[0]; g < &fontp->glyph[MAXCHARS]; g += 1) {
		g->bitmap.bits = NULL;
		g->bitmap.w = two(fontp->file);	/* leave this for shrink_bitmap */
		g->bitmap.h = two(fontp->file);	/* leave this for shrink_bitmap */
		g->x = stwo(fontp->file) / shrink_factor;
		g->y = stwo(fontp->file) / shrink_factor;
		g->addr = four(fontp->file) * 4;
		/*
		**  The TFM-width word is kind of funny in the units
		**  it is expressed in.  It works this way:
		**
		**  If a glyph has width 'w' in a font with design-size
		**  'd' (both in same units), the TFM-width word is
		**
		**                    (w/d) * 2^20
		**
		**  Therefore, in order to find the glyph width in
		**  DVI units (1 / 2^16 points), we take the design-size
		**  'd' (in DVI's), the magnification 'm' of the PXL file
		**  and the TFM-width word 't' to the width (in DVI's)
		**  as follows:
		**
		**                     dmt
		**                w = -----
		**                    2^20
		**
		**  But the magnification of the PXL file is just the
		**  scaled size 's' over the design size, so the final
		**  expression for the width is
		**
		**                     st
		**                w = ----
		**                    2^20
		**
		*/      
		g->dvi_adv =
			((double) fontp->scale * four(fontp->file)) / (1 << 20);
		g->pxl_adv = pixel_round(g->dvi_adv);
	}
}

#define nope(str)       error("xdvi: %s not implemented", str)
#define correct()       (PXL_H = pixel_round(DVI_H))

do_pages()
{
        ubyte ch;

	min_x = 0;
	min_y = 0;
	max_x = screen_w;
	max_y = screen_h;
	base_x = min_x;
	base_y = min_y;
	current_page = 0;
	for (;;) {
		ch = one(dvi_file);
		if (debug & DBG_DVI)
			print_dvi(ch);
		if (ch <= SETCHAR0 + 127) {
			set_char(ch);
			DVI_H += current_font->glyph[ch].dvi_adv;
			PXL_H += current_font->glyph[ch].pxl_adv;
			correct();
		} else if (FNTNUM0 <= ch  &&  ch <= FNTNUM0 + 63) {
			change_font((unsigned long) (ch - FNTNUM0));
		} else {
			long a, b;

			switch (ch) {
			    case SET1:
				nope("SET1");
				break;

			    case SETRULE:
				a = sfour(dvi_file); b = sfour(dvi_file);
				if (a > 0  &&  b > 0) {
				    correct();
				    set_rule(pixel_round(a), pixel_round(b));
				}
				DVI_H += b;
				PXL_H =  pixel_round(DVI_H);
				break;

			    case PUT1:
				nope("PUT1");
				break;

			    case PUTRULE:
				a = sfour(dvi_file); b = sfour(dvi_file);
				if (a > 0  &&  b > 0) {
				    correct();
				    set_rule(pixel_round(a), pixel_round(b));
				}
				break;

			    case NOP:
				break;

			    case BOP:
				num(dvi_file, 11*4);
				stackp = 0;
				DVI_H = dvi_round(X_PAGE_OFFSET);
				PXL_H = X_PAGE_OFFSET;
				DVI_V = dvi_round(Y_PAGE_OFFSET);
				PXL_V = Y_PAGE_OFFSET;
				WW = XX = YY = ZZ = 0;
				begin_page();
				break;

			    case EOP:
				if (stackp > 0)
				    error("Stack not empty at EOP (%d)",
				    	   stackp);
				end_page();
				if (ftell(dvi_file) > last_page_offset)
				    return;
				break;

			    case PUSH:
				stackp++;
				if (stackp > maxstack)
				    error("xdvi: More PUSHes than were promised");
				stack[stackp] = stack[stackp - 1];
				break;

			    case POP:
				stackp--;
				if (stackp < 0)
				    error("xdvi: More POPs than PUSHes");
				break;

			    case RIGHT1:
			    case RIGHT2:
			    case RIGHT3:
			    case RIGHT4:
				DVI_H += snum(dvi_file, ch - RIGHT1 + 1);
				PXL_H = pixel_round(DVI_H);
				break;

			    case X0:
			    case X1:
			    case X2:
			    case X3:
			    case X4:
				if (ch != X0)
				    XX = snum(dvi_file, ch - X0);
				DVI_H += XX;
				PXL_H += pixel_round(XX);
				correct();
				break;

			    case W0:
			    case W1:
			    case W2:
			    case W3:
			    case W4:
				if (ch != W0)
				    WW = snum(dvi_file, ch - W0);
				DVI_H += WW;
				PXL_H = pixel_round(DVI_H);
				break;

			    case Y0:
			    case Y1:
			    case Y2:
			    case Y3:
			    case Y4:
				if (ch != Y0)
				    YY = snum(dvi_file, ch - Y0);
				DVI_V += YY;
				PXL_V = pixel_round(DVI_V);
				break;

			    case Z0:
			    case Z1:
			    case Z2:
			    case Z3:
			    case Z4:
				if (ch != Z0)
				    ZZ = snum(dvi_file, ch - Z0);
				DVI_V += ZZ;
				PXL_V = pixel_round(DVI_V);
				break;

			    case DOWN1:
			    case DOWN2:
			    case DOWN3:
			    case DOWN4:
				DVI_V += snum(dvi_file, ch - DOWN1 + 1);
				PXL_V = pixel_round(DVI_V);
				break;

			    case FNT1:
			    case FNT2:
			    case FNT3:
			    case FNT4:
				change_font(num(dvi_file, ch - FNT1 + 1));
				break;

			    case XXX1:
			    case XXX2:
			    case XXX3:
			    case XXX4:
				a = num(dvi_file, ch - XXX1 + 1);
				if(a > 0)
				    special((unsigned long) a);
				break;

			    case FNTDEF1:
			    case FNTDEF2:
			    case FNTDEF3:
			    case FNTDEF4:
				fseek(dvi_file, (long) (12 + ch - FNTDEF1 + 1), 1);
				a = one(dvi_file) + one(dvi_file);
				fseek(dvi_file, (long) a, 1);
				break;

			    case PRE:
				error("xdvi: Shouldn't happen: PRE encountered.");
				break;

			    case POST:
				error("xdvi: Shouldn't happen: POST encountered.");
				break;

			    case POSTPOST:
				error("xdvi: Shouldn't happen: POSTPOST encountered.");
				break;

			    default:
				error("xdvi: Unknown op-code %d, offset %d",
					ch, ftell(dvi_file));
			} /* end switch*/
		} /* end else (ch not a SETCHAR or FNTNUM) */
	} /* end for */
}

set_char(ch)
	ubyte ch;
{
	register struct glyph *g;

	g = &current_font->glyph[ch];
	if (g->bitmap.bits == NULL) {
		read_pxl_bitmap(ch, g);
		if (backwards) reverse_bytes(&g->bitmap);
	      }
	
	put_bitmap(&g->bitmap, (PXL_H - g->x), (PXL_V - g->y), forepix);
}

reverse_bytes(bitmap)
 	register struct bitmap *bitmap;
{
 	register long x, y;
 	register char *bp ;
 	register char c ;
 
 	bp = bitmap->bits ;
 	for ( y = 0 ; y < bitmap->h ; y++) {
 		for ( x = 0 ; x < bitmap->bytes_wide ; x += 2) {
 			c = *bp ;
 			*bp++ = *(bp + 1) ;
 			*bp++ = c ;
 		}
 	}
}
 
set_rule(h, w)
	long h, w;
{
	/* (w,h) specifies lower left corner of rule box */
	put_rectangle(PXL_H, PXL_V - h, w, h, forepix);
}

begin_page()
{
	if (debug)
		return;
	if (!redisplay)
	    clear_page();
	put_border(0, 0, page_w, page_h, 1);
}

end_page()
{
	int ch, arg, sign, number, next_page;
	XEvent event;
	char *string;
	int nbytes;

#ifdef lint
	number = 0;
#endif
	if (debug) {
		if (++current_page == total_pages)
			exit(0);
		return;
	}
	if (redisplay) {
	    min_x = smin_x;
	    max_x = smax_x;
	    min_y = smin_y;
	    max_y = smax_y;
	    redisplay = 0;
	}
	arg = 0;
	for (;;) {
		XNextEvent (&event);
		switch (event.type) {
		case ExposeWindow:
		    screen_h = ((XExposeEvent *)(&event))->height;
		    screen_w = ((XExposeEvent *)(&event))->width;
		    max_x = min_x + screen_w;
		    max_y = min_y + screen_h;
		    string = "\f";
		    nbytes = 1;
		    break;
		case ExposeRegion:
		    smin_x = min_x;
		    smax_x = max_x;
		    smin_y = min_y;
		    smax_y = max_y;
		    min_x = min_x + ((XExposeEvent *)(&event))->x;
		    min_y = min_y + ((XExposeEvent *)(&event))->y;
		    max_x = min_x + ((XExposeEvent *)(&event))->width;
		    max_y = min_y + ((XExposeEvent *)(&event))->height;
		    redisplay = 1;
		    string = "\f";
		    nbytes = 1;
		    break;
		case ButtonPressed:
		    {
		    short detail = ((XButtonPressedEvent *) (&event))->detail;
		    switch (detail & ValueMask) {
		    case LeftButton:
			if (detail & ShiftMask)
			    string = "l";
			else
			    string = "b";
			nbytes = 1;
			break;
		    case MiddleButton:
			if (detail & ShiftMask)
			    string = "u";
			else
			    string = "d";
			nbytes = 1;
			break;
		    case RightButton:
			if (detail & ShiftMask)
			    string = "r";
			else
			    string = "f";
			nbytes = 1;
			break;
		    }
		    }
		    break;
		case KeyPressed:
		    string = XLookupMapping (&event, &nbytes);
		    break;
		}
		if (nbytes == 0) continue;
		if (nbytes > 1) goto bad;
		switch (ch = *string) {
		    case 'q':
		    case '\003':	/* control-C */
		    case '\004':	/* control-D */
			stop_output(0);
			break;
		    case 'n':
		    case 'f':
		    case ' ':
			/* scroll forward */
			min_x = 0;
			min_y = 0;
			max_x = screen_w;
			max_y = screen_h;
			next_page = current_page + 1;
			break;
		    case 'p':
		    case 'b':
		    case '\b':
			/* scroll backward */
			min_x = 0;
			min_y = 0;
			max_x = screen_w;
			max_y = screen_h;
			next_page = current_page - 1;
			break;
		    case 'u':
			if (min_y == 0) goto bad;
			min_y -= screen_h;
			if (min_y < 0)
			    min_y = 0;
			base_y = min_y;
			max_y = min_y + screen_h;
			next_page = current_page;
			break;
		    case 'd':
			if (min_y >= page_h - screen_h) goto bad;
			min_y += screen_h;
			if (min_y > page_h - screen_h)
			    min_y = page_h - screen_h;
			if (min_y < 0)
			    min_y = 0;
			base_y = min_y;
			max_y = min_y + screen_h;
			next_page = current_page;
			break;
		    case 'l':
			if (min_x == 0) goto bad;
			min_x -= screen_w;
			if (min_x < 0)
			    min_x = 0;
			base_x = min_x;
			max_x = min_x + screen_w;
			next_page = current_page;
			break;
		    case 'r':
			if (min_x >= page_w - screen_w) goto bad;
			min_x += screen_w;
			if (min_x > page_w - screen_w)
			    min_x = page_w - screen_w;
			if (min_x < 0)
			    min_x = 0;
			base_x = min_x;
			max_x = min_x + screen_w;
			next_page = current_page;
			break;
		    case 's':
			if (!arg) {
			    int shrink = shrink_factor;
			    long fac1, fac2;
			    shrink_factor = 1;
			    fac1 = ROUNDUP(PAPER_WIDTH, screen_w);
			    fac2 = ROUNDUP(PAPER_HEIGHT, screen_h);
			    if (fac1 < fac2)
				number = fac2;
			    else
				number = fac1;
			    shrink_factor = shrink;
			}
			if (number <= 0) goto bad;
			if (number != shrink_factor) {
			    shrink_factor = number;
			    min_x = 0;
			    min_y = 0;
			    max_x = screen_w;
			    max_y = screen_h;
			    init_page();
			    define_conv();
			    reset_fonts();
			}
		    case '\f':
			/* redisplay current page */
			next_page = current_page;
			break;
		    case '\r':
		    case '\n':
			/* go to relative page */
			min_x = 0;
			min_y = 0;
			max_x = screen_w;
			max_y = screen_h;
			next_page = current_page + (arg ? number : 1);
			break;
		    case 'g':
			/* go to absolute page */
			min_x = 0;
			min_y = 0;
			max_x = screen_w;
			max_y = screen_h;
			next_page = (arg ? number : total_pages) - 1;
			break;
		    case '0': case '1': case '2': case '3': case '4':
		    case '5': case '6': case '7': case '8': case '9':
			if (! arg) {
				arg = 1;
				sign = 1;
				number = 0;
			}
			number = 10*number + sign*(ch - '0');
			continue;
		    case '-':
			if (! arg) {
				arg = 1;
				sign = -1;
				number = 0;
				continue;
			} else
				goto bad;
		    default:
			goto bad;
		}
		if (0 <= next_page && next_page < total_pages) {
			current_page = next_page;
			fseek(dvi_file, page_offset[current_page], 0);
			break;
		}
	bad:
		XFeep(0);
		arg = 0;		/* throw away numeric argument */
		continue;
	}
}

special(nbytes)
	unsigned long nbytes;
{
	char *cmd;
	int i;

	cmd = malloc((unsigned) nbytes+1);
	if (cmd == NULL)
		error("xdvi: Can't allocate memory for special (%d bytes)", nbytes);
	for (i = 0; i < nbytes; i += 1)
		cmd[i] = getc(dvi_file);
	cmd[i] = '\0';
	fprintf(stderr, "special ``%s'' not implemented\n", cmd);
	free(cmd);
}

/*
**
**      Read size bytes from the FILE fp, constructing them into a
**      signed/unsigned integer.
**
*/
unsigned long
num(fp, size)
	register FILE *fp;
	register int size;
{
        register int i;
	register long x;

	x = 0;
	for (i = 0; i < size; i += 1)
		x = x * 0x100 + (unsigned) getc(fp);
	return (x);
}

long
snum(fp, size)
	register FILE *fp;
	register int size;
{
        register int i;
	register long x;

	x = getc(fp) & 0xff;
	if (x & 0x80)
        	x -= 0x100;
	for (i = 1; i < size; i += 1)
	    x = x * 0x100 + (unsigned) getc(fp);
	return (x);
}

stop_output(sig)
{
	exit(sig);
}

/* VARARGS1 */
error(message, a, b, c, d, e, f)
	char *message;
{
	fprintf(stderr, message, a, b, c, d, e, f);
	putc('\n', stderr);
	exit(1);
}

init_page()
{
	page_h = PAPER_HEIGHT;
	page_w = PAPER_WIDTH;
}

clear_page()
{
	XClear(win);
}

put_border(x, y, w, h, t)
	long x, y, w, h, t;
{
	put_rectangle(x, y, w, t, highpix);
	put_rectangle(x, y, t, h, highpix);
	put_rectangle(x, y + h - t, w, t, highpix);
	put_rectangle(x + w - t, y, t, h, highpix);
}

put_rectangle(x, y, w, h, pix)
	long x, y, w, h;
	int pix;
{
	if (x < max_x && x + w >= min_x && y < max_y && y + h >= min_y)
		XPixSet(win, x - base_x, y - base_y, w, h, pix);
}

put_bitmap(bitmap, x, y, pix)
	register struct bitmap *bitmap;
	register long x, y;
	int pix;
{
	if ((bitmap->w == 0) || (bitmap->h == 0)) return;
	if (x < max_x && x + bitmap->w >= min_x &&
	    y < max_y && y + bitmap->h >= min_y)
		XBitmapBitsPut(win, x - base_x, y - base_y,
				bitmap->w, bitmap->h, (char *) bitmap->bits,
				pix, backpix, NULL, GXfunc, AllPlanes);
}

sample(bitmap, x, y, w, h)
	register struct bitmap *bitmap;
	int x, y, w, h;
{
	register char *ptr, *endp;
	register int b, i, j, m, n;

	ptr = bitmap->bits
		+ (y * bitmap->bytes_wide)
		+ (x / BITS_PER_BYTE);
	endp = bitmap->bits + (bitmap->h * bitmap->bytes_wide);
	b = (1 << (x % BITS_PER_BYTE));
	n = 0;
	for (i = 0; i < h && ptr < endp; i += 1, ptr += bitmap->bytes_wide)
		for (m = b, j = 0; j < w; j += 1, m <<= 1)
			if (*ptr & m)
				n += 1;
	return (n >= (i * w) / 3);
}

shrink_bitmap(bitmap, x_factor, y_factor)
	register struct bitmap *bitmap;
	int x_factor, y_factor;
{
	char *shrunk_bits;
	int shrunk_height, shrunk_width, shrunk_bytes_wide;
	register char *ptr;
	char *cp;
	register int x, y, b, m;

	shrunk_height = ROUNDUP(bitmap->h, y_factor);
	shrunk_width = ROUNDUP(bitmap->w, x_factor);
	shrunk_bytes_wide = ROUNDUP(shrunk_width, BITS_PER_SHORT)*BYTES_PER_SHORT;
		/* width must be multiple of 16 bits for raster_op */
	ptr = shrunk_bits = calloc((unsigned) shrunk_height * shrunk_bytes_wide, 1);
	if (ptr == NULL)
		error("Can't allocate shrunken bitmap (%d by %d)",
			shrunk_height, shrunk_width);
	for (y = 0; y < bitmap->h; y += y_factor) {
		b = 0;
		m = (1 << 0);
		cp = ptr;
		for (x = 0; x < bitmap->w; x += x_factor) {
			if (sample(bitmap, x, y, x_factor, y_factor))
				*ptr |= m;
			else
				*ptr &= ~m;
			b += 1;
			m <<= 1;
			if (b % BITS_PER_BYTE == 0) {
				b = 0;
				m = (1 << 0);
				ptr += 1;
			}
		}
		ptr = cp + shrunk_bytes_wide;
	}
	free(bitmap->bits);
	bitmap->bits = shrunk_bits;
	bitmap->h = shrunk_height;
	bitmap->w = shrunk_width;
	bitmap->bytes_wide = shrunk_bytes_wide;
}

print_char(ch, g)
	ubyte ch;
	struct glyph *g;
{
	printf("char %d", ch);
	if (isprint(ch))
		printf(" (%c)", ch);
	putchar('\n');
	printf("x = %d, y = %d, pxl = %d, dvi = %d\n",
		g->x, g->y, g->pxl_adv, g->dvi_adv);
	print_bitmap(&g->bitmap);
}

print_bitmap(bitmap)
	register struct bitmap *bitmap;
{
	register char *ptr;
	register int x, y, i;

	ptr = bitmap->bits;
	if (ptr == NULL)
		return;
	printf("w = %d, h = %d, bytes wide = %d\n",
		bitmap->w, bitmap->h, bitmap->bytes_wide);
	for (y = 0; y < bitmap->h; y += 1) {
		for (x = 0; x < bitmap->bytes_wide; x += 1) {
			for (i = 0; i < BITS_PER_BYTE; i += 1)
				if (*ptr & (1 << i))
					putchar('@');
				else
					putchar(' ');
			ptr += 1;
		}
		putchar('\n');
	}
}

print_dvi(ch)
	ubyte ch;
{
	printf("%4d %4d ", PXL_H, PXL_V);
	if (ch <= SETCHAR0 + 127) {
		printf("SETCHAR%-3d", ch - SETCHAR0);
		if (isprint(ch))
			printf(" (%c)", ch);
	} else if (FNTNUM0 <= ch  &&  ch <= FNTNUM0 + 63) {
		printf("FNTNUM%d", ch - FNTNUM0);
	} else {
		switch (ch) {
		    case SET1:
			printf("SET1");
			break;
		    case SETRULE:
			printf("SETRULE");
			break;
		    case PUT1:
			printf("PUT1");
			break;
		    case PUTRULE:
			printf("PUTRULE");
			break;
		    case NOP:
			printf("NOP");
			break;
		    case BOP:
			printf("BOP");
			break;
		    case EOP:
			printf("EOP");
			break;
		    case PUSH:
			printf("PUSH");
			break;
		    case POP:
			printf("POP");
			break;
		    case RIGHT1:
		    case RIGHT2:
		    case RIGHT3:
		    case RIGHT4:
			printf("RIGHT%d", ch - RIGHT1 + 1);
			break;
		    case X0:
		    case X1:
		    case X2:
		    case X3:
		    case X4:
			printf("X%d", ch - X0);
			break;
		    case W0:
		    case W1:
		    case W2:
		    case W3:
		    case W4:
			printf("W%d", ch - W0);
			break;
		    case Y0:
		    case Y1:
		    case Y2:
		    case Y3:
		    case Y4:
			printf("Y%d", ch - Y0);
			break;
		    case Z0:
		    case Z1:
		    case Z2:
		    case Z3:
		    case Z4:
			printf("Z%d", ch - Z0);
			break;
		    case DOWN1:
		    case DOWN2:
		    case DOWN3:
		    case DOWN4:
			printf("DOWN%d", ch - DOWN1 + 1);
			break;
		    case FNT1:
		    case FNT2:
		    case FNT3:
		    case FNT4:
			printf("FNT%d", ch - FNT1 + 1);
			break;
		    case XXX1:
		    case XXX2:
		    case XXX3:
		    case XXX4:
			printf("XXX%d", ch - XXX1 + 1);
			break;
		    case FNTDEF1:
		    case FNTDEF2:
		    case FNTDEF3:
		    case FNTDEF4:
			printf("FNTDEF%d", ch - FNTDEF1 + 1);
			break;
		    case PRE:
			printf("PRE");
			break;
		    case POST:
			printf("POST");
			break;
		    case POSTPOST:
			printf("POSTPOST");
			break;
		    default:
			error("xdvi: Unknown op-code %d, offset %d",
				ch, ftell(dvi_file));
		} /* end switch*/
	} /* end else (ch not a SETCHAR or FNTNUM) */
	putchar('\n');
}
