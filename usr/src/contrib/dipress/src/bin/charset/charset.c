/*
 *
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 * HISTORY
 * 02-Sep-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Changed exit status to be 0 or 1.
 *
 * 07-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Converted for use with getopt.
 *
 * 25-Mar-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Fixed the "usage" message to be more helpful.
 *
 * 01-Dec-85  lee at Xerox, WRC
 *	Modified to pass lint.
 *
 *	28-apr-85  ed flint	add conditional code for vax11-c (vms)
 *				now use -s for point size and added -o for
 *				output file
 */

#ifdef vax11c
#include stdio
#include ctype
#else
# include <stdio.h>
# include <ctype.h>
# include <sys/file.h>
# include <strings.h>
#endif

# include "iptokens.h"
# include "literal.h"
# include "operator.h"

/* frame buffer numbers */
# define    F_backgroundfont	1
# define    F_font		2
# define    F_transform		3

# define    INCH		720
# define    Top_margin		(10 * INCH)
# define    Left_margin		(INCH * 2)
# define    Left_table		(Left_margin + INCH/2)

main(argc, argv)

int  argc;
char *argv[];

{
    char    u_fontname[256],
    	    buffer[256],
    	    *outputname= 0;
    int c;
    int ipressfile;
    int point_size = 10;
    int char_spacing;
    int line_spacing;
    int x_pos;
    int y_pos;
    int i;
    int num;
    int prefix = 0;
    extern int optind;
    extern char *optarg;

    while ((c = getopt(argc, argv, "o:O:p:P:s:S:")) != EOF)
	switch (c) {
	    case 'o':
	    case 'O':
		    outputname = optarg;
		    break;
	    case 'p':
	    case 'P':
		    (void) sscanf(optarg, "%o", &prefix);
		    break;
	    case ('s'):
	    case ('S'):
		    point_size = atoi(optarg);
		    break;
	    }

    if (argc - optind != 1) {
	fprintf(stderr, "charset: [-pcharSet] [-ooutput] [-sptSize] fontName\n");
	exit(1);
    }

    (void) strcpy(u_fontname, "Xerox/XC1-1-1/");
    (void) strcat(u_fontname, argv[optind]);

    /* set spacing parameters */
    line_spacing = (point_size + 2) * 15;
    char_spacing = point_size * 30;

    if ( outputname != 0 )
    {
#ifdef vax11c
	if ( (ipressfile= creat(outputname, 0, "rfm=udf")) == -1 )
#else
	if ( (ipressfile= open(outputname, O_WRONLY | O_CREAT, 0666)) == -1 )
#endif
	{
	    perror(outputname);
	    exit(1);
	}
    }
    else		/* default to stdout */
    {
	ipressfile= 1;
    }

    ip_select(ipressfile);

    AppendOp(OP_beginBlock);
    AppendOp(OP_beginBody);

    /* set up the two fonts: the background and the font in question */
    SetupFont("Xerox/XC1-1-1/Classic", 100., F_backgroundfont);
    SetupFont(u_fontname, point_size * 10., F_font);

    /* set frame[2] to a scaling transform that uses 1/10 point */
    /* co-ordiate system */
    AppendRational(353L, 10000000);
    AppendOp(OP_scale);
    AppendInteger((long) F_transform);
    AppendOp(OP_fset);

    AppendOp(OP_endBody);	/* end preamble */

    AppendOp(OP_beginBody);	/* page 1 (and only) */

    Fget(F_transform);
    AppendOp(OP_concatt);
    Setfont(F_backgroundfont);
    Setxy((double)Left_margin, (double)Top_margin);
    (void) sprintf(buffer, "Xerox/XC1-1-1/%s at %d point, character set %o (octal)",
	argv[optind], point_size, prefix);
    ShowString(buffer);

    buffer[0] = '0';
    buffer[1] = '\0';
    for (x_pos = Left_table, i = 0; i < 8;
	 x_pos += char_spacing, i++)
    {
	Setxy((double)x_pos, (double)(Top_margin - 2 * line_spacing));
	ShowString(buffer);
	buffer[0] += 1;
    }

    for (y_pos = Top_margin - 3 * line_spacing, num = 000; num < 0377;
	 y_pos -= line_spacing)
    {
	Setxy((double)Left_margin, (double)y_pos);
	(void) sprintf(buffer, "%03o", num);
	ShowString(buffer);
	Setfont(F_font);

	for (x_pos = Left_table, i = 0; i < 8;
	     x_pos += char_spacing, i++, num++)
	{
	    Setxy((double)x_pos, (double)y_pos);
	    if (num != 0377)
	    {
		if (prefix > 0)
		{
		    (void) sprintf(buffer, "\377%c%c", prefix, num);
		    AppendString(buffer);		
		}
		else
		{
		    buffer[0] = num;
		    buffer[1] = '\0';
		    AppendString(buffer);
		}
		AppendOp(OP_show);
	    }
	}
	Setfont(F_backgroundfont);
    }

    /* wrap it up */
    AppendOp(OP_endBody);
    AppendOp(OP_endBlock);

    ip_flush();
    exit(0);
}
