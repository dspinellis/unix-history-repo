/*
 * xtotroff
 *
 * convert X font metrics into troff font metrics
 */

# include	<X11/Xlib.h>
# include	<stdio.h>
# include	<ctype.h>
# include	"libXdvi/XFontName.h"
# include	"libXdvi/DviChar.h"

# define charWidth(fi,c)	((fi)->per_char[(c) - (fi)->min_char_or_byte2].width)
# define charHeight(fi,c)	((fi)->per_char[(c) - (fi)->min_char_or_byte2].ascent)
# define charDepth(fi,c)	((fi)->per_char[(c) - (fi)->min_char_or_byte2].descent)
# define charLBearing(fi,c)	((fi)->per_char[(c) - (fi)->min_char_or_byte2].lbearing)
# define charRBearing(fi,c)	((fi)->per_char[(c) - (fi)->min_char_or_byte2].rbearing)

Display		*dpy;
int		groff_flag = 0;

int charExists (fi, c)
	XFontStruct	*fi;
	int		c;
{
	XCharStruct *p;

	if (c < fi->min_char_or_byte2 || c > fi->max_char_or_byte2)
		return 0;
	p = fi->per_char + (c - fi->min_char_or_byte2);
	return (p->lbearing != 0 || p->rbearing != 0 || p->width != 0
		|| p->ascent != 0 || p->descent != 0 || p->attributes != 0);
}

MapFont (font_name, troff_name)
	char	*font_name;
	char	*troff_name;
{
	XFontStruct	*fi;
	int		count;
	char		**names;
	FILE		*out;
	int		c;
	unsigned int	attributes;
	XFontName	parsed;
	int		j, k;
	DviCharNameMap	*char_map;
	char		encoding[256];
	char		*s;
	int		wid;

	names = XListFonts (dpy, font_name, 100000, &count);
	if (count < 1) {
		fprintf (stderr, "bad font name: %s\n", font_name);
		return 0;
	}
	if (count > 1) {
		fprintf (stderr, "ambiguous font name: %s\n", font_name);
		return 0;
	}
	if (!XParseFontName (names[0], &parsed, &attributes)) {
		fprintf (stderr, "not a standard name: %s\n", names[0]);
		return 0;
	}
	sprintf (encoding, "%s-%s", parsed.CharSetRegistry,
				    parsed.CharSetEncoding);
	for (s = encoding; *s; s++)
		if (isupper (*s))
			*s = tolower (*s);
	char_map = DviFindMap (encoding);
	if (!char_map) {
		fprintf (stderr, "not a standard encoding: %s\n", encoding);
		return 0;
	}

	out = fopen (troff_name, "w");
	if (!out) {
		perror (troff_name);
		return 0;
	}
	fprintf (out, "name %s\n", troff_name);
	if (!strcmp (char_map->encoding, "adobe-fontspecific"))
		fprintf (out, "special\n");
	fi = XLoadQueryFont (dpy, names[0]);
	if (charExists (fi, ' ')) {
		int w = charWidth (fi, ' ');
		if (w > 0)
			fprintf (out, "spacewidth %d\n", w);
	}
	fprintf (out, "charset\n");
	for (c = fi->min_char_or_byte2; c <= fi->max_char_or_byte2; c++) {
		char *name = DviCharName (char_map,c,0);
		if (charExists (fi, c) && (groff_flag || name)) {

			wid = charWidth (fi, c);

			fprintf (out, "%s\t%d",
					name ? name : "---",
	 				wid);
			if (groff_flag) {
				int	param[5];
				param[0] = charHeight (fi, c);
				param[1] = charDepth (fi, c);
				param[2] = 0 /* charRBearing (fi, c) - wid */;
				param[3] = 0 /* charLBearing (fi, c) */;
				param[4] = 0; /* XXX */
				for (j = 0; j < 5; j++)
					if (param[j] < 0)
						param[j] = 0;
				for (j = 4; j >= 0; j--)
					if (param[j] != 0)
						break;
				for (k = 0; k <= j; k++)
					fprintf (out, ",%d", param[k]);
			}
			fprintf (out, "\t0\t0%o\n", c);
			
			if (name) {
				for (k = 1; DviCharName(char_map,c,k); k++) {
					fprintf (out, "%s\t\"\n",
						 DviCharName (char_map,c,k));
				}
			}
		}
	}
	XUnloadFont (dpy, fi->fid);
	fclose (out);
	return 1;
}

static usage(prog)
	char	*prog;
{
	fprintf (stderr, "usage: %s [-g] FontMap\n", prog);
	exit (1);
}

main (argc, argv)
	char	**argv;
{
	char	troff_name[1024];
	char	font_name[1024];
	char	line[1024];
	char	*a, *b, c;
	int	position;
	FILE	*map;
	int	opt;
	extern int optind;

	while ((opt = getopt(argc, argv, "g")) != EOF) {
		switch (opt) {
		case 'g':
			groff_flag = 1;
			break;
		default:
			usage();
		}
	}
	if (argc - optind != 1)
		usage(argv[0]);

	dpy = XOpenDisplay (0);
	if (!dpy) {
		fprintf (stderr, "Can't connect to the X server.\n");
		fprintf (stderr, "Make sure the DISPLAY environment variable is set correctly.\n");
		exit (1);
	}
        position = 1;

	map = fopen (argv[optind], "r");
	if (map == NULL) {
		perror (argv[optind]);
		exit (1);
	}

	while (fgets (line, sizeof (line), map)) {
		for (a=line,b=troff_name; *a; a++,b++) {
			c = (*b = *a);
			if (c == ' ' || c == '\t')
				break;
		}
		*b = '\0';
		while (*a && (*a == ' ' || *a == '\t'))
			++a;
		for (b=font_name; *a; a++,b++)
			if ((*b = *a) == '\n')
				break;
		*b = '\0';
		printf ("%s -> %s\n", font_name, troff_name);
		if (!MapFont (font_name, troff_name))
			exit (1);
		++position;
	}
	exit (0);
}

/*
Local Variables:
c-indent-level: 8
c-continued-statement-offset: 8
c-brace-offset: -8
c-argdecl-indent: 8
c-label-offset: -8
c-tab-always-indent: nil
End:
*/
