#ifdef lint
static char sccsid[] = "@(#)makedev.c	1.2	(CWI)	1.2	85/10/24";
#endif lint
/*
  makedev:
	read text info about a particular device
	(e.g., cat, 202, aps5) from file, convert
	it into internal (binary) form suitable for
	fast reading by troff initialization (ptinit()).

	Usage:

	makedev DESC [ F ... ]
		uses DESC to create a description file
		using the information therein.
		It creates the file DESC.out.

	makedev F ...
		makes the font tables for fonts F only,
		creates files F.out.

	DESC.out contains:
	dev structure with fundamental sizes
	list of sizes (nsizes+1) terminated by 0, as short's
	indices of char names (nchtab * sizeof(short))
	char names as hy\0em\0... (lchname)
	nfonts occurrences of
		widths (nwidth)
		kerning (nwidth) [ascender+descender only so far]
		codes (nwidth) to drive actual typesetter
		fitab (nchtab+128-32)
	each of these is an array of char.

	dev.filesize contains the number of bytes
	in the file, excluding the dev part itself.

	F.out contains the font header, width, kern, codes, and fitab.
	Width, kern and codes are parallel arrays.
	(Which suggests that they ought to be together?)
	Later, we might allow for codes which are actually
	sequences of formatting info so characters can be drawn.
*/

/*
 * Changes made at the Mathematisch centrum:
 *
 * 1	added another table of shorts, with contains a number of the
 *	physical font where the char actual lives. This is done because
 *	the typesetter at the MC (harris 7400)has "mixed fonts".
 *	(for instance, the font with number 4478 contains the 
 *	ligatures etc. for as well the times roman
 *	and the times italic fonts).
 *	This table (fcode(nwidth)) starts after the fitab,
 *	this is done so we don't have to change in troff t6.c setfp().
 *	For the sake of compatibility, this change will only be effective
 *	if the field fonttab is set to 1. This change has to be wired
 *	in troff etc. as well.
 * 
 * 2	The sans-serif fonts of the Harris doesn't have an italian
 *	equivalent, they are suppose to be made bij the Oblique
 *	facility of the Harris. This will slant the character by
 *	9, 12 or 15 degrees. So another field in the font sruct is added
 *	for this function as well.
 *
 * 3	added some checking of input desciption (for instance declaring
 *	too many sizes).
 *
 * These changes are made by jaap akkerhuis, 1982
 *
 * Warning: using the spare fields may give bizarre effects, see comment
 *	at font.spare1
 */

#include	"stdio.h"
#include	"../dev.h"

#define	BMASK	0377
#define	skipline(f)	while(getc(f) != '\n')

struct	dev	dev;
struct	Font	font;

#define	NSIZE	100	/* maximum number of sizes */
short	size[NSIZE];
#define	NCH	256	/* max number of characters with funny names */
char	chname[5*NCH];	/* character names, including \0 for each */
short	chtab[NCH];	/* index of character in chname */
char	nmseen[5*NCH];	/* The names we have seen */

#define	NFITAB	(NCH + 128-32)	/* includes ascii chars, but not non-graphics */
char	fitab[NFITAB];	/* font index table: position of char i on this font. */
			/* zero if not there */

#define	FSIZE	200	/* size of a physical font (e.g., 102 for cat) */
char	width[FSIZE];	/* width table for a physical font */
char	kern[FSIZE];	/* ascender+descender info */
char	code[FSIZE];	/* actual device codes for a physical font */
short	fcode[FSIZE];	/* MC:jna physical font number */

#define	NFONT	60	/* max number of default fonts */
				/*
				 * 60 to support Versatec Berkeley style
				 * filters. Aargh!
				 */
char	fname[NFONT][10];	/* temp space to hold default font names */

int	fflag	= 0;	/* on if font table to be written */
int	fdout;	/* output file descriptor */
char	*fout	= "DESC.out";

main(argc, argv)
char *argv[];
{
	FILE *fin;
	char cmd[100], *p;
	int i, totfont, v, check;

	if ((fin = fopen("DESC", "r")) == NULL) {
		fprintf(stderr, "makedev: can't open %s\n", argv[1]);
		exit(1);
	}
	while (fscanf(fin, "%s", cmd) != EOF) {
		if (cmd[0] == '#')	/* comment */
			skipline(fin);
		else if (strcmp(cmd, "res") == 0) {
			fscanf(fin, "%hd", &dev.res);
		} else if (strcmp(cmd, "hor") == 0) {
			fscanf(fin, "%hd", &dev.hor);
		} else if (strcmp(cmd, "vert") == 0) {
			fscanf(fin, "%hd", &dev.vert);
		} else if (strcmp(cmd, "unitwidth") == 0) {
			fscanf(fin, "%hd", &dev.unitwidth);
		} else if (strcmp(cmd, "sizescale") == 0) {
			fscanf(fin, "%hd", &dev.sizescale);
		} else if (strcmp(cmd, "paperwidth") == 0) {
			fscanf(fin, "%hd", &dev.paperwidth);
		} else if (strcmp(cmd, "paperlength") == 0) {
			fscanf(fin, "%hd", &dev.paperlength);
		} else if (strcmp(cmd, "spare1") == 0) {
			fscanf(fin, "%hd", &dev.spare1);
		} else if (strcmp(cmd, "spare2") == 0) {
			fscanf(fin, "%hd", &dev.spare2);
		} else if (strcmp(cmd, "sizes") == 0) {
			dev.nsizes = 0;
			while (fscanf(fin, "%d", &v) != EOF && v != 0) {
				size[dev.nsizes++] = v;
				if(dev.nsizes >= NSIZE) { /*MC:jna addition */
					fprintf(stderr, "Too many sizes\n");
					exit(3);
				}
			}
			size[dev.nsizes] = 0;	/* need an extra 0 at the end */
		} else if (strcmp(cmd, "fonts") == 0) {
			fscanf(fin, "%hd", &dev.nfonts);
			for (i = 0; i < dev.nfonts; i++) {
				if ( i >=  NFONT) {
					fprintf(stderr,"Too many default fonts\n");
					exit(3);
				}
				fscanf(fin, "%s", fname[i]);
			}
		} else if (strcmp(cmd, "charset") == 0) {
			p = chname;
			dev.nchtab = 0;
			while (fscanf(fin, "%s", p) != EOF) {
				chtab[dev.nchtab++] = p - chname;
				if (dev.nchtab > NCH) {
					fprintf(stderr,
						"Too many charnames at %s\n",p);
					exit(3);
				}
				for (i = 0; i < dev.nchtab - 1; i++)
					if (strcmp(&chname[chtab[i]], p) == 0)
						printf("Warning: charname %s used more then once\n", p);
				while (*p++)	/* skip to end of name */
					;
			}
			dev.lchname = p - chname;
			chtab[dev.nchtab++] = 0;	/* terminate properly */
		} else
			fprintf(stderr, "makedev: unknown command %s\n", cmd);
	}
	if (argc > 0 && strcmp(argv[1], "DESC") == 0) {
		check++;
		fdout = creat(fout, 0666);
		if (fdout < 0) {
			fprintf(stderr, "makedev: can't open %s\n", fout);
			exit(1);
		}
		write(fdout, &dev, sizeof(struct dev));
		write(fdout, size, (dev.nsizes+1) * sizeof(size[0]));	/* we need a 0 on the end */
		write(fdout, chtab, dev.nchtab * sizeof(chtab[0]));
		write(fdout, chname, dev.lchname);
		totfont = 0;
		for (i = 0; i < dev.nfonts; i++) {
			/*
			 * Get fontinfo ...
			 */
			dofont(fname[i]);
			/*
			 * ... and force space in troff allocated for the
			 * biggest font possible and limited by makedev
			 * to be loaded in troff by faking font.nwfont
			 * (and bumping up the size of DESC.out) by
			 * recalculating the padded out fontsize (v)
			 *	jna
			 */
			font.nwfont = FSIZE;
			v = sizeof(struct Font) + 3 * FSIZE + dev.nchtab + 128-32;
				/*
				 * This is not correct, we can still
				 * have too less space if the default
				 * mounted fonts does not contain a
				 * fonttab, but I don't want to change
				 * troff on the moment...
				 */
			if(font.fonttab == 1)
				v += FSIZE * sizeof( short );
			totfont += v;
			write(fdout, &font, sizeof(struct Font));
			write(fdout, width, font.nwfont & BMASK);
			write(fdout, kern, font.nwfont & BMASK);
			write(fdout, code, font.nwfont & BMASK);
			write(fdout, fitab, dev.nchtab+128-32);
			if(font.fonttab == 1)
				write(fdout, fcode, (font.nwfont & BMASK) * sizeof(fcode[0]));
		}
		lseek(fdout, 0L, 0);	/* back to beginning to install proper size */
		dev.filesize =		/* excluding dev struct itself */
			(dev.nsizes+1) * sizeof(size[0])
			+ dev.nchtab * sizeof(chtab[0])
			+ dev.lchname * sizeof(char)
			+ totfont * sizeof(char);
		write(fdout, &dev, sizeof(struct dev));
		close(fdout);
		argc--;
		argv++;
	}
	for (i = 1; i < argc; i++)
		dofont(argv[i]);
	if( check)
		checknames();
	exit(0);
}

dofont(name)	/* create fitab and width tab for font */
char *name;
{
	FILE *fin;
	int fdout;
	int i, nw, spacewidth, n, v;
	char buf[100], ch[10], s1[10], s2[10], s3[10],s4[10], cmd[30];
	char *p, *p1;

	if ((fin = fopen(name, "r")) == NULL) {
		fprintf(stderr, "makedev: can't open font %s\n", name);
		exit(2);
	}
	sprintf(cmd, "%s.out", name);
	fdout = creat(cmd, 0666);
	for (i = 0; i < NFITAB; i++)
		fitab[i] = 0;
	for (i = 0; i < FSIZE; i++) {
		width[i] = kern[i] = code[i] = 0;
		fcode[i] = 0;
	}
	font.fonttab = font.slant = font.specfont = font.ligfont = spacewidth = 0;
	font.spare1 = NULL;
	font.namefont[0] = font.intname[0] = NULL;

	while (fscanf(fin, "%s", cmd) != EOF) {
		if (cmd[0] == '#')
			skipline(fin);
		else if (strcmp(cmd, "name") == 0)
			fscanf(fin, "%s", font.namefont);
		else if (strcmp(cmd, "internalname") == 0)
			fscanf(fin, "%s", font.intname);
		else if (strcmp(cmd, "special") == 0)
			font.specfont = 1;
		else if (strcmp(cmd, "fonttab") == 0)
			font.fonttab = 1;
		else if (strcmp(cmd, "slant") == 0) {
			fscanf(fin, "%d", &i);
			font.slant = i;
		} else if (strcmp(cmd, "spare1") == 0) {
			fscanf(fin, "%1s", s1); /*MC:jna %1s will get 1 char +
						 * a \0 added, so
						 * &font.spare1 will place
						 * a \0 in font.name[0]
						 */
			font.spare1 = s1[0];
		} else if (strcmp(cmd, "ligatures") == 0) {
			font.ligfont = getlig(fin);
		} else if (strcmp(cmd, "spacewidth") == 0) {
			fscanf(fin, "%d", &spacewidth);
			width[0] = spacewidth;	/* width of space on this font */
		} else if (strcmp(cmd, "charset") == 0) {
			skipline(fin);
			nw = 0;
			/* widths are origin 1 so fitab==0 can mean "not there" */
			/*
			 *MC:jna and so width[0] can mean spacewidth
			 *
			 * note that in charset part cann't be any comments!
			 */
			while (fgets(buf, 100, fin) != NULL) {
				int x;
/*
printf(buf);
*/
				if(font.fonttab == 1) {
					x = sscanf(buf, "%s %s %s %s %s", ch,s1,s2,s3,s4);
					if (x != 5 && s1[0] != '"')
						printf("sscanf mismatch %d\n", x);
				}
				else {
					x = sscanf(buf, "%s %s %s %s", ch, s1, s2, s3);
					if (x != 4 && s1[0] != '"')
						printf("sscanf mismatch %d\n", x);
				}
				if (s1[0] != '"') {	/* it's a genuine new character */
					nw++;
					if( nw > FSIZE) {
						fprintf(stderr,"Too big font!\n");
						exit(1);
					}
					width[nw] = atoi(s1);
					kern[nw] = atoi(s2);
					/* temporarily, pick up one byte as code */
					if (s3[0] == '0')
						sscanf(s3, "%o", &i);
					else
						sscanf(s3, "%d", &i);
					code[nw] = i;
					if(font.fonttab == 1)
						fcode[nw] = atoi(s4);
				}
				/* otherwise it's a synonym for previous character,
				/* so leave previous values intact
				*/
				if (strlen(ch) == 1) {	/* it's ascii */
					if(fitab[ch[0] - 32])
						printf("Warning: redefining character %c\n", ch[0]);
					fitab[ch[0] - 32] = nw;	/* fitab origin omits non-graphics */
				} else {		/* it has a funny name */
					for (i = 0; i < dev.nchtab; i++)
						if (strcmp(&chname[chtab[i]], ch) == 0) {
							if(fitab[i+128-32])
								printf("Warning: redefining character %s\n", ch);
							fitab[i + 128-32] = nw;	/* starts after the ascii */
							if(!nmseen[chtab[i]]) {
								p = &nmseen[chtab[i]];
								p1 = ch;
								while(*p1)
									*p++ = *p1++;
							}
							break;
						}
					if (i >= dev.nchtab)
						fprintf(stderr, "makedev: font %s: %s not in charset\n", name, ch);
				}
			}
			nw++;
			font.nwfont = n = nw;
		}
	}
	if (spacewidth == 0)
		width[0] = dev.res * dev.unitwidth / 72 / 3;
	if (font.intname[0] == NULL)
		fprintf(stderr, "Keyword internalname not specified\n");
	if (font.namefont[0] == NULL)
		fprintf(stderr, "Keyword fontname not specified\n");
	fclose(fin);

	write(fdout, &font, sizeof(struct Font));
	write(fdout, width, font.nwfont & BMASK);
	write(fdout, kern, font.nwfont & BMASK);
	write(fdout, code, font.nwfont & BMASK);
	write(fdout, fitab, dev.nchtab+128-32);
	if(font.fonttab == 1)
		write(fdout, fcode, (font.nwfont & BMASK) * sizeof(fcode[0]));
	close(fdout);
	v = sizeof(struct Font) + 3 * n + dev.nchtab + 128-32;
	if(font.fonttab == 1)
		v += n * sizeof( short );
	fprintf(stderr, "%3s: %3d chars, width %3d, size %3d\n",
		font.namefont, nw, width[0], v);
	return v;
	/*
	 * MC:jna v is the filesize of one font in bytes
	 */
}

getlig(fin)	/* pick up ligature list */
	FILE *fin;
{
	int lig;
	char temp[100];

	lig = 0;
	while (fscanf(fin, "%s", temp) != EOF && strcmp(temp, "0") != 0) {
		if (strcmp(temp, "fi") == 0)
			lig |= LFI;
		else if (strcmp(temp, "fl") == 0)
			lig |= LFL;
		else if (strcmp(temp, "ff") == 0)
			lig |= LFF;
		else if (strcmp(temp, "ffi") == 0)
			lig |= LFFI;
		else if (strcmp(temp, "ffl") == 0)
			lig |= LFFL;
		else
			fprintf(stderr, "illegal ligature %s\n", temp);
	}
	skipline(fin);
	return lig;
}

checknames()
{	register int i, error;

	error = 0;

	for( i =0; i < dev.nchtab; i++) {
		if( !nmseen[chtab[i]]) {
			printf("Warning: %s not defined in any of the font(s)\n",
							&chname[chtab[i]]);
			if ( !error)
				error++;
		}
	}
	if (error) {
		printf("This can lead to strange results when these ");
		printf("characters are actually used!\n");
	}
	printf("Number of special character names: %d\n", dev.nchtab -1);
}
