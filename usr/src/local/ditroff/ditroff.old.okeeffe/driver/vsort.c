/* vsort.c	1.2	83/07/29
 *
 *	sort troff output into troff output that only goes one
 *	direction down the page.
 */

/*******************************************************************************

output language from troff:
all numbers are character strings

sn	size in points
fn	font as number from 1-n
cx	ascii character x
Cxyz	funny char xyz. terminated by white space
Hn	go to absolute horizontal position n
Vn	go to absolute vertical position n (down is positive)
hn	go n units horizontally (relative)
vn	ditto vertically
nnc	move right nn, then print c (exactly 2 digits!)
		(this wart is an optimization that shrinks output file size
		 about 35% and run-time about 15% while preserving ascii-ness)
Dt ...\n	draw operation 't':
	Dt x		line thickness setting
	Ds x		line style (bit mask) setting
	Dl x y		line from here by x,y
	Dc d		circle of diameter d with left side here
	De x y		ellipse of axes x,y with left side here
	Da x y r	arc counter-clockwise by x,y of radius r
	D~ (or Dg) x y x y ...	wiggly line by x,y then x,y ...
nb a	end of line (information only -- no action needed)
	b = space before line, a = after
p	new page begins -- set v to 0
#...\n	comment
x ...\n	device control functions:
	x i	init
	x T s	name of device is s
	x r n h v	resolution is n/inch
		h = min horizontal motion, v = min vert
	x p	pause (can restart)
	x s	stop -- done for ever
	x t	generate trailer
	x f n s	font position n contains font s
	x H n	set character height to n
	x S n	set slant to N

	Subcommands like "i" are often spelled out like "init".

*******************************************************************************/

#include	<stdio.h>
#include	<ctype.h>


#define	FATAL	1
#define	NVLIST	1500
#define	OBUFSIZ	40000
#define	SLOP	1000

#define hgoto(n)	hpos = n
#define hmot(n)		hgoto(hpos + (n))
#define vmot(n)		vgoto(vpos + (n))


int	dbg	= 0;	/* debug flag != 0 means do debug output */
int	size	= 10;	/* current size */
int	font	= 1;	/* current font */
int	thick	= 3;	/* line thickness */
int	style	= 255;	/* line style bit-mask */
int	hpos	= 0;	/* horizontal position to be at next (left = 0) */
int	vpos	= 0;	/* current vertical position (down positive) */

struct vlist {
	int	v;	/* vertical position of this spread */
	int	h;	/* horizontal position */
	int	s;	/* point size */
	int	t;	/* line thickness */
	char	f;	/* font number */
	char	st;	/* style mask */
	char	*p;	/* text pointer to this spread */
};

struct	vlist	vlist[NVLIST + 1];
struct	vlist	*vlp	= vlist;
int	nvlist	= 0;
int	obufsiz	= OBUFSIZ;
char	obuf[OBUFSIZ + SLOP];
char	*op = obuf;


main(argc, argv)
int argc;
char *argv[];
{
	FILE *fp;


	while (argc > 1 && **++argv == '-') {
	    switch ((*argv)[1]) {
		case 'd':
			dbg = atoi(&(*argv)[2]);
			if (dbg == 0) {
			    dbg = 1;
			    obufsiz = 50;
			} else
			    obufsiz = dbg;
			break;
	    }
	    argc--;
	}
	if (argc <= 1)
	    conv(stdin);
	else
	    while (--argc > 0) {
		if ((fp = fopen(*argv, "r")) == NULL)
		    error(FATAL, "can't open %s", *argv);
		conv(fp);
		fclose(fp);
	    }
    done();
}

			/* read number from input:  copy to output */
int getnumber (fp)
register FILE *fp;
{
    register int k;
    register char c;

    while ((c = getc(fp)) == ' ')
	;
    k = 0;
    do {
	k = 10 * k + (*op++ = c) - '0';
    } while (isdigit(c = getc(fp)));
    ungetc(c, fp);
    return (k);
}

			/* read number from input:  do _N_O_T copy to output */
int ngetnumber (fp)
register FILE *fp;
{
    register int k;
    register char c;

    while ((c = getc(fp)) == ' ')
	;
    k = 0;
    do {
	k = 10 * k + c - '0';
    } while (isdigit(c = getc(fp)));
    ungetc(c, fp);
    return (k);
}


conv(fp)
register FILE *fp;
{
	register int c;
	int m, n, m1, n1;
	char buf[SLOP];

	while ((c = getc(fp)) != EOF) {
	    if (dbg) fprintf(stderr, "%c i=%d V=%d\n", c, op-obuf, vpos);
	    if (op > obuf + obufsiz)
		oflush();
	    switch (c) {
		case '\n':	/* let text input through */
		case '\t':
		case ' ':
			*op++ = c;
			break;
		case '{':	/* push down current environment */
			*op++ = c;
			t_push();
			break;
		case '}':	/* pop up last environment */
			*op++ = c;
			t_pop();
			break;
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
				/* two motion digits plus a character */
			*op++ = c;
			hmot((c-'0') * 10 + (*op++ = getc(fp)) - '0');
			*op++ = getc(fp);
			break;
		case 'c':	/* single ascii character */
			*op++ = c;
			*op++ = getc(fp);
			break;
		case 'C':	/* white-space terminated funny character */
			*op++ = c;
			while ((*op++ = c = getc(fp)) != ' ' && c != '\n')
				;
			break;
		case 't':	/* straight text */
			*op++ = c;
			fgets(op, SLOP, fp);
			op += strlen(op);
			break;
		case 'D':	/* draw function */
			fgets(buf, SLOP, fp);
			switch (buf[0]) {
			case 's':	/* "style" */
				sscanf(buf+1, "%d", &style);
				sprintf(op, "D%s", buf);
				break;
			case 't':	/* thickness */
				sscanf(buf+1, "%d", &thick);
				sprintf(op, "D%s", buf);
				break;
			case 'l':	/* draw a line */
				sscanf(buf+1, "%d %d", &n, &m);
						/* if line starts higher, put */
						/* it on its own span */
				if (m < 0) {
				    startspan(vpos + m);
				    sprintf(op, "V%d ", vpos);
				    op += strlen(op);
				}
				sprintf(op, "D%s", buf);
				op += strlen(op);
				hmot(n);
				vmot(m);
				break;
			case 'c':	/* circle */
				sscanf(buf+1, "%d", &n);	/* always put */
				startspan(vpos + n/2);		/* circles on */
				sprintf(op, "V%d D%s", vpos, buf);/*their own */
				op += strlen(op);		/* vertl list */
				hmot(n);
				startspan(vpos);
				break;
			case 'e':	/* ellipse */
				sscanf(buf+1, "%d %d", &m, &n);	/* same here */
				startspan(vpos + n/2);
				sprintf(op, "V%d D%s", vpos, buf);
				op += strlen(op);
				hmot(m);
				startspan(vpos);
				break;
			case 'a':	/* arc */
				sscanf(buf+1, "%d %d %d %d", &n, &m, &n1, &m1);
				sprintf(op, "D%s", buf);
				op += strlen(op);
				hmot(n + n1);
				vmot(m + m1);
				break;
			case '~':	/* wiggly line */
			case 'g':	/* gremlin curve */
			    {
				register char *pop;
							/* a curve goes on */
				startspan(vpos);	/* its own span */
				sprintf(op, "V%dD", vpos);  /* vertical move */
				pop = op += strlen(op);     /* to curve start */
				do {			   /* read in rest of */
				    sprintf(op, "%s", buf);/* point input */
				    op += strlen(op);
				    if (*(op - 1) != '\n')
					fgets(buf, SLOP, fp);
				} while (*(op - 1) != '\n');
				n = vpos;		/* = minimum vertical */
							/* position for curve */
				while (*++pop == ' ');	/* skip '~' & blanks */
				do {			/* calculate minimum */
				    hpos += atoi(pop);		/* vertical */
				    while (isdigit(*++pop));	/* position */
				    while (*++pop == ' ');
				    vpos += atoi(pop);
				    while (isdigit(*++pop));
				    while (*pop == ' ') pop++;
				    if (vpos < n) n = vpos;
				} while (*pop != '\n');

				(vlp - 1)->v = n;
				startspan(vpos);
			    }
			    break;

			default:
				error(FATAL,"unknown drawing command %s\n",buf);
				break;
			}
			break;
		case 's':
			*op++ = c;
			size = getnumber (fp);
			break;
		case 'f':
			*op++ = c;
			font = getnumber (fp);
			break;
		case 'H':	/* absolute horizontal motion */
			*op++ = c;
			hgoto(getnumber (fp));
			break;
		case 'h':	/* relative horizontal motion */
			*op++ = c;
			hmot(getnumber (fp));
			break;
		case 'w':	/* useless */
			break;
		case 'V':	/* absolute vertical motion */
			vgoto(ngetnumber (fp));
			break;
		case 'v':
			vmot(ngetnumber (fp));
			break;
		case 'p':	/* new page */
			vpos = 0;
			oflush();
			*op++ = c;
			fscanf(fp, "%s", op);
			op += strlen(op);
			break;
		case 'n':	/* end of line */
			hpos = 0;
		case '#':	/* comment */
			*op++ = c;
			while ((*op++ = getc(fp)) != '\n')
				;
			break;
		case 'x':	/* device control */
			oflush();
			putchar(c);
			fgets(buf, sizeof buf, fp);
			fputs(buf, stdout);
			fflush(stdout);
			break;
		default:
			error(!FATAL, "unknown input character %o %c\n", c, c);
			done();
	    }
	}
}


oflush()	/* sort, then dump out contents of obuf */
{
	register struct vlist *vp;
	register int lastv = -1;
	register int i;
	int compar();

	if (dbg) fprintf(stderr, "into oflush, V=%d\n", vpos);
	if (op == obuf)
		return;
 	qsort((char *) vlist, nvlist, sizeof (struct vlist), compar);
	*op++ = 0;
	vp = vlist;
	for (i = 0; i < nvlist; i++, vp++) {
	    register char *p;

	    if (lastv != vp->v) printf("V%d", lastv = vp->v);
	    printf("H%ds%df%d Ds %d\nDt %d\n",vp->h,vp->s,vp->f,vp->st,vp->t);
	    for (p = vp->p; *p != 0; p++) putchar(*p);
	}
	fflush(stdout);
	vlp = vlist;
	vlp->p = op = obuf;
	vlp->h = hpos;
	vlp->v = vpos;
	vlp->s = size;
	vlp->f = font;
	vlp->st = style;
	vlp->t = thick;
	*op = 0;
	vlp++;
	nvlist = 1;
}


compar(p1, p2)
struct vlist *p1, *p2;
{
	return(p1->v - p2->v);
}

done()
{
	oflush();
	exit(0);
}

error(f, s, a1, a2, a3, a4, a5, a6, a7) {
	fprintf(stderr, "vsort: ");
	fprintf(stderr, s, a1, a2, a3, a4, a5, a6, a7);
	fprintf(stderr, "\n");
	if (f)
		done();
}

#define	MAXSTATE	5

struct state {
	int	ssize;
	int	sfont;
	int	shpos;
	int	svpos;
};
struct	state	state[MAXSTATE];
struct	state	*statep = state;

t_push()	/* begin a new block */
{
	statep->ssize = size;
	statep->sfont = font;
	statep->shpos = hpos;
	statep->svpos = vpos;
	hpos = vpos = 0;
	if (statep++ >= state+MAXSTATE)
		error(FATAL, "{ nested too deep");
	hpos = vpos = 0;
}

t_pop()	/* pop to previous state */
{
	if (--statep < state)
		error(FATAL, "extra }");
	size = statep->ssize;
	font = statep->sfont;
	hpos = statep->shpos;
	vpos = statep->svpos;
}


	/* vertical motion:  start new vertical span if necessary */
vgoto(n)
register int n;
{
    if (n != vpos)
	startspan(n);
    vpos = n;
}


startspan(n)
register int n;
{
	*op++ = 0;
	if (nvlist >= NVLIST) {
	    oflush();
	}
	vlp->p = op;
	vlp->v = n;
	vlp->h = hpos;
	vlp->s = size;
	vlp->f = font;
	vlp->st = style;
	vlp->t = thick;
	vlp++;
	nvlist++;
}
