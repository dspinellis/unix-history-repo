#ifndef lint
static char sccsid[] = "@(#)t10.c	1.1 (CWI) 85/07/17";
#endif lint

#include "tdef.h"
extern
#include "d.h"
extern
#include "v.h"
/*
troff10.c

CAT interface
*/

#include <sgtty.h>
#include "ext.h"
int	vpos	 = 0;	/* absolute vertical position on page */
int	hpos	 = 0;	/* ditto horizontal */

#define	T_IESC	16

short	*chtab;
char	*chname;
char	*fontab[NFONT+1];
char	*kerntab[NFONT+1];
char	*fitab[NFONT+1];

int	Inch;
int	Hor;
int	Vert;
int	Unitwidth;
int	nfonts;
int	nsizes;
int	nchtab;

/* these characters are used as various signals or values
/* in miscellaneous places.
/* values are set in specnames in t10.c
*/

int	c_hyphen;
int	c_emdash;
int	c_rule;
int	c_minus;
int	c_narsp;
int	c_hnarsp;
int	c_fi;
int	c_fl;
int	c_ff;
int	c_ffi;
int	c_ffl;
int	c_acute;
int	c_grave;
int	c_under;
int	c_rooten;
int	c_boxrule;
int	c_lefthand;

#include "dev.h"
struct dev dev;
struct font *fontbase[NFONT+1];


ptinit()
{
	int	i, fin, nw;
	char	*setbrk(), *filebase, *p;

	/* open table for device,
	/* read in resolution, size info, font info, etc.
	/* and set params
	*/
	strcat(termtab, "/dev");
	strcat(termtab, devname);
	strcat(termtab, "/DESC.out");	/* makes "..../devXXX/DESC.out" */
	if ((fin = open(termtab, 0)) < 0) {
		fprintf(stderr, "troff: can't open tables for %s\n", termtab);
		done3(1);
	}
	read(fin, &dev, sizeof(struct dev ));
	Inch = dev.res;
	Hor = dev.hor;
	Vert = dev.vert;
	Unitwidth = dev.unitwidth;
	nfonts = dev.nfonts;
	nsizes = dev.nsizes;
	nchtab = dev.nchtab;
	filebase = setbrk(dev.filesize + EXTRAFONT);	/* enough room for whole file */
		/* plus room for fontcache */
	read(fin, filebase, dev.filesize);	/* all at once */
	pstab = (short *) filebase;
	chtab = pstab + nsizes + 1;
	chname = (char *) (chtab + dev.nchtab);
	p = chname + dev.lchname;
	for (i = 1; i <= nfonts; i++) {
		fontbase[i] = (struct font *) p;
		nw = *p & BMASK;	/* 1st thing is width count */
		fontlab[i] = PAIR(fontbase[i]->namefont[0], fontbase[i]->namefont[1]);
		/* for now, still 2 char names */
		if (smnt == 0 && fontbase[i]->specfont == 1)
			smnt = i;	/* first special font */
		p += sizeof(struct font);	/* that's what's on the beginning */
		fontab[i] = p;
		kerntab[i] = p + nw;
		fitab[i] = p + 3 * nw;	/* skip width, kern, code */
		p += 3 * nw + dev.nchtab + 128 - 32;
		/*
		 *MC:jna skip also fcode, if there
		 *See also comment in makedev.c
		 */
		if(fontbase[i]->fonttab == 1)
			p += nw * sizeof(short);
	}
	fontbase[0] = (struct font *) p;	/* the last shall be first */

	/*
	fontbase[0]->nwfont = EXTRAFONT - dev.nchtab - (128-32) - sizeof (struct font) - 255*(sizeof(short) + 2*sizeof(char));
	last is for 255*(fonttab, kerntab and codetab(leaves us the
		space for maximum chars
	Yes, this is correct if you want to calculate this, but it has
	been defined as well now
	*/

	fontbase[0]->nwfont = MAXCHARS;
	fontab[0] = p + sizeof (struct font);
	close(fin);
	/* there are a lot of things that used to be constant
	/* that now require code to be executed.
	*/
	sps = SPS;
	ics = ICS;
	for (i = 0; i < 16; i++)
		tabtab[i] = DTAB * (i + 1);
	pl = 11 * INCH;
	po = PO;
	spacesz = SS;
	lss = lss1 = VS;
	ll = ll1 = lt = lt1 = LL;
	specnames();	/* install names like "hyphen", etc. */
	if (ascii)
		return;
	fprintf(ptid, "x T %s\n", devname);
	fprintf(ptid, "x res %d %d %d\n", Inch, Hor, Vert);
	fprintf(ptid, "x init\n");	/* do initialization for particular device */
	for (i = 1; i <= nfonts; i++)
		fprintf(ptid, "x font %d %s\n", i, fontbase[i]->namefont);
  /*
	fprintf(ptid, "x xxx fonts=%d sizes=%d unit=%d\n", nfonts, nsizes, Unitwidth);
	fprintf(ptid, "x xxx nchtab=%d lchname=%d nfitab=%d\n",
		dev.nchtab, dev.lchname, dev.nchtab+128-32);
	fprintf(ptid, "x xxx sizes:\nx xxx ");
	for (i = 0; i < nsizes; i++)
		fprintf(ptid, " %d", pstab[i]);
	fprintf(ptid, "\nx xxx chars:\nx xxx ");
	for (i = 0; i < dev.nchtab; i++)
		fprintf(ptid, " %s", &chname[chtab[i]]);
	fprintf(ptid, "\nx xxx\n");
  */
}

specnames()
{
	static struct {
		int	*n;
		char	*v;
	} spnames[] = {
		&c_hyphen, "hy",
		&c_emdash, "em",
		&c_rule, "ru",
		&c_minus, "\\-",
		&c_narsp, "\\|",
		&c_hnarsp, "\\^",
		&c_fi, "fi",
		&c_fl, "fl",
		&c_ff, "ff",
		&c_ffi, "Fi",
		&c_ffl, "Fl",
		&c_acute, "aa",
		&c_grave, "ga",
		&c_under, "ul",
		&c_rooten, "rn",
		&c_boxrule, "br",
		&c_lefthand, "lh",
		0, 0
	};
	int	i;

	for (i = 0; spnames[i].n; i++)
		*spnames[i].n = findch(spnames[i].v);
}

findch(s)	/* find char s in chname */
register char	*s;
{
	register int	i;

	for (i = 0; i < nchtab; i++)
		if (strcmp(s, &chname[chtab[i]]) == 0)
			return(i + 128);
	return(0);
}

ptout(i)
tchar	i;
{
	register dv, ik;
	register tchar	*k;
	int	temp, a, b;

	if (cbits(i) != '\n') {
		*olinep++ = i;
		return;
	}
	if (olinep == oline) {
		lead += lss;
		return;
	}

	hpos = po;	/* ??? */
	esc = 0;	/* ??? */
	ptesc();	/* the problem is to get back to the left end of the line */
	dv = 0;
	for (k = oline; k < olinep; k++) {
		if (ismot(*k) && isvmot(*k)) {
			temp = absmot(*k);
			if (isnmot(*k))
				temp = -temp;
			dv += temp;
		}
	}
	if (dv) {
		vflag++;
		*olinep++ = makem(-dv);
		vflag = 0;
	}

	b = dip->blss + lss;
	lead += dip->blss + lss;
	dip->blss = 0;
	for (k = oline; k < olinep; )
		k += ptout0(k);	/* now passing a pointer! */
	olinep = oline;
	lead += dip->alss;
	a = dip->alss;
	dip->alss = 0;
	/*
	fprintf(ptid, "x xxx end of line: hpos=%d, vpos=%d\n", hpos, vpos);
*/
	fprintf(ptid, "n%d %d\n", b, a);	/* be nice to chuck */
}

ptout0(pi)
tchar	*pi;
{
	register short j, k, w;
	short	z, dx, dy, dx2, dy2, n;
	tchar	i;
	int outsize;	/* size of object being printed */

	outsize = 1;	/* default */
	i = *pi;
	k = cbits(i);
	if (ismot(i)) {
		j = absmot(i);
		if (isnmot(i))
			j = -j;
		if (isvmot(i))
			lead += j;
		else 
			esc += j;
		return(outsize);
	}
	if (k == CHARHT) {
		if (xpts != mpts)
			ptps();
		fprintf(ptid, "x H %d\n", sbits(i));
		return(outsize);
	}
	if (k == SLANT) {
		fprintf(ptid, "x S %d\n", sfbits(i)-180);
		return(outsize);
	}
	if (k == WORDSP) {
		oput('w');
		return(outsize);
	}
	if (k == FONTPOS) {
		char temp[3];
		n = i >> 16;
		temp[0] = n & BMASK;
		temp[1] = n >> BYTE;
		temp[2] = 0;
		fprintf(stderr, "troff: Oops, still finding PONTPOS, save the files and warn jaap\n");
		ptfpcmd(0, temp);
		return(outsize);
	}
	xbitf = 2;
	if (sfbits(i) == oldbits) {
		xfont = pfont;
		xpts = ppts;
		xbitf = 0;
	} else 
		xbits(i);
	if (k < 040 && k != DRAWFCN)
		return(outsize);
	w = getcw(k - 32);
	j = z = 0;
	if (k != DRAWFCN) {
		if (cs) {
			if (bd)
				w += (bd - 1) * HOR;
			j = (cs - w) / 2;
			w = cs - j;
			if (bd)
				w -= (bd - 1) * HOR;
		}
		if (iszbit(i)) {
			if (cs)
				w = -j; 
			else 
				w = 0;
			z = 1;
		}
	}
	esc += j;
	if (xfont != mfont)
		ptfont();
	if (xpts != mpts)
		ptps();
	if (lead)
		ptlead();
	/* put out the real character here */
	if (k == DRAWFCN) {
		if (esc)
			ptesc();
		dx = absmot(pi[3]);
		if (isnmot(pi[3]))
			dx = -dx;
		dy = absmot(pi[4]);
		if (isnmot(pi[4]))
			dy = -dy;
		switch (cbits(pi[1])) {
		case DRAWCIRCLE:	/* circle */
			fprintf(ptid, "D%c %d\n", DRAWCIRCLE, dx);	/* dx is diameter */
			w = 0;
			hpos += dx;
			break;
		case DRAWELLIPSE:
			fprintf(ptid, "D%c %d %d\n", DRAWELLIPSE, dx, dy);
			w = 0;
			hpos += dx;
			break;
		case DRAWLINE:	/* line */
			k = cbits(pi[2]);
			fprintf(ptid, "D%c %d %d ", DRAWLINE, dx, dy);
			if (k < 128)
				fprintf(ptid, "%c\n", k);
			else
				fprintf(ptid, "%s\n", &chname[chtab[k - 128]]);
			w = 0;
			hpos += dx;
			vpos += dy;
			break;
		case DRAWARC:	/* arc */
			dx2 = absmot(pi[5]);
			if (isnmot(pi[5]))
				dx2 = -dx2;
			dy2 = absmot(pi[6]);
			if (isnmot(pi[6]))
				dy2 = -dy2;
			fprintf(ptid, "D%c %d %d %d %d\n", DRAWARC,
				dx, dy, dx2, dy2);
			w = 0;
			hpos += dx + dx2;
			vpos += dy + dy2;
			break;
		case DRAWWIG:	/* wiggly line */
			fprintf(ptid, "D%c %d %d", DRAWWIG, dx, dy);
			w = 0;
			hpos += dx;
			vpos += dy;
			for (n = 5; cbits(pi[n]) != '.'; n += 2) {
				dx = absmot(pi[n]);
				if (isnmot(pi[n]))
					dx = -dx;
				dy = absmot(pi[n+1]);
				if (isnmot(pi[n+1]))
					dy = -dy;
				fprintf(ptid, " %d %d", dx, dy);
				hpos += dx;
				vpos += dy;
			}
			fprintf(ptid, "\n");
			break;
		}
		for (n = 3; cbits(pi[n]) != '.'; n++)
			;
		outsize = n + 1;
	} else if (k < 128) {
		/* try to go faster and compress output */
		/* by printing nnc for small positive motion followed by c */
		/* kludgery; have to make sure set all the vars too */
		if (esc > 0 && esc < 100) {
			oput(esc / 10 + '0');
			oput(esc % 10 + '0');
			oput(k);
			hpos += esc;
			esc = 0;
		} else {
			if (esc)
				ptesc();
			fprintf(ptid, "c%c\n", k);
		}
	} else {
		if (esc)
			ptesc();
		fprintf(ptid, "C%s\n", &chname[chtab[k - 128]]);
	}
	if (bd) {
		bd -= HOR;
		if (esc += bd)
			ptesc();
		if (k < 128) {
			fprintf(ptid, "c%c\n", k);
		} else
			fprintf(ptid, "C%s\n", &chname[chtab[k - 128]]);
		if (z)
			esc -= bd;
	}
	esc += w;
	return(outsize);
}

ptps()
{
	register i, j, k;

	i = xpts;
	for (j = 0; i > (k = pstab[j]); j++)
		if (!k) {
			k = pstab[--j];
			break;
		}
	fprintf(ptid, "s%d\n", k);	/* really should put out string rep of size */
	mpts = i;
}

ptfont()
{
	mfont = xfont;
	if( xfont > nfonts) {
		char temp[3];	/* TODO: make a routine to convert */
		temp[0] = fontlab[xfont] & BMASK;	/* a name */
		temp[1] = fontlab[xfont] >> BYTE;	/* like */
		temp[2] = 0;				/* this */

		ptfpcmd(0, temp);	/* Put the desired font in the
					 * fontcache of the filter */
		fprintf(ptid, "f0\n");	/* make sure that it gets noticed */
	} else
		fprintf(ptid, "f%d\n", xfont);
}

ptfpcmd(f, s)
int	f;
char	*s;
{
	if (ascii)
		return;
	fprintf(ptid, "x font %d %s\n", f, s);
}

ptlead()
{
	vpos += lead;
	if (!ascii)
		fprintf(ptid, "V%d\n", vpos);
	lead = 0;
}

ptesc()
{
	hpos += esc;
	if (esc > 0)
		fprintf(ptid, "h%d", esc);
	else
		fprintf(ptid, "H%d\n", hpos);
	esc = 0;
}

newpage(n)	/* called at end of each output page (we hope) */
{
	ptlead();
	vpos = 0;
	if (ascii)
		return;
	flusho();
	fprintf(ptid, "p%d\n", n);	/* new page */
	ptps();
	ptfont();
}

pttrailer()
{
	fprintf(ptid, "x trailer\n");
}

ptstop()
{
	fprintf(ptid, "x stop\n");
}

dostop()
{
	if (ascii)
		return;
	ptlead();
	vpos = 0;
	/*	fprintf(ptid, "x xxx end of page\n");*/
	if (!nofeed)
		pttrailer();
	ptlead();
	fprintf(ptid, "x pause\n");
	flusho();
	mpts = mfont = 0;
	paper = 0;
	esc = T_IESC;	/* this is a dreg */
	ptesc();
	esc = po;
	hpos = vpos = 0;	/* probably in wrong place */
}
