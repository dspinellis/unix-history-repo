#ifndef lint
static char sccsid[] = "@(#)io.c	4.2 (Berkeley) %G%";
#endif

/*
 * io.c: font file I/O subroutines for fed.
 */

#include "fed.h"

getglyph(c)
char c;
{
	register int i, j;
	int windno;
	int vertoff, horoff;
	char *tmp;

	if (trace)
		fprintf(trace, "\n\ngetglyph(%s)\n", rdchar(c));
	if (disptable[c].nbytes == 0) {
		if (trace)
			fprintf(trace, "no such char: %s\n", rdchar(c));
		sprintf(msgbuf, "no such character: %s", rdchar(c));
		message(msgbuf);
		return;
	}

	curchar = c;
	turnofcurs();
	if (cht[curchar].wherewind >= 0) {
		/* It's already in a window.  Don't have to do much. */
		if (trace)
			fprintf(trace, "already in %d\n", cht[curchar].wherewind);
		windno = cht[curchar].wherewind;
		/* Put a box around the current window */
		if (windno != curwind) {
			drawbox(base[curwind].r-1, base[curwind].c-1, 0, GLROW+2, GLCOL+2);
			drawbox(base[windno].r-1, base[windno].c-1, 1, GLROW+2, GLCOL+2);
		}
		curwind = windno;
		syncwind(windno);
		/* should center base */
	} else {
		/*
		 * Not on screen.  First find a suitable window,
		 * using round robin.
		 */
		windno = nextwind;
		if (trace)
			fprintf(trace, "chose window %d\n", windno);
		if (++nextwind >= NWIND)
			nextwind = 0;
#ifdef TWOWIND
		/*
		 * This is for debugging what happens when we run out
		 * of windows.
		 */
		if (nextwind >= 2)
			nextwind = 0;
#endif

		/* Put a box around the current window */
		if (windno != curwind) {
			if (trace)
				fprintf(trace, "drawbox (%d %d)\n", base[windno].r-1, base[windno].c-1);
			drawbox(base[curwind].r-1, base[curwind].c-1, 0, GLROW+2, GLCOL+2);
			drawbox(base[windno].r-1, base[windno].c-1, 1, GLROW+2, GLCOL+2);
		}

		/* Print the char at the lower left of the window */
		sprintf(msgbuf, "%s", rdchar(curchar));
		dispmsg(msgbuf, base[windno].c, base[windno].r-11, 2);
		
		/* Now make room in the window */
		if (wind[windno].onscreen == NULL) {
			/* Brand new window, have to allocate space */
			wind[windno].onscreen = newmat(GLROW, GLCOL);
		} else {
			/* Save prev glyph for later */
			cht[wind[windno].used].whereat = wind[windno].val;
			cht[wind[windno].used].wherewind = -2;
			if (trace)
				fprintf(trace, "windno=%s, wind[windno].used=%d, cht[..].wherewind set to -2\n", rdchar(windno), wind[windno].used);
		}
		if (wind[windno].undval != NULL) {
			if (trace)
				fprintf(trace, "getglyph frees undo: %x\n", wind[windno].undval);
			free(wind[windno].undval);
		}
		wind[windno].undval = NULL;
		wind[windno].used = curchar;

		/*
		 * Vertical & horizontal offsets.  Line up the baseline
		 * of the char at BASELINE from bottom, but center
		 * horizontally.
		 */
		vertoff = GLROW - BASELINE - disptable[curchar].up;
		/* Check to see if the glyph is being nosed off the edge. */
		if (vertoff < 0) {
			vertoff = 0;
		} else if (vertoff + disptable[curchar].up + disptable[curchar].down >= GLROW) {
			vertoff = GLROW - disptable[curchar].up - disptable[curchar].down;
		}
		horoff = (GLCOL-(disptable[curchar].left+disptable[curchar].right)) / 2;
		wind[windno].val = findbits(curchar, GLROW, GLCOL, horoff, vertoff, &curs_r, &curs_c);
		cht[curchar].rcent = curs_r;
		cht[curchar].ccent = curs_c;
		curwind = windno;
		cht[curchar].wherewind = windno;
		syncwind(windno);
	}
}

/*
 * writeback: write the font back to the file at the end of editing.
 * Also have to write width table.
 */
writeback()
{
	writefont(fontfile);
}

/*
 * writefont: write current font on file fname.
 */
writefont(fname)
char *fname;
{
	register int i, j;
	register int c;
	FILE *fntout;
	int bytes;
	bitmat tmp;
	int nextoff = 0;
	int charcount, bytecount;
	extern char *sys_errlist[];
	extern int errno;

	if (trace)
		fprintf(trace, "writefont(%s)\n", fname);
	/*
	 * The following unlink is important because we are about to
	 * do an fopen( , "w") on fname.  We still have fontdes open
	 * for reading.  If we don't do the unlink the fopen will truncate
	 * the file and subsequent reads will fail.  If we do the unlink
	 * the file won't go away until it is closed, so we can still
	 * read from the old version.
	 */
	if (strcmp(fname, fontfile)==0 && unlink(fname) < 0) {
		sprintf(msgbuf, "unlink %s: %s", fname, sys_errlist[errno]);
		error(msgbuf);
	}

	fntout = fopen(fname, "w");
	if (fntout == NULL) {
		sprintf(msgbuf, "%s: %s", fname, sys_errlist[errno]);
		if (trace)
			fprintf(trace, "%s\n", msgbuf);
		error(msgbuf);
	}
	sprintf(msgbuf, "\"%s\"", fname);
	message(msgbuf);
	fflush(stdout);

	fwrite(&FontHeader, sizeof FontHeader, 1, fntout);
	fwrite(&disptable[0], sizeof disptable, 1, fntout);
	charcount = 0; bytecount = fbase;
	for (c=0; c<256; c++)
		if (disptable[c].nbytes || cht[c].wherewind != -1) {
			if (trace)
				fprintf(trace, "char %s, nbytes %d, wherewind %d.. ", rdchar(c), disptable[c].nbytes, cht[c].wherewind);
			packmat(c, &tmp, &bytes);
			disptable[c].addr = nextoff;
			disptable[c].nbytes = bytes;
			if (trace)
				fprintf(trace, "offset %d size %d\n", nextoff, bytes);
			nextoff += bytes;
			fwrite(tmp, bytes, 1, fntout);
			charcount++;
			bytecount += bytes;
		}
	FontHeader.size = nextoff;
	fseek(fntout, 0L, 0);
	fwrite(&FontHeader, sizeof FontHeader, 1, fntout);
	fwrite(&disptable[0], sizeof disptable, 1, fntout);

	/* Should fix the width tables here */
	fclose(fntout);
	sprintf(msgbuf, "%s %d glyphs, %d bytes", fname, charcount, bytecount);
	message(msgbuf);
	changes = 0;
}

/*
 * make a packed matrix of the bits for char c.
 * return the matrix ptr in result and the size in bytes in nbytes.
 */
packmat(c, result, nbytes)
int c;
bitmat *result;
int *nbytes;
{
	register int i, j;
	bitmat wp;
	int nb, nr, nc;
	int rmin, cmin, rmax, cmax;
	static char tmp[WINDSIZE];

	if (cht[c].wherewind == -1) {
		/* It has never been read from file.  Just copy from file. */
		nb = disptable[c].nbytes;
		fseek(fontdes, (long) fbase+disptable[c].addr, 0);
		fread(tmp, nb, 1, fontdes);
	} else {
		if (cht[c].wherewind == -2)
			wp = cht[c].whereat;
		else
			wp = wind[cht[c].wherewind].val;
		minmax(wp, GLROW, GLCOL, &rmin, &cmin, &rmax, &cmax);
		nr = rmax-rmin+1; nc = cmax-cmin+1;
		zermat(tmp, nr, nc);
		for (i=rmin; i<=rmax; i++)
			for (j=cmin; j<=cmax; j++) {
				setmat(tmp, nr, nc, i-rmin, j-cmin,
					mat(wp, GLROW, GLCOL, i, j));
			}
		nb = ((nc + 7) >> 3) * nr;
		disptable[c].up = cht[c].rcent - rmin;
		disptable[c].down = rmax - cht[c].rcent + 1;
		disptable[c].left = cht[c].ccent - cmin;
		disptable[c].right = cmax - cht[c].ccent + 1;
		if (trace) {
			fprintf(trace, "rmax=%d, rcent=%d, rmin=%d, cmax=%d, ccent=%d, cmin=%d, ", rmax, cht[c].rcent, rmin, cmax, cht[c].ccent, cmin);
			fprintf(trace, "up=%d, down=%d, left=%d, right=%d\n", disptable[c].up, disptable[c].down, disptable[c].left, disptable[c].right);
		}
	}
	*result = tmp;
	*nbytes = nb;
	if (trace)
		fprintf(trace, "nbytes = %d, ", nb);
	return;
}

/*
 * editfont: make the file fname be the current focus of attention,
 * including reading it into the buffer.
 */
editfont(fname)
char *fname;
{
	register char *cp;

	clearfont();
	editing = 1;
	truename(fname, fontfile);
	fontdes = fopen(fontfile, "r");
	readfont(fontfile, 0, 255);

	/*
	 * Figure out the point size, and make a guess as to the
	 * appropriate width of the heavy pen.
	 */
	for (cp=fontfile; *cp && *cp!='.'; cp++)
		;
	if (*cp) {
		pointsize = atoi(++cp);
		setpen(pointsize>30?3 : pointsize>15?2 : pointsize>8?1 : 0);
	} else {
		pointsize = 0;
		setpen(2);
	}
}

/*
 * readfont: read in a font, overlaying the current font.
 * also used to edit a font by clearing first.
 *
 * Conflicts are handled interactively.
 */
readfont(fname, c1, c2)
char *fname;
int c1, c2;
{
	register int i;
	register char *cp;
	struct dispatch d;
	char choice, mode = 0;
	FILE *hold_fontdes;
	int horoff, vertoff;
	long ftsave;

	hold_fontdes = fontdes;
	fontdes = fopen(fname, "r");
	if (fontdes == NULL) {
		sprintf(msgbuf, "%s not found", fname);
		fontdes = hold_fontdes;
		error(msgbuf);
	}
	fread(&FontHeader, sizeof FontHeader, 1, fontdes);
	fseek(fontdes, c1*sizeof d, 1);	/* skip over unread chars */
	ftsave = ftell(fontdes);
	for (i=c1; i<=c2; i++) {
		fseek(fontdes, ftsave, 0);
		fread(&d, sizeof d, 1, fontdes);
		ftsave = ftell(fontdes);
		/* Decide which of the two to take */
		if (d.nbytes == 0)
			continue;	/* We take the one in the buffer */
		if (disptable[i].nbytes > 0) {
			/* Conflict */
			switch(mode) {
			case 'f':
				/* fall through */
				break;
			case 'b':
				continue;
			default:
				sprintf(msgbuf, "%s <file> or <buffer>", rdchar(i));
				message(msgbuf);
				choice = inchar();
				switch(choice) {
				case 'F':
					mode = 'f';
				default:
				case 'f':
					break;
				case 'B':
					mode = 'b';
				case 'b':
					continue;
				}
			}
		}
		disptable[i] = d;	/* We take the one in the file */
		cht[i].nrow = d.up + d.down;
		cht[i].ncol = d.left + d.right;
		if (!editing && disptable[i].nbytes) {
			horoff = (GLCOL-(disptable[i].left+disptable[i].right))/2;
			vertoff = GLROW - BASELINE - disptable[i].up;
			/* Check to see if the glyph is being nosed off the edge. */
			if (vertoff < 0) {
				vertoff = 0;
			} else if (vertoff + disptable[curchar].up + disptable[curchar].down >= GLROW) {
				vertoff = GLROW - disptable[curchar].up - disptable[curchar].down;
			}
			if (cht[i].wherewind >= 0) {
				/* The old glyph is in a window - destroy it */
				wind[cht[i].wherewind].used = -1;
			}
			cht[i].wherewind = -1;
			cht[i].whereat = findbits(i, GLROW, GLCOL, horoff, vertoff, &cht[i].rcent, &cht[i].ccent);
			cht[i].wherewind = -2;
			if (trace)
				fprintf(trace, "setting cht[%d].wherewind to -2 in readfont\n", i);
		} else
			cht[i].wherewind = -1;
	}
	fbase = sizeof FontHeader + sizeof disptable;	/* ftell(fontdes) */

	sprintf(msgbuf, "\"%s\", raster data %d bytes, width %d, height %d, xtend %d", fname, FontHeader.size, FontHeader.maxx, FontHeader.maxy, FontHeader.xtend);

	fontdes = hold_fontdes;
	message(msgbuf);
}

/*
 * Figure out the true name of the font file, given that
 * the abbreviated name is fname.  The result is placed
 * in the provided buffer result.
 */
truename(fname, result)
char *fname;
char *result;
{
	FILE *t;

	strcpy(result, fname);
	if ((t = fopen(result, "r")) == NULL) { 
		sprintf(result,"/usr/lib/vfont/%s",fname);
		if ((t = fopen(result, "r")) == NULL) { 
			strcpy(result, fname);
			sprintf(msgbuf, "Can't find %s\n",fname);
			error(msgbuf);
		}
	}
	fclose(t);
}


/*
 * clearfont: delete all characters in the current font.
 */
clearfont()
{
	register int i;

	if (fontdes)
		fclose(fontdes);
	for (i=0; i<256; i++) {
		cht[i].wherewind = -1;
		disptable[i].addr = 0;
		disptable[i].nbytes = 0;
		disptable[i].up = 0;
		disptable[i].down = 0;
		disptable[i].left = 0;
		disptable[i].right = 0;
		disptable[i].width = 0;
	}
}

/*
 * fileiocmd: do a file I/O command.  These all take optional file
 * names, defaulting to the current file.
 */
fileiocmd(cmd)
char cmd;
{
	char fname[100], truefname[100];

	readline("file: ", fname, sizeof fname);
	if (fname[0] == 0 || fname[0] == ' ')
		strcpy(fname, fontfile);
	switch(cmd) {
	case 'E':
		confirm();
		editfont(fname);
		break;

	case 'N':
		if (changes)
			writeback();
		editfont(fname);
		break;

	case 'R':
		editing = 0;
		truename(fname, truefname);
		readfont(truefname, 0, 255);
		changes++;
		break;

	case 'W':
		editing = 0;
		writefont(fname);
		break;
	}
	if (editing)
		changes = 0;
}

/*
 * readchars: read in a partial font (the P command).
 */
readchars()
{
	int c1, c2;
	char fnamebuf[100];
	char truebuf[100];
	char buf[5];

	message("Partial read <firstchar>");
	c1 = inchar();
	sprintf(msgbuf, "Partial read %s thru <lastchar>", rdchar(c1));
	message(msgbuf);
	c2 = inchar();
	strcpy(buf, rdchar(c1));
	sprintf(msgbuf, "Partial read %s thru %s from file: ", buf, rdchar(c2));
	readline(msgbuf, fnamebuf, sizeof fnamebuf);
	editing = 0;
	if (fnamebuf[0] == 0 || fnamebuf[0] == ' ')
		strcpy(fnamebuf, fontfile);
	truename(fnamebuf, truebuf);
	changes++;
	readfont(truebuf, c1, c2);
}
