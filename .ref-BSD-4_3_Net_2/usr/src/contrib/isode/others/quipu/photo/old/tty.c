/* tty.c - display on any terminal */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/others/quipu/photo/RCS/tty.c,v 7.0 89/11/23 22:01:49 mrose Rel $";
#endif

/* 
 * $Header: /f/osi/others/quipu/photo/RCS/tty.c,v 7.0 89/11/23 22:01:49 mrose Rel $
 *
 *
 * $Log:	tty.c,v $
 * Revision 7.0  89/11/23  22:01:49  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */



#include "stdio.h"
#include "quipu/photo.h"

#define GREYSCALE "#OI&\\*o=_\"-;:,.  "

char    mapping[16];
char * strcpy ();

char    buffer[500][500];
int     lineno = 0,
        pos = 0;
extern int PIC_LINESIZE;
extern int NUMLINES;
int     scale = 8;
int     invert = 0;
int     equal = 0;
int     ln = 0;
int     scalediv = 0;
int     edges = 0;

char    level[128];
char    show[128];
char    display[50][50];

/* ARGSUSED */
photo_start (name)
char   *name;
{
	char   *ptr,
	       *getenv ();


	if ((ptr = getenv ("photo_invert")) != (char *) NULL)
		if (strcmp (ptr, "true") == 0)
			invert = 1;

	if ((ptr = getenv ("photo_equal")) != (char *) NULL) {
		if (strcmp (ptr, "true") == 0)
			equal = 1;
		else if (strcmp (ptr, "edge") == 0) {
			int     i,
			        j;

			for (i = 1; i < PIC_LINESIZE; i++)
				for (j = 1; j < NUMLINES; j++)
					buffer[i][j] = 0;
			edges = 1;
		}
	}
	if ((ptr = getenv ("photo_mapping")) != (char *) NULL)
		(void) strcpy (mapping, ptr);
	else
		(void) strcpy (mapping, GREYSCALE);

	if ((ptr = getenv ("photo_scale")) != (char *) NULL) {
		if (strcmp (ptr, "large") == 0)
			scale = 1;
		else if (strcmp (ptr, "small") == 0)
			scale = 8;
		else if (strcmp (ptr, "medium") == 0)
			scale = 4;
	}
	scalediv = scale * scale / 8;

	(void) printf ("\n");
	return (0);
}

/* ARGSUSED */
photo_end (name)
char   *name;
{
	int     i,
	        j,
	        k;
	int     numlev;
	int     total = 0;
	int     totlevel = 0;
	int     cnt;

	if (equal) {
		numlev = scale * scale * 2;
		for (i = 0; i < numlev; i++)
			level[i] = 0;

		for (i = 0; i < ln; i++)
			for (j = 0; j < PIC_LINESIZE / scale; j++)
				level[display[i][j]]++;

		for (i = 0; i < numlev; i++)
			totlevel += level[i];

		for (i = 0; i < numlev; i++) {
			total += level[i];
			show[i] = (total * 16) / totlevel;
			if (show[i] >= 16)
				show[i] = 15;
		}

		for (i = 0; i < ln; i++) {
			for (j = 0; j < PIC_LINESIZE / scale; j++) {
				if (invert)
					(void) putc (mapping[show[display[i][j]]],stdout);
				else
					(void) putc (mapping[15 - show[display[i][j]]],stdout);
			}
			(void) printf ("\n");
		}
	} else if (edges) {
		/* edges by expansion */
		char    ebuf[500][500];

		for (i = 1; i < PIC_LINESIZE; i++)
			for (j = 1; j < NUMLINES; j++)
				ebuf[i][j] = 0;

		for (i = 1; i < PIC_LINESIZE; i++)
			for (j = 1; j < NUMLINES; j++)
				if (buffer[i][j] == 1) {
					ebuf[i - 1][j - 1] = 1;
					ebuf[i - 1][j] = 1;
					ebuf[i - 1][j + 1] = 1;
					ebuf[i][j - 1] = 1;
					ebuf[i][j + 1] = 1;
					ebuf[i + 1][j - 1] = 1;
					ebuf[i + 1][j] = 1;
					ebuf[i + 1][j + 1] = 1;
				}
		for (i = 1; i < PIC_LINESIZE; i++)
			for (j = 1; j < NUMLINES; j++)
				if (buffer[i][j] == 1)
					ebuf[i][j] = 0;

		for (lineno = 0; lineno < NUMLINES; lineno += (2 * scale)) {
			for (k = 0; k < PIC_LINESIZE; k += scale) {
				cnt = 0;
				for (i = k; i < k + scale; i++)
					for (j = lineno; j < (2 * scale) + lineno; j++)
						cnt += ebuf[i][j];

/* Need to select a grey level on the strength of the edge
 *
 *				cnt *= 4;
 *				if (cnt == (scalediv * 16))
 *					cnt = (scalediv * 16) -1;
 *
 * Just set "strong" edge cells "on" for now ...
 */
				if (cnt > (scalediv * 2) )
					cnt = (scalediv * 16) -1;
				else 
					cnt = 0;	

				if (invert)
					(void) putc (mapping[(cnt / scalediv)],stdout);
				else
					(void) putc (mapping[15 - (cnt / scalediv)],stdout);
			}
			(void) putc ('\n',stdout);
		}

	}
	return (0);

}

photo_black (length)
int     length;
{
	int     i;

	if (scale == 1) {
		for (i = pos; i < length + pos; i++)
			if (invert)
				(void) putc (' ',stdout);
			else
				(void) putc ('#',stdout);
		return;
	}
	for (i = pos; i < length + pos; i++)
		buffer[i][lineno] = 1;
	pos += length;
}

photo_white (length)
int     length;
{
	int     i;

	if (scale == 1) {
		for (i = pos; i < length + pos; i++)
			if (invert)
				(void) putc ('#',stdout);
			else
				(void) putc (' ',stdout);
		return;
	}
	for (i = pos; i < length + pos; i++)
		buffer[i][lineno] = 0;
	pos += length;
}

/* ARGSUSED */
photo_line_end (line)
bit_string *line;
{
	int     i,
	        j,
	        k,
	        cnt;

	if (scale == 1) {
		(void) putc ('\n',stdout);
		return;
	}
	lineno++;
	pos = 0;

	if (edges)
		return;

	if (lineno >= 2 * scale) {
		ln++;
		lineno = 0;
		for (k = 0; k < PIC_LINESIZE; k += scale) {
			cnt = 0;
			for (i = k; i < k + scale; i++)
				for (j = 0; j < 2 * scale; j++)
					cnt += buffer[i][j];

			if (equal) {
				display[ln][k / scale] = cnt;
				continue;
			}
			if (cnt == (scalediv * 16))
				cnt--;


			if (invert)
				(void) putc (mapping[cnt / scalediv],stdout);
			else
				(void) putc (mapping[15 - (cnt / scalediv)],stdout);
		}

		if (!equal)
			(void) putc ('\n',stdout);
	}
}
