/*	cons.c	1.2	89/04/25	*/

/*
 * Asynchronous versions of getchar and gets
 * for use in disk formatter.
 */
#include "machine/mtpr.h"

#include "param.h"
#include "../tahoe/cp.h"

int	wait_for_char = 1;		/* synchronous by default */
extern	struct	cpdcb_i cpin;		/* in ../prf.c */

agetchar()
{
	static	char_read = 0;		/* read operation pending */
	char	c;

	if (!char_read) {
		char_read = 1;
		cpin.cp_hdr.cp_unit = CPCONS;	/* Resets done bit */
		cpin.cp_hdr.cp_comm = CPREAD;
		cpin.cp_hdr.cp_count = 1;
		mtpr(CPMDCB, &cpin);
	}
	uncache(&cpin.cp_hdr.cp_unit);
	if (wait_for_char) {
		while ((cpin.cp_hdr.cp_unit & CPDONE) == 0) 
			uncache(&cpin.cp_hdr.cp_unit);
	} else
		if ((cpin.cp_hdr.cp_unit & CPDONE) == 0)
			return (0);
	uncache(&cpin.cpi_buf[0]);
	c = cpin.cpi_buf[0] & 0x7f;
	char_read = 0;
	if (c == '\r')
		c = '\n';
	if (c != '\b' && c != '\177')
		putchar(c);
	return (c);
}

agets(buf)
	char *buf;
{
	static char	line[256];
	static char	*lp = line;
	register	c;

	*buf = '\0';
	for (;;) {
		c = agetchar() & 0177;
		switch(c) {
		case '\0' :
			if (!wait_for_char)
				return;
			break;
		case '\n':
		case '\r':
			c = '\n';
			*lp = '\0';
			strcpy(buf, line);
			lp = line;
			return;
		case '\b':
		case '\177':
			if (lp > line) {
				lp--;
				putchar('\b');
				putchar(' ');
				putchar('\b');
			}
			continue;
		case '#':
			lp--;
			if (lp < line)
				lp = line;
			continue;
		case '@':
		case 'u'&037:
			lp = line;
			putchar('\n');
			continue;
		default:
			if ((lp - line) < 256)
				*lp++ = c;
		}
	}
}
