#include <X/mit-copyright.h>

/* Copyright 1985 by the Massachusetts Institute of Technology */
/*
 * Parse and unparse ANSI sequences.
 */
#include	<stdio.h>
#include	<X/Xlib.h>
#include	"ptyx.h"

parseseq(c, ap, tek)
register int	c;
register ANSI	*ap;
{
	/*
	 * Start up sequence if this is a sequence introducer.
	 */
	if (c==ESC || c==DCS || c==CSI || c==OSC || c==PM || c==APC) {
		ap->a_type = c;
		ap->a_pintro = 0;
		ap->a_final = 0;
		ap->a_inters = 0;
		ap->a_nparam = 0;
		ap->a_nastyf = 0;
		return (-1);
	}
	/*
	 * Just pass the character through if not in a sequence.
	 */
	if (ap->a_type == 0)
		return (c);
	/*
	 * Abort on C1 control, cancel or random communication error.
	 */
	if ((c>=0x80 && c<=0x9F)) {
		ap->a_type = 0;
		ap->a_pintro = 0;
		ap->a_final = 0;
		return (c);
	}
	/*
	 * C0 control characters are passed through in the middle,
	 * except for a couple of special ones in Tek mode.
	 */
	if (c<0x20 || c==DEL || c==RDEL) {
		if (tek
		    && ap->a_pintro==0
		    && ap->a_final==0
		    && ap->a_inters==0
		    && ap->a_nparam==0
		    && ap->a_nastyf==0) {
			if (ap->a_type==ESC && (c==FF || c==SUB || c==INQ))
				goto finalchar;
			if (ap->a_type==CSI && c==US)
				goto finalchar;
		}
		if (c==CAN || c==SUB) {
			ap->a_type = 0;
			ap->a_pintro = 0;
			ap->a_final = 0;
		}
		return (c);
	}
	c &= 0x7F;
	/*
	 * Intermediate characters.
	 */
	if (c>=0x20 && c<=0x2F) {
		ap->a_inters <<= 8;
		ap->a_inters |= c;
		return (-1);
	}
	/*
	 * Parameter character in DCS, CSI, et al.
	 */
	if (ap->a_type!=ESC && c>=0x30 && c<=0x3F) {
		if (ap->a_inters != 0)
			ap->a_nastyf = 1;
		/*
		 * Only "?" and "<" are legal at Digital.
		 */
		if (c >= 0x3C) {
			if (ap->a_pintro != 0 || ap->a_nparam != 0)
				ap->a_nastyf = 1;
			ap->a_pintro = c;
			return (-1);
		}
		/*
		 * set the default flag and init param value to 0
		 * if we haven't seen any param's yet.
		 */
		if (ap->a_nparam == 0) {
			ap->a_dflt[ap->a_nparam] = 1;
			ap->a_param[ap->a_nparam++] = 0;
		}
		/*
		 * ";" or ":", only the ";" is legal at Digital.
		 */
		if (c >= 0x3A) {
			if (c != 0x3B || ap->a_nparam >= NPARAM)
				ap->a_nastyf = 1;
			else {
				ap->a_dflt[ap->a_nparam] = 1;
				ap->a_param[ap->a_nparam++] = 0;
			}
			return (-1);
		}
		/*
		 * Part of a numeric parameter.
		 * Clear the default flag for this param.
		 */
		ap->a_dflt[ap->a_nparam-1] = 0;
		ap->a_param[ap->a_nparam-1] *= 10;
		ap->a_param[ap->a_nparam-1] += c - 0x30;
		return (-1);
	}
	/*
	 * C1 control in its 7 bit ESC Fe representation.
	 */
	if (ap->a_type==ESC && ap->a_inters==0 && c>=0x40 && c<=0x5F) {
		c += 0x40;
		/*
		 * May need to start up a new sequence.
		 */
		if (c!=DCS && c!=CSI && c!=OSC && c!=PM && c!=APC) {
			ap->a_type = 0;
			ap->a_pintro = 0;
			ap->a_final = 0;
			return (c);
		}
		ap->a_type  = c;
		ap->a_pintro = 0;
		ap->a_final = 0;
		ap->a_nastyf = 0;
		return (-1);
	}
	/*
	 * Final character in sequence.
	 */
finalchar:
	ap->a_final = c;
	return (ap->a_type);
}

unparseseq(ap, tx8flag, fd)
register ANSI	*ap;
{
	register int	c;
	register int	i;
	register int	inters;

	c = ap->a_type;
	if (c>=0x80 && c<=0x9F && tx8flag==0) {
		unparseputc(ESC, fd);
		c -= 0x40;
	}
	unparseputc(c, fd);
	c = ap->a_type;
	if (c==ESC || c==DCS || c==CSI || c==OSC || c==PM || c==APC) {
		if (ap->a_pintro != 0)
			unparseputc(ap->a_pintro, fd);
		for (i=0; i<ap->a_nparam; ++i) {
			if (i != 0)
				unparseputc(';', fd);
			if (ap->a_param[i] != 0)
				unparseputn(ap->a_param[i], fd);
		}
		inters = ap->a_inters;
		for (i=3; i>=0; --i)
			c = (inters >> (8*i)) & 0xff;
			if (c != 0)
				unparseputc(c, fd);
		unparseputc(ap->a_final, fd);
	}
}

unparseputn(n, fd)
unsigned int	n;
{
	unsigned int	q;

	q = n/10;
	if (q != 0)
		unparseputn(q, fd);
	unparseputc((n%10) + '0', fd);
}

unparseputc(c, fd)
{
	char	buf[1];

	buf[0] = c;
	if (write(fd, buf, 1) != 1)
		Panic("unparseputc: error writing character\n", 0);
}
