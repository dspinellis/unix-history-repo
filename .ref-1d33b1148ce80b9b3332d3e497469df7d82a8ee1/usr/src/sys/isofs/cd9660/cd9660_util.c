/*-
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley
 * by Pace Willisson (pace@blitz.com).  The Rock Ridge Extension
 * Support code is derived from software contributed to Berkeley
 * by Atsushi Murai (amurai@spec.co.jp).
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cd9660_util.c	8.3 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/namei.h>
#include <sys/resourcevar.h>
#include <sys/kernel.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/conf.h>
#include <sys/mount.h>
#include <sys/vnode.h>
#include <miscfs/specfs/specdev.h> /* XXX */
#include <miscfs/fifofs/fifo.h> /* XXX */
#include <sys/malloc.h>
#include <sys/dir.h>

#include <isofs/cd9660/iso.h>

/*
 * translate and compare a filename
 * Note: Version number plus ';' may be omitted.
 */
int
isofncmp(fn, fnlen, isofn, isolen)
	u_char *fn, *isofn;
	int fnlen, isolen;
{
	int i, j;
	char c;
	
	while (--fnlen >= 0) {
		if (--isolen < 0)
			return *fn;
		if ((c = *isofn++) == ';') {
			switch (*fn++) {
			default:
				return *--fn;
			case 0:
				return 0;
			case ';':
				break;
			}
			for (i = 0; --fnlen >= 0; i = i * 10 + *fn++ - '0') {
				if (*fn < '0' || *fn > '9') {
					return -1;
				}
			}
			for (j = 0; --isolen >= 0; j = j * 10 + *isofn++ - '0');
			return i - j;
		}
		if (c != *fn) {
			if (c >= 'A' && c <= 'Z') {
				if (c + ('a' - 'A') != *fn) {
					if (*fn >= 'a' && *fn <= 'z')
						return *fn - ('a' - 'A') - c;
					else
						return *fn - c;
				}
			} else
				return *fn - c;
		}
		fn++;
	}
	if (isolen > 0) {
		switch (*isofn) {
		default:
			return -1;
		case '.':
			if (isofn[1] != ';')
				return -1;
		case ';':
			return 0;
		}
	}
	return 0;
}

/*
 * translate a filename
 */
void
isofntrans(infn, infnlen, outfn, outfnlen, original, assoc)
	u_char *infn, *outfn;
	int infnlen;
	u_short *outfnlen;
	int original;
	int assoc;
{
	int fnidx = 0;
	
	if (assoc) {
		*outfn++ = ASSOCCHAR;
		fnidx++;
		infnlen++;
	}
	for (; fnidx < infnlen; fnidx++) {
		char c = *infn++;
		
		if (!original && c >= 'A' && c <= 'Z')
			*outfn++ = c + ('a' - 'A');
		else if (!original && c == '.' && *infn == ';')
			break;
		else if (!original && c == ';')
			break;
		else
			*outfn++ = c;
	}
	*outfnlen = fnidx;
}
