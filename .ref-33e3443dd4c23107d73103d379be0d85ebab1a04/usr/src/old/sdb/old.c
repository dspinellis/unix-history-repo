static	char sccsid[] = "@(#)old.c 4.2 %G%";
/*
 * sdb - a symbolic debugger for UNIX.
 */

/*
 * This file contains support routines for older versions of the system.
 */

#ifndef VMUNIX
/*
 * These routines are used only if the system
 * doesn't have virtual memory.  They
 * are used only to read the symbol table, which
 * is simply kept in VM on VMUNIX.
 */
#include <pagsiz.h>
#include "bio.h"

bread(brs, buff, nbytes)
struct brbuf *brs; char *buff; {
	register int k, nb;

	if (nbytes > 0) {
		for (nb=nbytes; nb>0; nb--) {
			if (brs->nr == 0) {
				brs->nr = read(brs->fd, brs->next=brs->b, BSIZE);
				brs->nl = 0;
				if (brs->nr < 0) return(-1);
				if (brs->nr == 0) return(nbytes-nb);
				}
			*buff++ = *brs->next++;
			brs->nr--;
			brs->nl++;
			}
		}
	else {
		nbytes = -nbytes;
		for (nb=nbytes; nb>0; nb--) {
			if (brs->nl == 0) {
				if ((k=tell(brs->fd)) >= BSIZE + brs->nr) {
					lseek(brs->fd, (long) -(BSIZE + brs->nr), 1);
					brs->nl = read(brs->fd, brs->b, BSIZE);
				} else {
					lseek(brs->fd, 0L, 0);
					k = k - brs->nr;
					if (k < 0) k = 0;
					brs->nl = read(brs->fd, brs->b, k);
				}
				if (brs->nl == 0) return(nbytes-nb);
				brs->next = brs->b + brs->nl;
				brs->nr = 0;
				}
			*--buff = *--brs->next;
			brs->nr++;
			brs->nl--;
			}
		}
	return(nbytes);
	}

blseek(brs, offset, flag) 
struct brbuf *brs; long offset; {
	brs->nl = 0;
	brs->nr = 0;
	return(lseek(brs->fd,offset,flag));
	}

binit(brs)
struct brbuf *brs; {
	brs->nl = brs->nr = 0;
}

long
tell(fildes) {
	return(lseek(fildes, 0L, 1));
}
#endif
