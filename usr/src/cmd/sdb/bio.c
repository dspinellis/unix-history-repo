#include "bio.h"

/*
 * NAMES:  bread(), brseek(), blseek()
 *
 * DESCRIPTION:
 *	 This is a buffered read package.
 *
 *       Bread may be called with a negative nbytes which causes it to
 *      read backwards.  In this case, buffer should point to the first
 *      byte following the buffer.  If only a partial read is possible
 *      (due to beginning of file), only the last bytes of the buffer
 *      will be filled.
 */


bread(brs, buff, nbytes)
struct brbuf *brs; char *buff; {
	register int k, nb;

	if (nbytes > 0) {
		for (nb=nbytes; nb>0; nb--) {
			if (brs->nr == 0) {
				brs->nr = read(brs->fd, brs->next=brs->b, 512);
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
				lseek(brs->fd, (long) -(512 + brs->nr), 1);
				brs->nl = read(brs->fd, brs->b, 512);
				if (brs->nl < 0) {
					for (k=511; k>0; k--) {
						lseek(brs->fd, 1, 1);
						brs->nl = read(brs->fd, brs->b, k);
						if (brs->nl >= 0) break;
						}
					if (brs->nl < 0) return(nbytes-nb);
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
