#ifndef lint
static char sccsid[] = "@(#)access.c	5.3 (Berkeley) %G%";
#endif

/*
 * Adb: access data in file/process address space.
 */

#include "defs.h"
#include <sys/file.h>
#include <sys/ptrace.h>

off_t	lseek();

/*
 * Read or write from or to the given address, accessing or altering
 * only the given byte(s).  Return the number of bytes transferred.
 * Remote (debuggee) addresses are specified as a <space,address> pair.
 * Neither the remote nor the local address need be aligned.
 *
 * If there is a current process, ask the system to do this (via ptrace
 * [ick]).  If debugging the kernel, use vtophys() to map virtual to
 * physical locations (in a system-dependent manner).  Otherwise we
 * can just read or write the files being debugged directly.
 */
int
adbio(rw, space, rmtaddr, localaddr, cnt)
	enum rwmode rw;
	int space;
	addr_t rmtaddr;
	caddr_t localaddr;
	int cnt;
{
	register int ret;
	register struct map *mp;
	struct m1 *mm;

	static char *derr = "data address not found";
	static char *terr = "text address not found";
#define rwerr() errflag = space & SP_DATA ? derr : terr
#define	within(which) (rmtaddr >= which.b && rmtaddr < which.e)

	if (space == SP_NONE) {
		/* The no-space is all zero. */
		bzero(localaddr, cnt);
		return (cnt);
	}
	if (pid) {
		ret = io_ptrace(rw, space, rmtaddr, localaddr, cnt);
		if (ret != cnt)
			rwerr();
		return (ret);
	}
	if (rw == RWMODE_WRITE && !wtflag)
		error("not in write mode");
	mp = space & SP_DATA ? &datmap : &txtmap;
	if ((space & SP_STAR) == 0 && within(mp->m1))
		mm = &mp->m1;
	else if (within(mp->m2))
		mm = &mp->m2;
	else {
		rwerr();
		return (0);
	}
	rmtaddr += mm->f - mm->b;
	if (kernel && space == SP_DATA) {
		char *err = NULL;

		rmtaddr = vtophys(rmtaddr, &err);
		if (err) {
			errflag = err;
			return (0);
		}
	}
	if (lseek(mp->ufd, (off_t)rmtaddr, 0) == -1) {
		rwerr();
		return (0);
	}
	if (rw == RWMODE_READ) {
		ret = read(mp->ufd, localaddr, cnt);
		/* gratuitously supply extra zeroes at end of file */
		if (ret > 0 && ret < cnt) {
			bzero(localaddr + ret, cnt - ret);
			ret = cnt;
		}
	} else
		ret = write(mp->ufd, localaddr, cnt);
	if (ret != cnt)
		rwerr();
	return (ret);
#undef rwerr
#undef within
}

/*
 * Read a single object of length `len' from the core file at the
 * given offset.  Return the length read.  (This routine allows vtophys
 * and kernel crash startup code to read ptes, etc.)
 */
int
readcore(off, addr, len)
	off_t off;
	caddr_t addr;
	int len;
{

	if (lseek(corefile.fd, off, L_SET) == -1)
		return (-1);
	return (read(corefile.fd, addr, len));
}

/*
 * THE FOLLOWING IS GROSS.  WE SHOULD REPLACE PTRACE WITH SPECIAL
 * FILES A LA /proc.
 *
 * Read or write using ptrace.  io_ptrace arranges that the
 * addresses passed to ptrace are an even multiple of sizeof(int),
 * and is able to read or write single bytes.
 *
 * Since ptrace is so horribly slow, and some commands do repeated
 * reading of units smaller than an `int', io_ptrace calls cptrace
 * (cached ptrace) to allow some cacheing.  cptrace also converts a
 * read/write op and a space into a ptrace op, and returns 0 on success
 * and hence takes a pointer to the value cell rather than the value.
 */
struct cache {
	short	rop, wop;		/* ptrace ops for read and write */
	int	valid;			/* true iff cache entry valid */
	int	*addr;			/* address of cached value */
	int	val;			/* and the value */
};
static struct cache icache = { PT_READ_I, PT_WRITE_I };
static struct cache dcache = { PT_READ_D, PT_WRITE_D };

/*
 * Invalidate one or both caches.
 * This is the only function that accepts two spaces simultaneously.
 */
cacheinval(space)
	int space;
{

	if (space & SP_INSTR)
		icache.valid = 0;
	if (space & SP_DATA)
		dcache.valid = 0;
}

int	cachehit, cachemiss;		/* statistics */

static int
cptrace(rw, space, p, addr, val)
	enum rwmode rw;
	int space, p, *addr, *val;
{
	register struct cache *c = space & SP_DATA ? &dcache : &icache;
	int v;

	if (rw == RWMODE_READ) {
		if (c->valid && c->addr == addr) {
			cachehit++;
			*val = c->val;
			return (0);
		}
		cachemiss++;
		errno = 0;
		if ((v = ptrace(c->rop, p, addr, 0)) == -1 && errno)
			return (-1);
		*val = v;
	} else {
		c->valid = 0;		/* paranoia */
		errno = 0;
		if (ptrace(c->wop, p, addr, v = *val) == -1 && errno)
			return (-1);
	}
	c->valid = 1;
	c->addr = addr;
	c->val = v;
	return (0);
}

int
io_ptrace(rw, space, rmtaddr, localaddr, cnt)
	register enum rwmode rw;
	register int space;
	addr_t rmtaddr;
	register caddr_t localaddr;
	register int cnt;
{
	register addr_t addr;
	register int nbytes, ret = 0, off;
	int tmp;

	/*
	 * Start by aligning rmtaddr; set nbytes to the number of bytes of
	 * useful data we shall obtain.
	 */
	off = rmtaddr % sizeof(int);	/* addr_t is unsigned */
	addr = rmtaddr - off;
	nbytes = sizeof(int) - off;
	while (cnt != 0) {
		if (cnt < nbytes)
			nbytes = cnt;
		if (rw == RWMODE_READ) {
			if (cptrace(rw, space, pid, (int *)addr, &tmp))
				return (ret);
			bcopy((caddr_t)&tmp + off, localaddr, nbytes);
		} else {
			if (nbytes < sizeof(int) &&
			    cptrace(RWMODE_READ, space, pid, (int *)addr, &tmp))
				return (ret);
			bcopy(localaddr, (caddr_t)&tmp + off, nbytes);
			if (cptrace(rw, space, pid, (int *)addr, &tmp))
				return (ret);
		}
		addr += sizeof(int);
		localaddr += nbytes;
		ret += nbytes;
		cnt -= nbytes;
		/*
		 * For the rest of the loop, the offset is 0 and we can
		 * use all the bytes obtained.
		 */
		off = 0;
		nbytes = sizeof(int);
	}
	return (ret);
}
