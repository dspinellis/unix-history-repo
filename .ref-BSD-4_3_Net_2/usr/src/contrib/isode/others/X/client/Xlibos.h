/*
 * Xlib include file for 4.2BSD based systems.
 */

#include <netinet/in.h>
#include <sys/ioctl.h>
#include <netdb.h>

#include <sys/uio.h>	/* needed for XlibInt.c */

#include <sys/param.h> /* needed for XConnDis.c */

#define MSKCNT ((NOFILE + 31) / 32)	/* size of bit array */

#if (MSKCNT==1)
#define BITMASK(i) (1 << (i))
#define MASKIDX(i) 0
#endif
#if (MSKCNT>1)
#define BITMASK(i) (1 << ((i) & 31))
#define MASKIDX(i) ((i) >> 5)
#endif

#define MASKWORD(buf, i) buf[MASKIDX(i)]
#define BITSET(buf, i) MASKWORD(buf, i) |= BITMASK(i)
#define BITCLEAR(buf, i) MASKWORD(buf, i) &= ~BITMASK(i)
#define GETBIT(buf, i) (MASKWORD(buf, i) & BITMASK(i))

#if (MSKCNT==1)
#define COPYBITS(src, dst) dst[0] = src[0]
#define CLEARBITS(buf) buf[0] = 0
#define MASKANDSETBITS(dst, b1, b2) dst[0] = (b1[0] & b2[0])
#define ORBITS(dst, b1, b2) dst[0] = (b1[0] | b2[0])
#define UNSETBITS(dst, b1) (dst[0] &= ~b1[0])
#define ANYSET(src) (src[0])
#endif
#if (MSKCNT==2)
#define COPYBITS(src, dst) { dst[0] = src[0]; dst[1] = src[1]; }
#define CLEARBITS(buf) { buf[0] = 0; buf[1] = 0; }
#define MASKANDSETBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] & b2[0]);\
		      dst[1] = (b1[1] & b2[1]); }
#define ORBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] | b2[0]);\
		      dst[1] = (b1[1] | b2[1]); }
#define UNSETBITS(dst, b1) {\
                      dst[0] &= ~b1[0]; \
                      dst[1] &= ~b1[1]; }
#define ANYSET(src) (src[0] || src[1])
#endif
#if (MSKCNT==3)
#define COPYBITS(src, dst) { dst[0] = src[0]; dst[1] = src[1]; dst[2] = src[2]; }
#define CLEARBITS(buf) { buf[0] = 0; buf[1] = 0; buf[2] = 0; }
#define MASKANDSETBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] & b2[0]);\
		      dst[1] = (b1[1] & b2[1]);\
		      dst[2] = (b1[2] & b2[2]); }
#define ORBITS(dst, b1, b2)  {\
		      dst[0] = (b1[0] | b2[0]);\
		      dst[1] = (b1[1] | b2[1]);\
		      dst[2] = (b1[2] | b2[2]); }
#define UNSETBITS(dst, b1) {\
                      dst[0] &= ~b1[0]; \
                      dst[1] &= ~b1[1]; \
                      dst[2] &= ~b1[2]; }
#define ANYSET(src) (src[0] || src[1] || src[2])
#endif
#if (MSKCNT>3)
#define COPYBITS(src, dst) bcopy((caddr_t) src, (caddr_t) dst,\
				 MSKCNT*sizeof(long))
#define CLEARBITS(buf) bzero((caddr_t) buf, MSKCNT*sizeof(long))
#define MASKANDSETBITS(dst, b1, b2)  { int cri;\
		      for (cri=0; i<MSKCNT; cri++) \
		          dst[cri] = (b1[cri] & b2[cri]); }
#define ORBITS(dst, b1, b2)  { int cri;\
		      for (cri=0; i<MSKCNT; cri++) \
		          dst[cri] = (b1[cri] | b2[cri]); }
#define UNSETBITS(dst, b1)  { int cri;\
		      for (cri=0; i<MSKCNT; cri++) \
		          dst[cri] &= ~b1[cri]; }
#define ANYSET(src) (src[0] || src[1] || src[2])
#endif

char *malloc(), *realloc(), *calloc(), *alloca();

char *strncpy(), *strncat();

void exit();
void bcopy();
void perror();
void free();

#define LockDisplay(dis)
#define LockMutex(mutex)
#define UnlockMutex(mutex)
#define UnlockDisplay(dis)
#define Xfree(ptr) free((ptr))
#define Xalloca(size) alloca((size))

#if (defined ibm032)&&(defined __HIGHC__)
pragma on(alloca);
#endif

/*
 * Note that some machines do not return a valid pointer for malloc(0), in
 * which case we provide an alternate under the control of the
 * define MALLOC_0_RETURNS_NULL.  This is necessary because some
 * Xlib code expects malloc(0) to return a valid pointer to storage.
 */
#ifdef MALLOC_0_RETURNS_NULL

# define Xmalloc(size) malloc(((size) > 0 ? (size) : 1))
# define Xrealloc(ptr, size) realloc((ptr), ((size) > 0 ? (size) : 1))
# define Xcalloc(nelem, elsize) calloc(((nelem) > 0 ? (nelem) : 1), (elsize))

#else

# define Xmalloc(size) malloc((size))
# define Xrealloc(ptr, size) realloc((ptr), (size))
# define Xcalloc(nelem, elsize) calloc((nelem), (elsize))

#endif

#ifdef ISOCONN
/*
 * Should pick up next from server header file...
 */
#define MAXSOCKS 64

#define UNIX_IO 0
#define ISODE_IO 1

#define BytesReadable(fd, ptr) \
ioctlfn[fd2family[fd]](fd, ptr)

#define ReadFromServer(fd, data, size) \
readfn[fd2family[fd]]((fd), (data), (size))

#define WriteToServer(fd, bufind, size) \
writefn[fd2family[fd]]((fd), (bufind), (size))

#define ReadvFromServer(fd, iov, iovcnt) \
readvfn[fd2family[fd]]((fd), (iov), (iovcnt))

#define WritevToServer(fd, iov, iovcnt) \
writevfn[fd2family[fd]]((fd), (iov), (iovcnt))

#define CloseToServer(fd) \
closefn[fd2family[fd]](fd)

extern int isodexbug;

#else /* ISOCONN */
#define BytesReadable(fd, ptr) ioctl ((fd), FIONREAD, (ptr))
#if !defined (mips) || !defined (SYSTYPE_SYSV)
#define ReadFromServer(dpy, data, size) read((dpy), (data), (size))
#define WriteToServer(dpy, bufind, size) write((dpy), (bufind), (size))
#endif /* !mips || !SYSTYPE_SYSV */
#define ReadvFromServer(dpy, iov, iovcnt) readv((dpy), (iov), (iovcnt))
#define WritevToServer(dpy, iov, iovcnt) writev((dpy), (iov), (iovcnt))
#endif /* ISOCONN */
/*
 *	ReadvFromServer and WritevToSever use struct iovec, normally found
 *	in Berkeley systems in <sys/uio.h>.  See the readv(2) and writev(2)
 *	manual pages for details.
 *
 *	struct iovec {
 *		caddr_t iov_base;
 *		int iov_len;
 *	};
 */

extern char *index();
#define SearchString(string, char) index((string), (char))
