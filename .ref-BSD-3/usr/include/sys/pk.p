/*
 * kernel level
 */
#ifdef	KERNEL

#define PADDR		((struct pack *)tp->t_linep)
#define TURNOFF		pkturnoff(tp)
#define UCOUNT		u.u_count
#define S		tp
#define P		pk->p_ttyp
#define SDEF		struct tty *tp
#define FS		, tp

#define SIGNAL		signal(pk->p_ttyp->t_pgrp, SIGPIPE)
#define TERROR		pk->p_istate == R_ERROR
#define SETERROR	u.u_error = EIO
#define OBUSY		tp->t_state&BUSY
#define GETEPACK	getepack(pk->p_bits)
char * getepack();
#define FREEPACK(a,b)	freepack(a, b)


#define q1	tp->t_rawq
#define q2	tp->t_canq
#define q3	tp->t_outq

#define LOCK		s = spl6()
#define UNLOCK		splx(s)
#define DSYSTEM		struct tty *p_ttyp
#define ISYSTEM		tp = pk->p_ttyp
#define	SLEEP(a, b)	sleep((caddr_t)a, b)
#define	SLEEPNO		(tp->t_chan!=NULL)
#define WAKEUP(a)	wakeup((caddr_t)a)
#define IOMOVE(p, c, f) iomove(p, c, f)
#define PKGETPKT(p)
#define DTOM(a)		dtom(a)
#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/pk.h"
#include "../h/tty.h"
#include "../h/buf.h"
#include "../h/proc.h"

#endif
/*
 * user level
 */
#ifdef	USER
#define SLEEP(a, b) 
#define SIGNAL
#define WAKEUP(a)
#define DSYSTEM		int p_ifn, p_ofn
#define ISYSTEM
#define GETEPACK	malloc(pk->p_xsize)
#define FREEPACK(a, b)	free(a)
#define OBUSY		0
#define PKGETPKT(p)	pkgetpack(p);
#define DTOM(a)		1;
#define S		ipk, ibuf, icount
#define SDEF		int icount; char *ibuf; struct pack *ipk
#define UCOUNT		icount
#define IOMOVE(p, c, f)	pkmove(p, ibuf, c, f) ; ibuf += c; UCOUNT -= c
#define PADDR		ipk
#define TURNOFF
#define LOCK
#define UNLOCK
#define SETERROR
#define GENERROR(p, s)
#define	PACKSIZE	64
#define	WINDOWS		3
#define	PKDEBUG(l, f, s) { extern Debug; if (Debug >= l) fprintf(stderr, f, s);}
#define	PKASSERT(e, f, v) if (!(e)) {\
fprintf(stderr, "AERROR - (%s) ", "e");\
fprintf(stderr, f, v);\
pkfail();};
#endif
