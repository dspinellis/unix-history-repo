/*	subr_log.c	6.1	84/07/16	*/

/*
 * Error log buffer for kernel printf's.
 */

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/ioctl.h"
#include "../h/msgbuf.h"
#include "../h/file.h"
#include "../h/errno.h"

#define LOG_RDPRI	(PZERO + 1)

#define LOG_OPEN	0x01
#define LOG_NBIO	0x02
#define LOG_ASYNC	0x04
#define LOG_RDWAIT	0x08

struct logsoftc {
	int	sc_state;		/* see above for possibilities */
	struct	proc *sc_selp;		/* process waiting on select call */
	int	sc_pgrp;		/* process group for async I/O */
} logsoftc;

logopen(dev)
	dev_t dev;
{

#ifdef LOGDEBUG
	printf("logopen: dev=0x%x\n", dev);
#endif
	if (logsoftc.sc_state & LOG_OPEN)
		return(EBUSY);
	logsoftc.sc_state |= LOG_OPEN;
	logsoftc.sc_selp = 0;
	logsoftc.sc_pgrp = u.u_procp->p_pgrp;
	/*
	 * Potential race here with putchar() but since putchar should be
	 * called by autoconf, msg_magic should be initialized by the time
	 * we get here.
	 */
	if (msgbuf.msg_magic != MSG_MAGIC) {
		register int i;

		msgbuf.msg_magic = MSG_MAGIC;
		msgbuf.msg_bufx = msgbuf.msg_bufr = 0;
		for (i=0; i < MSG_BSIZE; i++)
			msgbuf.msg_bufc[i] = 0;
	}
	return(0);
}

logclose(dev, flag)
	dev_t dev;
{
	logsoftc.sc_state = 0;
	logsoftc.sc_selp = 0;
	logsoftc.sc_pgrp = 0;
#ifdef LOGDEBUG
	printf("logclose: dev=0x%x\n", dev);
#endif
}

logread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register long l;
	register u_int c;
	register struct iovec *iov;
	register int s;
	int error = 0;

#ifdef LOGDEBUG
	printf("logread: dev=0x%x\n", dev);
#endif

	s = splhigh();
	while (msgbuf.msg_bufr == msgbuf.msg_bufx) {
		if (logsoftc.sc_state & LOG_NBIO) {
			splx(s);
			return(EWOULDBLOCK);
		}
		logsoftc.sc_state |= LOG_RDWAIT;
		sleep((caddr_t)&msgbuf, LOG_RDPRI);
	}
	splx(s);
	logsoftc.sc_state &= ~LOG_RDWAIT;

	while (uio->uio_resid > 0) {
		l = msgbuf.msg_bufx - msgbuf.msg_bufr;
		if (l < 0)
			l = MSG_BSIZE - msgbuf.msg_bufr;
		c = min((u_int) l, (u_int)uio->uio_resid);
		error = uiomove((caddr_t)&msgbuf.msg_bufc[msgbuf.msg_bufr],
			(int)c, UIO_READ, uio);
		if (error)
			break;
		msgbuf.msg_bufr += c;
		if (msgbuf.msg_bufr < 0 || msgbuf.msg_bufr >= MSG_BSIZE)
			msgbuf.msg_bufr = 0;
	}
	return(error);
}

logselect(dev, rw)
	dev_t dev;
	int rw;
{
	int s = splhigh();

	switch (rw) {

	case FREAD:
		if (msgbuf.msg_bufr != msgbuf.msg_bufx)
			goto win;
#ifdef LOGDEBUG
		if (logsoftc.sc_selp)
			printf("logselect: collision\n");
#endif
		logsoftc.sc_selp = u.u_procp;
		break;

	case FWRITE:
#ifdef LOGDEBUG
		printf("logselect: FWRITE\n");
#endif
		break;
	}
	splx(s);
	return(0);
win:
	splx(s);
	return(1);
}

logwakeup()
{

	if (logsoftc.sc_selp) {
		selwakeup(logsoftc.sc_selp, 0);
		logsoftc.sc_selp = 0;
	}
	if (logsoftc.sc_state & LOG_ASYNC)
		gsignal(logsoftc.sc_pgrp, SIGIO); 
	if (logsoftc.sc_state & LOG_RDWAIT) {
		wakeup((caddr_t)&msgbuf);
		logsoftc.sc_state &= ~LOG_RDWAIT;
	}
}

/*ARGSUSED*/
logioctl(com, data, flag)
	caddr_t data;
{
	long l;
	int s;

	switch (com) {

	/* return number of characters immediately available */
	case FIONREAD:
		s = splhigh();
		l = msgbuf.msg_bufx - msgbuf.msg_bufr;
		splx(s);
		if (l < 0)
			l += MSG_BSIZE;
		*(off_t *)data = l;
		break;

	case FIONBIO:
		if (*(int *)data)
			logsoftc.sc_state |= LOG_NBIO;
		else
			logsoftc.sc_state &= ~LOG_NBIO;
		break;

	case FIOASYNC:
		if (*(int *)data)
			logsoftc.sc_state |= LOG_ASYNC;
		else
			logsoftc.sc_state &= ~LOG_ASYNC;
		break;

	case TIOCSPGRP:
		logsoftc.sc_pgrp = *(int *)data;
		break;

	case TIOCGPGRP:
		*(int *)data = logsoftc.sc_pgrp;
		break;

	default:
		return(-1);
	}
	return(0);
}
