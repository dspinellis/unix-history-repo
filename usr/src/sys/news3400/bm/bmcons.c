/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
 *
 * %sccs.include.redist.c%
 *
 * from: $Hdr: cons.c,v 4.300 91/06/09 06:34:41 root Rel41 $ SONY
 *
 *	@(#)bmcons.c	7.6 (Berkeley) %G%
 */

/*
 * console driver
 */
#include <sys/param.h>
#include <machine/pte.h>
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <sys/tty.h>
#include <sys/map.h>
#include <sys/buf.h>
#include <sys/clist.h>
#include <sys/file.h>
#include "bm.h"

#include <news3400/hbdev/rsreg.h>
#include <news3400/sio/sccparam.h>

#define	CN_RXE		RXE
#define	CN_TXE		TXE
#define	CN_ON		(RXE|TXE|RTS|DTR)
#define	CN_OFF		0
#define	CN_RTS		RTS
#define	CN_DTR		DTR
#define	CN_CTS		CTS
#define	CN_DCD		DCD
#define	CN_DSR		DSR
#define	CN_RI		RI
#define	CN_BRK		XBREAK

/*
 * Local variables for the driver
 */

#define	splcons	spltty

char cn_active[1];
char cn_stopped[1];
struct tty cn_tty[1];

void	cnstart();
int	ttrstrt();
int	cnrint(), cnxint(), cnsint();

bmattach(i)
{
	        /* temporary hack for pseudo-device initialization */;
}

/*
 * Open console. Turn on console if this is the first use of it.
 */
/*ARGSUSED*/
cnopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp = &cn_tty[0];

	if (cn_active[0] == 0) {
		if (cn_init() < 0)
			return (ENXIO);
		cn_enable();
		cn_active[0] = 1;
	}
	if (tp->t_state & TS_XCLUDE && p->p_ucred->cr_uid != 0)
		return (EBUSY);
	tp->t_addr = (caddr_t)0;
	tp->t_oproc = cnstart;
	tp->t_state |= TS_WOPEN;
	/*
	 * If this is first open, initialze tty state to default.
	 */
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = TTYDEF_LFLAG;
			tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
		}
		cnparam(tp, &tp->t_termios);
		ttsetwater(tp);
	}
	/*
	 * Wait receiver and status interrupt
	 */
	(void) cnmctl(CN_ON, DMSET);
	tp->t_state |= TS_CARR_ON;
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}

/*
 * Close console.
 */
/*ARGSUSED*/
cnclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp = &cn_tty[0];

	(*linesw[tp->t_line].l_close)(tp);
	(void) cnmctl(CN_BRK, DMBIC);
	ttyclose(tp);
	return (0);
}

/*ARGSUSED*/
cnread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	register struct tty *tp = &cn_tty[0];

	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

/*ARGSUSED*/
cnwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	register struct tty *tp = &cn_tty[0];

	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

/*
 * console receiver interrupt.
 */
_cnrint(buf, n)
	register char *buf;
	register int n;
{
	register struct tty *tp = &cn_tty[0];
	register int (*rint)();

	if ((tp->t_state & TS_ISOPEN) == 0) {
		wakeup((caddr_t)&tp->t_rawq);
		cn_enable();
		return;
	}
	/*
	 * Loop fetching characters from the silo for console
	 * until there are no more in the silo.
	 */
	rint = linesw[tp->t_line].l_rint;
	while (--n >= 0)
		(*rint)(*buf++, tp);
	cn_enable();
}

/*
 * Ioctl for console.
 */
/*ARGSUSED*/
cnioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct tty *tp = &cn_tty[0];
	int error;

	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	switch (cmd) {

	case TIOCSBRK:
		(void) cnmctl(CN_BRK, DMBIS);
		break;

	case TIOCCBRK:
		(void) cnmctl(CN_BRK, DMBIC);
		break;

	case TIOCSDTR:
		(void) cnmctl(CN_DTR|CN_RTS, DMBIS);
		break;

	case TIOCCDTR:
		(void) cnmctl(CN_DTR|CN_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) cnmctl(dmtocn(*(int *)data), DMSET);
		break;

	case TIOCMBIS:
		(void) cnmctl(dmtocn(*(int *)data), DMBIS);
		break;

	case TIOCMBIC:
		(void) cnmctl(dmtocn(*(int *)data), DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = cntodm(cnmctl(0, DMGET));
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

dmtocn(bits)
	register int bits;
{
	register int b;

	b = 0;
	if (bits & DML_LE)  b |= CN_TXE|CN_RXE;
	if (bits & DML_DTR) b |= CN_DTR;
	if (bits & DML_RTS) b |= CN_RTS;
	if (bits & DML_CTS) b |= CN_CTS;
	if (bits & DML_CAR) b |= CN_DCD;
	if (bits & DML_RNG) b |= CN_RI;
	if (bits & DML_DSR) b |= CN_DSR;
	return(b);
}

cntodm(bits)
	register int bits;
{
	register int b;

	b = 0;
	if (bits & (CN_TXE|CN_RXE)) b |= DML_LE;
	if (bits & CN_DTR) b |= DML_DTR;
	if (bits & CN_RTS) b |= DML_RTS;
	if (bits & CN_CTS) b |= DML_CTS;
	if (bits & CN_DCD) b |= DML_CAR;
	if (bits & CN_RI)  b |= DML_RNG;
	if (bits & CN_DSR) b |= DML_DSR;
	return(b);
}
 
/*
 * Set parameters from open or stty into the console hardware
 * registers.
 */
cnparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register int param;
	register int cflag = t->c_cflag;
	int s;

	/*
	 * Block interrupts so parameters will be set
	 * before the line interrupts.
	 */
	s = splcons();
	if ((tp->t_ispeed)==0) {
		tp->t_cflag |= HUPCL;
		(void) cnmctl(CN_OFF, DMSET);
		(void) splx(s);
		return;
	}

	param = cn_get_param() &
		~(CHAR_SIZE|PARITY|EVEN|STOPBIT|BAUD_RATE|NOCHECK);
	if ((cflag & CREAD) == 0)
		param &= ~RXE;
	switch (cflag & CSIZE) {
	    case CS5: break;
	    case CS6: param |= C6BIT; break;
	    case CS7: param |= C7BIT; break;
	    case CS8: param |= C8BIT; break;
	}
	if (cflag & PARENB)
		param |= PARITY;
	if ((cflag & PARODD) == 0)
		param |= EVEN;
	if ((tp->t_iflag & INPCK) == 0)
		param |= NOCHECK;
	if (cflag & CSTOPB)
		param |= STOP2;
	else
		param |= STOP1;
	cn_set_param(param);
	(void) splx(s);
}

/*
 * console transmitter interrupt.
 * Restart the idle line.
 */
_cnxint(count)
	int count;
{
	register struct tty *tp = &cn_tty[0];
	int s;

	cn_stopped[0] = 0;
	tp->t_state &= ~TS_BUSY;
	s = splcons();
	if (tp->t_state & TS_FLUSH)
		tp->t_state &= ~TS_FLUSH;
	else
		ndflush(&tp->t_outq, count);
	(void) splx(s);
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		cnstart(tp);
}

/*
 * Start (restart) transmission on the console.
 */
void
cnstart(tp)
	register struct tty *tp;
{
	register int nch;
	int s;

	/*
	 * Must hold interrupts in following code to prevent
	 * state of the tp from changing.
	 */
	s = splcons();
	/*
	 * If it's currently active, or delaying, no need to do anything.
	 */
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	/*
	 * If ther are still characters in the IOP,
	 * just reenable transmit.
	 */
	if (cn_stopped[0]) {
		cn_stopped[0] = 0;
		cn_start();
		goto out;
	}
	/*
	 * If there are sleepers, and output has drained below low
	 * water mark, wake up the sleepers.
	 */
	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}
	/*
	 * Now restart transmission unless the output queue is
	 * empty.
	 */
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (tp->t_flags & (RAW|LITOUT))
		nch = ndqb(&tp->t_outq, 0);
	else {
		nch = ndqb(&tp->t_outq, 0200);
		/*
		 * If first thing on queue is a delay process it.
		 */
		if (nch == 0) {
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch&0x7f)+6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}
	/*
	 * If characters to transmit, restart transmission.
	 */
	if (nch) {
		tp->t_state |= TS_BUSY;
		cn_output(tp, nch);
	}
out:
	(void) splx(s);
}

/*
 * Stop output on a line, e.g. for ^S/^Q or output flush.
 */
/*ARGSUSED*/
cnstop(tp, flag)
	register struct tty *tp;
{
	register int s;

	/*
	 * Block input/output interrupts while messing with state.
	 */
	s = splcons();
	if (tp->t_state & TS_BUSY) {
		cn_stop(0);
		cn_stopped[0] = 1;
		if ((tp->t_state & TS_TTSTOP) == 0) {
			tp->t_state |= TS_FLUSH;
			cn_stop(1);
		}
	}
	(void) splx(s);
}

/*
 * console modem control
 */
cnmctl(bits, how)
	int bits, how;
{
	register int mbits;
	int s;

	bits &= (RXE|TXE|RTS|DTR|XBREAK);

	s = splcons();

	mbits = cn_get_param();
	switch (how) {
	case DMSET:
		mbits = mbits & ~(RXE|TXE|RTS|DTR|XBREAK) | bits;
		break;

	case DMBIS:
		mbits |= bits;
		break;

	case DMBIC:
		mbits &= ~bits;
		break;

	case DMGET:
		(void) splx(s);
		return(mbits);
	}
	cn_set_param(mbits);

	(void) splx(s);
	return(mbits);
}

/*
 * console status interrupt
 */
_cnsint(stat)
	int stat;
{
	register struct tty *tp = &cn_tty[0];

	if (stat & OVERRUN_ERROR)
		printf("console: fifo overflow\n");
	if (stat & RBREAK)
		(*linesw[tp->t_line].l_rint)
		    (tp->t_flags & RAW ? '\0' : tp->t_cc[VINTR], tp);
}

/*
 * console control interrupt
 */
cncint()
{
	printf("cncint:\n");
}

/*
 * Machine dependent functions
 *
 *	cn_init()
 *	cnrint()
 *	cnxint()
 *	cnsint()
 *	cn_enable()
 *	cn_output()
 *	cn_start()
 *	cn_stop()
 *	cn_get_param()
 *	cn_set_param()
 */
#ifdef IPC_MRX
#include <news3400/newsipc/newsipc.h>
#include <news3400/mrx/h/cio.h>
#include <news3400/mrx/h/console.h>

#ifdef mips
#define ipc_phys(x)	K0_TT0(x)
#define ipc_log(x)	TT0_K0(x)
#else
#define ipc_phys(x)	(caddr_t)((int)(x) & ~0x80000000)
#define ipc_log(x)	(caddr_t)((int)(x) | 0x80000000)
#endif

#if NBM > 0
extern char *ext_fnt_addr[];
extern char *ext_fnt24_addr[];
#endif /* NBM > 0 */

int	port_cnrecv;
int	port_cnxmit;
int	port_cnstat;
int	port_cnctrl;
int	port_cnfont;
int	port_cnrecv_iop;
int	port_cnxmit_iop;
int	port_cnstat_iop;
int	port_cnctrl_iop;

int	cnfont();

cn_init()
{
	struct cons_ctrl_req req;
	int *reply;

	port_cnrecv = port_create("@cnrecv", cnrint, -1);
	port_cnxmit = port_create("@cnxmit", cnxint, -1);
	port_cnctrl = port_create("@cnctrl", NULL, 0);
	port_cnstat = port_create("@cnstat", cnsint, -1);
	port_cnfont = port_create("@cnfont", cnfont, -1);

	/* use NULL action port */
	port_cnrecv_iop = object_query("cons_input");
	port_cnxmit_iop = object_query("cons_output");
	port_cnctrl_iop = object_query("cons_ctrl");
	port_cnstat_iop = object_query("cons_stat");
	req.cons_func = CIO_ASKDEVICE;
	msg_send(port_cnctrl_iop, port_cnctrl, &req, sizeof(req), 0);
	msg_recv(port_cnctrl, NULL, &reply, NULL, 0);
	tty00_is_console = *reply;
	msg_free(port_cnctrl);
#if NBM > 0
	req.cons_func = CIO_SET16FNT;
	req.cons_addr = (char *)ipc_phys(ext_fnt_addr);
	msg_send(port_cnctrl_iop, port_cnctrl, &req, sizeof(req), 0);
	msg_recv(port_cnctrl, NULL, NULL, NULL, 0);
	req.cons_func = CIO_SET24FNT;
	req.cons_addr = (char *)ipc_phys(ext_fnt24_addr);
	msg_send(port_cnctrl_iop, port_cnctrl, &req, sizeof(req), 0);
	msg_recv(port_cnctrl, NULL, NULL, NULL, 0);
#endif
	return (0);
}

cn_enable()
{
	int len;

	len = MAX_CIO;
	msg_send(port_cnrecv_iop, port_cnrecv, &len, sizeof(len), 0);
}

cnrint(port)
	int port;
{
	int len;
	char *buf;

	msg_recv(port, NULL, &buf, &len, 0);
#ifdef mips
	_cnrint((char *)MACH_CACHED_TO_UNCACHED(buf), len);
#else
	dcia();
	_cnrint(buf, len);
#endif
	msg_free(port);
}

cnxint(port)
	int port;
{
	int *len;

	msg_recv(port, NULL, &len, NULL, 0);
	_cnxint(*len);
}

cn_start()
{
	int func;

	func = CIO_START;
	msg_send(port_cnctrl_iop, 0, &func, sizeof(func), 0);
}

cn_output(tp, n)
	struct tty *tp;
	int n;
{

	msg_send(port_cnxmit_iop, port_cnxmit, tp->t_outq.c_cf,
	    min(n, MAX_CIO), 0);
}

cn_stop(flush)
	int flush;
{
	int	func;

	func = flush ? CIO_FLUSH : CIO_STOP;
	msg_send(port_cnctrl_iop, 0, &func, sizeof(func), 0);
}

cnsint(port)
	int port;
{
	int *stat;

	msg_recv(port, NULL, &stat, NULL, 0);
	_cnsint(*stat);
	msg_send(port_cnstat_iop, port_cnstat, NULL, 0, 0);
}

cn_get_param()
{
	struct cons_ctrl_req req;
	int *reply, param;

	req.cons_func = CIO_GETPARAMS;
	/* message length 8 means 2 * sizeof(int) : func and status */
	msg_send(port_cnctrl_iop, port_cnctrl, &req, 8, 0);
	msg_recv(port_cnctrl, NULL, &reply, NULL, 0);
	param = *reply;
	msg_free(port_cnctrl);

	return (param);
}

cn_set_param(param)
	int param;
{
	struct cons_ctrl_req req;

	req.cons_func = CIO_SETPARAMS;
	req.cons_status = param;

	/* message length 8 means 2 * sizeof(int) : func and status */
	msg_send(port_cnctrl_iop, 0, &req, 8, 0);
}

cnfont(port)
	int port;
{
	int *func;

	msg_recv(port, NULL, &func, NULL, 0);
#if NBM > 0
	switch (*func) {

	case FONT_JISROMAN:
		font_jisroman();
		font_jisroman24();
		break;

	case FONT_ASCII:
		font_ascii();
		font_ascii24();
		break;
	}
#endif /* NBM > 0 */
	msg_free(port);
}
#endif /* IPC_MRX */

#ifdef CPU_SINGLE
#include <news3400/hbdev/rsreg.h>
#include <news3400/iop/framebuf.h>
#include <news3400/fb/fbdefs.h>

int lastcount;
int start_dimmer = 1;

cn_init()
{

	if (start_dimmer) {
		auto_dimmer();
		start_dimmer = 0;
	}
	return (0);
}

cn_enable()
{

	/* nothing to do */
}

cnrint(code)
	char code;
{

	_cnrint(&code, 1);
}

cnxint()
{

	_cnxint(lastcount);
}

cn_start()
{

	/* nothing to do */
}

cn_output(tp, n)
	struct tty *tp;
	int n;
{

	lastcount = vt100_write(0, tp->t_outq.c_cf, n);
	cnxint();
}

cn_stop(flush)
	int flush;
{

	/* nothing to do */
}

cnsint(param)
	int param;
{

	_cnsint(param);
}

cn_get_param()
{

	return (bitmap_get_param());
}

cn_set_param(param)
	int param;
{

	bitmap_set_param(param);
}
#endif /* CPU_SINGLE */
