/*
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratories.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kgdb_stub.c	8.3 (Berkeley) %G%
 */

/*
 * "Stub" to allow remote cpu to debug over a serial line using gdb.
 */
#ifdef KGDB
#ifndef lint
static char rcsid[] = "$Header: /usr/src/sys/hp300/hp300/RCS/kgdb_stub.c,v 1.5 92/12/20 15:49:01 mike Exp $";
#endif

#include <sys/param.h>
#include <sys/systm.h>

#include <machine/trap.h>
#include <machine/cpu.h>
#include <machine/psl.h>
#include <machine/reg.h>
#include <machine/frame.h>

#include <sys/buf.h>
#include <hp/dev/cons.h>

#include <hp300/hp300/kgdb_proto.h>
#include <machine/remote-sl.h>

extern int kernacc();
extern void chgkprot();

#ifndef KGDBDEV
#define KGDBDEV NODEV
#endif
#ifndef KGDBRATE
#define KGDBRATE 9600
#endif

dev_t kgdb_dev = KGDBDEV;	/* remote debugging device (NODEV if none) */
int kgdb_rate = KGDBRATE;	/* remote debugging baud rate */
int kgdb_active = 0;            /* remote debugging active if != 0 */
int kgdb_debug_init = 0;	/* != 0 waits for remote at system init */
int kgdb_debug_panic = 1;	/* != 0 waits for remote on panic */
int kgdb_debug = 0;

#define GETC	((*kgdb_getc)(kgdb_dev))
#define PUTC(c)	((*kgdb_putc)(kgdb_dev, c))
#define PUTESC(c) { \
	if (c == FRAME_END) { \
		PUTC(FRAME_ESCAPE); \
		c = TRANS_FRAME_END; \
	} else if (c == FRAME_ESCAPE) { \
		PUTC(FRAME_ESCAPE); \
		c = TRANS_FRAME_ESCAPE; \
	} else if (c == FRAME_START) { \
		PUTC(FRAME_ESCAPE); \
		c = TRANS_FRAME_START; \
	} \
	PUTC(c); \
}
static int (*kgdb_getc)();
static int (*kgdb_putc)();

/*
 * Send a message.  The host gets one chance to read it.
 */
static void
kgdb_send(type, bp, len)
	register u_char type;
	register u_char *bp;
	register int len;
{
	register u_char csum;
	register u_char *ep = bp + len;

	PUTC(FRAME_START);
	PUTESC(type);

	csum = type;
	while (bp < ep) {
		type = *bp++;
		csum += type;
		PUTESC(type)
	}
	csum = -csum;
	PUTESC(csum)
	PUTC(FRAME_END);
}

static int
kgdb_recv(bp, lenp)
	u_char *bp;
	int *lenp;
{
	register u_char c, csum;
	register int escape, len;
	register int type;

restart:
	csum = len = escape = 0;
	type = -1;
	while (1) {
		c = GETC;
		switch (c) {

		case FRAME_ESCAPE:
			escape = 1;
			continue;

		case TRANS_FRAME_ESCAPE:
			if (escape)
				c = FRAME_ESCAPE;
			break;

		case TRANS_FRAME_END:
			if (escape)
				c = FRAME_END;
			break;

		case TRANS_FRAME_START:
			if (escape)
				c = FRAME_START;
			break;
			
		case FRAME_START:
			goto restart;

		case FRAME_END:
			if (type < 0 || --len < 0) {
				csum = len = escape = 0;
				type = -1;
				continue;
			}
			if (csum != 0) {
				return (0);
			}
			*lenp = len;
			return type;
		}
		csum += c;
		if (type < 0) {
			type = c;
			escape = 0;
			continue;
		}
		if (++len > SL_RPCSIZE) {
			while (GETC != FRAME_END)
				;
			return (0);
		}
		*bp++ = c;
		escape = 0;
	}
}

/*
 * Translate a trap number into a unix compatible signal value.
 * (gdb only understands unix signal numbers).
 */
static int 
computeSignal(type)
	int type;
{
	int sigval;

	switch (type) {
	case T_BUSERR:
		sigval = SIGBUS;
		break;
	case T_ADDRERR:
		sigval = SIGBUS;
		break;
	case T_ILLINST:
		sigval = SIGILL;
		break;
	case T_ZERODIV:
		sigval = SIGFPE;
		break;
	case T_CHKINST:
		sigval = SIGFPE;
		break;
	case T_TRAPVINST:
		sigval = SIGFPE;
		break;
	case T_PRIVINST:
		sigval = SIGILL;
		break;
	case T_TRACE:
		sigval = SIGTRAP;
		break;
	case T_MMUFLT:
		sigval = SIGSEGV;
		break;
	case T_SSIR:
		sigval = SIGSEGV;
		break;
	case T_FMTERR:
		sigval = SIGILL;
		break;
	case T_FPERR:
		sigval = SIGFPE;
		break;
	case T_COPERR:
		sigval = SIGFPE;
		break;
	case T_ASTFLT:
		sigval = SIGINT;
		break;
	case T_TRAP15:
		sigval = SIGTRAP;
		break;
	default:
		sigval = SIGEMT;
		break;
	}
	return (sigval);
}

/*
 * Trap into kgdb to wait for debugger to connect, 
 * noting on the console why nothing else is going on.
 */
kgdb_connect(verbose)
	int verbose;
{

	if (verbose)
		printf("kgdb waiting...");
	/* trap into kgdb */
	asm("trap #15;");
	if (verbose)
		printf("connected.\n");
}

/*
 * Decide what to do on panic.
 */
kgdb_panic()
{

	if (kgdb_active == 0 && kgdb_debug_panic && kgdb_dev != NODEV)
		kgdb_connect(1);
}

/*
 * Definitions exported from gdb.
 */
#define NUM_REGS 18
#define REGISTER_BYTES ((16+2)*4)
#define REGISTER_BYTE(N)  ((N)*4)

#define GDB_SR 16
#define GDB_PC 17

static inline void
kgdb_copy(src, dst, nbytes)
	register u_char *src, *dst;
	register u_int nbytes;
{
	register u_char *ep = src + nbytes;

	while (src < ep)
		*dst++ = *src++;
}

/*
 * There is a short pad word between SP (A7) and SR which keeps the
 * kernel stack long word aligned (note that this is in addition to
 * the stack adjust short that we treat as the upper half of a longword
 * SR).  We must skip this when copying into and out of gdb.
 */
static inline void
regs_to_gdb(fp, regs)
	struct frame *fp;
	u_long *regs;
{
	kgdb_copy((u_char *)fp->f_regs, (u_char *)regs, 16*4);
	kgdb_copy((u_char *)&fp->f_stackadj, (u_char *)&regs[GDB_SR], 2*4);
}

static inline void
gdb_to_regs(fp, regs)
	struct frame *fp;
	u_long *regs;
{
	kgdb_copy((u_char *)regs, (u_char *)fp->f_regs, 16*4);
	kgdb_copy((u_char *)&regs[GDB_SR], (u_char *)&fp->f_stackadj, 2*4);
}

static u_long reg_cache[NUM_REGS];
static u_char inbuffer[SL_RPCSIZE+1];
static u_char outbuffer[SL_RPCSIZE];

/*
 * This function does all command procesing for interfacing to 
 * a remote gdb.
 */
int 
kgdb_trap(type, frame)
	int type;
	struct frame *frame;
{
	register u_long len;
	u_char *addr;
	register u_char *cp;
	register u_char out, in;
	register int outlen;
	int inlen;
	u_long gdb_regs[NUM_REGS];

	if (kgdb_dev < 0) {
		/* not debugging */
		return (0);
	}
	if (kgdb_active == 0) {
		if (type != T_TRAP15) {
			/* No debugger active -- let trap handle this. */
			return (0);
		}
		kgdb_getc = 0;
		for (inlen = 0; constab[inlen].cn_probe; inlen++)
		    if (major(constab[inlen].cn_dev) == major(kgdb_dev)) {
			kgdb_getc = constab[inlen].cn_getc;
			kgdb_putc = constab[inlen].cn_putc;
			break;
		}
		if (kgdb_getc == 0 || kgdb_putc == 0)
			return (0);
		/*
		 * If the packet that woke us up isn't an exec packet,
		 * ignore it since there is no active debugger.  Also,
		 * we check that it's not an ack to be sure that the 
		 * remote side doesn't send back a response after the
		 * local gdb has exited.  Otherwise, the local host
		 * could trap into gdb if it's running a gdb kernel too.
		 */
		in = GETC;
		/*
		 * If we came in asynchronously through the serial line,
		 * the framing character is eaten by the receive interrupt,
		 * but if we come in through a synchronous trap (i.e., via
		 * kgdb_connect()), we will see the extra character.
		 */
		if (in == FRAME_START)
			in = GETC;

		/*
		 * Check that this is a debugger exec message.  If so,
		 * slurp up the entire message then ack it, and fall
		 * through to the recv loop.
		 */
		if (KGDB_CMD(in) != KGDB_EXEC || (in & KGDB_ACK) != 0)
			return (0);
		while (GETC != FRAME_END)
			;
		/*
		 * Do the printf *before* we ack the message.  This way
		 * we won't drop any inbound characters while we're 
		 * doing the polling printf.
		 */
		printf("kgdb started from device %x\n", kgdb_dev);
		kgdb_send(in | KGDB_ACK, (u_char *)0, 0);
		kgdb_active = 1;
	}
	/*
	 * Stick frame regs into our reg cache then tell remote host
	 * that an exception has occured.
	 */
	regs_to_gdb(frame, gdb_regs);
	if (type != T_TRAP15) {
		/*
		 * Only send an asynchronous SIGNAL message when we hit
		 * a breakpoint.  Otherwise, we will drop the incoming
		 * packet while we output this one (and on entry the other 
		 * side isn't interested in the SIGNAL type -- if it is,
		 * it will have used a signal packet.)
		 */
		outbuffer[0] = computeSignal(type);
		kgdb_send(KGDB_SIGNAL, outbuffer, 1);
	}

	while (1) {
		in = kgdb_recv(inbuffer, &inlen);
		if (in == 0 || (in & KGDB_ACK))
			/* Ignore inbound acks and error conditions. */
			continue;

		out = in | KGDB_ACK;
		switch (KGDB_CMD(in)) {

		case KGDB_SIGNAL:
			/*
			 * if this command came from a running gdb,
			 * answer it -- the other guy has no way of
			 * knowing if we're in or out of this loop
			 * when he issues a "remote-signal".  (Note
			 * that without the length check, we could
			 * loop here forever if the ourput line is
			 * looped back or the remote host is echoing.)
			 */
			if (inlen == 0) {
				outbuffer[0] = computeSignal(type);
				kgdb_send(KGDB_SIGNAL, outbuffer, 1);
			}
			continue;

		case KGDB_REG_R:
		case KGDB_REG_R | KGDB_DELTA:
			cp = outbuffer;
			outlen = 0;
			for (len = inbuffer[0]; len < NUM_REGS; ++len) {
				if (reg_cache[len] != gdb_regs[len] ||
				    (in & KGDB_DELTA) == 0) {
					if (outlen + 5 > SL_MAXDATA) {
						out |= KGDB_MORE;
						break;
					}
					cp[outlen] = len;
					kgdb_copy((u_char *)&gdb_regs[len],
						  &cp[outlen + 1], 4);
					reg_cache[len] = gdb_regs[len];
					outlen += 5;
				}
			}
			break;
			
		case KGDB_REG_W:
		case KGDB_REG_W | KGDB_DELTA:
			cp = inbuffer;
			for (len = 0; len < inlen; len += 5) {
				register int j = cp[len];

				kgdb_copy(&cp[len + 1],
					  (u_char *)&gdb_regs[j], 4);
				reg_cache[j] = gdb_regs[j];
			}
			gdb_to_regs(frame, gdb_regs);
			outlen = 0;
			break;
				
		case KGDB_MEM_R:
			len = inbuffer[0];
			kgdb_copy(&inbuffer[1], (u_char *)&addr, 4);
			if (len > SL_MAXDATA) {
				outlen = 1;
				outbuffer[0] = E2BIG;
			} else if (!kgdb_acc(addr, len, B_READ)) {
				outlen = 1;
				outbuffer[0] = EFAULT;
			} else {
				outlen = len + 1;
				outbuffer[0] = 0;
				kgdb_copy(addr, &outbuffer[1], len);
			}
			break;

		case KGDB_MEM_W:
			len = inlen - 4;
			kgdb_copy(inbuffer, (u_char *)&addr, 4);
			outlen = 1;
			if (!kgdb_acc(addr, len, B_READ))
				outbuffer[0] = EFAULT;
			else {
				outbuffer[0] = 0;
				if (!kgdb_acc(addr, len, B_WRITE))
					chgkprot(addr, len, B_WRITE);
				kgdb_copy(&inbuffer[4], addr, len);
				ICIA();
			}
			break;

		case KGDB_KILL:
			kgdb_active = 0;
			printf("kgdb detached\n");
			/* fall through */
		case KGDB_CONT:
			kgdb_send(out, 0, 0);
			frame->f_sr &=~ PSL_T;
			return (1);

		case KGDB_STEP:
			kgdb_send(out, 0, 0);
			frame->f_sr |= PSL_T;
			return (1);

		case KGDB_EXEC:
		default:
			/* Unknown command.  Ack with a null message. */
			outlen = 0;
			break;
		}
		/* Send the reply */
		kgdb_send(out, outbuffer, outlen);
	}
}

/*
 * XXX do kernacc call if safe, otherwise attempt
 * to simulate by simple bounds-checking.
 */
kgdb_acc(addr, len, rw)
	caddr_t addr;
	int len, rw;
{
	extern char proc0paddr[], kstack[];	/* XXX */
	extern char *kernel_map;		/* XXX! */

	if (kernel_map != NULL)
		return (kernacc(addr, len, rw));
	if (addr < proc0paddr + UPAGES * NBPG  ||
	    kstack <= addr && addr < kstack + UPAGES * NBPG)
		return (1);
	return (0);
}
#endif /* KGDB */
