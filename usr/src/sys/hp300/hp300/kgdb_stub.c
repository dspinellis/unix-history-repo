/*
 * Copyright (c) 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kgdb_stub.c	7.5 (Berkeley) %G%
 */
/*
 * "Stub" to allow remote cpu to debug over a serial line using gdb.
 */
#ifdef KGDB
#ifndef lint
static char rcsid[] = "$Header: kgdb_stub.c,v 1.6 91/03/05 01:15:03 van Exp $";
#endif

#include "param.h"
#include "systm.h"
#include "machine/trap.h"
#include "machine/cpu.h"
#include "machine/psl.h"
#include "machine/reg.h"
#include "frame.h"
#include "buf.h"
#include "../hp300/cons.h"

#include "kgdb_proto.h"
#include "machine/remote-sl.h"

extern void printf();
extern void bcopy();
extern int kernacc();
extern void chgkprot();

/* (XXX from trap.c) user-mode flag in type */
#define	USER	0x20

#ifndef KGDBDEV
#define KGDBDEV -1
#endif
#ifndef KGDBRATE
#define KGDBRATE 9600
#endif

int kgdb_dev = KGDBDEV;		/* remote debugging device (-1 if none) */
int kgdb_rate = KGDBRATE;	/* remote debugging baud rate */
int kgdb_active = 0;            /* remote debugging active if != 0 */
int kgdb_debug_init = 0;	/* != 0 waits for remote at system init */
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

	csum = type;
	PUTESC(type)

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
		if (++len > SL_MAXMSG) {
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

	switch (type &~ USER) {
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
 * Definitions exported from gdb.
 */
#define NUM_REGS 18
#define REGISTER_BYTES ((16+2)*4)
#define REGISTER_BYTE(N)  ((N)*4)

#define GDB_SR 16
#define GDB_PC 17

/*
 * # of additional bytes in 680x0 exception frame format n.
 */
static int frame_bytes[16] = {
	0, 0, sizeof(struct fmt2), 0,
	0, 0, 0, 0,
	0, sizeof(struct fmt9), sizeof(struct fmtA), sizeof(struct fmtB),
	0, 0, 0, 0
};

/*
 * Translate the values stored in the kernel frame struct to the format
 * understood by gdb.
 */
static void
regs_to_gdb(fp, gdb_regs)
	struct frame *fp;
	u_long *gdb_regs;
{
	bcopy((caddr_t)fp->f_regs, (caddr_t)gdb_regs, sizeof(fp->f_regs) + 8);
}

/*
 * Convert gdb register values to kernel format.
 */
static void
gdb_to_regs(fp, gdb_regs)
	struct frame *fp;
	u_long *gdb_regs;
{
	bcopy((caddr_t)gdb_regs, (caddr_t)fp->f_regs, sizeof(fp->f_regs) - 4);
	fp->f_sr = gdb_regs[GDB_SR];
	fp->f_pc = gdb_regs[GDB_PC];
}

static u_long reg_cache[NUM_REGS];

/*
 * This function does all command procesing for interfacing to 
 * a remote gdb.
 */
int 
kgdb_trap(int type, unsigned code, unsigned v, struct frame *frame)
{
	int i;
	u_long length;
	caddr_t addr;
	u_char *cp;
	int out, in;
	int inlen, outlen;
	static u_char inbuffer[SL_MAXMSG+1];
	static u_char outbuffer[SL_MAXMSG];
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
		kgdb_getc = constab[major(kgdb_dev)].cn_getc;
		kgdb_putc = constab[major(kgdb_dev)].cn_putc;
		if (kgdb_getc == 0 || kgdb_putc == 0) {
			return (0);
		}
		kgdb_active = 1;
	}
	/*
	 * Stick frame regs into our reg cache then tell remote host
	 * that an exception has occured.
	 */
	if ((type & USER) == 0)
		/*
		 * After a kernel mode trap, the saved sp points at the
		 * PSW and is useless.  The correct saved sp should be
		 * the top of the frame.
		 */
		frame->f_regs[SP] = (int)&frame->F_u +
				    frame_bytes[frame->f_format];
	regs_to_gdb(frame, gdb_regs);

	outbuffer[0] = computeSignal(type);
	kgdb_send(KGDB_SIGNAL, outbuffer, 1);

	while (1) {
		in = kgdb_recv(inbuffer, &inlen);
		if (in == 0 || (in & KGDB_ACK))
			/* Ignore inbound acks and error conditions. */
			continue;

		out = in | KGDB_ACK;
		switch (in) {

		case KGDB_SIGNAL:
			outbuffer[0] = computeSignal(type);
			outlen = 1;
			break;

		case KGDB_REG_R:
		case KGDB_REG_R | KGDB_DELTA:
			cp = outbuffer;
			outlen = 0;
			for (i = inbuffer[0]; i < NUM_REGS; ++i) {
				if (reg_cache[i] != gdb_regs[i] ||
				    (in & KGDB_DELTA) == 0) {
					if (outlen + 5 > SL_MAXMSG) {
						out |= KGDB_MORE;
						break;
					}
					cp[outlen] = i;
					bcopy(&gdb_regs[i], 
					      &cp[outlen + 1], 4);
					outlen += 5;
					reg_cache[i] = gdb_regs[i];
				}
			}
			break;
			
		case KGDB_REG_W:
		case KGDB_REG_W | KGDB_DELTA:
			cp = inbuffer;
			for (i = 0; i < inlen; i += 5) {
				register int j = cp[i];

				bcopy(&cp[i + 1], &gdb_regs[j], 4);
				reg_cache[j] = gdb_regs[j];
			}
			gdb_to_regs(frame, gdb_regs);
			outlen = 0;
			break;
				
		case KGDB_MEM_R:
			length = inbuffer[0];
			bcopy(&inbuffer[1], &addr, 4);
			if (length + 1 > SL_MAXMSG) {
				outlen = 1;
				outbuffer[0] = E2BIG;
			} else if (!kernacc(addr, length, B_READ)) {
				outlen = 1;
				outbuffer[0] = EFAULT;
			} else {
				outlen = length + 1;
				outbuffer[0] = 0;
				bcopy(addr, &outbuffer[1], length);
			}
			break;

		case KGDB_MEM_W:
			length = inlen - 4;
			bcopy(inbuffer, &addr, 4);
			outlen = 1;
			if (!kernacc(addr, length, B_READ))
				outbuffer[0] = EFAULT;
			else {
				outbuffer[0] = 0;
				if (!kernacc(addr, length, B_WRITE))
					chgkprot(addr, length, B_WRITE);
				bcopy(&inbuffer[4], addr, length);
			}
			break;

		case KGDB_KILL:
			kgdb_active = 0;
			/* fall through */
		case KGDB_CONT:
			kgdb_send(out, 0, 0);
			frame->f_sr &=~ PSL_T;
			return (1);

		case KGDB_STEP:
			kgdb_send(out, 0, 0);
			frame->f_sr |= PSL_T;
			return (1);

		default:
			/* Unknown command.  Ack with a null message. */
			outlen = 0;
			break;
		}
		/* Send the reply */
		kgdb_send(out, outbuffer, outlen);
	}
}
#endif
