/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kgdb_stub.c	7.4 (Berkeley) %G%
 *
 * from: $Header: kgdb_stub.c,v 1.13 92/11/26 03:04:55 torek Exp $
 */

/*
 * "Stub" to allow remote cpu to debug over a serial line using gdb.
 */
#ifdef KGDB

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>

#include <machine/ctlreg.h>
#include <machine/psl.h>
#include <machine/pte.h>
#include <machine/reg.h>
#include <machine/remote-sl.h>
#include <machine/trap.h>

#include <sparc/sparc/asm.h>
#include <sparc/sparc/kgdb_proto.h>

#ifndef KGDBDEV
#define KGDBDEV -1
#endif
#ifndef KGDBRATE
#define KGDBRATE 38400
#endif

int kgdb_dev = KGDBDEV;		/* remote debugging device (-1 if none) */
int kgdb_rate = KGDBRATE;	/* remote debugging baud rate */
int kgdb_active = 0;		/* remote debugging active if != 0 */
int kgdb_debug_panic = 0;	/* != 0 waits for remote on panic */

#define	getpte(va)	lda(va, ASI_PTE)
#define	setpte(va, pte)	sta(va, ASI_PTE, pte)

/*
 * This little routine exists simply so that bcopy() can be debugged.
 */
kgdb_copy(src, dst, len)
	register char *src, *dst;
	register int len;
{

	while (--len >= 0)
		*dst++ = *src++;
}

static int (*kgdb_getc) __P((void *));
static void (*kgdb_putc) __P((void *, int));
static void *kgdb_ioarg;

#define GETC()	((*kgdb_getc)(kgdb_ioarg))
#define PUTC(c)	((*kgdb_putc)(kgdb_ioarg, c))
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

kgdb_attach(getfn, putfn, ioarg)
	int (*getfn) __P((void *));
	void (*putfn) __P((void *, int));
	void *ioarg;
{

	kgdb_getc = getfn;
	kgdb_putc = putfn;
	kgdb_ioarg = ioarg;
}

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
		PUTESC(type);
	}
	csum = -csum;
	PUTESC(csum);
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
		c = GETC();
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
			if (csum != 0)
				return (0);
			*lenp = len;
			return (type);
		}
		csum += c;
		if (type < 0) {
			type = c;
			escape = 0;
			continue;
		}
		if (++len > SL_RPCSIZE) {
			while (GETC() != FRAME_END)
				continue;
			return (0);
		}
		*bp++ = c;
		escape = 0;
	}
}

/*
 * Translate a trap number into a unix compatible signal value.
 * (gdb only understands unix signal numbers).
 * ### should this be done at the other end?
 */
static int 
computeSignal(type)
	int type;
{
	int sigval;

	switch (type) {

	case T_AST:
		sigval = SIGINT;
		break;

	case T_TEXTFAULT:
	case T_DATAFAULT:
		sigval = SIGSEGV;
		break;

	case T_ALIGN:
		sigval = SIGBUS;
		break;

	case T_ILLINST:
	case T_PRIVINST:
	case T_DIV0:
		sigval = SIGILL;
		break;

	case T_FPE:
		sigval = SIGFPE;
		break;

	case T_BREAKPOINT:
		sigval = SIGTRAP;
		break;

	case T_KGDB_EXEC:
		sigval = SIGIOT;
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

	if (kgdb_dev < 0 || kgdb_getc == NULL)
		return;
	fb_unblank();
	if (verbose)
		printf("kgdb waiting...");
	__asm("ta %0" :: "n" (T_KGDB_EXEC));	/* trap into kgdb */
}

/*
 * Decide what to do on panic.
 */
kgdb_panic()
{

	if (kgdb_dev >= 0 && kgdb_getc != NULL &&
	    kgdb_active == 0 && kgdb_debug_panic)
		kgdb_connect(1);
}

/*
 * Definitions exported from gdb (& then made prettier).
 */
#define	GDB_G0		0
#define	GDB_O0		8
#define	GDB_L0		16
#define	GDB_I0		24
#define	GDB_FP0		32
#define	GDB_Y		64
#define	GDB_PSR		65
#define	GDB_WIM		66
#define	GDB_TBR		67
#define	GDB_PC		68
#define	GDB_NPC		69
#define	GDB_FSR		70
#define	GDB_CSR		71

#define NUM_REGS 72

#define REGISTER_BYTES		(NUM_REGS * 4)
#define REGISTER_BYTE(n)	((n) * 4)

/*
 * Translate the values stored in the kernel regs struct to the format
 * understood by gdb.
 */
void
regs_to_gdb(tf, gdb_regs)
	struct trapframe *tf;
	u_long *gdb_regs;
{

	/* %g0..%g7 and %o0..%o7: from trapframe */
	gdb_regs[0] = 0;
	kgdb_copy((caddr_t)&tf->tf_global[1], (caddr_t)&gdb_regs[1], 15 * 4);

	/* %l0..%l7 and %i0..%i7: from stack */
	kgdb_copy((caddr_t)tf->tf_out[6], (caddr_t)&gdb_regs[GDB_L0], 16 * 4);

	/* %f0..%f31 -- fake, kernel does not use FP */
	bzero((caddr_t)&gdb_regs[GDB_FP0], 32 * 4);

	/* %y, %psr, %wim, %tbr, %pc, %npc, %fsr, %csr */
	gdb_regs[GDB_Y] = tf->tf_y;
	gdb_regs[GDB_PSR] = tf->tf_psr;
	gdb_regs[GDB_WIM] = tf->tf_global[0];	/* input only! */
	gdb_regs[GDB_TBR] = 0;			/* fake */
	gdb_regs[GDB_PC] = tf->tf_pc;
	gdb_regs[GDB_NPC] = tf->tf_npc;
	gdb_regs[GDB_FSR] = 0;			/* fake */
	gdb_regs[GDB_CSR] = 0;			/* fake */
}

/*
 * Reverse the above.
 */
void
gdb_to_regs(tf, gdb_regs)
	struct trapframe *tf;
	u_long *gdb_regs;
{

	kgdb_copy((caddr_t)&gdb_regs[1], (caddr_t)&tf->tf_global[1], 15 * 4);
	kgdb_copy((caddr_t)&gdb_regs[GDB_L0], (caddr_t)tf->tf_out[6]);
	tf->tf_y = gdb_regs[GDB_Y];
	tf->tf_psr = gdb_regs[GDB_PSR];
	tf->tf_pc = gdb_regs[GDB_PC];
	tf->tf_npc = gdb_regs[GDB_NPC];
}

static u_long reg_cache[NUM_REGS];
static u_char inbuffer[SL_RPCSIZE];
static u_char outbuffer[SL_RPCSIZE];

/*
 * This function does all command procesing for interfacing to 
 * a remote gdb.
 */
int 
kgdb_trap(type, tf)
	int type;
	register struct trapframe *tf;
{
	register u_long len;
	caddr_t addr;
	register u_char *cp;
	register u_char out, in;
	register int outlen;
	int inlen;
	u_long gdb_regs[NUM_REGS];

	if (kgdb_dev < 0 || kgdb_getc == NULL) {
		/* not debugging */
		return (0);
	}
	if (kgdb_active == 0) {
		if (type != T_KGDB_EXEC) {
			/* No debugger active -- let trap handle this. */
			return (0);
		}

		/*
		 * If the packet that woke us up isn't an exec packet,
		 * ignore it since there is no active debugger.  Also,
		 * we check that it's not an ack to be sure that the 
		 * remote side doesn't send back a response after the
		 * local gdb has exited.  Otherwise, the local host
		 * could trap into gdb if it's running a gdb kernel too.
		 */
		in = GETC();
		/*
		 * If we came in asynchronously through the serial line,
		 * the framing character is eaten by the receive interrupt,
		 * but if we come in through a synchronous trap (i.e., via
		 * kgdb_connect()), we will see the extra character.
		 */
		if (in == FRAME_START)
			in = GETC();

		if (KGDB_CMD(in) != KGDB_EXEC || (in & KGDB_ACK) != 0)
			return (0);
		while (GETC() != FRAME_END)
			continue;
		/*
		 * Do the printf *before* we ack the message.  This way
		 * we won't drop any inbound characters while we're 
		 * doing the polling printf.
		 */
		printf("kgdb started from device %x\n", kgdb_dev);
		kgdb_send(in | KGDB_ACK, (u_char *)0, 0);
		kgdb_active = 1;
	}
	if (type == T_KGDB_EXEC) {
		/* bypass the trap instruction that got us here */
		tf->tf_pc = tf->tf_npc;
		tf->tf_npc += 4;
	} else {
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
	/*
	 * Stick frame regs into our reg cache then tell remote host
	 * that an exception has occured.
	 */
	regs_to_gdb(tf, gdb_regs);

	for (;;) {
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
			 * loop here forever if the output line is
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
					kgdb_copy((caddr_t)&gdb_regs[len],
					    (caddr_t)&cp[outlen + 1], 4);
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

				kgdb_copy((caddr_t)&cp[len + 1],
				    (caddr_t)&gdb_regs[j], 4);
				reg_cache[j] = gdb_regs[j];
			}
			gdb_to_regs(tf, gdb_regs);
			outlen = 0;
			break;
				
		case KGDB_MEM_R:
			len = inbuffer[0];
			kgdb_copy((caddr_t)&inbuffer[1], (caddr_t)&addr, 4);
			if (len > SL_MAXDATA) {
				outlen = 1;
				outbuffer[0] = E2BIG;
			} else if (!kgdb_acc(addr, len, B_READ, 1)) {
				outlen = 1;
				outbuffer[0] = EFAULT;
			} else {
				outlen = len + 1;
				outbuffer[0] = 0;
				kgdb_copy(addr, (caddr_t)&outbuffer[1], len);
			}
			break;

		case KGDB_MEM_W:
			len = inlen - 4;
			kgdb_copy((caddr_t)inbuffer, (caddr_t)&addr, 4);
			outlen = 1;
			if (!kgdb_acc(addr, len, B_READ, 0))
				outbuffer[0] = EFAULT;
			else {
				outbuffer[0] = 0;
				if (!kgdb_acc(addr, len, B_WRITE, 0))
					kdb_mkwrite(addr, len);
				kgdb_copy((caddr_t)&inbuffer[4], addr, len);
			}
			break;

		case KGDB_KILL:
			kgdb_active = 0;
			printf("kgdb detached\n");
			/* FALLTHROUGH */

		case KGDB_CONT:
			kgdb_send(out, 0, 0);
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

extern int kernacc();
extern void chgkprot();
extern char *kernel_map;			/* XXX! */
extern char *curproc;				/* XXX! */

/*
 * XXX do kernacc and useracc calls if safe, otherwise use PTE protections.
 */
kgdb_acc(addr, len, rw, usertoo)
	caddr_t addr;
	int len, rw, usertoo;
{
	extern char end[];
	int pte;

	/* XXX icky: valid address but causes timeout */
	if (addr >= (caddr_t)0xfffff000)
		return (0);
	if (kernel_map != NULL) {
		if (kernacc(addr, len, rw))
			return (1);
		if (usertoo && curproc && useracc(addr, len, rw))
			return (1);
	}
	addr = (caddr_t)((int)addr & ~PGOFSET);
	for (; len > 0; len -= NBPG, addr += NBPG) {
		if (((int)addr >> PG_VSHIFT) != 0 &&
		    ((int)addr >> PG_VSHIFT) != -1)
			return (0);
		pte = getpte(addr);
		if ((pte & PG_V) == 0 || rw == B_WRITE && (pte & PG_W) == 0)
			return (0);
	}
	return (1);
}

kdb_mkwrite(addr, len)
	register caddr_t addr;
	register int len;
{

	if (kernel_map != NULL) {
		chgkprot(addr, len, B_WRITE);
		return;
	}
	addr = (caddr_t)((int)addr & ~PGOFSET);
	for (; len > 0; len -= NBPG, addr += NBPG)
		setpte(addr, getpte(addr) | PG_W);
}
#endif
