/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)kgdb_stub.c	7.3 (Berkeley) 6/22/90
 */

/*
 *
 *    The following gdb commands are supported:
 *
 * command          function                               Return value
 *
 *    g             return the value of the CPU registers  hex data or ENN
 *    G             set the value of the CPU registers     OK or ENN
 *
 *    mAA..AA,LLLL  Read LLLL bytes at address AA..AA      hex data or ENN
 *    MAA..AA,LLLL: Write LLLL bytes at address AA.AA      OK or ENN
 *
 *    c             Resume at current address              SNN   ( signal NN)
 *    cAA..AA       Continue at address AA..AA             SNN
 *
 *    s             Step one instruction                   SNN
 *    sAA..AA       Step one instruction from AA..AA       SNN
 *
 *    k             kill
 *
 *    ?             What was the last sigval ?             SNN   (signal NN)
 *
 * All commands and responses are sent with a packet which includes a
 * checksum.  A packet consists of
 *
 * $<packet info>#<checksum>.
 *
 * where
 * <packet info> :: <characters representing the command or response>
 * <checksum>    :: < two hex digits computed as modulo 256 sum of <packetinfo>>
 *
 * When a packet is received, it is first acknowledged with either '+' or '-'.
 * '+' indicates a successful transfer.  '-' indicates a failed transfer.
 *
 * Example:
 *
 * Host:                  Reply:
 * $m0,10#2a               +$00010203040506070809101112131415#42
 *
 ****************************************************************************/

#ifdef KGDB
#include "param.h"
#include "systm.h"
#include "trap.h"
#include "cpu.h"
#include "psl.h"
#include "reg.h"
#include "frame.h"
#include "buf.h"

extern void printf();
extern void bcopy();
extern int kernacc();
extern void chgkprot();

/* # of additional (beyond 4) bytes in 680x0 exception frame format n */
static int frame_bytes[16] = {
	4,	4,	8,	4,
	4,	4,	4,	4,
	54,	16,	28,	88,
	4,	4,	4,	4
};

#define USER    040             /* (XXX from trap.c) user-mode flag in type */

/*
 * BUFMAX defines the maximum number of characters in inbound/outbound
 * buffers.  At least NUMREGBYTES*2 are needed for register packets.
 */
#define BUFMAX 512

#ifndef KGDBDEV
#define KGDBDEV -1
#endif
#ifndef KGDBRATE
#define KGDBRATE 9600
#endif

int kgdb_dev = KGDBDEV;		/* remote debugging device (-1 if none) */
int kgdb_rate = KGDBRATE;	/* remote debugging baud rate */
int kgdb_debug_init = 0;	/* != 0 waits for remote at system init */
int kgdb_debug = 0;		/* > 0 prints command & checksum errors */

#include "../hp300/cons.h"

#define GETC	\
	(constab[major(kgdb_dev)].cn_getc ? \
		(*constab[major(kgdb_dev)].cn_getc)(kgdb_dev) : 0)
#define PUTC(c)	{ \
	if (constab[major(kgdb_dev)].cn_putc) \
		(*constab[major(kgdb_dev)].cn_putc)(kgdb_dev, c); \
}

static char hexchars[] = "0123456789abcdef";

/*
 * There are 180 bytes of registers on a 68020 w/68881.  Many of the fpa
 * registers are 12 byte (96 bit) registers.
 */
#define NUMREGBYTES 180

static char inbuffer[BUFMAX];
static char outbuffer[BUFMAX];

static inline int 
hex(ch)
	char ch;
{
	if ((ch >= '0') && (ch <= '9'))
		return (ch - '0');
	if ((ch >= 'a') && (ch <= 'f'))
		return (ch - ('a' - 10));
	return (0);
}

/* scan for the sequence $<data>#<checksum>     */
static void 
getpacket(char *buffer)
{
	unsigned char checksum;
	unsigned char xmitcsum;
	int i;
	int count;
	char ch;

	do {
		/*
		 * wait around for the start character, ignore all other
		 * characters
		 */
		while ((ch = GETC) != '$')
			;
		checksum = 0;
		count = 0;
		xmitcsum = 1;

		/* now, read until a # or end of buffer is found */
		while (count < BUFMAX) {
			ch = GETC;
			if (ch == '#')
				break;
			checksum = checksum + ch;
			buffer[count] = ch;
			count = count + 1;
		}
		buffer[count] = 0;

		if (ch == '#') {
			xmitcsum = hex(GETC) << 4;
			xmitcsum += hex(GETC);
			if (kgdb_debug && (checksum != xmitcsum)) {
				printf(
			"bad checksum.  My count = 0x%x, sent=0x%x. buf=%s\n",
					checksum, xmitcsum, buffer);
			}
			if (checksum != xmitcsum) {
				PUTC('-');	/* failed checksum */
			} else {
				PUTC('+');	/* successful transfer */
				/*
				 * if a sequence char is present, reply the
				 * sequence ID
				 */
				if (buffer[2] == ':') {
					PUTC(buffer[0]);
					PUTC(buffer[1]);
					/* remove sequence chars from buffer */
					for (i = 3; i <= count; ++i)
						buffer[i - 3] = buffer[i];
				}
			}
		}
	} while (checksum != xmitcsum);
}

/*
 * send the packet in buffer.  The host gets one chance to read it.  This
 * routine does not wait for a positive acknowledge.
 */
static void 
putpacket(char *buffer)
{
	unsigned char checksum;
	int count;
	char ch;

	/* $<packet info>#<checksum>. */
	do {
		PUTC('$');
		checksum = 0;
		count = 0;

		while (ch = buffer[count]) {
			PUTC(ch);
			checksum += ch;
			count += 1;
		}
		PUTC('#');
		PUTC(hexchars[checksum >> 4]);
		PUTC(hexchars[checksum & 15]);

	} while (0);	/* (GETC != '+'); */

}

static inline void 
debug_error(char *format, char *parm)
{
	if (kgdb_debug)
		printf(format, parm);
}

/*
 * Convert at most 'dig' digits of hex data in buf into a value.
 * Stop on non-hex char.  Return a pointer to next char in buf.
 */
static char *
hex2val(char *buf, int *val, int dig)
{
	int i, v;
	char ch;

	v = 0;
	for (i = dig; --i >= 0; ) {
		ch = *buf++;
		if ((ch >= '0') && (ch <= '9'))
			v = (v << 4) | (ch - '0');
		else if ((ch >= 'a') && (ch <= 'f'))
			v = (v << 4) | (ch - ('a' - 10));
		else {
			--buf;
			break;
		}
	}
	*val = v;
	return (buf);
}

/*
 * convert the integer value 'val' into 'dig' hex digits, placing
 * result in buf.  Return a pointer to the last char put in buf (null).
 */
static char *
val2hex(char *buf, int val, int dig)
{
	for (dig <<= 2; (dig -= 4) >= 0; )
		*buf++ = hexchars[(val >> dig) & 0xf];
	*buf = 0;
	return (buf);
}

/*
 * convert the memory pointed to by mem into hex, placing result in buf.
 * return a pointer to the last char put in buf (null).
 */
static char *
mem2hex(char *buf, char *mem, int count)
{
	if ((count & 1) || ((int)mem & 1)) {
		char ch;

		while(--count >= 0) {
			ch = *mem++;
			*buf++ = hexchars[ch >> 4];
			*buf++ = hexchars[ch & 15];
		}
	} else {
		u_short s;
		u_short *mp = (u_short *)mem;

		for (count >>= 1; --count >= 0; ) {
			s = *mp++;
			*buf++ = hexchars[(s >> 12) & 15];
			*buf++ = hexchars[(s >> 8) & 15];
			*buf++ = hexchars[(s >> 4) & 15];
			*buf++ = hexchars[s & 15];
		}
	}
	*buf = 0;
	return (buf);
}

/*
 * Convert the hex array pointed to by buf into binary to be placed in mem.
 * Return a pointer to next char in buf.
 */
static char *
hex2mem(char *buf, char *mem, int count)
{
	int i;
	unsigned char ch;

	for (i = 0; i < count; ++i) {
		ch = hex(*buf++) << 4;
		ch = ch + hex(*buf++);
		*mem++ = ch;
	}
	return (buf);
}

/*
 * Translate a trap number into a unix compatible signal value.
 * (gdb only understands unix signal numbers).
 */
static int 
computeSignal(int type)
{
	int sigval;

	switch (type &~ USER) {
	case T_BUSERR:
		sigval = SIGBUS;
		break;		/* bus error           */
	case T_ADDRERR:
		sigval = SIGBUS;
		break;		/* address error       */
	case T_ILLINST:
		sigval = SIGILL;
		break;		/* illegal instruction */
	case T_ZERODIV:
		sigval = SIGFPE;
		break;		/* zero divide         */
	case T_CHKINST:
		sigval = SIGFPE;
		break;		/* chk instruction     */
	case T_TRAPVINST:
		sigval = SIGFPE;
		break;		/* trapv instruction   */
	case T_PRIVINST:
		sigval = SIGILL;
		break;		/* privilege violation */
	case T_TRACE:
		sigval = SIGTRAP;
		break;		/* trace trap          */
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
		sigval = SIGIOT;
		break;
	default:
		sigval = SIGEMT;
		break;
	}
	return (sigval);
}

#define RESPOND(str) (bcopy(str, outbuffer, sizeof(str)))

/*
 * This function does all command procesing for interfacing to 
 * a remote gdb.
 */
int 
kgdb_trap(int type, unsigned code, unsigned v, struct frame *frame)
{
	int sigval;
	int addr, length;
	char *ptr;

	if (kgdb_dev < 0)
		/* not debugging */
		return (0);

	if (kgdb_debug)
		printf("type=%d, code=%d, vector=0x%x, pc=0x%x, sr=0x%x\n",
			type, code, frame->f_vector, frame->f_pc, frame->f_sr);

	/* reply to host that an exception has occurred */
	sigval = computeSignal(type);
	outbuffer[0] = 'S';
	(void)val2hex(&outbuffer[1], sigval, 2);
	putpacket(outbuffer);

	while (1) {
		outbuffer[0] = 0;
		getpacket(inbuffer);
		ptr = inbuffer;
		switch (*ptr++) {
		case '?':
			outbuffer[0] = 'S';
			(void)val2hex(&outbuffer[1], sigval, 2);
			break;
		case 'g':	/* return the value of the CPU registers */
			ptr = outbuffer;
			if (type & USER)
				ptr = mem2hex(ptr, (char *)frame->f_regs,
					      sizeof(frame->f_regs));
			else {
				ptr = mem2hex(ptr, (char *)frame->f_regs,
					      sizeof(frame->f_regs) - 4);
				addr = (int)&frame->f_pc -
					frame_bytes[frame->f_format];
				ptr = mem2hex(ptr, (char *)&addr, sizeof(addr));
			}
			addr = frame->f_sr;
			ptr = mem2hex(ptr, (char *)&addr, sizeof(addr));
			ptr = mem2hex(ptr, (char *)&frame->f_pc,
				      sizeof(frame->f_pc));
			break;
		case 'G':	/* set the value of the CPU registers */
			ptr = hex2mem(ptr, (char *)frame->f_regs,
				      sizeof(frame->f_regs) - 4);
			ptr = hex2mem(ptr, (char *)&addr, sizeof(addr));
			ptr = hex2mem(ptr, (char *)&addr, sizeof(addr));
			frame->f_sr = addr;
			ptr = hex2mem(ptr, (char *)&frame->f_pc,
				      sizeof(frame->f_pc));
			RESPOND("OK");
			break;

			/* mAA..AA,LLLL  Read LLLL bytes at address AA..AA */
		case 'm':
			ptr = hex2val(ptr, &addr, 8);
			if (*ptr++ != ',') {
				RESPOND("E01");
				debug_error("malformed read memory cmd: %s\n",
					    inbuffer);
				break;
			}
			ptr = hex2val(ptr, &length, 8);
			if (length <= 0 || length >= BUFMAX*2) {
				RESPOND("E02");
				if (kgdb_debug)
					printf("bad read memory length: %d\n",
					       length);
				break;
			}
			if (! kernacc(addr, length, B_READ)) {
				RESPOND("E03");
				if (kgdb_debug)
					printf("read access violation addr 0x%x len %d\n", addr, length);
				break;
			}
			(void)mem2hex(outbuffer, (char *)addr, length);
			break;

			/*
			 * MAA..AA,LLLL: Write LLLL bytes at address AA.AA
			 * return OK
			 */
		case 'M':
			ptr = hex2val(ptr, &addr, 8);
			if (*ptr++ != ',') {
				RESPOND("E01");
				debug_error("malformed write memory cmd: %s\n",
					    inbuffer);
				break;
			}
			ptr = hex2val(ptr, &length, 8);
			if (*ptr++ != ':') {
				RESPOND("E01");
				debug_error("malformed write memory cmd: %s\n",
					    inbuffer);
				break;
			}
			if (length <= 0 || length >= BUFMAX*2 - 32) {
				RESPOND("E02");
				if (kgdb_debug)
					printf("bad write memory length: %d\n",
					       length);
				break;
			}
			if (! kernacc(addr, length, B_READ)) {
				RESPOND("E03");
				if (kgdb_debug)
					printf("write access violation addr 0x%x len %d\n", addr, length);
				break;
			}
			if (! kernacc(addr, length, B_WRITE))
				chgkprot(addr, length, B_WRITE);
			(void)hex2mem(ptr, (char *)addr, length);
			RESPOND("OK");
			break;

			/*
			 * cAA..AA  Continue at address AA..AA
			 * sAA..AA  Step one instruction from AA..AA
			 * (addresses optional)
			 */
		case 'c':
		case 's':
			/*
			 * try to read optional start address.
			 */
			if (ptr != hex2val(ptr, &addr, 8)) {
				frame->f_pc = addr;
				if (kgdb_debug)
					printf("new pc = 0x%x\n", addr);
			}
			/* deal with the trace bit */
			if (inbuffer[0] == 's')
				frame->f_sr |= PSL_T;
			else
				frame->f_sr &=~ PSL_T;

			if (kgdb_debug)
				printf("restarting at 0x%x\n", frame->f_pc);

			return (1);

			/* kill the program (same as continue for now) */
		case 'k':
			return (1);
		}
		/* reply to the request */
		putpacket(outbuffer);
	}
}
#endif
