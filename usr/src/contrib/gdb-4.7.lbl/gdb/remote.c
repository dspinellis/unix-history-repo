/*
 * Copyright (c) 1990, 1991, 1992 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Lawrence Berkeley Laboratory,
 * Berkeley, CA.  The name of the University may not be used to
 * endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char rcsid[] = "$Header: remote.c,v 1.3 93/04/26 16:23:50 mccanne Exp $";
#endif

#include "defs.h"

#include <varargs.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/file.h>

#include "target.h"
#include "frame.h"
#include "inferior.h"
#include "wait.h"
#include "gdbcmd.h"
#include "remote.h"
#include "kgdb_proto.h"

extern struct target_ops remote_ops;	/* Forward decl */

static int remote_debugging;
static int remote_quiet;

static int to_clamp = 8000;
static int to_base = 1000;
/*
 * Eponentially back off a timer value.  Clamp the value at to_clamp.
 */
#define BACKOFF(to) ((2 * (to) < to_clamp) ? 2 * (to) : to_clamp)

static FILE *kiodebug;
static int icache = 1;

static int remote_cache_valid;
static int remote_instub;

static void remote_signal();
static void remote_debug();
static void print_msg();

static struct remote_fn remote_fn;

static u_char *inbuffer;
static u_char *outbuffer;

/*
 * Statistics.
 */
static int remote_ierrs;
static int remote_oerrs;
static int remote_seqerrs;
static int remote_spurious;

#define PUTCMD(cmd) m_xchg(cmd, (u_char *)0, 0, (u_char *)0, (int *)0)

/*
 * Send an outbound message to the remote machine and read the reply.
 * Either or both message buffers may be NULL.
 */
static int
m_xchg(type, out, outlen, in, inlen)
	int type;
	u_char *out;
	int outlen;
	u_char *in;
	int *inlen;
{
	register int err, nrecv;
	register int (*send)() = remote_fn.send, (*recv)() = remote_fn.recv;
	int ack;
	u_long to = to_base;
	static int seqbit = 0;

	if (!remote_instub) {
		remote_instub = 1;
		(void)PUTCMD(KGDB_EXEC);
	}

	seqbit ^= KGDB_SEQ;
	while (1) {
		nrecv = 0;
		err = (*send)(type | seqbit, out, outlen);
		if (err) {
			++remote_oerrs;
			if (kiodebug)
				remote_debug("send error %d\n", err);
			/* XXX shouldn't we retry the !@#$^*ing send? */
		}
		if (kiodebug)
			print_msg(type | seqbit, out, outlen, 'O');

	again:
		err = (*recv)(&ack, in, inlen, to);
		if (err != 0) {
			++remote_ierrs;
			if (kiodebug)
				remote_debug("recv error %d\n", err);
			remote_cache_valid = 0;
			if (err == EKGDB_TIMEOUT) {
				to = BACKOFF(to);
				if (to >= to_clamp)
					error("remote host not responding");
				continue;
			}
			if (++nrecv < 5)
				/*
				 * Try to receive five times before
				 * retransmitting.
				 */
				goto again;
			continue;
		} else if (kiodebug)
			print_msg(ack, in, inlen ? *inlen : 0, 'I');

		if ((ack & KGDB_ACK) == 0 || KGDB_CMD(ack) != KGDB_CMD(type)) {
			++remote_spurious;
			continue;
		}
		if ((ack & KGDB_SEQ) ^ seqbit) {
			++remote_seqerrs;
			goto again;
		}
		return ack;
	}
}

/*
 * Wait for the specified message type.  Discard anything else.
 * (this is used by 'remote-signal' to help us resync with other side.)
 */
static void
m_recv(type, in, inlen)
	int type;
	u_char *in;
	int *inlen;
{
	int reply, err;

	while (1) {
		err = (*remote_fn.recv)(&reply, in, inlen, -1);
		if (err) {
			++remote_ierrs;
			if (kiodebug)
				remote_debug("recv error %d\n", err);
		} else if (kiodebug)
			print_msg(reply, in, inlen ? *inlen : 0, 'I');

		if (KGDB_CMD(reply) == type)
			return;
		++remote_spurious;
	}
}

/*
 * Send a message.  Do not wait for *any* response from the other side.
 * Some other thread of control will pick up the ack that will be generated.
 */
static void
m_send(type, buf, len)
	int type;
	u_char *buf;
	int len;
{
	int err;

	if (!remote_instub) {
		remote_instub = 1;
		(void)PUTCMD(KGDB_EXEC);
	}

	err = (*remote_fn.send)(type, buf, len);
	if (err) {
		++remote_ierrs;
		if (kiodebug)
			remote_debug("[send error %d] ", err);
	}
	if (kiodebug)
		print_msg(type, buf, len, 'O');
}

void
start_remote()
{
	init_wait_for_inferior();
	remote_go();
}

void
restart_remote()
{
	init_wait_for_inferior_keep_brkpts();
#ifdef notdef
	inferior_pid = 3;
#endif
	remote_go();
}

static void
remote_bail(arg)
	void *arg;
{
	printf("Remote attach interrupted.\n");
	remote_quiet = 1;
	pop_target();
}

/*
 * Open a connection to a remote debugger.
 * NAME is the filename used for communication.
 */
void
remote_open(name, from_tty)
	char *name;
	int from_tty;
{
	extern void sl_open(), fp_open();
	void (*func)();
	struct cleanup *chain;

	if (name == 0)
		error (
"\
To open a remote debug connection, you need to specify what serial\n\
device is attached to the remote system (e.g. /dev/ttya).\
");
	if (remote_debugging) 
		error("Already remote debugging.  Detach first.\n");

	target_preopen(from_tty);
	/* Right now, we only support fastpath and serial debugging */

#ifndef NOFASTPATH
	if (*name == '@') {
		++name;
		func = fp_open;
	} else
#endif
		func = sl_open;

	bzero((caddr_t)&remote_fn, sizeof(remote_fn));
	(*func)(name, &remote_fn);

	if (from_tty)
		printf("Remote debugging using %s\n", name);
	remote_debugging = 1;

	remote_cache_valid = 0;

	/* remote protocol limits max data to one byte */
	if (remote_fn.maxdata > 255)
		remote_fn.maxdata = 255;

	/* allocate msg buffers */
	inbuffer = (u_char *)malloc(remote_fn.rpcsize);
	outbuffer = (u_char *)malloc(remote_fn.rpcsize);

	chain = make_cleanup(remote_bail, 0);
	push_target(&remote_ops);

	remote_ierrs = 0;
	remote_oerrs = 0;
	remote_spurious = 0;

	/*
	 * XXX
	 * Signal the remote kernel and set things up
	 * so gdb views it as a process.
	 */
	remote_signal();
	start_remote();

	discard_cleanups(chain);
}

/*
 * Take the remote kernel out of debugging mode.
 */
void
remote_kill()
{
	/* XXX
	 * Clear all breakpoints.
	 * It's a very, very bad idea to go away leaving
	 * breakpoints in a remote kernel or to leave it
	 * stopped at a breakpoint.
	 */
	clear_breakpoints();

	/*
	 * Take remote machine out of debug mode.
	 */
	(void)PUTCMD(KGDB_KILL);
	remote_debugging = 0;
}

/*
 * Close the open connection to the remote debugger. Use this when you want
 * to detach and do something else with your gdb.
 */
void
remote_detach(args, from_tty)
	char *args;
	int from_tty;
{
	if (args) {
		if (args[0] == 'q')
			remote_quiet = 1;
		else
			error("Bad argument to remote \"detach\".");
	}	
	/* this will invoke remote_close() */
	pop_target();
	if (from_tty)
		printf("Ending remote debugging.\n");
}

void
remote_close(quitting)
	int quitting;
{
#ifdef notdef
	if (!remote_debugging)
		error("Remote debugging not enabled");
#endif
	if (!remote_quiet) {
		remote_quiet = 0;
		remote_kill();
	} else
		remote_debugging = 0;

	(void)(*remote_fn.close)();

	free((caddr_t)inbuffer);
	free((caddr_t)outbuffer);
}

/*
 * Tell the remote machine to resume.
 */
void
remote_resume(step, sig)
	int step, sig;
{
#ifdef notdef
	if (sig != 0)
		error("Cannot signal a remote system");
#endif
	if (!step) {
		(void)PUTCMD(KGDB_CONT);
		remote_instub = 0;
	} else {
#ifdef NO_SINGLE_STEP
		single_step(0);
#else
		(void)PUTCMD(KGDB_STEP);
#endif
	}
}

/*
 * Wait until the remote machine stops, then return, storing status in STATUS
 * just as `wait' would.
 */
int
remote_wait(status)
	WAITTYPE *status;
{
	int len;

	WSETEXIT((*status), 0);
	/*
	 * When the machine stops, it will send us a KGDB_SIGNAL message,
	 * so we wait for one of these.
	 */
	m_recv(KGDB_SIGNAL, inbuffer, &len);
	WSETSTOP((*status), inbuffer[0]);
	/*
	 * Let the machine dependent module have a chance to 
	 * lookup current process context etc.
	 */
	set_curproc();

	return (0);
}

/*
 * Register context as of last remote_fetch_registers().
 */
static char reg_cache[REGISTER_BYTES];
extern char registers[];

/*
 * Read the remote registers into the block REGS.
 */
/* XXX Currently we just read all the registers, so we don't use regno.  */
/* ARGSUSED */
void
remote_fetch_registers(unused_regno)
	int unused_regno;
{
	int regno, len, rlen, ack;
	u_char *cp, *ep;

	regno = -1;
	do {
		outbuffer[0] = regno + 1;
		ack = m_xchg(remote_cache_valid ?
		    KGDB_REG_R|KGDB_DELTA : KGDB_REG_R,
		    outbuffer, 1, inbuffer, &len);
		cp = inbuffer;
		ep = cp + len;
		while (cp < ep) {
			regno = *cp++;
			rlen = REGISTER_RAW_SIZE(regno);
			bcopy((caddr_t)cp,
			    &reg_cache[REGISTER_BYTE(regno)], rlen);
			cp += rlen;
		}
	} while (ack & KGDB_MORE);

	remote_cache_valid = 1;
	bcopy(reg_cache, registers, REGISTER_BYTES);
}

/*
 * Store the remote registers from the contents of the block REGS.
 */
/* XXX Currently we just read all the registers, so we don't use regno.  */
/* ARGSUSED */
void
remote_store_registers(unused_regno)
	int unused_regno;
{
	char *regs = registers;
	u_char *cp, *ep;
	int regno, off, rlen;

	cp = outbuffer;
	ep = cp + remote_fn.maxdata;

	for (regno = 0; regno < NUM_REGS; ++regno) {
		off = REGISTER_BYTE(regno);
		rlen = REGISTER_RAW_SIZE(regno);
		if (!remote_cache_valid ||
		    bcmp(&regs[off], &reg_cache[off], rlen) != 0) {
			if (cp + rlen + 1 >= ep) {
				(void)m_xchg(KGDB_REG_W,
				    outbuffer, cp - outbuffer,
				    (u_char *)0, (int *)0);
				cp = outbuffer;
			}
			*cp++ = regno;
			bcopy(&regs[off], (caddr_t)cp, rlen);
			cp += rlen;
		}
	}
	if (cp != outbuffer)
		(void)m_xchg(KGDB_REG_W, outbuffer, cp - outbuffer,
		    (u_char *)0, (int *)0);

	bcopy(regs, reg_cache, REGISTER_BYTES);
}

/*
 * XXX DOES THE NEW PROTOCOL NEED THIS?
 * Prepare to store registers.  Since we send them all, we have to
 * read out the ones we don't want to change first.
 */
void 
remote_prepare_to_store ()
{
	remote_fetch_registers(-1);
}

/*
 * Store a chunk of memory into the remote host.
 * 'remote_addr' is the address in the remote memory space.
 * 'cp' is the address of the buffer in our space, and 'len' is
 * the number of bytes.  Returns an errno status.
 */
int
remote_write(addr, cp, len)
	CORE_ADDR addr;
	u_char *cp;
	int len;
{
	int cnt;

	while (len > 0) {
		/* XXX assumes sizeof(CORE_ADDR) is 4? */
		cnt = min(len, remote_fn.maxdata - 4);
		bcopy((caddr_t)&addr, (caddr_t)outbuffer, 4);
		SWAP_TARGET_AND_HOST(outbuffer, 4);
		bcopy((caddr_t)cp, (caddr_t)outbuffer + 4, cnt);
		(void)m_xchg(KGDB_MEM_W, outbuffer, cnt + 4, inbuffer, &len);

		if (inbuffer[0])
			return (inbuffer[0]);

		addr += cnt;
		cp += cnt;
		len -= cnt;
	}
	return 0;
}

/*
 * Read memory data directly from the remote machine.
 * 'addr' is the address in the remote memory space.
 * 'cp' is the address of the buffer in our space, and 'len' is
 * the number of bytes.  Returns an errno status.
 */
/* XXX is this really needed? */
static int
remote_read(addr, cp, len)
	CORE_ADDR addr;
	u_char *cp;
	int len;
{
	int cnt, inlen;

	while (len > 0) {
		cnt = min(len, remote_fn.maxdata);
		outbuffer[0] = cnt;
		bcopy((caddr_t)&addr, (caddr_t)&outbuffer[1], 4);
		SWAP_TARGET_AND_HOST(&outbuffer[1], 4);
		(void)m_xchg(KGDB_MEM_R, outbuffer, 5, inbuffer, &inlen);
		if (inlen < 1)
			error("remote_read(): remote protocol botch");

		/* Return errno from remote side */
		if (inbuffer[0])
			return (inbuffer[0]);

		--inlen;

		if (inlen <= 0)
			error("remote_read(): inlen too small (%d)", inlen);

		if (inlen > cnt) {
			printf(
		"remote_read(): warning: asked for %d, got %d\n",
			    cnt, inlen);
			inlen = cnt;
		}

		bcopy((caddr_t)&inbuffer[1], (caddr_t)cp, inlen);

		addr += inlen;
		cp += inlen;
		len -= inlen;
	}
	return (0);
}

static int
remote_read_text(addr, cp, len, target)
	CORE_ADDR addr;
	char *cp;
	int len;
	struct target_ops *target;
{
	register int cc;
	/*
	 * Look down the target stack (we're on top) for the text file.
	 * If it can transfer the whole chunk, let it.  Otherwise,
	 * revert to the remote call.
	 */
	for (; target != 0; target = target->to_next) {
		if (target->to_stratum != file_stratum)
			continue;
		cc = target->to_xfer_memory(addr, cp, len, 0, target);
		if (cc == len)
			return (0);
	}		
	return (remote_read(addr, cp, len));
}

/*
 * Read or write LEN bytes from inferior memory at MEMADDR, transferring
 * to or from debugger address MYADDR.  Write to inferior if SHOULD_WRITE is
 * nonzero.  Returns length of data written or read; 0 for error.
 */
/* ARGSUSED */
int
remote_xfer_memory(addr, cp, len, should_write, target)
	register CORE_ADDR addr;
	register char *cp;
	register int len;
	int should_write;
	struct target_ops *target;
{
	register int st;

	if (should_write)
		st = remote_write(addr, cp, len);
	else {
#ifdef NEED_TEXT_START_END
		extern CORE_ADDR text_start, text_end;
		st = (icache && addr >= text_start && addr + len <= text_end) ?
			remote_read_text(addr, cp, len, target) :
			remote_read(addr, cp, len);
#else
		st = remote_read(addr, cp, len);
#endif
	}
	return ((st == 0) ? len : 0);
}

/*
 * Signal the remote machine.  The remote end might be idle or it might
 * already be in debug mode -- we need to handle both case.  Thus, we use
 * the framing character as the wakeup byte, and send a SIGNAL packet.
 * If the remote host is idle, the framing character will wake it up.
 * If it is in the kgdb stub, then we will get a SIGNAL reply.
 */
static void
remote_signal()
{
	if (!remote_debugging)
		printf("Remote agent not active.\n");
	else {
		remote_instub = 0;
		m_send(KGDB_SIGNAL, (u_char *)0, 0);
	}
}

static void
remote_signal_command()
{
	if (!remote_debugging)
		error("Remote agent not active.\n");
	remote_cache_valid = 0;
	remote_signal();
	restart_remote();
}

/*
 * Print a message for debugging.
 */
static void
print_msg(type, buf, len, dir)
	int type;
	u_char *buf;
	int len;
	int dir;
{
	int i;
	char *s;

	switch (KGDB_CMD(type)) {
	case KGDB_MEM_R:	s = "memr"; break;
	case KGDB_MEM_W:	s = "memw"; break;
	case KGDB_REG_R:	s = "regr"; break;
	case KGDB_REG_W:	s = "regw"; break;
	case KGDB_CONT:		s = "cont"; break;
	case KGDB_STEP:		s = "step"; break;
	case KGDB_KILL:		s = "kill"; break;
	case KGDB_SIGNAL:	s = "sig "; break;
	case KGDB_EXEC:		s = "exec"; break;
	default:		s = "unk "; break;
	}
	remote_debug("%c %c%c%c%c %s (%02x): ", dir,
	    (type & KGDB_ACK) ? 'A' : '.',
	    (type & KGDB_DELTA) ? 'D' : '.',
	    (type & KGDB_MORE) ? 'M' : '.',
	    (type & KGDB_SEQ) ? '-' : '+',
	    s, type);
	if (buf)
		for (i = 0; i < len; ++i)
			remote_debug("%02x", buf[i]);
	remote_debug("\n");
}

static void
remote_debug_command(arg, from_tty)
	char *arg;
	int from_tty;
{
	char *name;

	if (kiodebug != 0 && kiodebug != stderr)
		(void)fclose(kiodebug);

	if (arg == 0) {
		kiodebug = 0;
		printf("Remote debugging off.\n");
		return;
	}
	if (arg[0] == '-') {
		kiodebug = stderr;
		name = "stderr";
	} else {
		kiodebug = fopen(arg, "w");
		if (kiodebug == 0) {
			printf("Cannot open '%s'.\n", arg);
			return;
		}
		name = arg;
	}
	printf("Remote debugging output routed to %s.\n", name);
}

/* ARGSUSED */
static void
remote_files_info()
{
	printf("Using %s for text references.\n",
		icache? "local executable" : "remote");
	printf("Protocol debugging is %s.\n", kiodebug? "on" : "off");
	printf("%d spurious input messages.\n", remote_spurious);
	printf("%d input errors; %d output errors; %d sequence errors.\n",
	    remote_ierrs, remote_oerrs, remote_seqerrs);
}

/* VARARGS */
static void
remote_debug(va_alist)
	va_dcl
{
	register char *cp;
	va_list ap;

	va_start(ap);
	cp = va_arg(ap, char *);
	(void)vfprintf(kiodebug, cp, ap);
	va_end(ap);
	fflush(kiodebug);
}

/* Define the target subroutine names */

struct target_ops remote_ops = {
	"remote",			/* shortname */
	"Remote serial target in gdb-specific protocol", /* longname */
	"Debug a remote host, using a gdb-specific protocol.\n\
Specify the target as an argument (e.g. /dev/ttya for a serial link).\n\
Use \"detach\" or \"quit\" to end remote debugging gracefully, which\n\
removes breakpoints from and resumes the remote kernel.\n\
Use \"detach quiet\" to end remote debugging with no negotiation.\n\
This latter method is desirable, for instance, when the remote kernel\n\
has crashed and messages from gdb could wreak havoc.\n",
	remote_open,		/* open */
	remote_close,		/* close */
	0,			/* attach */
	remote_detach,		/* detach */
	remote_resume,		/* resume */
	remote_wait,		/* wait */
	remote_fetch_registers,	/* fetch_registers */
	remote_store_registers,	/* store_registers */
	remote_prepare_to_store, /* prepare_to_store */
	remote_xfer_memory,	/* xfer_memory */
	remote_files_info,	/* files_info */
	0,			/* insert_breakpoint */
	0,			/* remove_breakpoint */
	0,			/* terminal_init */
	0,			/* terminal_inferior */
	0,			/* terminal_ours_for_output */
	0,			/* terminal_ours */
	0,			/* terminal_info */
	remote_kill,		/* kill */
	0,			/* load */
	0,			/* lookup_symbol */
	0,			/* create_inferior */
	0,			/* mourn_inferior */
	0,			/* can_run */
	0,			/* notice_signals */
	process_stratum,	/* stratum */
	0,			/* next */
	1,			/* has_all_memory */
	1,			/* has_memory */
	1,			/* has_stack */
	1,			/* has_registers */
	1,			/* has_execution XXX can't start an inferior */
	0,			/* sections */
	0,			/* sections_end */
	OPS_MAGIC,		/* magic */
};

void
_initialize_remote ()
{
	struct cmd_list_element *c;
	extern struct cmd_list_element *setlist;

	add_com("remote-signal", class_run, remote_signal_command,
		"If remote debugging, send interrupt signal to remote.");

	c = add_set_cmd ("remote-text-refs", class_support, var_boolean,
			 (char *)&icache,
"Set use of local executable for text segment references.\n\
If on, all memory read/writes go to remote.\n\
If off, text segment reads use the local executable.",
		&setlist);
	add_show_from_set (c, &showlist);

	c = add_set_cmd ("remote-timeout", class_support, var_uinteger,
			 (char *)&to_base,
"Set remote timeout interval (in msec).  The gdb remote protocol\n\
uses an exponential backoff retransmit timer that begins initialized\n\
to this value (on each transmission).",
		&setlist);
	add_show_from_set (c, &showlist);

	add_com("remote-debug", class_run, remote_debug_command,
"With a file name argument, enables output of remote protocol debugging\n\
messages to said file.  If file is `-', stderr is used.\n\
With no argument, remote debugging is disabled.");

#ifdef notdef
	add_info("remote", remote_info,
		 "Show current settings of remote debugging options.");
#endif
	add_target (&remote_ops);
}
