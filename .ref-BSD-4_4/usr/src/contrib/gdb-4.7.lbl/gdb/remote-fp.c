/*
 * Memory-access and commands for remote kbox (Kinetics ethernet/appletalk
 * bridge), for GDB. Copyright (C) 1988 Free Software Foundation, Inc.
 *
 * GDB is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY.  No author or distributor accepts responsibility to anyone for
 * the consequences of using it or for whether it serves any particular
 * purpose or works at all, unless he says so in writing. Refer to the GDB
 * General Public License for full details.
 *
 * Everyone is granted permission to copy, modify and redistribute GDB, but only
 * under the conditions described in the GDB General Public License.  A copy
 * of this license is supposed to have been given to you along with GDB so
 * you can know your rights and responsibilities.  It should be in a file
 * named COPYING.  Among other things, the copyright notice and this notice
 * must be preserved on all copies.
 *
 * In other words, go ahead and share GDB, but don't try to stop anyone else
 * from sharing it farther.  Help stamp out software hoarding!
 */

/*
 * Written by Van Jacobson (van@helios.ee.lbl.gov)
 * Sat Jan  7 03:42:57 PST 1989:
 *
 * This is a first cut at remote debugging suport for a fastpath.  All
 * this version can do is read & write memory in the box.  Support
 * for writing regs or continue/resume really requires support in
 * the prom ethernet driver or it's too easy to step on yourself.
 *
 * To use a version of gdb that contains this code (which we typically
 * call kbdb for "kbox debugger") do the following:
 *
 *   kbdb gw.sym	(gw.sym is the a.out form of the .srec currently
 *			 downloaded into the kbox)
 *   (gdb) attach @foo	("foo" is the host name or ip address of the
 *			 kbox you want to debug)
 *
 * kbdb will print out a few lines including "Program received signal 5,
 * Trace/BPT trap" and an address (usually ip4me in gw2.c).  The "signal
 * 5" has to do with fooling gdb into thinking that the attach worked.
 * Everything else is legit (ie, the registers, stack, etc., should
 * indicate that you're in the piece of code that processes incoming
 * "debug" packets.  You can now do all the normal gdb things (bt,
 * print, set, etc.) except set breakpoints or change the execution
 * path.  If you type "return" it will appear to work (but doesn't
 * really).  If you type "continue", you'll get the "Program received
 * signal 5," again.  When you quit, you'll get the usual confirmer:
 * "The program is running.  Quit anyway? (y or n)".  Just answer
 * yes.
 *
 * This code will only work with the LBL modified KIP code (it
 * requires some opcodes we added to the gateway debugging protocol).
 */

#include <stdio.h>
#include <signal.h>

#include "defs.h"
#include "frame.h"
#include "inferior.h"

#include "remote.h"

#include "wait.h"
#include <a.out.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <strings.h>
#include <netdb.h>

#include "kgdb_proto.h"

/*
 * KIP fastpath gateway debug protocol
 */
struct gwdb {
	u_long	magic;		/* magic number */
	u_char	op,seq;		/* op code, sequence number */
	u_short	count;		/* byte count */
	u_long	address;	/* address of read/write */
	u_char	data[512];
};

/* Maximum number of bytes that can be read or written */
#define	FP_MAXDATA	sizeof(((struct gwdb *)0)->data)
/* errno byte + data */
#define FP_RPCSIZE (1 + FP_MAXDATA)

#define	gwdbMagic	((u_long)0xFF068020)
#define	gwdbPort	900	/* udp port number */
/* op codes */
#define	gwdbRead	1
#define	gwdbWrite	2
#define	gwdbCall	3
#define	gwdbStats	4
#define	gwdbState	5
#define	gwdbFrame	6

int kiodebug;

static struct sockaddr_in fsin = { AF_INET };	/* foreign sockaddr_in */
static int fsinlen;

/* Descriptor for I/O to remote machine.  */
int fp_desc = -1;

static int fp_send(), fp_recv();
static void fp_close();
static int fp_put(), fp_get();

/*
 * Open a connection to a remote debugger. NAME is the filename used for
 * communication.
 */
void
fp_open(name, remote_fnp)
	char *name;
	struct remote_fn *remote_fnp;
{
	register u_long iaddr;
	struct hostent *hp;

	if (*name >= '0' && *name <= '9' && (iaddr = inet_addr(name)) != -1) {
		fsin.sin_family = AF_INET;
		fsin.sin_addr.s_addr = iaddr;
	} else if (hp = gethostbyname(name)) {
		fsin.sin_family = hp->h_addrtype;
		bcopy(hp->h_addr, (caddr_t)&fsin.sin_addr, hp->h_length);
	} else {
		error("Bad hostname/ip-address \"%s\"", name);
		/* NOTREACHED */
	}
	fp_desc = socket(AF_INET, SOCK_DGRAM, 0);
	if (fp_desc < 0) {
		perror_with_name(name);
		/* NOTREACHED */
	}

	fsin.sin_port = gwdbPort;

	remote_fnp->send = fp_send;
	remote_fnp->recv = fp_recv;
	remote_fnp->close = fp_close;
	remote_fnp->maxdata = FP_MAXDATA;
	remote_fnp->rpcsize = FP_RPCSIZE;
}

/*
 * Close down connection to remote debugger.
 */
static void
fp_close()
{
	if (fp_desc < 0)
		printf("fp_close(): fp_desc not open!\n");
	else {
		(void)close(fp_desc);
		fp_desc = -1;
	}
}

static int fp_lasttype;

static int
fp_send(type, bp, len)
	register u_char type;
	register u_char *bp;
	register int len;
{
	register int ret;
	static struct gwdb dbpacket;
	register struct gwdb *dp = &dbpacket;

	fp_lasttype = type;
	switch(KGDB_CMD(type)) {

	case KGDB_MEM_R:
		/* Read memory */
		if (len != 5)
			printf("fp_send: MEM_R: bad len (%d != 5)\n", len);
		dp->magic = gwdbMagic;
		dp->op = gwdbRead;
		dp->seq = 1;
		dp->count = *bp++;
		bcopy((caddr_t)bp, (caddr_t)&dp->address, sizeof(dp->address));

		if (ret = fp_put(dp))
			return(ret);
		break;

	case KGDB_MEM_W:
		/* Write memory */
		if (len < 5)
			printf("fp_send: MEM_W: bad len (%d < 5)\n", len);
		dp->magic = gwdbMagic;
		dp->op = gwdbWrite;
		dp->seq = 1;
		bcopy((caddr_t)bp, (caddr_t)&dp->address, sizeof(dp->address));
		bp += sizeof(dp->address);
		len -= sizeof(dp->address);
		dp->count = len;
		bcopy((caddr_t)bp, (caddr_t)dp->data, len);

		if (ret = fp_put(dp))
			return(ret);
		break;

	case KGDB_REG_R:
		/* Read registers */
		dp->magic = gwdbMagic;
		dp->op = gwdbFrame;
		dp->seq = 1;
		dp->address = 0;
		dp->count = REGISTER_BYTES;

		if (ret = fp_put(dp))
			return(ret);
		break;
	}
	return(0);
}

static int
fp_recv(tp, ip, lenp, to)
	int *tp;
	register u_char *ip;
	int *lenp;
	int to;
{
	register int type;
	register int i, ret;
	static struct gwdb dbpacket;
	register struct gwdb *dp = &dbpacket;
	char *fmt = "(fastpath %s not implemented)\n";

	if (lenp)
		*lenp = 0;
	type = KGDB_CMD(fp_lasttype);
	*tp = type | (fp_lasttype & KGDB_SEQ)  | KGDB_ACK;

	switch (type) {

	case KGDB_MEM_R:
		if (ret = fp_get(dp, to))
			return(ret);
		if (ip) {
			/*
			 * Build a rpc KGDB_MEM_R reply. This is just
			 * an errno (which is always zero in the case
			 * of a fastpath) and the data.
			 */
			ip[0] = 0;
			bcopy((caddr_t)dp->data, (caddr_t)&ip[1], dp->count);
			if (lenp)
				*lenp = dp->count + 1;	/* rpc reply size */
		}
		break;

	case KGDB_MEM_W:
		if (ret = fp_get(dp, to))
			return(ret);
		break;

	case KGDB_REG_R:
		if (ret = fp_get(dp, to))
			return(ret);
		    if (ip) {
			    register u_char *bp;

			bp = dp->data;
			for (i = 0; i < dp->count / sizeof(int); i++) {
				*ip++ = i;
				bcopy((caddr_t)bp, (caddr_t)ip, sizeof(int));
				ip += sizeof(int);
				bp += sizeof(int);
			}
			if (lenp)
				*lenp = i * (sizeof(int) + 1);
		}
		break;

	case KGDB_REG_W:
		printf(fmt, "REG_W");
		break;

	case KGDB_CONT:
		/* Next time, respond with a "signal" */
		fp_lasttype = KGDB_SIGNAL;
		printf(fmt, "CONT");
		break;

	case KGDB_STEP:
		printf(fmt, "STEP");
		break;

	case KGDB_KILL:
		printf(fmt, "KILL");
		break;

	case KGDB_SIGNAL:
		printf(fmt, "SIGNAL");
		if (ip) {
			/* Build a fake rpc KGDB_SIGNAL reply */
			ip[0] = SIGTRAP;
			if (lenp)
				*lenp = 1;
		}
		break;

	case KGDB_EXEC:
		printf(fmt, "EXEC");
		break;

	default:
		printf("fp_send(): message type 0x%x unknown\n", fp_lasttype);
		break;
	}
	return(0);
}

/*
 * Send the command in BUF to the remote machine, and read the reply into
 * BUF. Report an error if we get an error reply.
 */
static int
fp_put(dp)
	struct gwdb *dp;
{

	if (sendto(fp_desc, (caddr_t)dp, sizeof(*dp), 0,
	    (struct sockaddr *)&fsin, sizeof(fsin)) < 0) {
		perror("fp_put(): sendto");
		return(EKGDB_IO);
	}
	return(0);
}

#ifndef FD_SETSIZE
	/* Gag. */
#define FD_SET(n, p)	((p)->fds_bits[0] |= (1<<(n)))
#define FD_CLR(n, p)	((p)->fds_bits[0] &= ~(1<<(n)))
#define FD_ISSET(n, p)	((p)->fds_bits[0] & (1<<(n)))
#define FD_ZERO(p)	((p)->fds_bits[0] = 0)
#endif

static int
fp_get(dp, to)
	struct gwdb *dp;
	int to;
{
	register int n;
	struct timeval timeout;
	fd_set readfds;

	/* Wait for data to show up */
	timeout.tv_sec = to / 1000;
	timeout.tv_usec = to % 1000;
	FD_ZERO(&readfds);
	FD_SET(fp_desc, &readfds);
	n = select(fp_desc + 1, &readfds, (fd_set*)0, (fd_set*)0, &timeout);
	/* XXX should probably check for EINTR */
	if (n < 0) {
		perror("fp_get(): select");
		return(EKGDB_IO);
	}
	if (n == 0) {
		fprintf(stderr, "fp_get(): timeout\n");
		return(EKGDB_TIMEOUT);
	}

	fsinlen = sizeof(fsin);
	bzero((caddr_t)dp, sizeof(*dp));
	n = recvfrom(fp_desc, (caddr_t)dp, sizeof(*dp), 0,
	    (struct sockaddr *)&fsin, &fsinlen);
	if (n < 0) {
		perror("fp_get(): recvfrom");
		return(EKGDB_IO);
	}

	/* Sanity checks */
	if (n < sizeof(*dp))
		return(EKGDB_RUNT);
	if (n > sizeof(*dp))
		return(EKGDB_2BIG);
	if (dp->op == 0)
		return(EKGDB_BADOP);

	return(0);
}
