/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tape.c	5.1 (Berkeley) %G%";
#endif not lint

#include "dump.h"
#include <signal.h>

char	(*tblock)[TP_BSIZE];	/* Pointer to malloc()ed buffer for tape */
int	writesize;		/* Size of malloc()ed buffer for tape */
int	trecno = 0;
extern	int ntrec;		/* blocking factor on tape */

/*
 * Streaming dump mods (Caltech) - disk block reading and tape writing
 * are exported to several slave processes.  While one slave writes the
 * tape, the others read disk blocks; they pass control of the tape in
 * a ring via pipes.  The parent process traverses the filesystem and
 * sends daddr's, inode records, etc, through pipes to each slave.
 * Speed from Eagle to TU77 on VAX/780 is about 140 Kbytes/second.
 * #ifdef RDUMP version is CPU-limited to about 40 Kbytes/second.
 */
struct req {			/* instruction packets sent to slaves */
	daddr_t dblk;
	int count;
} *req;
int reqsiz;

#define SLAVES 3		/* 2 slaves read disk while 3rd writes tape */
#define LAG 2			/* Write behind by LAG tape blocks (rdump) */
int slavefd[SLAVES];		/* Pipes from master to each slave */
int rotor;			/* Current slave number */
int master;			/* Pid of master, for sending error signals */
int trace = 0;			/* Protocol trace; easily patchable with adb */
#define  tmsg	if (trace) msg

#ifdef RDUMP
extern int rmtape;
#endif

/*
 * Allocate tape buffer contiguous with the array of instruction packets,
 * so they can be written with a single write call in flusht().
 */
alloctape()
{

	writesize = ntrec * TP_BSIZE;
	reqsiz = ntrec * sizeof(struct req);
	req = (struct req *)malloc(reqsiz+writesize);	/* array of packets */
	tblock = (char (*)[TP_BSIZE]) &req[ntrec];	/* Tape buffer */
	return (req != NULL);
}

/*
 * Send special record to be put on tape
 */
taprec(dp)
	char *dp;
{

	tmsg("taprec %d\n", trecno);
	req[trecno].dblk = (daddr_t)0;
	req[trecno].count = 1;
	*(union u_spcl *)(*tblock++) = *(union u_spcl *)dp;
	spcl.c_tapea++;
	if (++trecno >= ntrec)
		flusht();
}

dmpblk(blkno, size)
	daddr_t blkno;
	int size;
{
	int tpblks, dblkno;
	register int avail;

	if (size % TP_BSIZE != 0)
		msg("bad size to dmpblk: %d\n", size);
	dblkno = fsbtodb(sblock, blkno);
	tpblks = size / TP_BSIZE;
	while ((avail = MIN(tpblks, ntrec - trecno)) > 0) {
		tmsg("dmpblk %d\n", avail);
		req[trecno].dblk = dblkno;
		req[trecno].count = avail;
		trecno += avail;
		spcl.c_tapea += avail;
		if (trecno >= ntrec)
			flusht();
		dblkno += avail * (TP_BSIZE / DEV_BSIZE);
		tpblks -= avail;
	}
}

int	nogripe = 0;

tperror() {
	if (pipeout) {
		msg("Tape write error on %s\n", tape);
		msg("Cannot recover\n");
		dumpabort();
		/* NOTREACHED */
	}
	msg("Tape write error on tape %d\n", tapeno);
	broadcast("TAPE ERROR!\n");
	if (!query("Do you want to restart?"))
		dumpabort();
	msg("This tape will rewind.  After it is rewound,\n");
	msg("replace the faulty tape with a new one;\n");
	msg("this dump volume will be rewritten.\n");
	nogripe = 1;
	close_rewind();
	Exit(X_REWRITE);
}

senderr()
{

	perror("  DUMP: pipe error in command to slave");
	dumpabort();
}

#ifdef RDUMP
tflush(cnt)
	int cnt;
{
	int i;

	for (i = 0; i < ntrec; i++)
		spclrec();
}
#endif RDUMP

flusht()
{
	int sig, siz = (char *)tblock - (char *)req;

	tmsg("flusht %d\n", siz);
	sig = sigblock(1<<SIGINT-1 | 1<<SIGIOT-1);  /* Don't interrupt write */
	if (write(slavefd[rotor], req, siz) != siz)
		senderr();
	sigsetmask(sig);
	if (++rotor >= SLAVES) rotor = 0;
	tblock = (char (*)[TP_BSIZE]) &req[ntrec];
	trecno = 0;
	asize += writesize/density;
	asize += 7;			/* inter-record gap (why fixed?) */
	blockswritten += ntrec;
	if (!pipeout && asize > tsize) {
		close_rewind();
		otape();
	}
	timeest();
}

rewind()
{
	register int f;

	if (pipeout)
		return;
	for (f = 0; f < SLAVES; f++)
		close(slavefd[f]);
	while (wait(NULL) >= 0)    ;	/* wait for any signals from slaves */
	msg("Tape rewinding\n");
#ifdef RDUMP
	rmtclose();
	while (rmtopen(tape, 0) < 0)
		sleep(10);
	rmtclose();
#else
	close(to);
	while ((f = open(tape, 0)) < 0)
		sleep (10);
	close(f);
#endif
}

close_rewind()
{
	rewind();
	if (!nogripe) {
		msg("Change Tapes: Mount tape #%d\n", tapeno+1);
		broadcast("CHANGE TAPES!\7\7\n");
	}
	while (!query("Is the new tape mounted and ready to go?"))
		if (query("Do you want to abort?"))
			dumpabort();
}

/*
 *	We implement taking and restoring checkpoints on the tape level.
 *	When each tape is opened, a new process is created by forking; this
 *	saves all of the necessary context in the parent.  The child
 *	continues the dump; the parent waits around, saving the context.
 *	If the child returns X_REWRITE, then it had problems writing that tape;
 *	this causes the parent to fork again, duplicating the context, and
 *	everything continues as if nothing had happened.
 */

otape()
{
	int	parentpid;
	int	childpid;
	int	status;
	int	waitpid;
	int	interrupt();

	parentpid = getpid();

    restore_check_point:
	signal(SIGINT, interrupt);
	/*
	 *	All signals are inherited...
	 */
	childpid = fork();
	if (childpid < 0) {
		msg("Context save fork fails in parent %d\n", parentpid);
		Exit(X_ABORT);
	}
	if (childpid != 0) {
		/*
		 *	PARENT:
		 *	save the context by waiting
		 *	until the child doing all of the work returns.
		 *	don't catch the interrupt
		 */
		signal(SIGINT, SIG_IGN);
#ifdef TDEBUG
		msg("Tape: %d; parent process: %d child process %d\n",
			tapeno+1, parentpid, childpid);
#endif TDEBUG
		while ((waitpid = wait(&status)) != childpid)
			msg("Parent %d waiting for child %d has another child %d return\n",
				parentpid, childpid, waitpid);
		if (status & 0xFF) {
			msg("Child %d returns LOB status %o\n",
				childpid, status&0xFF);
		}
		status = (status >> 8) & 0xFF;
#ifdef TDEBUG
		switch(status) {
			case X_FINOK:
				msg("Child %d finishes X_FINOK\n", childpid);
				break;
			case X_ABORT:
				msg("Child %d finishes X_ABORT\n", childpid);
				break;
			case X_REWRITE:
				msg("Child %d finishes X_REWRITE\n", childpid);
				break;
			default:
				msg("Child %d finishes unknown %d\n",
				    childpid, status);
				break;
		}
#endif TDEBUG
		switch(status) {
			case X_FINOK:
				Exit(X_FINOK);
			case X_ABORT:
				Exit(X_ABORT);
			case X_REWRITE:
				goto restore_check_point;
			default:
				msg("Bad return code from dump: %d\n", status);
				Exit(X_ABORT);
		}
		/*NOTREACHED*/
	} else {	/* we are the child; just continue */
#ifdef TDEBUG
		sleep(4);	/* allow time for parent's message to get out */
		msg("Child on Tape %d has parent %d, my pid = %d\n",
			tapeno+1, parentpid, getpid());
#endif
#ifdef RDUMP
		while ((to = rmtopen(tape, 2)) < 0)
#else
		while ((to = pipeout ? 1 : creat(tape, 0666)) < 0)
#endif
			if (!query("Cannot open tape.  Do you want to retry the open?"))
				dumpabort();

		enslave();  /* Share open tape file descriptor with slaves */

		asize = 0;
		tapeno++;		/* current tape sequence */
		newtape++;		/* new tape signal */
		spcl.c_volume++;
		spcl.c_type = TS_TAPE;
		spclrec();
		if (tapeno > 1)
			msg("Tape %d begins with blocks from ino %d\n",
				tapeno, ino);
	}
}

dumpabort()
{
	if (master != 0 && master != getpid())
		kill(master, SIGIOT);
	msg("The ENTIRE dump is aborted.\n");
	Exit(X_ABORT);
}

Exit(status)
{
#ifdef TDEBUG
	msg("pid = %d exits with status %d\n", getpid(), status);
#endif TDEBUG
	exit(status);
}

#define OK 020
char tok = OK;

enslave()
{
	int prev[2], next[2], cmd[2];	/* file descriptors for pipes */
	int i, j, ret, slavepid;

	master = getpid();
	signal(SIGPIPE, dumpabort);
	signal(SIGIOT, tperror); /* SIGIOT asks for restart from checkpoint */
	pipe(prev);
	for (i = rotor = 0; i < SLAVES; ++i) {
		if ((i < SLAVES - 1 && pipe(next) < 0) || pipe(cmd) < 0
				|| (slavepid = fork()) < 0) {
			perror("  DUMP: too many slaves");
			dumpabort();
		}
		if (i >= SLAVES - 1)
			next[1] = prev[1];	    /* Last slave loops back */
		slavefd[i] = cmd[1];
		if (slavepid == 0) {		    /* Slave starts up here */
			for (j = 0; j <= i; j++)
				close(slavefd[j]);
			if (i < SLAVES - 1) {
				close(prev[1]);
				close(next[0]);
			} else {		    /* Insert initial token */
				if ((ret = write(next[1], &tok, 1)) != 1)
					ringerr(ret, "cannot start token");
			}
			doslave(i, cmd[0], prev[0], next[1]);
			close(next[1]);
			j = read(prev[0], &tok, 1);   /* Eat the final token */
#ifdef RDUMP				    /* Read remaining acknowledges */
			for (; j > 0 && (tok &~ OK) > 0; tok--) {
				if (rmtwrite2() != writesize && (tok & OK)) {
					kill(master, SIGIOT);
					tok &= ~OK;
				}
			}
#endif
			Exit(X_FINOK);
		}
		close(cmd[0]);
		close(next[1]);
		close(prev[0]);
		prev[0] = next[0];
	}
	master = 0;
}

/*
 * Somebody must have died, should never happen
 */
ringerr(code, msg, a1, a2)
	int code;
	char *msg;
	int a1, a2;
{
	char buf[BUFSIZ];

	fprintf(stderr, "  DUMP: ");
	sprintf(buf, msg, a1, a2);
	if (code < 0)
		perror(msg);
	else if (code == 0)
		fprintf(stderr, "%s: unexpected EOF\n", buf);
	else
		fprintf(stderr, "%s: code %d\n", buf, code);
	kill(master, SIGPIPE);
	Exit(X_ABORT);
}

int childnum;
sigpipe()
{

	ringerr(childnum, "SIGPIPE raised");
}

doslave(num, cmd, prev, next)
	int num, cmd, prev, next;
{
	int ret;

	tmsg("slave %d\n", num);
	signal(SIGINT, SIG_IGN); 		/* Master handles it */
	signal(SIGTERM, SIG_IGN);
	signal(SIGPIPE, sigpipe);
	childnum = num;
	close(fi);
	if ((fi = open(disk, 0)) < 0) {		/* Need our own seek pointer */
		perror("  DUMP: can't reopen disk");
		kill(master, SIGPIPE);
		Exit(X_ABORT);
	}
	while ((ret = readpipe(cmd, req, reqsiz)) == reqsiz) {
		register struct req *p = req;
		for (trecno = 0; trecno < ntrec; trecno += p->count, p += p->count) {
			if (p->dblk) {
				tmsg("%d READS %d\n", num, p->count);
				bread(p->dblk, tblock[trecno],
				    p->count * TP_BSIZE);
			} else {
				tmsg("%d PIPEIN %d\n", num, p->count);
				if (p->count != 1)
					ringerr(11, "%d PIPEIN %d", num,
						p->count);
				if (readpipe(cmd, tblock[trecno], TP_BSIZE) != TP_BSIZE)
					senderr();
			}
		}
		if ((ret = read(prev, &tok, 1)) != 1)
			ringerr(ret, "read token");	/* Wait your turn */
		tmsg("%d WRITE\n", num);
#ifdef RDUMP
		if (tok & OK) {
			rmtwrite0(writesize);
			rmtwrite1(tblock[0], writesize);
			tok++;		/* Number of writes in progress */
		}
		if (tok > (LAG|OK) && (--tok, rmtwrite2() != writesize)) {
#else
		if ((tok & OK) &&
		    write(to, tblock[0], writesize) != writesize) {
			perror(tape);
#endif
			kill(master, SIGIOT);	/* restart from checkpoint */
			tok &= ~OK;
		}
		if ((ret = write(next, &tok, 1)) != 1)
			ringerr(ret, "write token"); /* Next slave's turn */
	}
	if (ret != 0)
		ringerr(ret, "partial record?");
	tmsg("%d CLOSE\n", num);
}

/*
 * Since a read from a pipe may not return all we asked for
 * we must loop until we get all we need
 */
readpipe(fd, buf, cnt)
	int fd;
	char *buf;
	int cnt;
{
	int rd, got;

	for (rd = cnt; rd > 0; rd -= got) {
		got = read(fd, buf, rd);
		if (got < 0)
			return (got);
		if (got == 0)
			return (cnt - rd);
		buf += got;
	}
	return (cnt);
}
