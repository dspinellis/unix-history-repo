/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)tape.c	5.2 (Berkeley) %G%";
#endif not lint

#include "dump.h"
#include <sys/file.h>

char	(*tblock)[TP_BSIZE];	/* Pointer to malloc()ed buffer for tape */
int	writesize;		/* Size of malloc()ed buffer for tape */
int	trecno = 0;
extern	int ntrec;		/* blocking factor on tape */
extern	int cartridge;
int	tenths; 		/* length of tape used per block written */

/*
 * Concurrent dump mods (Caltech) - disk block reading and tape writing
 * are exported to several slave processes.  While one slave writes the
 * tape, the others read disk blocks; they pass control of the tape in
 * a ring via flock().	The parent process traverses the filesystem and
 * sends spclrec()'s and lists of daddr's to each slave via pipes.
 *
 * from "@(#)dumptape.c 2.1 (Berkeley+Caltech mods) 4/7/85";
 */
struct req {			/* instruction packets sent to slaves */
	daddr_t dblk;
	int count;
} *req;
int reqsiz;

#define SLAVES 3		/* 1 slave writing, 1 reading, 1 for slack */
int slavepid[SLAVES];
int slavefd[SLAVES];		/* Pipes from master to each slave */
int rotor;			/* Current slave number */
int master;			/* Pid of master, for sending error signals */
int trace = 0;			/* Protocol trace; easily patchable with adb */
#define  tmsg	if (trace) msg

/*
/* Allocate tape buffer contiguous with the array of instruction
 * packets, so flusht() can write them together with one write().
 * Align tape buffer on page boundary to speed up tape write().
 */
alloctape()
{

	int pgoff = getpagesize() - 1;
	writesize = ntrec * TP_BSIZE;
	/*
	 * 92185 NEEDS 0.4"; 92181 NEEDS 0.8" to start/stop (see TU80 manual)
	 */
	tenths = writesize/density + (cartridge ? 16 : density == 625 ? 4 : 8);

	reqsiz = ntrec * sizeof(struct req);
	req = (struct req *)malloc(reqsiz + writesize + pgoff);
	if (req == NULL)
		return(0);
	tblock = (char (*)[TP_BSIZE]) (((long)&req[ntrec] + pgoff) &~ pgoff);
	req = (struct req *)tblock;
	req = &req[-ntrec];	/* Cmd packets go in front of tape buffer */
	return(1);
}

/*
 * Make copy of spclrec, to later send to tape writer.
 */
taprec(dp)
	char *dp;
{

	tmsg("taprec %d\n", trecno);
	req[trecno].dblk = (daddr_t)0;
	req[trecno].count = 1;
	*(union u_spcl *)(*tblock++) = *(union u_spcl *)dp;	/* movc3 */
	trecno++;
	spcl.c_tapea++;
	if(trecno >= ntrec)
		flusht();
}

dmpblk(blkno, size)
	daddr_t blkno;
	int size;
{
	int tpblks, dblkno;
	register int avail;

	dblkno = fsbtodb(sblock, blkno);
	tpblks = size / TP_BSIZE;
	while ((avail = MIN(tpblks, ntrec - trecno)) > 0) {
		tmsg("dmpblk %d\n", avail);
		req[trecno].dblk = dblkno;
		req[trecno].count = avail;
		spcl.c_tapea += avail;
		if ((trecno += avail) >= ntrec)
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
	killall();
	nogripe = 1;
	close_rewind();
	Exit(X_REWRITE);
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
	sig = sigblock(1 << SIGINT-1);		/* Don't abort pipe write */
	if (write(slavefd[rotor], req, siz) != siz) {
		perror("  DUMP: pipe error in command to slave");
		dumpabort();
	}
	sigsetmask(sig);
	if (++rotor >= SLAVES) rotor = 0;
	tblock = (char (*)[TP_BSIZE]) &req[ntrec];
	trecno = 0;
	asize += tenths;
	blockswritten += ntrec;
	if (!pipeout && asize > tsize) {
		close_rewind();
		otape();
	}
	timeest();
}

rewind()
{
	int f;

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
#endif RDUMP
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
	int	(*interrupt)();

	parentpid = getpid();

    restore_check_point:
	interrupt = signal(SIGINT, SIG_IGN);
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
		signal(SIGINT, interrupt);
#ifdef RDUMP
		while ((to = rmtopen(tape, 2)) < 0)
#else
		while ((to = pipeout ? 1 : creat(tape, 0666)) < 0)
#endif RDUMP
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
		kill(master, SIGPIPE);
	else {
		killall();
		msg("The ENTIRE dump is aborted.\n");
	}
	Exit(X_ABORT);
}

Exit(status)
{
#ifdef TDEBUG
	msg("pid = %d exits with status %d\n", getpid(), status);
#endif TDEBUG
	exit(status);
}

/*
 * prefer pipe(), but flock() barfs on them
 */
lockfile(fd)
	int fd[2];
{
	char tmpname[20];

	strcpy(tmpname, "/tmp/dumplockXXXXXX");
	mktemp(tmpname);
	if ((fd[1] = creat(tmpname, 0400)) < 0)
		return(fd[1]);
	fd[0] = open(tmpname, 0);
	unlink(tmpname);
	return (fd[0] < 0 ? fd[0] : 0);
}

enslave()
{
	int first[2], prev[2], next[2], cmd[2];     /* file descriptors */
	register int i, j;

	master = getpid();
	signal(SIGPIPE, dumpabort);  /* Slave quit/died/killed -> abort */
	signal(SIGIOT, tperror);     /* SIGIOT -> restart from checkpoint */
	lockfile(first);
	for (i = 0; i < SLAVES; i++) {
		if (i == 0) {
			prev[0] = first[1];
			prev[1] = first[0];
		} else {
			prev[0] = next[0];
			prev[1] = next[1];
			flock(prev[1], LOCK_EX);
		}
		next[0] = first[0];
		next[1] = first[1];	    /* Last slave loops back */
		if ((i < SLAVES-1 && lockfile(next) < 0) || pipe(cmd) < 0
				|| (slavepid[i] = fork()) < 0) {
			perror("  DUMP: too many slaves (recompile smaller)");
			dumpabort();
		}
		slavefd[i] = cmd[1];
		if (slavepid[i] == 0) {		    /* Slave starts up here */
			for (j = 0; j <= i; j++)
				close(slavefd[j]);
			signal(SIGINT, SIG_IGN);     /* Master handles these */
			signal(SIGTERM, SIG_IGN);
			doslave(i, cmd[0], prev, next);
			Exit(X_FINOK);
		}
		close(cmd[0]);
		if (i > 0) {
			close(prev[0]);
			close(prev[1]);
		}
	}
	close(first[0]);
	close(first[1]);
	master = 0; rotor = 0;
}

killall()
{
	register int i;

	for (i = 0; i < SLAVES; i++)
		if (slavepid[i] > 0)
			kill(slavepid[i], SIGKILL);
}

/*
 * Synchronization - each process has a lockfile, and shares file
 * descriptors to the following process's lockfile.  When our write
 * completes, we release our lock on the following process's lock-
 * file, allowing the following process to lock it and proceed. We
 * get the lock back for the next cycle by swapping descriptors.
 */
doslave(mynum,cmd,prev,next)
	int mynum, cmd, prev[2], next[2];
{
	register int toggle = 0, firstdone = mynum;

	tmsg("slave %d\n", mynum);
	close(fi);
	if ((fi = open(disk, 0)) < 0) {		/* Need our own seek pointer */
		perror("  DUMP: slave couldn't reopen disk");
		kill(master, SIGPIPE);		/* dumpabort */
		Exit(X_ABORT);
	}
	/*
	 * Get list of blocks to dump
	 */
	while (readpipe(cmd, req, reqsiz) > 0) {
		register struct req *p = req;
		for (trecno = 0; trecno < ntrec; trecno += p->count, p += p->count) {
			if (p->dblk) {
				tmsg("%d READS %d\n", mynum, p->count);
				bread(p->dblk, tblock[trecno],
				    p->count * TP_BSIZE);
			} else {
				tmsg("%d PIPEIN %d\n", mynum, p->count);
				if (p->count != 1 ||
				    readpipe(cmd, tblock[trecno], TP_BSIZE) <= 0) {
					msg("Master/slave protocol botched");
					dumpabort();
				}
			}
		}
		flock(prev[toggle], LOCK_EX);	/* Wait our turn */
		tmsg("%d WRITE\n", mynum);
#ifdef RDUMP
#ifndef sun	/* Defer checking first write until next one is started */
		rmtwrite0(writesize);
		rmtwrite1(tblock[0],writesize);
		if (firstdone == 0) firstdone = -1;
		else if (rmtwrite2() != writesize) {
			rmtwrite2();		/* Don't care if another err */
#else
		/* Asynchronous writes can hang Suns; do it synchronously */
		if (rmtwrite(tblock[0],writesize) != writesize) {
#endif sun
#else		/* Local tape drive */
		if (write(to,tblock[0],writesize) != writesize) {
			perror(tape);
#endif RDUMP
			kill(master, SIGIOT);	/* restart from checkpoint */
			for (;;) sigpause(0);
		}
		toggle ^= 1;
		flock(next[toggle], LOCK_UN);	/* Next slave's turn */
	}					/* Also jolts him awake */
#ifdef RDUMP			/* One more time around, to check last write */
#ifndef sun
	flock(prev[toggle], LOCK_EX);
	tmsg("%d LAST\n", mynum);
	if (firstdone < 0 && rmtwrite2() != writesize) {
		kill(master, SIGIOT);
		for (;;)
			sigpause(0);
	}
	toggle ^= 1;
	flock(next[toggle], LOCK_UN);
#endif sun
#endif RDUMP
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
		if (got <= 0) {
			if (rd == cnt && got == 0)
				return (0);		/* Normal EOF */
			msg("short pipe read");
			dumpabort();
		}
		buf += got;
	}
	return (cnt);
}
