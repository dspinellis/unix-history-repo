/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)dumptape.c	5.1 (Berkeley) %G%";
#endif not lint

#include "dump.h"

char	tblock[NTREC][BSIZE];
daddr_t	tdaddr[NTREC];
int	trecno;

taprec(dp)
char *dp;
{
	register i;

	for(i=0; i<BSIZE; i++)
		tblock[trecno][i] = *dp++;
	tdaddr[trecno] = 0;
	trecno++;
	spcl.c_tapea++;
	if(trecno >= NTREC)
		flusht();
}

tapsrec(d)
daddr_t d;
{

	if(d == 0)
		return;
	tdaddr[trecno] = d;
	trecno++;
	spcl.c_tapea++;
	if(trecno >= NTREC)
		flusht();
}

int	nogripe = 0;

flusht()
{
	register i, si;
	daddr_t d;

	while(trecno < NTREC)
		tdaddr[trecno++] = 1;

loop:
	d = 0;
	for(i=0; i<NTREC; i++)
		if(tdaddr[i] != 0)
		if(d == 0 || tdaddr[i] < d) {
			si = i;
			d = tdaddr[i];
		}
	if(d != 0) {
		bread(d, tblock[si], BSIZE);
		tdaddr[si] = 0;
		goto loop;
	}
	trecno = 0;
	if (write(to, tblock[0], sizeof(tblock)) != sizeof(tblock) ){
		if (pipeout) {
			msg("Tape write error on %s\n", tape);
			msg("Cannot recover\n");
			dumpabort();
			/*NOTREACHED*/
		}
		msg("Tape write error on tape %d\n", tapeno);
		broadcast("TAPE ERROR!\n");
		if (query("Do you want to restart?")){
			msg("This tape will rewind.  After it is rewound,\n");
			msg("replace the faulty tape with a new one;\n");
			msg("this dump volume will be rewritten.\n");
			/*
			 *	Temporarily change the tapeno identification
			 */
			tapeno--;
			nogripe = 1;
			close_rewind();
			nogripe = 0;
			tapeno++;
			Exit(X_REWRITE);
		} else {
			dumpabort();
			/*NOTREACHED*/
		}
	}

	asize += sizeof(tblock)/density;
	asize += 7;
	blockswritten += NTREC;
	if (!pipeout && asize > tsize) {
		close_rewind();
		otape();
	}
	timeest();
}

rewind()
{
	int	secs;
#ifdef DEBUG
	msg("Waiting 10 seconds to rewind.\n");
	sleep(10);
#else
	/*
	 *	It takes about 3 minutes, 25secs to rewind 2300' of tape
	 */
	secs = (( (60*3) + 25)*asize)/(2300L*12L*10L);
	msg("Waiting %d seconds to rewind.\n", secs);
	sleep(secs);
#endif
}

close_rewind()
{
	if (pipeout)
		return;
	close(to);
	if (!nogripe){
		rewind();
		msg("Change Tapes: Mount tape #%d\n", tapeno+1);
		broadcast("CHANGE TAPES!\7\7\n");
	}
	do{
		if (query ("Is the new tape mounted and ready to go?"))
			break;
		if (query ("Do you want to abort?")){
			dumpabort();
			/*NOTREACHED*/
		}
	} while (1);
}

/*
 *	We implement taking and restoring checkpoints on
 *	the tape level.
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
	int	sig_ign_parent();
	int	interrupt();

	/*
	 *	Force the tape to be closed
	 */
	if (!pipeout)
		close(to);
	parentpid = getpid();

    restore_check_point:
	signal(SIGINT, interrupt);
	/*
	 *	All signals are inherited...
	 */
	childpid = fork();
	if (childpid < 0){
		msg("Context save fork fails in parent %d\n", parentpid);
		Exit(X_ABORT);
	}
	if (childpid != 0){
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
		for (;;){
			waitpid = wait(&status);
			if (waitpid != childpid){
				msg("Parent %d waiting for child %d has another child %d return\n",
					parentpid, childpid, waitpid);
			} else
				break;
		}
		if (status & 0xFF){
			msg("Child %d returns LOB status %o\n",
				childpid, status&0xFF);
		}
		status = (status >> 8) & 0xFF;
#ifdef TDEBUG
		switch(status){
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
				msg("Child %d finishes unknown %d\n", childpid,status);
				break;
		}
#endif TDEBUG
		switch(status){
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
		do{
			if (pipeout)
				to = 1;
			else
				to = creat(tape, 0666);
			if (to < 0) {
				if (!query("Cannot open tape. Do you want to retry the open?"))
					dumpabort();
			} else break;
		} while (1);

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

/*
 *	The parent still catches interrupts, but does nothing with them
 */
sig_ign_parent()
{
	msg("Waiting parent receives interrupt\n");
	signal(SIGINT, sig_ign_parent);
}

dumpabort()
{
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
