/*
 * Copyright (c) 1992 William F. Jolitz, TeleMuse
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This software is a component of "386BSD" developed by 
	William F. Jolitz, TeleMuse.
 * 4. Neither the name of the developer nor the name "386BSD"
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS A COMPONENT OF 386BSD DEVELOPED BY WILLIAM F. JOLITZ 
 * AND IS INTENDED FOR RESEARCH AND EDUCATIONAL PURPOSES ONLY. THIS 
 * SOFTWARE SHOULD NOT BE CONSIDERED TO BE A COMMERCIAL PRODUCT. 
 * THE DEVELOPER URGES THAT USERS WHO REQUIRE A COMMERCIAL PRODUCT 
 * NOT MAKE USE THIS WORK.
 *
 * FOR USERS WHO WISH TO UNDERSTAND THE 386BSD SYSTEM DEVELOPED
 * BY WILLIAM F. JOLITZ, WE RECOMMEND THE USER STUDY WRITTEN 
 * REFERENCES SUCH AS THE  "PORTING UNIX TO THE 386" SERIES 
 * (BEGINNING JANUARY 1991 "DR. DOBBS JOURNAL", USA AND BEGINNING 
 * JUNE 1991 "UNIX MAGAZIN", GERMANY) BY WILLIAM F. JOLITZ AND 
 * LYNNE GREER JOLITZ, AS WELL AS OTHER BOOKS ON UNIX AND THE 
 * ON-LINE 386BSD USER MANUAL BEFORE USE. A BOOK DISCUSSING THE INTERNALS 
 * OF 386BSD ENTITLED "386BSD FROM THE INSIDE OUT" WILL BE AVAILABLE LATE 1992.
 *
 * THIS SOFTWARE IS PROVIDED BY THE DEVELOPER ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE DEVELOPER BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 */
#include <signal.h>
#include <setjmp.h>

char *shargv[] = { /*"sh"*/ "-", 0 } ;
char *shenv[] = { "HOME=/", "TERM=pc3", "USER=root", 0, 0 } ;
char initarg[30];
jmp_buf buf;
int cpid;

main(argc, argv) char *argv[]; {
	int n;
	int off = 0;
	extern child();

	/* did some idiot try to run us? */
	if(getpid() != 1) {
		write(2,"System daemon, runnable only by system\n",  39);
		exit(0xff);
	}

	/* look for system's argument */
	if(argc > 1 && argv[1]) {
		strcpy(initarg, "INITARG=");
		strcat(initarg, argv[1]);
		shenv[3] = initarg;
	}

	/* restart here if shell dies */
	setjmp(buf);
	
	/* allocate a session for init */
	(void) setsid();

	/* protect against signals, listen for children */
	signal (SIGTERM, SIG_IGN);
	signal (SIGTSTP, SIG_IGN);
	signal (SIGTTIN, SIG_IGN);
	signal (SIGTTOU, SIG_IGN);
	signal (SIGCHLD, child);

	/* close any open files */
	for(n=0; n < 20; n++) close(n);

	/* make a process */
	if ((cpid = fork()) == 0) {

		/* signals, to default state */
		signal (SIGTERM, SIG_DFL);
		signal (SIGHUP, SIG_DFL);
		signal (SIGALRM, SIG_DFL);
		signal (SIGTSTP, SIG_DFL);
		signal (SIGCHLD, SIG_DFL);

		/* clean off console */
		revoke("/dev/console");

		/* do open and configuration of console */
		login_tty(open("/dev/console", 2));
	
		/* all ready to execute the shell */
		execve("/bin/sh", shargv, shenv);

		/* oops */
		write("execve failed: /bin/sh\n");
		exit(0xff);
	}
	for(;;)pause();
}

#include <sys/types.h>
#include <sys/wait.h>

child() {
	int status;

	if (cpid == waitpid(-1, &status, WNOHANG))
		longjmp(buf, 0);
	signal(SIGCHLD, child);
}
