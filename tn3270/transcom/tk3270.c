/******************************************************************/
/*								  */
/* Transparent mode command for Suntools tektronix support for    */
/* tn3270. Not to be directly called from shell. Specify	  */
/* 'tk3270' as tn3270 '-t' option or using 'transcom' command 	  */
/*  stdin and stdout are ends of pipes from tn3270		  */
/*								  */
/* Steve Jacobson   U.C. Berkeley  6/86				  */
/******************************************************************/

#ifndef lint  
static char sccsid[] = "@(#)tk3270.c	1.1 (Berkeley) 6/30/86";  
#endif not lint


#include <stdio.h>
#include <signal.h>
#include <strings.h>
#include <sys/time.h>
#include <setjmp.h>

char fnamei[] = "/tmp/tkiXXXXXX";
char fnameo[] = "/tmp/tkoXXXXXX";
int ioflag = 0;
jmp_buf env;

main(argc, argv)
	int argc;
	char *argv[];
{
	char command[300], inbuf[BUFSIZ];
	int chldpid = -1, c, abortit(), sigin();
	FILE *fopen(), *fpi, *fpo, *tout;

/* tk3270 calls itself in a child process; tektool makes this necessary */
	if (argc == 3 && *argv[1] == '/' && *argv[2] == '/') {
	   exit(secondpass(argc, argv));
	}
	(void) mktemp(fnamei);
	(void) mktemp(fnameo);
/* communicating to program running in tektool via files is necessary */
/* because tektool won't leave fds alone */
	if ((fpo = fopen(fnameo, "w")) == NULL) {
	   exit(1);
	}
	if ((tout = fopen("/dev/tty", "w")) == NULL) {
	   exit(1);
	}
/* second pass needs to know this process id */
	fprintf(fpo, "%d\n", getpid());
	(void) fflush(fpo);
	(void) signal(SIGCHLD, abortit);
	(void) signal(SIGIO, sigin);
	(void) strcpy(command, "/usr/bin/tektool ");
	for (c = 1; c < argc; ++c) { /* copy suntool options to command line */
	    (void) strcat(command, argv[c]);
	    (void) strcat(command, " ");
	}
	(void) strcat(command, "-r ");
	(void) strcat(command, argv[0]);
	(void) strcat(command, " ");
	(void) strcat(command, fnameo);
	(void) strcat(command, " ");
	(void) strcat(command, fnamei);
/* invoke tektool and call tk3270 again */
	if ((c = fork()) == 0) {
	   if (execl("/bin/csh", "csh", "-c", command, (char *) 0) < 0) {
	      (void) fflush(fpo);
	      (void) fclose(fpo);
	      (void) unlink(fnameo);
	      exit(1);
	   }
	}
	if (c < 0) {
	   (void) fflush(fpo);
	   (void) fclose(fpo);
	   (void) unlink(fnameo);
	   exit(1);
	}
	fprintf(tout,"%c[10;10Hstarting tektool...please wait", '\033');
	(void) fflush(tout);
/* remember that sun longjmp always returns 1 */
/* will longjmp if child dies or second pass (child) sends SIGIO */
	if (setjmp(env) && ioflag == 0) { /* child died */
	   (void) fclose(fpo);
	   (void) unlink(fnameo);
	   (void) unlink(fnamei);
	   if (chldpid > 0) {
	      kill(chldpid, SIGKILL);
	   }
	   exit(0);
	}
	if (ioflag) { /* there is io waiting from child tek screen */
	   ioflag = 0;
	   if (chldpid < 0) {
/* read child pid so can kill when stdio closes */
	      if ((fpi = fopen(fnamei, "r")) == NULL) { 
	         (void) fflush(fpo);
	         (void) fclose(fpo);
	         (void) unlink(fnameo);
	         exit(1);
	      }
	      fscanf(fpi,"%d\n", &chldpid);
	    }
	    while ((c = getc(fpi)) != EOF) {
/* remember that tn3270 is in cbreak mode */
		  if (c == '\n') {
	    	     c = '\r';
		  }
		  putc(c, stdout);
  	    }
	    (void) fflush(stdout);
	    (void) clearerr(fpi);
        }
/* endless read loop until tn3270 closes pipe, child unexpectly dies, or */
/* child signals that io from tek screen is waiting to go to tn3270 */
	while ((c = read(fileno(stdin), inbuf, sizeof(inbuf))) != 0) {
	      if (c > 0) {
		 write(fileno(fpo), inbuf, c);
	      }
	}
	(void) fclose(fpo);
	(void) unlink(fnameo);
	(void) unlink(fnamei);
	if (chldpid > 0) {
	   kill(chldpid, SIGKILL);
	}
	exit(0);
}

abortit()
{
	longjmp(env, 1);
}

sigin()
{
	ioflag++;
	longjmp(env, 1);
}

/* secondpass is run by tektool, so output graphics commands result */
/* in moves, draws, etc. */
secondpass(argc, argv)
	int argc;
	char *argv[];
{
	FILE *fopen(), *fpi, *fpo, *tin, *tout;
	int c, parpid, readfds;
	struct timeval time;

	time.tv_sec = 0;
	time.tv_usec = 500;
	if ((fpi = fopen(argv[1], "r")) == NULL) {
	   return(1);
	}
/* read first pass process id from file for sigio */
	fscanf(fpi, "%d\n", &parpid);
	if ((fpo = fopen(argv[2], "w")) == NULL) {
	   return(1);
	}
/* tell first pass this process id for kill */
	fprintf(fpo, "%d\n", getpid());
	if ((tin = fopen("/dev/tty", "r")) == NULL) {
	   return(1);
	}
	if ((tout = fopen("/dev/tty", "w")) == NULL) {
	   return(1);
	}
	while (1) {	 /* loop forever (1st pass will kill) */
	      while ((c = getc(fpi)) != EOF) {
		    putc(c, tout);
	      }
	      (void) fflush(tout);
	      (void) clearerr(fpi);
	      readfds = 1 << fileno(tin);
	      if (select(32, &readfds, 0, 0, &time) == 1) {
		 while ((c = getc(tin)) != '\n') {
		       putc(c, fpo);
		 }
		 putc('\r', fpo);
		 (void) fflush(fpo);
		 (void) kill(parpid, SIGIO);
	      }
	}
}
