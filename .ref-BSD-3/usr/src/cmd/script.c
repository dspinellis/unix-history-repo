 /*
  *
  * script - makes copy of terminal conversation. usage:
  * script [ -n ] [ -s ] [ -q ] [ -a ] [ -S shell ] [ file ]
  * conversation saved in file. default is DFNAME
  *
  */

#define DFNAME "typescript"

#ifdef HOUXP
#define STDSHELL "/bin/sh"
#define NEWSHELL "/p4/3723mrh/bin/csh"
char *shell = NEWSHELL;
#endif

#ifdef HOUXT
#define STDSHELL "/bin/sh"
#define NEWSHELL "/t1/bruce/ucb/bin/csh"
char *shell = NEWSHELL;
#endif

#ifdef CORY
#define STDSHELL "/bin/sh"
#define NEWSHELL "/bin/csh"
char *shell = NEWSHELL;
#endif

#ifdef CC
#define STDSHELL "/bin/sh"
#define NEWSHELL "/bin/csh"
char *shell = NEWSHELL;
#endif

#ifndef STDSHELL
# define V7ENV
#endif

#ifdef V7ENV
/* used for version 7 with environments - gets your environment shell */
#define STDSHELL "/bin/sh"
#define NEWSHELL "/bin/csh"
char *shell;	/* initialized in the code */
# include <sys/types.h>
# include <sys/stat.h>
# define MODE st_mode
# define STAT stat
char *getenv();

#else

/*
 * The following is the structure of the block returned by
 * the stat and fstat system calls.
 */

struct inode {
	char	i_minor;	/* +0: minor device of i-node */
	char	i_major;	/* +1: major device */
	int	i_number;	/* +2 */
	int	i_flags;	/* +4: see below */
	char	i_nlinks;	/* +6: number of links to file */
	char	i_uid;		/* +7: user ID of owner */
	char	i_gid;		/* +8: group ID of owner */
	char	i_size0;	/* +9: high byte of 24-bit size */
	int	i_size1;	/* +10: low word of 24-bit size */
	int	i_addr[8];	/* +12: block numbers or device number */
	int	i_actime[2];	/* +28: time of last access */
	int	i_modtime[2];	/* +32: time of last modification */
};

#define	IALLOC	0100000
#define	IFMT	060000
#define		IFDIR	040000
#define		IFCHR	020000
#define		IFBLK	060000
#define MODE i_flags
#define STAT inode
#endif

char	*tty;		/* name of users tty so can turn off writes */
char	*ttyname();	/* std subroutine */
int	mode = 0622;	/* old permission bits for users tty */
int	outpipe[2];	/* pipe from shell to output */
int	fd;		/* file descriptor of typescript file */
int	inpipe[2];	/* pipe from input to shell */
long	tvec;		/* current time */
char	buffer[256];	/* for block I/O's */
int	n;		/* number of chars read */
int	status;		/* dummy for wait sys call */
char	*fname;		/* name of typescript file */
int	forkval, ttn;	/* temps for error checking */
int	qflg;		/* true if -q (quiet) flag */
int	aflg;		/* true if -q (append) flag */
struct STAT sbuf;
int	flsh();

main(argc,argv) int argc; char **argv; {

	if ((tty = ttyname(2)) < 0) {
		printf("Nested script not allowed.\n");
		fail();
	}

#ifdef V7ENV
	shell = getenv("SHELL");
#endif

	while ( argc > 1 && argv[1][0] == '-') {
		switch(argv[1][1]) {
			case 'n':
				shell = NEWSHELL;
				break;
			case 's':
				shell = STDSHELL;
				break;
			case 'S':
				shell = argv[2];
				argc--; argv++;
				break;
			case 'q':
				qflg++;
				break;
			case 'a':
				aflg++;
				break;
			default:
				printf("Bad flag %s - ignored\n",argv[1]);
		}
		argc--; argv++;
	}

	if (argc > 1) {
		fname = argv[1];
		if (!aflg && stat(fname,&sbuf) >= 0) {
			printf("File %s already exists.\n",fname);
			done();
		}
	} else	fname = DFNAME;
	if (!aflg) {
		fd = creat(fname,0);	/* so can't cat/lpr typescript from inside */
	} else {
		/* try to append to existing file first */
		fd = open(fname,1);
		if (fd >= 0) lseek(fd,0l,2);
		    else     fd = creat(fname,0);
	}
	if (fd<0) {
		printf("Can't create %s\n",fname);
		if (unlink(fname)==0) {
			printf("because of previous typescript bomb - try again\n");
		}
		fail();
	}

	chmod(fname,0);	/* in case it already exists */
	fixtty();
	if (!qflg) {
		printf("Script started, file is %s\n",fname);
		check(write(fd,"Script started on ",18));
		time(&tvec);
		check(write(fd,ctime(&tvec),25));
	}
	pipe(inpipe);
	pipe(outpipe);

	forkval = fork();
	if (forkval < 0) {
		printf("Fork failed - try again.\n");
		fail();
	}
	if (forkval == 0) doshell();

	forkval = fork();
	if (forkval < 0) {
		printf("Fork failed. Try again.\n");
		fail();
	}
	if (forkval == 0) dooutput();
		else      doinput();
}

doinput() {
	int done();
	/* input process - copy tty to pipe and file */
	signal(2,1);	/* ignore interrupts from delete */
	signal(3,done);	/* fix files when users quits. */

	close(inpipe[0]);
	close(outpipe[0]);
	close(outpipe[1]);

	/* main input loop - copy until end of file (ctrl D) */
	while (n=read(0,buffer,256)) {
		check(write(fd,buffer,n));
		write(inpipe[1],buffer,n);
	}

	/* end of script - close files and exit */
	close(inpipe[1]);
	close(fd);
	wait(&status);	/* wait for shell to terminate */
	wait(&status);	/* wait for output to terminate */
	done();
}

dooutput() {
	/* do output process - copy to tty & file */
	signal(2,flsh);	/* trap to flsh on interrupts */
	signal(3,1);	/* ignore quits */

	close(0);
	close(inpipe[0]);
	close(inpipe[1]);
	close(outpipe[1]);

	/* main output proc loop */
	while (n=read(outpipe[0],buffer,256)) {
		if (n > 0) { /* -1 means trap to flsh just happened */
			write(1,buffer,n);
			check(write(fd,buffer,n));
		}
	}

	/* output sees eof - close files and exit */
	if (!qflg) {
		printf("Script done, file is %s\n",fname);
		check(write(fd,"\nscript done on ",16));
		time(&tvec);
		check(write(fd,ctime(&tvec),25));
	}
	close(fd);
	exit();
}

doshell() {
	/* exec shell, after divirting std input & output */
	close(0);
	dup(inpipe[0]);
	close(1);
	dup(outpipe[1]);
	close(2);
	dup(outpipe[1]);

	/* close useless files */
	close(inpipe[0]);
	close(inpipe[1]);
	close(outpipe[0]);
	close(outpipe[1]);
/*	signal(2,1);	/* shell should ignore interrupts */
	execl(shell,"sh","-i",0);
	execl(STDSHELL,"sh","-i",0);
	execl(NEWSHELL,"sh","-i",0);
	printf("Can't execute shell\n");
	fail();
}

fixtty()
{

	fstat(2, &sbuf);
	mode = sbuf.MODE&0777;
	chmod(tty, 0600);
}

flsh() {
	/* come here on rubout to flush output - this doesn't work */
	signal(2,flsh);
	/* lseek(outpipe[0],0l,2);	/* seeks on pipes don't work !"$"$!! */
}

fail() {
	unlink(fname);
	kill(0,15);	/* shut off other script processes */
	done();
}

done() {
	chmod(tty,mode);
	chmod(fname,0664);
	exit();
}

#ifndef V7ENV
#ifndef CC
char *ttyname(i) int i; {
	char *string;
	string = "/dev/ttyx";
	string[8] = ttyn(fd);
	if (string[8] == 'x') return((char *) (-1));
		else return(string);
}
#endif
#endif

check(n) int n; {
	/* checks the result of a write call, if neg
	   assume ran out of disk space & die */
	if (n < 0) {
		write(1,"Disk quota exceeded - script quits\n",35);
		kill(0,15);
		done();
	}
}
