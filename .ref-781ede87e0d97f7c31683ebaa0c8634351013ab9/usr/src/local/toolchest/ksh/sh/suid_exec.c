/*
 * This is a program to execute 'execute only' and suid/sgid shell scripts.
 * This program must be owned by root and  must have the suid bit set.
 * This program must be installed as define parameter THISPROG for this to work
 *  on system V machines
 *
 *  Written by David Korn
 *  AT&T Bell Laboratories
 *  ulysses!dgk
 */

/* The file name of the script to execute is argv[0]
 * Argv[1] is the  program name
 * The basic idea is to open the script as standard input, set the effective
 *   user and group id correctly, and then exec the shell.
 * The complicated part is getting the effective uid of the caller and 
 *   setting the effective uid/gid.  The program which execs this program
 *   must pass file descriptor FDIN as an open file with mode SPECIAL if
 *   the effective user id is not the real user id.  The effective
 *   user id for authentication purposes will be the owner of this
 *   open file.  On systems without the setreuid() call, e[ug]id is set
 *   by copying this program to a /tmp/file, making it a suid and/or sgid
 *   program, and then execing this program.
 * A forked version of this program waits until it can unlink the /tmp
 *   file and then exits.  Actually, we fork() twice so the parent can
 *   wait for the child to complete.  A pipe is used to guarantee that we
 *   do not remove the /tmp file too soon.
 */

#ifdef BSD
# ifdef BSD_4_2
#  include	<fcntl.h>
#  include	<sys/param.h>
# else
#  define fcntl(a,b,c)	dup2(a,c)
#  include	<sys/types.h>
# endif /* BSD_4_2 */
#else
#  include	<fcntl.h>
# include	<sys/types.h>
#endif /* BSD */
#include	<sys/stat.h>
#include	<errno.h>

#define SPECIAL		04100	/* setuid execute only by owner */
#define FDIN		10	/* must be same as /dev/fd below */
#define FDSYNC		11	/* used on sys5 to syncronize cleanup */
#define BLKSIZE		sizeof(char*)*1024
#define THISPROG	"/etc/suid_exec"
#define DEFSHELL	"/bin/sh"

extern char *getenv();
extern int errno;

static int error();
static int in_dir();
static int endsh();
#ifndef BSD_4_2
static int copy();
static void mktemp();
#endif /* BSD_4_2 */

static char version[] 	= "@(#)suid_exec 06/03/86a";
static char badopen[] = "cannot open";
static char badexec[] = "cannot exec";
static char tmpname[] = "/tmp/SUIDXXXXXX";
static char devfd[]	= "/dev/fd/10";
static char **arglist;

static char *shell;
static char *command;
static int created;
static int ruserid;
static int euserid;
static int rgroupid;
static int egroupid;
static struct stat statb;

main(argc,argv)
char *argv[];
{
	register int n;
	register char *p;
	int mode;
	int effuid;
	int effgid;
	int priv = 0;
	arglist = argv;
	command = argv[1];
	ruserid = getuid();
	euserid = geteuid();
	rgroupid = getgid();
	egroupid = getegid();
	p = argv[0];
#ifndef BSD_4_2
	mktemp(tmpname);
	if(strcmp(p,tmpname)==0)
	{
		/* This enables the grandchild to clean up /tmp file */
		close(FDSYNC);
		/* make sure that this is a valid invocation of /tmp prog */
		/* the /tmp file must be owned by root, setuid, and not a link */
		if(euserid==0 && ruserid!=0)
		{
			/* keep out forgers */
			if(stat(tmpname,&statb) < 0)
				error(badexec);
			/* don't trust /tmp file unless suid root */
			if((statb.st_mode&S_ISUID)==0 || statb.st_uid
				|| statb.st_nlink!=1)
					error(badexec);
		}
		goto exec;
	}
	/* make sure that this is the real setuid program, not the clone */
	if(euserid || command==(char*)0)
		error(badexec);
#endif /* BSD_4_2 */
	/* validate execution rights to this script */
	if(fstat(FDIN,&statb)<0)
	{
		effuid++;
		euserid = ruserid;
	}
	else
	{
		priv++;
		if((statb.st_mode&~S_IFMT) != SPECIAL)
			error(badexec);
		euserid = statb.st_uid;
	}
	/* do it the easy way if you can */
	if(euserid == ruserid && egroupid==rgroupid)
	{
		if(access(p,1) < 0)
			error(badexec);
	}
	else
	{
		/* have to check access on each component */
		while(*p++)
		{
			if(*p == '/' || *p==0)
			{
				n = *p;
				*p = 0;
				if(eaccess(argv[0],1) < 0)
					error(badexec);
				*p = n;
			}
		}
		p = argv[0];
	}
	/* open the script for reading and make it be FDIN */
	n = open(p,0);
	if(n < 0)
		error(badopen);
	if(fstat(n,&statb)<0)
		error(badopen);
	close(FDIN);
	if(fcntl(n,F_DUPFD,FDIN)!=FDIN)
		error(badexec);
	close(n);
	/* compute the desired new effective user and group id */
	effuid = euserid;
	effgid = egroupid;
	mode = 0;
	if(priv && statb.st_mode & S_ISUID)
		effuid = statb.st_uid;
	if(priv && statb.st_mode & S_ISGID)
		effgid = statb.st_gid;
	/* see if group needs setting */
	if(effgid  != egroupid)
		if(effgid != rgroupid || setgid(rgroupid)<0)
			mode = S_ISGID;
		
	/* now see if the uid needs setting */
	if(effuid)
		if(mode || effuid != ruserid || setuid(ruserid)<0)
			mode |= S_ISUID;
	if(mode)
		setids(mode,effuid,effgid);
exec:
	/* only use SHELL if file is in trusted directory and ends in sh */
	shell = getenv("SHELL");
	if(shell==0 || !endsh(shell) || (
		!in_dir("/bin",shell) &&
		!in_dir("/usr/bin",shell) &&
		!in_dir("/usr/lbin",shell)))
			shell = DEFSHELL;
	argv[0] = command;
	argv[1] = devfd;
	execv(shell,argv);
	error(badexec);
}

/*
 * return true of shell ends in sh
 */

static int endsh(shell)
register char *shell;
{
	while(*shell)
		shell++;
	if(*--shell != 'h' || *--shell != 's')
		return(0);
	return(1);
}


/*
 * return true of shell is in <dir> directory
 */

static int in_dir(dir,shell)
register char *dir;
register char *shell;
{
	while(*dir)
	{
		if(*dir++ != *shell++)
			return(0);
	}
	/* return true if next character is a '/' */
	return(*shell=='/');
}

static int error(message)
{
	printf("%s: %s\n",command,message);
	if(created)
		unlink(tmpname);
	exit(1);
}


/*
 * This version of access checks against effective uid and effective gid
 */

eaccess(name, mode)
register char	*name;
register int mode;
{	
	struct stat statb;
	if (stat(name, &statb) == 0)
	{
		if(euserid == 0)
		{
			if((statb.st_mode&S_IFMT) != S_IFREG || mode != 1)
				return(0);
		    	/* root needs execute permission for someone */
			mode = (S_IEXEC|(S_IEXEC>>3)|(S_IEXEC>>6));
		}
		else if(euserid == statb.st_uid)
			mode <<= 6;
		else if(egroupid == statb.st_gid)
			mode <<= 3;
#ifdef BSD
# ifdef BSD_4_2
		/* in BSD_4_2 you can be in several groups */
		else
		{
			int groups[NGROUPS];
			register int n;
			n = getgroups(NGROUPS,groups);
			while(--n >= 0)
			{
				if(groups[n] == statb.st_gid)
				{
					mode <<= 3;
					break;
				}
			}
		}
# endif /* BSD_4_2 */
#endif /* BSD */
		if(statb.st_mode & mode)
			return(0);
	}
	return(-1);
}

#ifdef BSD_4_2
setids(mode,owner,group)
{
	if(mode & S_ISGID)
		setregid(rgroupid,group);
	if(mode & S_ISUID)
		setreuid(ruserid,owner);
}

#else
/*
 * This version of setids creats a /tmp file and copies this program into
 * it.  The /tmp file is made executable with appropriate suid/sgid bits.
 *  Finally, the /tmp file is exec'ed.  The /tmp file is unlinked by a
 * grandchild of this program, who waits until the text is free
 */

setids(mode,owner,group)
{
	register int n;
	int pv[2];
	/* create a setuid program with a copy of this program without RW */
	if((n=creat(tmpname,mode|SPECIAL)) < 0)
		error(badexec);
	created++;
	if(chown(tmpname,owner,group) < 0)
		error(badexec);
	copy(n);
	/* create a pipe for syncronization */
	pv[0] = pv[1] = -1;
	pipe(pv);
	/* pipe failure could cause the /tmp file to be removed prematurely */
	/*  This is not worth waiting for */
	if((n=fork())==0)
	{
		char buf[2];
		if(fork())
			exit(0);
		/* the grandchild has to clean up the text file */
		close(pv[1]);
		/* wait until the parent closes the pipe */
		read(pv[0],buf,1);
		while(1)
		{
			/* sleep granularity too crude to trust sleep(1) */
			sleep(1);
			if(unlink(tmpname) >= 0)
				exit(0);
			else if(errno != ETXTBSY)
				exit(1);
		}
	}
	else if(n == -1)
		error(badexec);
	else
	{
		arglist[0] = tmpname;
		close(pv[0]);
		while(wait(0)!= -1);
		/* put write end of pipe into FDSYNC */
		close(FDSYNC);
		if(pv[1] != FDSYNC)
			n = fcntl(pv[1],F_DUPFD,FDSYNC);
		if(n != FDSYNC)
			close(n);
		close(pv[1]);
		execv(tmpname,arglist);
		error(badexec);
	}
}

/*
 * create a unique name into the <template>
 */

static void mktemp(template)
char *template;
{
	register char *cp = template;
	register int n = getpid();
	/* skip to end of string */
	while(*++cp);
	/* convert process id to string */
	while(n > 0)
	{
		*--cp = (n%10) + '0';
		n /= 10;
	}
	
}

/*
 *  copy THISPROG into the open file number <fdo> and close <fdo>
 */

static int copy(fdo)
int fdo;
{
	char buffer[BLKSIZE];
	register int n;
	int fdi;
	if((fdi = open(THISPROG,0)) < 0)
		error(badexec);
	while((n = read(fdi,buffer,BLKSIZE))!=0)
	{
		if(n < 0)
			error(badexec);
		write(fdo,buffer,n);
	}
	close(fdi);
	return(close(fdo));
}

#endif /* BSD_4_2 */
