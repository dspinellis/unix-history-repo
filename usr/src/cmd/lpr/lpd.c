static	char *sccsid = "@(#)lpd.c	4.5 (Berkeley) 7/2/81";
/*
 * Line-printer daemon
 */

#include <sys/types.h>
#include <stdio.h>
#include <dir.h>
#include <signal.h>
#include <stat.h>
#include <sgtty.h>

char	line[128];
char	banbuf[64];
char	middle[64];
int	linel;
FILE	*dfb;
char	dfname[50] = "/usr/spool/lpd/";
char	*SPOOL_DIR = "/usr/spool/lpd/";
char	*LOCK	   = "/usr/spool/lpd/lock";
int	waittm	= 60;
struct	dir dbuf;
int	onalrm();

main(argc, argv)
{
	register char *p1, *p2;
	register int df;
	register FILE *dp;
	struct stat stb;
	char	*file_name, *get_file();

	umask(022);
	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGTERM, SIG_IGN);
/*
 * Close all files, open root as 0, 1, 2
 * to assure standard environment
 */
	for (df=0; df<=15; df++)
		close(df);
	open("/", 0);
	dup(0);
	dup(0);
	if (stat(LOCK, &stb) >= 0)
		exit(0);
	if ((df=creat(LOCK, 0)) < 0)
		exit(0);
	close(df);
	if (fork())
		exit(0);
again:
	file_name = get_file();
	strcpy(dfname, SPOOL_DIR);
	strcatn(dfname, file_name, DIRSIZ);
	if (trysend(dfname) == 0)
		goto again;
	sleep(waittm);
	goto again;
}

struct files {
	char file_name[DIRSIZ];
	long	time;
	struct files *next;
} ;

char	*get_file()
{
	FILE	*fp;
	static	struct	files	*top = NULL;
	struct	files	*ptr;
	extern	char	*calloc();
	static	char	sp[DIRSIZ+1];
	struct	stat	Stat;

	if ( top == NULL )
	{
		top = (struct files *)calloc(1,sizeof (struct files));
		ptr = top;
		top->next = top;
		if ( (fp = fopen(SPOOL_DIR,"r")) == NULL )
			exit(1);
		while (fread(&dbuf, sizeof dbuf, 1, fp) == 1) 
		{
			if (dbuf.d_ino==0 || dbuf.d_name[0]!='d' || dbuf.d_name[1]!='f')
				continue;
			ptr = ptr->next;
			stat(dbuf.d_name,&Stat);
			ptr->time = Stat.st_ctime;
			strcatn(ptr->file_name, dbuf.d_name, DIRSIZ);
			if ( ptr != top )
				sort(top,ptr);
			ptr->next  = (struct files *)calloc(1,sizeof (struct files));
		}
		if ( ptr->next  == top )
		{
			feedpage();
			unlink(LOCK);
			exit(0);
		}
		cfree(ptr->next);
		ptr->next = NULL;
	}

	sp[0] = '\0';
	strcatn(sp, top->file_name, DIRSIZ);
	sp[DIRSIZ] = '\0';
	ptr = top;
	top = top->next;
	cfree(ptr);
	return(sp);
}

sort(top,add)
register struct	files *top, *add;
{
	register struct	files *ptr;

	for ( ptr = top ; ptr != NULL ; ptr = ptr->next)
	{
		if (ptr->next == NULL || (ptr->next)->time > add->time)
		{
			add->next = ptr->next;
			ptr->next = add;
			return;
		}
	}
}



trysend(file)
	char *file;
{
	register char *p1, *p2;
	register int i;
	extern int badexit();

	dfb = fopen(file, "r");
	if (dfb == NULL)
		return(0);
	banbuf[0] = 0;
	middle[0] = 0;
	while (getline()) switch (line[0]) {
	case 'L':
		p1 = line+1;
		p2 = banbuf;
		while (*p2++ = *p1++);
		continue;

	case 'B':
		p1 = line+1;
		p2 = middle;
		while ( *p2++ = *p1++)
			continue;
		continue;

	case 'F':
		if (send())
			return(1);
		continue;

	case 'U':
		continue;

	case 'M':
		continue;
	}
/*
 * Second pass.
 * Unlink files and send mail.
 */
	fseek(dfb, 0L, 0);
	while (getline()) switch (line[0]) {

	default:
		continue;

	case 'U':
		unlink(&line[1]);
		continue;

	case 'M':
		sendmail();
		continue;
	}
	fclose(dfb);
	unlink(file);
}

sendmail()
{
	static int p[2];
	register i;
	int stat;

	pipe(p);
	if (fork()==0) {
		alarm(0);
		if (p[0] != 0) {
			close(0);
			dup(p[0]);
			close(p[0]);
		}
		close(p[1]);
		for (i=3; i<=15; i++)
			close(i);
		execl("/bin/mail", "mail", &line[1], 0);
		exit(0);
	}
	write(p[1], "Your printer job is done\n", 25);
	close(p[0]);
	close(p[1]);
	wait(&stat);
}

getline()
{
	register char *lp;
	register int c;

	lp = line;
	linel = 0;
	while ((c = getc(dfb)) != '\n') {
		if (c<0)
			return(0);
		if (c=='\t') {
			do {
				*lp++ = ' ';
				linel++;
			} while ((linel & 07) != 0);
			continue;
		}
		*lp++ = c;
		linel++;
	}
	*lp++ = 0;
	return(1);
}

int	pid;

send()
{
	int p;

	if (pid = fork()) {
		if (pid == -1)
			return(1);
		setexit();
		signal(SIGALRM, onalrm);
		alarm(30);
		wait(&p);
		alarm(0);
		return(p);
	}
	if (banbuf[0]) {
		if (middle[0])
			execl("/usr/lib/lpf","lpf","-b",banbuf,
			      "-m",middle,line+1,0);
		else
			execl("/usr/lib/lpf", "lpf", "-b", banbuf, line+1, 0);
		return(1);
	}
	execl("/usr/lib/lpf", "lpf", line, 0);
	return(1);
}

onalrm()
{
	struct stat stb;

	signal(SIGALRM, onalrm);
	if (stat(dfname, &stb) < 0)
		kill(pid, SIGEMT);
	reset();
}

struct	sgttyb ttyb = {
	B1200, B1200,
	0, 0,
	CRMOD|XTABS|ANYP
};

FILE	*out;

feedpage()
{
	register int i = 66;
	FILE *lp;
	int retry = 0;

	out = fopen("/dev/lp", "w");
	if (out == NULL)
		return;
	stty(fileno(out), &ttyb);
	putc('\f', out);
	fclose(out);
}

#define	ACK	06
#define	NAK	025
#define	STX	2
#define	ETX	3

nothing()
{
	;
}
