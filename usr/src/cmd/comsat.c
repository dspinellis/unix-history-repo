static	char *sccsid = "@(#)comsat.c	4.2 (Berkeley) 10/20/80";
#include <stdio.h>
#include <sys/mx.h>
#include <sgtty.h>
#include <utmp.h>
#include <sys/types.h>
#include <stat.h>
#include <wait.h>
#include <signal.h>

/*
 * comsat
 */
#define	dprintf	if (0) printf
int	xd;

struct  ctp {
	short   ctrl;
	short   ctrlarg;
	struct  sgttyb ctrlv;
} ctp;

#define MAXUTMP 100		/* down from init */

struct	utmp utmp[100];
int	nutmp;
int	uf;
unsigned utmpmtime;			/* last modification time for utmp */
int	onalrm();

#define NAMLEN (sizeof (uts[0].ut_name) + 1)

main(argc, argv)
char **argv;
{
	register cc;
	char buf[BUFSIZ];

	if (fork())
		exit();
	chdir("/usr/spool/mail");
	if((uf = open("/etc/utmp",0)) < 0)
		perror("/etc/utmp"), exit(1);
	while (fork())
		wait(0);
	sleep(10);
	onalrm();
	sigset(SIGALRM, onalrm);
	sigignore(SIGTTOU);
	unlink("/dev/mail");
	xd = mpx("/dev/mail", 0666);
	if (xd < 0) {
		close(2);
		open("/dev/console", 1);
		perror("/dev/mail");
		exit(1);
	}
	while((cc=read(xd, buf, BUFSIZ)) >= 0) {
		dprintf("0: got %d bytes\n", cc);
		unpack(buf, cc);
	}
	_exit(1);
}

#define	skip(rp, c)	((struct rh *)(((char *)rp)+c))

unpack(rp, cc)
	register struct rh *rp;
{
	register struct rh *end;
	int i;

	i = 0;
	end = skip(rp, cc);
	while (rp < end) {
		dprintf("%d: ", ++i);
		if (rp->count==0) {
			dprintf("%d byte control message\n", rp->ccount);
			control(rp->index, rp+1, rp->ccount);
		} else {
			dprintf("%*.*s\n", rp->count, rp->count, rp+1);
			sighold(SIGALRM);
			mailfor(rp+1);
			sigrelse(SIGALRM);
		}
		rp->count += rp->ccount;
		if (rp->count & 1)
			rp->count++;
		rp = skip(rp, rp->count);
		rp++;
	}
}

control(x, cb, cc)
	register char *cb;
{
	register char *end;
	int cmd;
	short *sp;
	struct wh or;

	end = cb + cc;
	cmd = *cb++;
	sp = (short *)cb+1;
	switch (cmd) {

	case M_WATCH:
		dprintf("attach %x, uid %d\n", x, *sp);
		attach(x, xd);
		break;

	case M_CLOSE:
		sp = (short *)cb;
		dprintf("detach %x, uid %d\n", x, *sp);
		detach(x, xd);
		break;

	case M_IOCTL:
		dprintf("ioctl %x\n", x);
		or.index = x;
		or.count = 0;
		or.ccount = sizeof ctp;
		or.data = (char *) &ctp.ctrlarg;
		ctp.ctrlarg = M_IOANS;
		write(xd, &or, sizeof or);
		break;
		
	default:
		dprintf("unknown command %d\n", cmd);
		return;
	}
}

onalrm()
{
	struct stat statbf;
	struct utmp *utp;

	dprintf("alarm\n");
	alarm(15);
	fstat(uf,&statbf);
	if (statbf.st_mtime > utmpmtime) {
		dprintf(" changed\n");
		utmpmtime = statbf.st_mtime;
		lseek(uf, 0, 0);
		nutmp = read(uf,utmp,sizeof(utmp))/sizeof(struct utmp);
	} else
		dprintf(" ok\n");
}

mailfor(name)
	char *name;
{
	register struct utmp *utp = &utmp[nutmp];
	register char *cp;
	char *rindex();
	int offset;

	dprintf("mailfor %s\n", name);
	cp = name;
	while (*cp && *cp != '@')
		cp++;
	if (*cp == 0) {
		dprintf("bad format\n");
		return;
	}
	*cp = 0;
	offset = atoi(cp+1);
	while (--utp >= utmp)
		if (!strncmp(utp->ut_name, name, sizeof(utmp[0].ut_name)))
			if (fork() == 0) {
				signal(SIGALRM, SIG_DFL);
				alarm(30);
				notify(utp, offset), exit(0);
			} else
				while (wait3(0, WNOHANG, 0) > 0)
					continue;
}

char *cr;

notify(utp, offset)
	register struct utmp *utp;
{
	FILE *tp;
	struct sgttyb gttybuf;
	char tty[20];
	char name[sizeof (utmp[0].ut_name) + 1];
	struct stat stb;

	strcpy(tty, "/dev/");
	strncat(tty, utp->ut_line, sizeof(utp->ut_line));
	dprintf("notify %s on %s\n", utp->ut_name, tty);
	if (stat(tty, &stb) == 0 && (stb.st_mode & 0100) == 0) {
		dprintf("wrong mode\n");
		return;
	}
	if ((tp = fopen(tty,"w")) == 0) {
		dprintf("fopen failed\n");
		return;
	}
	gtty(fileno(tp),&gttybuf);
	cr = (gttybuf.sg_flags & CRMOD) ? "" : "\r";
	strncpy(name, utp->ut_name, sizeof (utp->ut_name));
	name[sizeof (name) - 1] = 0;
	fprintf(tp,"%s\n\007New mail for %s\007 has arrived:%s\n",
	    cr, name, cr);
	fprintf(tp,"----%s\n", cr);
	jkfprintf(tp, name, offset);
	 fclose(tp);
}

jkfprintf(tp, name, offset)
	register FILE *tp;
{
	register FILE *fi;
	register int linecnt, charcnt;

	dprintf("HERE %s's mail starting at %d\n",
	    name, offset);
	if ((fi = fopen(name,"r")) == NULL) {
		dprintf("Cant read the mail\n");
		return;
	}
	fseek(fi, offset, 0);
	linecnt = 7;
	charcnt = 560;
	/* 
	 * print the first 7 lines or 560 characters of the new mail
	 * (whichever comes first)
	 */
	for (;;) {
		register ch;

	 	if ((ch = getc(fi)) == EOF) {  
			fprintf(tp,"----%s\n", cr);
			break;
		}
		if (ch == '\n') {  
			fprintf(tp,"%s\n", cr);
		 	if (linecnt-- < 0) {  
				fprintf(tp,"...more...%s\n", cr);
				break;
			}
		} else if(linecnt <= 0) {  
			fprintf(tp,"...more...%s\n", cr);
			break;
		} else
			putc(ch, tp);
		if (charcnt-- == 0) {   
			fprintf(tp, "%s\n", cr);
			break;
		}
	}
}
