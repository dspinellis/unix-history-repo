#ifndef lint
static char sccsid[] = "@(#)deliver.c	4.4 9/25/83";
#endif

#include "mh.h"
#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/stat.h>
#include <signal.h>
#include <strings.h>

#define	DELIVERMAIL	"/usr/lib/sendmail"
#define EVERYONE 10
#define RECVPROG "/.mh_receive"
#define FCCS    10      /* Max number of fcc's allowed */

struct shome {          /* Internal name/uid/home database */
	struct shome *h_next;
	char *h_name;
	int   h_uid;
	int   h_gid;
	char *h_home;
} *homes, *home();

struct swit switches[] = {
	"debug",         -1,      /* 0 */
	"format",         0,      /* 1 */
	"noformat",       0,      /* 2 */
	"msgid",          0,      /* 3 */
	"nomsgid",        0,      /* 4 */
	"verbose",        0,      /* 5 */
	"noverbose",      0,      /* 6 */
	"help",           4,      /* 7 */
	0,                0
};

int     verbose, format, msgid, debug, myuid, addrp;
int     lockwait;       /* Secs to wait for mail lock (From strings/lockdir.c) */
#define LOCKWAIT (lockwait * 5) /* Ignore lock if older than this */
int     donecd;
long    now;
char    tmpfil[32], fccfold[FCCS][128];
char	tmp2fil[32];
int     fccind;
char	*rindex();

struct  mailname {
	struct mailname *m_next;
	char            *m_name;
}  addrlist;

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *cp, *addrs, **argp;
	register struct mailname *lp;
	char buf[BUFSIZ], name[NAMESZ];
	char *msg;
	int state, compnum, fd;
	FILE *in, *out;
	char *getname();

	msg = 0;
	argp = argv + 1;
	while(cp = *argp++) {
		if(*cp == '-')
			switch(smatch(++cp, switches)) {
			case -2:ambigsw(cp, switches);       /* ambiguous */
				done(1);
							/* unknown */
			case -1:fprintf(stderr, "sndmsg: -%s unknown switch.\n", cp);
				done(1);
			case 0: verbose++; debug++; continue; /* -debug */
			case 1: fprintf(stderr, "Deliver: -format not yet.\n");
				done(1);
/***                    case 1: format = 1; continue;         /* -format */
			case 2: format = 0; continue;         /* -noformat */
			case 3: msgid = 1;  continue;         /* -msgid */
			case 4: msgid = 0;  continue;         /* -nomsgid */
			case 5: verbose = 1; continue;        /* -verbose */
			case 6: verbose = 0; continue;        /* -noverbose */
			case 7: help("deliver [switches] file",
				     switches);
				done(1);
			}
		if(msg) {
			fprintf(stderr, "Deliver: Only one message at a time!\n");
			done(1);
		} else
			msg = cp;
	}
	if(!msg) {
		fprintf(stderr, "Deliver: No Message specified.\n");
		fprintf(stderr, "Deliver: Usage: deliver [switches] file\n");
		done(1);
	}
	if((in = fopen(msg, "r")) == NULL) {
		fprintf(stderr, "Deliver: Can't open ");
		perror(msg);
		done(1);
	}
	copy(makename("locs", ".tmp"), copy("/tmp/", tmpfil));
	if(!debug) {
		if((out = fopen(tmpfil, "w")) == NULL) {
			fprintf(stderr, "Can't create %s\n", tmpfil);
			done(1);
		}
		chmod(tmpfil, 0744);
	} else
		out = stdout;

	gethomes();
	myuid = getuid();
#ifndef UNIXCOMP
	putdate(0, out);           /* Tack on the date */
	putfrom(out);           /* Tack on the from */
#endif
	putmsgid(out);          /* and msg id if desired */
	for(addrs = 0, compnum = 1, state = FLD;;) {
		state = m_getfld(state, name, buf, sizeof buf, in);
   swt:         switch(state) {

		case FLD:
		case FLDEOF:
		case FLDPLUS:
			compnum++;
			if(uleq(name, "to") || uleq(name, "cc") ||
			   uleq(name, "bcc"))
				addrs = add(buf, addrs);
			if(!uleq(name, "bcc")) {
				cp = buf;
				while(*cp == ' ' || *cp == '\t')
					cp++;
				fprintf(out, "%s: %s", name, cp);
				if(uleq(name, "fcc")) {
					if(fccind >= FCCS) {
						fprintf(stderr, "Deliver: too many fcc's.\n");
						done(1);
					}
					copy(cp, fccfold[fccind]);
					if(cp = rindex(fccfold[fccind], '\n'))
						*cp = 0;
					fccind++;
				}
			}
			while(state == FLDPLUS || state == FLDEOF) {
				state = m_getfld(state, name, buf, sizeof buf, in);
				addrs = add(buf, addrs);
				fputs(buf, out);
			}
			if(state == FLDEOF)
				goto process;
			break;

		case BODY:
		case BODYEOF:
			fprintf(out, "\n%s", buf);
			while(state == BODY) {
				state=m_getfld(state,name,buf,sizeof buf,in);
				fputs(buf, out);
			}

		case FILEEOF:
			goto process;

		case LENERR:
		case FMTERR:
			fprintf(stderr, "??Message Format Error ");
			fprintf(stderr, "in Component #%d.\n", compnum);
			done(1);

		default:
			fprintf(stderr, "Getfld returned %d\n", state);
			done(1);
		}
	}
process:
	if(!debug)
		fclose(out);
	else
		printf("-----\n");
	fclose(in);
	while(cp = getname(addrs))      /* Put addrs in chain */
		insert(cp);
	if(debug) {
		printf("Before alias(): "); pl();
	}

	alias();                        /* Map names if needed */

	for(lp = addrlist.m_next; lp; lp = lp->m_next) {
#ifdef UNIXCOMP
		if (netname(lp->m_name))
			continue;
#endif
#ifndef DELIVERMAIL
		if(home(lp->m_name) == NULL) {
			fprintf(stderr, "Deliver: Unknown User: %s.\n", lp->m_name);
			fprintf(stderr, "Deliver: Message Not Delivered.\n");
			if(!debug)
				unlink(tmpfil);
			done(1);
		}
#endif DELIVERMAIL
	}
	if(debug) {
		printf("Addrs: "); pl();
	}

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	if(!debug) {                    /* Send the mail */
		fd = open(tmpfil, 0);
		for(lp = addrlist.m_next; lp; lp = lp->m_next)
			sendmail(lp->m_name, fd);
		stflush(fd);
		close(fd);
		if(fccind) {
#ifdef UNIXCOMP
			strcpy(tmp2fil, "/tmp/mhFXXXXXX");
			mktemp(tmp2fil);
			out = fopen(tmp2fil, "w");
			putdate(0, out);           /* Tack on the date */
			putfrom(out);           /* Tack on the from */
			appendfile(tmpfil, out);
			fclose(out);
			for(state = 0; state < fccind; state++)
				fcc(tmp2fil, fccfold[state]);
			unlink(tmp2fil);
#else
			for(state = 0; state < fccind; state++)
				fcc(tmp2fil, fccfold[state]);
#endif
		}
		unlink(tmpfil);
	}
	done(donecd);
}

#ifdef UNIXCOMP

appendfile(name, fout)
	char name[];
	register FILE *fout;
{
	register int c;
	register FILE *fin;

	if ((fin = fopen(name, "r")) == NULL) {
		perror(name);
		return;
	}
	while ((c = getc(fin)) != EOF)
		putc(c, fout);
	fclose(fin);
}

netname(name)
	char name[];
{
	register char *cp;
	char *index();

	cp = name;
	if (index(cp, ':') || index(cp, '!') ||
	    index(cp, '@') || index(cp, '^'))
		return(1);
	return(0);
}
#endif


gethomes()
{
	register struct passwd *pw;
	register struct shome *h, *ph;
	struct passwd *getpwent();

	ph = (struct shome *) &homes;
	while((pw = getpwent()) != NULL) {
		h = (struct shome *) malloc(sizeof *h);
		h->h_next = NULL;
		h->h_name = (char *) malloc(strlen(pw->pw_name)+1);
		strcpy(h->h_name, pw->pw_name);
		h->h_uid = pw->pw_uid;
		h->h_gid = pw->pw_gid;
		h->h_home = (char *) malloc(strlen(pw->pw_dir)+1);
		strcpy(h->h_home, pw->pw_dir);
		ph->h_next = h;
		ph = h;
	}
}


struct shome *
home(name)
	register char *name;
{
	register struct shome *h;

	for(h = homes; h; h = h->h_next)
		if(uleq(name, h->h_name))
			return(h);
	return(NULL);
}


char *
getname(addrs)
	char *addrs;
{
	register char *tp;
	static char name[32];
	static char *getaddrs;

	if(!getaddrs)
		getaddrs = addrs;
	tp = name;

	while(*getaddrs && !isalpha(*getaddrs))
		getaddrs++;
	if(!*getaddrs) {
		getaddrs = 0;
		return NULL;
	}
#ifdef UNIXCOMP
	while(!isspace(*getaddrs))
		*tp++ = *getaddrs++;
#else
	while(isalnum(*getaddrs) || index(".:@/-", *getaddrs))
		*tp++ = *getaddrs++;
#endif
	*tp = 0;
	if(name[0] == 0)
		return NULL;
	return name;
}

char *bracket = "\1\1\1\1\n";

sendmail(name, fd)              /* Runs with signals ignored! */
	char *name;
	register int fd;
{
	char buf[BUFSIZ], *myname;
	register int i, m, c;
	register char *mail, *receive, *lock = 0;
	register struct shome *h;
	int pid, status;
	struct stat stbuf;

	if(verbose) {
		printf("%s: ", name);
		fflush(stdout);
	}
	lseek(fd, 0l, 0);
	h = home(name);

	/*
	 * If no home for this user, as in network address
	 * being forced down this path, just assume normal
	 * delivery
	 */

	if (h == NULL)
		goto force;

	mail = concat(mailboxes, h->h_name, 0);
/***    mail = concat(h->h_home, mailbox, 0);           ***/
	receive = concat(h->h_home, RECVPROG, 0);
	if(access(receive, 1) == 0) {   /* User has a receive prog */

		calluserprog(receive, mail, fd, h);

	} else {

force:

#ifdef UNIXCOMP
#ifdef DELIVERMAIL
		nstash(name);
		if (verbose)
			printf("Sent.\n");
		return;
#endif DELIVERMAIL
#ifndef DELIVERMAIL
		myname = getenv("USER");
		if (myname == (char *) 0)
			myname = "nobody";
		pid = fork();
		switch (pid) {
		case 0:
			/* fiddle with files, then . . . */
			lseek(fd, 0L, 0);
			close(0);
			dup(fd);
			close(fd);
			execl(Mailprog, "Mail", "-r", myname, name, 0);
			execl("/bin/mail", "mail", "-r", myname, name, 0);
			perror(Mailprog);
			_exit(1);
		
		case -1:
			perror("fork");
			return;

		default:
			while (wait(&status) != pid)
				;
		}
#endif DELIVERMAIL
#endif UNIXCOMP
#ifndef UNIXCOMP
		if((m = open(mail, 2)) < 0) {
			if((m = creat(mail, 0600)) < 0) {
				fprintf(stderr, "Deliver: Can't write ");
				perror(mail);
				goto out;
			}
			chown(mail, h->h_uid, h->h_gid);
			if(verbose) {
				fprintf(stderr, "Creating %s ", mail);
				fflush(stdout);
			}
		}
		lock = concat(lockdir, h->h_name, ".lock", 0);
		for(i = 0; i < lockwait; i += 2) {
			if(link(mail, lock) >= 0)
				break;
			if(i == 0 && stat(mail, &stbuf) >= 0 &&
			   stbuf.st_ctime + LOCKWAIT < time((long *) 0)) {
				i = lockwait;
				break;
			}
			if(verbose) {
				printf("Busy ");
				fflush(stdout);
			}
			sleep(2);
		}
		if(i >= lockwait) {
			unlink(lock);
			if(verbose) {
				printf("Removing lock. ");
				fflush(stdout);
			}
			if(link(mail, lock) < 0) {
				fprintf(stderr, "Can't lock %s to %s\n",
					mail, lock);
				donecd = 1;
				goto out1;
			}
		}
		lseek(m, 0l, 2);
		write(m, bracket, 5);
		do
			if((c = read(fd, buf, sizeof buf)) > 0)
				if(write(m, buf, c) != c) {
					fprintf(stderr, "Write error on ");
					perror(mail);
					donecd = 1;
					goto out1;
				}
		while(c == sizeof buf);
		write(m, bracket, 5);
out1:           close(m);
out:            cndfree(mail);
		if(lock) {
			unlink(lock);
			cndfree(lock);
		}
#endif UNIXCOMP
	}
	if(verbose && !donecd)
		printf("Sent.\n");
}

#ifdef DELIVERMAIL

/*
 * Salt away a name to send to, ultimately.
 */

char	*stashname[100];
char	**stashp;

nstash(name)
	char name[];
{
	register char *cp;

	cp = (char *) calloc(1, strlen(name) + 1);
	strcpy(cp, name);
	if (stashp == 0) {
		stashp = stashname;
		*stashp++ = "sendmail";
		*stashp++ = "-i";
		if (verbose)
			*stashp++ = "-v";
	}
	*stashp++ = cp;
}

stflush(fd)
{
	int pid;

	if (stashp == 0)
		return;
	while ((pid = fork()) == -1)
		sleep(2);
	if (pid == 0) {
		signal(SIGHUP, SIG_IGN);
		signal(SIGINT, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		close(0);
		dup(fd);
		close(fd);
		execv(DELIVERMAIL, stashname);
		perror(DELIVERMAIL);
		_exit(1);
	}
}
#endif

putfrom(out)
	register FILE *out;
{
	register struct shome *h;

	for(h = homes; h; h = h->h_next)
		if(h->h_uid == myuid) {
			fprintf(out, "From: %s\n", h->h_name);
			return;
		}
	fprintf(stderr, "Deliver: WHO ARE YOU?\n");
	done(1);
}


putmsgid(sp)
	FILE *sp;
{

	if(!msgid)
		return;
	if(!now)
		time(&now);
	fprintf(sp, "Message-ID: <%u.%u.%ld.%s@%s>\n", getpid(), myuid,
	    now, localname, hostname);
}

/***/
pl()
{
	register struct mailname *mp;

	for(mp = addrlist.m_next; mp; mp=mp->m_next)
		printf("%s%s", mp->m_name, mp->m_next?", ":"");
	printf("\n");
}
/***/

insert(name)
	char *name;
{
	register struct mailname *mp;
	char *getcpy();

/***    printf("insert(%s)\n", name);   ***/

	for(mp = &addrlist; mp->m_next; mp = mp->m_next)
		if(uleq(name, mp->m_next->m_name))
			return;         /* Don't insert existing name! */
	mp->m_next = (struct mailname *) malloc(sizeof *mp->m_next);
	mp = mp->m_next;
	mp->m_next = 0;
	mp->m_name = getcpy(name);
}


/* alias implementation below...
 */



#define GROUP   "/etc/group"
char    *AliasFile =    "/usr/local/lib/MailAliases";

char *termptr;

char *
parse(ptr, buf)
	register char *ptr;
	char *buf;
{
	register char *cp;

	cp = buf;
	while(isspace(*ptr) || *ptr == ',' || *ptr == ':')
		ptr++;
	while(isalnum(*ptr) || *ptr == '/' || *ptr == '-' ||
	      *ptr == '.' || *ptr == '*')
		*cp++ = *ptr++;
	if(cp == buf) {
		switch(*ptr) {
		case '<':
		case '=':
			*cp++ = *ptr++;
		}
	}
	*cp = 0;
	if(cp == buf)
		return 0;
	termptr = ptr;
	return buf;
}

char *
advance(ptr)
	register char *ptr;
{
	return(termptr);
}

alias()
{
	register char *cp, *pp;
	register struct mailname *lp;
	char line[256], pbuf[64];
	FILE *a;

	if((a = fopen(AliasFile, "r")) == NULL)
		return;
	while(fgets(line, sizeof line, a)) {
		if(line[0] == ';' || line[0] == '\n')   /* Comment Line */
			continue;
		if((pp = parse(line, pbuf)) == NULL) {
	    oops:       fprintf(stderr, "Bad alias file %s\n", AliasFile);
			fprintf(stderr, "Line: %s", line);
			done(1);
		}
		for(lp = &addrlist; lp->m_next; lp = lp->m_next) {
			if(aleq(lp->m_next->m_name, pp)) {
				remove(lp);
				if(!(cp = advance(line)) ||
				   !(pp = parse(cp, pbuf)))
					goto oops;
				switch(*pp) {
				case '<':       /* From file */
					cp = advance(cp);
					if((pp = parse(cp, pbuf)) == NULL)
						goto oops;
					addfile(pp);
					break;
				case '=':       /* UNIX group */
					cp = advance(cp);
					if((pp = parse(cp, pbuf)) == NULL)
						goto oops;
					addgroup(pp);
					break;
				case '*':       /* ALL Users */
					addall();
					break;
				default:        /* Simple list */
					for(;;) {
						insert(pp);
						if(!(cp = advance(line)) ||
						   !(pp = parse(cp, pbuf)))
							break;
					}
				}
				break;
			}
		}
	}
}


addfile(file)
	char *file;
{
	register char *cp, *pp;
	char line[128], pbuf[64];
	FILE *f;

/***    printf("addfile(%s)\n", file);          ***/
	if((f = fopen(file, "r")) == NULL) {
		fprintf(stderr, "Can't open ");
		perror(file);
		done(1);
	}
	while(fgets(line, sizeof line, f)) {
		cp = line;
		while(pp = parse(cp, pbuf)) {
			insert(pp);
			cp = advance(cp);
		}
	}
	fclose(f);
}

addgroup(group)
	char *group;
{
	register char *cp, *pp;
	int found = 0;
	char line[128], pbuf[64], *rindex();
	FILE *f;

/***    printf("addgroup(%s)\n", group);        ***/
	if((f = fopen(GROUP, "r")) == NULL) {
		fprintf(stderr, "Can't open ");
		perror(GROUP);
		done(1);
	}
	while(fgets(line, sizeof line, f)) {
		pp = parse(line, pbuf);
		if(strcmp(pp, group) == 0) {
			cp = rindex(line, ':');
			while(pp = parse(cp, pbuf)) {
				insert(pp);
				cp = advance(cp);
			}
			found++;
		}
	}
	if(!found) {
		fprintf(stderr, "Group: %s non-existent\n", group);
		done(1);
	}
	fclose(f);
}

addall()
{
	register struct shome *h;

/***    printf("addall()\n");                   ***/
	for(h = homes; h; h = h->h_next)
		if(h->h_uid >= EVERYONE)
			insert(h->h_name);
}

remove(mp)              /* Remove NEXT from argument node! */
	register struct mailname *mp;
{
	register struct mailname *rp;

	rp = mp->m_next;
	mp->m_next = rp->m_next;
	cndfree(rp->m_name);
	cndfree(rp);
}

int     alarmed;

alarmclock()
{
	alarmed++;
}

extern  char **environ;
char	envhome[60] = "HOME=";
char	envname[60] = "NAME=";
char	*envinit[] = {envhome, envname, (char *) 0};

calluserprog(prog, mail, fd, h)
	char *prog, *mail;
	int fd;
	register struct shome *h;
{
	register int pid, child, i;
	int status;

	if(verbose) {
		printf("Invoking %s ", prog);
		fflush(stdout);
	}
	i = 0;
	while((child = fork()) == -1)
		if(++i > 10) {
			fprintf(stderr, "Can't get a fork to invoke %s!\n",
				prog);
			donecd = 1;
			return;
		} else
			sleep(2);
	if(child == 0) {                /* In child... */
		if(fd != 3)
			dup2(fd, 3);
		for(i = 4; i < 15; i++)
			close(i);
		strcat(envname, h->h_name);
		strcat(envhome, h->h_home);
		environ = envinit;
		setgid(h->h_gid);
		setuid(h->h_uid);
		execlp(prog, prog, tmpfil, mail, h->h_home, 0);
		perror(prog);
		done(-1);
	}
	signal(SIGALRM, alarmclock);
	alarmed = 0;
	alarm(30);                      /* Give receive proc 30 secs */
	status = 0;
	while((pid = wait(&status)) != -1 && pid != child && !alarmed) ;
	if(alarmed) {
		kill(0, SIGINT);
		signal(SIGALRM, alarmclock);
		alarmed = 0;
		alarm(30);
		while((pid = wait(&status)) != -1 && pid != child && !alarmed) ;
		if(alarmed) {
			kill(child, SIGKILL);
			signal(SIGALRM, alarmclock);
			alarmed = 0;
			alarm(30);
			while((pid = wait(&status)) != -1 && pid != child && !alarmed) ;
		}
		fprintf(stderr, "Deliver: Killed %s--Took more than 30 seconds!\n",
			prog);
		donecd = 1;
		status = 0;
	} else
		alarm(0);
	if(status) {
		printf("Deliver: %s error %d from %s on delivery to %s\n",
			status&0377? "System" : "User",
			status&0377? status&0377 : status>>8,
			prog, h->h_name);
		donecd = 1;
	}
	if(pid == -1) {
		fprintf(stderr, "Deliver: wait on receive process returned -1\n");
		perror("");
		donecd = 1;
	}
}

aleq(string, aliasent)
	register char *string, *aliasent;
{
	register int c;

	while(c = *string++)
		if(*aliasent == '*')
			return 1;
		else if((c|040) != (*aliasent|040))
			return(0);
		else
			aliasent++;
	return(*aliasent == 0 | *aliasent == '*');
}

fcc(file, folder)
	char *file, *folder;
{
	int child, pid, status;
	char fold[128];

	if(verbose) {
		printf("Fcc: %s...", folder);
		fflush(stdout);
	}
	while((child = fork()) == -1) sleep(5);
	if(child == 0) {
		if(*file != '+')
			strcpy(fold, "+");
		strcat(fold, folder);
		setuid(myuid);
		execl(fileproc, "file", "-link", "-file", file, fold, 0);
		exit(-1);
	} else while((pid = wait(&status)) != -1 && pid != child) ;
	if(status)
		fprintf(stderr, "Deliver: Error on fcc to %s\n", folder);
	else if(verbose)
		putchar('\n');
}
