#ifndef lint
static char sccsid[] = "@(#)inc.c	1.2 9/25/83";
#endif

#include "mh.h"
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <strings.h>
#include <signal.h>

char   *anoyes[];       /* Std no/yes gans array        */

extern	int errno;
char    scanl[];
struct  msgs *mp;
FILE    *in, *aud;
struct  stat stbuf;
char    *locknode;
int     lockwait;       /* Secs to wait for lock-Def in strings/lockdir.c */
#define LOCKWAIT (lockwait*5) /* If lock is this old, simply ignore it! */

struct  swit switches[] = {
	"audit audit-file",     0,      /* 0 */
	"ms ms-folder",         0,      /* 1 */
	"help",                 4,      /* 2 */
	"file mail-file",	0,	/* 3 */
	0,                      0
};

#ifdef UNIXCOMP
char	unixtmp[] = "/tmp/mhXXXXXX";
#endif

main(argc, argv)
char *argv[];
{

	char *newmail, maildir[128], *folder, *from, *audfile, *srcfile;
	char mailspace[128];
	char *myname, *savemail;
	register char *cp;
	register int i, msgnum;
	long now;
	char **ap;
	char *arguments[50], **argp;
	int done();
	long time();
	int pid, status, fildes;
	extern char _sobuf[];

	setbuf(stdout, _sobuf);
#ifdef NEWS
	m_news();
#endif

	from = 0; folder = 0; audfile = 0; srcfile = 0;
	cp = r1bindex(argv[0], '/');
	if((cp = m_find(cp)) != NULL) {
		ap = brkstring(cp = getcpy(cp), " ", "\n");
		ap = copyip(ap, arguments);
	} else
		ap = arguments;
	copyip(argv+1, ap);
	argp = arguments;
	while(cp = *argp++) {
		if(*cp == '-')
			switch(smatch(++cp, switches)) {
			case -2:ambigsw(cp, switches);       /* ambiguous */
				goto leave;
							     /* unknown */
			case -1:fprintf(stderr, "inc: -%s unknown\n", cp);
				goto leave;
			case 0: if(!(audfile = *argp++)) {   /* -audit */
      missing:  fprintf(stderr, "inc: Missing argument for %s switch\n", argp[-2]);
					goto leave;
				}
				continue;
			case 1: if(!(from = *argp++))        /* -ms */
					goto missing;
				continue;
			case 2:                              /* -help */
				help("inc [+folder]  [switches]", switches);
				goto leave;
			case 3:				     /* -file */
				if (!(srcfile = *argp++))
					goto missing;
				continue;
			}
		if(*cp == '+') {
			if(folder) {
				fprintf(stderr, "Only one folder at a time.\n");
				goto leave;
			} else
				folder = cp + 1;
		} else {
			fprintf(stderr, "Bad arg: %s\n", argp[-1]);
	fprintf(stderr, "Usage: inc [+folder] [-ms ms-folder] [-audit audit-file]\n");
			goto leave;
		}
	}
	if (from && srcfile) {
		fprintf(stderr, "Only one of \"-ms\" and \"-file\" allowed\n");
		goto leave;
	}
	if(from)
		newmail = from;
	else {
		if((myname = getenv("USER")) == 0) {
			fprintf(stderr,
"Environment Variable \"USER\" Must be set to your login name!\n");
			done(1);
		}
		if (srcfile) {
			newmail = srcfile;
			goto statit;
		}
		newmail = concat(mailboxes, myname, 0);
		if (stat(newmail, &stbuf) >= 0
		    && (stbuf.st_mode & S_IFMT) == S_IFDIR) {
			strcpy(mailspace, newmail);
			newmail = mailspace;
			strcat(newmail, "/");
			strcat(newmail, myname);
		}
statit:
		if(stat(newmail, &stbuf) < 0 ||
		   stbuf.st_size == 0) {
			fprintf(stderr, "No Mail to incorporate.\n");
			goto leave;
		}
	}
	fildes = -1;
	if (srcfile)
		if ((fildes = open(srcfile, 0)) < 0) {
			perror(srcfile);
			goto leave;
		}
	if(!folder) {
		folder = defalt;
		if(from && strcmp(from, "inbox") == 0) {
			cp = concat("Do you really want to convert from ",
				from, " into ", folder, "?? ", 0);
			if(!gans(cp, anoyes))
				goto leave;
			cndfree(cp);
		}
	}
	copy(m_maildir(folder), maildir);
	if(stat(maildir, &stbuf) < 0) {
		if(errno != ENOENT) {
			fprintf(stderr, "Error on folder ");
			perror(maildir);
			goto leave;
		}
		cp = concat("Create folder \"", maildir, "\"? ", 0);
		if(!gans(cp, anoyes))
			goto leave;
		if(!makedir(maildir)) {
			fprintf(stderr, "Can't create folder \"%s\"\n", maildir);
			goto leave;
		}
	}
	if(chdir(maildir) < 0) {
		fprintf(stderr, "Can't chdir to: ");
		perror(maildir);
		goto leave;
	}
	if(!(mp = m_gmsg(folder))) {
		fprintf(stderr, "Can't read folder!?\n");
		goto leave;
	}
					/* Lock the mail file */
	if(!from && !srcfile) {
		signal(SIGINT, done);
		cp = concat(lockdir, myname, ".lock", 0);
		for(i = 0; i < lockwait; i += 2) {
			if(link(newmail, cp) == -1) {
				fprintf(stderr, "Mailbox busy...\n");
				if(i == 0 && stat(newmail, &stbuf) >= 0)
					if(stbuf.st_ctime + LOCKWAIT < time((long *)0)) {
						unlink(cp);
						fprintf(stderr, "Removing lock.\n");
						continue;
					}
				sleep(2);
			} else {
				locknode = cp;  /* We own the lock now! */
				break;
			}
		}
		if(i >= lockwait) {
			fprintf(stderr, "Try again.\n");
			done(1);
		}
	}

#ifdef UNIXCOMP
	/*
	 * If trying to be compatible with standard
	 * UNIX mailing scheme, call the program unixtomh
	 * to convert the mailbox to some temporary name.
	 */
	
	mktemp(unixtmp);
	switch  (pid = fork()) {
	case 0:
		if (srcfile) {
			int nout;

			if ((nout = creat(unixtmp, 0600)) < 0) {
				perror(unixtmp);
				_exit(1);
			}
			close(0);
			dup(fildes);
			close(fildes);
			close(1);
			dup(nout);
			close(nout);
			execl(unixtomh, "unixtomh", 0);
			perror(unixtomh);
			_exit(1);
		}
		execl(unixtomh, "unixtomh", newmail, unixtmp, 0);
		perror(unixtomh);
		_exit(1);
		break;

	case -1:
		perror("fork");
		goto leave;

	default:
		while (wait(&status) != pid)
			;
		if (status != 0) {
			fprintf(stderr, "unixtomh failed!?\n");
			goto leave;
		}
	}
	if (fildes >= 0)
		close(fildes);
	savemail = newmail;
	newmail = unixtmp;
	if((in = fopen(newmail, "r")) == NULL) {
		fprintf(stderr, "Can't open "); perror(newmail);
		goto leave;
	}
#else
	if (srcfile)
		in = fdopen(fildes, "r");
	else
		if((in = fopen(newmail, "r")) == NULL) {
			fprintf(stderr, "Can't open "); perror(newmail);
			goto leave;
		}
#endif

	if(audfile) {
		cp = m_maildir(audfile);
		if((i = stat(cp, &stbuf)) < 0)
			fprintf(stderr, "Creating Receive-Audit: %s\n", cp);
		if((aud = fopen(cp, "a")) == NULL) {
			fprintf(stderr, "Can't append to ");
			perror(cp);
			goto leave;
		} else if(i < 0)
			chmod(cp, 0600);
		time(&now);
		fputs("<<inc>> ", aud);
		cp = cdate(&now);
		cp[9] = ' ';
		fputs(cp, aud);
		if(from) {
			fputs("  -ms ", aud);
			fputs(from, aud);
		}
		putc('\n', aud);
	}
	printf("Incorporating new mail into %s...\n\n", folder);
	fflush(stdout);
	msgnum = mp->hghmsg;

	while((i = scan(in, msgnum+1, msgnum+1, msgnum == mp->hghmsg))) {
		if(i == -1) {
			fprintf(stderr, "inc aborted!\n");
			if(aud)
				fputs("inc aborted!\n", aud);
			goto leave;
		}
		if(aud)
			fputs(scanl, aud);
		fflush(stdout);
		msgnum++;
	}

	fclose(in);
	if(aud)
		fclose(aud);

#ifdef UNIXCOMP
	unlink(newmail);
	newmail = savemail;
#endif

	if(!from && !srcfile) {
		if (unlink(newmail) < 0) {
			if((i = creat(newmail, 0600)) >= 0)
				close(i);
			else
				fprintf(stderr, "Error zeroing %s\n", newmail);
		}
	}

	i = msgnum - mp->hghmsg;
	if(!i)
		fprintf(stderr, "[No messages incorporated.]\n");
	else {
		m_replace(pfolder, folder);
		m_setcur(mp->hghmsg + 1);
	}
leave:
	m_update();
	done(0);
}


done(status)
{
	if(locknode);
		unlink(locknode);
	exit(status);
}
