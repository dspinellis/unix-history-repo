/*
 *
 * utils.c - miscellaneous utilities
 *
 * This file is part of zsh, the Z shell.
 *
 * This software is Copyright 1992 by Paul Falstad
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 *
 */

#include "zsh.h"
#include <pwd.h>
#include <errno.h>
#ifdef __hpux
#include <ndir.h>
#else
#ifndef SYSV
#include <sys/dir.h>
#endif
#endif
#include <fcntl.h>

#ifdef SYSV
#define direct dirent
#undef TIOCGWINSZ
int readlink(s,t,z)
char *s;char *t;int z;
{
return -1;
}
#endif

/* source a file */

int source(s) /**/
char *s;
{
int fd,cj = thisjob;
int oldlineno = lineno,oldshst;
FILE *obshin = bshin;

	fd = SHIN;
	lineno = 0;
	oldshst = opts[SHINSTDIN];
	opts[SHINSTDIN] = OPT_UNSET;
	if ((SHIN = movefd(open(s,O_RDONLY))) == -1)
		{
		SHIN = fd;
		thisjob = cj;
		opts[SHINSTDIN] = oldshst;
		return 1;
		}
	bshin = fdopen(SHIN,"r");
	loop();
	fclose(bshin);
	bshin = obshin;
	opts[SHINSTDIN] = oldshst;
	SHIN = fd;
	thisjob = cj;
	errflag = 0;
	retflag = 0;
	lineno = oldlineno;
	return 0;
}

/* try to source a file in the home directory */

void sourcehome(s) /**/
char *s;
{
char buf[MAXPATHLEN];
char *h;

	if (!(h = getsparam("ZDOTDIR")))
		h = home;
	sprintf(buf,"%s/%s",h,s);
	(void) source(buf);
}

/* print an error */

void zerrnam(cmd,fmt,str,num) /**/
char *cmd; char *fmt; char *str;int num;
{
	if (cmd)
		{
		if (errflag || noerrs)
			return;
		errflag = 1;
		trashzle();
		if (isset(SHINSTDIN))
			fprintf(stderr,"%s: ",cmd);
		else
			fprintf(stderr,"%s: %s: ",argzero,cmd);
		}
	while (*fmt)
		if (*fmt == '%')
			{
			fmt++;
			switch(*fmt++)
				{
				case 's':
					while (*str)
						niceputc(*str++,stderr);
					break;
				case 'l':
					while (num--)
						niceputc(*str++,stderr);
					break;
				case 'd':
					fprintf(stderr,"%d",num);
					break;
				case '%':
					putc('%',stderr);
					break;
				case 'c':
					niceputc(num,stderr);
					break;
				case 'e':
					if (num == EINTR)
						{
						fputs("interrupt\n",stderr);
						errflag = 1;
						return;
						}
					if (num == EIO)
						fputs(sys_errlist[num],stderr);
					else
						{
						fputc(tulower(sys_errlist[num][0]),stderr);
						fputs(sys_errlist[num]+1,stderr);
						}
					break;
				}
			}
		else
			putc(*fmt++,stderr);
	if (unset(SHINSTDIN) && lineno)
		fprintf(stderr," [%ld]\n",lineno);
	else
		putc('\n',stderr);
	fflush(stderr);
}

void zerr(fmt,str,num) /**/
char *fmt; char *str;int num;
{
	if (errflag || noerrs)
		return;
	errflag = 1;
	trashzle();
	fprintf(stderr,"%s: ",(isset(SHINSTDIN)) ? "zsh" : argzero);
	zerrnam(NULL,fmt,str,num);
}

void niceputc(c,f) /**/
int c;FILE *f;
{
	if (itok(c))
		{
		if (c >= Pound && c <= Comma)
			putc(ztokens[c-Pound],f);
		return;
		}
	c &= 0xff;
	if (isprint(c))
		putc(c,f);
	else if (c == '\n')
		{
		putc('\\',f);
		putc('n',f);
		}
	else
		{
		putc('^',f);
		putc(c|'@',f);
		}
}

/* enable ^C interrupts */

void intr() /**/
{
#ifdef SV_INTERRUPT
static struct sigvec vec = { handler,sigmask(SIGINT),SV_INTERRUPT };

	if (interact)
		sigvec(SIGINT,&vec,NULL);
#else
	if (interact)
		signal(SIGINT,handler);
#endif
}

void noholdintr() /**/
{
	intr();
}

void holdintr() /**/
{
#ifdef SV_INTERRUPT
static struct sigvec vec = { handler,sigmask(SIGINT),0 };

	if (interact) sigvec(SIGINT,&vec,NULL);
#else
	if (interact) signal(SIGINT,SIG_IGN);
#endif
}

#ifndef	__386BSD__
char *fgetline(buf,len,in) /**/
char *buf;int len;FILE *in;
{
	if (!fgets(buf,len,in))
		return NULL;
	buf[len] = '\0';
	buf[strlen(buf)-1] = '\0';
	return buf;
}
#endif

/* get a symlink-free pathname for s relative to PWD */

char *findpwd(s) /**/
char *s;
{
char *t;

	if (*s == '/')
		return xsymlink(s);
	s = tricat((pwd[1]) ? pwd : "","/",s);
	t = xsymlink(s);
	free(s);
	return t;
}

static char xbuf[MAXPATHLEN];

#if 0
char *fixpwd(s) /**/
char *s;
{
struct stat sbuf,tbuf;
char *t;

	strcpy(xbuf,"");
	if (*s == '/')
		t = ztrdup(s);
	else
		t = tricat((pwd[1]) ? pwd : "","/",s);
	(void) xsymlinks(t+1,0); 
	free(t);
	if (!*xbuf)
		strcpy(xbuf,"/");
	if (stat(xbuf,&sbuf) == 0 && stat(".",&tbuf) == 0)
		if (!(sbuf.st_dev == tbuf.st_dev && sbuf.st_ino == tbuf.st_ino))
			chdir(xbuf);
	return ztrdup(xbuf);
}
#endif

int ispwd(s) /**/
char *s;
{
struct stat sbuf,tbuf;

	if (stat(s,&sbuf) == 0 && stat(".",&tbuf) == 0)
		if (sbuf.st_dev == tbuf.st_dev && sbuf.st_ino == tbuf.st_ino)
			return 1;
	return 0;
}

/* expand symlinks in s, and remove other weird things */

char *xsymlink(s) /**/
char *s;
{
	if (unset(CHASELINKS))
		return ztrdup(s);
	if (*s != '/')
		return NULL;
	strcpy(xbuf,"");
	if (xsymlinks(s+1,1))
		return ztrdup(s);
	if (!*xbuf)
		return ztrdup("/");
	return ztrdup(xbuf);
}

char **slashsplit(s) /**/
char *s;
{
char *t,**r,**q;
int t0;

	if (!*s)
		return (char **) zcalloc(sizeof(char **));
	for (t = s, t0 = 0; *t; t++)
		if (*t == '/')
			t0++;
	q  = r = (char **) zalloc(sizeof(char **)*(t0+2));
	while (t = strchr(s,'/'))
		{
		*t = '\0';
		*q++ = ztrdup(s);
		*t = '/';
		while (*t == '/')
			t++;
		if (!*t)
			{
			*q = NULL;
			return r;
			}
		s = t;
		}
	*q++ = ztrdup(s);
	*q = NULL;
	return r;
}

int islink(s) /**/
char *s;
{
	return readlink(s,NULL,0) == 0;
}

/* expands symlinks and .. or . expressions */
/* if flag = 0, only expand .. and . expressions */

int xsymlinks(s,flag) /**/
char *s;int flag;
{
char **pp,**opp;
char xbuf2[MAXPATHLEN],xbuf3[MAXPATHLEN];
int t0;

	opp = pp = slashsplit(s);
	for (; *pp; pp++)
		{
		if (!strcmp(*pp,"."))
			{
			free(*pp);
			continue;
			}
		if (!strcmp(*pp,".."))
			{
			char *p;

			free(*pp);
			if (!strcmp(xbuf,"/"))
				continue;
			p = xbuf+strlen(xbuf);
			while (*--p != '/');
			*p = '\0';
			continue;
			}
		if (unset(CHASELINKS))
			{
			strcat(xbuf,"/");
			strcat(xbuf,*pp);
			free(*pp);
			continue;
			}
		sprintf(xbuf2,"%s/%s",xbuf,*pp);
		t0 = readlink(xbuf2,xbuf3,MAXPATHLEN);
		if (t0 == -1 || !flag)
			{
			strcat(xbuf,"/");
			strcat(xbuf,*pp);
			free(*pp);
			}
		else
			{
			xbuf3[t0] = '\0'; /* STUPID */
			if (*xbuf3 == '/')
				{
				strcpy(xbuf,"");
				if (xsymlinks(xbuf3+1,flag))
					return 1;
				}
			else
				if (xsymlinks(xbuf3,flag))
					return 1;
			free(*pp);
			}
		}
	free(opp);
	return 0;
}

/* print a directory */

void fprintdir(s, f) /**/
char *s; FILE *f;
{
int t0;

	t0 = finddir(s);
	if (t0 == -1)
		{
		if (!strncmp(s,home,t0 = strlen(home)) && t0 > 1)
			{
			putc('~', f);
			fputs(s+t0,f);
			}
		else
			fputs(s,f);
		}
	else
		{
		putc('~', f);
		fputs(usernames[t0],f);
		fputs(s+strlen(userdirs[t0]),f);
		}
}

void printdir(s) /**/
char *s;
{
	fprintdir(s, stdout);
}

void printdircr(s) /**/
char *s;
{
	fprintdir(s, stdout);
	putchar('\n');
}

/* see if a path has a named directory as its prefix */

int finddir(s) /**/
char *s;
{
int t0,t1,step;

	if (userdirsz)
		{
		step = t0 = userdirsz/2;
		for(;;)
			{
			t1 = (userdirs[t0]) ? dircmp(userdirs[t0],s) : 1;
			if (!t1)
				{
				while (t0 != userdirsz-1 && userdirs[t0+1] && 
						!dircmp(userdirs[t0+1],s)) 
					t0++;
				return t0;
				}
			if (!step)
				break;
			if (t1 > 0)
				t0 = t0-step+step/2;
			else
				t0 += step/2;
			step /= 2;
			}
		}
	return -1;
}

/* add a named directory */

void adduserdir(s,t) /**/
char *s;char *t;
{
int t0,t1;

	if (!interact || ((t0 = finddir(t)) != -1 && !strcmp(s,usernames[t0])))
		return;
	if (!strcmp(t,"/"))
		return;
	if ((t0 = finddir(t)) != -1 && !strcmp(s,usernames[t0]))
		return;
	if (userdirsz == userdirct)
		{
		userdirsz *= 2;
		userdirs = (char **) realloc((char *) userdirs,
			sizeof(char **)*userdirsz);
		usernames = (char **) realloc((char *) usernames,
			sizeof(char **)*userdirsz);
		for (t0 = userdirct; t0 != userdirsz; t0++)
			userdirs[t0] = usernames[t0] = NULL;
		}
	for (t0 = 0; t0 != userdirct; t0++)
		if (strcmp(userdirs[t0],t) > 0)
			break;
	for (t1 = userdirct-1; t1 >= t0; t1--)
		{
		userdirs[t1+1] = userdirs[t1];
		usernames[t1+1] = usernames[t1];
		}
	userdirs[t0] = ztrdup(t);
	usernames[t0] = ztrdup(s);
	userdirct++;
}

int dircmp(s,t) /**/
char *s;char *t;
{
	for (; *s && *t; s++,t++)
		if (*s != *t)
			return *s-*t;
	if (!*s && (!*t || *t == '/'))
		return 0;
	return *s-*t;
}

int ddifftime(t1,t2) /**/
time_t t1;time_t t2;
{
	return ((long) t2-(long) t1);
}

/* see if jobs need printing */

void scanjobs() /**/
{
int t0;

	for (t0 = 1; t0 != MAXJOB; t0++)
		if (jobtab[t0].stat & STAT_CHANGED)
			printjob(jobtab+t0,0);
}

/* do pre-prompt stuff */

void preprompt() /**/
{
int diff;
List list;
struct schedcmd *sch,*schl;

	if (unset(NOTIFY))
		scanjobs();
	if (errflag)
		return;
	if (list = getshfunc("precmd")) doshfuncnoval(list,NULL,0);
	if (errflag)
		return;
	if (period && (time(NULL) > lastperiod+period) &&
			(list = getshfunc("periodic"))) {
		doshfuncnoval(list,NULL,0);
		lastperiod = time(NULL);
	}
	if (errflag)
		return;
	if (watch)
		{
		diff = (int) ddifftime(lastwatch,time(NULL));
		if (diff > logcheck)
			{
			dowatch();
			lastwatch = time(NULL);
			}
		}
	if (errflag)
		return;
	diff = (int) ddifftime(lastmailcheck,time(NULL));
	if (diff > mailcheck)
		{
		if (mailpath && *mailpath)
			checkmailpath(mailpath);
		else if (mailfile)
			{
			char *x[2];

			x[0] = mailfile;
			x[1] = NULL;
			checkmailpath(x);
			}
		lastmailcheck = time(NULL);
		}
	for (schl = (struct schedcmd *) &schedcmds, sch = schedcmds; sch;
			sch = (schl = sch)->next)
		{
		if (sch->time < time(NULL))
			{
			execstring(sch->cmd);
			schl->next = sch->next;
			free(sch->cmd);
			free(sch);
			}
		if (errflag)
			return;
		}
}

int arrlen(s) /**/
char **s;
{
int t0;

	for (t0 = 0; *s; s++,t0++);
	return t0;
}

void checkmailpath(s) /**/
char **s;
{
struct stat st;
char *v,*u,c;

	while (*s)
		{
		for (v = *s; *v && *v != '?'; v++);
		c = *v;
		*v = '\0';
		if (c != '?')
			u = NULL;
		else
			u = v+1;
		if (stat(*s,&st) == -1)
			{
			if (errno != ENOENT)
				zerr("%e: %s",*s,errno);
			}
		else if (S_ISDIR(st.st_mode))
			{
			Lklist l;
			DIR *lock = opendir(*s);
			char buf[MAXPATHLEN*2],**arr,**ap;
			struct direct *de;
			int ct = 1;

			if (lock)
				{
				pushheap();
				heapalloc();
				l = newlist();
				readdir(lock); readdir(lock);
				while (de = readdir(lock))
					{
					if (errflag)
						break;
					if (u)
						sprintf(buf,"%s/%s?%s",*s,de->d_name,u);
					else
						sprintf(buf,"%s/%s",*s,de->d_name);
					addnode(l,strdup(buf));
					ct++;
					}
				closedir(lock);
				ap = arr = (char **) alloc(ct*sizeof(char *));
				while (*ap++ = ugetnode(l));
				checkmailpath(arr);
				popheap();
				}
			}
		else
			{
			if (st.st_size && st.st_atime <= st.st_mtime &&
					st.st_mtime > lastmailcheck)
				if (!u)
					{
					fprintf(stderr,"You have new mail.\n",*s);
					fflush(stderr);
					}
				else
					{
					char *z = u;

					while (*z)
						if (*z == '$' && z[1] == '_')
							{
							fprintf(stderr,"%s",*s);
							z += 2;
							}
						else
							fputc(*z++,stderr);
					fputc('\n',stderr);
					fflush(stderr);
					}
			if (isset(MAILWARNING) && st.st_atime > st.st_mtime &&
					st.st_atime > lastmailcheck && st.st_size)
				{
				fprintf(stderr,"The mail in %s has been read.\n",*s);
				fflush(stderr);
				}
			}
		*v = c;
		s++;
		}
}

void saveoldfuncs(x,y) /**/
char *x;Cmdnam y;
{
Cmdnam cc;

	if (y->type == SHFUNC || y->type == DISABLED)
		{
		cc = (Cmdnam) zcalloc(sizeof *cc);
		*cc = *y;
		y->u.list = NULL;
		addhnode(ztrdup(x),cc,cmdnamtab,freecmdnam);
		}
}

/* create command hashtable */

void newcmdnamtab() /**/
{
Hashtab oldcnt;

	oldcnt = cmdnamtab;
	permalloc();
	cmdnamtab = newhtable(101);
	addbuiltins();
	if (oldcnt) {
		listhtable(oldcnt,(HFunc) saveoldfuncs);
		freehtab(oldcnt,freecmdnam);
	}
	lastalloc();
	pathchecked = path;
}

void freecmdnam(a) /**/
vptr a;
{
struct cmdnam *c = (struct cmdnam *) a;

	if (c->type == SHFUNC) {
		if (c->u.list)
			freestruct(c->u.list);
	} else if (c->type != BUILTIN && c->type != DISABLED)
		free(c->u.nam);
	free(c);
}

void freecompctl(a) /**/
vptr a;
{
Compctl cc = (Compctl) a;

	free(cc);
}

void freestr(a) /**/
vptr a;
{
	free(a);
}

void freeanode(a) /**/
vptr a;
{
struct alias *c = (struct alias *) a;

	free(c->text);
	free(c);
}

void freepm(a) /**/
vptr a;
{
struct param *pm = (Param) a;

	free(pm);
}

void restoretty() /**/
{
	settyinfo(&shttyinfo);
}

void gettyinfo(ti) /**/
struct ttyinfo *ti;
{
	if (SHTTY != -1)
		{
#ifdef TERMIOS
#ifdef HAS_TCCRAP
		if (tcgetattr(SHTTY,&ti->tio) == -1)
#else
		if (ioctl(SHTTY,TCGETS,&ti->tio) == -1)
#endif
			zerr("bad tcgets: %e",NULL,errno);
#else
#ifdef TERMIO
		ioctl(SHTTY,TCGETA,&ti->tio);
#else
		ioctl(SHTTY,TIOCGETP,&ti->sgttyb);
		ioctl(SHTTY,TIOCLGET,&ti->lmodes);
		ioctl(SHTTY,TIOCGETC,&ti->tchars);
		ioctl(SHTTY,TIOCGLTC,&ti->ltchars);
#endif
#endif
#ifdef TIOCGWINSZ
		if (ioctl(SHTTY,TIOCGWINSZ,&ti->winsize) == -1)
		/*	zerr("bad tiocgwinsz: %e",NULL,errno)*/;
#endif
		}
}

void settyinfo(ti) /**/
struct ttyinfo *ti;
{
	if (SHTTY != -1)
		{
#ifdef TERMIOS
#ifdef HAS_TCCRAP
#ifndef TCSADRAIN
#define TCSADRAIN 1   /* XXX Princeton's include files are screwed up */
#endif
		if (tcsetattr(SHTTY, TCSADRAIN, &ti->tio) == -1)
#else
		if (ioctl(SHTTY,TCSETS,&ti->tio) == -1)
#endif
		/*	zerr("settyinfo: %e",NULL,errno)*/;
#else
#ifdef TERMIO
		ioctl(SHTTY,TCSETA,&ti->tio);
#else
		ioctl(SHTTY,TIOCSETN,&ti->sgttyb);
		ioctl(SHTTY,TIOCLSET,&ti->lmodes);
		ioctl(SHTTY,TIOCSETC,&ti->tchars);
		ioctl(SHTTY,TIOCSLTC,&ti->ltchars);
#endif
#endif
#ifdef TIOCGWINSZ
		signal(SIGWINCH,SIG_IGN);
		if (ioctl(SHTTY,TIOCSWINSZ,&ti->winsize) == -1)
		/*	zerr("settyinfo: %e",NULL,errno)*/;
		signal(SIGWINCH,handler);
#endif
		}
}

#define SANEKEY(X) \
	if (ti->X == -1 && savedttyinfo.X != -1) ti->X = savedttyinfo.X;

void sanetty(ti) /**/
struct ttyinfo *ti;
{
int t0;

#ifdef TIO
	ti->tio.c_lflag |= ICANON|ECHO;
#ifdef FLUSHO
	ti->tio.c_lflag &= ~FLUSHO;
#endif
	for (t0 = 0; t0 !=
#ifdef NCCS
	NCCS
#else
	NCC
#endif
	; t0++)
		if (ti->tio.c_cc[t0] == VDISABLEVAL &&
				savedttyinfo.tio.c_cc[t0] != VDISABLEVAL)
			ti->tio.c_cc[t0] = savedttyinfo.tio.c_cc[t0];
#else
	ti->sgttyb.sg_flags = (ti->sgttyb.sg_flags & ~CBREAK) | ECHO;
	ti->lmodes &= ~LFLUSHO;
	SANEKEY(tchars.t_quitc);
	SANEKEY(tchars.t_startc);
	SANEKEY(tchars.t_stopc);
	SANEKEY(ltchars.t_suspc);
	SANEKEY(ltchars.t_dsuspc);
	SANEKEY(ltchars.t_lnextc);
	SANEKEY(ltchars.t_flushc);
#endif
}

void adjustwinsize() /**/
{
#ifdef TIOCGWINSZ
	ioctl(SHTTY,TIOCGWINSZ,&shttyinfo.winsize);
	if (!(columns = shttyinfo.winsize.ws_col)) columns = 80;
	lines = shttyinfo.winsize.ws_row;
	setintenv("COLUMNS",columns);
	setintenv("LINES",lines);
	if (zleactive) refresh();
#endif
}

int zyztem(s,t) /**/
char *s;char *t;
{
#ifdef WAITPID
int pid,statusp;

	if (!(pid = fork()))
		{
		s = tricat(s," ",t);
		execl("/bin/sh","sh","-c",s,(char *) 0);
		_exit(1);
		}
	while (waitpid(pid,&statusp,WUNTRACED) == -1 && errno == EINTR);
	if (WIFEXITED(statusp))
		return WEXITSTATUS(statusp);
	return 1;
#else
	if (!waitfork())
		{
		s = tricat(s," ",t);
		execl("/bin/sh","sh","-c",s,(char *) 0);
		_exit(1);
		}
	return 0;
#endif
}

#ifndef WAITPID

/* fork a process and wait for it to complete without confusing
	the SIGCHLD handler */

int waitfork() /**/
{
int pipes[2];
char x;

	pipe(pipes);
	if (!fork())
		{
		close(pipes[0]);
		signal(SIGCHLD,SIG_DFL);
		if (!fork())
			return 0;
		wait(NULL);
		_exit(0);
		}
	close(pipes[1]);
	read(pipes[0],&x,1);
	close(pipes[0]);
	return 1;
}

#endif

/* move a fd to a place >= 10 */

int movefd(fd) /**/
int fd;
{
int fe;

	if (fd == -1)
		return fd;
#ifdef F_DUPFD
	fe = fcntl(fd,F_DUPFD,10);
#else
	if ((fe = dup(fd)) < 10)
		fe = movefd(fe);
#endif
	close(fd);
	return fe;
}

/* move fd x to y */

void redup(x,y) /**/
int x;int y;
{
	if (x != y)
		{
		dup2(x,y);
		close(x);
		}
}

void settrap(t0,l) /**/
int t0;List l;
{
Cmd c;

	if (l)
		{
		c = l->left->left->left;
		if (c->type == SIMPLE && !full(c->args) && !full(c->redir)
				&& !full(c->vars) && !c->flags)
			l = NULL;
		}
	if (t0 == -1)
		return;
	if (jobbing && (t0 == SIGTTOU || t0 == SIGTSTP || t0 == SIGTTIN
			|| t0 == SIGPIPE))
		{
		zerr("can't trap SIG%s in interactive shells",sigs[t0-1],0);
		return;
		}
	if (!l)
		{
		sigtrapped[t0] = 2;
		if (t0 && t0 < SIGCOUNT && t0 != SIGCHLD)
			{
			signal(t0,SIG_IGN);
			sigtrapped[t0] = 2;
			}
		}
	else
		{
		if (t0 && t0 < SIGCOUNT && t0 != SIGCHLD)
			signal(t0,handler);
		sigtrapped[t0] = 1;
		permalloc();
		sigfuncs[t0] = (List) dupstruct(l);
		heapalloc();
		}
}

void unsettrap(t0) /**/
int t0;
{
	if (t0 == -1)
		return;
	if (jobbing && (t0 == SIGTTOU || t0 == SIGTSTP || t0 == SIGTTIN
			|| t0 == SIGPIPE)) {
		return;
	}
	sigtrapped[t0] = 0;
	if (t0 == SIGINT) intr();
	else if (t0 && t0 < SIGCOUNT && t0 != SIGCHLD) signal(t0,SIG_DFL);
	if (sigfuncs[t0]) freestruct(sigfuncs[t0]);
}

void dotrap(sig) /**/
int sig;
{
int sav,savval;

	sav = sigtrapped[sig];
	savval = lastval;
	if (sav == 2)
		return;
	sigtrapped[sig] = 2;
	if (sigfuncs[sig]) {
		lexsave();
		doshfuncnoval(sigfuncs[sig],NULL,0);
		lexrestore();
	}
	sigtrapped[sig] = sav;
	lastval = savval;
}

/* copy len chars from t into s, and null terminate */

void ztrncpy(s,t,len) /**/
char *s;char *t;int len;
{
	while (len--) *s++ = *t++;
	*s = '\0';
}

/* copy t into *s and update s */

void strucpy(s,t) /**/
char **s;char *t;
{
char *u = *s;

	while (*u++ = *t++);
	*s = u-1;
}

void struncpy(s,t,n) /**/
char **s;char *t;int n;
{
char *u = *s;

	while (n--)
		*u++ = *t++;
	*s = u;
	*u = '\0';
}

void checkrmall(s) /**/
char *s;
{
	fflush(stdin);
	if (*s == '/')
		fprintf(stderr,"zsh: sure you want to delete all the files in %s? ",s);
	else
		fprintf(stderr,"zsh: sure you want to delete all the files in %s/%s? ",
			(pwd[1]) ? pwd : "",s);
	fflush(stderr);
	feep();
	errflag |= (getquery() != 'y');
}

int getquery() /**/
{
char c;
long val;

	setcbreak();
#ifdef FIONREAD
	ioctl(SHTTY,FIONREAD,&val);
	if (val) { unsetcbreak(); write(2,"n\n",2); return 'n'; }
#endif
	if (read(SHTTY,&c,1) == 1)
		if (c == 'y' || c == 'Y' || c == '\t') c = 'y';
	unsetcbreak();
	if (c != '\n')
		write(2,"\n",1);
	return (int) c;
}

static int d;
static char *guess,*best;

void spscannodis(s,cn) /**/
char *s;char *cn;
{
	if (((Cmdnam) cn)->type != DISABLED)
		spscan(s,NULL);
}

void spscan(s,junk) /**/
char *s;char *junk;
{
int nd;

	nd = spdist(s,guess,strlen(guess)/4+1);
	if (nd <= d) {
		best = s;
		d = nd;
	}
}

/* spellcheck a word */
/* fix s and s2 ; if s2 is non-null, fix the history list too */

void spckword(s,s2,tptr,cmd,ask) /**/
char **s;char **s2;char **tptr;int cmd;int ask;
{
char *t,*u;
char firstchar;
int x;
int pram = 0;

	if (**s == '-' || **s == '%')
		return;
	if (!strcmp(*s,"in"))
		return;
	if (!(*s)[0] || !(*s)[1]) return;
	if (gethnode(*s,cmdnamtab) || gethnode(*s,aliastab)) return;
	t = *s;
	if (*t == Tilde || *t == Equals || *t == String) t++;
	for (; *t; t++) if (itok(*t)) return;
	best = NULL;
	for (t = *s; *t; t++) if (*t == '/') break;
	if (**s == String) {
		if (*t) return;
		pram = 1;
		guess = *s+1;
		d = 100;
		listhtable(paramtab,spscan);
	} else {
		if ((u = spname(guess = *s)) != *s)
			best = u;
		if (!*t && !cmd) {
			if (access(*s,F_OK) == 0) return;
			if (hashcmd(*s,pathchecked)) return;
			guess = *s;
			d = 100;
			listhtable(aliastab,spscan);
			listhtable(cmdnamtab,spscan);
		}
	}
	if (errflag) return;
	if (best && strlen(best) > 1 && strcmp(best,guess)) {
		if (ask) {
			char *pp;
			int junk;

			rstring = best; Rstring = guess;
			firstchar = *guess;
			if (*guess == Tilde) *guess = '~';
			else if (*guess == String) *guess = '$';
			else if (*guess == Equals) *guess = '=';
			pp = putprompt(sprompt,&junk);
			*guess = firstchar;
			fprintf(stderr,"%s",pp);
			fflush(stderr);
			feep();
			x = getquery();
		} else
			x = 'y';
		if (x == 'y') {
			if (!pram) {
				*s = strdup(best);
			} else {
				*s = alloc(strlen(best)+2);
				strcpy(*s+1,best);
				**s = String;
			}
			if (s2) {
				if (*tptr && !strcmp(hlastw,*s2) && hlastw < hptr) {
					char *z;
					hptr = hlastw;
					if (pram) hwaddc('$');
					for (z = best; *z; z++) hwaddc(*z);
					hwaddc(HISTSPACE);
					*tptr = hptr-1;
					**tptr = '\0';
				}
				*s2 = strdup(best);
			}
		} else if (x == 'a') {
			histdone |= HISTFLAG_NOEXEC;
		} else if (x == 'e') {
			histdone |= HISTFLAG_NOEXEC|HISTFLAG_RECALL;
		}
	}
}

int ztrftime(buf,bufsize,fmt,tm) /**/
char *buf;int bufsize;char *fmt;struct tm *tm;
{
static char *astr[] = {"Sun","Mon","Tue","Wed","Thu","Fri","Sat"};
static char *estr[] = {"Jan","Feb","Mar","Apr","May","Jun","Jul",
	"Aug","Sep","Oct","Nov","Dec"};
static char *lstr[] = {"12"," 1"," 2"," 3"," 4"," 5"," 6"," 7"," 8"," 9",
	"10","11"};
char tmp[3];
#ifdef HAS_STRFTIME
char *origbuf = buf;
#endif

	tmp[0] = '%'; tmp[2] = '\0';
	while (*fmt)
		if (*fmt == '%')
			{
			fmt++;
			switch(*fmt++)
				{
				case 'a':
					strucpy(&buf,astr[tm->tm_wday]);
					break;
				case 'b':
					strucpy(&buf,estr[tm->tm_mon]);
					break;
				case 'd':
					*buf++ = '0'+tm->tm_mday/10;
					*buf++ = '0'+tm->tm_mday%10;
					break;
				case 'e':
					if (tm->tm_mday > 9)
						*buf++ = '0'+tm->tm_mday/10;
					*buf++ = '0'+tm->tm_mday%10;
					break;
				case 'k':
					if (tm->tm_hour > 9)
						*buf++ = '0'+tm->tm_hour/10;
					*buf++ = '0'+tm->tm_hour%10;
					break;
				case 'l':
					strucpy(&buf,lstr[tm->tm_hour%12]);
					break;
				case 'm':
					*buf++ = '0'+(tm->tm_mon+1)/10;
					*buf++ = '0'+(tm->tm_mon+1)%10;
					break;
				case 'M':
					*buf++ = '0'+tm->tm_min/10;
					*buf++ = '0'+tm->tm_min%10;
					break;
				case 'p':
					*buf++ = (tm->tm_hour > 11) ? 'p' : 'a';
					*buf++ = 'm';
					break;
				case 'S':
					*buf++ = '0'+tm->tm_sec/10;
					*buf++ = '0'+tm->tm_sec%10;
					break;
				case 'y':
					*buf++ = '0'+tm->tm_year/10;
					*buf++ = '0'+tm->tm_year%10;
					break;
				default:
#ifdef HAS_STRFTIME
					*buf = '\0';
					tmp[1] = fmt[-1];
					strftime(buf,bufsize-strlen(origbuf),tmp,tm);
					buf += strlen(buf);
#else
					*buf++ = '%';
					*buf++ = fmt[-1];
#endif
					break;
				}
			}
		else
			*buf++ = *fmt++;
	*buf = '\0';
	return 0;
}

char *join(arr,delim) /**/
char **arr;int delim;
{
int len = 0;
char **s,*ret,*ptr;
static char *lastmem = NULL;

	for (s = arr; *s; s++)
		len += strlen(*s)+1;
	if (!len) return "";
	if (lastmem) free(lastmem);
	lastmem = ptr = ret = zalloc(len);
	for (s = arr; *s; s++) {
		strucpy(&ptr,*s);
		*ptr++ = delim;
	}
	ptr[-1] = '\0';
	return ret;
}

char *spacejoin(s) /**/
char **s;
{
	return join(s,*ifs);
}

char *colonjoin(s) /**/
char **s;
{
	return join(s,':');
}

char **colonsplit(s) /**/
char *s;
{
int ct;
char *t,**ret,**ptr;
char **lastmem = NULL;

	for (t = s, ct = 0; *t; t++) if (*t == ':') ct++;
	if (lastmem) freearray(lastmem);
	lastmem = ptr = ret = (char **) zalloc(sizeof(char **)*(ct+2));
	t = s;
	do {
		for (s = t; *t && *t != ':'; t++);
		*ptr = zalloc((t-s)+1);
		ztrncpy(*ptr++,s,t-s);
	}
	while (*t++);
	*ptr = NULL;
	return ret;
}

char **spacesplit(s) /**/
char *s;
{
int ct;
char *t,**ret,**ptr;

	for (t = s, ct = 0; *t; t++)
		if (isep(*t)) ct++;
	ptr = ret = (char **) zalloc(sizeof(char **)*(ct+2));
	t = s;
	do {
		for (s = t; *t && !isep(*t); t++);
		*ptr = zalloc((t-s)+1);
		ztrncpy(*ptr++,s,t-s);
	} while (*t++);
	*ptr = NULL;
	return ret;
}

List getshfunc(nam) /**/
char *nam;
{
Cmdnam x = (Cmdnam) gethnode(nam,cmdnamtab);

	return (x && x->type == SHFUNC) ? x->u.list : NULL;
}

/* allocate a tree element */

vptr allocnode(type) /**/
int type;
{
int t0;
struct node *n = (struct node *) alloc(sizeof *n);
static int typetab[N_COUNT][4] = {
	NT_NODE,NT_NODE,0,0,
	NT_NODE,NT_NODE,0,0,
	NT_NODE,NT_NODE,0,0,
	NT_STR|NT_LIST,NT_NODE,NT_NODE|NT_LIST,NT_NODE|NT_LIST,
	NT_STR,0,0,0,
	NT_NODE,NT_NODE,0,0,
	NT_STR,NT_NODE,0,0,
	NT_NODE,NT_STR,NT_NODE,0,
	NT_NODE,NT_NODE,NT_NODE,0,
	NT_NODE,NT_NODE,0,0,
	NT_STR,NT_STR,NT_STR|NT_LIST,0
	};

	n->type = type;
	for (t0 = 0; t0 != 4; t0++)
		n->types[t0] = typetab[type][t0];
	return (vptr) n;
}

/* duplicate a syntax tree */

vptr dupstruct(a) /**/
vptr a;
{
struct node *n = a,*m;
int t0;

	if (!a) return NULL;
	m = alloc(sizeof *m);
	*m = *n;
	for (t0 = 0; t0 != 4; t0++)
		if (m->ptrs[t0])
			switch(m->types[t0])
				{
				case NT_NODE: m->ptrs[t0] = dupstruct(m->ptrs[t0]); break;
				case NT_STR: m->ptrs[t0] =
					(useheap) ? strdup(m->ptrs[t0]) : ztrdup(m->ptrs[t0]); break;
				case NT_LIST|NT_NODE:
					m->ptrs[t0] = duplist(m->ptrs[t0],dupstruct); break;
				case NT_LIST|NT_STR:
					m->ptrs[t0] = duplist(m->ptrs[t0],(VFunc)
						((useheap) ? strdup : ztrdup));
					break;
				}
	return (vptr) m;
}

/* free a syntax tree */

void freestruct(a) /**/
vptr a;
{
struct node *n = (struct node *) a;
int t0;

	for (t0 = 0; t0 != 4; t0++)
		if (n->ptrs[t0])
			switch(n->types[t0])
				{
				case NT_NODE: freestruct(n->ptrs[t0]); break;
				case NT_STR: free(n->ptrs[t0]); break;
				case NT_LIST|NT_STR: freetable(n->ptrs[t0],freestr); break;
				case NT_LIST|NT_NODE: freetable(n->ptrs[t0],freestruct); break;
				}
	free(n);
}

Lklist duplist(l,func) /**/
Lklist l;VFunc func;
{
Lklist ret;
Lknode node;

	ret = newlist();
	for (node = firstnode(l); node; incnode(node))
		addnode(ret,func(getdata(node)));
	return ret;
}

char **mkarray(s) /**/
char *s;
{
char **t = (char **) zalloc((s) ? (2*sizeof s) : (sizeof s));

	if (*t = s) t[1] = NULL;
	return t;
}

void feep() /**/
{
	if (unset(NOBEEP))
		write(2,"\07",1);
}

void freearray(s) /**/
char **s;
{
char **t = s;

	while (*s)
		free(*s++);
	free(t);
}

int equalsplit(s,t) /**/
char *s;char **t;
{
	for (; *s && *s != '='; s++);
	if (*s == '=')
		{
		*s++ = '\0';
		*t = s;
		return 1;
		}
	return 0;
}

/* see if the right side of a list is trivial */

void simplifyright(l) /**/
List l;
{
Cmd c;

	if (!l->right)
		return;
	if (l->right->right || l->right->left->right ||
			l->right->left->left->right)
		return;
	c = l->left->left->left;
	if (c->type != SIMPLE || full(c->args) || full(c->redir)
			|| full(c->vars))
		return;
	l->right = NULL;
	return;
}

/* initialize the ztypes table */

void inittyptab() /**/
{
int t0;
char *s;

	for (t0 = 0; t0 != 256; t0++)
		typtab[t0] = 0;
	for (t0 = 0; t0 != 32; t0++)
		typtab[t0] = typtab[t0+128] = ICNTRL;
	typtab[127] = ICNTRL;
	for (t0 = '0'; t0 <= '9'; t0++)
		typtab[t0] = IDIGIT|IALNUM|IWORD|IIDENT|IUSER;
	for (t0 = 'a'; t0 <= 'z'; t0++)
		typtab[t0] = typtab[t0-'a'+'A'] = IALPHA|IALNUM|IIDENT|IUSER|IWORD;
	for (t0 = 0240; t0 != 0400; t0++)
		typtab[t0] = IALPHA|IALNUM|IIDENT|IUSER|IWORD;
	typtab['_'] = IIDENT|IUSER;
	typtab['-'] = IUSER;
	typtab[' '] |= IBLANK|INBLANK;
	typtab['\t'] |= IBLANK|INBLANK;
	typtab['\n'] |= INBLANK;
	for (t0 = (int) (unsigned char) ALPOP; t0 <= (int) (unsigned char) Nularg;
			t0++)
		typtab[t0] |= ITOK;
	for (s = ifs; *s; s++)
		typtab[(int) (unsigned char) *s] |= ISEP;
	for (s = wordchars; *s; s++)
		typtab[(int) (unsigned char) *s] |= IWORD;
	for (s = SPECCHARS; *s; s++)
		typtab[(int) (unsigned char) *s] |= ISPECIAL;
}

char **arrdup(s) /**/
char **s;
{
char **x,**y;

	y = x = (char **) ncalloc(sizeof(char *)*(arrlen(s)+1));
	while (*x++ = strdup(*s++));
	return y;
}

/* next few functions stolen (with changes) from Kernighan & Pike */
/* "The UNIX Programming Environment" (w/o permission) */

char *spname (oldname) /**/
char *oldname;
{
	char *p,guess[MAXPATHLEN+1],best[MAXPATHLEN+1];
	static char newname[MAXPATHLEN+1];
	char *new = newname, *old;

	if (itok(*oldname)) {
		singsub(&oldname);
		if (!oldname) return NULL;
	}
	if (access(oldname,F_OK) == 0) return NULL;
	old = oldname;
	for (;;) {
		while (*old == '/') *new++ = *old++;
		*new = '\0';
		if (*old == '\0') return newname;
		p = guess;
		for (; *old != '/' && *old != '\0'; old++)
			if (p < guess+MAXPATHLEN) *p++ = *old;
		*p = '\0';
		if (mindist(newname,guess,best) >= 3) return NULL;
		for (p = best; *new = *p++; ) new++;
	}
}

int mindist(dir,guess,best) /**/
char *dir;char *guess;char *best;
{
	int d,nd;
	DIR *dd;
	struct direct *de;
	char buf[MAXPATHLEN];

	if (dir[0] == '\0')
		dir = ".";
	d = 100;
	sprintf(buf,"%s/%s",dir,guess);
	if (access(buf,F_OK) == 0) { strcpy(best,guess); return 0; }
	if (!(dd = opendir(dir))) return d;
	while (de = readdir(dd)) {
		nd = spdist(de->d_name,guess,strlen(guess)/4+1);
		if (nd <= d) {
			strcpy(best,de->d_name);
			d = nd;
			if (d == 0) break;
		}
	}
	closedir(dd);
	return d;
}

int spdist(s,t,thresh) /**/
char *s;char *t;int thresh;
{
char *p,*q;
char *keymap =
"\n\n\n\n\n\n\n\n\n\n\n\n\n\n\
\t1234567890-=\t\
\tqwertyuiop[]\t\
\tasdfghjkl;'\n\t\
\tzxcvbnm,./\t\t\t\
\n\n\n\n\n\n\n\n\n\n\n\n\n\n\
\t!@#$%^&*()_+\t\
\tQWERTYUIOP{}\t\
\tASDFGHJKL:\"\n\t\
\tZXCVBNM<>?\n\n\t\
\n\n\n\n\n\n\n\n\n\n\n\n\n\n";

	if (!strcmp(s,t))
		return 0;
	/* any number of upper/lower mistakes allowed (dist = 1) */
	for (p = s, q = t; *p && tulower(*p) == tulower(*q); p++,q++);
	if (!*p && !*q)
		return 1;
	if (!thresh)
		return 200;
	for (p = s, q = t; *p && *q; p++,q++)
		if (*p == *q) continue;	/* don't consider "aa" transposed, ash */
		else if (p[1] == q[0] && q[1] == p[0])  /* transpositions */
			return spdist(p+2,q+2,thresh-1)+1;
		else if (p[1] == q[0])	/* missing letter */
			return spdist(p+1,q+0,thresh-1)+2;
		else if (p[0] == q[1])	/* missing letter */
			return spdist(p+0,q+1,thresh-1)+2;
		else if (*p != *q)
			break;
	if ((!*p && strlen(q) == 1) || (!*q && strlen(p) == 1))
		return 2;
	for (p = s, q = t; *p && *q; p++,q++)
		if (p[0] != q[0] && p[1] == q[1])
			{
			int t0;
			char *z;

			/* mistyped letter */

			if (!(z = strchr(keymap,p[0])) || *z == '\n' || *z == '\t')
				return spdist(p+1,q+1,thresh-1)+1;
			t0 = z-keymap;
			if (*q == keymap[t0-15] || *q == keymap[t0-14] ||
					*q == keymap[t0-13] ||
					*q == keymap[t0-1] || *q == keymap[t0+1] ||
					*q == keymap[t0+13] || *q == keymap[t0+14] ||
					*q == keymap[t0+15])
				return spdist(p+1,q+1,thresh-1)+2;
			return 200;
			}
		else if (*p != *q)
			break;
	return 200;
}

char *zgetenv(s) /**/
char *s;
{
char **av,*p,*q;

	for (av = environ; *av; av++)
		{
		for (p = *av, q = s; *p && *p != '=' && *q && *p == *q; p++,q++);
		if (*p == '=' && !*q)
			return p+1;
		}
	return NULL;
}

int tulower(c) /**/
int c;
{
	c &= 0xff;
	return (isupper(c) ? tolower(c) : c);
}

int tuupper(c) /**/
int c;
{
	c &= 0xff;
	return (islower(c) ? toupper(c) : c);
}

#ifdef SYSV
#include <sys/utsname.h>

int gethostname(nameptr, maxlength)
char *nameptr;
int maxlength;
{
struct utsname *name;
int result;

	result = uname(name);
	if (result >= 0) {
		strcpy(nameptr,name->sysname);
		return 0;
	} else return -1;
}
#endif

/* set cbreak mode, or the equivalent */

void setcbreak() /**/
{
struct ttyinfo ti;

	ti = shttyinfo;
#ifdef TIO
	ti.tio.c_lflag &= ~ICANON;
	ti.tio.c_cc[VMIN] = 1;
	ti.tio.c_cc[VTIME] = 0;
#else
	ti.sgttyb.sg_flags |= CBREAK;
#endif
	settyinfo(&ti);
}

int getlineleng() /**/
{
int z;

#ifdef TIOCSWINSZ
	z = shttyinfo.winsize.ws_col;
	return (z) ? z : 80;
#else
	return 80;
#endif
}

void unsetcbreak() /**/
{
	settyinfo(&shttyinfo);
}

/* give the tty to some process */

void attachtty(pgrp) /**/
long pgrp;
{
static int ep = 0;

	if (jobbing) {
#ifdef HAS_TCSETPGRP
		if (SHTTY != -1 && tcsetpgrp(SHTTY,pgrp) == -1 && !ep)
#else
		int arg = pgrp;
		if (SHTTY != -1 && ioctl(SHTTY,TIOCSPGRP,&arg) == -1 && !ep)
#endif
			{
			zerr("can't set tty pgrp: %e",NULL,errno);
			fflush(stderr);
			opts[MONITOR] = OPT_UNSET;
			ep =1;
			errflag = 0;
			}
	}
}

/* get the tty pgrp */

long gettygrp() /**/
{
int arg = -1;

	if (SHTTY == -1) return -1;
#ifdef HAS_TCSETPGRP
	arg = tcgetpgrp(SHTTY);
#else
	ioctl(SHTTY,TIOCGPGRP,&arg);
#endif
	return arg;
}
