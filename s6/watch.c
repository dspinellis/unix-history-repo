#
/*
 * watch - watch for logins/logouts of specified users
 *
 * Author: Howard Katseff UCB July, 1977
 *
 * Watch watches the specified users and reports their logins
 * and logouts.  If no people are specified, watch execl's its
 * companion routine, syswatch, which watches over the whole system.
 */

#define NUSERS 20
#define LATENCY 15
#define ROOTSYS ((5<<8) | 8)
#define NULLDEV ((8<<8) | 2)

int lastuser,valid;
char *who_file "/etc/utmp";
char *syswatch "/usr/pascal/syswatch";
char *nil 0177777;

struct {
	char name[10];
	struct t_entry *tlink;
} users[NUSERS];

struct	t_entry	*checkout(), *talloc();
struct t_entry {
	char tty;
	int time[2];
	struct t_entry *flink;
	struct t_entry *blink;
	int valid;
};

struct	utmp {
	char	Xname[10];
	char	Xtty;
	char	pad1;
	int	ltime[2];
	int	pad2;
};

main(ct,av)
	char **av;
{
	register i;
	register char *cp,*cp2;
	int *p,pid,count;
	signal(2,1);
	signal(3,1);
	if (ct < 2)
	{
		execl(syswatch, "- ", 0);
		perror(syswatch);
		exit(1);
	}
	p = -2;
	if (pid=fork())
	{
		printf("%d\n",pid);
		exit(0);
	}
	i = 0;
	while (--ct)
	{
		cp = users[i].name;
		cp2 = *++av;
		copy(cp2,cp);
		users[i++].tlink = nil;
	}
	lastuser = --i;
	*p = -1;
	for (;;)
	{
		++count;
		count =& 037;
		if (!count) if (fork()) exit(0);
		execute();
		sleep(LATENCY);
	}
}

execute()
{
	struct utmp line;
	struct t_entry *t;
	register fd,i;
	int buf[18];
	if (fstat(1,buf)) exit(0);
	if (buf[0] == ROOTSYS && buf[6] == NULLDEV) exit(0);
	if (*buf == ctoi(2,8)) exit(0);
	fd = open(who_file,0);
	if (fd < 0)
	{
		perror(who_file);
		sleep(90);
		return;
	}
	++valid;
	while (getuser(fd,&line))
	{
		i = 0;
		while (i <= lastuser)
		{
			if (equal(line.Xname, users[i].name))
			{
				if (t=cdrondown(users[i].tlink,&line))
					users[i].tlink = t;
				break;
			}
			++i;
		}
	}
	for (i=0;i<=lastuser;++i)
		if (t=checkout(users[i].name,users[i].tlink)) users[i].tlink=t;
	close(fd);
}

cdrondown(tlist,wholine)
	struct t_entry *tlist;
	struct utmp *wholine;
{
	struct t_entry *t,*save;
	save = tlist;
	while (tlist != nil)
	{
		if (dcmp(tlist->time,wholine->ltime))
		{
			tlist->valid = valid;
			return(0);
		}
		tlist = tlist->flink;
	}
/* this tty not on the user's tty list, so it's a new entry */
	t = talloc();
	t->time[0] = wholine->ltime[0];
	t->time[1] = wholine->ltime[1];
	t->tty = wholine->Xtty;
	t->valid = valid;
	t->flink = save;
	if (save != nil) save->blink = t;
	t->blink = nil;
	message(wholine->Xname,t->tty,t->time);
	return(t);
}

struct t_entry *checkout(user,tlist)
	struct t_entry *tlist;
	char *user;
{
	struct t_entry *t,*rvalue;
	rvalue = 0;
	while (tlist != nil)
	{
		if (tlist->valid != valid)
		{
			t = tlist;
			printf("%s on tty%c logged out.\n",user,t->tty);
			if (t->blink == nil) rvalue = t->flink;
			else (t->blink)->flink = t->flink;
			if (t->flink != nil) (t->flink)->blink = t->blink;
			tlist = t->flink;
			free(t);
		} else tlist=tlist->flink;
	}
	return(rvalue);
}

struct t_entry *talloc()
{
	struct t_entry sam;
	return(alloc(sizeof sam));
}

getuser(f,w)
	struct utmp *w;
{
	register c;
	char buf[16];
	c = read(f,buf,16);
	if (c != 16) return(0);
	for (c=0;c<8;++c) w->Xname[c] = (buf[c] == 040 ? 0 : buf[c]);
	w->Xname[8] = 0;
	w->Xtty = buf[8];
	w->ltime[0] = ctoi(buf[11],buf[10]);
	w->ltime[1] = ctoi(buf[13],buf[12]);
	return(16);
}

dcmp(a,b)
	int a[],b[];
{
	return(a[0]==b[0] && a[1]==b[1]);
}

message(nm,tty,tim)
	char *nm,tty;
	int tim[];
{
	char *cvtime;
	cvtime = ctime(tim);
	cvtime[16]=0;
	cvtime =+ 4;
	printf("%s tty%c %s\n",nm,tty,cvtime);
}

copy(c1,c2)
	char *c1,*c2;
{
	while (*c2++ = *c1++);
}

equal(left,right)
	char *left,*right;
{
	while (*left && *right)
	{
		if (*left != *right) return(0);
		++left;
		++right;
	}
	return(*left == *right);
}

ctoi(a,b)
	char a,b;
{
	return(((a & 0377) << 8) | (b & 0377));
}
