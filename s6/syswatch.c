#
/*
 * syswatch - login-logout monitoring
 *
 * Author: Howard Katseff UCB May/August 1977
 *
 * Syswatch monitors all logins and logouts on the system.
 * It is a companion routine to watch which monitors specific
 * users.  The difference between these two routines is that
 * the linked list stuff in watch has been abolished here in favor
 * of a simpler scheme.
 *
 * ROOTDEV and NULLDEV are for determining when a user's tty has
 * been chfile'd to /dev/null, in which case, the user has logged
 * out and is probably no longer interested in the results of
 * this program.
 *
 * This check will not work on non-UCB type UNIX'es.
 * The check should be changed, or the syswatch killed explicitly.
 */

#define DLATENCY 15
#define ROOTSYS ((5<<8) | 8)
#define NULLDEV ((8<<8) | 2)

int lastuser,valid;
int zero[2];
char *who_file "/etc/utmp";
char *nil 0177777;


struct t_entry {
	char name[10];
	char tty;
	int time[2];
	int valid;
} people[128], newpeople[128];

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
	int *p,pid,LATENCY;
	LATENCY = DLATENCY;
	p = -2;
	if (pid=fork())
	{
		printf("%d\n",pid);
		exit(0);
	}
	signal(2,1);
	signal(3,1);
	build();
	for (i=0;i<128;++i) assign(people[i].name, newpeople[i].name);
	for (;;)
	{
		execute();
		sleep(LATENCY);
	}
}

execute()
{
	register i;
	int buf[18];
	if (fstat(1, buf)) exit(0);
	if (buf[0] == ROOTSYS && buf[6] == NULLDEV) exit(0);
	valid++;
	build();
	for (i=0;i<128;++i)
	{
		if (!dcmp(newpeople[i].time,people[i].time))
		{
			report(i);
			assign(people[i].name, newpeople[i].name);
		}
	}
}

build()
{
	register fd,i;
	struct utmp line;
	if ((fd=open(who_file,0)) < 0)
	{
		perror(who_file);
		exit(1);
	}
	for (i=0;i<128;++i) newpeople[i].time[0] = newpeople[i].time[1] = 0;
	while (getuser(fd,&line))
	{
		i = line.Xtty;
		newpeople[i].valid = valid;
		copy(line.Xname, newpeople[i].name);
		newpeople[i].tty = i;
		newpeople[i].time[0] = line.ltime[0];
		newpeople[i].time[1] = line.ltime[1];
	}
	close(fd);
}

assign(l,r)
	struct t_entry *l,*r;
{
	copy(r->name, l->name);
	l->tty = r->tty;
	l->time[0] = r->time[0];
	l->time[1] = r->time[1];
	l->valid++;
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
	if (w->Xname[0] == 0)
	{
		w->ltime[0] = w->ltime[1] = 0;
		return(16);
	}
	w->ltime[0] = ctoi(buf[11],buf[10]);
	w->ltime[1] = ctoi(buf[13],buf[12]);
	return(16);
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

dcmp(a,b)
	int a[],b[];
{
	return(a[0]==b[0] && a[1]==b[1]);
}

ctoi(a,b)
	char a,b;
{
	return(((a & 0377) << 8) | (b & 0377));
}

report(t)
	char t;
{
	if (dcmp(people[t].time, zero))
	{
		printf("%s on tty%c %s",newpeople[t].name,t,ctime(newpeople[t].ltime));
		return;
	}
	if (dcmp(newpeople[t].time,zero))
	{
		printf("%s on tty%c logged out.\n",people[t].name,t);
		return;
	}
	printf("%s on tty%c logged out.\n",people[t].name,t);
	printf("%s on tty%c %s",newpeople[t].name,t,ctime(newpeople[t].ltime));
}
