#ifndef lint
static char sccsid[] = "@(#)xget.c	4.5 5/11/89";
#endif

#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <ctype.h>
#include <pwd.h>
#include "xmail.h"
#include "pathnames.h"

char *myname;
int uid;
struct direct *dbuf;
char *maildir = _PATH_SECRETMAIL;
FILE *kf, *mf;
DIR *df;
MINT *x, *b, *one, *t45, *z, *q, *r;
MINT *two, *t15, *mbuf;
char buf[256], line[128];
#define MXF 100
int fnum[MXF], fcnt;
struct stat stbuf;
main()
{	int i;
	char *p;

	uid = getuid();
	myname = (char *)getlogin();
	if(myname == NULL)
		myname = getpwuid(uid)->pw_name;
	comminit();
	mbuf = itom(0);
	files();
	setup(getpass("Key: "));
	mkb();
	mkx();
#ifndef debug
	invert(x, b, x);
#else
	invert(x, b, z);
	mult(x, z, z);
	mdiv(z, b, q, z);
	omout(z);
	invert(x, b, x);
#endif
	for(i=0; i<fcnt; i++)
	{	sprintf(line, "%s%s.%d", maildir, myname, fnum[i]);
		if(stat(line, &stbuf)<0)
		{	perror(line);
			continue;
		}
		if(stbuf.st_size == 0)
		{	printf("zero length mail file\n");
			unlink(line);
			continue;
		}
		if((mf = fopen(line, "r"))==NULL)
		{	perror(line);
			continue;
		}
		decipher(mf, stdout);
	cmnd:
		printf("? ");
		fgets(buf, sizeof(buf), stdin);
		if(feof(stdin)) exit(0);
		switch(buf[0])
		{
		case 'q':
			exit(0);
		case 'n':
		case 'd':
		case '\n':
			fclose(mf);
			unlink(line);
			break;
		case '!':
			system(buf+1);
			printf("!\n");
			goto cmnd;
		case 's':
		case 'w':
			rewind(mf);
			if(buf[1] == '\n' || buf[1] == '\0')
				strcpy(buf, "s mbox\n");
			for(p = buf+1; isspace(*p); p++);
			p[strlen(p)-1] = 0;
			kf = fopen(p, "a");
			if(kf == NULL)
			{	perror(p);
				goto cmnd;
			}
			decipher(mf, kf);
			fclose(mf);
			fclose(kf);
			unlink(line);
			break;
		default:
			printf("Commands are:\n");
			printf("q	quit, leaving unread messages\n");
			printf("n	delete current message and goto next\n");
			printf("d	same as above\n");
			printf("\\n	same as above\n");
			printf("!	execute shell command\n");
			printf("s	save message in the named file or mbox\n");
			printf("w	same as above\n");
			printf("?	prints this list\n");
			goto cmnd;
		}
	}
	exit(0);
}
icmp(a, b) int *a, *b;
{
	return(*a - *b);
}
files()
{	int i;
	if((df = opendir(maildir)) == NULL)
	{	perror(maildir);
		exit(1);
	}
	strcpy(line, myname);
	strcat(line, ".%d");
	while ((dbuf = readdir(df)) != NULL) 
	{
		if(sscanf(dbuf->d_name, line, &i) != 1)
			continue;
		if(fcnt >= MXF)
			break;
		fnum[fcnt++] = i;
	}
	closedir(df);
	if(fcnt == 0)
	{	printf("no secret mail\n");
		exit(0);
	}
	qsort(fnum, fcnt, sizeof(int), icmp);
}
decipher(u, w) FILE *u, *w;
{	int i;
	short a;
	for(;;)
	{	nin(mbuf, u);
		if(feof(u)) break;
		mult(mbuf, x, mbuf);
		mdiv(mbuf, b, q, mbuf);
		for(i=1; i<=3; i++)
		{	a = mbuf->val[i];
			putc(a&0177, w);
			a >>= 8;
			putc(a&0177, w);
		}
	}
}
