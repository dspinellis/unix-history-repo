#include "xmail.h"
#include "sys/types.h"
#include "sys/dir.h"
#include "ctype.h"
#include "pwd.h"
#include "sys/stat.h"
char *myname;
int uid;
struct direct dbuf;
char *maildir = "/usr/spool/secretmail/";
FILE *kf, *mf, *df;
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
	myname = getlogin();
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
			unlink(line);
			fclose(mf);
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
			for(p=buf; !isspace(*p); p++);
			for(; isspace(*p); p++);
			p[strlen(p)-1] = 0;
			kf = fopen(p, "a");
			if(kf == NULL)
			{	perror(p);
				break;
			}
			decipher(mf, kf);
			fclose(mf);
			fclose(kf);
			unlink(line);
			break;
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
	if((df = fopen(maildir, "r")) == NULL)
	{	perror(maildir);
		exit(1);
	}
	strcpy(line, myname);
	strcat(line, ".%d");
	for(; !feof(df);)
	{	fread(&dbuf, sizeof(dbuf), 1, df);
		if(feof(df)) break;
		if(dbuf.d_ino == 0) continue;
		if(sscanf(dbuf.d_name, line, &i) != 1)
			continue;
		if(fcnt >= MXF)
			break;
		fnum[fcnt++] = i;
	}
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
