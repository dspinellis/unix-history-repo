#ifndef lint
static char *sccsid = "@(#)glue4.c	4.2 (Berkeley) 4/24/88";
#endif

#include <stdio.h>
#include <ctype.h>

extern char gfile[];

grepcall (in, out, arg)
char *in, *out, *arg;
{
	char line[200], *s, argig[100], *cv[50];
	char *inp, inb[500];
	FILE *qf, *gf;
	int c, oldc = 0, alph = 0, nv = 0;
	int sv0, sv1;
	strcpy (argig, arg); 
	strcat(argig, ".ig");
	strcpy (inp=inb, in);
	if (gfile[0]==0)
		sprintf(gfile, "/tmp/rj%dg", getpid());
# if D1
	fprintf(stderr, "in grepcall, gfile %s in %o out %o\n", gfile,in,out);
# endif
	for(cv[nv++] = "fgrep"; c = *inp; inp++)
	{
		if (c== ' ')
			c = *inp = 0;
		else if (isupper(c))
			*inp = tolower(c);
		alph = (c==0) ? 0 : alph+1;
		if (alph == 1)
			cv[nv++] = inp;
		if (alph > 6)
			*inp = 0;
		oldc=c;
	}
# if D1
	fprintf(stderr, "%d args set up\n", nv);
# endif
	{
		sv0 = dup(0);
		close(0);
		if (open (argig, 0) != 0)
			err("Can't read fgrep index %s", argig);
		sv1 = dup(1);
		close(1);
		if (creat(gfile, 0666) != 1)
			err("Can't write fgrep output %s", gfile);
		fgrep(nv, cv);
# if D1
		fprintf(stderr, "fgrep returned, output is..\n");
# endif
		close (0); 
		dup(sv0); 
		close(sv0);
		close (1); 
		dup(sv1); 
		close(sv1);
	}

# if D1
	fprintf(stderr, "back from fgrep\n");
# endif
	gf = fopen(gfile, "r");
	if (gf==NULL)
		err("can't read fgrep output %s", gfile);
	while (fgets(line, 100, gf) == line)
	{
		line[100]=0;
# if D1
		fprintf(stderr, "read line as //%s//\n",line);
# endif
		for(s=line; *s && (*s != '\t'); s++);
		if (*s == '\t')
		{
			*s++ = '\n';
			*s++ = 0;
		}
		if (line[0])
			strcat(out, line);
# if D1
		fprintf(stderr, "out now /%s/\n",out);
# endif
		while (*s) s++;
# if D1
		fprintf(stderr, "line %o s %o s-1 %o\n",line,s,s[-1]);
# endif
		if (s[-1]!= '\n')
			while (!feof(gf) && getc(gf)!= '\n') ;
	}
	fclose(gf);
# if D1
	fprintf(stderr, "back from reading %, out %s\n",out);
# else
	unlink (gfile);
# endif
	return(0);
}

clfgrep()
{
	if (gfile[0])
		unlink(gfile);
}
