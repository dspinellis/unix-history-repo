# include "stdio.h"
extern char *soutput, *tagout, usedir[];

result(master, nf, fc)
	union ptr {unsigned *a; long *b;} *master;
	FILE *fc;
{
int i, c;
char *s;
long lp;
extern int iflong;
char res[100];

for(i=0; i<nf; i++)
	{
	lp = iflong ? master.b[i] : master.a[i];
	fseek(fc,lp, 0);
	fgets(res, 100, fc);
	for(s=res; c = *s; s++)
		if (c== ';')
			{
			*s=0;
			break;
			}
	if (tagout !=0)
		{
		if (res[0]=='/' || usedir[0]==0)
			sprintf(tagout, "%s", res);
		else
		sprintf(tagout, "%s/%s", usedir, res);
		while (*tagout) tagout++;
		}
	else
		{
		if (res[0]!='/' || usedir[0]==0)
			printf("%s/", usedir);
		printf("%s\n", res);
		}
	}
}

# include "sys/types.h"
# include "sys/stat.h"
long gdate(f)
	FILE *f;
{
struct stat sb;
fstat (f->_file, &sb);
return  (sb . st_mtime);
}
