# include <sys/param.h>
# include <stat.h>
int	status;

main(argc, argv)
int argc;
char **argv;
{
register i;
register char *c1, *c2;
char dirname[100];

if(argc < 3)
	{
	prs("arg count\n");
	exit(1);
	}
argc--;
c1 = dirname;
c2 = argv[argc];
while(*c1++ = *c2++);
c1[-1] = '/';
c1[0] = '.';
c1[1] = '\0';
if(filetype(dirname) !=  S_IFDIR)
	{
	prs(dirname);
	prs(" is not a directory.\n");
	exit(1);
	}

for(i=1; i<argc; i++) 
	{
	if(filetype(argv[i]) != S_IFREG)
		{
		prs(argv[i]);
		prs(" is not an ordinary file.\n");
		}
	else if(fork() == 0)
		{
		execl("/bin/mv", "mv", argv[i], dirname,0);
		exit(1);
		}
	else wait(&status);
	}
}

filetype(filename)
char *filename;
{
struct	stat buf ;

if(stat(filename,&buf) < 0) 
	return(-1);
else return(buf.st_mode&S_IFMT);
}


prs(s)
register char *s;
{
while(*s)
	write(2, s++, 1);
}
