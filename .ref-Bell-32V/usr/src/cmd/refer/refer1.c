# include "signal.h"
# include "refer..c"
main(argc,argv)
	char *argv[];
{
char line[LLINE], *s;
int nodeflt =0;

signals();
while (argv[1][0] == '-')
	{
	switch(argv[1][1])
		{
		case 'e':
			endpush++; break;
		case 'l': 
			labels++;
			s = argv[1]+2;
			nmlen = atoi(s);
			while (*s)
				if (*s++ == ',')
					break;
			dtlen = atoi(s);
			break;
		case 'k':
			keywant = (argv[1][2] ? argv[1][2] : 'L');
			labels++;
			break;
		case 's':
			sort++;
			if (argv[1][2])
				keystr= argv[1]+2;
			break;
		case 'p':
			argc--; argv++;
			*search++ = argv[1];
			if (search-rdata>NSERCH)
				err("too many -p options (%d)", NSERCH);
			break;
		case 'n':
			nodeflt=1;
			break;
		case 'b':
			bare = (argv[1][2] == '1') ? 1 : 2;
			break;
		case 'c':
			smallcaps = argv[1]+2;
			break;
		case 'a':
			authrev = atoi (argv[1]+2);
			if (authrev<=0)
				authrev = 1000;
			break;
		}
	argc--; argv++;
	}
if (nodeflt==0)
	*search++ = "/usr/dict/papers/Ind";

if (sort)
	endpush=1;
if (endpush)
	{
	sprintf(tfile, "/tmp/rj%da", getpid());
	fo = fopen(tfile, "w");
	if (fo==NULL)
		{
		fo = ftemp;
		fprintf(stderr,"Can't open scratch file");
		}
	sep = 002; /* separate records without confusing sort..*/
	}


if (sort && !labels)
	{
	sprintf(ofile,"/tmp/rj%db", getpid());
	ftemp = fopen(ofile, "w");
	if (ftemp==NULL)
		{
		fprintf(stderr, "Can't open scratch file\n");
		exit(1);
		}
	}

do
	{
	if (argc>1)
		{
		fclose(in);
		Iline=0;
		in = fopen(Ifile=argv[1], "r");
		argc--; argv++;
		if (in==NULL)
			{
			err("Can't read %s", Ifile);
			continue;
			}
		}
	while (input(line))
		{
		Iline++;
# ifdef D1
		fprintf(stderr, "line %.20s\n",line);
# endif
		if (!prefix(".[", line))
			output(line);
		else
			doref(line);
# if D1
		fprintf(stderr, "past output/doref\n");
# endif
		}
	}
	while (argc>1);
if (endpush && fo!=NULL)
	dumpold();
output("", ftemp);
if (sort && !labels)
	recopy(ofile);
clfgrep();
cleanup();
exit(0);
}

extern int intr();
signals()
{
	int oldint;
oldint = signal(SIGINT, &intr);
if (oldint==1)
	signal (SIGINT, 1);
signal (SIGHUP, &intr);
signal (SIGPIPE, &intr);
signal (SIGTERM, &intr);
}

intr()
{
	int oldsig;
signal(SIGINT, 1);
cleanup();
exit(1);
}
cleanup()
{
if (tfile[0]) unlink(tfile);
if (gfile[0]) unlink(gfile);
if (ofile[0]) unlink(ofile);
if (hidenam[0]) unlink(hidenam);
}
