# include "signal.h"
# include "refer..c"
main(argc,argv)
	char *argv[];
{
char line[LLINE], *s;
int nodeflt =0;

signals();
while (argc>1 &&argv[1][0] == '-')
	{
	switch(argv[1][1])
		{
		case 'e':
			endpush++; break;
		case 'l': 
			labels++;
			s = argv[1]+2;
			nmlen = atoi(s);
			for ( ; *s; s++)
				if (*s == ',' || *s =='_' || *s =='"')
					break;
			if (*s==0) break;
			switch(*s++)
				{
				case ',': labblkflg=0; break;
				case '_': labblkflg=" "; break;
				case '"':
					labblkflg=s;
					while (*s && *s != '"')
						s++;
					*s++= 0; break;
				}
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
		case 'B':
			nobracket++; /* falls through */
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
			for(s=argv[1]+2; isdigit(*s); s++)
				;
			if (*s==',') nocomma++;
			break;
		case 'd': /* reduce date to year only */
			yearonly=1;
			break;
		case 'A': /* these fields get appended */
			appfld = argv[1]+2;
			break;
		case 'I': /* these fields get ignored */
			ignfld = argv[1]+2;
			break;
		case 'P': /* preserve original: no .ds's */
			preserve = 1;
			break;
		case 'L':
			labsepstr= argv[1]+2; break;
		}
	argc--; argv++;
	}
if (nodeflt==0)
	*search++ = "/usr/dict/papers/Ind";

if (sort)
	endpush=1;


if (endpush)
	sprintf(tfile, "/tmp/rj%da", getpid());
if (sort)
	{
	sprintf(ofile,"/tmp/rj%db", getpid());
	sprintf(tdfile, "/tmp/rj%de", getpid());
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
		if (in!=stdin)
			fclose(in);
		Iline=0;
		if (strcmp(argv[1], "-")==SAME)
			in = stdin;
		else
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
		if (prefix(".[", line) || (nobracket && line[0]!='\n'))
			{
			if (endpush && (fo==NULL || fo == stdout))
				{
				fo = fopen(tfile, "w");
# if D1
				fprintf(stderr, "opened %s as %o\n",tfile,fo);
# endif
				if (fo==NULL)
					{
					fprintf(stderr,"Can't open scratch file");
					exit(1);
					}
				sep = 002; /* separate records without confusing sort..*/
				}
			doref(line);
			}
		else
			output(line);
# if D1
		fprintf(stderr, "past output/doref\n");
# endif
		}
	}
	while (argc>1);
# if D1
fprintf(stderr, "before dumpold, endpush %d fo %o\n",endpush, fo);
# endif
widelab();
if (endpush && (fo!=NULL && fo != stdout))
	dumpold();
output("");
fflush (ftemp);
if (sort)
	recopy(ofile);
clfgrep();
# ifndef D1
cleanup();
# endif
exit(0);
}

extern int intr();
signals()
{
signal (SIGINT, intr);
signal (SIGHUP, intr);
signal (SIGPIPE, intr);
signal (SIGTERM, intr);
}

intr()
{
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
