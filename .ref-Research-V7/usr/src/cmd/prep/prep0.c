# include <stdio.h>
# include "prep.h"

int	(*acts[])() =	{0,
			coll,
			save,
			ctout,
			gobble2,
			hyphen,
			bsp,
			bslash,
			punc
			};


char	*ignonl = "/usr/lib/eign";

char tab[NUMS][NUMC] = {

/*SKIP*/
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 3, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 8, 0, 0, 0, 0, 0, 0,
	8, 8, 0, 0, 8, 0, 8, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 8, 8, 0, 0, 0, 8,
	0, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 0, 7, 0, 0, 0,
	0, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 0, 0, 0, 0, 0,
/*COLLECT*/
	2, 2, 2, 2, 2, 2, 2, 2,
	6, 2, 3, 2, 2, 2, 2, 2,
	2, 2, 2, 2, 2, 2, 2, 2,
	2, 2, 2, 2, 2, 2, 2, 2,
	2, 8, 2, 2, 2, 2, 2, 0,
	8, 8, 2, 2, 8, 5, 8, 2,
	2, 2, 2, 2, 2, 2, 2, 2,
	2, 2, 8, 8, 2, 2, 2, 8,
	2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 2, 7, 2, 2, 2,
	2, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 2, 2, 2, 2, 2,
/*SKIP2*/
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 3, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 8, 0, 0, 0, 0, 0, 0,
	8, 8, 0, 0, 8, 0, 8, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 8, 8, 0, 0, 0, 8,
	0, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 0, 0, 0, 0, 0,
	0, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 0, 0, 0, 0, 0
};

main(argc,argv)
	char	*argv[];
{
	auto	i,j;

	if(argc > 1 && *argv[1] == '-') {
		j = flags(argv);
		argv += j;
		argc -= j;
	}

	init();

	i = 0;
	if(argc == 1) {
		fi = stdin;
		goto pipe;
	}
	while(++i < argc) {

		if((fi = fopen(argv[i], "r")) == NULL) {
			fprintf(stderr, "Can't open %s\n",argv[i]);
			exit(1);
		}

pipe:
		lno = 1;

		driver(argv[i]);

		fclose(fi);
	}
	flsh();

	exit(0);
}

driver(arg)
char	*arg;
{
	auto	p;

	l = -1;
	while((c = line[++l] = getc(fi)) != -1) {
/*	fprintf(stderr, "driver: c = %o l = %d\n",c,l); /*DEBUG*/
		if(l >= 299) {
			lflag++;
			l--;
		}

		if(c & 0200) {
			fprintf(stderr, "Illegal character: %o line %d file %s\n",
				c, lno, arg);
			exit(1);
		}

		if(l == 0 && (c == '.' || c == '\'')) {
			while((c = getc(fi)) != '\n' )
				if(c == -1)	return;
			lno++;
			l = -1;
			continue;
		}

		if(fl) {
			if((*flag[fl])())
				continue;
		}

/*fprintf(stderr, "cs = %d cc = %c ca = %d\n",cs,c,tab[cs][c]);	/*DEBUG*/

		if(p = tab[cs][c])
			(*acts[p])();
		continue;
	}
	return;

}

init()
{
	FILE	*fio, *fopen();
	extern	coll(),save(),ctout(),asym(),asw(),csym(),csw();
	extern	incl(),decl(),sk(),sk2();


	if(!igflg)return;

	itab.hptr = ipsp;
	itab.symt = issp;
	itab.hsiz = PTRI;
	itab.ssiz = CHARI;
	itab.nsym = 0;
	itab.curb = 1;

	if((fio = fopen(ignonl, "r")) == NULL) {
		fprintf(stderr, "Cannot open ignore/only file.\n");
		exit(1);
	}
	compile(fio);
	return;
}


flags(argv)
	char	*argv[];
{
	int	j;
	char	*ap;

	j = 1;
	ap = argv[1];
	while(*++ap != '\0') {
		switch(*ap) {

			default:
				fprintf(stderr, "Unrecognized flag: %c\n",*ap);
				exit(1);


			case 'i':	/* Ignore file */
				if(!igflg) {
					igflg = 1;
					only = 0;
					ignonl = argv[++j];
				}
				continue;

			case 'o':	/*only file*/
				if(!igflg) {
					igflg = 1;
					only = 1;
					ignonl = argv[++j];
				}
				continue;
			case 'd':	/*word number*/
				wdflg = 1;
				wdnum = 0;
				num[WIDTH] = ' ';
				continue;

			case 'p':
				puncfl = 1;
				continue;
		}

	}
	return(j);
}
compile(fio)
FILE	*fio;
{
	char	buf[40],*b;
	int	i,v;


	b = buf - 1;
	while((i = *++b = getc(fio)) != EOF) {
		if(*b == '\n') {
			*b = '\0';
			search(buf,b - buf,&itab,1);
			b = buf - 1;
		} else {
			if(*b == '\t') {
				v = 0;
				while((i = getc(fio)) != -1) {
					if(i == '\n')	break;
					v = v*10 + (i - '0');
				}
				search(buf,b - buf,&itab,v);
				b = buf - 1;
			} else {
				if((b - buf) > 39) {
					fprintf(stderr, "Ignore/only symbol too long.\n");
					exit(1);
				}
			}
		}
	}
	return;
}
