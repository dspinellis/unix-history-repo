#
/* paste: concatenate corresponding lines of each file in parallel(GWRL) */
/*	(-s option: serial concatenation like old (127's) paste command */
/* make :  cc paste.c -lS */
# include <stdio.h>
# define MAXOPNF 12  	/* maximal no. of open files (not with -s option) */
# define MAXLINE 512  	/* maximal line length */
#define RUB  '\177'
	char del[MAXLINE] = {"\t"};
  
main(argc, argv)
int argc;
char ** argv;
{
	int i, j, k, eofcount, nfiles;
	int delcount = { 1 } ;
	int onefile  = { 0 } ;
	char outbuf[MAXLINE], c, l ;
	register char *p;
	FILE *inptr[MAXOPNF];
  
	while (argc > 1 && argv[1][0] == '-' && (c = argv[1][1]) != '\0'){
		switch (c) {
			case 's' :  onefile++;
				c = argv[1][2];
				argv[1]++;
				break ;
			case 'd' : argv[1] += 2;
				if((delcount = move(argv[1], &del[0])) == 0) diag("no delimiters\n",1);;
				break;
			default :
				diag("Usage: paste [-s] [-d<delimiterstring>] file1 file2 ...", 1);
				break;
		}
		--argc;
		++argv;
	} /* end options */
	--argc;
 
	if ( ! onefile) {	/* not -s option: parallel line merging */
		for (i = 0; argc >0 && i < MAXOPNF; i++) {
			if (argv[i + 1][0] == '-') {
				inptr[i] = stdin;
			} else inptr[i] = fopen(argv[i + 1], "r");
			if (inptr[i] == NULL) {
				diag(argv[i + 1], 0);
				diag(" : cannot open\n", 1);
			}
		argc--;
		}
		if (argc > 0) diag("too many files\n",1);
		nfiles = i;
  
		do {
			p = &outbuf[0];
			eofcount = 0;
			j = k = 0;
			for (i = 0; i < nfiles; i++) {
				while((c = getc(inptr[i])) != '\n' && c != EOF)   {
					if (++j <= MAXLINE - 2) *p++ = c ;
					else {
					diag("line too long\n",1);
					}
				}
				if ( (l = del[k]) != RUB) *p++ = l;
				k = (k + 1) % delcount;
				if( c == EOF) eofcount++;
			}
			if (l != RUB) *--p = '\n'; else  *p = '\n';
			*++p = 0;
			if (eofcount < nfiles) fputs(outbuf, stdout);
		}while (eofcount < nfiles);
  
	} else {	/* -s option: serial file pasting (old 127 paste command) */
		p = &outbuf[0];
		j = 0;
		k = 0;
		for (i = 1; i <= argc; i++) {
			if (argv[i][0] == '-') {
				inptr[0] = stdin;
			} else inptr[0] = fopen(argv[i], "r");
			if (inptr[0] == NULL) {
				diag(argv[i], 0);
				diag(" : cannot open\n", 1);
			}
	  
			while((c = getc(inptr[0])) != EOF)   {
				if(c != '\n') {
					if (++j <= MAXLINE - 2) *p++ = c ;
					else diag("line too long\n",1);
				} else {
					l = del[k];
					if (l != RUB) *p++ = l ;
					k = (k + 1) % delcount;
					*p = 0;
					fputs(outbuf, stdout);
					p = &outbuf[0];
					j = 0;
				}
			}
		}
		if (l != '\n') fputs("\n", stdout);
	}
}
diag(s,r)
char *s;
int r;
{
	write(2, "paste : ", 8);
	while(*s)write(2,s++,1);
	if(r != 0) exit(r);
}
  
move(from, to)
char *from, *to;
{
int c, i;
	i = 0;
	do {
		c = *from++;
		i++;
		if (c != '\\') *to++ = c;
		else { c = *from++;
			switch (c) {
				case '0' : *to++ = RUB;
						break;
				case 't' : *to++ = '\t';
						break;
				case 'n' : *to++ = '\n';
						break;
				default  : *to++ = c;
						break;
			}
		}
	} while (c) ;
return(--i);
}
