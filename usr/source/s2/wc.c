/* wc line and word count */

char buf[518];
int wordct[2];
int linect[2];

main(argc,argv)
char **argv;
{
	int i;
	register int c;
	int token;

	i = 1;
	do {
		if(argc<=1) buf[0] = 0;
		else if(fopen(argv[i],buf)<0) {
			diag(argv[i]);
			diag(": cannot open\n");
			continue;
		}
		linect[0] = linect[1] = 0;
		wordct[0] = wordct[1] = 0;
		token = 0;
		while((c=getc(buf))>=0) {
			if(' '<c&&c<0177) {
				if(!token++) {
					if(++wordct[0]==0)
						wordct[1]++;
				}
			} else {
				if(c=='\n') {
					if(++linect[0]==0)
						linect[1]++;
				}
				else if(c!=' '&&c!='\t')
					continue;
				token = 0;
			}
		}
		printf("%7s ",locv(linect[1],linect[0]));
		printf("%7s ",locv(wordct[1],wordct[0]));
		printf("%s\n", argc<=1?"":argv[i]);
		close(buf[0]);
	} while(++i<argc);
}

diag(s)
char *s;
{
	while(*s)
		write(2,s++,1);
}
