/* wc line and word count */

int	buf[259];
int wordct[2];
int twordct[2];
int linect[2];
int tlinect[2];

main(argc,argv)
char **argv;
{
	int i, token;
	register char *p1, *p2;
	register int c;

	i = 1;
	do {
		if(argc<=1) buf[0] = 0;
		else if(fopen(argv[i],buf)<0) {
			diag(argv[i]);
			diag(": cannot open\n");
			continue;
		}
		p1 = 0;
		p2 = 0;
		linect[0] = linect[1] = 0;
		wordct[0] = wordct[1] = 0;
		token = 0;
		for(;;) {
			if(p1 >= p2) {
				p1 = &buf[1];
				c = read(buf[0], p1, 512);
				if(c <= 0)
					break;
				p2 = p1+c;
			}
			c = 0;
			c =| *p1++;
			if(' '<c&&c<0177) {
				if(!token++) {
					if(++wordct[1]==0)
						wordct[0]++;
				}
			} else {
				if(c=='\n') {
					if(++linect[1]==0)
						linect[0]++;
				}
				else if(c!=' '&&c!='\t')
					continue;
				token = 0;
			}
		}
		printf("%7s ",locv(linect[0],linect[1]));
		printf("%7s ",locv(wordct[0],wordct[1]));
		printf("%s\n", argc<=1?"":argv[i]);
		close(buf[0]);
		ladd(tlinect, tlinect, linect);
		ladd(twordct, twordct, wordct);
	} while(++i<argc);
	if(argc > 2) {
		printf("%7s ",locv(tlinect[0],tlinect[1]));
		printf("%7s ",locv(twordct[0],twordct[1]));
		printf("total\n");
	}
}

diag(s)
char *s;
{
	while(*s)
		write(2,s++,1);
}
