/*
 * froc - from card format (as produced by toc)
 *
 * Author: Howard Katseff UCB June, 1977
 *
 * This program decodes the cards produced by the program toc
 * used to send binary data over an unfriendly medium
 * by converting to the characters common to ASCII and BCD.
 *
 * If the input is from a file and looks like output from the campus 6400,
 * then strip carriage control, header and trailer.
 */
extern linecnt, bytecnt;
extern nlflag, ccflag;

main(argc, argv)
	int argc;
	char *argv[];
{
	int t;
	register n;
	char c1,c2,c3;
	char buf[36];

	argc--, argv++;
	if (argc > 1) {
		write(2, "Usage: froc [ file ]\n", 21);
		exit (1);
	}
	if (argc == 1) {
		close (0);
		if (open(argv[0], 0) < 0) {
			perror(argv[0]);
			exit (1);
		}
	}

	linecnt = 1;
	bytecnt = 0;
	nlflag = 1;

	if (gtty(0, buf) == -1 && seek(0, 0, 0) != -1) {
		for (n=0; n<10; n++) bread(0, &c1, 1);
		if (c1 == ' ') ccflag = 1;
		brseek(0, 0, 0);
		c1 = readb();
		for(;;) {
			n = c1 = readb();
			if (c1 == '\f') break;
			if (n == 0) {
				brseek(0, 0, 0);
				break;
				}
			}
		}

	for(;;) {
		do {
			n = c1 = readb();
			if (c1 == '\f') n = 0;
			if(n==0) { wflush(1); return;}
			} while (c1 <= ' ');
		c1 = convert(c1);

		do{
			n = c2 = readb();
			if (c2 == '\f') n = 0;
			if(n==0) {
				wflush(1);
				close(1); dup(2);
				printf("error - unexpected eof\n");
				return;
				}
			} while (c2 <= ' ');
		c2 = convert(c2);

		do {
			n = c3 = readb();
			if (c3 == '\f') n = 0;
			} while (c3 <= ' ' && n);

		if (n) {
			c3 = convert(c3);
			t = c1+(41*(c2+(41*c3)));
			bwrite(1,&t,2);
			}
		else {
			t = c1 + (41*c2);
			bwrite(1, &t, 1);
			wflush(1);
			return;
			}
		}
	}




char convert(c)
char c; {
	if(c>='a' && c<='z') return(c-'a');
	if(c>='0' && c<='9') return(c-'0'+26);
	switch(c) {
		case '.':	return(36);
		case ',':	return(37);
		case '/':	return(38);
		case '$':	return(39);
		case '-':	return(40);
		default:	wflush(1);
				close(1); dup(2);
				printf("error - illegal character '%c' ", c);
				printf(" (byte %d on line %d)\n", bytecnt,
					linecnt);
				exit();
		}
	}


char readb() {
	char c;

 l1:	if (bread(0, &c, 1) == 0) return(0);

	if (!ccflag) return(c);

	if (nlflag && c != '\n') {
		nlflag = 0;
		switch (c) {
			case '1':
			case '6':
				return('\014');
			default:
				goto l1;
			}
		}
	if (c == '\n') nlflag = 1;
	return(c);
	}
extern bytecnt;
extern linecnt;

int n;
char *next;
char b[512];
bread(file,buff,nbytes) char *buff; {
	register int nb;

	nb = nbytes;
	while(nb) {
		if(n==0) {
			n = read(file,next=b,512);
			if(n<0) return(-1);
			if(n==0) return(nbytes-nb);
		}
		bytecnt++;
		if (*next == '\n') linecnt++;
		*buff++ = *next++;
		n--;
		nb--;
	}
	return(nbytes-nb);
}

brseek(file, offset, flag) {
	n = 0;
	linecnt = 1;
	bytecnt = 0;
	return(seek(file, offset, flag));
	}

int wn;
char wb[512];
bwrite(file,buff,nbytes) char *buff; {
	register int nb;

	nb=nbytes;
	while(nb) {
		if(wn==512) {
			if(512 != write(file,wb,512)) return(-1);
			wn = 0;
		}
		wb[wn++] = *buff++;
		nb--;
	}
	return(nbytes);
}
wflush(file) {
	if(wn) write(file,wb,wn);
}
