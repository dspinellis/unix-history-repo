/*
 * toc - convert to card format
 *
 * Author: Howard Katseff UCB June, 1977
 *
 * Toc converts the specified file or the standard input
 * to card format for submission over an unfriendly medium
 * such as the rcs link.
 * The original input can then be recovered by froc.
 */
int c 1;
char table[] "abcdefghijklmnopqrstuvwxyz0123456789.,/$-";
char nl '\012';
extern int ldivr;

main(argc, argv)
	int argc;
	char *argv[];
{
	int w, k;
	register n, i;

	argc--, argv++;
	if (argc > 1) {
		write(2, "Usage: toc [ file ]\n", 20);
		exit (1);
	}
	if (argc == 1) {
		close(0);
		if (open(argv[0], 0) < 0) {
			perror(argv[0]);
			exit(1);
		}
	}
	for(;;) {
		w = 0;
		n = bread(0, &w, 2);
		if( n==0 ) {
			if(c!= 1) bwrite(1,&nl,1);
			wflush(1);
			return;
			}
		for(i = 1; i <= 3; i++) {
			w = ldiv(0, w, 41);
			bwrite(1, &table[ldivr], 1);
			if (c++ >= 80) {
				c = 1;
				bwrite(1,&nl, 1);;
				}
			if(i==2 && n==1) break;
			}
		}
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
