/*
 * sess - determines clock time used so far this session by this user
 *	  or for tty or user supplied as an argument
 *
 * Author: Howard Katseff
 *
 * Note that 'sess ~' gives time since system last started.
 */

struct {long l1;};
struct {char c1, c2, c3, c4;};
char buf[32];

main(argc, argv)
char **argv; {
	int f, i;
	char t[2], *p;
	long lx;
	int intrp();

	t[1] = 0;
	f = open("/usr/adm/wtmp", 0);
	if (f < 0) {
		printf("Can't open /usr/adm/wtmp\n");
		exit();
		}
	fstat(f, buf);

	lx.c2 = 0;
	lx.c1 = buf[9];
	lx.c3 = buf[10] & 0360;
	lx.c4 = buf[11];

	blseek(f, lx, 0);

	if (argc <= 1) {
		t[0] = ttyn(2);
		p = t;
		}
	else p = argv[1];

	if ((signal(2,1) && 01) == 0) signal(2, &intrp);

	for (;;) {
		if (bread(f, buf+16, -16) < 16) exit();
		lx =- 16;
		if (equal(p, buf)) {
			if (*(p+1) != '\0') {
				t[0] = buf[8];
				p = t;
				blseek(f, lx+16, 0);
				while(bread(f, buf, 16) > 0)
					if (equal(p, buf)) goto l1;
				blseek(f, lx, 0);
				bread(f, buf, 16);
			   l1:; }
			time(&lx);
			lx =+ 231231630 - (buf+10)->l1;
			printf("Elapsed time:");
			if (lx < 231318030)
				printf(" %5.5s", 11+ctime(&lx));
			else
				printf(" %ld + %5.5s",(lx-231231630)/86400,
					11+ctime(&lx));
			if (buf[0] == '\0' && *p != '~')
				printf(" since logout\n");
			else putchar('\n');
			exit();
			}
		}
	}


equal(c1, c2)
char *c1, *c2; {
	int i;

	if (*(c1+1) == '\0') {
		if (*c1 == *(c2+8)) return(1);
		else return(0);
		}

	for (i=0; i<8; i++) {
		if (*c1 == '\0') return(*c2 == ' ');
		if (*c1++ != *c2++) return(0);
		}

	return(1);
	}

intrp() {
	register char *q;

	signal(2,1);

	q = ctime(buf+10);
	printf("interrupted at  %10.10s  %5.5s \n",
		q, 11+q);

	exit();
	}
/*
 * NAME:  bread(), brseek(), blseek()
 *
 * DESCRIPTION:
 *	 This is a buffered read package which simulates  read(), seek() and
 *      lseek().
 *       Bread may be called with a negative nbytes which causes it to
 *      read backwards .  In this case, buffer should point to the first
 *      byte following the buffer.  If only a partial read is possible
 *      (due to beginning of file), only the last bytes of the buffer
 *      will be filled.
 */

int	i, j, k;
int	nl, nr;
char	*next;
char	b[512];

bread(file, buff, nbytes)
char *buff; {
	register nb;

	if (nbytes > 0) {
		for (nb=nbytes; nb>0; nb--) {
			if (nr == 0) {
				nr = read(file, next=b, 512);
				nl = 0;
				if (nr < 0) return(-1);
				if (nr == 0) return(nbytes-nb);
				}
			*buff++ = *next++;
			nr--;
			nl++;
			}
		}
	else {
		nbytes = -nbytes;
		for (nb=nbytes; nb>0; nb--) {
			if (nl == 0) {
				seek(file, -(512 + nr), 1);
				nl = read(file, b, 512);
				if (nl < 0) {
					for (k=511; k>0; k--) {
						seek(file, 1, 1);
						nl = read(file, b, k);
						if (nl >= 0) break;
						}
					if (nl < 0) return(nbytes-nb);
					}
				if (nl == 0) return(nbytes-nb);
				next = b + nl;
				nr = 0;
				}
			*--buff = *--next;
			nr++;
			nl--;
			}
		}
	return(nbytes);
	}


brseek(file, offset, flag) {
	nl = 0;
	nr = 0;
	return(seek(file,offset,flag));
	}


blseek(file, offset, flag) 
long offset; {
	nl = 0;
	nr = 0;
	return(lseek(file,offset,flag));
	}
