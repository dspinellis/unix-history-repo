/*
 * NAME: last
 *
 * SYNOPSIS: last [-] [name ...]
 *
 * DESCRIPTION: Displays login history of named users or tty's.
 *    		 Last with no argument prints history for all users.
 *      	 Last - displays history for all non-class users.
 *
 * EXTERNAL ROUTINES USED: bread.c blseek.c
 *
 * AUTHOR - Howard P. Katseff
 */

struct {char c1, c2, c3, c4;};
struct {long l1;};
int	allflag;
char buf[32];
extern	fout;

main(argc, argv)
char **argv; {
	register char tty;
	register char **p;
	register char *q;
	int f, i, count;
	int intrp();
	long lx;
	long otime, ntime;
	long logouts[128];

	count = 0;
	fout = dup(1);
	if (allflag = argc==1) argv--;
	f = open("/usr/adm/wtmp", 0);
	if (f < 0) {
		printf("Can't open /usr/adm/wtmp\n");
		exit();
		}
x:      fstat(f, buf);

	for (i=0; i<127; i++) logouts[i] = 0;

	lx.c2 = 0;
	lx.c1 = buf[9];
	lx.c3 = buf[10] & 0360;
	lx.c4 = buf[11];

	blseek(f, lx, 0);
	lx =- 16;

	if ((signal(2,1) && 01) == 0) signal(2, &intrp);

	for(;;) {
		if (lx < 16) {
			f = open("/usr/adm/owtmp", 0);
			if (count++ == 0 && f > 0) goto x;
			q = ctime(buf+10);
			printf("\nwtmp begins at  %10.10s  %5.5s \n",
				q, 11+q);
			flush();
			exit();
			}

		if (bread(f, buf+16, -16) < 16) {
			perror("impossible error");
			exit();
			}

		for (p = 1+argv; *p != -1; p++) {
			if (equal(*p, buf)) {
				tty = buf[8];
				q = ctime(buf+10);
				if (tty >= ' ') 
					printf("%8.8s  tty%c  %10.10s  %5.5s ",
						buf, tty, q, 11+q);
				else
					printf("%8.8s  tty^%c %10.10s  %5.5s ",
						buf, tty+0140, q, 11+q);
				otime = (buf+10)->l1;
				ntime = logouts[tty];
				if (ntime == 0) {
					printf("  still logged in\n");
					}
				else {
					if (ntime <= 0) {
						ntime = -ntime;
						printf("- crash");
						}
					else printf("- %5.5s", 11+ctime(&ntime));
					otime = ntime - otime;
					otime =+ 231231630;
					if (otime < 231318030)
						printf("  (%5.5s)\n", 11+
						ctime(&otime));
					else
						printf(" (%ld+%5.5s)\n",
						(otime-231231630)/86400,
						11+ctime(&otime));
					}
				flush();
				break;
				}
			}
		lx =- 16;

		if (buf[0] == '\0') {
			tty = buf[8];
			if (tty == '~')
			    for (i=0; i<127; i++) logouts[i] = -(buf+10)->l1;
			else logouts[tty] = (buf+10)->l1;
			}
		}
	}


equal(c1, c2)
char *c1, *c2; {
	int i;

	if (allflag && (*c2 != '\0' || *(c2+8) >= '|')) return(1);

	if (*(c1+1) == '\0') {
		if ((*c1 == '-') || (*c1 == 'x')) {
			if (*c2 == '\0' && *(c2+8) < '|') return(0);
			if (*c2 >= '0' && *c2 <= '9') return(0);
			if (*(c2+2) >= '0' && *(c2+2) <= '9') return(0);
			for(i=0; i<8; i++) if (*c2++ == '-') return(0);
			return(1);
			}

		if (*c1 == *(c2+8)  &&  (*c1 >= '|' || *c2 != '\0')) return(1);

		return(0);
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
	fout = 1;
	*(&fout+1) = 1;
	*(&fout+2) = 0;
	*(&fout+3) = 0;

	q = ctime(buf+10);
	printf("\ninterrupted at  %10.10s  %5.5s \n",
		q, 11+q);

	exit();
	}
/*
 * NAMES:  bread(), brseek(), blseek()
 *
 * DESCRIPTION:
 *	 This is a buffered read package which simulates  read(), seek() and
 *      lseek().
 *       Bread may be called with a negative nbytes which causes it to
 *      read backwards.  In this case, buffer should point to the first
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
