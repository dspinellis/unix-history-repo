static char *sccsid = "@(#)crypt.c	4.4 (Berkeley) %G%";

/*
 *	A one-rotor machine designed along the lines of Enigma
 *	but considerably trivialized.
 */

#define ECHO 010
#include <stdio.h>
#include "pathnames.h"
#define ROTORSZ 256
#define MASK 0377
char	t1[ROTORSZ];
char	t2[ROTORSZ];
char	t3[ROTORSZ];
char	deck[ROTORSZ];
char	*getpass();
char	buf[13];

setup(pw)
char *pw;
{
	int ic, i, k, temp, pf[2];
	int pid, wpid;
	unsigned random;
	long seed;

	strncpy(buf, pw, 8);
	while (*pw)
		*pw++ = '\0';
	buf[8] = buf[0];
	buf[9] = buf[1];
	pipe(pf);
	if ((pid=fork())==0) {
		close(0);
		close(1);
		dup(pf[0]);
		dup(pf[1]);
		execl(_PATH_MAKEKEY, "-", 0);
		exit(1);
	}
	write(pf[1], buf, 10);
	while ((wpid = wait((int *)NULL)) != -1 && wpid != pid)
	    ;
	if (read(pf[0], buf, 13) != 13) {
		fprintf(stderr, "crypt: cannot generate key\n");
		exit(1);
	}
	seed = 123;
	for (i=0; i<13; i++)
		seed = seed*buf[i] + i;
	for(i=0;i<ROTORSZ;i++) {
		t1[i] = i;
		deck[i] = i;
	}
	for(i=0;i<ROTORSZ;i++) {
		seed = 5*seed + buf[i%13];
		random = seed % 65521;
		k = ROTORSZ-1 - i;
		ic = (random&MASK)%(k+1);
		random >>= 8;
		temp = t1[k];
		t1[k] = t1[ic];
		t1[ic] = temp;
		if(t3[k]!=0) continue;
		ic = (random&MASK) % k;
		while(t3[ic]!=0) ic = (ic+1) % k;
		t3[k] = ic;
		t3[ic] = k;
	}
	for(i=0;i<ROTORSZ;i++)
		t2[t1[i]&MASK] = i;
}

main(argc, argv)
char *argv[];
{
	register i, n1, n2, nr1, nr2;
	int secureflg = 0;

	if (argc > 1 && argv[1][0] == '-' && argv[1][1] == 's') {
		argc--;
		argv++;
		secureflg = 1;
	}
	if (argc != 2){
		setup(getpass("Enter key:"));
	}
	else
		setup(argv[1]);
	n1 = 0;
	n2 = 0;
	nr2 = 0;

	while((i=getchar()) >=0) {
		if (secureflg) {
			nr1 = deck[n1]&MASK;
			nr2 = deck[nr1]&MASK;
		} else {
			nr1 = n1;
		}
		i = t2[(t3[(t1[(i+nr1)&MASK]+nr2)&MASK]-nr2)&MASK]-nr1;
		putchar(i);
		n1++;
		if(n1==ROTORSZ) {
			n1 = 0;
			n2++;
			if(n2==ROTORSZ) n2 = 0;
			if (secureflg) {
				shuffle(deck);
			} else {
				nr2 = n2;
			}
		}
	}
}

shuffle(deck)
	char deck[];
{
	int i, ic, k, temp;
	unsigned random;
	static long seed = 123;

	for(i=0;i<ROTORSZ;i++) {
		seed = 5*seed + buf[i%13];
		random = seed % 65521;
		k = ROTORSZ-1 - i;
		ic = (random&MASK)%(k+1);
		temp = deck[k];
		deck[k] = deck[ic];
		deck[ic] = temp;
	}
}
