#include <stdio.h>
#include <signal.h>
#define	MAX	100

char	types[10];
int	right[MAX];
int	left[MAX];
int	rights;
int	wrongs;
long	stvec;
long	etvec;
long	dtvec;

main(argc,argv)
char	*argv[];
{
	int range, k, dif, l;
	char line[100];
	int ans,pans,i,j,t;
	char	dir,sense;
	extern	delete();

	signal(SIGINT, delete);

	range = 11;
	dif = 0;
	while(argc > 1) {
		switch(*argv[1]) {
		case '+':
		case '-':
		case 'x':
		case '/':
			while(types[dif] = argv[1][dif])
				dif++;
			break;

		default:
			range = getnum(argv[1]) + 1;
		}
		argv++;
		argc--;
	}
	if(range > MAX) {
		printf("Range is too large.\n");
		exit();
	}

	if(dif == 0) {
		types[0] = '+';
		types[1] = '-';
		dif = 2;
	}

	for(i = 0; i < range; i++) {
		left[i] = right[i] = i;
	}
	time(&stvec);
	k = stvec;
	srand(k);
	k = 0;
	l = 0;
	goto start;

loop:
	if(++k%20 == 0)
		score();

start:
	i = skrand(range);
	j = skrand(range);
	if(dif > 1)
		l = random(dif);

	switch(types[l]) {
		case '+':
		default:
			ans = left[i] + right[j];
			printf("%d + %d =   ", left[i], right[j]);
			break;

		case '-':
			t = left[i] + right[j];
			ans = left[i];
			printf("%d - %d =   ", t, right[j]);
			break;

		case 'x':
			ans = left[i] * right[j];
			printf("%d x %d =   ", left[i], right[j]);
			break;

		case '/':
			while(right[j] == 0)
				j = random(range);
			t = left[i] * right[j] + random(right[j]);
			ans = left[i];
			printf("%d / %d =   ", t, right[j]);
			break;
	}


loop1:
	getline(line);
	dtvec += etvec - stvec;
	if(line[0]=='\n') goto loop1;
	pans = getnum(line);
	if(pans == ans) {
		printf("Right!\n");
		rights++;
		goto loop;
	}
	else {
		printf("What?\n");
		wrongs++;
		if(range >= MAX)	goto loop1;
		left[range] = left[i];
		right[range++] = right[j];
		goto loop1;
	}
}

getline(s)
char *s;
{
	register char	*rs;

	rs = s;

	while((*rs = getchar()) == ' ');
	while(*rs != '\n')
		if(*rs == 0)
			exit();
		else if(rs >= &s[99]) {
			while((*rs = getchar()) != '\n')
				if(*rs == '\0')	exit();
		}
		else
			*++rs = getchar();
	while(*--rs == ' ')
		*rs = '\n';
}

getnum(s)
char *s;
{
	int	a;
	char	c;

	a = 0;
	while((c = *s++) >= '0' && c <= '9') {
		a = a*10 + c - '0';
	}
	return(a);
}

int arand;

srand(n)
{
	arand = n&077774 | 01;
}

rand()		/*uniform on 0 to 2**13-1*/
{

	arand *= 3125;
	arand &= 077777;
	return(arand/4);
}

random(range)
{
	return(hmul(rand(), 8*range));
}

skrand(range){
int temp;
	temp = rand() + rand();
	if(temp >017777) temp = 040000 - temp;
	return(hmul(temp,8*range));
	}

/* 'hmul' returns the upper 16 bits of the product, where the operands
   are assumed to be 16-bit integers. It replaces an old PDP-11 
   assembler language subroutine. -- dks.
*/
hmul(a,b) { return(a*b >> 16); }
score()
{
	time(&etvec);

	printf("\n\nRights %d; Wrongs %d; Score %d%%\n", rights, wrongs,
		(rights * 100)/(rights + wrongs));

	if(rights == 0)	return;
	printf("Total time %ld seconds; %.1f seconds per problem\n\n\n",
		etvec - stvec,
		(etvec - stvec) / (rights + 0.));

	sleep(3);
	time(&dtvec);
	stvec += dtvec - etvec;
	return(0);
}

delete()
{
	if(rights + wrongs == 0.) {
		printf("\n");
		exit();
	}
	score();
	exit();
}

