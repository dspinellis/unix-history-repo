# include "../mcons.h"
struct	{
		int	cl[NUMC];
	}tab[NUMS];
char	state[NUMS][SIZS];
char	class[NUMC][SIZC];
char	act[NUMA][SIZA];
char	def[NUMS][SIZA];
char	temp[SIZA];

char	*st[NUMS];
char	*df[NUMS];
char	*cl[NUMC];
char	*ac[NUMA];

int	t1;
int	t2;


main(argc,argv)	char	**argv;
{

	extern	fin;
	char	fl,nlfl,c,bfl,fo,brk;
	int	cs,ca,cc,i,j,cd;

	if(argc != 3) {
		printf("Usage: mtab input output\n");
		exit();
	}

	if((fo = creat(argv[2],0644)) < 0) {
		printf("Output file.\n");
		exit();
	}
	if((fin = open(argv[1],0)) < 0) {
		printf("Input file.\n");
		exit();
	}



	nlfl = 1;
	i = -1;
	while(brk = rword()) {
		switch (brk) {
			case '\n':
				if(nlfl) {
					move(temp,state[++i]);
					st[i] = &state[i];
				} else {
					move(temp,def[i]);
					df[i] = &def[i];
					nlfl = 1;
				}
				continue;
			case ' ':
				if(nlfl) {
					move(temp,state[++i]);
					st[i] = &state[i];
					nlfl = 0;
				} else {
					error(7);
				}
				continue;
		}
	}


	i = 128;
	while(--i) {
		class[i][0] = i;
		class[i][1] = '\0';
		cl[i] = &class[i];
	}
	cl[0] = &class[0];


	bfl = nlfl = 0;
	t1 = 0;
	t2 = -1;
	while(c = getchar()) {

		switch(c) {

			default:
				if(t1 >= NUMA)	error(4);
				bfl = nlfl = 0;
				act[t1][++t2<8?t2:7] = c;
				continue;

			case '\n':
				if(nlfl)	break;
				nlfl = 1;

			case ' ':
				if(bfl)	continue;
				bfl = 1;
				act[t1][++t2<8?t2:7] = '\0';
				ac[t1] = &act[t1];
				t1++;
				t2 = -1;
				continue;
		}
	break;
	}
	if(c == '\0')	exit();

	i = -1;
	while(++i < NUMS) {
		if(df[i]) {
			cd = find(ac,df[i],NUMA);
			j = -1;
			while(++j < NUMC)
				tab[i].cl[j] = cd;
		}
	}


	fl = 0;
	i = -1;
	while(c = getchar()) {

		switch(c) {

			case '\\':
				temp[++i] = getchar();
				continue;

			case '\n':
				if(fl != 1)	continue;

			default:
				temp[++i] = c;
				continue;

			case '/':
				temp[++i] = '\0';
				i = -1;
				switch(fl) {
					case 0:
						cs = find(st,temp,NUMS);
						fl = 1;
						continue;

					case 1:
						cc = find(cl,temp,NUMC);
						fl = 2;
						continue;

					default:
						error(1);

				}

			case ';':

				if(fl != 2)	error(2);

				temp[++i] = '\0';
				i = -1;
				ca = find(ac,temp,NUMA);

/*printf("%o %o %o\n",cs,cc,ca); /*DEBUG*/
				tab[cs].cl[cc] = ca;

				fl = 0;
				continue;


		}
	}

	i = -1;
	while(++i < NUMS)
		write(fo,tab[i].cl,256);

}

error(a)
{

	printf("Error %d\n",a);
	exit();

}

find(a,b,c)	char	(*a[])[];
		char	b[];
		int	c;
{
	int	i,j;

/*	printf("%s\n",b); /*DEBUG*/
	i = -1;
	while(++i < c) {
/*	printf("	%s\n",a[i]); /*DEBUG*/
		j = 0;
/*	printf("b = %c\ta = %c\n",b[0],(*a[i])[0]); /*DEBUG*/
		while(b[j] == (*a[i])[j]) {
			if(b[j] == '\0')	goto found;
			j++;
		}
	}
found:
	return(i);
}

rword() {

	char	c;
	int	ct;

	ct = -1;
	while(c = getchar()) {
		switch(c) {

			default:
				temp[++ct] = c;
				continue;

			case '\n':
				if(ct == -1)	return('\0');
			case ' ':
				temp[++ct] = '\0';
				return(c);
		}
	}
}

move(a,b)	char	*a;
		char	*b;
{
	while((*b++ = *a++) != '\0');
	return;
}

