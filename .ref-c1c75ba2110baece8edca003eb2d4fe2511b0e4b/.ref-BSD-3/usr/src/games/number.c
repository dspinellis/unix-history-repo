int	flag;
int max = 21;
char	*card[] =
{
	"hundred",
	"thousand",
	"million",
	"billion",
	"trillion",
	"quadrillion",
	"quintillion",
	"sextillion",
	"septillion",
	"octillion",
	"nonillion",
	"decillion",
	"undecillion",
	"duodecillion",
	"tredecillion",
	"quattuordecillion",
	"quindecillion",
	"sexdecillion",
	"septendecillion",
	"octodecillion",
	"novemdecillion",
	"vigintillion"
};
char *unit[] = {
	"zero",
	"one",
	"two",
	"three",
	"four",
	"five",
	"six",
	"seven",
	"eight",
	"nine"
};
char *teen[] = {
	"ten",
	"eleven",
	"twelve",
	"thirteen",
	"fourteen",
	"fifteen",
	"sixteen",
	"seventeen",
	"eighteen",
	"nineteen"
};
char *decade[] = {
	"zero",
	"ten",
	"twenty",
	"thirty",
	"forty",
	"fifty",
	"sixty",
	"seventy",
	"eighty",
	"ninety"
};
char	line[100];
main()
{
	register c, i, fraction;
	int r;


	fraction = 0;
	while(c = getchar()) {
		if(!digit(c))  {
			fraction = (c == '.');
			putchar(c);
			continue;
		}
		if(fraction) {
			while(digit(c)) {
				putchar(' ');
				putchar(c);
				if(!(c=getchar()))
					exit(1);
			}
			putchar(' ');
			goto out;
		}

		putchar(' ');
		i = 0;
		line[i++] = '0';
		line[i++] = '0';
		while(c == '0')
			if(!(c=getchar()))
				exit(1);
		while(digit(c)) {
			if(i < 98)
				line[i++] = c;
			if(!(c=getchar()))
				exit(1);
		}
		line[i] = 0;
		r = i/3;
		if(r == 0) {
			print("zero");
			goto out;
		}
		conv(line+i-3*r, r);

out:
		fraction = (c == '.');
		nline();
		printf("...\n");
		if(c != '\n')
			putchar(c);
	}
}

conv(p, c)
char *p;
{

	if(c > max) {
		conv(p, c-max);
		print(card[max]);
		nline();
		p += (c-max)*3;
		c = max;
	}
	while(c > 1) {
		c--;
		conv(p, 1);
		cprint(card[c]);
		nline();
		p += 3;
	}
	ones(p[0]);
	cprint(card[0]);
	tens(p);
	ones(p[2]);
}

ones(d)
{
	if(d=='0')
		return;
	print(unit[d-'0']);
}

tens(p)
char *p;
{

	switch(p[1]) {

	case '0':
		return;

	case '1':
		print(teen[p[2]-'0']);
		p[2] = '0';
		return;
	}

	print(decade[p[1]-'0']);
}


digit(c)
{

	if(c < '0' || c > '9')
		return(0);
	return(1);
}

nline()
{

	if(flag)
		printf(".\n");
	flag = 0;
}

cprint(s)
{

	if(flag)
		print(s);
}

print(s)
{

	if(flag)
		printf(" ");
	printf(s);
	flag = 1;
}
