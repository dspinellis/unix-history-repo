char	dayw[]
{
	" S  M Tu  W Th  F  S"
};
char	*smon[]
{
	"Jan", "Feb", "Mar", "Apr",
	"May", "Jun", "Jul", "Aug",
	"Sep", "Oct", "Nov", "Dec",
};
char	string[432];
main(argc, argv)
char *argv[];
{
	register y, i, j;
	int m;

	if(argc < 2) {
		printf("usage: cal [month] year\n");
		exit();
	}
	if(argc == 2)
		goto xlong;

/*
 *	print out just month
 */

	m = number(argv[1]);
	if(m<1 || m>12)
		goto badarg;
	y = number(argv[2]);
	if(y<1 || y>9999)
		goto badarg;
	printf("      %s %l\n", smon[m-1], y);
	printf("%s\n", dayw);
	cal(m, y, string, 24);
	for(i=0; i<6*24; i=+24)
		pstr(string+i, 24);
	exit();

/*
 *	print out complete year
 */

xlong:
	y = number(argv[1]);
	if(y<1 || y>9999)
		goto badarg;
	printf("\n\n\n");
	printf("				%l\n", y);
	printf("\n");
	for(i=0; i<12; i=+3) {
		for(j=0; j<6*72; j++)
			string[j] = '\0';
		printf("	 %s", smon[i]);
		printf("			%s", smon[i+1]);
		printf("		       %s\n", smon[i+2]);
		printf("%s   %s   %s\n", dayw, dayw, dayw);
		cal(i+1, y, string, 72);
		cal(i+2, y, string+23, 72);
		cal(i+3, y, string+46, 72);
		for(j=0; j<6*72; j=+72)
			pstr(string+j, 72);
	}
	printf("\n\n\n");
	exit();

badarg:
	printf("Bad argument\n");
}

number(str)
char *str;
{
	register n, c;
	register char *s;

	n = 0;
	s = str;
	while(c = *s++) {
		if(c<'0' || c>'9')
			return(0);
		n = n*10 + c-'0';
	}
	return(n);
}

pstr(str, n)
char *str;
{
	register i;
	register char *s;

	s = str;
	i = n;
	while(i--)
		if(*s++ == '\0')
			s[-1] = ' ';
	i = n+1;
	while(i--)
		if(*--s != ' ')
			break;
	s[1] = '\0';
	printf("%s\n", str);
}

char	mon[]
{
	0,
	31, 29, 31, 30,
	31, 30, 31, 31,
	30, 31, 30, 31,
};

cal(m, y, p, w)
char *p;
{
	register d, i;
	register char *s;

	s = p;
	d = jan1(y);
	mon[2] = 29;
	mon[9] = 30;

	switch((jan1(y+1)+7-d)%7) {

	/*
	 *	non-leap year
	 */
	case 1:
		mon[2] = 28;
		break;

	/*
	 *	1752
	 */
	default:
		mon[9] = 19;
		break;

	/*
	 *	leap year
	 */
	case 2:
		;
	}
	for(i=1; i<m; i++)
		d =+ mon[i];
	d =% 7;
	s =+ 3*d;
	for(i=1; i<=mon[m]; i++) {
		if(i==3 & mon[m]==19) {
			i =+ 11;
			mon[m] =+ 11;
		}
		if(i > 9)
			*s = i/10+'0';
		s++;
		*s++ = i%10+'0';
		s++;
		if(++d == 7) {
			d = 0;
			s = p+w;
			p = s;
		}
	}
}

/*
 *	return day of the week
 *	of jan 1 of given year
 */

jan1(yr)
{
	register y, d;

/*
 *	normal gregorian calendar
 *	one extra day per four years
 */

	y = yr;
	d = 4+y+(y+3)/4;

/*
 *	julian calendar
 *	regular gregorian
 *	less three days per 400
 */

	if(y > 1800) {
		d =- (y-1701)/100;
		d =+ (y-1601)/400;
	}

/*
 *	great calendar changeover instant
 */

	if(y > 1752)
		d =+ 3;

	return(d%7);
}
