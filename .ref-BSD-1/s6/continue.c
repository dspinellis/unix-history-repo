#
#define DELAY	60
/*
 * continue - continue at a specified date and time
 */

char *ptr;
int times[8];

main(argc, argv)
char **argv;
{
	register int i;
	register char *aptr;
	int tvec[2], *iptr;

	if (argc != 2) {
		printf("CONTINUE MMDDHHMM[YY]\n");
		exit();
	}
	ptr = *++argv;
	range(times[4]=next(), 1, 12);
	times[4]--;
	range(times[3]=next(), 0, 31);
	range(times[2]=next(), 0, 23);
	range(times[1]=next(), 0, 59);
	if (*ptr != '\0')
		range(times[5]=next(), 70, 99);
	else {
		time(tvec);
		iptr = localtime(tvec);
		times[5] = iptr[5];
	}
	if (*ptr != '\0') {
		printf("Illegal time. Enter as MMDDHHMM[YY]\n");
		exit();
	}
	while ((i=checktime()) > 0)
		sleep(i);
}

next()
{
	register int num;

	num = *ptr++ - '0';
	num = num*10 + *ptr++ - '0';
	return(num);
}

range(n1, n2, n3)
{
	if (n1>=n2 && n1<=n3)
		return;
	printf("Illegal number %d\n", n1);
	exit();
}

checktime()
{
	register i, j;
	int tvec[2], *iptr;

	time(tvec);
	iptr = localtime(tvec);
	for (i=0; i<5; i++) {
		j = 5 - i;
		if (iptr[j] > times[j])
			return(0);
		if (iptr[j] < times[j])
			return(DELAY);
	}
	return(0);
}
