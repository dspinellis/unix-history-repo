/*
biorhythm predictions
	biorhythm mmddyy [y1] [y2]
	jeff schriebman		3-75
*/

extern fout;

char *bptr;

int month,day,year,nday;

char *mon[] {
	"jan",
	"feb",
	"mar",
	"apr",
	"may",
	"jun",
	"jul",
	"aug",
	"sep",
	"oct",
	"nov",
	"dec"
};

int dy[] {
	31,28,31,30,31,30,
	31,31,30,31,30,31
};

main(argc,argv)
char **argv;
{
	register char ph,em,in;
	int i,ystart,yfin;

	if (argc < 2) {
		printf("bior mmddyy [yy] [yy]\n");
		exit();
	}
	fout = dup(1);
	printf("Biorhythm transition predictions\n\n");
	printf("ph em in  date\n");
	nday = -1;
	bptr = *++argv;
	month = gettwo();
	day = gettwo();
	day =- 1;
	year = gettwo();
	year =+ 1900;
	if (*bptr!='\0') {
		printf("\n\nIllegal input data. Please enter ");
		printf("mmddyy of birth\n");
		exit();
	}
	ystart = 0;
	if (argc>=3) {
		bptr = *++argv;
		ystart = gettwo();
		ystart =+ 1900;
	}
	yfin = 30000;
	if (argc>=4) {
		bptr = *++argv;
		yfin = gettwo();
		yfin =+ 1900;
	}
	for (i=year;year<i+100;) {
		update();
		if (year<ystart)
			continue;
		if (year>yfin)
			exit();
		ph = ' ';
		em = ' ';
		in = ' ';
		if(lrem(0,nday,23)==11)   ph = '-';
		if(lrem(0,nday,23)==0)   ph = '+';
		if(lrem(0,nday,28)==14)   em = '-';
		if(lrem(0,nday,28)==0)   em = '+';
		if(lrem(0,nday,33)==16)   in = '-';
		if(lrem(0,nday,33)==0)   in = '+';
		if (ph==' ' && em==' ' && in==' ')
			continue;
printf("%c  %c  %c  %s %2d %d\n",ph,em,in,mon[month-1],day,year);
	}
	flush();
}

update()
{
	nday =+ 1;
	day =+ 1;
	if(day>dy[month-1]) {
		if(month==2 && lrem(0,year,4)==0 && day==29)
			return;
		day = 1;
		month =+ 1;
		if (month>12) {
			month = 1;
			year =+ 1;
		}
	}
}

gettwo()
{
	register int num;

	num = *bptr++ - '0';
	num = num*10 + *bptr++ - '0';
	return(num);
}
