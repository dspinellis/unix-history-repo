int floppydes;
char *flopname = "/dev/floppy";

main(argc,argv)
char *argv[];
{
	static char buff[512];
	register count = 77 * 26 * 128, startad = -26 * 128;
	register int n, file;

	if(argc==2) {
		printf("Halftime!\n");
		if(strcmp(argv[1],"-h")!=0)
			printf("Bad halftime option.\n"),
			exit(1);
		if((file = open("floppy",0))<0)
			printf("failed to open floppy image"),
			exit(1);
		goto halftime;
	}
	file = creat("floppy",0666);
	close(file);
	file = open("floppy",2);
	if(file < 0) exit(1);
	for( ; count > 0 ; count -= 512) {
		n = count > 512 ? 512 : count ;
		lread(startad,n,buff);
		write(file,buff,n);
		startad += 512;
	}
halftime:
	printf("Change Floppy, Hit return when done.\n");
	gets(buff);
	lseek(file,0,0);
	count = 77 * 26 * 128; startad = -26 * 128;
	for( ; count > 0 ; count -= 512) {
		n = count > 512 ? 512 : count ;
		read(file,buff,n);
		lwrite(startad,n,buff);
		startad += 512;
	}
}
rt_init()
{
	static initized = 0;
	int mode = 2;

	if(initized) return;
	initized = 1;
	if((floppydes = open(flopname,mode)) < 0) {
		printf("Floppy open failed\n");
		exit(1);
	}
}
		
long trans(logical)
register int logical;
{
	/*  Logical to physical adress translation */
	register int sector, bytes, track;

	logical += 26 * 128;
	bytes = (logical & 127);
	logical >>= 7;
	sector = logical % 26;
	if(sector >= 13)
		sector = sector *2 +1;
	else
		sector *= 2;
	sector += 26 + ((track = (logical / 26)) - 1) * 6;
	sector %= 26;
	return( (((track *26) + sector) << 7) + bytes);
}
lread(startad,count,obuff)
register startad, count;
register char * obuff;
{
	long trans();
	extern floppydes;
	rt_init();
		while( (count -= 128) >= 0) {
			lseek(floppydes, trans(startad), 0);
			read(floppydes,obuff,128);
			obuff += 128;
			startad += 128;
		}
}
lwrite(startad,count,obuff)
register startad, count;
register char * obuff;
{
	long trans();
	extern floppydes;
	rt_init();
		while( (count -= 128) >= 0) {
			lseek(floppydes, trans(startad), 0);
			write(floppydes,obuff,128);
			obuff += 128;
			startad += 128;
		}
}
