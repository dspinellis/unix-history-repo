/*	drtest.c	4.1	82/12/26	*/

/*
 * Standalone program to test a disk driver by reading
 * every sector on the disk in chunks of CHUNK.
 */
extern	struct hpst  {
	short nsect;
	short ntrak;
	short nspc;
	short ncyl;
	short *off;
} hpst[] ;

extern	struct upst  {
	short nsect;
	short ntrak;
	short nspc;
	short ncyl;
	short *off;
} upst[] ;
extern struct updevice;
extern struct hpdevice;
extern char up_type[];
extern char hp_type[];

#define CHUNK 32

main()
{
	char buf[50], buffer[CHUNK*512];
	int unit,fd,chunk,j;
	register struct upst *st;
	register i;

	printf("Testprogram for stand-alone hp or up driver\n");
askunit:
	printf("Enter device name (e.g, hp(0,0) ) or q to quit >");
	gets(buf);
	unit = (int)*(buf+3) - '0';
	if (unit <0 || unit > 3 ) {
		printf("unit number out of range\n");
		goto askunit;
	}
	if ((fd=open(buf,0)) < 0) {
		printf("Can't open %s \n",buf);
		goto askunit;
	}
	switch(*buf) {

	case 'u':
		st = &upst[up_type[unit]];
		break;

	case 'h':
		st = (struct upst *)&hpst[hp_type[unit]];
		break;

	default:
		printf("Illegal device name\n");
		goto askunit;
	}

	chunk = st->nsect;
	printf("Testing %s\n",buf);
	printf("Start ...Make sure %s is online\n",buf);
	lseek(fd,0,0);
	for (i=0;i < st->ncyl;i++) {
		for (j=8;j<st->ntrak+8;j++) {
			lseek(fd,(i*st->nspc+((j%st->ntrak)*st->nsect))*512,0);
			read(fd,buffer, chunk*512);
		}
		printf("%d\015",i);
	}
	goto askunit;
}
