#ifndef lint
static char sccsid[] = "@(#)profile.c	1.3 (Berkeley/CCI) %G%";
#endif


#include	"vdfmt.h"

#define	cycles	10

/*
**
*/

profile()
{
	unsigned int	total_time, i, step, remainder;
	dskadr		ead, zero;
	char		digit_buf[20];

	print("Disk seek profile for ");
	printf("controller %d, drive %d, ", cur.controller, cur.drive);
	printf("type %s.\n", lab->d_typename);

	indent();
	if(is_formatted() == false) {
		print("Can not profile unformatted drives!\n");
		_longjmp(abort_environ, 1);
	}
	print(" Seek  |                  Seek time (ms)\n");
	print("Length |0    5    10   15   20   25   30   35   40   45   50\n");
	print("-------|-----+----+----+----+----+----+----+----+----+----+\n");

	cur.state = prof;
	zero.cylinder = zero.track = zero.sector=0;
	ead.track = ead.sector=0;
	step = lab->d_ncylinders / 55;
	for(ead.cylinder=1; ead.cylinder<lab->d_ncylinders; ead.cylinder+=step){
		total_time = 0;
		for(i=0; i<cycles; i++) {
			access_dsk((char *)save, &zero, VDOP_SEEK, 1, 60);
			access_dsk((char *)save, &ead, VDOP_SEEK, 1, 60);
			if(kill_processes == true)
				_longjmp(quit_environ, 1);
			total_time += ((2*60*1000*1000) - vdtimeout);
		}
		print("");
		sprintf(digit_buf, "%d      ", ead.cylinder);
		for(i=0; i<7; i++)
			putchar(digit_buf[i]);
		putchar('|');	
		total_time /= cycles;
		remainder = total_time % 10;
		total_time /= 10;
		while(total_time--)
			putchar(' ');
		if(remainder >= 5)
			printf("+\n");
		else
			printf("*\n");
		DELAY(400000);
	}
	print("-------|-----+----+----+----+----+----+----+----+----+----+\n");
	print("       |0    5    10   15   20   25   30   35   40   45   50\n");
	exdent(1);
	printf("Profile completed successfully.\n");
}

