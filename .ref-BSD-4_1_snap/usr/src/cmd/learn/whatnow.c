#include "stdio.h"
#include "lrnref"

whatnow()
{
	if (todo == 0) {
		more=0;
		return;
	}
	if (didok) {
		strcpy(level,todo);
		if (speed<=9) speed++;
	}
	else {
		speed -= 4;
		/* the 4 above means that 4 right, one wrong leave
		    you with the same speed. */
		if (speed <0) speed=0;
	}
	if (wrong) {
		speed -= 2;
		if (speed <0 ) speed = 0;
	}
	if (didok && more) {
		printf("\nGood.  Lesson %s (%d)\n\n",level, speed);
		fflush(stdout);
	}
}
