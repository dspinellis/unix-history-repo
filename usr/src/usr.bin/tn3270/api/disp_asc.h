/*
 * Define the translate tables used to go between 3270 display code
 * and ascii
 */

extern unsigned char
	disp_asc[256],		/* Goes between display code and ascii */
	asc_disp[256];		/* Goes between ascii and display code */
