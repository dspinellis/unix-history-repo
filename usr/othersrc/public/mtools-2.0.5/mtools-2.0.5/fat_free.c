#include <stdio.h>
#include "msdos.h"

extern int fat_error;
extern unsigned int last_fat;

/*
 * Remove a string of FAT entries (delete the file).  The argument is
 * the beginning of the string.  Does not consider the file length, so
 * if FAT is corrupted, watch out!
 */

int
fat_free(fat)
unsigned int fat;
{
	unsigned int next, fat_decode();
					/* a zero length file? */
	if (fat == 0)
		return(0);

	/* CONSTCOND */
	while (1) {
					/* get next cluster number */
		next = fat_decode(fat);
					/* mark current cluster as empty */
		if (fat_encode(fat, 0) || next == 1) {
			fprintf(stderr, "fat_free: FAT problem\n");
			fat_error++;
			return(-1);
		}
		if (next >= last_fat)
			break;
		fat = next;
	}
	return(0);
}
