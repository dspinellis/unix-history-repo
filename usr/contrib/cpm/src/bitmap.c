/*	bitmap.c	1.9	83/05/13	*/

#include <stdio.h>
#include "cpmio.h"
int	bm_size;

/*
 * Bit map handling routines;
 * - build the disk allocation bit map
 * - allocate a new block
 * - dump the bitmap in hex to stdout (debugging)
 * - count the number of blocks in use
 * Note: the first block is number zero and the
 * 	 directory always occupies the first few blocks,
 * 	 depending on the disk format. For a standard
 * 	 SSSD disk the blocks are numbered 0 thru 243 and
 * 	 the directory occupies blocks 0 and 1.
 */

/* 
 * Allocate a new disk block, return NULL if disk full 
 */
char
alloc()

{

	int i, j, blk;

	for (i=0; i<bm_size; i++) {
		j = ffc(0, INTSIZE, *(bitmap+i));
		if ( j < INTSIZE) break;
	}
	blk = i * INTSIZE + j;
	if (blk >= (seclth*sectrk*(tracks-2))/blksiz) 
		return ('\0');
	*(bitmap+i) |= (1 << j); /* set the appropriate bit in the bitmap */
#ifdef DEBUG
	printf("block number allocated: %d (0x%x)\n", blk, blk);
	dbmap("new bitmap:"); 
#endif
	return ((char) blk);
}


/* 
 * Dump the bitmap in hex to stdout, used only for debugging 
 */

dbmap(str)
	char *str;
{
#ifdef DEBUG

	int	i;

	printf("%s\n",str);
	for (i=0; i<bm_size; i++)
		printf("%.8x\n", *(bitmap+i));
#endif
}

/* 
 * Return the number of bloks used in the
 * directory, including the directory blocks
 */

blks_used()
{

	int j, i, temp;
	int buse = 0;

	for (i=0; i < bm_size; i++) {
		if (*(bitmap+i) == 0) 
			continue;
		if (*(bitmap+i) == -1) {
			buse += INTSIZE;
		} else  {
			temp = *(bitmap+i);
			for (j=0; j < INTSIZE; j++) {
				if (1 & temp) 
					++buse;
				temp >>= 1;
			}
		}
	}
	return (buse);
}


build_bmap()
{

	int	i, j, *malloc(), offset, block;

	bm_size = 1 + ((seclth*sectrk*(tracks-2))/blksiz)/INTSIZE;

	if (!bitmap) {
		if ((bitmap = malloc(bm_size*4)) == NULL) {
			printf("can't allocate memory for bitmap\n");
			exit(1);
		}
	}
	/*
	 * clear bitmap
	 */
	for (i=0; i<bm_size; i++)
		bitmap[i] = 0;

	/* i equals the number of blocks occupied by the directory */
	i = (maxdir*32)/blksiz; 

	/* set the directory blocks busy in the bitmap */
	*bitmap = (1 << i) -1;  
	for (i=0; i<maxdir; i++) {
		if ((dirbuf+i)->status != (char) 0xe5)	{
#ifdef DEBUG
		      printf("%d ->%8s\n", i, (dirbuf+i)->name);  
#endif
		      for (j=0; (j<16)&&((dirbuf+i)->pointers[j] != '\0'); j++){
			   block = 0xff & (int)(dirbuf+i)->pointers[j];
			   offset = block / INTSIZE;
#ifdef DEBUG
		printf("blk:%d, offs:%d, bit:%d\n", block,offset,block%INTSIZE);
#endif
			  *(bitmap+offset) |= (1 << block % INTSIZE);
		      }
		}
	}
#ifdef DEBUG
	dbmap("initial bit map:"); 
#endif
}
