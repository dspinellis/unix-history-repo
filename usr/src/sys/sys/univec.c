# include "../h/param.h"
/* controller no.'s for bits 27-31 of ISR addr */
# define DEV_1  0x08000000
# define DEV_2  0x10000000
/* Interrupt Service Routine (ISR) addresses */
extern ubastray() ;
extern  dzrint() , dzxint() ;
 
int *UNIvec[BSIZE/NBPW] = {
/* 0x0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x10 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x20 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x30 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x40 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x50 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x60 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x70 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x80 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x90 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0xa0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0xb0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0xc0 */
	(int *)dzrint,	/* DZ-11 # 0 */
	(int *)dzxint,
	(int *)((int)dzrint+DEV_1),	/*  DZ-11  # 1 */
	(int *)((int)dzxint+DEV_1),
/* 0xd0 */
	(int *)ubastray, /* DR-11B, VAX-11/45 link */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0xe0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0xf0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x100 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x110 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x120 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x130 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x140 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x150 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x160 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x170 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x180 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x190 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x1a0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x1b0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x1c0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x1d0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x1e0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
/* 0x1f0 */
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	(int *)ubastray,
	} ;
 
ubastray()
{
printf("stray UBA interrupt\n") ;
}
