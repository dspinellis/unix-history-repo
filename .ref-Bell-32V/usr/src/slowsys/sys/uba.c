# include "../h/param.h"
#include "../h/uba.h"
# include "../h/map.h"
 
int uballoc(baddr,bcnt,bdpflg)
char  bdpflg ;
char * baddr ;
unsigned short bcnt;
{
/*
*  Allocate as many contiguous UBA mapping registers
*  as are necessary to do transfer of 'bcnt' bytes
*  to/from location 'baddr'.
*  Wait for enough map registers.
*  'bdpflg' is non-zero if a "buffered data path" (BDP) is
*  to be used, else 0 -> use direct data path (DDP) .
*  Return
*
*    |31 - - 28|27 - - 18|17 - - - 9|8 - - 0|
*    |         |         |          |       |
*    |  BDP    |   no.   |  start   | byte  |
*    |   no.   | mapping |   map    | offset|
*    |         |  reg's  | reg. no. |       |
*    |         |         |          |       |
*
*/
 
register regnum , nmreg , bdp , pfn , j ;
 
/* calculate no. of mapping reg's required */
nmreg = btoc(bcnt) + 2;
pfn = ((int)baddr>>9) & 0xfff ; /* start page frame no. */
 
spl6() ;
while ((regnum = malloc(ubamap,nmreg) - 1) < 0) {
	/* wait for no. of mapping reg's requested */
	umrwant++ ;
	sleep(ubamap,PSWP) ;
	}
if (bdpflg)  /* buffered data path BDP 1-15 */
	while ( (bdp=malloc(bdpmap,1)) == NULL)
		{
		bdpwant++;
		sleep(bdpmap, PSWP);
		}
else { /* BDP 0 = DDP */
	bdp = 0 ;
	}
 
spl0() ;
 
j = (bdp<<28) | (nmreg<<18) | (regnum<<9) | ((int)baddr & 0x01ff) ;
pfn |= (MRV | (bdp<<21)) ; /* map reg entry */
 
if (bdp && ((int)baddr & 01)) pfn |= BO ; /* byte offset */
while (--nmreg)  /* fill the memory mapping reg's */
	((struct uba_regs *)UBA0)->uba_map[regnum++] = pfn++ ;
((struct uba_regs *)UBA0)->uba_map[regnum] = 0 ; /* last entry is invalid */
 
return(j) ;
}
 
/*							*/
 
ubafree(mr)
int mr ;
{
/*
*  Free UBA memory mapping reg's and a BDP no..
*    mr :
*		bits 0 - 3 :  bdp no.
*	    	 4 - 15 : start map reg. no.
*			16 - 31 : no. of mapping reg's
*
*/
 
register bdp , nmreg , regnum ;
 
spl6();
bdp = (mr>>28) & 0x0f ;  /* BDP no. */
if (bdp)
	{
	((struct uba_regs *)UBA0)->uba_dpr[bdp] |= BNE ; /* purge */
	mfree(bdpmap, 1, bdp);
	if (bdpwant) {
		bdpwant = 0;
		wakeup(bdpmap);
	}
}

nmreg = (mr>>18) & 0x3ff ; /* no. of mapping reg's */
regnum = (mr>>9) & 0x1ff ; /* 1st map reg. no. */
 
/* free mapping reg's */
mfree(ubamap,nmreg,regnum+1) ;
if (umrwant) {
	umrwant = 0;
	wakeup(ubamap);
}
spl0() ;
}

ubainit()
{
	mfree(ubamap, 496, 1);
	mfree(bdpmap, 15, 1);
}
