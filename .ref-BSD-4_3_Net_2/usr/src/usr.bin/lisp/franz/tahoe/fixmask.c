/*
** file: fixmask.c
** new & improved version by P. S. Housel 04/27/86
**
** note: "changing register save masks" involves making sure r6 and r7
**	 are saved for use as "np" and "lbot"
*/

/*
 * 	fixmask.c
 * complete program to change register save masks on the CCI "tahoe"
 *
 * (c) copyright 1982, Regents of the University of California
 */

#include <stdio.h>

char mybuf[BUFSIZ];
int mask;

main()
{
 register savesize = 0;
 char *cp;

 while(fgets(mybuf,BUFSIZ,stdin) != NULL)
      {
       if(*mybuf=='#')
	  if(strcmpn(mybuf,"#protect", 8)==0)
	    {
	     savesize = 1;
	    }

       if(savesize && strcmpn(mybuf,"	.set	L",7)==0)
         {
	  for(cp=mybuf;*cp++!=',';) ;
	  sscanf(cp, "0x%x", &mask);
	  sprintf(cp,"0x%X\n", mask | 0x0C0);
	  savesize = 0;
	 }

       fputs(mybuf,stdout);
      }
}
