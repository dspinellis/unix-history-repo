/*
 * callg_ - call with 'general' argument list for CCI `tahoe'
 *
 * by P. S. Housel 04/30/86
 *
 */

callg_(funct, arglist)
register int (*funct)();		/* r12 */
register int *arglist;			/* r11 */
{
 register int *argptr, n;		/* r10, r9 */

 n = (*arglist + 1) * 4;

 argptr = arglist + *arglist;

 while(argptr > arglist)
      {asm("pushl	(r10)");
       argptr--;
      }
 
 asm("	calls	r9,(r12)");
}
