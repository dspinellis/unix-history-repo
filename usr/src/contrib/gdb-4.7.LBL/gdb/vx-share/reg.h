#ifndef _REG_
#define _REG_

#ifdef I80960

/* Intel 960 register values passed over the wire by RPC:  */

struct regs
{
  int r_lreg[16];		/* local registers              */
  int r_greg[16];		/* global registers             */
  int r_pcw;			/* process control word         */
  int r_acw;			/* arithmetic control word      */
  int r_tcw;			/* trace control word           */
};

#define FP_REG_SIZE	12

struct fp_status {
	char	fps_regs[4][FP_REG_SIZE];	/* floating point regs */
};

#else  /* For now, just 68000 */

/* THE 68000 VERSION OF THIS FILE WAS `BORROWED' FROM A COPYRIGHTED
   SUN MICROSYSTEMS INCLUDE FILE.  IT NEEDS TO BE REBUILT FROM SCRATCH.

	John Gilmore
	Cygnus Support
 */
FIXME
   
#endif /* !I80960 */
#endif !_REG_
