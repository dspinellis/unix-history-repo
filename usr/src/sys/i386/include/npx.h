/*
 * 287/387 Floating Point Data Structures and Constants
 * W. Jolitz 1/90
 */

#ifndef	___FPU87___
#define	___FPU87___

/* Environment information of floating point unit */
struct	env87 {
	long	en_cw;		/* control word (16bits) */
	long	en_sw;		/* status word (16bits) */
	long	en_tw;		/* tag word (16bits) */
	long	en_fip;		/* floating point instruction pointer */
	u_short	en_fcs;		/* floating code segment selector */
	u_short	en_opcode;	/* opcode last executed (11 bits ) */
	long	en_foo;		/* floating operand offset */
	long	en_fos;		/* floating operand segment selector */
};

/* Contents of each floating point accumulator */
struct	fpacc87 {
	u_long	fp_mantlo;	/* mantissa low (31:0) */
	u_long	fp_manthi;	/* mantissa high (63:32) */
	int	fp_exp:15;	/* exponent */
	int	fp_sgn:1;	/* mantissa sign */
};

/* Floating point context */
struct	save87 {
	struct	env87 sv_env;		/* floating point control/status */
	struct	fpacc87	sv_ac[8];	/* accumulator contents, 0-7 */
};

#endif	___FPU87___
