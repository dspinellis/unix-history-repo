/*
 *
 *	Routines to deal with the VMS User Authorization File:
 *
 *	get_vms_uaf_record(name,uaf);
 *	validate_vms_user(name,password,uaf);
 *	hash_vms_password(output_buf,input_buf,input_len,username,type,salt);
 *
 *
 */


/*
 * Includes
 */
#include <vms/fab.h>			/* File access block */
#include <vms/rab.h>			/* Record access block */
#include <vms/rmsdef.h>			/* RMS return codes */
#include <vms/ssdef.h>			/* System service return codes */
#include <vms/uafdef.h>			/* Authorization file records */
#include <vms/logdef.h>			/* Logical name table seach masks */

/*
 * defines
 */
#define	RETRY_RLK	2		/* number of retries if record locked */
#define	SLEEP_RLK	75		/* MS to sleep before retrying a GET */
#define	RECSIZ		80		/* Maximum length for password string */
#define	UAFNAME	"SYSUAF"		/* Name of authorization file */
#define	UAFSIZE 6			/* Size of above */
#define	DEFNAME	"SYS$SYSTEM:.DAT"	/* Default path to authorization file */
#define	DEFSIZE	15			/* Size of above */
#define	DEFUSER	"DEFAULT     "		/* "Default user" name */

#define	UAF$_NORMAL	1		/* Normal completion */
#define	UAF$_INVUSR	-2		/* Invalid User Name */
#define	UAF$_INVPWD	-4		/* Invalid Password  */

#define	UAF$S_USERNAME	12		/* User Name Size */
#define	UAF$S_PWD	8		/* Password Size */

struct descr {int size; char *ptr;};	/* VMS descriptor		*/

/*
 * OWN STORAGE:
 */
static int wakedelta[] = {-10*1000*SLEEP_RLK,-1};


/*
 *
 *	status = get_vms_uaf_record(name,uaf);
 *
 */
int get_vms_uaf_record(name,uaf)
char *name;
register struct uaf *uaf;
{
	struct FAB fab;
	struct RAB rab;
	unsigned int old_privs[2],new_privs[2];
	int status;
	int default_user = 1;
	register int i;
	register char *cp,*cp1,*cp2;

	/*
	 *	Zero the fab and rab
	 */
	bzero(&fab,sizeof(fab));
	bzero(&rab,sizeof(rab));
	/*
	 *	Setup the fab
	 */
	fab.fab$b_bid = FAB$C_BID;
	fab.fab$b_bln = sizeof(fab);
	fab.fab$l_fna = UAFNAME;
	fab.fab$b_fns = UAFSIZE;
	fab.fab$l_dna = DEFNAME;
	fab.fab$b_dns = DEFSIZE;
	fab.fab$b_dsbmsk = (1<<LOG$C_GROUP) | (1<<LOG$C_PROCESS);
	fab.fab$b_fac |= FAB$M_GET;
	fab.fab$b_shr = (FAB$M_GET|FAB$M_PUT|FAB$M_UPD|FAB$M_DEL);
	fab.fab$b_org = FAB$C_IDX;
	fab.fab$b_rfm = FAB$C_VAR;
	/*
	 *	setup the rab
	 */
	rab.rab$b_bid = RAB$C_BID;
	rab.rab$b_bln = sizeof(rab);
	rab.rab$b_rac = RAB$C_KEY;
	rab.rab$l_rop |= RAB$M_NLK;
	rab.rab$b_mbc = 10;
	rab.rab$w_usz = sizeof(struct uaf);
	rab.rab$l_ubf = (char *)uaf;
	rab.rab$l_kbfpbf.rab$l_kbf = (unsigned long int *)uaf;
	rab.rab$b_kszpsz.rab$b_ksz = UAF$S_USERNAME;
	rab.rab$l_fab = &fab;
	/*
	 *	Enable all privileges that we are authorized to have just to
	 *	enhance the possibility of accessing the SYSUAF file.
	 */
	new_privs[0] = -1; new_privs[1] = -1;
	sys$setprv(1,new_privs,0,old_privs);
	/*
	 *  Open the File and connect the RAB
	 */
	status = sys$open(&fab);
	if (!(status & 1)) {
		if ((status == RMS$_SNE) ||
		    (status == RMS$_SPE) ||
		    (status == RMS$_DME)) {
			fab.fab$b_shr = 0;
			status = sys$open(&fab);
			if (!(status & 1)) goto exit;
		} else goto exit;
	}
	status = sys$connect(&rab);
	if (!(status & 1)) goto exit;
	/*
	 *	Move the USERNAME to the uaf$t_username field (as a buffer)
	 *	uppercaseify it along the way and check it against the
	 *	username "DEFAULT" (which is not a real username).  Pad
	 *	the uaf$t_username field with blanks.
	 */
	i = UAF$S_USERNAME;
	cp = name;
	cp1 = uaf->uaf$t_username;
	cp2 = DEFUSER;
	while(--i >= 0) {
		if (*cp == 0) break;
		if (*cp != *cp2++) default_user = 0;
		*cp1++ = ((*cp >= 'a') && (*cp <= 'z')) ?
				*cp++ + ('A' - 'a') : *cp++;
	}
	i++;
	while(--i >= 0) {
		*cp1++ = ' ';
		if (*cp2++ != ' ') default_user = 0;
	}
	/*
	 *	The "DEFAULT" user is illegal!
	 */
	if (default_user) {
		status = UAF$_INVUSR;
		goto exit;
	}
	/*
	 *	Look up the User's UAF record
	 */
	status = get_record(&rab);
	if (status == RMS$_RNF) status = UAF$_INVUSR;

exit:
	/*
	 *	Done: close the file, release privileges and return status
	 */
	sys$disconnect(&rab);
	sys$close(&fab);
	sys$setprv(0,new_privs,0,0);
	sys$setprv(1,old_privs,0,0);
	return(status);
}


/*
 *	Read the record and deal with file locking
 */
static get_record(rab)
struct RAB *rab;
{
	int retries = RETRY_RLK;
	int status;

	/*
	 *	Re-try the appropriate number of times
	 */
	while(1) {
		/*
		 *	Get the record
		 */
		status = sys$get(rab);
		/*
		 *	If the return code is not "Record Locked" it is either
		 *	a success or error that we can't handle, return it.
		 */
		if (status != RMS$_RLK) break;
		/*
		 *	Record Locked:  If retries exceeded, return error
		 */
		if (--retries < 0) break;
		/*
		 *	Retry: Sleep first
		 */
		status = sys$schdwk(0,0,wakedelta,0);
		if (status & 1) sys$hiber();
	}
	/*
	 *	Done: Return status
	 */
	return(status);
}


/*
 *
 *	Validate a UserName/Password pair and return the user's UAF record
 *
 */
int validate_vms_user(name,password,uaf)
char *name;
char *password;
register struct uaf *uaf;
{
	char password_buf[RECSIZ];
	char username_buf[UAF$S_USERNAME];
	char encrypt_buf[UAF$S_PWD];
	register int i;
	register char *cp,*cp1;

	/*
	 *	Get the User's UAF record
	 */
	i = get_vms_uaf_record(name,uaf);
	if (!(i & 1)) return(i);
	/*
	 *	Limit the username to "UAF$S_USERNAME" size while copying and
	 *	uppercasifying it.  Pad with spaces to "UAF$S_USERNAME" size.
	 */
	i = UAF$S_USERNAME;
	cp = name;
	cp1 = username_buf;
	while(--i >= 0) {
		if (*cp == 0) break;
		*cp1++ = ((*cp >= 'a') && (*cp <= 'z')) ?
				*cp++ + ('A' - 'a') : *cp++;
	}
	i++;
	while(--i >= 0) *cp1++ = ' ';
	/*
	 *	Limit the password to "RECSIZ" size while copying and
	 *	uppercasifying it.
	 */
	i = RECSIZ;
	cp = password;
	cp1 = password_buf;
	while(--i >= 0) {
		if (*cp == 0) break;
		*cp1++ = ((*cp >= 'a') && (*cp <= 'z')) ?
				*cp++ + ('A' - 'a') : *cp++;
	}
	i = (RECSIZ - 1) - i;	/* Compute size of password string */
	/*
	 *	Encrypt the password
	 */
	hash_vms_password(encrypt_buf,password_buf,i,username_buf,
			  uaf->uaf$b_encrypt,uaf->uaf$w_salt);
	if (bcmp(encrypt_buf,uaf->uaf$l_pwd,UAF$S_PWD) == 0)
		return(UAF$_NORMAL);
	else	return(UAF$_INVPWD);
}


/*
 *
 *	PASSWORD SMASHING CODE:
 *		The real bit hacking is done in "asm" statements, since
 *		"C" is poorly suited towards quad-word arithmetic!
 *
 */

/*
 *	AUTODIN II CRC Coefficients:
 */
static unsigned long autodin[] = {
	000000000000,003555610144,007333420310,004666230254,
	016667040620,015332650764,011554460530,012001270474,
	035556101440,036003711504,032665521750,031330331614,
	023331141260,020664751324,024002561170,027557371034
	};


/*
 *	PURDY Polynomial Coefficients
 */
static long c[] = {
	-83,	-1,		/* C1 */
	-179,	-1,		/* C2 */
	-257,	-1,		/* C3 */
	-323,	-1,		/* C4 */
	-363,	-1		/* C5 */
	};

/*
 *	Hashing routine
 */
hash_vms_password(output_buf,input_buf,input_length,username,type,salt)
char *output_buf,*input_buf,*username;
unsigned short salt;
{
	register int i;
	register char *cp;

	/*
	 *	Dispatch on encryption type
	 */
	if (type == 0) {
		/*
		 *	AUTODIN II CRC:
		 */
		crc(autodin,-1,input_length,input_buf,output_buf);
		return;
	} else {
		/*
		 *	PURDY:
		 */

		i = 8;
		cp = output_buf;
		while(--i >= 0) *cp++ = 0;	/* Init output buffer */
		/*
		 *	Collapse the password into a quadword
		 */
		collapse(input_length,input_buf,output_buf);
		/*
		 *	Add salt to middle of quadword
		 */
		*((unsigned short *)(output_buf+3)) += salt;
		/*
		 *	Collapse the username into the quadword
		 */
		collapse(/*UAF$S_USERNAME*/12,username,output_buf);
		/*
		 *	Compute the PURDY polynomial:
		 */
		purdy(output_buf,c);
	}
}

/*
 *	CRC routine:
 */
static crc(table,initial_crc,input_len,input_buf,output_buf)
{
	asm("	crc	*4(ap),8(ap),12(ap),*16(ap)");
	asm("	clrl	r1");
	asm("	movq	r0,*20(ap)");
}

/*
 *	Routine to collapse a string into a quadword:
 */
static collapse(input_len,input_buf,output_buf)
register unsigned char *input_buf;
register int input_len;
register unsigned char *output_buf;
{
	while(input_len > 0) {
		output_buf[input_len & ~(-8)] += *input_buf++;
		input_len--;
	}
}


/*
 *
 *	GROMMY STUFF TO COMPUTE THE PURDY POLYNOMIAL
 *
 */
static purdy(U,C)
{
/*
 * This routine computes f(U) = p(U) mod P.  Where P is a prime of the form
 * P = 2^64 - a.  The function p is the following polynomial:
 * X^n0 + X^n1*C1 + X^3*C2 + X^2*C3 + X*C4 + C5
 * The input U is an unsigned quadword.
 */
	asm("	.set	A,59");		/* 2^64 -59 is the biggest quad prime */

	asm("	movq	*4(ap),-(sp)");	/* Push U */
	asm("	bsbw	PQMOD_R0");	/* Ensure U less than P */
	asm("	movaq	(sp),r4");	/* Maintain a pointer to X*/
	asm("	pushl	8(ap)");
	asm("	movl	(sp)+,r5");	/* Point to the table of coefficients */
	asm("	movq	(r4),-(sp)");
	asm("	pushl	$((1<<24)-63)");/* n1 */
	asm("	bsbb	PQEXP_R3");	/* X^n1 */
	asm("	movq	(r4),-(sp)");
	asm("	pushl	$((1<<24)-3)");
	asm("	subl2	$((1<<24)-63),(sp)");/* n0-n1 */
	asm("	bsbb	PQEXP_R3");
	asm("	movq	(r5)+,-(sp)");	/* C1 */
	asm("	bsbw	PQADD_R0");	/* X^(n0 - n1) + C1 */
	asm("	bsbw	PQMUL_R2");	/* X^n0 + X^n1*C1 */
	asm("	movq	(r5)+,-(sp)");	/* C2 */
	asm("	movq	(r4),-(sp)");
	asm("	bsbw	PQMUL_R2");	/* X*C2 */
	asm("	movq	(r5)+,-(sp)");	/* C3 */
	asm("	bsbw	PQADD_R0");	/* X*C2 + C3 */
	asm("	movq	(r4),-(sp)");
	asm("	bsbb	PQMUL_R2");	/* X^2*C2 + X*C3 */
	asm("	movq	(r5)+,-(sp)");	/* C4 */
	asm("	bsbw	PQADD_R0");	/* X^2*C2 + X*C3 + C4 */
	asm("	movq	(r4),-(sp)");
	asm("	bsbb	PQMUL_R2");	/* X^3*C2 + X^2*C3 + C4*X */
	asm("	movq	(r5)+,-(sp)");	/* C5 */
	asm("	bsbw	PQADD_R0");	/* X^3*C2 + X^2*C3 + C4*X + C5 */
	asm("	bsbw	PQADD_R0");	/* Add in the high order terms */
	asm("	movq	(sp)+,*4(ap)");	/* Replace U with f(X) */
	asm("	movl	$1,r0");
	asm("	ret");


	/*  Replaces the inputs with U^n mod P where P is of the form */
	/*  P = 2^64 - a. */
	/* U is a quadword, n is an unsigned longword. */

	asm("PQEXP_R3:");
	asm("	popr	$8");		/* Record return address */
	asm("	movq	$1,-(sp)");	/* Initialize */
	asm("	movq	8+4(sp),-(sp)");/* Copy U to top of stack for speed */
	asm("	tstl	8+8(sp)");	/* Only handle n greater than */
	asm("	beqlu	3f");
	asm("1:	blbc	8+8(sp),2f");
	asm("	movq	(sp),-(sp)");	/* Copy the current power of U */
	asm("	movq	8+8(sp),-(sp)");/* Multiply with current value */
	asm("	bsbb	PQMUL_R2");
	asm("	movq	(sp)+,8(sp)");	/* Replace current value */
	asm("	cmpzv	$1,$31,8+8(sp),$0");
	asm("	beqlu	3f");
	asm("2:	movq	(sp),-(sp)");	/* Proceed to next power of U */
	asm("	bsbb	PQMUL_R2");
	asm("	extzv	$1,$31,8+8(sp),8+8(sp)");
	asm("	brb	1b");
	asm("3:	movq	8(sp),8+8+4(sp)");/* Copy the return value */
	asm("	movaq	8+8+4(sp),sp");	/* Discard the exponent */
	asm("	jmp	(r3)");		/* return */

	/* Replaces the quadword U on the stack with U mod P where P is of the */
	/* form P = 2^64 - a. */
	asm("	.set	U,0");		/* Low  longword of U */
	asm("	.set	V,U+4");	/* High longword of U */
	asm("	.set	Y,U+8");	/* Low  longword of Y */
	asm("	.set	Z,Y+4");	/* High longword of Y */
	asm("PQMOD_R0:");
	asm("	popr	$1");		/* Record return address */
	asm("	cmpl	V(sp),$-1");	/* Replace U with U mod P */
	asm("	blssu	1f");
	asm("	cmpl	U(sp),$-A");
	asm("	blssu	1f");
	asm("	addl2	$A,U(sp)");
	asm("	adwc	$0,V(sp)");
	asm("1:	jmp	(r0)");		/* return */


	/* Computes the product U*Y mod P where P is of the form
	 * P = 2^64 - a.  U, Y are quadwords less than P.  The product
	 * replaces U and Y on the stack.
	 *
	 * The product may be formed as the sum of four longword
	 * multiplications which are scaled by powers of 2^32 by evaluating:
	 * 2^64*v*z + 2^32*(v*y + u*z) + u*y
	 * The result is computed such that division by the modulus P
	 * is avoided.
	 */
	asm("PQMUL_R2:");
	asm("	popr	$2");		/* Record return address */
	asm("	movl	sp,r2");	/* Record initial stack value */
	asm("	pushl	Z(r2)");
	asm("	pushl	V(r2)");
	asm("	bsbb	EMULQ");
	asm("	bsbb	PQMOD_R0");
	asm("	bsbb	PQLSH_R0");	/* Obtain 2^32*v*z */
	asm("	pushl	Y(r2)");
	asm("	pushl	V(r2)");
	asm("	bsbb	EMULQ");
	asm("	bsbb	PQMOD_R0");
	asm("	pushl	Z(r2)");
	asm("	pushl	U(r2)");
	asm("	bsbb	EMULQ");
	asm("	bsbb	PQMOD_R0");
	asm("	bsbb	PQADD_R0");	/* Obtain (v*y + u*z) */
	asm("	bsbb	PQADD_R0");	/* Add in 2^32*v*z */
	asm("	bsbb	PQLSH_R0");	/* Obtain the first two terms */
	asm("	pushl	Y(r2)");
	asm("	pushl	U(r2)");
	asm("	bsbb	EMULQ");
	asm("	bsbb	PQMOD_R0");	/* Obtain the third term:  u*y */
	asm("	bsbb	PQADD_R0");	/* Add it in */
	asm("	movq	(sp)+,Y(r2)");	/* Copy the return value */
	asm("	movaq	Y(r2),sp");	/* Point the stack to the return value */
	asm("	jmp	(r1)");		/* return */


	/* This routine knows how to multiply two unsigned longwords,
	 * replacing them with the unsigned quadword product on the stack.
	 */
	
	asm("EMULQ:");
	asm("	emul	4(sp),8(sp),$0,-(sp)");
	asm("	clrl	-(sp)");
	asm("	tstl	4+8+4(sp)");	/* Check both longwords to see if we */
	asm("	bgeq	1f");		/* must compensate for the unsigned
								bias. */
	asm("	addl2	4+8+8(sp),(sp)");
	asm("1:	tstl	4+8+8(sp)");
	asm("	bgeq	2f");
	asm("	addl2	4+8+4(sp),(sp)");
	asm("2:	addl2	(sp)+,4(sp)");	/* Add in the compensation. */
	asm("	movq	(sp)+,4(sp)");	/* Replace the longwords with their
							product. */
	asm("	rsb");

	/*
	 * Computes the product 2^32*U mod P where P is of the form
	 * P = 2^64 - a.  U is a quadword less than P.  The product replaces
	 * U on the stack.
	 *
	 * This routine is used by PQMUL in the formation of quadword
	 * products in such a way as to avoid division by the modulus P.
	 * The product 2^64*v + 2^32*u is congruent a*v + 2^32*u mod P
	 * (where u, v are longwords).
	 */
	asm("PQLSH_R0:");
	asm("	popr	$1");		/* Record return address */
	asm("	pushl	V(sp)");
	asm("	pushl	$A");
	asm("	bsbb	EMULQ");	/* Push a*v */
	asm("	ashq	$32,Y(sp),Y(sp)");/* Form Y = 2^32*u */
	asm("	brb	PQADD_R0_1");	/* Return the sum U + Y mod P. */

	/*
	 * Computes the sum U + Y mod P where P is of the form P = 2^64 - a.
	 * U, Y are quadwords less than P.  The sum replaces U and Y on
	 * the stack.
	 */
	asm("PQADD_R0:");
	asm("	popr	$1");		/* Record return address */
	asm("PQADD_R0_1:");
	asm("	addl2	U(sp),Y(sp)")	/* Add the low longwords */
	asm("	adwc	V(sp),Z(sp)");	/* Add the high longwords with the carry */
	asm("	bcs	2f");		/* If the result is greater than a quadword */
	asm("	cmpl	Z(sp),$-1");
	asm("	blssu	3f");
	asm("	cmpl	Y(sp),$-A");	/* or simply greater than or equal to P */
	asm("	blssu	3f");
	asm("2:	addl2	$A,Y(sp)");	/* we must subtract P.*/
	asm("	adwc	$0,Z(sp)");
	asm("3:	movaq	Y(sp),sp");	/* Point the stack to the return value */
	asm("	jmp	(r0)");		/* return */
}
