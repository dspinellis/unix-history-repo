/*	adreg.h	4.1	82/06/26	*/

struct addevice {
	short int ad_csr;			/* Control status register */
	short int ad_data;			/* Data buffer */
};

#define AD_CHAN		(('a'<<8)|0)
#define AD_READ		(('a'<<8)|1)

/*
 * Unibus CSR register bits
 */

#define AD_START		01
#define AD_SCHMITT		020
#define AD_CLOCK		040
#define AD_IENABLE		0100
#define AD_DONE 		0200
#define AD_INCENABLE		040000
#define AD_ERROR		0100000
