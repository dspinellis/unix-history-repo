/* VAX Massbus adapter registers
 */

struct mba_regs {
	int mba_csr,
	    mba_cr,
	    mba_sr,
	    mba_var,
	    mba_bcr;
};

/* NOTE:
	mba_erb at displacement 0x400
	mba_as at displacement 0x410
	mba_map at displacement 0x800
 */

#define MBA0 0x80028000
#define MBA1 0x8002a000

#define MBA_ERB 0x400
#define MBA_MAP 0x800

#define MBA0_MAP MBA0 + 0x800
#define MBA1_MAP MBA1 + 0x800

#define MBAEBITS 0xe0770


#define MBAIE 0x4

