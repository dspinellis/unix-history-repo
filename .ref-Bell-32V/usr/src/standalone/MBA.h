/*
*	MBA  definitions
*/
/*
*	register  offsets
*/
# define M_csr  0  /*  configuration/staus reg */
# define M_cr  1  /* control reg */
# define M_sr  2  /* status reg */
# define M_var  3  /* virtual address reg */
# define M_bc  4  /* byte count reg */
# define M_map  0x200  /* start of map regs-longword offset */
/*
*  misc
*/
# define M_BASE  0x20010000  /* phys addr MBA 0 */
# define NEXSPC  0x2000  /*  no.bytes/NEXUS space */
# define MAXMBA 4  /*  max. no. of MBA's */
# define MBAinit  1  /* MBA init in cr */
# define M_extern  0x400  /*  offset from MBA base to extern reg's */
# define EXTSIZ 0x80  /*  extern reg space size/device */
# define M_BCMAX  65536  /* max byte transfer by MBA */
/*
*  Status  Reg
*/
# define M_RDT  1  /* Read Data Timeout */
# define M_IST  2  /* Interface Sequence Timeout */
# define M_RDS  4  /* Read Data Substitute */
# define M_EC  8  /* Error Confirmation */
# define M_IM 0x10  /* Invalid Map */
# define M_MPE  0x20  /* Map Parity Error */
# define M_MDPE  0x40  /* Massbus Data Parity Error */
# define M_ME  0x80  /* Massbus Exception */
# define M_MT  0x100  /* Missed Transfer */
# define M_WCKL  0x200  /* Write Check Lower */
# define M_WCKU  0x400  /* Write Check Upper */
# define M_DTL  0x800  /* Data Transfer Late */
# define M_DTA  0x1000  /* Data Transfer Abort */
# define M_DTC  0x2000  /* Data Transfer Complete */
# define M_MA  0x10000  /*  Massbus Attention */
# define M_MCPE  0x20000  /* Massbus Control Parity Error */
# define M_NED  0x40000  /* Non-Existent Drive */
# define M_PE  0x80000  /* Programming Error */
# define M_CRD  0x20000000  /* Corrected Read Data */
# define M_NRC  0x40000000  /* No Response Confirmation */
# define M_DTB  0x80000000  /* Data Transfer Busy */
