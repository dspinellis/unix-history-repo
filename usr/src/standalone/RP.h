/*
*	*  RP06  Definitions  *
*/
/*
*	RP06  Device  Register  Offsets
*/
# define RP_cr  0  /* control reg */
# define RP_sr  1  /* status reg */
# define RP_er1  2  /*  Err reg 1 */
# define RP_mr  3  /* Maintenance reg */
# define RP_as  4  /* attention summary */
# define RP_stk  5  /* desired track/sector */
# define RP_dt  6  /* drive type */
# define RP_la  7  /* look ahead */
# define RP_sn  8  /*  serial no. */
# define RP_off  9  /* offset reg */
# define RP_cyl  10  /* desired cyl */
# define RP_cca  11  /* current cyl addr */
# define RP_er2  12  /* err reg 2 */
# define RP_er3  13  /*  err reg 3 */
# define RP_Epos  14  /*  ECC position reg */
# define RP_Epat  15  /*  ECC pattern reg */
/*
*	RP06  Function  Codes
*/
# define RP_GO  1  /*  go bit */
# define RP_NOP  /* no-op function code in control reg */
# define RP_UNL  2  /* Unload */
# define RP_RCAL  6  /* Recalibrate */
# define RP_DC  010  /*  Drive Clear */
# define RP_REL  012  /* Release */
# define RP_SRCH  030  /* Search */
# define RP_WCD  050 /* Write Check Data */
# define RP_WCH  052  /* Write Check Header & Data */
# define RP_WR  060  /* Write Data */
# define RP_WHD  062 /* Write Header & Data (Format) */
# define RP_RED  070  /* Read */
# define RP_RHD  072  /* Read Header & Data */
# define RP_SEEK  4  /* Seek */
# define RP_OFF  014  /* Centerline Offset */
# define RP_RTC  016  /* Return to Centerline */
# define RP_PA  022  /* Pack Acknowledge */
# define RP_RIP  020  /* Read-in Preset */
/*
*	RP06 Offset Register
*/
# define RP_HCI  0x400  /*  Header Compare Inhibit */
# define RP_ECI  0x800  /*  ECC Inhibit */
/*
*	RP06  Status Reg
*/
# define RP_ATA  0x8000  /* Attention Active */
# define RP_ERR  0x4000  /* Composite Error */
# define RP_PIP  0x2000  /* Positioning Operation in Progress */
# define RP_MOL  0x1000  /* Medium On-Line */
# define RP_WRL  0x800  /*  Write Lock */
# define RP_LST  0x400  /* Last Sector Transferred */
# define RP_PGM  0x200  /* Programmable (Dual Port) */
# define RP_DRP  0x100  /* Drive Preset (Dual Port) */
# define RP_DRY  0x80  /*  Drive Ready */
# define RP_VV  0x40  /* Volume Valid */
/*
*  RP06  Error  Register
*/
# define RP_DCK  0x8000  /*  Data Check */
# define RP_UNS  0x4000  /* Drive Unsafe */
# define RP_OPI  0x2000  /* Operation Incomplete */
# define RP_DTE  0x1000  /* Drive Timing Error */
# define RP_WLE  0x800  /* Write LOck Error */
# define RP_IAE  0x400  /* Invalid Address */
# define RP_ADE  0x200  /* Address Overflow */
# define RP_HCRC  0x100  /* Header CRC Error */
# define RP_HCE  0x80  /* Header Compare Error */
# define RP_ECH  0x40  /* ECC Hard Error */
# define RP_WCF  0x20  /* Write Clock Fail */
# define RP_FER  0x10  /* Format Error */
# define RP_PAR  8  /* Parity Error */
# define RP_RMR  4  /* Register Modification Refused */
# define RP_ILR  2  /* Illegal Register */
# define RP_ILF  1  /* Illegal Function */
/*
*  misc
*/
# define RP_BASE  0x400  /* start of external reg's on MBA */
# define RP6CYL  815  /* no. RP06 cylinders/pack */
# define RP6TRK  19  /* no. tracks/cyl */
# define RP6SEC  22  /* no. sectors/track */
# define RP6ST  (rptrk*rpsec)  /* no. sectors/cyl */
# define MAXSEC  (rpcyl*rptrk*rpsec)  /* no. sectors/pack */
# define RP_FMT  0x1000  /* format bit in offset reg */
/*		*/
# define RM3CYL  823  /*  RM03 */
# define RM3TRK  5
# define RM3SEC  32
# define RP6typ  022
# define RM3typ  024
# define RP6HD  8  /* no. bytes RP06 sector header */
# define RM3HD  4  /*  no. bytes  RM03 sector header */
# define RM_BSB  0xc000  /*  bad sector bits for format */
