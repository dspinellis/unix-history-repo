/*	TM02  register  definitions	*/
# define TM_cs1  0  /*  TM2 control 1 reg */
# define TM_ds  1  /*  TM02 status reg */
# define TM_er  2  /*  TM02 error reg */
# define TM_mr  3  /*  TM02 maint. reg */
# define TM_as  4  /*  TM02 attention summary reg */
# define TM_fc 5  /*  TM02 frame count reg */
# define TM_dt  6  /*  TM02 drive type reg */
# define TM_ck  7  /* TM02 check char. reg. */
# define TM_sn  8  /*  TM02 serial no. reg. */
# define TM_tc  9  /*  TM02 tape control reg */
/*		*/
# define TM_GO  1  /*  TM2 go bit */
# define TM_RWND  06  /*  TM2 Rewind  */
# define TM_DCLR  010  /*  TM2 drive clear function code */
# define TM_RIP  020  /*  TM02 Read-In Preset command */
# define TM_ERAS  024  /*  TM02 erase command */
# define TM_WTM  026  /*  TM02 write-tape-mark command */
# define TM_SFWD  030  /*  space forward function code */
# define TM_SREV  032  /*  TM02 space reverse command */
# define TM_WRTF	060	/* write forward function code */
# define TM_REDF  070  /*  read forward function code */
# define TM_RREV  076  /*  TM02 read reverse command */
/*		*/
# define TM_DRDY  0x80  /*  TM2/drive ready,status reg */
# define TM_ERR 0x4000  /*  TM2 composite error, status reg */
# define TM_MOL  0x1000  /* TM02 medium-on-line status bit */
# define TM_TM  0x4  /* tape mark bit in TM02 status reg */
# define TM_EOT  0x400  /*  end-of-tape bit in status reg */
# define TM_FCE  0x200  /* TM02 frame count error bit */
/*		*/
# define TM_EPAR  0x8  /* even parity bit in TM02 control reg */
# define TM_PNOR  0xc0  /*  PDP11 normal mode for tape control reg */
# define TM_D800  0x300  /* TM02 800BPI for tc reg */
# define TM_D1600  0x400  /*  TM02 1600BPI PE for tc reg */
# define TM_EABO  0x1000  /*  TM02 enable abort bit in tc reg */
