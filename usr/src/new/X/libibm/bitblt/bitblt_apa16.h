/* $Header: bitblt_apa16.h,v 10.1 86/11/19 10:51:47 jg Exp $ */
/*
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 *
 * Written by Daniel Stone, Brown University/IRIS April 23, 1986.
 *
 * This file contains the defines to do hardware bit block transfers (bitblt's)
 * on the APA-16.
 */

/* $Header: bitblt_apa16.h,v 10.1 86/11/19 10:51:47 jg Exp $ */
/* $Source: /u1/X/libibm/bitblt/RCS/bitblt_apa16.h,v $ */

/*
 * Log 2 of the screen width.
 */
#define LOG2_BPSW       7
 
/*
 * Divide and multiply by Bytes Per Screen Width.
 */
#define DIV_BPSW(value)  ((value) >> LOG2_BPSW)
#define MUL_BPSW(value)  ((value) << LOG2_BPSW)

/*
 * Screen dimensions in pixels.
 */
#define SCREEN_WD	1024
#define HIDDEN_WD	SCREEN_WD

#define SCREEN_HT	768
#define HIDDEN_HT	256

/*
 * Screen dimensions in words.
 */
#define SCREEN_WORD_WD (SCREEN_WD/BPW)
#define HIDDEN_WORD_WD (HIDDEN_WD/BPW)

#define SCREEN_WORD_HT (SCREEN_HT/BPW)
#define HIDDEN_WORD_HT (HIDDEN_HT/BPW)

#define IO_ADDR 0xF0000000
#define MODEL_IO_ADDR	(IO_ADDR | 0x04000000)

/*
 * Hidden frame buffer address.
 */
#define HID_OFFSET		0x00018000
#define HID_APA16BASE 		(APA16BASE + HID_OFFSET)

/*
 * Within the hidden screen area there is an area for 2 hardware locators.
 */
#define LOCATOR_OFFSET		(HID_OFFSET + 0x800)
#define LOCATOR_APA16BASE	(APA16BASE + LOCATOR_OFFSET)

/*
 * The bottom right corners of both locators.
 */
#define AND_LOCATOR_RT		48
#define AND_LOCATOR_BM		848
#define XOR_LOCATOR_RT		(48*2)
#define XOR_LOCATOR_BM		AND_LOCATOR_BM

#define SAVE_AND_LOCATOR_RT	(48*3)
#define SAVE_AND_LOCATOR_BM	848
#define SAVE_XOR_LOCATOR_RT	(48*4)
#define SAVE_XOR_LOCATOR_BM	AND_LOCATOR_BM

/*
 * Dimensions of the locators found in the hidden screen area.
 */
#define HARD_LOCATOR_WD  48
#define HARD_LOCATOR_HT  64

/*
 * Spot on the APA-16's memory where the hardware cursor is located.
 */
#define AND_LOCATOR		LOCATOR_APA16BASE
#define XOR_LOCATOR		(LOCATOR_APA16BASE + BTOB(HARD_LOCATOR_WD))

/*
 * Spot on the APA-16's memory where we will save the current locator.
 */
#define SAVE_AND_LOCATOR	(LOCATOR_APA16BASE + BTOB(HARD_LOCATOR_WD*2))
#define SAVE_XOR_LOCATOR	(LOCATOR_APA16BASE + BTOB(HARD_LOCATOR_WD*3))

/*
 * Within the hidden screen area there is an area for hidden fonts.
 */
#define FONT_Y_COORD		848
#define FONT_OFFSET 		(HID_OFFSET + 0x2800) 
#define FONT_APA16BASE 		(APA16BASE + FONT_OFFSET)
#define FONT_XBASE		1
#define FONT_YBASE		848
#define FONT_YBASE_BOTTOM	896

/*
 * Within the hidden screen area there is an area for a rasterop queue.
 */
#define QUE_Y_COORD		896
#define QUE_OFFSET		(HID_OFFSET + 0x4000)
#define QUE_APA16BASE		(APA16BASE + QUE_OFFSET)

#define RESERVE_Y_COORD		1006
#define RESERVE_OFFSET		(HID_OFFSET + 0x7700)
#define LAST_QUE_OFFSET		(RESERVE_OFFSET - 2)
#define LAST_QUE_APA16BASE	(APA16BASE + LAST_QUE_OFFSET)
#define QUE_WD			SCREEN_WD
#define QUE_HT			110

/*
 * Macro that builds the queue pointer using the screen address which is a
 * short pointer.
 */
#define SPTR_TO_QPTR(sptr)	((short)(DIV_2((long)sptr & 0x0000ffff) | \
				 0xE000))
/*
 * Macro that takes a queue pointer (an unsigned short) and builds a unsigned
 * short pointer from it.
 */
#define QPTR_TO_SPTR(qptr)	((unsigned short *)(MUL_2(qptr)|QUE_APA16BASE))

/*
 * Macros that take a short pointer to the bitmap and return the x value in bits
 * and the y value in scanlines.
 */
#define SPTR_TO_X(sptr)		(MUL_BPB((long)sptr & 0x007f))
#define SPTR_TO_Y(sptr)		(DIV_BPSW((long)sptr & 0x1ff80))

#define X_LOCATOR_R	((unsigned short *)(MODEL_IO_ADDR | 0xD9F800))
#define Y_LOCATOR_R	((unsigned short *)(MODEL_IO_ADDR | 0xD9F802))
#define QUE_COUNT_R	((unsigned short *)(MODEL_IO_ADDR | 0xD9F804))
#define QUE_PTR_R	((unsigned short *)(MODEL_IO_ADDR | 0xD9f806))
#define QUE_LINK_PTR_R	((unsigned short *)(MODEL_IO_ADDR | 0xD9f814))
#define QUE_MODE_R	((unsigned short *)(MODEL_IO_ADDR | 0xD9f816))

/*
 * Macros and defines for the mode register.
 */
#define MODE_R (IO_ADDR | 0x0D10)

#define ACCESS_BIT		0x8000
#define SET_ACCESS_BIT(mode)	(mode |= ACCESS_BIT)
#define CLR_ACCESS_BIT(mode)	(mode &= ~ACCESS_BIT)

#define PAGE_SELECT		0x4000

#define WR_MASK			0x0f00
#define SET_WR_MASK(mode,msk)	mode = ((mode & ~WR_MASK)|((msk<<8) & WR_MASK))
#define CLR_WR_MASK(mode)	mode &= ~WR_MASK

#define LOGIC_FUNC		0x00f0
#define SET_LOGIC_FUNC(mode,fc)	mode = ((mode & ~LOGIC_FUNC)| \
						((fc<<4) & LOGIC_FUNC))
#define CLR_LOGIC_FUNC(mode)	mode &= ~LOGIC_FUNC

#define START_BIT		0x000f
#define SET_START_BIT(mode,sb)	mode = ((mode & ~START_BIT)|(sb & START_BIT))
#define CLR_START_BIT(mode)	mode &= ~START_BIT

/*
 * More understandable names for access bit.
 */
#define HORZ_ACCESS(mode)	SET_ACCESS_BIT(mode)
#define VERT_ACCESS(mode)	CLR_ACCESS_BIT(mode)

/*
 * Control/Status register.
 */
#define CS_R (IO_ADDR | 0x0D12)

#define BACKGRND_BIT	0x0400
#define BLACK_ON_WHITE	*(unsigned short *)(CS_R) |= BACKGRND_BIT
#define WHITE_ON_BLACK	*(unsigned short *)(CS_R) &= ~BACKGRND_BIT
#define TOGGLE_BACKGRND *(unsigned short *)(CS_R) ^= BACKGRND_BIT

/*
 * Increment Queue register.  NOTE: Reg is any register the data is ignored.
 */
#define INCR_QUE_R	    (IO_ADDR | 0x0D14)
#define INCR_QUE_COUNT(reg) *(unsigned short *)INCR_QUE_R = (unsigned short)reg

/*
 * Wait for the rasterop engine to stop.  If it does not stop then kill it
 * after a specified time.  The way this works is this, we poll the queue
 * counter register waiting for it to zero (which means the rasterop has
 * stopped), if it doesn't zero by QUE_TIME_OUT then assume something drastic
 * went wrong and initialize the APA-16.
 * 
 * Later we will re-write this to receive an interrupt.
 */
#define QUE_TIME_OUT 500000

#define WAIT_QUE(c,num)	{					\
	if (*QUE_COUNT_R != 0)	{				\
		c = 0;						\
		while (*QUE_COUNT_R != 0) {			\
			if (c++ == QUE_TIME_OUT) {		\
				c = *QUE_COUNT_R;		\
				RESET_APA16(c);		\
				printf("RESET APA-16! n:%d count:%d\r\n", \
					num,c);			\
				break;				\
			}					\
		}						\
	}							\
}

/*
 * Video data output.
 */
#define VD_OUT (IO_ADDR | 0xD1A)

/*
 * Disable video data output (pass it a temporary register).
 */
#define DISABLE_VD_OUT(reg)	reg = *(unsigned short *)VD_OUT

/*
 * Enable video data output.  NOTE: Reg is any register. The data in "reg"
 * is ignored.
 */
#define ENABLE_VD_OUT(reg)	*(unsigned short *)VD_OUT = (unsigned short)reg

/*
 * Reset adaptor register.
 */
#define RA_R	(IO_ADDR | 0x0D20)
#define RESET_APA16(reg)	*(unsigned short *)RA_R = (unsigned short)reg;

/*
 * Disable DMA processing.  NOTE: Reg is any register the data is ignored.
 */
#define DISABLE_DMA		(IO_ADDR | 0x0D26)
#define DISABLE_DMA_PROC(reg) *(unsigned short *)DISABLE_DMA=(unsigned short)reg

#define ENABLE_DMA		(IO_ADDR | 0x0D28)
#define ENABLE_DMA_PROC(reg) *(unsigned short *)ENABLE_DMA = (unsigned short)reg

/*
 * Default initial values for the APA-16 registers. 
 */
#define MODER_DEFAULT	0x8090
#define CSR_DEFAULT	0x0400

/*
 * Defines and macros for the rasterop commands.
 * ROT stands for Raster Operation Type.
 * LF stands for logic function.
 */
#define DECR_QUE_COUNT		0x0800

#define ROT_WRDST		0x02F0
#define ROT_RECTCP		0x0300
#define ROT_TDDMA		0x0370

#define LF_CLEAR		0x0000
#define LF_DSTandSRC		0x0001
#define LF_NotDSTandSRC		0x0002
#define LF_COPYDST		0x0003
#define LF_DSTandNotSRC		0x0008
#define LF_COPYSRC		0x0009
#define LF_NotDST		0x0009
#define LF_DSTxorSRC		0x000a
#define LF_DSTorSRC		0x000b
#define LF_NotDSTorNotSRC	0x000e
#define LF_SET			0x000f

#define IS_ROT_WRDST(excmd)	((excmd & 0x03f0) == ROT_WRDST)
/*
 * Rasterop execute command instruction.
 */
#define BUILD_EXCMD(decrflag,ROT_type,LF_type) \
				(0xD000 | decrflag | ROT_type | LF_type)

#define Y_BRCH_BASE		896
#define X_BRCH_BASE		0

/*
 * Rasterop branch instruction.  Both X and Y should be given in bits.
 * Y must be between 896 and 1023. X must fall on a word boundrary.  
 */
#define BUILD_BRCH(x,y)	((Y_BRCH_BASE << 6) | ((y) << 6) | DIV_BPW(x))

#define HIGH_BYTE(addr)	(((unsigned long)addr & 0x1fe0000) >> 17)
#define MID_BYTE(addr)	(((unsigned long)addr & 0x1fe00) >> 9)
#define LOW_BYTE(addr)	(((unsigned long)addr & 0x1fe) >> 1)

/*
 * Queue command codes for the registers on the APA-16.
 */
#define REG_Y_LOOP		0x0000
#define REG_X_LOOP		0x1000
#define REG_GEN_A		0x2000
#define REG_GEN_B		0x3000
#define REG_Y_SRC		0x4000
#define REG_X_SRC		0x5000
#define REG_Y_DST		0x6000
#define REG_X_DST		0x7000
#define REG_Y_BACKUP		0x8000
#define REG_X_BACKUP		0x9000
#define REG_HIGHBYTE		0xa000
#define REG_LOWBYTE		0xb000

/*
 * Now that we've defined those names for hardware guru's lets have some
 * real names.  Notice how the HIGHBYTE register (whatever that is) is
 * used as the Y destination register when dealing with rectangle copys
 * also notice that the Y_LOOP register is used (as expected) as the height
 * register during rectangle copies but that the Y_BACKUP register is used
 * when only dealing with the destination.
 */
#define XDST_REG		REG_X_DST
#define YDST_REG		REG_Y_DST
#define XSRC_REG		REG_X_SRC
#define YSRC_REG		REG_Y_SRC
#define RECT_YDST_REG		REG_HIGHBYTE
#define WIDTH_REG		REG_X_BACKUP
#define HEIGHT_REG		REG_Y_BACKUP
#define RECT_HEIGHT_REG		REG_Y_LOOP

#define MAXPARAM		1024
#define VISPARAM		768
#define MINPARAM		1

/*
 * Register load command.
 */
#define BUILD_REGLOAD(REG_type,param) (REG_type | (param & (MAXPARAM-1)))

/*
 * Indicates a there is no bit image for this font.
 */
#define NULLIMAGE	MAXPARAM+1

/*
 * A nulls for pointers.
 */
#define US_NIL	((unsigned short *)0)
#define S_NIL	((short *)0)

/*
 * Execute the commands set up in the queue at Qaddr but first wait for
 * the last command to finish, then set up the queue register pointer
 * and increment the count nexec number of times.
 */
#define EXECUTE_QUE_CMDS(Qaddr,nexec,index)			\
	{							\
		register i,tmp;					\
		WAIT_QUE(index);				\
		*QUE_PTR_R = Qaddr;				\
		for (i = nexec + 1; --i;)			\
			INCR_QUE_COUNT(tmp);			\
	}

/*
 * Take an index into one of the queue instructions, wait for the last command
 * to finish, set up the queue register pointer and increment the count the
 * right number of times.
 */
#define DO_QUE_CMD(index) EXECUTE_QUE_CMDS(QUEinstr[index].Qaddr, \
					   QUEinstr[index].nexec,index)

/*
 * Number of words needed to do a destination only command is 5 and
 * the number of words needed to do a rectangle copy is 7.
 */
#define DSTCMDWORDS	5
#define RECTCMDWORDS	7
