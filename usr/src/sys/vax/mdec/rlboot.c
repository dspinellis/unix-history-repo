/*	rlboot.c	4.2	83/02/20	*/

/*
 * RL02 disk boot program to load "/boot" from
 * a UNIX filesystem (starting at block 1 on pack on
 * drive 0) into low core and to execute that file.
 *
 * This program can only read regular small 1k byte (3bsd+) files
 * from the root of a UNIX filesystem.
 *
 * Hacked from a rk07 boot by Hank Trapnell, IMS.
 *
 *	/sys/mdec/hlboot.s	4.1	82/12/17
 */
        .set    BLKSIZ,1024             /* file system block size */
        .set    BPSECT,256              /* size of sector in bytes */
        .set    RELOC,0x50000
        .set    INOSIZ,64               /* no. bytes/inode entry */
        .set    INOBLK,BLKSIZ/INOSIZ    /* no. inodes/disc block */
        .set    INOMSK,0xfffffff0       /* changes with inode size */
        .set    NAMSIZ,14               /* bytes in directory name */
        .set    ENTADR,024              /* offset to entry addr in a.out */
        .set    DIRSIZ,16               /* size of directory entry, bytes */
        .set    ROOTINO,2               /* root dir inode no. */
        .set    NBOO,1                  /* no. of boot blocks */
        .set    NSUP,1                  /* no. of superblocks */
        .set    SID,62                  /* system ID register */
/* UBA registers */
        .set    UBA_CNFGR,0             /* UBA configuration register */
        .set    UBA_CR,4                /* UBA control register offset */
        .set    UBA_MAP,0x800           /* UBA offset to map reg's */
        .set    UBAinit,1               /* UBA init bit in UBA control reg */
        .set    pUBIC,16                /* Unibus init complete */
/* RL11 registers and bits */
        .set    HL,0174400-0160000      /* address of RL11 */
        .set    HL_cs,HL+0              /* control and status */
        .set    HL_ba,HL+2              /* bus address */
        .set    HL_da,HL+4              /* disk address */
        .set    HL_mp,HL+6              /* word count */
        .set    HLSECT,40               /* sectors per track */
        .set    HLTRAC,2                /* tracks per cylinder */
        .set    HLST,HLSECT*HLTRAC
        .set    HL_GO,0                 /* go bit */
        .set    HL_RCOM,014             /* read command */
        .set    HL_SEEK,06              /* seek */
        .set    HL_GSTAT,04             /* get status command */
        .set    HL_RHDR,010             /* read header */
        .set    HL_RESET,013            /* reset drive */
        .set    HL_HS,020               /* head select */
        .set    HL_DIR,04               /* seek direction */
        .set    HL_pRDY,7               /* position of ready bit */
        .set    HL_pERR,15              /* position of error bit */

init:
/* r9   UBA address */
/* r10  umem addr */
        .word   0                       /* entry mask for dec monitor */
        nop;nop;nop;nop;nop;nop;nop;nop /* some no-ops for 750 boot to skip */
	nop;nop;
/* get cpu type and find the first uba */
        mfpr    $SID,r0
        extzv   $24,$8,r0,r0            /* get cpu type */
        ashl    $2,r0,r1
        movab   physUBA,r2              /* get physUBA[cpu] */
        addl2   r1,r2
        movl    (r2),r9
        movab   physUMEM,r2             /* get physUMEM[cpu] */
        addl2   r1,r2
        movl    (r2),r10
/* if 780, init uba */
        cmpl    r0,$1
        bneq    2f
        movl    $UBAinit,UBA_CR(r9)
1:
        bbc     $pUBIC,UBA_CNFGR(r9),1b
2:
/* init rl11, and drive 0, don't check for any errors now */
        movw    $HL_RESET,HL_da(r10)
        movw    $HL_GSTAT+HL_GO,HL_cs(r10)

/* relocate to high core */
start:
        movl    r5,r11                  /* boot flags */
        movl    $RELOC,sp
        moval   init,r6
        movc3   $end,(r6),(sp)
        jmp     *$RELOC+start2
/* now running relocated */
/* search for ``boot'' in root inode */
start2:
        movl    $names+RELOC,r6
        movzbl  $ROOTINO,r0
nxti:
        clrw    *$bno		/* start with first block in inode */
        bsbw    iget
        tstb    (r6)
        beql    getfil		/* found correct inode! */
get1b:
        bsbw    rmblk		/* read inode from block now in memory */
        beql    start2
        movl    $buf,r7
nxtent:
        tstw    (r7)
        beql    dirchk
        cmpc3   $NAMSIZ,(r6),2(r7)
        bneq    dirchk
        movzwl  (r7),r0
        addl2   $NAMSIZ,r6
        brb     nxti
dirchk:
        acbl    $buf+BLKSIZ-1,$DIRSIZ,r7,nxtent 
        brb     get1b
/* found inode for desired file... read it in */
getfil:
        clrl    bufptr
getlop:
        bsbb    rmblk
        beql    clear
        addl2   $BLKSIZ,bufptr
        brb     getlop
clear:
        movl    *$size,r3
clrcor:
        clrq    (r3)
        acbl    $RELOC,$8,r3,clrcor
/* run loaded program */
        movl    $8,r10                  /* major("/dev/hl0a") */
        calls   $0,*$0
        brw     start2
/* iget: get inode block whose # is in r0 */
iget:
        addl3   $(INOBLK*(NBOO+NSUP))-1,r0,r8
        divl3   $INOBLK,r8,r4
        bsbw    rblk
        bicl2   $INOMSK,r8
        mull2   $INOSIZ,r8
        addl2   $buf,r8
        movc3   $time-inode,(r8),*$inode
        rsb
/* rmblk: read in bno into addr */
rmblk:
        movzwl  *$bno,r0
        addw2   $3,*$bno
        addl2   $addr,r0
/* this boot assumes only small files (<=10k bytes, ie. no indirect blocks) */
        extzv   $0,$24,(r0),r4
        bneq    rblk
        rsb
/* rblk: read disk block whose number is in r4 */
rblk:
        movzbw	$HL_RHDR+HL_GO,HL_cs(r10)
        bsbw    hlwait
        movzwl  HL_mp(r10),r0
        extzv   $7,$9,r0,r3             /* get current cylinder */
        mull2   $BLKSIZ/BPSECT,r4
        clrl    r5
        ediv	$HLST,r4,r0,r1          /* get desired cylinder */
        movzbl	$1,r5
        subl2   r0,r3
        bgeq    1f
        mnegl   r3,r3
        bisl2   $HL_DIR,r5              /* move towards the spindle */
1:
        insv    r3,$7,$9,r5
        clrl    r2
        ediv    $HLSECT,r1,r2,r1
        tstl    r2
        beql    1f
        bisl2   $HL_HS,r5
1:
        movl    r5,HL_da(r10)
	ashl	$7,r0,r5
        movw    $HL_SEEK+HL_GO,HL_cs(r10)
        bsbb    hlwait
	tstl	r2
	beql	1f
	bisl2	$0x40,r5
1:
	addl2	r1,r5
        ashl    $-9,bufptr,r0
        bisl3   $0x80000000,r0,UBA_MAP(r9)
        incl    r0
        bisl3   $0x80000000,r0,UBA_MAP+4(r9)
        clrw    HL_ba(r10)
	movw	r5,HL_da(r10)
        movw    $-BLKSIZ/2,HL_mp(r10)
        movw    $HL_RCOM+HL_GO,HL_cs(r10)
hlwait:         /* wait for controller ready */
        movw    HL_cs(r10),r0
        bbc     $HL_pRDY,r0,hlwait
        bbs     $HL_pERR,r0,hlerr
        bicpsw  $2
        rsb
hlerr:
        halt    /* ungraceful */
bufptr: .long  buf
names:  .byte   'b,'o,'o,'t,0,0,0,0,0,0,0,0,0,0
        .byte  0
physUBA:
        .long   0
        .long   0x20006000      /* 11/780 */
        .long   0xf30000        /* 11/750 */
        .long   0xf26000        /* 11/7ZZ */
physUMEM:
        .long   0
        .long   0x2013e000      /* 11/780 */
        .long   0xffe000        /* 11/750 */
        .long   0xffe000        /* 11/7ZZ */
end:
        .set    buf,RELOC-1536
        .set    inode,RELOC-512
        .set    mode,inode
        .set    nlink,mode+2
        .set    uid,nlink+2
        .set    gid,uid+2
        .set    size,gid+2
        .set    addr,size+4
        .set    time,addr+40
        .set    bno,time+12
