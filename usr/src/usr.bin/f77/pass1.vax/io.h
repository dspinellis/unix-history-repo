/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)io.h	5.1 (Berkeley) %G%
 */

#define NDDATA	1
#define NDLABEL	2
#define NDADDR	3
#define NDNULL	4




typedef
  struct IoAddr
    {
      int stg;
      int memno;
      ftnint offset;
    }
  ioaddr;



typedef
  union IoValue
    {
      Constp cp;
      ftnint label;
      struct IoAddr addr;
    }
  iovalue;



typedef
  struct IoBlock
    {
      struct IoBlock *next;
      int blkno;
      ftnint len;
      struct OffsetList *olist;
    }
  ioblock;



typedef
  struct OffsetList
    {
      struct OffsetList *next;
      ftnint offset;
      int tag;
      union IoValue val;
    }
  offsetlist;




extern ioblock *iodata;

