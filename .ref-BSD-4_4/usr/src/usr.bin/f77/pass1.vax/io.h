/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 *
 *	@(#)io.h	5.2 (Berkeley) 4/12/91
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

