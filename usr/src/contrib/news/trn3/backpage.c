/* $Id: backpage.c,v 3.0 1991/09/09 20:18:23 davison Trn $
 */
/* This software is Copyright 1991 by Stan Barber. 
 *
 * Permission is hereby granted to copy, reproduce, redistribute or otherwise
 * use this software as long as: there is no monetary profit gained
 * specifically from the use or reproduction of this software, it is not
 * sold, rented, traded or otherwise marketed, and this copyright notice is
 * included prominently in any copy made. 
 *
 * The author make no claims as to the fitness or correctness of this software
 * for any use whatsoever, and it is provided as is. Any use of this software
 * is at the user's own risk. 
 */

#include "EXTERN.h"
#include "common.h"
#include "intrp.h"
#include "final.h"
#include "INTERN.h"
#include "backpage.h"

ART_LINE maxindx = -1;

void
backpage_init()
{
    char *varyname;
    
    varyname = filexp(VARYNAME);
    close(creat(varyname,0600));
    varyfd = open(varyname,2);
    UNLINK(varyname);
    if (varyfd < 0) {
	printf(cantopen,varyname) FLUSH;
	sig_catcher(0);
    }
    
}

/* virtual array read */

ART_POS
vrdary(indx)
ART_LINE indx;
{
    int subindx;
    long offset;

#ifdef DEBUG
    if (indx > maxindx) {
	printf("vrdary(%ld) > %ld\n",(long)indx, (long)maxindx) FLUSH;
	return 0;
    }
#endif
    if (indx < 0)
	return 0;
    subindx = indx % VARYSIZE;
    offset = (indx - subindx) * sizeof(varybuf[0]);
    if (offset != oldoffset) {
	if (oldoffset >= 0) {
#ifndef lint
	    (void)lseek(varyfd,oldoffset,0);
	    write(varyfd, (char *)varybuf,sizeof(varybuf));
#endif /* lint */
	}
#ifndef lint
	(void)lseek(varyfd,offset,0);
	read(varyfd,(char *)varybuf,sizeof(varybuf));
#endif /* lint */
	oldoffset = offset;
    }
    return varybuf[subindx];
}

/* write to virtual array */

void
vwtary(indx,newvalue)
ART_LINE indx;
ART_POS newvalue;
{
    int subindx;
    long offset;

#ifdef DEBUG
    if (indx < 0)
	printf("vwtary(%ld)\n",(long)indx) FLUSH;
    if (!indx)
	maxindx = 0;
    if (indx > maxindx) {
	if (indx != maxindx + 1)
	    printf("indx skipped %d-%d\n",maxindx+1,indx-1) FLUSH;
	maxindx = indx;
    }
#endif
    subindx = indx % VARYSIZE;
    offset = (indx - subindx) * sizeof(varybuf[0]);
    if (offset != oldoffset) {
	if (oldoffset >= 0) {
#ifndef lint
	    (void)lseek(varyfd,oldoffset,0);
	    write(varyfd,(char *)varybuf,sizeof(varybuf));
#endif /* lint */
	}
#ifndef lint
	(void)lseek(varyfd,offset,0);
	read(varyfd,(char *)varybuf,sizeof(varybuf));
#endif /* lint */
	oldoffset = offset;
    }
    varybuf[subindx] = newvalue;
}

