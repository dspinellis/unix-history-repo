/*
 *	@(#)mclock_.c	5.1 (Berkeley) %G%
 */

long int  mclock_()
  {
  int  buf[6];
  times(buf);
  return(buf[0]+buf[2]+buf[3]);
  }
