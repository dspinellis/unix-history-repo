/* $Header: sig.h,v 7.0 86/10/08 15:13:32 lwall Exp $ */

/* $Log:	sig.h,v $
 * Revision 7.0  86/10/08  15:13:32  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

void sig_catcher();
#ifdef SIGTSTP
void cont_catcher();
void stop_catcher();
#endif
void mytstp();
void sig_init();
void finalize();
