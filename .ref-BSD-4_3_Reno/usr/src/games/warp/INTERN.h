/* $Header: INTERN.h,v 7.0.1.1 86/12/12 16:51:45 lwall Exp $
 *
 * $Log:	INTERN.h,v $
 * Revision 7.0.1.1  86/12/12  16:51:45  lwall
 * Guarded the undefs.
 * 
 * Revision 7.0  86/10/08  15:11:37  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#ifdef EXT
#undef EXT
#endif
#define EXT

#ifdef INIT
#undef INIT
#endif
#define INIT(x) = x

#define DOINIT
