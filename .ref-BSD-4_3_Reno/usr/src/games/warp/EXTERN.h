/* $Header: EXTERN.h,v 7.0.1.1 86/12/12 16:46:50 lwall Exp $
 *
 * $Log:	EXTERN.h,v $
 * Revision 7.0.1.1  86/12/12  16:46:50  lwall
 * Guarded the undefs.
 * 
 * Revision 7.0  86/10/08  15:11:31  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#ifdef EXT
#undef EXT
#endif
#define EXT extern

#ifdef INIT
#undef INIT
#endif
#define INIT(x)

#ifdef DOINIT
#undef DOINIT
#endif
