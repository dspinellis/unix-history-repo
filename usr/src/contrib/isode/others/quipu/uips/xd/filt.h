/* $Header: /f/osi/others/quipu/uips/xd/RCS/filt.h,v 7.1 91/02/22 09:32:48 mrose Interim $ */
/*
 $Log:	filt.h,v $
 * Revision 7.1  91/02/22  09:32:48  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/06/12  13:10:55  mrose
 * *** empty log message ***
 * 
 * Revision 1.5  90/04/26  10:22:41  emsrssn
 * Installation fixed
 * 
 * 
 * Revision 1.4  90/04/25  17:28:11  emsrssn
 * Lint tidy up
 * 
 * 
 * Revision 1.3  90/04/19  13:54:12  emsrssn
 * keyboard accelerator now activates button highlight.
 * 
 * search types available is dependent on current position
 * to prevent unreasonable searches.
 * 
 * the help popup changes automatically depending on the 
 * position of the cursor
 * 
 * buttons remain a fixed size when the application is
 * resized
 * 
 * command line options are now handled properly
 * 
 * logging added
 * 
 * "reads" are now sorted to show mail address at top etc.
 * 
 * 
 * Revision 1.2  90/03/09  15:57:31  emsrssn
 * First public distribution
 * 
 * 
 * Revision 1.1  90/03/08  13:18:46  emsrssn
 * Initial revision
 * 
 * 
*/

#ifndef FILT
#define FILT

typedef struct stroid_list {
        int fi_type;
        char *stroid;
        char *name;
      } filt_item;

typedef struct filter_struct {
        int flt_type;
        union ftype {
                filt_item item;
                struct filter_struct *sub_filt;
              } fu_cont;
        struct filter_struct *next;
      } filt_struct;

#endif 
