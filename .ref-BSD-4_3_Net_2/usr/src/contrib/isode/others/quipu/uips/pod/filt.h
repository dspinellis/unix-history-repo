/*
 * $Header: /f/osi/others/quipu/uips/pod/RCS/filt.h,v 7.2 91/02/22 09:31:32 mrose Interim $
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
