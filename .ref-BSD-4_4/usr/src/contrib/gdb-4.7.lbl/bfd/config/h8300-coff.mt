# Target: Hitachi H8/300 using COFF and IEEE-695 object file format
# Avoid dragging in a lot of other architectures and formats.
TDEFAULTS=-DBFD -DSELECT_ARCHITECTURES=bfd_h8300_arch -DSELECT_VECS='&h8300coff_vec,&ieee_vec,&srec_vec'
CC=gcc
