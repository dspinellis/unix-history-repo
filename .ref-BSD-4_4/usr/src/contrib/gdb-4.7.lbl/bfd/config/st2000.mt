# Target: Tandem ST-2000 using COFF and IEEE-695 object file format
# Avoid dragging in a lot of other architectures and formats.
TDEFINES= -DMRI
TDEFAULTS=-DBFD -DSELECT_ARCHITECTURES=bfd_m68k_arch -DSELECT_VECS='&m68kcoff_vec,&ieee_vec,&srec_vec'

