# Target: HP 300 running BSD Unix.
TDEFAULTS = -DDEFAULT_VECTOR=hp300bsd_vec \
	-DSELECT_ARCHITECTURES=bfd_m68k_arch \
	-DSELECT_VECS='&hp300bsd_vec,&trad_core_vec'
TDEPFILES= hp300bsd.o
BFD_MACHINES = cpu-m68k.o
