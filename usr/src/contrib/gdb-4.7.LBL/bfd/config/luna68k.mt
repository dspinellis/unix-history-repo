# Target: OMRON LUNA-I/II running BSD Unix.
TDEFAULTS = -DDEFAULT_VECTOR=luna68k_vec \
	-DSELECT_ARCHITECTURES=bfd_m68k_arch \
	-DSELECT_VECS='&luna68k_vec,&trad_core_vec'
TDEPFILES= luna68k.o
BFD_MACHINES = cpu-m68k.o
