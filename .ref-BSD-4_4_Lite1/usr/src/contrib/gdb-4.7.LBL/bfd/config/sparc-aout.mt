# Target:  SPARC using a.out.
TDEFAULTS = -DDEFAULT_VECTOR=sunos_big_vec \
	-DSELECT_ARCHITECTURES=bfd_sparc_arch
BFD_MACHINES = cpu-sparc.o
