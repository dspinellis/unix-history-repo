# Target: Intel 960 embedded development, COFF format
TDEFAULTS = -DDEFAULT_VECTOR=icoff_little_vec \
	-DSELECT_VECS='&b_out_vec_little_host,&b_out_vec_big_host,&icoff_little_vec,&icoff_big_vec'
