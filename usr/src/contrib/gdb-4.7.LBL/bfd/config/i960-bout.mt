# Target: Intel 960 running b.out
TDEFAULTS = -DDEFAULT_VECTOR=b_out_vec_little_host \
	-DSELECT_VECS='&b_out_vec_little_host,&b_out_vec_big_host,&icoff_little_vec,&icoff_big_vec'
