#	@(#)excmd.awk	9.1 (Berkeley) 11/9/94
 
/^\/\* C_[0-9A-Z_]* \*\/$/ {
	printf("#define %s %d\n", $2, cnt++);
	next;
}
