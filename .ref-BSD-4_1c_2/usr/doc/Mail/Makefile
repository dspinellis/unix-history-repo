TBL=tbl
VTROFF=/usr/ucb/vtroff

vpr:
	$(TBL) mail?.nr | $(VTROFF) -me

lpr:
	$(TBL) mail?.nr | nroff -me -Tlpr | lpr

vcopy:
	$(TBL) mail?.nr | $(VTROFF) -me -t > vout
