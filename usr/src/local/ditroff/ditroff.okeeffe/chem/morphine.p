.cstart
# This is a structure of morphine, for comparison with the one
# in the preprint by Broadbent and Norris
#
R1:	ring6 double 1,2
	bond -60 from R1.V6 ; HO
R2:	ring6 with .V1 at R1.V3
	bond 60 from R2.V2 ; N
	bond right from N ; CH3
R3:	benzene with .V1 at R2.V5
	bond -120 from R3.V5 ; HO
# this is the furan ring
	bond -135 length .3 from R1.V5 ; O
	bond -45 length .3 from R3.V6
# this is the odd ring
	bond up length .1 from N ; BP
B1:	bond up length .35 from R1.V4
	bond to BP
.cend
