: "convert the active file format over"
L=$1
S=$2
cc cvtactive.c
a.out $L $S
mv $L/active $L/oactive
mv $L/nactive $L/active
