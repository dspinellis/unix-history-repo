# this will remove (comment out) all preprocessor traces from 
# cpp produced files:
# run this awk program as follows
#  awk -f zap_cpp.awk <file>
# end redirect output where you want it to
NF > 0 {
  if ($1 ~ /^#/)
	print "/*", $0, "*/"
  else
	print
}

	
