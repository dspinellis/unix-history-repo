BEGIN { print "(Doc)" }
/^\.Lf/ { print "(" $2 " " substr(FILENAME,1,length(FILENAME)-2) ")" }
/^\.Lx/ { print "(" $2 " " substr(FILENAME,1,length(FILENAME)-2) ")" }
