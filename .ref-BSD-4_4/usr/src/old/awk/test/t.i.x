{i=i+log($1); print i,log($1)}
END {print exp(i),i}
