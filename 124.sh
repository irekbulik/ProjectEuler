# take 6 minutes but who cares 
#21417 1947
#real	6m12.763s
#user	1m34.728s
#sys	7m44.097s


for i in `seq 2 100000` ; do factor $i | tr ' ' '\n' | uniq | tr '\n' ' ' | awk '{for (i=2; i<NF; i++) printf $i "*"; print $NF}' | bc | awk '{print '$i' " " $NF}'     ; done |  sort -n -k 2 -k 1 | sed -n '9999p'
