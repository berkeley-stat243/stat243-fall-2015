# challenge 1

echo "There are $(grep "^processor" /proc/cpuinfo | wc -l) processors on this machine"

# challenge 2, parts 1-2
grep Belgium cpds.csv  | cut -d',' -f 6 | sort -n | head -n 1)
echo "Belgium $(grep Belgium cpds.csv  | cut -d',' -f 6 | sort -n | head -n 1)"

# challenge 2, part 3
countries=$(tail -n +2 cpds.csv | cut -d"," -f2 | sort | uniq | grep -v " " | sed 's/\"//g')

# challenge 2, part 4
for c in $countries; do
   echo "$c $(grep $c cpds.csv  | cut -d',' -f 6 | sort -n | head -n 1)"
done

# challenge 2, part 5
for c in $countries; do
   echo "$c $(grep $c cpds.csv  | cut -d',' -f 6 | sort -n | head -n 1)" >> mingdp.txt
done

# challenge 3
cut -d"," -f4 RTADataSub.csv | sort | uniq | grep "[^0-9,\.]"

# challenge 3, second part
tail -n 1 RTADataSub.csv | grep -o "," | wc -l
nFields=62

for(( i=2; i<=62; i++ )); do
    echo "non-numeric values found in field number $i:"
    cut -d"," -f${i} RTADataSub.csv | sort | uniq | grep "[^0-9,\.]"
done

# challenge 4
echo '1,"America, United States of",45,96.1,"continental, coastal"' > file.csv
echo '2,"France",33,807.1,"continental, coastal"' >> file.csv

sed 's/, /% /g' file.csv | sed 's/,/|/g' | sed 's/%/,/g' > file2.csv


