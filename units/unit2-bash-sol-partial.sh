# challenge 1

echo "There are $(grep "^processor" /proc/cpuinfo | wc -l) processors on this machine"

# challenge 2, parts 1-2
grep Belgium cpds.csv  | cut -d',' -f 6 | sort -n | head -n 1)
echo "Belgium $(grep Belgium cpds.csv  | cut -d',' -f 6 | sort -n | head -n 1)"

