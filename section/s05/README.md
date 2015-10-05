# Section 05: code review (ps2) and more git

## code review

There are several different strategies to this problem:

1. Read one block at a time in R
2. Stripping out columns that you don't need using `cut`, then doing (1)
```{bash}
FILE=ss13hus.csv.bz2
# be lazy and automatically generate an expression for grep
echo "ST", "NP", "BDSP", "BLD", "RMSP", "TEN", "FINCP", "FPARC", "HHL", "NOC", "MV", "VEH", "YBL" | \
  tr -d " " | \
  tr , "\n" | \
  awk 'NR > 1 { printf("| ") } {print "^"$0"$"}' | 
  tr -d "\n" | \
  tr -d " " > column_expression.txt
WHICH_COLUMNS=$(cat column_expression.txt)

# extract the column numbers
bzcat $FILE | head -1 | \
  tr -d \" | tr , "\n" | \
  grep -n -E "$WHICH_COLUMNS" | \
  cut -f1 -d: | \
  awk 'NR > 1 { printf(",")}  {print $0}' | \
  tr -d "\n" > column_numbers.txt
COLUMN_NUMBERS=$(cat column_numbers.txt)

# use the column numbers to extract the columns
bzcat $FILE | tail -n +2 | cut -f$COLUMN_NUMBERS -d, | bzip2 > out.bz2
```
3. Do all processing at the command line (`cut`, `shuf`)

Let's discuss the strengths and drawbacks of each one.

## branches in git

We will continue going through the git tutorial, this time covering branches:

https://github.com/berkeley-scf/tutorial-git-basics