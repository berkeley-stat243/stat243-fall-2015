# Section 14: code review (ps6 & ps7)

# Problem set 6

## Queries and views

Which query is more efficient?

### query 1

```sql
create view myView
  select * from flights
  where DepDelay != 'NA'
select Origin, Dest, Month, DayOfWeek, Hour,
  count(DepDelay > 30) as Count30,
  from myView
  group by someGrouping
```

### query 2

```sql
select Origin, Dest, Month, DayOfWeek, Hour,
  count(DepDelay > 30) as Count30,
  from flights
  where DepDelay != 'NA'
  group by someGrouping
```

## Parallel R

Let's look at problem 3.
Assume there are 4 cores.
What's the best way to parallelize it?

```r
generate_query <- function(where_statement) {
  paste0("SELECT Origin, Dest, Month, DayOfWeek, Hour,",
                 "count(DepDelay > 30) as Count30,",
                 # the correct answer has some more stuff here
                 "count(*) as n ",
                 "FROM flightsClean WHERE ", where_statement, " ",
                 "GROUP BY someGrouping")
}
```

### solution 1

Assume `airports` is a character vector of unique airports.

```r
results <- foreach(id = seq_along(airports)) %dopar% {
  # some logic to deal with database
  generate_query(paste0("Origin = ", airports[id]))
  auth <- dbSendQuery(db, query)
  val <- fetch(auth, -1)
  dbClearResult(auth)

  val
}
```

### solution 2

Assume `months` is a character vector of unique months.

```r
results <- foreach(id = seq_along(months)) %dopar% {
  # some logic to deal with database
  generate_query(paste0("Month = ", months[id]))
  auth <- dbSendQuery(db, query)
  val <- fetch(auth, -1)
  dbClearResult(auth)

  val
}
```

### solution 3

Assume `months` is a character vector of unique months.

```r
months_grouped <- list(months[1:3], months[4:6], months[7:9], months[10:12])
results <- foreach(id = seq_along(months_grouped)) %dopar% {
  # some logic to deal with database
  generate_query(paste0("Month in ", paste0(months[[id]], collapse = ",")))
  auth <- dbSendQuery(db, query)
  val <- fetch(auth, -1)
  dbClearResult(auth)

  val
}
```

## Spark

What is the difference between the 2 statements?
Assume `lines = sc.textFile('/data/someFile.gz')`

```python
lines.filter(lambda x: 'NA' not in x).repartition(30)

lines.repartition(30).filter(lambda x: 'NA' not in x)
```

# Problem set 7

## problem 4

On board.

# Useful tools

## tmux

`tmux` is a terminal multiplexer.
It is extremely useful when working on remote machines.
Some useful commands might be:

- `tmux ls` to list the current sessions
- `tmux new -s sessionName` to create a new session named `sessionName`
- `tmux attach -t sessionName` to attach session named `sessionName`

## zsh

`zsh` is an alternative to `bash`.
Coupled with a configuration manager such as [oh-my-zsh](https://github.com/robbyrussell/oh-my-zsh) or [prezto](https://github.com/sorin-ionescu/prezto) it becomes quite powerful.
Some modules that I like to include (that are available in both tools) are:

- `git` for git completion and branch notification
- `syntax-highlighting` for syntax highlighting (will highlight incorrect commands)
- as well as the defaults included with `prezto`

With the setup you can do things like this:

- `cd a/s/R/s` and press `TAB` to expand the file `analysis/stat243/R/solution.R`
- Type `git ` followed by `TAB` twice and get a list of `git` commands (`commit`, `ls`, `push`, etc.)

## Make and friends

If you are interested in creating reproducible pipelines (e.g. clean data, do modeling, generate a report) using a tool such as `Make` is essential.
`Make` is nice because if exists on pretty much every (Unix-like) system, but has many shortcomings.
There are many alternatives, my personal favorite is [Snakemake](https://bitbucket.org/snakemake/snakemake/wiki/Home).
A long list of workflow tools can be found [here](https://github.com/common-workflow-language/common-workflow-language/wiki/Existing-Workflow-systems)

## ipython and notebooks

`ipython` is a good alternative to `python`.

- It has tab completion and a searchable documentation (`?`).
- Pasting can be made easier using:
  - `%paste` on a local computer (will paste from clipboard)
  - `%cpaste` on a remote computer (will allow you to paste from your clipboard)
- The history command is kind of cool
- [Jupyter notebook](http://jupyter.org/) is an alternative to `knitr` which supports many languages ([including `R`](http://blog.revolutionanalytics.com/2015/09/using-r-with-jupyter-notebooks.html))
