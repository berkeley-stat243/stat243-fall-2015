# Section 01: Git basics and formatting

## Git

First, we will go through some git basics. This will hopefully help prevent the
['final.doc'](http://www.phdcomics.com/comics/archive.php?comicid=1531) version
control style. We will refer to the tutorial by Jarrod Millman available on the
[Berkeley SCF github
repo](https://github.com/berkeley-scf/tutorial-git-basics).

```
git clone https://github.com/berkeley-scf/tutorial-git-basics
```

We will not cover the entire tutorial, only the basics. Please complete it on
your own time.

### Better logging

My personal favorite:

```
git config --global alias.lg "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit"
```

This will add:

```
[alias]
	lg = log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit
```

to your `~/.gitconfig`. You can now type:

```
git lg
```

### Code organization

You might find the
[ProjectTemplate](http://www.johnmyleswhite.com/notebook/2010/08/26/projecttemplate/)
package in R helpful.

## Submitting problem sets

We will go over submitting problem sets. You can find more info at
`howtos/submitting-electronically.txt` in this repo.

## Formatting

We will go over how to format problem sets using LaTeX + knitr or RMarkdown.

Some references we might look at:

- [Dynamic docs](https://github.com/berkeley-scf/tutorial-dynamic-docs)
- [Knitr in a knutshell](http://kbroman.org/knitr_knutshell/)
