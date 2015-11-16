# Discussion section 11: `git` merging and collaboration

Today we will be practicing working remotely with github and collaborators.

# Remote working exercise

For this exercise there will be 2 users `userA` and `userB`.

## 1. `userA`: create the repository

Go to [github.com](https://github.com) and create a new public repository called 'practiceMerging'.
Make sure to create a local repository using the instructions provided by github.

Invite `userB` to become a collaborator:

1. on the repositories webpage, click the 'Settings' tab
2. click 'collaborators' on the left
3. type their github username and they should be able to commit

## 2. `userA`: add a file

Add a file called "README.md". Put some text into it. Make sure to commit and push:

```
git add README.md
git commit -m 'userA adds README'
git push origin master
```

## 3. Making concurrent changes

Here we will make concurrent changes and see how git behaves.

### 3a. `userA`: make some more modifications

Open up "README.md" and make some more modifications.
They can be anything you want as long as they are __AFTER__ the existing text.

Commit the changes, but __do not push__:

```
git commit -m 'userA modifies README'
```

### 3b. `userB`: clone the repository and make changes

Now that you have been added as a collaborator, clone the repository and make your own changes.
Make sure that these changes are __BEFORE__ the existing text.

Commit these changes and __do not push__:

```
git commit -m 'userB modifies README concurrently'
```

## 4. Push changes

### 4a. `userA`: push changes

Now that both of you have concurrently made changes, push your changes to the central repository.

```
git push origin master
```

Did everything go okay?

### 4b. `userB`: push changes

`userA` has pushed their changes and now you will attempt to do so as well.

__(Q1)__ Before you proceed, talk to `userA` and try to predict what will happen.
Will git automatically merge the 2 commits?

```
git push origin master
```

TODO: put a picture of something

## 5. Resolve any issues

You should have observed that the push was not accepted at the previous stage and gotten a message something like this:

```
To git@github.com:berkeley-stat243/stat243-fall-2015.git
 ! [rejected]        master -> master (fetch first)
error: failed to push some refs to 'git@github.com:berkeley-stat243/stat243-fall-2015.git'
hint: Updates were rejected because the remote contains work that you do
hint: not have locally. This is usually caused by another repository pushing
hint: to the same ref. You may want to first integrate the remote changes
hint: (e.g., 'git pull ...') before pushing again.
hint: See the 'Note about fast-forwards' in 'git push --help' for details.
```

### 5b. `userB`: the `push` issues

The error message is related to the fact that both commits have the same parent, but the branch diverged and there is no clear place to put the commit for git.
The way to deal with this is to do a `git pull` and update your existing branch.

```
git pull origin master
```

What did the messages say?

If you were careful and put text above and below the text, it should have merged automatically.
If you are not careful and modified the same area of the file, you will get error the asks you to merge it manually.

If you followed all the directions, you should now be able to push:

```
git push origin master
```

TODO: add a note about looking at the graph

__(Q2)__ How can you prevent this from happening in the future?

## 6. Concurrent conflicting changes

Here we will explore making changes that conflict and how to resolve these conflicts.

### 6b. `userB`: make changes to first line

Make some changes to the first line of code.
It can be anything as long as it is on the first line of code.
After you are done, commit and push to the central repository:

```
git commit -a -m 'userB making some changes to the first line'
git push origin master
```
### 6a. `userA`: also make some changes to the first line

Also make some changes to the first line of code.
It can also be anything as long as it is on the first line of code.
After you are done, commit the changes.
After the commit, do a `git pull` to make sure that everything merges in nicely.

```
git commit -a -m 'userA making concurrent changes in the first line'
git pull origin master
```

You should have received an error telling you that there are some conflicts that must be manually merged in:

```
TODO: put conflict message here
```

## 7. Dealing with conflicts

Usually if you are careful, conflicts will not arise.
When they do, you will have to manually merge them in and do so carefully.

## 7a. `userA`: manual merge in the changes

To get a high-level view of the changes that must merged in, you can do a `git diff`.
When there is a conflict, git will place the difference in the file in line.
The difference usually looks like this:

```
<<<<<<< HEAD
More work on the master branch...
=======
This is going to be a problem...
>>>>>>> trouble
```

Where the first block denotes the code that is in your current branch and the second block denotes the code that is in the branch you are trying to merge in.
When fixing this problem, you must remove all of the markup (`<<<`, `===`, `>>>`) and make the code functional.
When you are done, make sure to test the code (if relevant) then you can commit.

```
git commit -a -m 'manual resolution of merge'
```

There are some tools that can help with dealing with the merge.
In atom, there is a tool called [merge-conflicts](https://atom.io/packages/merge-conflicts) that highlights the differences and lets you simply click to keep a particular block.

# resources
- [difference between merge and rebase](http://stackoverflow.com/questions/16666089/whats-the-difference-between-git-merge-and-git-rebase)
- [when to use merge and when to use rebase](http://www.derekgourlay.com/archives/428)
- https://www.atlassian.com/git/tutorials/comparing-workflows/centralized-workflow
