# Contributing to lhCore

:tada: First off, thanks for contributing! :tada:

[1. Definitions](#1-definitions)

[2. Types of contributions](#2-types-of-contributions)

[3. Getting started](#3-getting-started)

[4. Fixing a bug in `lhCore`](#4-fixing-a-bug-in-lhCore)

[5. Contributing or modifying a lesson to `lhLessons`](#5-contributing-or-modifying-a-lesson-to-lhLessons)

[6. Adding a course](#6-adding-a-course)

[7. Materials Development Workflow](#7-materials-development-workflow)

[8. Building the book](#8-building-the-book)

## 1. Definitions

- **forked repository**: this refers to contributors' forked copy of the
`nceas-learning-hub/lhCore` repository which is specified with the path `{yourUsername}/lhCore`
- **upstream repository**: this refers to the original repository that
contributors fork from which is `nceas-learning-hub/lhCore` 
- **origin or remote repository**: this refers to your forked repository on GitHub

## 2. Types of contributions

We welcome all types of contributions, including bug fixes, new lessons, lesson
improvements, and new training events.

- Report a bug or issue to our [issue tracker](https://github.com/nceas-learning-hub/lhCore/issues)
- Fix a bug or add a feature in `lhCore` with a Pull Request
- Contribute a lesson to `lhLessons` with a Pull Request
- Add a training event by creating a new repository using the functionality of `lhCore`.

## 3. Getting started

For this repository, external contributors use a [forking workflow](https://learning.nceas.ucsb.edu/2023-04-lhCore/session_17.html#forking-workflow). This means contributors make edits to files from the `nceas-learning-hub/lhCore` repository in their forked repository `{yourUsername}/lhCore`, and to merge these edits to `nceas-learning-hub/lhCore` (the original repository) they open a pull request (PR). 

(Internal contributors can use a forking workflow or a branching workflow with
Pull Requests)

We use pull requests to review and discuss changes before merging contributor's
additions or new features into `nceas-learning-hub/lhCore`. See documentation on pull requests [here](https://help.github.com/articles/about-pull-requests/) and [here](https://www.atlassian.com/git/tutorials/making-a-pull-request).

To contribute to this repository, use these steps to get started:

1. `Fork` the `nceas-learning-hub/lhCore` repository by clicking the "Fork" button 
    at the top right of the repository 
    a. In the "Create a new fork" screen make sure the Owner is set to your 
       GitHub user and don't change the repository name
    b. If you need access to more branches than `main` in the `nceas-learning-hub/lhCore` 
       repository, uncheck "Copy the `main` branch only"
2. Clone `{yourUsername}/lhCore` (this is your forked copy) into your workspace onto your computer
3. Sync your forked repository with the upstream repository. It's important to 
   note that no matter how you are contributing, you should check that the branch 
   you're working on in your forked repository is in the same state as the same 
   branch in the upstream repository. [See section 6 for more detailed instructions on how to sync your forked respository with the upstream repository](#6-syncing-your-forked-repository-with-the-upstream-repository)

## 4. Fixing a bug in `lhCore`

1. Add an [issue](https://github.com/nceas-learning-hub/lhCore/issues) describing 
   your planned changes, or add a comment to an existing issue
2. **Make sure you're working in the right branch and sync changes / pull from the 
   upstream respository before you start making changes.** [See section 6](#6-syncing-your-forked-repository-with-the-upstream-repository) 
    a. Once you've identified which branch you want to work in check that the 
       branch in your forked repository is in "Sync" with `nceas-learning-hub/lhCore`
       (aka the upstream repository) *before* you start making changes. This will 
       ensure that your forked repository (`{yourUsername}/lhCore`) and the 
       upstream repository (`nceas-learning-hub/lhCore`) are in the same state 
       and will prevent merge conflicts from occurring when you PR.
4. Make your changes and `push` changes to your forked repository 
5. Open a Pull Request (PR). Make sure that the base repository is 
   `nceas-learning-hub/lhCore` and the head repository is `{yourUsername}/lhCore`. 
   It's also important to double check that the base branch of `nceas-learning-hub/lhCore` 
   matches up with the compare branch in `{yourUsername}/lhCore`. Aka make sure that 
   you're PRing (and ultimately, merging) the right branches
6. Assign someone from the NCEAS Learning Hub GitHub Organization to review your changes
7. Your reviewer may request changes before merging in the changes and closing the PR. 
   This discussion can take place in the "Conversation" tab of the PR webpage
8. Once changes have been confirmed, the reviewer will merge in the changes and 
   close the PR, and you're done!

## 5. Contributing or modifying a lesson to `lhLessons`

Follow the same workflow as noted above for fixing a bug, but use the `lhLessons`
repository rather than lhCore.  

Add an [issue](https://github.com/nceas-learning-hub/lhLessons/issues) describing
planned lesson, or add a comment to an existing issue.

See further details in the [`lhLessons` contributing document](https://github.com/nceas-learning-hub/lhLessons/blob/main/contributing.md).


## 6. Adding a course

Course materials are contained in repositories in the `nceas-learning-hub` Github
Organization.  The functionality of the `lhCore` package sets up the structure of
the course, and pulls lesson content (including images and data as needed) from
the `lhLessons` R package.

If you wish to customize your course, create the course with the provided lessons
and then simply modify the Quarto documents in the `lessons` folder, and put associated
images and data into the `images` and `data` folders inside the appropriate subfolder.

You are welcome to use `lhCore` and `lhLessons` to create training events in a
different location or organization.  If you wish to have your training materials
hosted on the `nceas-learning-hub` organization, contact one of the
owners/administrators of the organization.

Because the training event repositories are independent of `lhCore` and `lhLessons`
packages, modifications (e.g., bug fixes, modifications to content) cannot be
automatically synced back to those package repositories.  Instead, use the fork
(or branch) workflow noted above.

## 7. Materials Development Workflow

Lesson materials development all occurs in conjunction with the `lhLessons` repository
and R package.  Please see the [`lhLessons` contributing document](https://github.com/nceas-learning-hub/lhLessons/blob/main/contributing.md)
for more information.

## 8. Building the book

While there are many workflows for building the qmd files into the rendered Quarto book,
probably the simplest to use during authoring is to click on the Build tab and select Render Book,
assuming your working directory is the root of the lhCore repository. This
will load the rendered view into the RStudio `Viewer` pane (or an external browser).

