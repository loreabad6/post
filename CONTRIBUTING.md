<!-- from sfnetworks, brin back how-to contribute in time -->

## Structured commit messages

When commiting changes with `git commit` we try to use structured commit messages, adapted from https://www.conventionalcommits.org/. The first line of commit message should have the following format:

```
<type>: <summary>
```

The summary should be short (preferably < 50 characters), starting with an upper case, and written in present tense. If the commit references a specific issue, include `Refs #<issue number>` in the summary. If the issue is a bug report, you may also use `Fix #<issue number>` such that the issue gets closed automatically.

The type should be one of the defined types listed below. If you feel artistic, you can end the commit message with the emoji belonging to the type :sunglasses:.

- **feat**: Implementation of a new feature. `:gift:` :gift:
- **fix**: A bug fix. `:wrench:` :wrench:
- **style**: Changes to code formatting. No change to program logic. `:art:` :art:
- **refactor**: Changes to code which do not change behaviour, e.g. renaming variables or splitting functions. `:construction:` :construction:
- **docs**: Adding, removing or updating user documentation or to code comments. `:books:` :books:
- **logs**: Adding, removing or updating log messages. `:sound:` :sound:
- **test**: Adding, removing or updating tests. No changes to user code. `:test_tube:` :test_tube:
- **cicd**: Adding, removing or updating CI/CD workflows. No changes to user code. `:robot:` :robot:
- **deps**: Adding, removing or updating dependencies. `:couple:` :couple:
- **release**: Preparing a release, e.g. updating version numbers. `:bookmark:` :bookmark:
- **repo**: Changes to the repository that do not involve code/documentation, e.g. adding templates or community files. `:package:` :package:

Example commit messages are:

```
git commit -m 'feat: Add bar parameter to foo(). Refs #10 :gift:'
git commit -m 'fix: Include type checking in foo(). Fix #12 :wrench:'
```
