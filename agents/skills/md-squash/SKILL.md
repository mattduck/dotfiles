---
name: md-squash
description: Squash branch commits into one coherent commit with standard layout
disable-model-invocation: false
---

Squash all commits on the current branch (relative to a base branch) into a
single well-structured commit. Follow every step below in order.

## Arguments

The user may provide arguments after `/md-squash`:

- A branch name to rebase/squash against (default: `master`).
- Additional context to include in the commit message (free text).

Parse these from the arguments. Examples:
- `/md-squash` — squash against master, no extra context.
- `/md-squash main` — squash against main.
- `/md-squash master this work adds a new caching layer` — squash
  against master with extra context.

## Conventions

- Use commitlint keywords: build chore ci docs feat fix perf refactor revert
  style test
- If we're operating on a known ticket ID, use the format
  "feat: MYTICKET-123 here's my commit title".
- Where below I refer to "you" or "Claude", that means you the agent. "User"
  or "me" means me.
- Wrap output at 72 columns. Do NOT attempt to wrap lines yourself —
  the ,wrap-message command handles this. See Step 6 for details.
- TONE: never use superlatives or subjective adjectives to describe
  changes. No "comprehensive", "robust", "elegant", "streamlined",
  "enhanced", etc. Stick to plain factual descriptions. Write "add
  tests for X" not "add comprehensive tests for X". This applies to
  all sections of the commit message.

## Step 1: Gather information

1. Determine the base branch (from arguments, default `master`).
2. Run `git merge-base HEAD <base-branch>` to find the fork point.
3. Run `git log --format='%H %h %s' <merge-base>..HEAD` to get all commits
   to squash. If there are 0 or 1 commits, abort — nothing to squash.
4. For each commit, run `git log -1 --format='%B' <sha>` to get the full
   commit message body.
5. Save all of this (full SHAs, short hashes, and complete log messages)
   to a temp file. You will need this later for git notes.
6. Run `git diff <merge-base>..HEAD` to see the full diff.

## Step 2: Create backup branch

Before any destructive operation, create a backup branch:

```
git branch "$(git rev-parse --abbrev-ref HEAD)-squash-backup-$(date +%Y%m%d-%H%M%S)"
```

Confirm the branch was created successfully before proceeding.

## Step 3: Determine commit keyword

Look at the commitlint keywords used across all the commits being squashed.
If they all use the same keyword, use that. If they differ, ask the user
which keyword to use, showing them the list of keywords found.

## Step 4: Check for ticket ID

Look at the commit messages for a ticket ID (e.g. PROJ-123). If exactly
one ticket ID is found consistently, use it. If multiple conflicting
ticket IDs are found, ask the user which one to use. If none is found,
ask the user:

> No ticket ID found in the commit messages. Is there a relevant ticket
> to include in the title?

## Step 5: Build the squashed commit message

Concatenate all the individual commit messages' sections together, then
produce ONE commit message with the following template. Apply the pruning
rules described in each section.

```
<keyword>: <optional ticket> <short summary>

Changes:
- Bullet points describing what changed in the code.
- Be literal and precise.
- PRUNING: remove intermediate issues that were resolved
  within the branch (e.g. fixing a build broken by an earlier
  commit, intermediate refactoring of something introduced in
  this branch). These are irrelevant to the final state.
- PRUNING: omit tiny details — this should NOT reproduce
  everything in the diff.
- LIMIT: maximum 8 bullet points. Prioritise the most
  important, user-visible, or architecturally significant
  changes.

Context:
- Why we're making the change: the top-level reasoning, goal,
  and key takeaways.
- LIMIT: maximum 3 bullet points. Focus on the high-level
  "why" and "what we're trying to achieve".
- If the top-level reasoning is not extremely clear from the
  commit messages and diff, ASK the user before writing this
  section.

Review:
- Aggregate all review items from the individual commits.
- COV: aggregate all test-coverage items into 1-2 bullets max.
- Keep all other review keywords (RISK, MISSING, DECISION,
  LATER, REFACTOR), EXCEPT remove items about intermediate
  issues that have since been resolved within the branch.

Prompts <short-hash> (<FULL original commit title>):
- "Verbatim user quotes from this commit's prompts section."
- Preserve question/answer nesting as in md-commit format.

Prompts <short-hash> (<FULL original commit title>):
- "Verbatim user quotes from this commit's prompts section."

... (one Prompts section per original commit that had prompts)

IMPORTANT: The parenthesised title in each Prompts heading MUST be
the FULL original commit title line, including the keyword and
ticket ID. For example:
  Prompts 05115fd (feat: IMP-30 improve SQS honeycomb tracing):
NOT:
  Prompts 05115fd (improve SQS honeycomb tracing):
Do NOT strip the keyword or ticket from the title.
```

If the prompts were part of a question/answer, include them like this:

### The user asked the question

Prompts <hash> (<summary>):
...
- "This is my question?"
  - [This summarises your answer]

### You asked the question

Prompts <hash> (<summary>):
...
- [This is your question?]
  - "This is my answer"

## Step 6: Perform the squash

IMPORTANT: You MUST run ,wrap-message to wrap the message. Do NOT
attempt to wrap lines yourself — the LLM cannot reliably wrap text.

1. Compose your draft commit message with NO attempt at wrapping —
   just write natural-length lines.

2. Reset and commit in one go:
   ```bash
   git reset --soft <merge-base> && ,wrap-message <<'DRAFT' | git commit -F -
   your message here
   DRAFT
   ```

3. If the user wants changes after seeing the commit, amend by
   repeating from step 1 with `git commit --amend -F -` instead.

Do NOT show the user text you composed yourself — only show the
output of ,wrap-message.

## Step 7: Store original commits as git notes


Using the temp file saved in Step 1, add a git note to the new squashed
commit containing the original commit information:

```bash
git notes add -m "<note content>" HEAD
```

The note should contain:

```
Squashed from the following commits:

<full SHA> <short hash> <oneline summary>
<full SHA> <short hash> <oneline summary>
...

--- Full original commit messages ---

commit <full SHA> (<short hash>)
<full commit message body>

commit <full SHA> (<short hash>)
<full commit message body>
...
```

## Step 8: Verify

Run `git log -1` and `git notes show HEAD` to confirm everything looks
correct. Report the backup branch name to the user.
