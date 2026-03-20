---
name: md-commit
description: Git commit using standard layout
disable-model-invocation: false
---

Create a git commit. The context for what needs committing should be clear from
the existing conversation. Follow the conventions and template below:

- Use commitlint keywords: build chore ci docs feat fix perf refactor revert style test

- If we're operating on a known ticket ID, use the format "feat: MYTICKET-123 here's my commit title".

- Where below I refer to "you" or "Claude", that means you the agent. "User" or "me" means me.

- Wrap output at 72 columns. Do NOT attempt to wrap lines yourself —
  the ,wrap-message command handles this. See the commit instructions
  below for details.

- TONE: never use superlatives or subjective adjectives to describe
  changes. No "comprehensive", "robust", "elegant", "streamlined",
  "enhanced", etc. Stick to plain factual descriptions. Write "add
  tests for X" not "add comprehensive tests for X". This applies to
  all sections of the commit message.

## Commit message template

```
<keyword>: <optional ticket> <short summary>

Changes:
- Bullet points describing what changed in the code.
- Be literal and precise.

Context:
- Why we're making the change: the goal and any key takeaways from our chat.

Review:
- Keywords to, where appropriate, tell a reviewer or future reader about the
  change to help them understand it. You should include many items when there are
  multiple things to say, but don't go very granular on tiny changes.
- COV: what is tested and what isn't. Always include this.
- RISK: any risk we're introducing with this change or associated with this
  change.
- MISSING: anything deliberately missing.
- DECISION: any key decision that was made, eg. a tradeoff or design decision.
- LATER: follow-up tasks that will or may be required.
- REFACTOR: where we know we're introducing something that will want
  refactoring.

Prompts:
- "This section includes verbatim quotes from the user."
- "Start with the initial prompt(s) that the user provided that led to this work item."
- "Include all quotes from the user verbatim (but wrapped), and in chronological order".
- "If the user was responding to your question (or vice versa), include the question
  and answer as a nested bullet. See example below"

```

If the prompts were part of a question/answer, then include them like this:

### The user asked the question

Prompts:
...
- "This is my question?"
  - [This summarises your answer]

### You asked the question

Prompts:
...
- [This is your question?]
  - "This is my answer"

## Committing

IMPORTANT: You MUST run ,wrap-message and show its output. Do NOT
show your own draft to the user — the LLM cannot reliably wrap text.
Follow these exact steps in order:

1. Compose your draft commit message with NO attempt at wrapping —
   just write natural-length lines.

2. Wrap and commit in a single command using a heredoc:
   ```bash
   ,wrap-message <<'DRAFT' | git commit -F -
   your message here
   DRAFT
   ```
   This both wraps the message and commits it in one step. The
   wrapped message will be visible in the command output.

3. If the user wants changes after seeing the commit, amend by
   repeating from step 1 with `git commit --amend -F -` instead.

Do NOT skip any of these steps. Do NOT show the user text you
composed yourself — only show the output of ,wrap-message.
