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
- Summarise what changed — don't reproduce the diff.
- Aim for 1–3 bullets. More than 3 is usually too granular
  unless it's a genuinely large change.
- Don't list every function rename, every file touched, or
  every small edit. Group related changes into one bullet.
- Name specific functions/files only when that's the point
  of the change (e.g. "rename fetchUser to getUser").

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
- "Only include this section if my own messages are in your context and you can
  quote them. See 'When there are no prompts to quote' below."

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

## When there are no prompts to quote

The Prompts section records what I actually said, so include it **only if my
own messages are present in your context and you can quote them verbatim.**

**Never write a Prompts bullet you cannot quote from something in front of
you.** Reconstructing what I probably said, or paraphrasing a summary and
presenting it as a quote, invents provenance — worse than having no section
at all.

You will have nothing to quote when:

- you are a subagent — you were handed a task, not a conversation
- the session was compacted or resumed and my verbatim messages are gone
- the commit came from a scheduled or otherwise autonomous run

In that case replace the section with what you *can* quote:

```
Task:
- "<the instruction you were given, verbatim>"
- Delegated by the main session; my own prompts are not in this agent's
  context.
```

A partly-compacted session is the awkward one: quote what you still have
under Prompts, and add a bullet saying earlier messages were lost to
compaction. Don't let a summary pass as a quote.

If you have neither my prompts nor a task instruction, omit both sections
and say so when you report the commit, rather than filling the gap.

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
