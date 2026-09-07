---
name: gnosis-themata
description: "Use when creating or repairing Gnosis themata."
---

# Gnosis thema authoring

## When to use

Create, coauthor, or repair source-linked retrieval questions in Gnosis.
This skill covers content and safe authoring, not software development,
clinical advice, or automatic grading of a learner.

## Procedure

1. Establish the requested concepts and source material. Read the notes
   repository's rules and actual passages. Search existing nodes and linked
   themata before creating anything. Preserve IDs, accurate learner wording,
   and existing drafts. Done: sources and candidate reuse are identified.
2. Choose a small set of consequential retrieval targets. Use mechanisms,
   distinctions, relationships, or predictions, not one card per sentence.
   Separate essential coverage from optional detail. Done: each proposed
   thema has a clear purpose and source-supported answer.
3. Draft with the rules below. For coauthoring, show the draft and agree its
   content before saving. Pilot a small batch before scaling a new format.
   Done: the learner can inspect questions, answers, and supporting context;
   proposed content is not reported as saved.
4. Use the installed version's native editor and supported writers. Inspect
   current source when automating; do not copy old SQLite import schemas or
   assume a historical Org template is current. Round-trip generated drafts
   through the real renderer/parser before replacing buffers or saving.
   Done: type, question, choices/hints, answer, tags, and links survive.
5. Save only authorized content. Read back saved IDs, fields, and source links
   from Gnosis. If a write returns uncertainly, inspect before retrying to
   avoid duplicates. Done: every intended thema is accounted for, failures
   are explicit, and unrelated drafts/history remain intact.
6. Try the agreed questions with the learner. Distinguish ambiguous wording,
   missing knowledge, and grading mismatch. Repair with authorization; a
   different retrieval target may need a new thema rather than overwriting
   an old target's history. Done: observed issues and unfinished work are
   recorded in the study checkpoint, not in this skill.

## Question design

- One independently checkable retrieval target per thema. Include enough
  context to make the answer unambiguous without exposing it.
- Prefer short clozes for focused recall: answers usually one or two words,
  at most three. Rewrite the sentence rather than blanking a long list.
- Use MCQs for useful distinctions. Supply one defensible answer and plausible
  alternatives of comparable specificity. Recognition is not free recall.
- Use basic questions when explanation or a relation matters. Match the
  expected answer to the actual grading mode; do not demand essay wording
  through short-string matching.
- Put concise explanation in parathema, including why a tempting alternative
  is wrong when useful. Cite uncertainty or conflicting sources rather than
  turning an unresolved claim into an absolute answer.
- For anatomy, preserve side, orientation, attachment, relation, and movement
  distinctions. A text question does not prove structure identification;
  use an actual inspected image when testing visual recognition.

## Sources and reuse

- Link sources in **keimenon and/or parathema**. Either field is sufficient;
  neither must duplicate the other's links. Multiple relevant sources are
  welcome: one thema can belong to several concepts or topic selections.
- Default supporting links to parathema, visible after answering. Use the
  exact relevant node or ID-bearing heading, with a descriptive title such
  as `Bone: Growth` or `Endochondral ossification`, not `Source topic`,
  `Source section`, or `See here`.
- In keimenon, integrate a link naturally into the question when its label
  is useful context, for example `[[id:<uuid>][Parasite name]] infects ...`.
  This is an illustrative placeholder, not a real ID. Do not append generic
  source footers or link a term whose label gives away the requested answer.
- Keep questions independent of a course, lecture, ticket, or syllabus number.
  A bone attachment is the same fact in osteology, myology, and regional
  anatomy. Reuse one thema and link the relevant concepts instead of making
  subject-specific copies. Syllabus links or tags may organize selection;
  they do not define the question's meaning or imply complete coverage.
- Preserve existing Org IDs. Resolve descriptive labels to actual nodes;
  never invent IDs or use attachment/file links in place of node identity.
  Links establish association, not proof that every linked source supports
  every sentence. Read each supporting passage.

## Pitfalls

- Authoring, seeing an answer, and immediate retry success are not evidence
  of durable retention. Keep whole-topic recall separate from card coverage.
- Do not manufacture learner grades, alter schedules to test authoring, or
  launch practice merely because saving succeeded. Agree practice scope and
  policy separately; preserve unfinished sessions.
- Do not load development code or migrate real data to obtain a missing
  authoring API. Report the capability blocker.
- Keep personal progress, local paths, and live session IDs out of this skill.

## Verification

Before saving, inspect every draft for source accuracy, ambiguity, answer
leakage, useful link labels, and duplicates across topics. Confirm the real
codec preserves all intended fields, including short cloze answers and
multiple source links. After saving, verify exact records, distinguish new
from reused themata, and report remaining coverage gaps. Never claim learning
from a successful save.
