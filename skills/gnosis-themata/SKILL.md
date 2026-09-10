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
3. Draft with the rules below. For interactive coauthoring, agree the draft
   before saving. When the learner authorizes autonomous preparation, the
   supervising agent owns content review, correction, and verified saving;
   do not return that work to the learner. Pilot new formats when useful.
   Done: the requested scope is reviewed, and proposed versus saved content
   remains explicit.
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

## Typed alternatives and visual targets

- For basic typed responses, image-occlusion and model-name, keep one canonical
  Answer and put explicitly accepted spellings in `** Accepted aliases`, one
  `- ` list item per nonempty single-line spelling. Never infer synonyms or
  move aliases into the Answer list. The ordinary configured text comparator
  checks canonical or aliases; feedback and input-method choice use canonical.
  MCQ, clozes, image-region and model Find do not accept aliases. Double
  authoring applies aliases only to its forward basic question.
- A saved full draft replaces aliases: an absent/empty section clears them.
  Programmatic update omission preserves them, explicit nil clears. Verify
  both canonical and aliases after save/reopen. Schema 9 stores them;
  portable content format 3 carries them, while formats 1/2 supply none.
- Use `gnosis-add-image-thema` for visual region/occlusion questions. Group
  repeated labels under one stable target: select a rectangle and Shift-drag
  another; ordinary drag creates a new target. Use `n`/`p` to reach overlaps,
  `r` to reassign, `l` to rename, `d` for one rectangle and `D` for the target.
  Occlusion's Image resource holds resource, target, then `hide-target` or
  `hide-all`; old cards default to hide-target. Annotate every answer-bearing
  label: hide-all masks annotated rectangles, not unannotated text or OCR.
- Use `gnosis-add-model-thema` for Find and `gnosis-add-model-name-thema` for
  Name. Whole objects, surface points with explicit original-unit tolerance,
  and triangle regions are targets, not alternate answer spellings. In native
  authoring, click a surface, then `p` creates a point, `m` moves it, `g` creates
  a region, `a` toggles a triangle, `t` selects a target, `e` edits label/tolerance
  and `d` removes it. `RET` accepts target/view; `q`/`C-g` cancels. Save the normal
  draft with `C-c C-c`; `C-c C-a` reattaches/reframes. Inspect source anatomy
  and coordinate units before setting tolerance; do not invent precision.
- Find Answer is a target ID; its resource field is resource/yaw/pitch/zoom.
  Name Answer is canonical text; its resource field adds target ID immediately
  after resource. Name highlights the target without a label; inspection and
  rotation precede `RET` to type. Clicks never grade. Check for cue leakage and
  ambiguous overlapping targets before saving either mode.
- Image and scene edits publish immutable revisions; old cards keep old bytes.
  Reimport changed geometry and explicitly attach it; do not reuse face indices
  across changed topology. Content exchange does not bundle managed media,
  including Name. Back up database plus assets separately from the Org vault.
  Native image decoding and the optional matched C3D2 canvas backend are
  capability requirements, not permission to install or activate on a learner.
  See `docs/gnosis.org` and `optional/canvas-3d/README.md` for setup/support limits.

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

## Tags and bulk coverage

- Establish shared collection and subject tags before parallel authoring.
  Reuse the notes repository's vocabulary. Use lowercase Org-compatible
  tags with underscores, not hyphens or spaces.
- Use a few stable conceptual tags for useful filtering. Avoid automatic
  per-topic numbered tags, synonyms, and one-off tags for every card.
  Source-node links own exact topic membership; tags organize collections.
- On reuse, add required collection/subject tags without removing unrelated
  tags or changing review history. Deduplicate identical targets across
  topics and preserve their multiple source associations.
- Enumerate every requested syllabus topic and its essential components.
  Map components to verified new/reused thema IDs. A token card per topic
  or an arbitrary per-topic quota does not establish coverage. State visual
  or source limitations separately; do not silently reduce the task to a
  starter set. The supervising agent reviews worker output before saving.

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
