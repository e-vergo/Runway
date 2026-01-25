/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Verso.Doc
import Verso.Doc.Elab
import Runway.Genre

/-!
# Blueprint Document DSL

Provides Blueprint-specific document extensions for Verso documents.

## Usage in Verso Documents

Within a `#doc (Blueprint) "Title" =>` document, you can use:

### Block-level: Node embedding
```
{node "mytheorem"}
```
Embeds the full statement + proof for a blueprint node.

### Block-level: Proof only
```
{proof "mylemma"}
```
Embeds just the proof for a blueprint node.

### Block-level: Dependency graph
```
{depGraph}
```
Embeds the dependency graph visualization.

### Inline: Node reference
```
See {nodeRef "mytheorem"}[link text] for the main result.
```
Creates a link to the referenced node.

### Inline: Lean declaration reference
```
The function {leanRef `Nat.add}[Nat.add] adds natural numbers.
```
Creates a link to the Lean documentation.

## Programmatic Construction

For building documents programmatically without Verso syntax:

```lean
open Runway.Doc in
def myDoc : Verso.Doc.Part Blueprint := mkSection "My Chapter" #[
  nodeBlock "mytheorem",
  para "This follows from the previous result.",
  nodeBlock "mylemma"
]
```
-/

namespace Runway.Doc

open Lean
open Verso.Doc.Elab
open Verso.ArgParse

/-! ## Block-level Extensions -/

/-- Create a block that embeds a blueprint node by its label -/
def nodeBlock (label : String) : Verso.Doc.Block Blueprint :=
  Verso.Doc.Block.other (Blueprint.BlockExt.node label) #[]

/-- Create a block that embeds just the proof of a blueprint node -/
def proofBlock (label : String) : Verso.Doc.Block Blueprint :=
  Verso.Doc.Block.other (Blueprint.BlockExt.proof label) #[]

/-- Create a block for the dependency graph visualization -/
def depGraphBlock : Verso.Doc.Block Blueprint :=
  Verso.Doc.Block.other Blueprint.BlockExt.depGraph #[]

/-- Create a code block with syntax highlighting -/
def codeBlock (language : String) (content : String) : Verso.Doc.Block Blueprint :=
  Verso.Doc.Block.other (Blueprint.BlockExt.code language content) #[]

/-- Create a div wrapper with CSS classes -/
def divBlock (classes : String) (content : Array (Verso.Doc.Block Blueprint)) : Verso.Doc.Block Blueprint :=
  Verso.Doc.Block.other (Blueprint.BlockExt.htmlDiv classes) content

/-- Create a theorem/lemma/definition environment -/
def theoremEnvBlock (envType : String) (label : Option String) (content : Array (Verso.Doc.Block Blueprint)) : Verso.Doc.Block Blueprint :=
  Verso.Doc.Block.other (Blueprint.BlockExt.theoremEnv envType label) content

/-! ## Inline-level Extensions -/

/-- Create an inline reference to a blueprint node -/
def nodeRefInline (label : String) : Verso.Doc.Inline Blueprint :=
  Verso.Doc.Inline.other (Blueprint.InlineExt.nodeRef label) #[]

/-- Create an inline reference to a Lean declaration -/
def leanRefInline (name : Name) : Verso.Doc.Inline Blueprint :=
  Verso.Doc.Inline.other (Blueprint.InlineExt.leanRef name) #[]

/-- Create inline LaTeX math -/
def mathInline (latex : String) : Verso.Doc.Inline Blueprint :=
  Verso.Doc.Inline.other (Blueprint.InlineExt.math latex) #[]

/-- Create an inline span with CSS classes -/
def spanInline (classes : String) (content : Array (Verso.Doc.Inline Blueprint)) : Verso.Doc.Inline Blueprint :=
  Verso.Doc.Inline.other (Blueprint.InlineExt.htmlSpan classes) content

/-! ## Verso Block Command Expanders

These allow using `{node "label"}` etc. within Verso documents.
-/

-- Use abbrev to preserve type class instances like Quote
abbrev node.Args := String
instance : FromArgs node.Args DocElabM := ⟨.positional `label .string "The blueprint node label"⟩

/-- Block command for `{node "label"}` - embeds a blueprint node -/
@[block_command]
def node : BlockCommandOf node.Args := fun label => do
  ``(Verso.Doc.Block.other (Blueprint.BlockExt.node $(quote label)) #[])

abbrev proof.Args := String
instance : FromArgs proof.Args DocElabM := ⟨.positional `label .string "The blueprint node label"⟩

/-- Block command for `{proof "label"}` - embeds just the proof -/
@[block_command]
def proof : BlockCommandOf proof.Args := fun label => do
  ``(Verso.Doc.Block.other (Blueprint.BlockExt.proof $(quote label)) #[])

/-- Block command for `{depGraph}` - embeds the dependency graph -/
@[block_command]
def depGraph : BlockCommandOf Unit := fun () => do
  ``(Verso.Doc.Block.other Blueprint.BlockExt.depGraph #[])

abbrev htmlDiv.Args := String
instance : FromArgs htmlDiv.Args DocElabM := ⟨.positional `classes .string "CSS classes for the div"⟩

/-- Block command for `{htmlDiv "classes"}` -/
@[block_command]
def htmlDiv : BlockCommandOf htmlDiv.Args := fun classes => do
  ``(Verso.Doc.Block.other (Blueprint.BlockExt.htmlDiv $(quote classes)) #[])

/-! ## Verso Role Expanders

These allow using `{nodeRef "label"}[text]` etc. within Verso documents.
-/

abbrev nodeRef.Args := String
instance : FromArgs nodeRef.Args DocElabM := ⟨.positional `label .string "The blueprint node label"⟩

/-- Role for `{nodeRef "label"}[...]` - inline reference to a node -/
@[role]
def nodeRef : RoleExpanderOf nodeRef.Args := fun label _inlines => do
  ``(Verso.Doc.Inline.other (Blueprint.InlineExt.nodeRef $(quote label)) #[])

abbrev leanRef.Args := Ident
instance : FromArgs leanRef.Args DocElabM := ⟨.positional `name .ident "The Lean declaration name"⟩

/-- Role for `{leanRef `Name}[...]` - inline reference to a Lean declaration -/
@[role]
def leanRef : RoleExpanderOf leanRef.Args := fun id _inlines => do
  let name ← Elab.realizeGlobalConstNoOverloadWithInfo id
  ``(Verso.Doc.Inline.other (Blueprint.InlineExt.leanRef $(quote name)) #[])

abbrev blueprintMath.Args := String
instance : FromArgs blueprintMath.Args DocElabM := ⟨.positional `latex .string "LaTeX math content"⟩

/-- Role for `{blueprintMath "latex"}[...]` - inline LaTeX math -/
@[role]
def blueprintMath : RoleExpanderOf blueprintMath.Args := fun latex _inlines => do
  ``(Verso.Doc.Inline.other (Blueprint.InlineExt.math $(quote latex)) #[])

abbrev htmlSpan.Args := String
instance : FromArgs htmlSpan.Args DocElabM := ⟨.positional `classes .string "CSS classes for the span"⟩

/-- Role for `{htmlSpan "classes"}[content]` - inline span with CSS classes -/
@[role]
def htmlSpan : RoleExpanderOf htmlSpan.Args := fun classes inlines => do
  let content ← inlines.mapM elabInline
  ``(Verso.Doc.Inline.other (Blueprint.InlineExt.htmlSpan $(quote classes)) #[$content,*])

/-! ## Document Construction Helpers -/

/-- Create a simple paragraph from text -/
def para (txt : String) : Verso.Doc.Block Blueprint :=
  Verso.Doc.Block.para #[Verso.Doc.Inline.text txt]

/-- Create a paragraph from multiple inlines -/
def paraOf (inlines : Array (Verso.Doc.Inline Blueprint)) : Verso.Doc.Block Blueprint :=
  Verso.Doc.Block.para inlines

/-- Create a simple text inline -/
def text (s : String) : Verso.Doc.Inline Blueprint :=
  Verso.Doc.Inline.text s

/-- Create emphasized text -/
def emph (s : String) : Verso.Doc.Inline Blueprint :=
  Verso.Doc.Inline.emph #[Verso.Doc.Inline.text s]

/-- Create bold text -/
def bold (s : String) : Verso.Doc.Inline Blueprint :=
  Verso.Doc.Inline.bold #[Verso.Doc.Inline.text s]

/-- Create inline code -/
def inlineCode (s : String) : Verso.Doc.Inline Blueprint :=
  Verso.Doc.Inline.code s

/-- Create a link -/
def link (linkText : String) (url : String) : Verso.Doc.Inline Blueprint :=
  Verso.Doc.Inline.link #[Verso.Doc.Inline.text linkText] url

/-- Create a Part (document section) with the given title and content -/
def mkSection (title : String) (content : Array (Verso.Doc.Block Blueprint)) (subParts : Array (Verso.Doc.Part Blueprint) := #[]) : Verso.Doc.Part Blueprint :=
  Verso.Doc.Part.mk #[Verso.Doc.Inline.text title] title none content subParts

/-- Create a Part with metadata -/
def mkSectionWithMeta (title : String) (theMeta : Blueprint.Meta) (content : Array (Verso.Doc.Block Blueprint)) (subParts : Array (Verso.Doc.Part Blueprint) := #[]) : Verso.Doc.Part Blueprint :=
  Verso.Doc.Part.mk #[Verso.Doc.Inline.text title] title (some theMeta) content subParts

/-! ## Convenience Aliases for Programmatic Use -/

/-- Alias for nodeRefInline -/
abbrev ref := nodeRefInline

/-- Alias for leanRefInline -/
abbrev leanRefFn := leanRefInline

end Runway.Doc
