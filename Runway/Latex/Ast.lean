/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Runway.Latex.Token

/-!
# LaTeX AST Types

This module defines the abstract syntax tree types for parsed LaTeX documents.
-/

namespace Runway.Latex

/-- Inline content (within paragraphs) -/
inductive Inline where
  /-- Plain text -/
  | text (s : String)
  /-- Emphasized text -/
  | emph (content : Array Inline)
  /-- Bold text -/
  | bold (content : Array Inline)
  /-- Inline code -/
  | code (content : String)
  /-- Inline math -/
  | math (content : String)
  /-- Cross-reference -/
  | ref (label : String)
  /-- Citation -/
  | cite (keys : Array String)
  /-- Hyperlink -/
  | href (url : String) (text : Array Inline)
  /-- Link to Lean declaration -/
  | lean (names : Array Lean.Name)
  /-- Raw LaTeX -/
  | raw (content : String)
  /-- Whitespace -/
  | space
  /-- Sequence of inline elements -/
  | seq (parts : Array Inline)
  deriving Repr, Inhabited

/-- Document preamble -/
structure Preamble where
  documentClass : String := "report"
  classOptions : Array String := #[]
  packages : Array (String × Array String) := #[]
  title : Option String := none
  author : Option String := none
  date : Option String := none
  rawContent : String := ""
  deriving Repr, Inhabited

/-- Blueprint-specific metadata for theorem environments -/
structure TheoremMetadata where
  leanDecls : Array Lean.Name := #[]
  uses : Array String := #[]
  proves : Option String := none
  leanOk : Bool := false
  notReady : Bool := false
  mathLibOk : Bool := false
  discussion : Option Nat := none
  label : Option String := none
  position : Option String := none
  signatureHtml : Option String := none
  proofHtml : Option String := none
  hoverData : Option String := none
  deriving Repr, Inhabited

/-- Block-level content -/
inductive Block : Type where
  /-- Complete document -/
  | document (preamble : Preamble) (body : Array Block)
  /-- Chapter -/
  | chapter (title : Inline) (label : Option String) (body : Array Block)
  /-- Section -/
  | section (level : Nat) (title : Inline) (label : Option String) (body : Array Block)
  /-- Theorem-like environment -/
  | theorem (env : String) (title : Option Inline) (metadata : TheoremMetadata) (statement : Array Block)
  /-- Proof environment -/
  | proof (metadata : TheoremMetadata) (content : Array Block)
  /-- Paragraph -/
  | paragraph (content : Array Inline)
  /-- Display math -/
  | displayMath (content : String)
  /-- Itemize list -/
  | itemize (items : Array (Array Block))
  /-- Enumerate list -/
  | enumerate (items : Array (Array Block))
  /-- Include Lean module -/
  | inputLeanModule (moduleName : Lean.Name)
  /-- Include Lean node -/
  | inputLeanNode (label : String)
  /-- Raw block -/
  | raw (content : String)
  /-- Comment -/
  | comment (content : String)
  deriving Repr, Inhabited

/-- A parsed LaTeX document -/
structure Document where
  root : Block
  sourcePath : Option System.FilePath := none
  deriving Repr, Inhabited

namespace Inline

/-- Convert inline to plain text -/
partial def toPlainText : Inline → String
  | Inline.text s => s
  | Inline.emph content => content.foldl (fun acc x => acc ++ toPlainText x) ""
  | Inline.bold content => content.foldl (fun acc x => acc ++ toPlainText x) ""
  | Inline.code s => s
  | Inline.math s => s
  | Inline.ref label => s!"[ref:{label}]"
  | Inline.cite keys => s!"[cite:{",".intercalate keys.toList}]"
  | Inline.href _ txt => txt.foldl (fun acc x => acc ++ toPlainText x) ""
  | Inline.lean names => names.map (·.toString) |>.toList |> ", ".intercalate
  | Inline.raw s => s
  | Inline.space => " "
  | Inline.seq parts => parts.foldl (fun acc x => acc ++ toPlainText x) ""

/-- Check if inline content is empty text -/
def isEmptyText : Inline → Bool
  | Inline.text s => s.isEmpty
  | Inline.space => false
  | _ => false

end Inline

namespace Block

/-- Get the label from a block if it has one -/
def getLabel : Block → Option String
  | Block.chapter _ label _ => label
  | Block.section _ _ label _ => label
  | Block.theorem _ _ md _ => md.label
  | Block.proof md _ => md.label
  | _ => none

/-- Check if this is a theorem-like environment -/
def isTheorem : Block → Bool
  | Block.theorem .. => true
  | _ => false

/-- Check if this is a proof block -/
def isProof : Block → Bool
  | Block.proof .. => true
  | _ => false

end Block

/-- Standard theorem-like environment names -/
def theoremEnvs : List String :=
  ["theorem", "lemma", "proposition", "corollary", "definition",
   "example", "remark", "note", "axiom", "conjecture"]

/-- Check if an environment name is theorem-like -/
def isTheoremEnv (name : String) : Bool :=
  theoremEnvs.contains name.toLower

end Runway.Latex
