/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# LaTeX Token Types

This module defines the token types produced by the LaTeX lexer.
-/

namespace Runway.Latex

/-- Source position in a LaTeX file -/
structure Position where
  line : Nat
  column : Nat
  offset : Nat
  deriving Repr, BEq, Inhabited

/-- A span in the source file -/
structure Span where
  start : Position
  stop : Position
  deriving Repr, BEq, Inhabited

/-- LaTeX token kinds -/
inductive TokenKind where
  /-- Command like `\foo` or `\begin` -/
  | command (name : String)
  /-- Opening brace `{` -/
  | braceOpen
  /-- Closing brace `}` -/
  | braceClose
  /-- Opening bracket `[` -/
  | bracketOpen
  /-- Closing bracket `]` -/
  | bracketClose
  /-- Inline math `$...$` -/
  | mathInline (content : String)
  /-- Display math `$$...$$` or `\[...\]` -/
  | mathDisplay (content : String)
  /-- Plain text content -/
  | text (content : String)
  /-- Comment `% ...` -/
  | comment (content : String)
  /-- Single newline -/
  | newline
  /-- Paragraph break (blank line) -/
  | paragraph
  /-- Whitespace (spaces, tabs) -/
  | whitespace (content : String)
  /-- End of file -/
  | eof
  deriving Repr, BEq, Inhabited

/-- A token with its source location -/
structure Token where
  kind : TokenKind
  span : Span
  /-- The raw text that produced this token -/
  raw : String
  deriving Repr, BEq, Inhabited

namespace TokenKind

/-- Check if this token is a specific command -/
def isCommand (tk : TokenKind) (name : String) : Bool :=
  match tk with
  | .command n => n == name
  | _ => false

/-- Check if this is the `\begin` command -/
def isBegin (tk : TokenKind) : Bool := tk.isCommand "begin"

/-- Check if this is the `\end` command -/
def isEnd (tk : TokenKind) : Bool := tk.isCommand "end"

/-- Check if this is any kind of whitespace (space, newline, paragraph) -/
def isWhitespace (tk : TokenKind) : Bool :=
  match tk with
  | .whitespace _ | .newline | .paragraph => true
  | _ => false

/-- Get the text content if this is a text token -/
def getText? (tk : TokenKind) : Option String :=
  match tk with
  | .text s => some s
  | _ => none

end TokenKind

namespace Token

/-- Create a token at a given position -/
def mk' (kind : TokenKind) (line col offset : Nat) (raw : String) : Token :=
  let start := { line, column := col, offset }
  let stop := { line, column := col + raw.length, offset := offset + raw.length }
  { kind, span := { start, stop }, raw }

end Token

end Runway.Latex
