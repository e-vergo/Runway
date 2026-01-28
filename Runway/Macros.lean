/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

/-!
# LaTeX Macro Parsing for MathJax Configuration

This module provides functions to parse `\newcommand` and `\renewcommand`
definitions from LaTeX files and convert them to MathJax macro format.

## Supported Patterns

- `\newcommand{\name}{definition}`
- `\newcommand{\name}[n]{definition}` (with argument count)
- `\renewcommand{\name}{definition}`
- `\renewcommand{\name}[n]{definition}`

## Example

Given LaTeX:
```latex
\newcommand{\Z}{\mathbb{Z}}
\newcommand{\Ord}{\mathrm{Ord}}
```

Produces MathJax macros JSON:
```json
"Z": "\\mathbb{Z}", "Ord": "\\mathrm{Ord}"
```
-/

namespace Runway.Macros

/-- A parsed LaTeX macro definition -/
structure MacroDef where
  /-- The macro name (without backslash) -/
  name : String
  /-- Number of arguments (0 if none) -/
  argCount : Nat
  /-- The macro definition body -/
  definition : String
  deriving Repr, Inhabited

/-- State for the brace-matching parser -/
private structure ParseState where
  pos : Nat
  content : String
  deriving Repr

/-- Get character at current position, or none if at end -/
private def ParseState.peek (s : ParseState) : Option Char :=
  if s.pos < s.content.length then
    some (s.content.get ⟨s.pos⟩)
  else
    none

/-- Advance position by n characters -/
private def ParseState.advance (s : ParseState) (n : Nat := 1) : ParseState :=
  { s with pos := s.pos + n }

/-- Check if we're at the end of input -/
private def ParseState.atEnd (s : ParseState) : Bool :=
  s.pos >= s.content.length

/-- Skip whitespace -/
private def ParseState.skipWhitespace (s : ParseState) : ParseState := Id.run do
  let mut state := s
  while !state.atEnd && (state.peek == some ' ' || state.peek == some '\n' ||
                         state.peek == some '\r' || state.peek == some '\t') do
    state := state.advance
  state

/-- Check if content at current position starts with given string -/
private def ParseState.startsWith (s : ParseState) (pref : String) : Bool :=
  (s.content.drop s.pos).startsWith pref

/-- Extract content between matching braces, handling nested braces.
    Assumes we're positioned at the opening brace.
    Returns (content, newState) where newState is positioned after closing brace. -/
private def ParseState.extractBraced (s : ParseState) : Option (String × ParseState) := Id.run do
  if s.peek != some '{' then return none
  let s := s.advance  -- skip opening brace
  let mut depth : Nat := 1
  let mut result := ""
  let mut state := s
  while !state.atEnd && depth > 0 do
    match state.peek with
    | some '{' =>
      depth := depth + 1
      result := result.push '{'
      state := state.advance
    | some '}' =>
      depth := depth - 1
      if depth > 0 then
        result := result.push '}'
      state := state.advance
    | some '\\' =>
      -- Handle escaped characters
      result := result.push '\\'
      state := state.advance
      if let some c := state.peek then
        result := result.push c
        state := state.advance
    | some c =>
      result := result.push c
      state := state.advance
    | none => break
  if depth == 0 then some (result, state) else none

/-- Extract content between square brackets (optional arguments).
    Returns (content, newState) or none if not starting with '['. -/
private def ParseState.extractBracketed (s : ParseState) : Option (String × ParseState) := Id.run do
  if s.peek != some '[' then return none
  let s := s.advance  -- skip opening bracket
  let mut result := ""
  let mut state := s
  while !state.atEnd && state.peek != some ']' do
    match state.peek with
    | some c =>
      result := result.push c
      state := state.advance
    | none => break
  if state.peek == some ']' then
    some (result, state.advance)
  else
    none

/-- Parse a single \newcommand or \renewcommand -/
private def parseMacroAt (s : ParseState) : Option (MacroDef × ParseState) := do
  -- Check for \newcommand or \renewcommand
  let isNewcommand := s.startsWith "\\newcommand"
  let isRenewcommand := s.startsWith "\\renewcommand"

  if !isNewcommand && !isRenewcommand then
    failure

  let cmdLen := if isNewcommand then 11 else 13  -- length of command
  let s := s.advance cmdLen
  let s := s.skipWhitespace

  -- Extract macro name: \name or {\name}
  let (name, s) ← if s.peek == some '{' then
    -- {\name} form
    let (content, s) ← s.extractBraced
    if content.startsWith "\\" then
      pure ((content.drop 1).toString, s)
    else
      failure
  else if s.peek == some '\\' then
    -- \name form (rare but possible)
    let s := s.advance
    let mut nameStr := ""
    let mut state := s
    while !state.atEnd && (state.peek.map Char.isAlpha |>.getD false) do
      if let some c := state.peek then
        nameStr := nameStr.push c
        state := state.advance
    pure (nameStr, state)
  else
    failure

  let s := s.skipWhitespace

  -- Check for optional argument count [n]
  let (argCount, s) ← match s.extractBracketed with
    | some (numStr, s) =>
      let n := numStr.trimAscii.toNat? |>.getD 0
      pure (n, s)
    | none => pure (0, s)

  let s := s.skipWhitespace

  -- Extract definition body
  let (definition, s) ← s.extractBraced

  return ({ name := name, argCount := argCount, definition := definition }, s)

/-- Parse all macro definitions from LaTeX content.
    Scans through the content looking for \newcommand and \renewcommand. -/
def parseLatexMacros (content : String) : Array MacroDef := Id.run do
  let mut macros : Array MacroDef := #[]
  let mut state : ParseState := { pos := 0, content := content }

  while !state.atEnd do
    -- Look for next backslash
    if state.peek == some '\\' then
      -- Try to parse a macro definition
      match parseMacroAt state with
      | some (macroDef, newState) =>
        macros := macros.push macroDef
        state := newState
      | none =>
        state := state.advance
    else
      state := state.advance

  return macros

/-- Escape a string for use in JavaScript string literal.
    Doubles backslashes since they're escape characters in JS. -/
def escapeForJs (s : String) : String :=
  s.foldl (fun acc c =>
    match c with
    | '\\' => acc ++ "\\\\"
    | '"' => acc ++ "\\\""
    | '\n' => acc ++ "\\n"
    | '\r' => acc ++ "\\r"
    | '\t' => acc ++ "\\t"
    | c => acc.push c
  ) ""

/-- Convert a MacroDef to MathJax format.
    For macros with arguments: "name": ["definition", argCount]
    For macros without arguments: "name": "definition" -/
def macroToMathJax (m : MacroDef) : String :=
  let escapedDef := escapeForJs m.definition
  if m.argCount > 0 then
    s!"\"{m.name}\": [\"{escapedDef}\", {m.argCount}]"
  else
    s!"\"{m.name}\": \"{escapedDef}\""

/-- Default macros that should always be included.
    These handle common LaTeX commands that may not have MathJax equivalents. -/
def defaultMacros : Array MacroDef := #[
  -- \textbf in math mode
  { name := "textbf", argCount := 1, definition := "\\mathbf{#1}" },
  -- Common mathematical notation not always defined in user's LaTeX
  { name := "GL", argCount := 0, definition := "\\mathrm{GL}" },
  { name := "SL", argCount := 0, definition := "\\mathrm{SL}" },
  { name := "SO", argCount := 0, definition := "\\mathrm{SO}" },
  { name := "SU", argCount := 0, definition := "\\mathrm{SU}" },
  { name := "R", argCount := 0, definition := "\\mathbb{R}" },
  { name := "C", argCount := 0, definition := "\\mathbb{C}" }
]

/-- Convert an array of MacroDefs to MathJax macros JSON object content.
    Returns the content to go inside `macros: { ... }` -/
def macrosToJson (macros : Array MacroDef) : String :=
  let allMacros := defaultMacros ++ macros
  let entries := allMacros.map macroToMathJax
  ", ".intercalate entries.toList

/-- Load and parse macros from a .tex file.
    Returns empty array if file doesn't exist or can't be read. -/
def loadMacrosFromFile (path : System.FilePath) : IO (Array MacroDef) := do
  if ← path.pathExists then
    let content ← IO.FS.readFile path
    return parseLatexMacros content
  else
    return #[]

/-- Generate the complete MathJax config script content with macros from MacroDef array. -/
def generateMathJaxConfigFromMacros (macros : Array MacroDef) : String :=
  let macrosJson := macrosToJson macros
  "
    MathJax = {
      tex: {
        inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],
        displayMath: [['$$', '$$'], ['\\\\[', '\\\\]']],
        processEscapes: true,
        macros: {" ++ macrosJson ++ "}
      },
      options: {
        skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
      }
    };
  "

/-- Generate the complete MathJax config script content from pre-computed macros JSON string.
    If macrosJson is empty, only default macros are included. -/
def generateMathJaxConfig (macrosJson : String) : String :=
  -- Prepend default macros to the provided ones
  let defaultMacrosJson := defaultMacros.map macroToMathJax |> (", ".intercalate ·.toList)
  let allMacrosJson := if macrosJson.isEmpty then
    defaultMacrosJson
  else
    defaultMacrosJson ++ ", " ++ macrosJson
  "
    MathJax = {
      tex: {
        inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],
        displayMath: [['$$', '$$'], ['\\\\[', '\\\\]']],
        processEscapes: true,
        macros: {" ++ allMacrosJson ++ "}
      },
      options: {
        skipHtmlTags: ['script', 'noscript', 'style', 'textarea', 'pre', 'code']
      }
    };
  "

/-- Parse macros from file and return the JSON string for MathJax config.
    Returns empty string if file doesn't exist. -/
def loadAndFormatMacros (path : System.FilePath) : IO String := do
  let macros ← loadMacrosFromFile path
  if macros.isEmpty then
    return ""
  else
    -- Return just the user macros (without defaults), as generateMathJaxConfig adds defaults
    let entries := macros.map macroToMathJax
    return ", ".intercalate entries.toList

end Runway.Macros
