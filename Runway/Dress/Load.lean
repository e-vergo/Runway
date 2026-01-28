/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Runway.Latex.Ast

/-!
# Dress Artifact Loading

Loads and decodes Dress artifacts (base64-encoded HTML and hover data)
from parsed LaTeX documents.
-/

namespace Runway.Dress

/-- Decode a base64 character to its value (0-63), or 0xFF for invalid -/
private def decodeBase64Char (c : Char) : UInt8 :=
  if 'A' ≤ c && c ≤ 'Z' then (c.toNat - 'A'.toNat).toUInt8
  else if 'a' ≤ c && c ≤ 'z' then (c.toNat - 'a'.toNat + 26).toUInt8
  else if '0' ≤ c && c ≤ '9' then (c.toNat - '0'.toNat + 52).toUInt8
  else if c == '+' then 62
  else if c == '/' then 63
  else 0xFF

/-- Decode base64 string to ByteArray -/
def decodeBase64 (s : String) : Option ByteArray := Id.run do
  let mut result := ByteArray.empty
  let mut buffer : Array UInt8 := #[]
  let mut valid := true

  for c in s.toList do
    if c == '=' then
      break  -- padding
    else if c == ' ' || c == '\n' || c == '\r' || c == '\t' then
      continue  -- skip whitespace
    else
      let idx := decodeBase64Char c
      if idx == 0xFF then
        valid := false
        break  -- invalid character
      buffer := buffer.push idx

      if buffer.size == 4 then
        -- Decode 4 base64 chars to 3 bytes
        let b0 := (buffer[0]! <<< 2) ||| (buffer[1]! >>> 4)
        let b1 := ((buffer[1]! &&& 0x0F) <<< 4) ||| (buffer[2]! >>> 2)
        let b2 := ((buffer[2]! &&& 0x03) <<< 6) ||| buffer[3]!
        result := result.push b0
        result := result.push b1
        result := result.push b2
        buffer := #[]

  if !valid then return none

  -- Handle remaining bytes
  if buffer.size == 2 then
    let b0 := (buffer[0]! <<< 2) ||| (buffer[1]! >>> 4)
    result := result.push b0
  else if buffer.size == 3 then
    let b0 := (buffer[0]! <<< 2) ||| (buffer[1]! >>> 4)
    let b1 := ((buffer[1]! &&& 0x0F) <<< 4) ||| (buffer[2]! >>> 2)
    result := result.push b0
    result := result.push b1

  return some result

/-- Decode base64 string to UTF-8 String -/
def decodeBase64String (s : String) : Option String :=
  match decodeBase64 s with
  | none => none
  | some bytes => some (String.fromUTF8! bytes)

/-- Process theorem metadata by decoding base64 fields -/
def processMetadata (md : Latex.TheoremMetadata) : Latex.TheoremMetadata :=
  { md with
    signatureHtml := md.signatureHtml.bind decodeBase64String
    proofHtml := md.proofHtml.bind decodeBase64String
    hoverData := md.hoverData.bind decodeBase64String
  }

/-- Process a block, decoding any Dress artifacts -/
partial def processBlock : Latex.Block → Latex.Block
  | .document preamble body =>
    .document preamble (body.map processBlock)
  | .chapter title label body =>
    .chapter title label (body.map processBlock)
  | .section level title label body =>
    .section level title label (body.map processBlock)
  | .theorem env title md statement =>
    .theorem env title (processMetadata md) (statement.map processBlock)
  | .proof md content =>
    .proof (processMetadata md) (content.map processBlock)
  | .itemize items =>
    .itemize (items.map fun item => item.map processBlock)
  | .enumerate items =>
    .enumerate (items.map fun item => item.map processBlock)
  | other => other

/-- Process a document, decoding all Dress artifacts -/
def processDocument (doc : Latex.Document) : Latex.Document :=
  { doc with root := processBlock doc.root }

/-- Artifact info for a declaration -/
structure DeclArtifact where
  /-- Declaration name -/
  name : Lean.Name
  /-- Label for cross-referencing -/
  label : String
  /-- Pre-rendered HTML for signature -/
  signatureHtml : Option String := none
  /-- Pre-rendered HTML for proof body -/
  proofHtml : Option String := none
  /-- JSON hover data -/
  hoverData : Option String := none
  /-- Source file position -/
  position : Option String := none
  /-- Is proven in Lean? -/
  leanOk : Bool := false
  /-- Mathlib-ready? -/
  mathLibOk : Bool := false
  deriving Repr, Inhabited

/-- Load artifact JSON from Dress build directory -/
def loadArtifactJson (buildDir : System.FilePath) (moduleName : Lean.Name)
    : IO (Option Lean.Json) := do
  let modulePath := moduleName.components.foldl (init := buildDir / "dressed")
    fun path component => path / component.toString
  let jsonPath := modulePath.withExtension "json"
  if ← jsonPath.pathExists then
    let content ← IO.FS.readFile jsonPath
    match Lean.Json.parse content with
    | .ok json => return some json
    | .error _ => return none
  else
    return none

/-- Extract artifacts from loaded JSON -/
def extractArtifacts (json : Lean.Json) : Array DeclArtifact := Id.run do
  let mut artifacts := #[]
  match json with
  | .obj entries =>
    for (name, value) in entries.toArray do
      let html := match value.getObjValAs? String "html" with
        | .ok s => s
        | .error _ => ""
      let artifact : DeclArtifact := {
        name := name.toName
        label := name
        signatureHtml := if html.isEmpty then none else some html
      }
      artifacts := artifacts.push artifact
  | _ => pure ()
  return artifacts

/-- Enhanced manifest data with stats and dashboard metadata -/
structure EnhancedManifest where
  /-- Status counts from the manifest -/
  stats : Option Lean.Json := none
  /-- Key declaration IDs -/
  keyDeclarations : Array String := #[]
  /-- Messages for nodes -/
  messages : Array (String × String) := #[]  -- (id, message)
  /-- Priority items -/
  priorityItems : Array (String × String) := #[]  -- (id, priority)
  /-- Blocked items with reasons -/
  blockedItems : Array (String × String) := #[]  -- (id, reason)
  /-- Potential issues -/
  potentialIssues : Array (String × String) := #[]  -- (id, issue)
  /-- Technical debt notes -/
  technicalDebt : Array (String × String) := #[]  -- (id, debt)
  /-- Misc notes -/
  miscItems : Array (String × String) := #[]  -- (id, note)
  /-- Node id to url mapping -/
  nodeUrls : Std.HashMap String String := {}
  deriving Inhabited

/-- Load and parse the enhanced manifest.json -/
def loadEnhancedManifest (dressedDir : System.FilePath) : IO EnhancedManifest := do
  let manifestPath := dressedDir / "manifest.json"
  if !(← manifestPath.pathExists) then
    return {}

  let content ← IO.FS.readFile manifestPath
  match Lean.Json.parse content with
  | .error _ => return {}
  | .ok json =>
    let mut manifest : EnhancedManifest := {}

    -- Extract stats (keep as Json for flexible parsing)
    manifest := { manifest with stats := json.getObjVal? "stats" |>.toOption }

    -- Extract key declarations
    if let .ok arr := json.getObjValAs? (Array String) "keyDeclarations" then
      manifest := { manifest with keyDeclarations := arr }

    -- Extract messages
    if let .ok messagesJson := json.getObjVal? "messages" then
      if let .arr msgs := messagesJson then
        let mut messages : Array (String × String) := #[]
        for msg in msgs do
          if let (.ok id, .ok message) := (msg.getObjValAs? String "id", msg.getObjValAs? String "message") then
            messages := messages.push (id, message)
        manifest := { manifest with messages := messages }

    -- Extract project notes
    if let .ok notes := json.getObjVal? "projectNotes" then
      -- Priority items
      if let .ok arr := notes.getObjVal? "priority" then
        if let .arr items := arr then
          let mut priorityItems : Array (String × String) := #[]
          for item in items do
            if let (.ok id, .ok priority) := (item.getObjValAs? String "id", item.getObjVal? "priority") then
              priorityItems := priorityItems.push (id, priority.compress)
          manifest := { manifest with priorityItems := priorityItems }

      -- Blocked items
      if let .ok arr := notes.getObjVal? "blocked" then
        if let .arr items := arr then
          let mut blockedItems : Array (String × String) := #[]
          for item in items do
            if let (.ok id, .ok reason) := (item.getObjValAs? String "id", item.getObjValAs? String "reason") then
              blockedItems := blockedItems.push (id, reason)
          manifest := { manifest with blockedItems := blockedItems }

      -- Potential issues
      if let .ok arr := notes.getObjVal? "potentialIssues" then
        if let .arr items := arr then
          let mut potentialIssues : Array (String × String) := #[]
          for item in items do
            if let (.ok id, .ok issue) := (item.getObjValAs? String "id", item.getObjValAs? String "issue") then
              potentialIssues := potentialIssues.push (id, issue)
          manifest := { manifest with potentialIssues := potentialIssues }

      -- Technical debt
      if let .ok arr := notes.getObjVal? "technicalDebt" then
        if let .arr items := arr then
          let mut technicalDebt : Array (String × String) := #[]
          for item in items do
            if let (.ok id, .ok debt) := (item.getObjValAs? String "id", item.getObjValAs? String "debt") then
              technicalDebt := technicalDebt.push (id, debt)
          manifest := { manifest with technicalDebt := technicalDebt }

      -- Misc items
      if let .ok arr := notes.getObjVal? "misc" then
        if let .arr items := arr then
          let mut miscItems : Array (String × String) := #[]
          for item in items do
            if let (.ok id, .ok note) := (item.getObjValAs? String "id", item.getObjValAs? String "note") then
              miscItems := miscItems.push (id, note)
          manifest := { manifest with miscItems := miscItems }

    -- Extract node urls mapping
    if let .ok nodesJson := json.getObjVal? "nodes" then
      if let .obj entries := nodesJson then
        let mut nodeUrls : Std.HashMap String String := {}
        for (id, urlJson) in entries.toArray do
          if let .str url := urlJson then
            nodeUrls := nodeUrls.insert id url
        manifest := { manifest with nodeUrls := nodeUrls }

    return manifest

/-- Check if manifest has enhanced format (with stats) vs simple format (just label->path) -/
def EnhancedManifest.hasStats (m : EnhancedManifest) : Bool :=
  m.stats.isSome

/-- Get key declaration status for a node ID -/
def EnhancedManifest.isKeyDeclaration (m : EnhancedManifest) (nodeId : String) : Bool :=
  m.keyDeclarations.contains nodeId

/-- Get message for a node ID -/
def EnhancedManifest.getMessage (m : EnhancedManifest) (nodeId : String) : Option String :=
  m.messages.find? (·.1 == nodeId) |>.map (·.2)

/-- Get priority for a node ID (deprecated, use getPriorityItem) -/
def EnhancedManifest.getPriority (m : EnhancedManifest) (nodeId : String) : Option String :=
  m.priorityItems.find? (·.1 == nodeId) |>.map (·.2)

/-- Check if a node is a priority item -/
def EnhancedManifest.getPriorityItem (m : EnhancedManifest) (nodeId : String) : Bool :=
  m.priorityItems.any (·.1 == nodeId)

/-- Get blocked reason for a node ID -/
def EnhancedManifest.getBlocked (m : EnhancedManifest) (nodeId : String) : Option String :=
  m.blockedItems.find? (·.1 == nodeId) |>.map (·.2)

/-- Get potential issue for a node ID -/
def EnhancedManifest.getPotentialIssue (m : EnhancedManifest) (nodeId : String) : Option String :=
  m.potentialIssues.find? (·.1 == nodeId) |>.map (·.2)

/-- Get technical debt for a node ID -/
def EnhancedManifest.getTechnicalDebt (m : EnhancedManifest) (nodeId : String) : Option String :=
  m.technicalDebt.find? (·.1 == nodeId) |>.map (·.2)

/-- Get misc note for a node ID -/
def EnhancedManifest.getMisc (m : EnhancedManifest) (nodeId : String) : Option String :=
  m.miscItems.find? (·.1 == nodeId) |>.map (·.2)

end Runway.Dress
