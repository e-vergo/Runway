/-
Copyright (c) 2025 Runway contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Static Assets

Embeds CSS and JavaScript as string literals for distribution.
These assets are written to the output directory during site generation.

The CSS and JS are migrated from leanblueprint (plasTeX-based) to provide
visual compatibility with existing blueprint sites.
-/

namespace Runway.Assets

/-- Blueprint CSS - migrated from leanblueprint
    Combines theme-blue.css base + blueprint.css side-by-side + style.css highlighting -/
def blueprintCss : String := r#"
/* ========== Theme Blue Base (from plasTeX) ========== */
@charset "UTF-8";
a,abbr,acronym,address,applet,article,aside,audio,b,big,blockquote,body,canvas,caption,center,cite,code,dd,del,details,dfn,div,dl,dt,em,embed,fieldset,figcaption,figure,footer,form,h1,h2,h3,h4,h5,h6,header,hgroup,html,i,iframe,img,ins,kbd,label,legend,li,mark,menu,nav,object,ol,output,p,pre,q,ruby,s,samp,section,small,span,strike,strong,sub,summary,sup,table,tbody,td,tfoot,th,thead,time,tr,tt,u,ul,var,video{font:inherit;font-size:100%;margin:0;padding:0;border:0}
article,aside,details,figcaption,figure,footer,header,hgroup,menu,nav,section{display:block}
body{line-height:1;overflow-x:hidden;background:#f4f4f4}
ol,ul{list-style:none}
blockquote,q{quotes:none}
blockquote:after,blockquote:before,q:after,q:before{content:"";content:none}
table{border-spacing:0;border-collapse:collapse}
.clear:after{font-size:0;display:block;visibility:hidden;clear:both;height:0;content:" "}
.clear{display:inline-block}
* html .clear{height:1%}
.clear{display:block}
*{box-sizing:border-box}
hr{clear:both;border:none;outline:none}
em{font-style:italic}
a{text-decoration:underline}
ul{list-style:disc}
ol{list-style:decimal}
ol,ul{font-size:14px;padding:0 0 0 33px}
ol li,ul li{margin:0}
blockquote{padding:0 15px 0 40px}
table{font-size:13px;width:100%;margin:20px 0;background:#fff}
table th{font-size:16px;font-weight:700}
table tr td{padding:7px}
::selection{color:#fff;background:#000}
::-moz-selection{color:#fff;background:#000}
body{height:100vh;margin:0;flex-direction:column;color:#0a0a14;background-color:#e8e8e8}
body,div.wrapper{display:flex;overflow:hidden}
div.wrapper{flex-grow:1}
div.content-wrapper{max-width:1800px}
@media (min-width:1024px){div.content-wrapper{margin-left:auto;margin-right:auto}}
div.content{flex-grow:1;display:flex;flex-direction:column;overflow:auto;padding:.5rem;margin-bottom:2.5rem}
@media (min-width:1024px){div.content{padding:1rem;margin-bottom:0}}
div.centered{display:flex;flex-direction:column;flex-wrap:wrap;align-items:center;justify-content:space-around;min-width:100%;margin:0 auto}
div.flushleft,div.raggedright{display:flex;justify-content:flex-start}
div.flushright,div.raggedbottom,div.raggedleft{display:flex;justify-content:flex-end}
div.raggedbottom{flex-direction:column}
div.content{line-height:1.4rem}
div.content>p{margin:2.1rem 0}
li>p{margin:.28rem 0}
.icon{display:inline-block;-webkit-user-select:none;-moz-user-select:none;user-select:none;width:1rem;height:1rem;stroke-width:0;stroke:currentColor;fill:currentColor}
h1,h2,h3,h4,h5,h6{font-family:Lucida Grande,Arial,Helvetica,sans-serif}
h1{font-size:2rem;line-height:2rem;margin:1rem 0}
h1,h2{color:#0f2f48}
h2{font-size:1.5rem;margin:.8rem 0}
h3,h4{margin:.67rem 0;color:#0f2f48}
h3,h4,p{font-size:1rem}
p{margin:.5rem 0}
.titlepage{text-align:center}
.titlepage h1{font-weight:400}
b,strong{font-weight:700}
dfn{font-style:italic}
code,kbd,pre,samp{font-family:monospace,serif;font-size:1rem}
pre{white-space:pre-wrap}
q{quotes:""" """ "'" "'"}
small{font-size:80%}
sub,sup{font-size:75%;line-height:0;position:relative;vertical-align:baseline}
sup{top:-.5rem}
sub{bottom:-.25rem}
.mdseries,.textmf{font-weight:400}
.bfseries,.textbf{font-weight:700}
.rmfamily,.textrm{font-family:serif}
.sffamily,.textsf{font-family:sans-serif}
.texttt,.ttfamily{font-family:monospace}
.textup,.upshape{text-transform:uppercase}
.itshape,.textit{font-style:italic}
.slshape,.textsl{font-style:oblique}
.scshape,.textsc{font-variant:small-caps}
small.tiny{font-size:x-small}
small.scriptsize{font-size:smaller}
small.footnotesize,small.small{font-size:small}
.normalsize{font-size:normal}
big.large{font-size:large}
big.xlarge,big.xxlarge{font-size:x-large}
big.huge,big.xhuge{font-size:xx-large}
.rm{font-family:serif;font-style:normal;font-weight:400}
.cal,.it{font-style:italic}
.cal,.it,.sl{font-family:serif;font-weight:400}
.sl{font-style:oblique}
.bf{font-family:serif;font-weight:700}
.bf,.tt{font-style:normal}
.tt{font-family:monospace;font-weight:400}
.underbar{text-decoration:underline}
.fbox,.framebox{border:1px solid #000;padding:1px 3px}
.quotation p,.quote p,.verse p{margin-top:0;margin-bottom:.5em}
hr{color:#000}
dd{margin-left:3rem}
dd p{padding:0;margin:0 0 1rem}
ul.breadcrumbs{margin:0;padding:0;list-style:none;font-size:small}
ul.breadcrumbs li{display:inline}
ul.breadcrumbs a{text-decoration:none;color:#396282}
li.crumb:after{content:" / "}
div.equation{display:flex;align-items:center;margin:1rem}
div.equation span.equation_label{margin-right:1rem}
div.equation span.equation_label:before{content:"("}
div.equation span.equation_label:after{content:")"}
div.equation div.equation_content{margin:auto}
figure{display:flex;flex-direction:column;vertical-align:bottom;overflow:auto}
figure img{display:block;margin:0 auto}
figcaption{display:block;text-align:center;margin-bottom:.1rem}
span.caption_ref,span.caption_title,span.subcaption{font-weight:700}
span.caption_ref:after{content:":"}
span.subref:after{content:")"}
footer#footnotes{clear:both;padding-top:1rem;padding-left:1rem;border-color:gray;border-top:1px solid}
footer#footnotes h1{font-size:1.5rem;margin:0;margin-bottom:.5rem;color:#000}
a.footnote{text-decoration:none}
a.footnote sup:after{content:"]"}
a.footnote sup:before{content:"["}
body>header{background:linear-gradient(180deg,#6696bb 0,#396282);color:#fff;text-shadow:1px 2px 0 rgba(0,0,0,.8);display:flex;align-items:center;padding:.5rem}
svg#toc-toggle{width:1.125rem;height:1.125rem;margin-right:.5rem;cursor:pointer}
h1#doc_title{color:#fff;font-size:1.5rem;margin:auto}
#doc_title a,#doc_title a:visited{text-decoration:none;color:#fff}
.theindex li{list-style-type:none}
nav.index-groups{margin-bottom:1rem}
a[class^=index-group]{text-decoration:none}
a.index-group:after{content:" |"}
section.theindex{display:flex;flex-direction:row;flex-wrap:wrap;margin-top:1rem}
section.theindex h2{min-width:100%;margin:1rem 0 .5rem}
ul.index-column{min-width:100%}
@media (min-width:1024px){ul.index-column{min-width:auto}}
nav.prev_up_next a.index{font-size:small;padding-left:.5rem;padding-right:.5rem}
dl.description dt{font-weight:700}
table.list{margin-left:15px;margin-top:1em;margin-bottom:1em}
table.list td{padding-right:5px}
div.displaymath{overflow:auto}
a.eqref:before{content:"("}
a.eqref:after{content:")"}
nav.prev_up_next{position:fixed;z-index:1;right:0;bottom:0;display:flex;height:2.5rem;background:linear-gradient(180deg,#6696bb 0,#396282)}
nav.prev_up_next a{font-size:150%;margin:auto;padding:.5rem 1rem;text-decoration:none;color:#fff;text-shadow:1px 2px 0 rgba(0,0,0,.8)}
hspace,vspace{margin:0;padding:0}
div.bigskip{margin-bottom:4rem}
div.medskip{margin:0;padding:0;margin-bottom:2rem}
div.bigskip{margin:0;padding:0;margin-bottom:1rem}
.tabular{border-collapse:collapse;color:#0a0a14;background-color:#e8e8e8;width:auto}
.tabular td,.tabular th{vertical-align:baseline;text-align:left;padding:.3em .6em;empty-cells:show}
td p:first-child,th p:first-child{margin-top:0;margin-bottom:0}
td p,th p{margin-top:1em;margin-bottom:0}
@keyframes a{0%{background-color:#c2c2c2}to{background-color:#e8e8e8}}
div[class$=_thmwrapper]{margin-top:1rem}
div[class$=_thmwrapper]:target{animation:a 1s ease}
div[class$=_thmheading]{display:flex;font-weight:700;line-height:150%}
span[class$=_thmtitle]:before{content:"("}
span[class$=_thmtitle]:after{content:")"}
div[class$=_thmcontent]{font-weight:400;margin-left:1rem;padding-top:.14rem;padding-left:1rem}
span[class$=_thmlabel]{margin-left:.5rem;margin-right:.5rem}
div[class$=proof_heading]{font-weight:700;line-height:120%;cursor:pointer}
div.proof_content{font-weight:400;margin-left:1rem;padding-top:.5rem;padding-left:1rem}
span.expand-proof{font-size:80%}
div.hilite{animation:a 1s ease}
span.qed{float:right}
button.modal{border:none;text-align:center;text-decoration:none;background:transparent;cursor:pointer;padding:0}
div.modal-container{position:fixed;z-index:2;top:0;left:0;display:none;width:100%;height:100%}
div.modal-content{font-weight:400;overflow:auto;margin:auto;vertical-align:middle;border:1px solid #497da5;border-radius:5px;background-color:#fff;box-shadow:0 4px 8px 0 rgba(0,0,0,.2),0 6px 20px 0 rgba(0,0,0,.19)}
div.modal-content header{position:relative;background:linear-gradient(180deg,#6696bb 0,#396282);color:#fff;text-shadow:1px 2px 0 rgba(0,0,0,.8);display:flex;flex-direction:row;min-height:1rem;min-width:100%;text-align:center;vertical-align:middle;padding:0 .5rem;justify-content:space-between}
div.modal-content header button.closebtn{font-size:120%;font-weight:700;background:Transparent;border:none;margin:auto 0;padding-right:.3rem;text-decoration:none;color:#fff;cursor:pointer}
div.modal-content header h1{font-size:120%;margin:auto 0;padding:.2rem;color:#fff}
div.modal-content a{text-decoration:none}
div.modal-content ul{padding:1rem;list-style:none}
div.modal-content li{padding-left:.5rem}
a.icon{text-decoration:none;color:#0a0a14;border:none;background-color:Transparent}
div[class$=_thmheading]:hover div.thm_header_hidden_extras{display:inline-block}
div.thm_header_hidden_extras{display:none}
ul.quizz{display:flex;flex-direction:column;list-style:circle!important}
ul.quizz li{display:flex;padding:.5rem;flex-direction:row;min-width:100%;min-height:3rem;flex-grow:1;align-items:center;justify-content:space-between}
ul.quizz li.active-qright{background-color:green}
ul.quizz li.active-qwrong{background-color:red}
ul.quizz svg.icon{display:none;padding-right:.5rem;width:2rem;height:2rem}
.tikzcd{overflow:auto}
.tikzcd,.tikzpicture{display:block;margin:.5rem auto}
.local_toc ul{padding-left:1rem;list-style:none}
.local_toc ul a{text-decoration:none;color:#0a0a14}
.local_toc ul li{padding:.2rem 0}
nav.toc{flex-shrink:0;display:none;width:100%;overflow-x:hidden;overflow-y:auto;flex-direction:column;margin-right:1rem;padding:0;padding-right:1rem;transition:left .5s ease;background-color:#6696bb;border-right:none}
@media (min-width:1024px){nav.toc{display:flex;max-width:25ch}}
nav.active{width:100%}
.toc ul{min-width:100%;padding-left:0;list-style:none}
.toc ul a{display:inline-block;max-width:90%;padding-top:.5rem;padding-right:.5rem;padding-bottom:.5rem;transition:all .1s ease;text-align:left;text-decoration:none;font-size:1.125rem;color:#fff;text-shadow:2px 2px 2px rgba(0,0,0,.8);flex-grow:1}
.toc ul a:hover{transition:all .2s ease;background:#497da5}
.toc ul li{display:flex;min-width:100%;align-items:center;flex-wrap:wrap;justify-content:space-between;background-color:#6696bb}
.toc ul li.current{background-color:#89aecb;font-weight:400}
.sub-toc-0 a{padding-left:.8rem}
.sub-toc-1 a{padding-left:1.6rem}
.sub-toc-2 a{padding-left:2.4rem}
.sub-toc-3 a{padding-left:3.2rem}
.sub-toc-4 a{padding-left:4rem}
ul.sub-toc-1,ul.sub-toc-2,ul.sub-toc-3,ul.sub-toc-4{display:none}
span.expand-toc{min-width:.7rem;width:.8rem;height:.8rem;padding:0;padding-right:.5rem;font-size:125%;cursor:pointer;text-align:center;color:#fff;background-color:Transparent}
svg.close-toc{min-width:1.3rem;min-height:1.3rem;margin:.5rem;margin-left:auto;display:none;cursor:pointer;text-align:center;color:#fff;background-color:Transparent}
nav.active svg.close-toc{display:inline-block}
ul.active{display:block}
code.verb{font-family:monospace;font-style:normal;font-weight:400}
pre.verbatim{margin:1rem 2rem;background-color:#dbdbdb;padding:.5rem}

/* ========== Side-by-side Layout (from leanblueprint) ========== */
a.github_link {
  font-weight: normal;
  font-size: 90%;
  text-decoration: none;
  color: inherit;
}

.sbs-container {
  display: grid;
  grid-template-columns: minmax(75ch, 75ch) minmax(0, 1fr);
  gap: 3rem;
  align-items: start;
  margin-bottom: 3rem;
  margin-left: 3rem;
}

.sbs-latex-column {
  width: 75ch;
  max-width: 75ch;
  overflow-wrap: break-word;
  word-wrap: break-word;
}

.sbs-lean-column {
  min-width: 0;
  overflow-wrap: break-word;
  word-wrap: break-word;
  overflow-x: auto;
  position: relative;
}

.sbs-lean-column pre.lean-code {
  margin: 0;
  padding: 0;
  background: transparent;
  font-family: 'JetBrains Mono', 'Fira Code', monospace;
  font-size: 0.85rem;
  line-height: 1.5;
  white-space: pre-wrap;
  word-wrap: break-word;
}

.sbs-lean-column pre.lean-code code {
  font-family: inherit;
}

.lean-github-hover {
  position: absolute;
  top: 0;
  right: 0;
  opacity: 0;
  transition: opacity 0.2s;
  padding: 0.25rem;
  color: #586069;
}

.lean-github-hover:hover {
  color: #0366d6;
}

.sbs-lean-column:hover .lean-github-hover {
  opacity: 1;
}

.lean-proof-body {
  display: none;
}

.thm_body_extras {
  display: none;
}

.proof_wrapper.proof_inline {
  display: block !important;
}

.proof_wrapper.proof_inline .proof_content {
  display: none;
}

/* ========== Lean Syntax Highlighting ========== */
.lean-keyword, .hl.lean .keyword, .keyword.token { color: #0000ff; }
.lean-const, .hl.lean .const, .const.token { color: #AF8700; }
.lean-const.lean-def, .hl.lean .const[data-defining="true"], .const.token[data-defining="true"] {
  font-weight: bold;
  text-decoration: underline;
  text-decoration-style: dotted;
  text-underline-offset: 2px;
}
.lean-var, .hl.lean .var, .var.token { color: #0070C1; }
.lean-string, .hl.lean .literal.string, .literal.string.token { color: #a31515; }
.lean-option, .hl.lean .option, .option.token { color: #795e26; }
.lean-docstring, .hl.lean .doc-comment, .doc-comment.token { color: #008000; font-style: italic; }
.lean-sort, .hl.lean .sort, .sort.token { color: #267f99; }
.lean-level, .hl.lean .level-var, .hl.lean .level-const, .hl.lean .level-op,
.level-var.token, .level-const.token, .level-op.token { color: #098658; }
.lean-module, .hl.lean .module-name, .module-name.token { color: #795e26; }
.lean-expr, .hl.lean .typed, .typed.token { color: inherit; }
.lean-text, .hl.lean .unknown, .hl.lean .inter-text, .unknown.token, .inter-text { color: inherit; }
.lean-sorry, .sorry.token { color: #ff0000; background-color: #ffeaea; font-weight: bold; }

.lean-number { color: #098658; }
.lean-operator { color: #000000; }
.lean-comment { color: #008000; font-style: italic; }

/* Rainbow brackets */
.lean-bracket-1 { color: #d000ff; }
.lean-bracket-2 { color: #5126ff; }
.lean-bracket-3 { color: #0184BC; }
.lean-bracket-4 { color: #4078F2; }
.lean-bracket-5 { color: #50A14F; }
.lean-bracket-6 { color: #E45649; }

/* Message spans */
.lean-span, .hl.lean .has-info { }
.lean-error, .hl.lean .has-info.error { text-decoration: wavy underline #c82829; }
.lean-warning, .hl.lean .has-info.warning { text-decoration: wavy underline #eab700; }
.lean-info, .hl.lean .has-info.information { text-decoration: underline dotted #4271ae; }

/* Token binding highlight */
.hl.lean .token.binding-hl {
  background-color: rgba(255, 255, 0, 0.3);
  border-radius: 2px;
}

/* Hover info */
.hl.lean .hover-info {
  display: block;
  padding: 8px;
  font-family: 'JetBrains Mono', 'Fira Code', monospace;
  font-size: 0.9em;
}

.hl.lean .hover-info code { font-family: inherit; }

.hl.lean .sep {
  display: block;
  height: 1px;
  background-color: #ccc;
  margin: 8px 0;
}

.hl.lean .docstring {
  color: #666;
  font-style: italic;
  margin-top: 8px;
}

.hl.lean .docstring p { margin: 0.5em 0; }

.hl.lean .docstring code {
  background-color: rgba(0, 0, 0, 0.05);
  padding: 0 4px;
  border-radius: 2px;
}

/* Extra doc links */
.extra-doc-links {
  list-style-type: none;
  margin-left: 0;
  padding: 0;
  margin-top: 8px;
  border-top: 1px solid #ccc;
  padding-top: 8px;
}

.extra-doc-links > li { display: inline-block; }

.extra-doc-links > li:not(:last-child)::after {
  content: '|';
  display: inline-block;
  margin: 0 0.25em;
  color: #999;
}

/* ========== Tippy.js Themes ========== */
[data-tippy-root] { z-index: 99999 !important; }

.tippy-box {
  -webkit-text-size-adjust: 100%;
  text-size-adjust: 100%;
}

.tippy-box[data-theme~='lean'] {
  background-color: #f5f5f5;
  color: black;
  border: 1px solid #333;
  border-radius: 4px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
}

.tippy-box[data-theme~='lean'] .tippy-content {
  padding: 8px 12px;
  font-family: 'JetBrains Mono', 'Fira Code', monospace;
  font-size: 0.85em;
}

.tippy-box[data-theme~='lean'][data-placement^='top'] > .tippy-arrow::before { border-top-color: #f5f5f5; }
.tippy-box[data-theme~='lean'][data-placement^='bottom'] > .tippy-arrow::before { border-bottom-color: #f5f5f5; }
.tippy-box[data-theme~='lean'][data-placement^='left'] > .tippy-arrow::before { border-left-color: #f5f5f5; }
.tippy-box[data-theme~='lean'][data-placement^='right'] > .tippy-arrow::before { border-right-color: #f5f5f5; }

.tippy-box[data-theme~='warning'] {
  background-color: #fff8e6;
  color: black;
  border: 2px solid #f0ad4e;
  border-radius: 4px;
}

.tippy-box[data-theme~='warning'][data-placement^='top'] > .tippy-arrow::before { border-top-color: #fff8e6; }
.tippy-box[data-theme~='warning'][data-placement^='bottom'] > .tippy-arrow::before { border-bottom-color: #fff8e6; }

.tippy-box[data-theme~='error'] {
  background-color: #fff0f0;
  color: black;
  border: 2px solid #d9534f;
  border-radius: 4px;
}

.tippy-box[data-theme~='error'][data-placement^='top'] > .tippy-arrow::before { border-top-color: #fff0f0; }
.tippy-box[data-theme~='error'][data-placement^='bottom'] > .tippy-arrow::before { border-bottom-color: #fff0f0; }

.tippy-box[data-theme~='info'] {
  background-color: #f0f8ff;
  color: black;
  border: 2px solid #5bc0de;
  border-radius: 4px;
}

.tippy-box[data-theme~='info'][data-placement^='top'] > .tippy-arrow::before { border-top-color: #f0f8ff; }
.tippy-box[data-theme~='info'][data-placement^='bottom'] > .tippy-arrow::before { border-bottom-color: #f0f8ff; }

.tippy-box[data-theme~='tactic'] {
  background-color: white;
  color: black;
  border: 1px solid #333;
  border-radius: 4px;
  box-shadow: 0 2px 12px rgba(0, 0, 0, 0.2);
}

.tippy-box[data-theme~='tactic'] .tippy-content { padding: 12px; }

.tippy-box[data-theme~='tactic'][data-placement^='top'] > .tippy-arrow::before { border-top-color: white; }
.tippy-box[data-theme~='tactic'][data-placement^='bottom'] > .tippy-arrow::before { border-bottom-color: white; }
.tippy-box[data-theme~='tactic'][data-placement^='left'] > .tippy-arrow::before { border-left-color: white; }
.tippy-box[data-theme~='tactic'][data-placement^='right'] > .tippy-arrow::before { border-right-color: white; }

/* Tactic state display - smooth animation like proof toggle */
/* Override Verso's display-based toggle with max-height animation */
.hl.lean input.tactic-toggle { display: none; }
.hl.lean .tactic-state {
  display: block !important;  /* Override Verso's display:none */
  max-height: 0;
  overflow: hidden;
  opacity: 0;
  transition: max-height 0.3s ease-out, opacity 0.3s ease-out, padding 0.3s ease-out, margin 0.3s ease-out;
  margin-top: 0;
  padding: 0 8px;
  background-color: #f9f9f9;
  border: 1px solid #ddd;
  border-radius: 4px;
}

.hl.lean input.tactic-toggle:checked ~ .tactic-state {
  display: block !important;  /* Override Verso's display:inline-block */
  max-height: 500px;
  opacity: 1;
  margin-top: 8px;
  padding: 8px;
}

.hl.lean .tactic > label { cursor: pointer; }

.hl.lean .tactic::before {
  content: '\25CF';
  color: #4271ae;
  font-size: 0.6em;
  vertical-align: super;
  margin-right: 2px;
  opacity: 0.5;
}

.hl.lean .tactic:hover::before { opacity: 1; }

.hl.lean .goal {
  margin-bottom: 12px;
  padding-bottom: 8px;
  border-bottom: 1px solid #eee;
}

.hl.lean .goal:last-child {
  margin-bottom: 0;
  padding-bottom: 0;
  border-bottom: none;
}

.hl.lean .goal-name {
  color: #666;
  font-style: italic;
  margin-bottom: 4px;
}

.hl.lean .hypotheses {
  display: table;
  border-collapse: collapse;
  margin-bottom: 8px;
}

.hl.lean .hypotheses .hypothesis { display: table-row; }

.hl.lean .hypotheses .name {
  display: table-cell;
  padding-right: 8px;
  text-align: right;
  color: #c82829;
  font-family: 'JetBrains Mono', 'Fira Code', monospace;
}

.hl.lean .hypotheses .colon {
  display: table-cell;
  padding-right: 8px;
  color: #666;
}

.hl.lean .hypotheses .type {
  display: table-cell;
  font-family: 'JetBrains Mono', 'Fira Code', monospace;
}

.hl.lean .conclusion { margin-top: 8px; }

.hl.lean .conclusion .goal-prefix {
  color: #666;
  margin-right: 4px;
}

.hl.lean .conclusion .type {
  font-family: 'JetBrains Mono', 'Fira Code', monospace;
}

/* ========== Multi-page Navigation ========== */
.prev-next-nav {
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-top: 3rem;
  padding-top: 1.5rem;
  border-top: 1px solid #e5e7eb;
}

.prev-next-nav a {
  color: #396282;
  text-decoration: none;
  padding: 0.5rem 1rem;
  border: 1px solid #e5e7eb;
  border-radius: 4px;
  transition: background-color 0.2s;
}

.prev-next-nav a:hover {
  background-color: #f3f4f6;
}

.prev-next-nav a.prev {
  margin-right: auto;
}

.prev-next-nav a.next {
  margin-left: auto;
}

/* Chapter list on index page */
.chapter-list {
  margin: 2rem 0;
}

.chapter-list h2 {
  margin-bottom: 1rem;
  color: #0f2f48;
}

.chapter-index {
  padding-left: 1.5rem;
}

.chapter-index li {
  margin: 0.5rem 0;
  line-height: 1.6;
}

.chapter-index a {
  color: #396282;
  text-decoration: none;
}

.chapter-index a:hover {
  text-decoration: underline;
}

/* Chapter page styling */
.chapter-page {
  margin: 1rem 0;
}

.chapter-title {
  color: #0f2f48;
  margin-bottom: 1.5rem;
}

.chapter-nodes, .section-nodes {
  margin: 1rem 0;
}

.section {
  margin: 2rem 0;
  padding-top: 1rem;
  border-top: 1px solid #e5e7eb;
}

.section-title {
  color: #0f2f48;
  margin-bottom: 1rem;
}

/* Active sidebar item highlighting */
nav.toc li.active a {
  background-color: #497da5;
  font-weight: bold;
}

/* ========== Blueprint Theme Styles (from Theme.lean) ========== */
/* CSS variables for theming */
:root {
  --bp-primary: #2563eb;
  --bp-success: #16a34a;
  --bp-warning: #ca8a04;
  --bp-danger: #dc2626;
  --bp-muted: #6b7280;
  --bp-bg: #ffffff;
  --bp-bg-alt: #f9fafb;
  --bp-border: #e5e7eb;
  --bp-text: #1f2937;
  --bp-text-muted: #6b7280;
  --bp-font-sans: system-ui, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  --bp-font-mono: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Monaco, Consolas, monospace;
  --bp-max-width: 1200px;
  --bp-spacing: 1.5rem;
}

@media (prefers-color-scheme: dark) {
  :root {
    --bp-bg: #111827;
    --bp-bg-alt: #1f2937;
    --bp-border: #374151;
    --bp-text: #f9fafb;
    --bp-text-muted: #9ca3af;
  }
}

/* Progress Section */
.progress-section {
  margin: 2rem 0;
  padding: 1.5rem;
  background: var(--bp-bg-alt);
  border-radius: 8px;
}

.progress-bar {
  height: 24px;
  background: var(--bp-border);
  border-radius: 12px;
  overflow: hidden;
  margin: 1rem 0;
}

.progress-fill {
  height: 100%;
  background: linear-gradient(90deg, var(--bp-success), var(--bp-primary));
  transition: width 0.3s ease;
}

.progress-stats {
  display: flex;
  flex-wrap: wrap;
  gap: 1rem;
  margin-top: 1rem;
  align-items: center;
}

.stat {
  display: inline-flex;
  align-items: center;
  padding: 0.25rem 0.75rem;
  border-radius: 4px;
  font-size: 0.875rem;
  margin-right: 0.5rem;
}

.stat:last-child {
  margin-right: 0;
}

.stat.proved { background: rgba(22, 163, 74, 0.1); color: var(--bp-success); }
.stat.mathlib { background: rgba(37, 99, 235, 0.1); color: var(--bp-primary); }
.stat.stated { background: rgba(202, 138, 4, 0.1); color: var(--bp-warning); }
.stat.not-ready { background: rgba(220, 38, 38, 0.1); color: var(--bp-danger); }
.stat.total { background: var(--bp-border); }

/* Node Styles */
.node {
  border: 1px solid var(--bp-border);
  border-radius: 8px;
  margin: 1.5rem 0;
  overflow: hidden;
}

.node-header {
  display: flex;
  align-items: center;
  gap: 0.75rem;
  padding: 1rem;
  background: var(--bp-bg-alt);
  border-bottom: 1px solid var(--bp-border);
}

.node-env {
  font-weight: 600;
  text-transform: capitalize;
}

.node-title {
  flex: 1;
}

.node-status {
  width: 12px;
  height: 12px;
  border-radius: 50%;
}

.status-proved .node-status, .node-proved .node-status { background: var(--bp-success); }
.status-mathlib-ok .node-status, .node-mathlib-ok .node-status { background: var(--bp-primary); }
.status-stated .node-status, .node-stated .node-status { background: var(--bp-warning); }
.status-not-ready .node-status, .node-not-ready .node-status { background: var(--bp-danger); }

.node-content {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 1rem;
  padding: 1rem;
}

@media (max-width: 768px) {
  .node-content {
    grid-template-columns: 1fr;
  }
}

.node-statement, .node-proof {
  padding: 1rem;
  background: var(--bp-bg-alt);
  border-radius: 4px;
}

.node-statement h4, .node-proof h4 {
  margin: 0 0 0.75rem 0;
  font-size: 0.875rem;
  color: var(--bp-text-muted);
  text-transform: uppercase;
  letter-spacing: 0.05em;
}

.node-footer {
  padding: 0.75rem 1rem;
  border-top: 1px solid var(--bp-border);
  font-size: 0.875rem;
  color: var(--bp-text-muted);
}

.node-decls, .node-deps {
  display: inline;
}

.node-decls + .node-deps::before {
  content: " | ";
  margin: 0 0.5rem;
}

.decl-link, .dep-link {
  color: var(--bp-primary);
  text-decoration: none;
}

.decl-link:hover, .dep-link:hover {
  text-decoration: underline;
}

/* Index Page */
.index-header {
  text-align: center;
  margin-bottom: 2rem;
}

.index-header h1 {
  margin-bottom: 1rem;
}

.github-link, .docs-link {
  margin: 0 0.5rem;
  color: var(--bp-primary);
}

/* Graph Section */
.graph-section {
  margin: 2rem 0;
}

.dep-graph-container {
  position: relative;
  width: 100%;
  border: 1px solid var(--bp-border);
  border-radius: 8px;
  background: var(--bp-bg-alt);
  overflow: hidden;
}

.dep-graph-toolbar {
  position: absolute;
  top: 0.5rem;
  right: 0.5rem;
  z-index: 10;
  display: flex;
  gap: 0.25rem;
  background: var(--bp-bg);
  border: 1px solid var(--bp-border);
  border-radius: 4px;
  padding: 0.25rem;
}

.dep-graph-toolbar button {
  padding: 0.25rem 0.5rem;
  border: 1px solid var(--bp-border);
  border-radius: 3px;
  background: var(--bp-bg);
  color: var(--bp-text);
  cursor: pointer;
  font-size: 0.875rem;
  line-height: 1;
  transition: background 0.15s ease;
}

.dep-graph-toolbar button:hover {
  background: var(--bp-bg-alt);
}

.dep-graph-toolbar button:active {
  background: var(--bp-border);
}

.dep-graph-viewport {
  width: 100%;
  height: 500px;
  overflow: hidden;
  cursor: grab;
}

.dep-graph-viewport:active {
  cursor: grabbing;
}

.dep-graph-svg {
  transform-origin: 0 0;
  transition: transform 0.05s ease-out;
}

.dep-graph-svg svg {
  display: block;
}

.dep-graph-placeholder {
  display: flex;
  align-items: center;
  justify-content: center;
  height: 200px;
  color: var(--bp-text-muted);
  font-style: italic;
}

/* Node highlight animation */
.node-highlight {
  animation: node-flash 2s ease-out;
}

@keyframes node-flash {
  0%, 100% { box-shadow: none; }
  20%, 80% { box-shadow: 0 0 0 4px var(--bp-primary); }
}

/* Node Lists */
.node-lists {
  margin: 2rem 0;
}

.node-list h3 {
  margin-bottom: 1rem;
}

.node-index {
  list-style: none;
  padding: 0;
  margin: 0;
}

.node-index li {
  padding: 0.5rem 0;
  border-bottom: 1px solid var(--bp-border);
}

.node-index a {
  color: var(--bp-text);
  text-decoration: none;
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.node-index a:hover {
  color: var(--bp-primary);
}

/* Lean Code Highlighting */
pre.lean-code {
  font-family: var(--bp-font-mono);
  font-size: 0.875rem;
  padding: 1rem;
  overflow-x: auto;
  background: var(--bp-bg);
}

/* Math rendering */
.math.inline { }
.math.display {
  overflow-x: auto;
  padding: 1rem 0;
}
"#

/-- PlasTeX JavaScript - proof toggle and TOC functionality -/
def plastexJs : String := r##"
$(document).ready(function() {
  var icon = function($icon, $class, $id) {
    if ($id) {
      $id = ' id="'+$id+'"';
    } else {
      $id = '';
    }
    return '<svg'+ $id + ' class="icon icon-' + $icon + ' ' + $class +'"><use xlink:href="symbol-defs.svg#icon-'+$icon+'"></use></svg>'
  };

  $("#toc-toggle").click(function() {
    $("nav.toc").toggle()
  });

  $("nav.toc").on("click", "span.expand-toc",
    function() {
      $(this).siblings("ul").slideToggle('fast');
      if ($(this).html() == "▼") {
        $(this).html("▶");
      } else {
        $(this).html("▼");
      };
    })

  $("div.proof_content p:last").append('<span class="qed">□</span>')

  $("div.proof_heading").click(
    function() {
      var expand_span = $(this).children('span.expand-proof');
      if ($(expand_span).html() == "▼") {
        $(expand_span).html("▶");
      } else {
        $(expand_span).html("▼");
      };
      $(this).siblings("div.proof_content").slideToggle()
    })

  $("a.proof").click(
    function() {
      var ref= $(this).attr('href').split('#')[1];
      var proof = $('#'+ref)
      proof.show()
      proof.children('.proof_content').each(
        function() {
          var proof_content = $(this)
          proof_content.show().addClass('hilite')
          setTimeout(function(){
            proof_content.removeClass('hilite')
          }, 1000);
        })
      var expand_icon = proof.find('svg.expand-proof');
      expand_icon.replaceWith(icon('cross', 'expand-proof'));
    })

  $("button.modal").click(
    function() {
      $(this).next("div.modal-container").css('display', 'flex');
    })
  $("button.closebtn").click(
    function() {
      $(this).parent().parent().parent().hide();
    })
});
"##

/-- Verso code hover tooltips and binding highlighting JavaScript -/
def versoCodeJs : String := r#"
/**
 * Verso-style interactive code highlighting for leanblueprint
 *
 * Provides:
 * - Tippy.js-based hover tooltips with type signatures
 * - Token binding highlights (highlight all occurrences of a variable)
 * - Tactic state toggles
 * - Error/warning message popups
 *
 * Ported from Verso (https://github.com/leanprover/verso)
 */

window.addEventListener('DOMContentLoaded', () => {
    // Don't show hovers inside of closed tactic states
    function blockedByTactic(elem) {
        let parent = elem.parentNode;
        while (parent && "classList" in parent) {
            if (parent.classList.contains("tactic")) {
                const toggle = parent.querySelector("input.tactic-toggle");
                if (toggle) {
                    return !toggle.checked;
                }
            }
            parent = parent.parentNode;
        }
        return false;
    }

    function blockedByTippy(elem) {
        var block = elem;
        const topLevel = new Set(["section", "body", "html", "nav", "header", "article", "main"]);
        while (block.parentNode && !topLevel.has(block.parentNode.nodeName.toLowerCase())) {
            block = block.parentNode;
        }
        for (const child of block.querySelectorAll(".token, .has-info")) {
            if (child._tippy && child._tippy.state.isVisible) { return true };
        }
        return false;
    }

    // Token binding highlights
    for (const c of document.querySelectorAll(".hl.lean .token")) {
        if (c.dataset.binding != "") {
            c.addEventListener("mouseover", (event) => {
                if (blockedByTactic(c)) { return; }
                const context = c.closest(".hl.lean").dataset.leanContext;
                for (const example of document.querySelectorAll(".hl.lean")) {
                    if (example.dataset.leanContext == context) {
                        for (const tok of example.querySelectorAll(".token")) {
                            if (c.dataset.binding == tok.dataset.binding) {
                                tok.classList.add("binding-hl");
                            }
                        }
                    }
                }
            });
        }
        c.addEventListener("mouseout", (event) => {
            for (const tok of document.querySelectorAll(".hl.lean .token")) {
                tok.classList.remove("binding-hl");
            }
        });
    }

    // Render docstrings with marked.js if available
    if ('undefined' !== typeof marked) {
        for (const d of document.querySelectorAll("code.docstring, pre.docstring")) {
            const str = d.innerText;
            const html = marked.parse(str);
            const rendered = document.createElement("div");
            rendered.classList.add("docstring");
            rendered.innerHTML = html;
            d.parentNode.replaceChild(rendered, d);
        }
    }

    // Initialize Tippy.js hovers
    const codeBlockHoverData = new Map();

    document.querySelectorAll('.lean-code[data-lean-hovers]').forEach(codeBlock => {
        try {
            const hoverData = JSON.parse(codeBlock.dataset.leanHovers);
            codeBlockHoverData.set(codeBlock, hoverData);
        } catch (e) {
            console.warn('Failed to parse hover data for code block:', e);
        }
    });

    function getHoverDataForElement(element) {
        const codeBlock = element.closest('.lean-code[data-lean-hovers]');
        if (!codeBlock) return null;
        return codeBlockHoverData.get(codeBlock) || null;
    }

    function hideParentTooltips(element) {
        let parent = element.parentElement;
        while (parent) {
            const tippyInstance = parent._tippy;
            if (tippyInstance) {
                tippyInstance.hide();
            }
            parent = parent.parentElement;
        }
    }

    const defaultTippyProps = {
        maxWidth: "none",
        appendTo: () => document.body,
        interactive: true,
        delay: [100, null],
        followCursor: 'initial',
        onShow(inst) {
            const hasVersoHover = inst.reference.dataset.versoHover !== undefined;
            const hasHoverInfo = inst.reference.querySelector(".hover-info");
            if (hasVersoHover || hasHoverInfo) {
                return;
            }
            return false;
        },
        content(tgt) {
            const content = document.createElement("span");
            if (tgt.classList.contains('tactic')) {
                const state = tgt.querySelector(".tactic-state").cloneNode(true);
                state.style.display = "block";
                content.appendChild(state);
                content.style.display = "block";
                content.className = "hl lean popup";
            } else {
                content.className = "hl lean";
                content.style.display = "block";
                content.style.maxHeight = "300px";
                content.style.overflowY = "auto";
                content.style.overflowX = "hidden";
                const hoverId = tgt.dataset.versoHover;
                const hoverInfo = tgt.querySelector(".hover-info");
                if (hoverId) {
                    const hoverData = getHoverDataForElement(tgt);
                    const data = hoverData ? hoverData[hoverId] : null;
                    if (data) {
                        const info = document.createElement("span");
                        info.className = "hover-info";
                        info.style.display = "block";
                        info.innerHTML = data;
                        content.appendChild(info);
                        if ('undefined' !== typeof marked) {
                            for (const d of content.querySelectorAll("code.docstring, pre.docstring")) {
                                const str = d.innerText;
                                const html = marked.parse(str);
                                const rendered = document.createElement("div");
                                rendered.classList.add("docstring");
                                rendered.innerHTML = html;
                                d.parentNode.replaceChild(rendered, d);
                            }
                        }
                    }
                } else if (hoverInfo) {
                    content.appendChild(hoverInfo.cloneNode(true));
                }
                const extraLinks = tgt.parentElement ? tgt.parentElement.dataset['versoLinks'] : null;
                if (extraLinks) {
                    try {
                        const extras = JSON.parse(extraLinks);
                        const links = document.createElement('ul');
                        links.className = 'extra-doc-links';
                        extras.forEach((l) => {
                            const li = document.createElement('li');
                            li.innerHTML = "<a href=\"" + l['href'] + "\" title=\"" + l.long + "\">" + l.short + "</a>";
                            links.appendChild(li);
                        });
                        content.appendChild(links);
                    } catch (error) {
                        console.error(error);
                    }
                }
            }
            return content;
        }
    };

    // Apply Tippy themes to different token types
    document.querySelectorAll('.hl.lean .const.token, .hl.lean .keyword.token, .hl.lean .literal.token, .hl.lean .option.token, .hl.lean .var.token, .hl.lean .typed.token, .hl.lean .level-var, .hl.lean .level-const, .hl.lean .level-op, .hl.lean .sort').forEach(element => {
        element.setAttribute('data-tippy-theme', 'lean');
    });
    document.querySelectorAll('.hl.lean .has-info.warning').forEach(element => {
        element.setAttribute('data-tippy-theme', 'warning message');
    });
    document.querySelectorAll('.hl.lean .has-info.information').forEach(element => {
        element.setAttribute('data-tippy-theme', 'info message');
    });
    document.querySelectorAll('.hl.lean .has-info.error').forEach(element => {
        element.setAttribute('data-tippy-theme', 'error message');
    });
    document.querySelectorAll('.hl.lean .tactic').forEach(element => {
        element.setAttribute('data-tippy-theme', 'tactic');
    });

    // Initialize Tippy on all hoverable elements
    if (typeof tippy !== 'undefined') {
        const selector = '.hl.lean .const.token, .hl.lean .keyword.token, .hl.lean .literal.token, .hl.lean .option.token, .hl.lean .var.token, .hl.lean .typed.token, .hl.lean .has-info, .hl.lean .tactic, .hl.lean .level-var, .hl.lean .level-const, .hl.lean .level-op, .hl.lean .sort';
        tippy(selector, defaultTippyProps);
    }
});

// Sync Lean proof body visibility with LaTeX proof toggle
document.addEventListener('DOMContentLoaded', function() {
    document.querySelectorAll('.sbs-container').forEach(function(container) {
        var expandIcon = container.querySelector('.expand-proof');
        var leanProofBody = container.querySelector('.lean-proof-body');

        if (!expandIcon || !leanProofBody) return;

        // Watch for plastex.js changing the expand icon text
        var observer = new MutationObserver(function() {
            var isExpanded = expandIcon.textContent.trim() === '▼';
            leanProofBody.style.display = isExpanded ? 'inline' : 'none';
        });

        observer.observe(expandIcon, { childList: true, characterData: true, subtree: true });
    });
});
"#

/-- Combined runway.js with both plastex and verso-code functionality -/
def runwayJs : String := plastexJs ++ "\n" ++ versoCodeJs

/-- Write all assets to the output directory -/
def writeAssets (outputDir : System.FilePath) : IO Unit := do
  IO.FS.writeFile (outputDir / "runway.css") blueprintCss
  IO.FS.writeFile (outputDir / "runway.js") runwayJs
  IO.FS.writeFile (outputDir / "plastex.js") plastexJs
  IO.FS.writeFile (outputDir / "verso-code.js") versoCodeJs

end Runway.Assets
