#import "@preview/mitex:0.2.7": *

#set document(title: "Programiranje u Haskelu", author: "Nikola Ubavić")
#set text(lang: "sr", script: "latn", size: 11pt)
#set par(justify: true)
#set heading(numbering: "1.1.")
#set page(paper: "a4", numbering: "1", margin: 2.2cm)

#show raw.where(block: true): it => block(width: 100%, fill: luma(245), inset: 8pt, radius: 3pt, it)

#show heading.where(level: 1): it => {
  pagebreak(weak: true)
  it
}

#show figure.caption: it => align(left, text(fill: black.transparentize(30%), it))
#show figure: set align(left)
#show figure.where(kind: image): set align(center)
#set figure(numbering: none, supplement: none)

#let sidebar(color, body) = block(
  width: 100%,
  inset: (left: 12pt, y: 8pt),
  stroke: (left: 2.5pt + color),
  body,
)

#let example(number, body) = sidebar(rgb("#97bbc7"))[
  *Primer #number.*
  #body
]

#let problem(number, statement, solution: none) = sidebar(rgb("#c79797"))[
  *Zadatak #number.* #statement
  #if solution != none {
    parbreak()
    [_Rešenje_. ]
    solution
  }
]

#page(
  fill: rgb("#382e57"),
  numbering: none,
  header: none,
  footer: none,
  margin: 2.5cm,
)[
  #set text(fill: white)
  #set par(justify: false)
  #align(center + horizon)[
    #text(size: 32pt, weight: "bold")[Programiranje u Haskelu]
    #v(0.8em)
    #text(size: 14pt)[Knjiga o funkcionalnom programiranju]
    #v(2.5em)
    #text(size: 13pt)[Nikola Ubavić]
    #v(2.5em)
    #text(size: 12pt)[#datetime.today().display("[day].[month].[year]")]
  ]
]

#counter(page).update(1)
#outline(title: [Sadržaj], indent: auto, depth: 2)
#pagebreak()
