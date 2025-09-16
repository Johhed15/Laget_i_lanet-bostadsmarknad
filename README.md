# Rapport Mall

Det finns två sätt att ha startsida:

## A) Egen landningssida (index.html)
- Fil: `index.html` + `landing.css`
- `_quarto.yml` måste **behålla**:
  - `resources: [index.html, landing.css]`
  - `render: ["*.qmd", "!index.qmd"]` (dvs. **utesluter** en ev. `index.qmd`)
  - Navbar-länkar pekar mot `index.html`

_____________________________________________________________________________________
Exempel: Med startsida för de större rapporterna 
### A) **MED landningssida (index.html)**

```yaml
project:
  type: website
  output-dir: _site
  render:
    - "*.qmd"       # vi renderar alla qmd...
    - "!index.qmd"  # ...men INTE en ev. index.qmd (vi använder index.html)
  resources:
    - index.html    # behåll dessa
    - styles.css
    - landing.css
    - Figurer/**

website:
  navbar:
    logo-href: index.html
    left:
      - text: "Start"
        href: index.html
      - text: "Om"
        href: Laget_i_lanet_Mall.qmd
format:
  html:
    css: styles.css
    self-contained: false
```
_____________________________________________________________________________________

## B) Ingen landningssida – använd Quarto-sida som start
- **Ta bort** `index.html` (och ev. `landing.css`)
- Skapa/byt namn på din startsida till `index.qmd`
- I `_quarto.yml` måste du **ändra**:
  - Ta bort `!index.qmd` från `render:`
  - Ta bort `index.html` och `landing.css` från `resources:`
  - Låt navbar-länken peka på `index.qmd`
- Kör:
  ```bash
  quarto clean   # rensa _site
  quarto render

_____________________________________________________________________________________
Exempel: utan startsida (landing)

```yaml
project:
  type: website
  output-dir: _site
  render:
    - "*.qmd"         # rendera alla qmd, inklusive index.qmd
  resources:
    - styles.css      # ta bort index.html och landing.css här
    - Figurer/**

website:
  navbar:
    logo-href: index.qmd   # eller bara "index.html" -> "index.qmd"
    left:
      - text: "Start"
        href: index.qmd
      - text: "Om"
        href: Laget_i_lanet_Mall.qmd
format:
  html:
    css: styles.css
    self-contained: false
```
_____________________________________________________________________________________


## Referenser 

referenser kan man skriva i references.bib filen 
----> exempel:

@report{tilv2024,
  author  = {Region Uppsala},
  title   = {Tillväxtrapport 2024},
  year    = {2024},
  url     = {https://exempel.se/rapport.pdf}
}

I text refererar man -> @--källa--

Obs. man måste lägga -> bibliography: references.bib <- i yamallen i respektiv .qmd fil
