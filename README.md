# Bostadsbestånd och bostadsmarknad i Uppsala län

Detta är ett Quarto-projekt som visualiserar och analyserar bostadsbestånd och bostadsmarknad i Uppsala län.

## Struktur

- `index.qmd` – startsida för webbplatsen
- `Data/` – alla datakällor
- `Script/` – R-skript för laddning och bearbetning
- `Figurer/` och `bilder/` – visualiseringar

## Hur man bygger webbplatsen

* I terminalen skriv *
quarto render

då skapas mappen "_site" där index.html hittas

## Bilder
Alla grafer är sparade och finns i mappen figurer, de flesta sparas genom att köra dess funktion från scriptet create_save_plots.R, vissa är sparade genom plotlys interaktiva funktion.
