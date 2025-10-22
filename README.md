# DRAFT TILL Bostadsbestånd och bostadsmarknad i Uppsala län

Detta är ett Quarto-projekt som visualiserar och analyserar bostadsbestånd och bostadsmarknad i Uppsala län.

## Struktur

-   `index.qmd` – startsida för webbplatsen
-   `Data/` – alla datakällor
-   `Script/` – R-skript för laddning och bearbetning
-   `Figurer/` – alla visualiseringar utom DeSo-karta

## Hur man bygger webbplatsen

-   I terminalen skriv \* quarto render

då skapas mappen "\_site" där index.html hittas

## Bilder

Alla grafer är sparade och finns i mappen figurer, de flesta sparas genom att köra dess funktion från scriptet create_save_plots.R, vissa är sparade genom plotlys interaktiva funktion.

För att rendera rapporten så behöver bilder från regionens mediabank laddas ned, dessa läses in i rapporterna med samma namn som de laddas ner med. Alla nedladdade bilder från mediabanken ska sparas i en mapp som heter 'Mediabank', i .gitignore så ställs det in så att dessa inte laddas upp på github utan sparas endast lokalt från nedladdningen på Region Uppsalas mediabank.
