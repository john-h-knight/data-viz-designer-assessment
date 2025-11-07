# Data Visualization Designer Assessment

This assessment involves reviewing a CORI publication, analyzing the data and graphics used, and creating improved R-based visualizations that tell a more compelling story. It should take approximately 2-3 hours to complete. We want to be respectful of your time and don't expect perfection. If you run into any issues, don't hesitate to reach out!

## The Task

For this exercise, review our recent publication on **[Rural America's Struggle to Access Private Capital](https://ruralinnovation.us/resources/reports/rural-americas-struggle-to-access-private-capital/)**. In the material provided, you have access to two high-impact chart graphics (also in `docs/` and reproduced below) and their corresponding data (in `data/`) that were utilized to support the article. Use the data provided to generate R-based graphics that work to enrich the images provided.

![docs/rural_nonrural_employment.png](docs/rural_nonrural_employment.png)

![docs/rural_nonrural_sectoral_employment.png](docs/rural_nonrural_sectoral_employment.png)

## Steps

**1) Fork this repository and clone it to your local computer**

If you'd prefer to keep your assessment private, feel free to clone this repo (rather than fork), create a private repository on GitHub, push your work there, and add us as collaborators when you're ready.

**2) Review the publication**

Read through **[Rural America's Struggle to Access Private Capital](docs/CORI_Rural-America-Struggle-Access-Private-Capital_May2025-compressed.pdf)** (which is available in the `docs` folder of this project) to understand the narrative, key findings, and how the existing graphics support the story.

**3) Build improved R graphics**

Take the provided data and build improved R graphics that tell a "better" story. The resulting graphics should be built in R. We have provided our organization's style/branding guidelines in the custom built package [cori.charts](https://ruralinnovation.github.io/cori.charts/).

```r
# install.packages("devtools")
devtools::install_github("ruralinnovation/cori.charts")
```

[`cori.charts`](https://github.com/ruralinnovation/cori.charts) contains several useful functions for loading fonts, implementing consistent styling, and exporting graphics:

- `load_fonts()` loads relevant Google Fonts (Lato) into your local environment.
- `theme_cori()` applies standardized formatting (consistent margins, fonts, etc.)
- `theme_cori_horizontal_bars()` extends theme_cori with horizontal bar specific styling.
- `theme_cori_map()` extends theme_cori with map specific styling.
- `save_plot()` exports images with a CORI logo and consistent formatting.

Feel free to reference our ["cookbook"](https://ruralinnovation.github.io/cori.charts/articles/cookbook.html) for chart examples. Save your charts as PNG files in the `export/` subfolder using `cori.charts::save_plot()`.

**4) Write a narrative for your improved graphics**

Following the spirit of the publication, produce a concise but detailed narrative (no longer than 1 page) that tells the story of your improved graphics. Ensure that your story and the graphics meld with the natural flow of the publication. _In theory, it should be able to easily replace the current story and graphics._

**5) Provide an analysis of your design choices**

Apart from the story narrative, provide a short analysis of the improved graphics explaining how they are superior to the previous version (no longer than 2-3 paragraphs). 
Include:
- Your rationale for your design choices
- How the new graphics better support the article's message
- Any methodological improvements or additional insights revealed

**6) Submit your work**

Please provide:
- The R code used to generate your graphics
- The resulting graphics (PNG files)
- A markdown file containing:
  - Your narrative (≤1 page)
  - Your design analysis (2-3 paragraphs)

When you are finished, commit and push your changes to GitHub and notify us in Workable. If working in a private repository, also add [Drew Rosebush](https://github.com/drewrosebush) to the repository as a collaborator.

## Misc Notes

- This repository is based on our standard data project template. By convention:
  - R scripts go in the `R/` folder
  - Data files (e.g., CSVs, Parquet files) go in the `data/` folder
  - Exported images (e.g., charts, maps) go in the `export/` folder
  - Documentation and narratives go in the root directory or a `docs/` folder
- Please comment your code where it helps explain your process or decisions.
- Focus on clarity, insight, and visual storytelling in your improved graphics.
