# render_sbgn_r

> [!IMPORTANT]
> This repository is archived. Active development has moved to
> [cannin/render_sbgn](https://github.com/cannin/render_sbgn). The R
> implementation is now maintained in the monorepo's
> [r directory](https://github.com/cannin/render_sbgn/tree/main/r).

SBGNML renderer implemented in R using base graphics and the xml2 parser.

## Installation

Install directly from GitHub:

```r
install.packages("remotes")
remotes::install_github("cannin/render_sbgn_r")
```

## Usage

Render a single SBGNML file from R:

```r
renderSbgnR::draw_sbgnml("input.sbgn", "output.png")
renderSbgnR::draw_sbgnml("input.sbgn", "output.svg")
```

Or run the source-checkout CLI:

```bash
Rscript draw_sbgnml.R --input input.sbgn --output output.png
```

Render all examples (PNG + SVG):

```sh
Rscript render_examples.R
```

## Notes

- SVG output is generated alongside PNG output using R's `svg()` device.
- Package installation installs `xml2` and `jsonlite` as dependencies.
- Font rendering assumes Liberation Sans is available on the system.
