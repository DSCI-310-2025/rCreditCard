# rCreditCard

**rCreditCard** is an R package designed to simplify and streamline visual analysis of credit card client data. It provides easy-to-use functions for generating informative plots including scatter plots, faceted views, and target distribution plots.

## Why this package?

While packages like [`ggplot2`](https://ggplot2.tidyverse.org/) and [`DataExplorer`](https://cran.r-project.org/package=DataExplorer) offer broad visualization capabilities, **rCreditCard** focuses specifically on credit card data in classification or customer default prediction tasks. It wraps common visualizations into high-level functions tailored to financial dataset structures.

## Key Features

- Clean data visualization for credit default datasets
- Faceted scatter plots for feature comparison
- Target distribution plots to assist in identifying imbalances
- Easy integration into analysis pipelines

## Installation

```r
# Install development version from GitHub
devtools::install_github("DSCI-310-2025/rCreditCard")
```

## Example

```r
library(rCreditCard)

clean_df <- clean_data(raw_df)
make_scatter_facet_plot(clean_df)
make_target_plot(clean_df, target = "default.payment.next.month")
```

# License

This package is licensed under the MIT License — see [LICENSE](LICENSE) for details.


MIT License

Copyright (c) 2025 Group 12

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the “Software”), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

---

# CODE_OF_CONDUCT.md

# Contributor Covenant Code of Conduct

## Our Pledge

We as members, contributors, and leaders pledge to make participation in our community a harassment-free experience for everyone, regardless of age, body size, disability, ethnicity, gender identity and expression, education, level of experience, nationality, personal appearance, race, religion, or sexual identity and orientation.

We pledge to act and interact in ways that contribute to an open, welcoming, diverse, inclusive, and healthy community.

## Our Standards

Examples of behavior that contributes to a positive environment:
- Demonstrating empathy and kindness toward other people
- Being respectful of differing opinions, viewpoints, and experiences
- Gracefully accepting constructive criticism
- Focusing on what is best for the community

Examples of unacceptable behavior:
- The use of sexualized language or imagery
- Trolling, insulting or derogatory comments
- Public or private harassment
- Other conduct which could reasonably be considered inappropriate

## Enforcement

Instances of abusive, harassing, or otherwise unacceptable behavior may be reported to the project team at **pjymar28@gmail.com**. All complaints will be reviewed and investigated and will result in a response that is deemed necessary and appropriate.

## Attribution

This Code of Conduct is adapted from the [Contributor Covenant](https://www.contributor-covenant.org/), version 2.1.

---

# CONTRIBUTING.md

# Contributing to rCreditCard

Thanks for your interest in contributing to **rCreditCard**! We welcome contributions from everyone.

## Workflow Guidelines

- Use feature branches: work on new features or bug fixes in dedicated branches named `feature/your-feature` or `fix/your-fix`.
- Open a pull request (PR) when your work is ready for review.
- Write clear commit messages (e.g., "Add target plot function").
- All functions must include roxygen2 documentation and unit tests.
- Run `devtools::check()` and `devtools::test()` before submitting your PR.
- Include examples in exported functions where possible.

## Development Setup

1. Fork the repository and clone your fork locally.
2. Install development dependencies:

```r
install.packages(c("devtools", "roxygen2", "testthat"))
```

3. Use `devtools::load_all()` to load your package as you work.
4. Add tests to `tests/testthat/`.

## Code Style

Follow the [tidyverse style guide](https://style.tidyverse.org/):
- Use `<-` for assignment.
- Snake_case for object names.
- One function per file (in `R/`).

We appreciate your help in improving this package!
