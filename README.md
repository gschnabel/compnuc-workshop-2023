## Materials for the Joint ICTP-IAEA School on AI for Nuclear, Plasma and Fusion Science

This repository contains the two lectures given at the school in `.odp` and `.pdf` format
as well as the Jupyter notebook of the hands-on workshop. The titles of the presentations
are:

- Introduction to nuclear data
- Introduction to probabilistic methods for nuclear data (An introduction to Gaussian process regression)

The presentations contain animations, which do not work in the pdf files. 
They should render correctly, though, if shown with LibreOffice Impress or compatible software.
Many of the animations and plots are also available in the `plots` directory and the R scripts
to produce them in the `plot_scripts` directory. Videos in `.mp4` format are available in
the `videos` directory.

The provided Jupyter notebooks cover the implementation of Gaussian process regression with
Tensorflow Probability. In this specific implementation the Gaussian process is discrtized
and therefore replaced by a multivariate normal distribution. The link to synthetic observations
is achieved by linear interpolation.
