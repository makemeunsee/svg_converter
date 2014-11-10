SVG paths to lines
==================

Turns paths to polylines in a svg document. Downsample / pixellize the lines in the process.

Usage
=====

Start sbt, then

run [svg path] ['pixel' size] [keep singular borders] [allow diagonals] [stroke width] [base filling color]

Example: run www/BlankMap-Equirectangular.svg 1 false true 0.1 FF6666

Outputs to www/out.svg

Not specifying a base filling color sets all polylines fill attribute to 'none'.

Gallery
=======

![Sample](sample.png "Sample")

[Before/after demo](http://www.jollycyb.org/svg/) (pre rendered)
