<a href="faqs.html"><IMG src="supraHex_logo.png" height="150px" id="logo"></a>

<B><h4>An open-source R/Bioconductor package for tabular omics data analysis using `a supra-hexagonal map`</h4></B>

## News
The artwork called <a href="Best_Artwork_Award_ISMB2014.pdf" target="artwork" style="font-size: 12px; color: #F87217; text-decoration: overline; border-bottom: 1px solid #F87217">supraHex</a> has won <a href="http://www.iscb.org/ismb2014-general-info/2234-ismb2014-award-winners#art" target="ISMB2014" >The Best Artwork Award in ISMB 2014</a>. This artwork is automatically done and is reproducible <a href="demo-ISMB2014.html" target="ISMB2014">here</a>.

## Introduction

`The supra-hexagonal map` is a giant hexagon on a 2-dimensional map grid seamlessly consisting of smaller hexagons. 

`supraHex` intends to meet the need for quickly understanding genome-wide biological data, which usually involve a large number of genomic coordinates (e.g. genes) but a much smaller number of samples. 

`supraHex` first uses a supra-hexagonal map to self-organise the input omics data, and then post-analyses the trained map for integrated tasks: simultaneous analysis of genes and samples, and multilayer omics data comparisons.

`supraHex` aims to deliver an eye-intuitive tool and a dedicated website with extensive online documentation and easy-to-follow demos.

For more, see <a href="slides_supraHex.pdf" target="slides" style="font-size: 12px; color: #F87217; text-decoration: overline; border-bottom: 1px solid #F87217">slides</a> and <a href="poster_ISMB2014.png" target="slides" style="font-size: 12px; color: #F87217; text-decoration: overline; border-bottom: 1px solid #F87217">poster in ISMB2014</a>.


## Features

* The supra-hexagonal map trained via a self-organising learning algorithm;
* Visualisations at and across nodes of the map;
* Partitioning of the map into gene meta-clusters;
* Sample correlation on 2D sample landscape;
* Overlaying additional data onto the trained map for exploring relationships between input and additional data;
* Support for heatmap and tree building and visualisations;
* Used by the package [dnet](http://dnet.r-forge.r-project.org) for network-based sample classifications;
* This package can run on `Windows`, `Mac` and `Linux`.

## Workflow

<a href="javascript:newWin('supraHex_workflow.png', 'supraHex_workflow.png', '1200', '600')" title="Click to enlarge"><img style="max-width:95%;border:1px solid #EEEEEE;box-shadow:5px 5px 2px #C0C0C0;" src='supraHex_workflow.png', width="800px" /></a>
