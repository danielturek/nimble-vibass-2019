---
title: "VIBASS NIMBLE Workshop: Logistics"
subtitle: "Applied Bayesian Computation: The NIMBLE Platform for Hierarchical Modeling and MCMC"
author: "Daniel Turek, Department of Mathematics & Statistics, Williams College"
output: html_document
---

## Where to find stuff

The Github site (<a href="https://github.com/danielturek/nimble-vibass-2019" target="_blank">https://github.com/danielturek/nimble-vibass-2019</a>) is the master repository for materials for the modules; you can also get the individual files from Github. 

To get the materials look ahead to slide 7 for instructions. Alternatively, simply go to <a href="https://github.com/danielturek/nimble-vibass-2019" target="_blank">https://github.com/danielturek/nimble-vibass-2019</a> and click on the 'Download ZIP' button in the right side of the window or simply <a href="https://github.com/danielturek/nimble-vibass-2019/archive/master.zip" target="_blank">click here</a>.

## Asking questions and making comments

Please ask questions and make comments as we go through the material. We will also have a shared document (Etherpad) at <a href="https://etherpad.net/p/nimble-vibass-2019" target="_blank">https://etherpad.net/p/nimble-vibass-2019</a> where you can more easily post longer code snippets or error messages.

## How we'll operate

The workshop will be organized in modules, each of which will be a combination of presentation (lecture and demos), in most cases followed by exercises/breakout work.

Feel free to work on your own models/methods during the breakout time rather than the questions I pose.

Each module has a set of three files:

 - The underlying R Markdown source file with text and code, e.g., `foo.Rmd`
 - The HTML slides generated from that R Markdown, e.g., `foo_slides.html`
 - The slides as a single HTML file, e.g., `foo.html`

If you're curious, you can regenerate the html files using the `make_slides` script, which can be run in a shell as `make_slides foo`.

## Suggestions on how to get the most out of the workshop

I encourage you to:

- Try out the code as we walk through it
- Keep your eyes open! -- We'll illustrate a lot of syntax and concepts by example
- Try to guess what the syntax means in cases we haven't yet seen that syntax
- Play with it and try variations and try to break it and see what happens
- Post a comment on the Etherpad if something interesting happens as you experiment
- Ask questions as we go


A few additional thoughts on my pedagogical philosophy here:

- I want to expose you to a lot of tools and ideas that you can come back to when you need them and when you have time for more depth.
- My aim is not to show you how to use specific statistical methods or analysis techniques. My goal is to get you up to speed generally in MCMC, programming algorithms, and NIMBLE.
- It can be hard to absorb everything in such a concentrated situation. Hopefully the material presented will be useful to you over the longer-term as you do more computation and work with NIMBLE.

## RStudio and R Markdown

We'll present most of the material from within RStudio, using R Markdown documents with embedded R code. R Markdown is an extension to the Markdown markup language that makes it easy to write HTML in a simple plain text format.  This allows us to both run the R/NIMBLE code directly as well as compile on-the-fly to an HTML file that can be used for presentation. All files will be available on <a href="https://github.com/danielturek/nimble-vibass-2019" target="_blank">GitHub</a>.

Warning: in some cases the processing of the R code in the R Markdown is finicky, and the slides have error messages that do not occur if you just run the code directly in R or RStudio. 

## Getting the workshop materials

To download the files, you can do any of the following. 

#### From a browser

Simply go to <a href="https://github.com/danielturek/nimble-vibass-2019" target="_blank">https://github.com/danielturek/nimble-vibass-2019</a> and click on the 'Download ZIP' button in the right side of the window or simply <a href="https://github.com/danielturek/nimble-vibass-2019/archive/master.zip" target="_blank">click here</a>.

<!--
#### Within RStudio

Within RStudio go to File->New Project->Version Control->Git and enter:

- "Repository URL": https://github.com/danielturek/nimble-vibass-2019
- "Project Directory Name": nimble-vibass-2019 (or something else of your choosing)
- "Directory": ~/Desktop (or somewhere of your choosing)

Then to update from the repository to get any changes we've made, you can select (from within RStudio):
Tools->Version Control->Pull Branches

or from the Environment/History/Git window, click on the Git tab and then on the blue down arrow.

Be warned that you probably do not want to make your own notes or changes to the files we are providing. Because if you do, and you then do a "Git Pull" to update the materials, you'll have to deal with the conflict between your local version and our version. You probably will want to make a personal copy of such files in another directory or by making copies of files with new names.
-->

#### From a terminal window (MacOS, Linux, or Windows Linux subsystem)

Run the following commands:

- `cd /directory/where/you/want/repository/located`
- `git clone https://github.com/danielturek/nimble-vibass-2019`

Then to update from the repository to get any changes we've made:

- `cd /directory/where/you/put/the/repository/nimble-vibass-2019`
- `git pull`

## Installing NIMBLE

NIMBLE is on CRAN, so in general it will be straightforward to install, but you do need a compiler and related tools on your system.  

In summary, here are the steps.

1. Install compiler tools on your system. <a href="https://r-nimble.org/download" target="_blank">https://r-nimble.org/download</a> has more details on how to install *Rtools* on Windows and how to install the
command line tools of *Xcode* on a Mac.
2. Install the *nimble* package from CRAN. 

More details can also be found in Section 4 of the <a href="http://r-nimble.org/manuals/NimbleUserManual.pdf" target="_blank">NIMBLE User Manual</a>.

For some of the later examples we'll discuss, you'll need version 0.8.0 (new on CRAN as of June 2019).

There are a couple other things that we'll demo that have bug fixes as of version 0.8.0, so it's best to install that version.

\  

\  

\  

\  

\  

\  

\  

\  

\  

\  

\  

\  

\  

\  

\  

