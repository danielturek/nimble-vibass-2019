---
title: "Applied Bayesian Computation: The NIMBLE Platform for Hierarchical Modeling and MCMC"
subtitle: "Logistics"
output: html_document
---


## Getting the Workshop Materials

The Github site (<a href="https://github.com/danielturek/nimble-vibass-2019" target="_blank" style="color: blue">https://github.com/danielturek/nimble-vibass-2019</a>) is the master repository for materials for the modules; you can also get the individual files from Github. 

To get the materials look ahead for detailed instructions. Alternatively, simply go to <a href="https://github.com/danielturek/nimble-vibass-2019" target="_blank" style="color: blue">https://github.com/danielturek/nimble-vibass-2019</a> and click on the 'Download ZIP' button in the right side of the window or simply <a href="https://github.com/danielturek/nimble-vibass-2019/archive/master.zip" target="_blank" style="color: blue">click here</a>.

#### From a terminal window (MacOS, Linux, or Windows Linux subsystem)

Run the following commands:

- `cd /directory/where/you/want/repository/located`
- `git clone https://github.com/danielturek/nimble-vibass-2019`

Then to update from the repository to get any changes we've made:

- `cd /directory/where/you/put/the/repository/nimble-vibass-2019`
- `git pull`



## RStudio and R Markdown

We'll present most of the material from within RStudio, using R Markdown documents with embedded R code. R Markdown is an extension to the Markdown markup language that makes it easy to write HTML in a simple plain text format.  This allows us to both run the R/NIMBLE code directly as well as compile on-the-fly to an HTML file that can be used for presentation. All files will be available on <a href="https://github.com/danielturek/nimble-vibass-2019" target="_blank" style="color: blue">GitHub</a>.





## Installing NIMBLE

NIMBLE is on CRAN, so in general it will be straightforward to install, but you do need a compiler and related tools on your system.  

In summary, here are the steps.

1. Install compiler tools on your system. <a href="https://r-nimble.org/download" target="_blank" style="color: blue">https://r-nimble.org/download</a> has more details on how to install *Rtools* on Windows and how to install the
command line tools of *Xcode* on a Mac.
2. Install the *nimble* package from CRAN. 

More details can also be found in Section 4 of the <a href="http://r-nimble.org/manuals/NimbleUserManual.pdf" target="_blank" style="color: blue">NIMBLE User Manual</a>.

For some of the later examples we'll discuss, you'll need version 0.8.0 (new on CRAN as of June 2019).

There are a couple other things that we'll demo that have bug fixes as of version 0.8.0, so it's best to install that version.



<!--
## Asking questions and making comments

Please ask questions and make comments as we go through the material. We will also have a shared document (Etherpad) at <a href="https://etherpad.net/p/nimble-vibass-2019" target="_blank" style="color: blue">https://etherpad.net/p/nimble-vibass-2019</a> where you can more easily post longer code snippets or error messages.
-->



## How we'll operate

The workshop will be organized in modules, each of which will be a combination of presentation (lecture and demos), in most cases followed by exercises/breakout work.

Feel free to work on your own models/methods during the breakout time rather than the questions I pose.

Each module has a set of three files:

 - The underlying R Markdown source file with text and code, e.g., `foo.Rmd`
 - The HTML slides generated from that R Markdown, e.g., `foo_slides.html`
 - The slides as a single HTML file, e.g., `foo.html`



## Suggestions on how to get the most out of the workshop

I encourage you to:

- Try out the code as we walk through it
- Keep your eyes open! -- We'll illustrate a lot of syntax and concepts by example
- Try to guess what the syntax means in cases we haven't yet seen that syntax
- Play with it and try variations and try to break it and see what happens
- Ask questions as we go


A few additional thoughts on my pedagogical philosophy here:

- I want to expose you to a lot of tools and ideas that you can come back to when you need them and when you have time for more depth.
- My aim is not to show you how to use specific statistical methods or analysis techniques. My goal is to get you up to speed generally in MCMC, programming algorithms, and NIMBLE.
- It can be hard to absorb everything in such a concentrated situation. Hopefully the material presented will be useful to you over the longer-term as you do more computation and work with NIMBLE.



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

