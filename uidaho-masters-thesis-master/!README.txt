This is Austin Nathaniel Fife's Thesis presented for a MSC
at the University of Idaho in 2018.

This folder includes all of the files associated with using a LaTex template
to write a Master's thesis. I wouldn't recommend it to anyone else, LaTex has a
very steep learning curve and a singular (but helpful) community. I plant on using R markdown
 and bookdown for my future dissertation, it is much more intuitive,
better supported and easier to convert to MS Word for collaborative editing.

There are four important main files and one important folder here, the rest
is generated when running LaTex. 

Mainly:
--'austin_n_fife_thesis_2018.pdf'
This is the main output file of LaTex, the thesis itself.
It is created from running 'austin_n_fife_thesis_2018.tex'

--'austin_n_fife_thesis_2018.tex'
This is a set of commands and text telling LaTex how to create the .pdf
document. It relies on the template 'UIdahoMastersThesis.cls' to work.
You need a download and install a 'Tex distribution' to run this file to
create the .pdf output

--'UIdahoMastersThesis.cls'
this file is a 'class' template file used to tell LaTex general formatting
information.

--'af_thesis_refs.bib'
this file contains all of the bibliographic information included
in the thesis. It is in bibtex format.

-- 'Figures'
contains figures used in the thesis



These are less important, but still interesting:
-- 'supplementary information'
contains the protocols and results from Karin Cruzado and the
modified leaf staining protocol used to test for salivary sheaths

--'thesis drafts'
contains some of the many revisions Erik and I made to the thesis,
as well as comments made from the committee.

The remainder of the files are related to creating the pdf file from the
tex file. They can be deleted or used for debugging.