* Literature Research Worklflow project  
Sometimes I want to refer to (cite) "org-ref style" specific passages and sentences from books that I have stored all over my file system and that don't have a consistent naming scheme. I would like to add a bibtex entry, do a fuzzy search for the file right there within emacs, and put the path to the file in an additional bibtex entry field called file or filepath. (At every export of the bibtex file to an outside source, I will remove this filepath, since the readers won't need that information.)
This would allow me to jump directly to a specific page and position in a pdf (information given inside the cite command). 

The cite command could be created by a 2 simple functions that 
1. store a link-type string in a variable on command (having e.g. selected some text in a pdf at a certain position (in emacs pdf-tools)) 
and 
2. insert a link to that pdf (a priori only a filename) and to that position. If the filename is not contained in any of the ~file = {...}~ fields of any of the bibtex entries in the associated =.bib= file, a notification is shown and the need to create a bibtex entry for the link's filename is expressed. That can then also be done pretty easily. 


Concretely: 
If I am in an org file and want to cite something, that is not a paper, or available in crossref, but I only have as pdf, I do this:

It needs a bibtex entry, to create that, I call
#+BEGIN_SRC emacs-lisp
M-x org-ref-bibtex-new-entry/bibtex-Book-and-exit
#+END_SRC
in the bibtex entry, I specify author/title/...
a feature I want is to write a function that, based on the title, does a search and finds the full file path and puts it in a separate ~file = {"~/whatever/path.pdf"}~ field. That way, I can have my files in separate folders, all over my file structure and they can have custom names. 

* useful stuff 
- In https://github.com/jkitchin/org-ref/issues/172, introducing a  ~file~ bibtex field with the filename is discussed. It may even integrate well with org-ref. 

- if the current buffer is an org-mode file that references your bib file (i.e. with ~bibliography:book.bib~ somewhere at the top), you can get the bibtex entry's text from the entry's key using e.g.
#+BEGIN_SRC emacs-lisp
(org-ref-get-bibtex-entry "elb05")
#+END_SRC

- you can get the full file name of the book from launching a command prompt background search
#+BEGIN_SRC emacs-lisp
(nth 0 (split-string (shell-command-to-string "find ~/Dropbox -name \"*elberfel*\"") "\n"))
#+END_SRC

- my custom function ~org-pdfview-store-link~ does a similar thing as ~M-x org-store-link~, it stores a custom-format link, ready to be inserted into the org buffer using ~M-x org-insert-link~ =return= =return=. Upon calling ~org-open-at-point~, the pdf opens directly at the file and page specified in the link. 
* what is already possible but not sufficient
- org-ref allows opening pdfs from cite links directly, but the  referenced pdfs (for that org file) must all be in the same path and their filenames must match the bibtex key names. I think it does not provide the functionality to cite a pdf with filename, page and position or open a citation's pdf (filename) at page and position (which I try to make, to allow more detail), but it already provides a very useable workflow. 
  See e.g. [[https://github.com/syl20bnr/spacemacs/issues/9013][this github thread]] and [[https://github.com/jkitchin/org-ref/issues/187][this one]] for a bit of discussion of how to cite bibtex entries with associated pdfs (only the files, not pages or positions) that must not all be in the same folder, but still adhere to the bibtex-key naming scheme.
