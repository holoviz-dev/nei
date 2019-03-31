[![BuildStatus](https://travis-ci.org/pyviz/nei.svg?branch=master)](https://travis-ci.org/pyviz/nei)
[![PyPI](https://img.shields.io/pypi/v/nei.svg)](https://pypi.python.org/pypi/nei)

# NEI

**Notebook Editor Interface.**


<img src="https://s3-eu-west-1.amazonaws.com/misc-static-assets/nei-readme-5mb.gif" width="90%"><img>

The goal of NEI is to allow Python [Jupyter
notebooks](https://jupyter.org/) to be edited efficiently as plaintext,
allowing you to use all the text and code editing tools you are familiar
with to work as effectively with notebooks as with regular code files.

The above GIF shows a demo of NEI being used in emacs on the
left-hand side while the results reflected live on the right-hand side
in Firefox. The file in emacs is a regular Python file using vanilla
Python syntax - the yellow code prompts are simply pretty printed for
clarity. For instance, the yellow ``In[1]`` is simply the visual
representation of the comment ``# In[1]``.

Vim support currently exists only as a proof of concept as the core code
stabilizes. Once NEI is released for emacs, a robust Vim interface is
planned with the help of contributors.

## Motivation

There are several projects offering alternatives to the [Jupyter
notebook](https://jupyter.org/) interface such as
[hydrogen](https://github.com/nteract/hydrogen) and
[nteract](https://github.com/nteract/nteract). Emacs users can already
make use of the excellent
[EIN](https://github.com/millejoh/emacs-ipython-notebook) project so why
use NEI?

Unfortunately EIN cannot display HTML and JavaScript output as there is
no way to inline such content. Emacs is not based on web technologies
(thankfully!) which means it does not have access to the required
rendering engines. This limitation greatly reduces the utility of EIN if
you have to work with interactive visualizations such as those generated
by [pyviz](pyviz.org) tools (most importantly
[holoviews](http://holoviews.org/),
[bokeh](https://bokeh.pydata.org/en/latest/),
[datashader](http://datashader.org/) and
[panel](http://panel.pyviz.org/))

The core features are NEI are:

1. Work with notebooks containing interactive visualizations
   without leaving your editor.
2. Freeform text editing without hard cell boundaries. Notebooks can now
   be edited and worked with as regular, plaintext Python files.
3. Editor agnostic by design, with robust Emacs support and planned Vim support.


To view notebook output you will need to have both your editor and
browser visible simultaneously as shown in the GIF above. This is no
problem for people working on multiple monitors and laptop users should
have no trouble splitting the screen, especially if they can easily tile
windows e.g OSX users can use
[spectacle](https://github.com/eczarny/spectacle). The partially
decoupled view in the web browser avoids the problem of having code
change position as output is generated and will allow NEI to view select
portions of the overall notebook as you work.

NEI is composed of three components, (1) a Python server using
[tornado](http://www.tornadoweb.org/en/stable/) which receives commands
from the editor via websockets, (2) HTML and Javascript that runs in the
browser, and (3) the code used to integrate with the editor. As
[emacs](http://emacs.org) is the first editor to be supported, this last
component largely consists of elisp. Rudimentary Vim support has been
prototyped and it is hoped that robust Vim support will be possible in
future. As the server and web component are editor agnostic, there is no
reason NEI cannot be extended to support any text editor that has
support for websockets.

**NEI is currently an experimental prototype and should not be
considered stable.**

## Python dependencies

You can conda install the four core python dependencies as follows:

```
conda install jupyter_client nbconvert nbformat ipykernel
```

There is an additional optional dependency on ``cssutils`` which is only
used to apply theming to holoviews bokeh plots. You can install
``cssutils`` with:

```
pip install cssutils
```

## Browser requirements

NEI is written in ES6 and is not yet configured to compile to ES5 with
babel. This means NEI should work in any recent browser but will not be
compatible with older versions.


## Tips

NEI currently supports [miniconda3](https://conda.io/miniconda.html)
environments. In order to specify an environment when working with a set
of notebook files, you can use a `.dir-locals.el` file to specify
[directory local
variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html):


```
((python-mode
  (eval conda-env-activate "nei")
  (nei-write-format . "clean")
  (eval nei-mode)))
```

This is a handy way to customize NEI's behavior for an entire directory
of files without having to record this metadata in the files
themselves. You can also specify [file variables](https://www.gnu.org/software/emacs/manual/html_node/emacs/Specifying-File-Variables.html#Specifying-File-Variables)
if you don't mind emacs related code appearing in your notebook source.

## Emacs configuration

NEI does not yet have an emacs package but it only has two elisp
dependencies given a recent version of emacs:
[``s``](https://melpa.org/#/s) and
[``websocket``](https://melpa.org/#/websocket). These can be easily
installed using the ``package-list-packages`` command if you have
pointed to a suitable elisp package repository such as
[MELPA](https://melpa.org/).

You will also need to add NEI to your ``.emacs`` file by pointing to
the ``emacs`` subdirectory of this repository:

```elisp
(add-to-list 'load-path "~/nei/emacs")
(require 'nei)
```

Eventually a MELPA package will be offered for NEI.

## Scope of the project

A few notes on the current scope of the project. As the project progress, support for features currently out of scope will be considered.

* At this time, NEI only aims to support Python and IPython syntax.
* The primary focus is currently on ensuring robust emacs support but contributions to support other editors are welcome.
* One key objective is to support rich interactive visualization with [holoviews](http://holoviews.org/), which means supporting [bokeh](https://bokeh.pydata.org/en/latest/) plots. 
* Support for other complex Javascript components such as ipywidgets can be added if contributors volunteer to maintain and test those aspects of the project.
