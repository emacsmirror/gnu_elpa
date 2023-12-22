

### Version 0.3


# About

Major mode for editing [Devicetree](http://www.devicetree.org/) files.

Features:

-   Font Lock
-   Indent
-   IMenu


# Installation


## With guix

    guix package -f guix.scm


## From source

Clone this repo.

    (use-package graphql-ts-mode
      :ensure nil
      :load-path "PATH TO WHICH THE REPOSITORY WAS CLONED"
      :init
      (with-eval-after-load 'treesit
        (add-to-list 'treesit-language-source-alist
                     '(devicetree "https://github.com/joelspadin/tree-sitter-devicetree"))))

This requires a working C compiler.


# License

GPLv3

