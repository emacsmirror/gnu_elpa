# Denote Silo extras

The `denote-silo` package makes it easier to work with multiple
"silos", as explained in the Denote manual. In short, a "silo" is a
localised `denote-directory` that is not connected to the
default/global `denote-directory` and other silos.

The code of `denote-silo` used to be bundled up with the `denote`
package before version `4.0.0` of the latter and was available in the
file `denote-silo-extras.el`. Users of the old code will need to adapt
their setup to use the `denote-silo` package. This can be done by
replacing all instances of `denote-silo-extras` with `denote-silo`
across their configuration.

+ Package name (GNU ELPA): `denote-silo`
+ Official manual: <https://protesilaos.com/emacs/denote-silo>
+ Git repository: <https://github.com/protesilaos/denote-silo>
+ Backronym: Denote... Silos Insulate Localised Objects.
