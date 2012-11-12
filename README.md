xv.glm
======

Cross-validation software for generalized linear models (R package).

The main and only function estimates *k*-fold cross-validation prediction error for GLMs. It builds upon [`boot::cv.glm`](http://cran.r-project.org/web/packages/boot/), providing the following new features: models of class other than [`"glm"`](http://stat.ethz.ch/R-manual/R-patched/library/stats/html/glm.html) can be evaluated, various cost functions can be efficiently used to estimate the cross-validation prediction error on the same *k*-fold groups, all *k*-fold scores are reported (and not just the weighted mean), and *k*-fold groups can be sampled pro rata with the relative frequencies of response levels.

## License

*[Do What The Fuck You Want To Public License](http://en.wikipedia.org/wiki/WTFPL)* (version 2).

## Citation

When using *xv.glm* for a publication, please cite my [doctoral dissertation](http://tel.archives-ouvertes.fr/tel-00746163).

    @phdthesis{
        author = {Boruta, Luc},
        title = {Indicators of Allophony and Phonemehood},
        school = {Universit{\'e} Paris Diderot},
        year = {2012},
    }