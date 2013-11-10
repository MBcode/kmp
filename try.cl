;A breakdown of a few MachineLearning/Parallel&Distributed/SemanticWeb Libraries:
<<<<<<< HEAD
;ml/cl-bayesian@            pd/Eager-Future2@          sw/cl-mql@
;ml/cl-bayesnet@            pd/Starlisp-simulator@     sw/cl-ntriples-2012.12.16@
;ml/cl-decision-tree@       pd/Starsim-f20@            sw/cl-rdfxml_0.9@
;ml/cl-decisiontree@        pd/chanl@                  sw/cl-semantic@
;ml/cl-libsvm-0.0.7@        pd/cl-muproc@              sw/dbpedia-sparql@
;ml/cl-simple-neuralnet@    pd/cl-simd@                sw/dydra-cl@
;ml/cl-svm@                 pd/erlisp@                 sw/n3@
;ml/clml@                   pd/hevent@                 sw/ontolisp-0.9@
;ml/clml-svm@               pd/lfarm@                  sw/porky@
;ml/clrl@                   pd/lparallel@              sw/rdf-serialiser_0.3@
;ml/decisiontree@           pd/patron@                 sw/rdf-store@
;ml/icbr@                   pd/pcall@                  sw/rdf-utils@
;ml/malecoli@               pd/philip-jose@            sw/rdf-wilbur-redis@
;ml/mgl@                    pd/sb-concurrency@         sw/sbcl-4store@
;ml/mgl-0.0.6@              pd/starsim@                sw/scoli@
;ml/micmac@                 pd/stmx@                   sw/semantic-wikipedia@
;ml/ml-progs@               sw/SWCLOS@                 sw/sicl@
;ml/mulm@                   sw/SWCLOS2@                sw/twinql_0.3.1@
;ml/pulcinella@             sw/SWCLOSforILC2005@       sw/reasoner-3.2@
;pd/Common-Lisp-Actors@     sw/cl-4store@              sw/cl-ace@
;cl-graph graph-utils epigraph ;vivace-graph-v2           cl-nlp langutils /cl-registry  /tagger
;clpython  rcl rclg rclmath;looked@lsp/clj-hdf5/sci-db /but R-sci-db might be better  so rcl/etc
=======
;ml/cl-bayesian            pd/Eager-Future2          sw/cl-mql
;ml/cl-bayesnet            pd/Starlisp-simulator     sw/cl-ntriples-2012.12.16
;ml/cl-decision-tree       pd/Starsim-f20            sw/cl-rdfxml_0.9
;ml/cl-decisiontree        pd/chanl                  sw/cl-semantic
;ml/cl-libsvm-0.0.7        pd/cl-muproc              sw/dbpedia-sparql
;ml/cl-simple-neuralnet    pd/cl-simd                sw/dydra-cl
;ml/cl-svm                 pd/erlisp                 sw/n3
;ml/clml                   pd/hevent                 sw/ontolisp-0.9
;ml/clml-svm               pd/lfarm                  sw/porky
;ml/clrl                   pd/lparallel              sw/rdf-serialiser_0.3
;ml/decisiontree           pd/patron                 sw/rdf-store
;ml/icbr                   pd/pcall                  sw/rdf-utils
;ml/malecoli               pd/philip-jose            sw/rdf-wilbur-redis
;ml/mgl                    pd/sb-concurrency         sw/sbcl-4store
;ml/mgl-0.0.6              pd/starsim                sw/scoli
;ml/micmac                 pd/stmx                   sw/semantic-wikipedia
;ml/ml-progs               sw/SWCLOS                 sw/sicl
;ml/mulm                   sw/SWCLOS2                sw/twinql_0.3.1
;ml/pulcinella             sw/SWCLOSforILC2005       sw/reasoner-3.2
;pd/Common-Lisp-Actors     sw/cl-4store              sw/cl-ace
;cl-graph graph-utils epigraph ;vivace-graph-v2           cl-nlp langutils /cl-registry  /tagger
;clpython  rcl rclg rclmath;lookedlsp/clj-hdf5/sci-db /but R-sci-db might be better  so rcl/etc
;>>>>>> 1c57083a0cb68ad37f8daefd84df5e315c7da9f6
;pd(parallel/distributed)stuff might be able to be done via the R/Py pkgs
;   r.cl py.cl (jl.cl, but ijulia, or just mq);ql: zeromq|zmq|pzmq
;lots of math/stat,  &data-table cl-linq
;
;<<<<<< HEAD
;m/Maxima-CAS@        m/gnoem@             m/mma@               m/rclmath@
;m/cl-mathstats@      m/gsll@              m/mulm@              m/simlab@
;m/cl-octave_0.1@     m/lassie@            m/nlisp_0.8.2@       m/soundlab@
;m/cl-rsm-fuzzy@      m/linear-algebra@    m/numerical-lisp@    m/units@
;m/cl-sparsematrix@   m/lisp-matrix@       m/nurarihyon@        m/vmath@
;m/clem@              m/lisphys@           m/pythononlisp@      m/wiz-util@
;m/common-lisp-stat@  m/maxima@            m/r@                 m/data-table
;m/embeddable-maxima@ m/micmac@            m/rcl@
;m/fsvd@              m/minpack@           m/rclg@
;NLP
;nl/EPILOG@                    nl/cl-earley-parser_0.9.2@    nl/langutils@
;nl/basic-english-grammar-1.0@ nl/cl-inflector@              nl/lassie@
;nl/cl-ace@                    nl/cl-nlp@
;lots of other reasoning/aid beyond KM; have considered ML via workflow or even blackboar;;gbbopen
(ql 'gbbopen)
(ql 'caveman) ;(or wuwei toot), to get a nicer front end on notional.no-ip.org
=======
;m/Maxima-CAS        m/gnoem             m/mma               m/rclmath
;m/cl-mathstats      m/gsll              m/mulm              m/simlab
;m/cl-octave_0.1     m/lassie            m/nlisp_0.8.2       m/soundlab
;m/cl-rsm-fuzzy      m/linear-algebra    m/numerical-lisp    m/units
;m/cl-sparsematrix   m/lisp-matrix       m/nurarihyon        m/vmath
;m/clem              m/lisphys           m/pythononlisp      m/wiz-util
;m/common-lisp-stat  m/maxima            m/r                 m/data-table
;m/embeddable-maxima m/micmac            m/rcl
;m/fsvd              m/minpack           m/rclg
;NLP
;nl/EPILOG                    nl/cl-earley-parser_0.9.2    nl/langutils
;nl/basic-english-grammar-1.0 nl/cl-inflector              nl/lassie
;nl/cl-ace                    nl/cl-nlp
;lots of other reasoning/aid beyond KM; have considered ML via workflow or even blackboar;;gbbopen
(ql 'gbbopen)
(ql 'caveman) ;(or wuwei toot), to get a nicer front end on notional.no-ip.org
;trying connections to octave/R/etc, &general slime/swank as well
;>>>>>> 1c57083a0cb68ad37f8daefd84df5e315c7da9f6
;dual-boot updates caused this problem, just from a pull&push, which was unexpected
;also I got another instances of an update not showing up in the calendar(graph)log
