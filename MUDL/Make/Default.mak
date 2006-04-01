##----------------------------------------------------------------------
## Default Variables

##-- induction corpus
#icorpus ?= utest.t
#icorpus ?= utrain.t
#icorpus ?= utrain.small.t
#icorpus ?= zeitarchiv.it
icorpus  ?= utrain-nl.t

##-- ntzeit: negra 'utrain.small.t' + 'zeitarchiv.t'
#icorpus ?= ntzeit.it

icbase ?= $(basename $(icorpus))

##-- test corpus
#tcorpus ?= $(icorpus:.t=.ttt)
#tcorpus ?= utest.small.ttt
#tcorpus ?= utest.tiny.ttt
tcorpus  ?= utest-nl.tiny.ttt

tbase   ?= $(basename $(tcorpus))


##-- supervised corpus
#scorpus ?= $(corpus:.t=.ttt)
scorpus ?= $(icorpus:.t=.ttt)


##------------------------------------------------------
## Profiling: L-/R- methods

#lrwhich ?= bg
#lrwhich ?= mi
#lrwhich ?= fbg
lrwhich  ?= hb

pbr_bds   ?= 200
bds_infix ?= $(pbr_bds)

## $(tgmethod): how to select targets:
##  freq   : select by raw frequency
##  select : select by co-occurrence with known bounds
#tgmethod ?= freq
tgmethod ?= select

ifeq "$(tgmethod)" "freq"

##-- raw frequency target selection
pbrmm_tgs ?= 0+$(pbr_bds)
pbrmm_tgs_min = $(shell echo '$(pbrmm_tgs)' | awk -F+ '{print $$1}')
pbrmm_tgs_max = $(shell echo '$(pbrmm_tgs)' | awk -F+ '{print $$2}')
tgs_infix ?= $(pbrmm_tgs)

else

##-- target selection via tgselect.perl
tgfmin ?= 1
ntgs   ?= $(pbr_bds)
tgs_infix ?= $(tgfmin)~$(ntgs)
tgs_ignore ?= -ignore '__$$'
tgs_addbounds  ?= -addbound '__$$'

endif

##-- filenames
#bounds   ?= bds-$(bds_infix).enum.bin
#targets  ?= tgs-$(tgs_infix).enum.bin
bounds    ?= bds-$(bds_infix).$(icorpus).bin
targets   ?= tgs-$(tgs_infix).$(icorpus).bin

lrinfix     ?= b$(bds_infix)-t$(tgs_infix)
lrdir       ?= .

##------------------------------------------------------
## Profiling: L-/R- trigrams (targets are trigrams)

##-- raw frequency trigram target selection
tgs3_fmin  ?= 1


## tg3method : trigram target-selection method
##
## tg3method  = freq
## tg3method  = select
## tg3method  = selectb
tg3method  ?= selectb

ifeq "$(tg3method)" "freq"

##-- select by raw frequency

## $(ntgs3) : number of frequency-selected 3gram targets
#ntgs3     ?= 2000
ntgs3      ?= 4000

## $(ntgs3r) : number of randomly selected 3gram targets
ntgs3r     ?= 0

pbr_tgs3   ?= $(ntgs3)
tgs3_infix ?= $(pbr_tgs3)-r$(ntgs3r)

else

ifeq "$(tg3method)" "select"

##-- select by bounds & raw frequency

ntgs3      ?= 0
ntgs3r     ?= 0
tgs3fmin   ?= 2
tgs3_infix ?= select-n$(ntgs3)-b$(pbr_bds)-f$(tgs3fmin)
tgs3add    ?= '__$$'

else

#ifeq "$(tg3method)" "selectb"

ntgs3      ?= 0
ntgs3r     ?= 0
tgs3fmin   ?= 2
tgs3add    ?= '__$$'

nbds3      ?= $(pbr_bds)
bds3_infix ?= b$(nbds3)

bounds3    ?= bds3-$(nbds3).$(icorpus).enum.bin
tgs3_infix ?= selectb-n$(ntgs3)-f$(tgs3fmin)-$(bds3_infix)

#endif
endif
endif

targets3     ?= tgs-3g-$(tgs3_infix).enum.bin
targets3elts ?= tgs-3g-$(tgs3_infix)-elts.enum.bin

lrinfix3     ?= 3g-b$(bds_infix)-t$(tgs3_infix)

lr3 ?= 0
ifneq "$(lr3)" "0"
  lrinfix = $(lrinfix3)
endif

##-- for attachment
## lr3bs : block size
lr3bs ?= 2048

## lr3a : number of trigrams to ensure are attached
lr3a  ?= 16384

lr3ctinfix ?= lr$(lrwhich)-$(lrinfix3).bs-$(lr3bs).$(tcinfix)

##------------------------------------------------------
## Profiling: L-/R-Bigrams (see above)

##------------------------------------------------------
## Profiling: L-/R-MI (see above)

##------------------------------------------------------
## Profiling: L-/R-Ngrams (see above)
lrngw ?= 2


##------------------------------------------------------
## Clustering

##-- $(tcclass): clustering subclass
## Tree   : MUDL::Cluster::Tree:   hierarchical agglomerative clustering
## KMeans : MUDL::Cluster::KMeans: k-means clustering
#ifeq "$(lr3)" "1"
#tcclass ?= Buckshot
#else
tcclass ?= Tree
#endif

##-- $(tcd): cluster distance metric
# H : min-safe Hoeffding distance
# h : Hoeffding distance
# D : KL-divergence
# A : symmetric KL-Divergence
# e : Euclidean
# b : Manhattan
# c : Pearson's correlation coefficient
# a : absolute value of the correlation
# u : uncentered correlation (cos of angle)
# x : absolute uncentered correlation
# s : Spearman's rank correlation
# k : Kendall's tau

ifeq "$(lrwhich)" "mi"
tcd           ?=b
else
tcd	      ?=s
endif


##-- $(tcm): clustering link method
# s : pairwise single-link (minimum)
# m : pairwise complete-link (maximum)
# a : pairwise average-link
# c : median-link
# f : centroid link ("fast & efficient")
tcm           ?=m

##-- $(tck): number of clusters
tck          ?=50
#tck          ?=100
#tck	      ?=200

##-- $(tcp): number of passes (for kmeans)
tcp	      ?=1


##-- clusterdistance() distance metric: see PDL::Cluster::clusterdistance()
##
#tccd ?= $(tcd)
tccd ?=s

##-- clusterdistance() link method
# a : d(c1,c2) := d(avg(c1),avg(c2))
# m : d(c1,c2) := d(med(c1),med(c2))
# s : d(c1,c2) := min(d(x1,x2)) , x1 \in c1, x2 \in c2
# x : d(c1,c2) := max(d(x1,x2)) , x1 \in c1, x2 \in c2
# v : d(c1,c2) := avg(d(x1,x2)) , x1 \in c1, x2 \in c2
#
##-- clusterDistanceMatrix(): tccm suffix flags
# +b : add hard-clustering bonus  [d_+(w,c)=(w \in c ? 0 : d(w,c))] in initial stage
#      -> in Cluster::Method::clusterDistanceMatrix()
#         \-> called from Cluster::Method::d2p_jaynes()
#             \-> called from Cluster::Method::membershipSimPdl()
#                 \-> called from Cluster::Method::membershipProbPdl()
#                     \-> called from MetaProfile::Attach::populatePhat()
# +bb : add hard-clustering bonus in all stages (in MetaProfile::Attach::populatePhat())
#-------------------------------------------------------
#tccm          ?=m
#tccm          ?=v
#tccm           ?=x
tccm           ?=x+bb


## x : better for lrhb, lrgb-3g for CLUSTERING
## --> 'v' or 's' may be better for EM


## $(tcniters): number of EM iterations for Buckshot etc.
##  --> BUGGY in Buckshot: empty clusters!
tcniters       ?=0

## $(svdr): number of dimensions to reduce to (0 for no reduction)
svdr           ?=0

## $(tcinfix): configuration identifier infix for clustering-dependent files
tcinfix ?= lr$(lrwhich)-$(lrinfix).$(tcd)$(tcm)-$(tccd)$(tccm).$(tcclass).k-$(tck).i-$(tcniters).svd-$(svdr)

##------------------------------------------------------
## Clustering / metaprofile-3

ifeq "$(tg3method)" "freq"
mp3targets ?= $(targets3)
mp3targets_infix ?= $(tgs3_infix)
else
ifeq "$(tg3method)" "select"
mp3targets_infix ?= select-n$(ntgs3)-f$(tgs3fmin)
mp3targets       ?= $(mpdir)/stage$(stage).mp3.$(mp3targets_infix).bin
else
#ifeq "$(tg3method)" "selectb"
mp3targets_infix ?= $(tgs3_infix)
mp3targets       ?= $(mpdir)/stage$(stage).mp3.$(mp3targets_infix).bin
#endif
endif
endif

mp3tcclass  ?= Tree
mp3tck ?= $(tck)
mp3tcd ?= $(tcd)
mp3tcm ?= $(tcm)
mp3tccd ?= $(tccd)
mp3tccm ?= $(tccm)
mp3tcp  ?= $(tcp)
mp3tcniters ?= $(tcniters)
mp3svdr     ?= $(svdr)

## $(mp3infix): configuration identifier infix for clustering-dependent files
#mp3infix ?= mp3.lr$(lrwhich).$(mp3tcd)$(mp3tcm)-$(mp3tccd)$(mp3tccm).$(mp3tcclass).k-$(mp3tck).i-$(mp3tcniters).svd-$(mp3svdr)

## $(mp3bsm) : bootstrap-method for trigram metaprofiles
##  hard  : use hard clustering results (fast)
##  dense : use phat, as in MetaProfile::Attach
#mp3bsm ?= hard
mp3bsm ?= dense

mp3dir ?= $(mpdir)/mp3.stage$(stage).tgs3-$(mp3targets_infix).$(mp3tcd)$(mp3tcm)-$(mp3tccd)$(mp3tccm).$(mp3tcclass).k-$(mp3tck).i-$(mp3tcniters).svd-$(mp3svdr).bs-$(lr3bs)

##------------------------------------------------------
## Multi-stage profiling

mpdir ?= mp$(mpc).$(mpt)-$(tcd)$(tcm)-$(tccd)$(tccm)-$(tcclass).k-$(tck).b0-$(bds_infix).t-$(tgmethod).ns-$(nstages).fmin-$(mpfmin).imax-$(mpimax).pn-$(mppn).pm-$(mppm).pb-$(mppb).pbeta-$(mppbeta).$(icbase).sr-$(srand).stages

stage ?=1
mpt   ?=lr$(lrwhich)

## $(mpc): MetaProfile subclass
## full   : full reclustering
## deep   : deep reclustering (bottom-up)
## attach : shallow attachment
##
#mpc ?=full
#mpc  ?=deep
mpc ?=attach

##-- old
#mpi   ?=200
#mpi   ?=400

nstages ?= 10
#nstages ?= 4
#nstages ?= 3

##-- for target-selection
## $(mpfmin) : minimum target frequency
## $(mpimax) : maximum number of new targets per stage
#mpfmin  ?= 2
#mpimax  ?= 8192
#--
mpfmin ?= 1
mpimax ?= 32768

#mppm  ?=nbest_gath
mppm  ?=nbest_jaynes
#mppm  ?=nbest_inverse
#mppm  ?=nbest_irank
#mppm  ?=nbest_icrank
#mppm  ?=nbest_pinkskerL1
#mppm  ?=nbest_base



##-- tck=100
#mppn   ?=32
#<
#mppn  ?=16
#<
#mppn  ?=8
#>
mppn  ?=4
#>
#mppn  ?=3
#>
#mppn  ?=2

mppb  ?=2

##-- beta
## NUMBER  : constant number for all stages
## freq    : actual individual unigram frequency at stage
## avg     : average unigram frequency for stage
## stage   : stage_index
## istage  : stage_index**-1
## bstage  : mppb * stage_index
## bistage : (mppb * stage_index)**-1m
mppbeta ?=istage



##------------------------------------------------------
## Multi-stage profiling: HMM generation & EM

## $(emam): arc-estimation mode for MetaProfile HMMs
#emam ?=estimate
#emaam ?=estimate1
emam ?=uniform

## $(embm): b-estimation mode for MetaProfile HMMs
#embm ?=sim
#embm ?=sim+mask
#embm ?=invert
#embm ?=invert+mask
#embm ?=invert+bonus-1
#embm ?=invert+ebbonus-1 
embm ?=invert+mask+ebonus-8

## $(hackb): obsolete parameter, now works out to something like: embmethod=invert+bonus-1
####hackb ?= 0


## $(emsu): em: unknown handling
#emum ?=none            ##-- no smoothing
#emum ?=uniform         ##-- use restricted smoothb='uniform'
#emum ?=types-$(total)  ##-- use number of types (hard cluster size), total unknown count $(total)
emum ?= 1
emum ?= types-$(emsmoothutotal)


## $(emrm): state restriction mode
#emrm ?= none                ##-- no restriction
#emrm ?= freq-z$(zero)+q$(q) ##-- restrict by frequency value
emrmfz ?= 0
emrmfq ?= $(mppn)
emrm   ?= freq-z$(emrmfz)+q$(emrmfq)


## $(embs0): observation-probability smoothing mode for HMM estimation ONLY
##   + should have no effect for HMMs with no zero-probabilties
#embs0 ?=none    ##-- no smoothing
#embs0 ?=joint   ##-- new, Laplace-like smoothing
#embs0 ?=uniform ##-- still there, now an alias for 'joint'
embs0 ?= uniform

## $(embs): observation-probability smoothing mode for HMM re-estimation ONLY
##   + should have no effect for HMMs with no zero-probabilties
#embs ?=none    ##-- no smoothing
#embs ?=all     ##-- gone
#embs ?=hapax   ##-- gone
#embs ?=uniform_c ##-- gone
#embs ?=joint   ##-- new, Laplace-like smoothing
#embs ?=uniform ##-- still there, now an alias for 'joint': CHANGED SEMANTICS: Wed, 20 Jul 2005 12:35:35 +0200
embs ?=uniform


## $(emas0): arc-probability (a,pi,omega) smoothing mode for HMM estimation ONLY
##   + should have no effect for HMMs with no zero-probabilties (i.e. truly re-estimated HMMs)
#emas0 ?= none    ##-- no smoothing
#emas0 ?= uniform ##-- alias for 'joint'
emas0 ?= none

## $(emas): arc-probability (a,pi,omega) smoothing mode for HMM re-estimation ONLY
##   + should have no effect for HMMs with no zero-probabilties (i.e. truly re-estimated HMMs)
#emas ?= none    ##-- no smoothing
#emas ?= uniform ##-- alias for 'joint'
emas ?= joint


## $(emmax): non-empty string indicating which HMM parameters to maximize
##   + recognized characters: [abpou]
##   + use something like 'X' for no maximization (but then, why are you running em?)
##   + for make-friendliness, it's probably best to use a canonical order
##     - proposed: a >> b >> p >> o >> u
#emmax     ?= X
emmax     ?= abpou

##-- em training corpus
emcorpus ?= $(icorpus)
#emcorpus ?= utest.small.t
#emcorpus ?= utrain.small.t
#emcorpus ?= zeitarchiv.t

## $(emgrow): flag for mudl-hmm.perl
##  + whether to add previously unknown tokens to HMM B() matrix on EM
##  + tricky: this is A Good Thing if we're running EM on the test corpus,
##    otherwise it's a pretty bad idea
#emgrow   ?=-grow   ##-- maximize unknowns as tokens in their own right (loses @UNKNOWN data: BUG)
#emgrow   ?=-nogrow ##-- leave unknowns unknown (maximize @UNKNOWN data)
ifeq "$(emcorpus)" "$(tbase).t"
emgrow ?=-grow
else
emgrow ?=-nogrow
endif

## $(embuffer): for mudl-hmm.perl: buffer EM corpus into a Pdl?
embuffer ?=-buffer

## $(emkeep): for mudl-hmm.perl: keep intermediate stages?
emkeep   ?=-keep

## $(emdir): EM directory
emdir    ?=$(mpdir)/em-stage$(stage).am-$(emam).bm-$(embm).um-$(emum).rm-$(emrm).as0-$(emas0).bs0-$(embs0).as-$(emas).bs-$(embs).$(emmax)$(emgrow)

#emkeepas ?=$(emdir)/em-%.3d.hmm.bin
emkeepas ?=$(emdir)/em-%.3d.$(emcorpus).hmm.bin


emi    ?= 0
emhmm0 ?= $(shell printf "$(emkeepas)" 0)
emhmm  ?= $(shell printf "$(emkeepas)" $(emi))

emfirstiter ?= 1
emniters    ?=$(emi)
emminpdiff  ?=-1


##------------------------------------------------------
## Evaluation (new)

## $(gtags): gold-standard tag file
gtags ?= $(tcorpus:.ttt=.tags)

## $(itags): induced tag file for $(tcorpus)
#itags =

## $(itext): induced text file for $(tcorpus)
itext ?= $(tcorpus:.ttt=.t)


##------------------------------------------------------
## Randomization / indexing
srand ?= 0
