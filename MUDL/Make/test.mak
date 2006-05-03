lrwhich ?= fbg

lractn ?= 3
lractd ?= 4
lractsuffix = +$(lractn)+$(lractd)

ifeq "$(lrwhich)" "act"
lrsuffix = $(lractsuffix)
endif
