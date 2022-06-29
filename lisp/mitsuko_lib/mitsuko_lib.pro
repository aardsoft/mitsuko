CONFIG += ecl

isEmpty(ECL_PRF){
  message("ecl prf empty")
}
TARGET = mitsuko_lib
ECL_SOURCES = mitsuko-core.lisp
ECL_LOAD = EQL5-symbols.lisp
ECL_EVAL = '(require :asdf)'
ECL_INIT_NAME = init_lib_MITSUKO__ALL_SYSTEMS