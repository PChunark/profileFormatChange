#!/usr/bin/env Rscript

executeTime <-
  function(code){
    start = Sys.time()
    code
    print(Sys.time()-start)
  }
