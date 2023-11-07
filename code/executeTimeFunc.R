executeTime <-
  function(code){
    start = Sys.time()
    code
    print(Sys.time()-start)
  }
