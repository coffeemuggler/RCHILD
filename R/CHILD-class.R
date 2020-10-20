CHILD <- setClass("CHILD",
  representation(
    summary      = "character",
    inputs       = "character",
    timesteps    = "numeric",
    pts          = "matrix",
    upliftmap    = "list",
    layers       = "list",
    nodes        = "list",
    edges        = "list",
    tri          = "list",
    random       = "numeric",
    varea        = "list",
    slp          = "list",
    area         = "list",
    vols         = "numeric",
    dvols        = "numeric",
    q            = "list",
    net          = "list",
    storm        = "matrix",
    tau          = "list",
    tx           = "list",
    z            = "list"
  )
)

setMethod("show", signature(object = "CHILD"), function(object){
  
  ##print
  cat("\n CHILD data object     ")
  cat("\n\t", object@summary[1])
  cat("\n\t", object@summary[2])
  cat("\n\t", object@summary[3])
  cat("\n\t", object@summary[4])
  cat("\n\t", object@summary[5])
})