getformula <- function(object, ...){

  if(class(object)!= "cERGM"){
    print("Not an cERGM object")
  }else{
    print(object$formula)
  }
}
