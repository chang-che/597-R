# Notice than this is sequence not number, number will be much easier.
Fin = function(x){
    if (x == 1) return(c(1))
    if (x == 2) return(c(1,1))
    if (x >2) {
      
      return (c(Fin(x-1),Fin(x-1)[length(Fin(x-1))] + Fin(x-1)[length(Fin(x-1))-1])
)
    }
    else {stop()}
}
Fin(100)
