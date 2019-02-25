vcov.BTm1 <- function(object, complete = TRUE, ...){
    # vcov of constrained parameters
    vcov <- NextMethod()
    factors <- attr(terms(object$formula), "factors")
    if (!(object$id %in% rownames(factors))) {
        return(vcov)
    } else {
        # setup factor reflecting contrasts used ..
        fac <- factor(object$xlevels[[object$id]], 
                      levels = object$xlevels[[object$id]],
                      labels = paste0(object$id, object$xlevels[[object$id]]))
        if (!is.null(object$refcat)) {
            fac <- C(relevel(fac, object$refcat),
                     "contr.treatment")
        } else fac <- C(fac, object$contrasts[[object$id]])
        contr <- contrasts(fac)
        # full vcov
        contr %*% vcov %*% t(contr)
    }
}