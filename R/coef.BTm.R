coef.BTm1 <- function(object, complete = TRUE, ...){
    factors <- attr(terms(object$formula), "factors")
    if (!(object$id %in% rownames(factors))) {
        return(NextMethod())
    } else {
        # add in contrained estimate
        coef <- na.exclude(object$coefficients)
        # setup factor reflecting contrasts used ..
        fac <- factor(object$xlevels[[object$id]], 
                      levels = object$xlevels[[object$id]],
                      labels = paste0(object$id, object$xlevels[[object$id]]))
        if (!is.null(object$refcat)) {
            fac <- C(relevel(fac, object$refcat),
                     "contr.treatment")
        } else fac <- C(fac, object$contrasts[[object$id]])
        contr <- contrasts(fac)
        ## calc abilities and s.e., fill in NA as necessary
        if (!is.null(attr(coef, "na.action"))) {
            contr <- contr[, -attr(coef, "na.action"), drop = FALSE]
        }
        drop(contr %*% coef)
    }
}