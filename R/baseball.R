#' Baseball Data from Agresti (2002)
#' 
#' Baseball results for games in the 1987 season between 7 teams in the Eastern
#' Division of the American League.
#' 
#' 
#' @name baseball
#' @docType data
#' @format A data frame with 42 observations on the following 4 variables.
#' \describe{ 
#' \item{home.team}{a factor with levels `Baltimore`,
#' `Boston`, `Cleveland`, `Detroit`, `Milwaukee`, `New York`, `Toronto`.} 
#' \item{away.team}{a factor with levels
#' `Baltimore`, `Boston`, `Cleveland`, `Detroit`,
#' `Milwaukee`, `New York`, `Toronto`.}
#' \item{home.wins}{a numeric vector.} 
#' \item{away.wins}{a numeric vector.} }
#' @note This dataset is in a simpler format than the one described in Firth
#' (2005).
#' @seealso [BTm()]
#' @references Firth, D. (2005) Bradley-Terry models in R.  *Journal of
#' Statistical Software*, **12**(1), 1--12.
#' 
#' Turner, H. and Firth, D. (2012) Bradley-Terry models in R: The BradleyTerry2
#' package.  *Journal of Statistical Software*, **48**(9), 1--21.
#' @source Page 438 of Agresti, A. (2002) *Categorical Data Analysis* (2nd
#' Edn.). New York: Wiley.
#' @keywords datasets
#' @examples
#' 
#' ##  This reproduces the analysis in Sec 10.6 of Agresti (2002).
#' data(baseball) # start with baseball data as provided by package
#' 
#' ##  Simple Bradley-Terry model, ignoring home advantage:
#' baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
#'                       data = baseball, id = "team")
#' 
#' ##  Now incorporate the "home advantage" effect
#' baseball$home.team <- data.frame(team = baseball$home.team, at.home = 1)
#' baseball$away.team <- data.frame(team = baseball$away.team, at.home = 0)
#' baseballModel2 <- update(baseballModel1, formula = ~ team + at.home)
#' 
#' ##  Compare the fit of these two models:
#' anova(baseballModel1, baseballModel2)
#' 
#' 
"baseball"
