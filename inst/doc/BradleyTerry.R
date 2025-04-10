## ----include=FALSE------------------------------------------------------------
library <- function(...) suppressPackageStartupMessages(base::library(...))
library(knitr)
opts_chunk$set(
tidy=FALSE
)

## ----set_options, echo = FALSE--------------------------------------
options(prompt = "R> ", continue = "+  ", width = 70,
        useFancyQuotes = FALSE, digits = 7)

## ----LoadBradleyTerry2----------------------------------------------
library("BradleyTerry2")

## ----CitationData---------------------------------------------------
data("citations", package = "BradleyTerry2")

## ----CitationData2--------------------------------------------------
citations

## ----countsToBinomial-----------------------------------------------
citations.sf <- countsToBinomial(citations)
names(citations.sf)[1:2] <- c("journal1", "journal2")
citations.sf

## ----citeModel------------------------------------------------------
citeModel <- BTm(cbind(win1, win2), journal1, journal2, ~ journal,
    id = "journal", data = citations.sf)
citeModel

## ----citeModelupdate------------------------------------------------
update(citeModel, refcat = "JASA")

## ----citeModelupdate2-----------------------------------------------
update(citeModel, br = TRUE)

## ----lizModel-------------------------------------------------------
options(show.signif.stars = FALSE)
data("flatlizards", package = "BradleyTerry2")
lizModel <- BTm(1, winner, loser, ~ SVL[..] + (1|..),
                data = flatlizards)

## ----summarize_lizModel---------------------------------------------
summary(lizModel)

## ----lizModel2------------------------------------------------------
lizModel2 <- BTm(1, winner, loser,
            ~ throat.PC1[..] + throat.PC3[..] +
            head.length[..] + SVL[..] + (1|..),
            data = flatlizards)
summary(lizModel2)

## ----baseball-------------------------------------------------------
data("baseball", package = "BradleyTerry2")
head(baseball)

## ----baseballModel--------------------------------------------------
baseballModel1 <- BTm(cbind(home.wins, away.wins), home.team, away.team,
                      data = baseball, id = "team")
summary(baseballModel1)

## ----baseballDataUpdate---------------------------------------------
baseball$home.team <- data.frame(team = baseball$home.team, at.home = 1)
baseball$away.team <- data.frame(team = baseball$away.team, at.home = 0)

## ----baseballModelupdate--------------------------------------------
baseballModel2 <- update(baseballModel1, formula = ~ team + at.home)
summary(baseballModel2)

## ----CEMSmodel------------------------------------------------------
data("CEMS", package = "BradleyTerry2")
table8.model <-  BTm(outcome = cbind(win1.adj, win2.adj),
    player1 = school1, player2 = school2, formula = ~ .. +
    WOR[student] * LAT[..] +  DEG[student] * St.Gallen[..] +
    STUD[student] * Paris[..] + STUD[student] * St.Gallen[..] +
    ENG[student] * St.Gallen[..] + FRA[student] * London[..] +
    FRA[student] * Paris[..] + SPA[student] * Barcelona[..] +
    ITA[student] * London[..] + ITA[student] * Milano[..] +
    SEX[student] * Milano[..],
    refcat = "Stockholm", data = CEMS)

## ----BTabilities----------------------------------------------------
BTabilities(baseballModel2)

## ----BTabilities2---------------------------------------------------
head(BTabilities(lizModel2), 4)

## ----figqvplot, echo=FALSE , fig.cap="Estimated relative abilities of baseball teams.", fig.alt="The ability for Baltimore is fixed at zero, with an interval ranging from -0.5 to 0.5. Boston has a relative ability near 1.2; Cleveland around 0.7. The remaining teams have relative abilities around 1.3 to 1.6. The intervals are based on quasi standard errors and all have length of approximately 1. Therefore, aside from Cleveland, all teams are clearly significantly stronger than Baltimore as the intervals do not overlap.", fig.show='hold', fig.align="center", out.width="67.0%"----
knitr::include_graphics(c("baseball-qvplot.png"))

## ----residuals------------------------------------------------------
res.pearson <- round(residuals(lizModel2), 3)
head(cbind(flatlizards$contests, res.pearson), 4)

## ----BTresiduals----------------------------------------------------
res <- residuals(lizModel2, type = "grouped")
#  with(flatlizards$predictors, plot(throat.PC2, res))
#  with(flatlizards$predictors, plot(head.width, res))

## ----residualWLS----------------------------------------------------
lm(res ~ throat.PC1, weights = attr(res, "weights"),
   data = flatlizards$predictors)
lm(res ~ head.length, weights = attr(res, "weights"),
   data = flatlizards$predictors)

## ----figresiduals, echo=FALSE , fig.cap="Lizard residuals for the simple model lizModel, plotted against throat.PC3.", fig.alt="The residuals are quite spread out over the range -2 to 2, but the distribution is clearly not uniform over the range of the predictor variable, throat.PC3. Residuals between -2 and -1 range correspond to values throat.PC3 between -6 and 4; residuals between -1 and 1 correspond to throat.PC3 values of -4 to 4, and residuals from 1 to 2 correspond to throat.PC3 values between -3 and 6. Thus there is an overall positive correlation bewteen the residuals and throat.PC3.", fig.show='hold', fig.align="center", out.width="69.0%"----
knitr::include_graphics(c("residuals.png"))

## ----baseballModel2_call--------------------------------------------
 baseballModel2$call

## ----str_baseball---------------------------------------------------
str(baseball, vec.len = 2)

## ----first_comparison-----------------------------------------------
baseball$home.team[1,]
baseball$away.team[1,]

## ----first_outcome--------------------------------------------------
 baseball[1, c("home.wins", "away.wins")]

## ----str_CEMS-------------------------------------------------------
str(CEMS, vec.len = 2)

## ----student-specific_data------------------------------------------
library("prefmod")
student <- cemspc[c("ENG", "SEX")]
student$ENG <- factor(student$ENG, levels = 1:2,
                      labels = c("good", "poor"))
student$SEX <- factor(student$SEX, levels = 1:2,
                      labels = c("female", "male"))

## ----student_factor-------------------------------------------------
cems <- list(student = student)
student <- gl(303, 1, 303 * 15) #303 students, 15 comparisons
contest <- data.frame(student = student)

## ----binomial_response----------------------------------------------
win <- cemspc[, 1:15] == 0
lose <- cemspc[, 1:15] == 2
draw <- cemspc[, 1:15] == 1
contest$win.adj <- c(win + draw/2)
contest$lose.adj <- c(lose + draw/2)

## ----school_factors-------------------------------------------------
lab <- c("London", "Paris", "Milano", "St. Gallen", "Barcelona",
         "Stockholm")
contest$school1 <- factor(sequence(1:5), levels = 1:6, labels = lab)
contest$school2 <- factor(rep(2:6, 1:5), levels = 1:6, labels = lab)

## ----cems_data------------------------------------------------------
cems$contest <- contest

## ----functions, echo = FALSE------------------------------
## cf. prompt
options(width = 55)
for (fn in getNamespaceExports("BradleyTerry2")) {
    name <- as.name(fn)
    args <- formals(fn)
    n <- length(args)
    arg.names <- arg.n <- names(args)
    arg.n[arg.n == "..."] <- "\\dots"
    is.missing.arg <- function(arg) typeof(arg) == "symbol" &&
        deparse(arg) == ""
    Call <- paste(name, "(", sep = "")
        for (i in seq_len(n)) {
        Call <- paste(Call, arg.names[i], if (!is.missing.arg(args[[i]]))
            paste(" = ", paste(deparse(args[[i]]),
                collapse = "\n"), sep = ""), sep = "")
        if (i != n)
            Call <- paste(Call, ", ", sep = "")
    }
    Call <- paste(Call, ")", sep = "")
    cat(deparse(parse(text = Call)[[1]], width.cutoff = 50), fill = TRUE)
}
options(width = 60)

