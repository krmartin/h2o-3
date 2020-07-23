setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../../scripts/h2o-r-test-setup.R")



test.CoxPH.predict <- function() {
    pred <- function(model, data) {
        hex.lp <- h2o.predict(model, data)
        lp <- as.data.frame(hex.lp)$lp
        lp[!is.na(lp)]
    }

    heart.hex <- h2o.importFile(locate("smalldata/coxph_test/heart.csv"))
    heart.df <- as.data.frame(heart.hex)

    hexModel <- h2o.coxph(x="age", event_column="event", start_column="start", stop_column="stop", training_frame=heart.hex)
    rModel <- survival::coxph(Surv(start, stop, event) ~ age, data = heart.df)

    rPredictor <- rModel$linear.predictors
    hexPredictor <- pred(hexModel, heart.hex)
    
    expect_equal(rPredictor, hexPredictor, scale = 1, tolerance = 1e-3)
    
    print(hexModel)

    rConcordance <- unname(summary(rModel)$concordance)[1]
    hexConcordance <- hexModel@model$concordance
    print(hexConcordance)
    print(rConcordance)
    
    print("------------------------")
    print("------------------------")
    print("------------------------")
    print("------------------------")
    print("------------------------")
    print("------------------------")
    print(summary(rModel))
    print("------------------------")
    print(summary(hexModel))
    print("------------------------")
    
    expect_equal(rConcordance, hexConcordance)
}

doTest("CoxPH: Predict Test", test.CoxPH.predict)
