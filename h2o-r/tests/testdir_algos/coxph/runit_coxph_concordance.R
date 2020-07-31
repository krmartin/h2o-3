setwd(normalizePath(dirname(R.utils::commandArgs(asValues=TRUE)$"f")))
source("../../../scripts/h2o-r-test-setup.R")



test.CoxPH.predict <- function() {
    pred <- function(model, data) {
        hex.lp <- h2o.predict(model, data)
        lp <- as.data.frame(hex.lp)$lp
        lp[!is.na(lp)]
    }
    
    tstdata <- cancer
    tstdata$sex <- as.factor(tstdata$sex)
    tstdataHex <- as.h2o(tstdata)
    tstdataHex$status <- tstdataHex$status == 2
    
    print(summary(tstdata))
 

    print(head(tstdata))
    
    rModel <- coxph(Surv(time, status) ~ age + sex + meal.cal + age:meal.cal, data = tstdata, ties = "efron")
    rPredictor <- rModel$linear.predictors
    hexModel <- h2o.coxph(x = c("age", "sex", "meal.cal"), interaction_pairs = list(c("age", "meal.cal")),
                          event_column = "status", stop_column = "time", ties = "efron", training_frame = tstdataHex)
    hexPredictor <- pred(hexModel, tstdataHex)
    
    expect_equal(rPredictor, hexPredictor, scale = 1, tolerance = 1e-3)
    
    print(hexModel)

    rConcordance <- unname(summary(rModel)$concordance)[1]
    hexConcordance <- hexModel@model$concordance
    print(hexConcordance)
    print(rConcordance)
    
    print("------------------------")
    print(summary(rModel))
    
    expect_equal(rConcordance, hexConcordance)
}

doTest("CoxPH: Predict Test", test.CoxPH.predict)
