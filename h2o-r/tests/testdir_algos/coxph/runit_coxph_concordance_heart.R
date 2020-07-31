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


    heart.hex <- as.h2o(heart.df)
    heart.hex$surgery <- as.factor(heart.hex$surgery)
    heart.hex$transplant <- as.factor(heart.hex$transplant)

    # without start column

    hexModel <- h2o.coxph(x="age", event_column="event",
                          stop_column="stop", training_frame=heart.hex, ties="breslow")
    rModel <- survival::coxph(Surv(stop, event) ~ age, data = heart.df, ties="breslow")

    rPredictor <- rModel$linear.predictors
    hexPredictor <- pred(hexModel, heart.hex)
    
    expect_equal(rPredictor, hexPredictor, scale = 1, tolerance = 1e-3)

    rConcordance <- unname(summary(rModel)$concordance)[1]
    hexConcordance <- hexModel@model$concordance
    
    
    expect_equal(rConcordance, hexConcordance, tolerance = 1e-5)

    
    # with multiple columns

    hexModel <- h2o.coxph(x=c("age", "year", "surgery", "transplant" ), event_column="event",
                          stop_column="stop", training_frame=heart.hex, ties="breslow")
    rModel <- survival::coxph(Surv(stop, event) ~ age  + year + surgery + transplant, data = heart.df, ties="breslow")

    rPredictor <- rModel$linear.predictors
    hexPredictor <- pred(hexModel, heart.hex)
    
    expect_equal(rPredictor, hexPredictor, scale = 1, tolerance = 1e-3)

    rConcordance <- unname(summary(rModel)$concordance)[1]
    hexConcordance <- hexModel@model$concordance
    
    
    expect_equal(rConcordance, hexConcordance, tolerance = 1e-5)
    
    # with multiple columns and interactions

    hexModel <- h2o.coxph(x=c("age", "year", "surgery", "transplant" ), event_column="event",
                          stop_column="stop", training_frame=heart.hex, ties="breslow",
                          interaction_pairs=list(c("year", "surgery"), c("transplant", "surgery")))
    rModel <- survival::coxph(Surv(stop, event) ~ age  + year + surgery + transplant + year:surgery + transplant:surgery, 
                              data = heart.df, ties="breslow")

    rPredictor <- rModel$linear.predictors
    hexPredictor <- pred(hexModel, heart.hex)
    
    expect_equal(rPredictor, hexPredictor, scale = 1, tolerance = 1e-3)

    rConcordance <- unname(summary(rModel)$concordance)[1]
    hexConcordance <- hexModel@model$concordance
    
    
    expect_equal(rConcordance, hexConcordance, tolerance = 1e-5)
    
    # with multiple columns and interactions and stratification

    hexModel <- h2o.coxph(x=c("age", "year", "surgery", "transplant" ), event_column="event",
                          stop_column="stop", training_frame=heart.hex, ties="breslow",
                          interaction_pairs=list(c("transplant", "surgery"), c("year", "surgery") ),
                          stratify_by=c("surgery"))

    rModel <- survival::coxph(Surv(stop, event) ~ age + year  + transplant + year:surgery + transplant:surgery + strata(surgery), 
                              data = heart.df, ties="breslow")

    rPredictor <- rModel$linear.predictors
    hexPredictor <- pred(hexModel, heart.hex)
    
    expect_equal(rPredictor, hexPredictor, scale = 1, tolerance = 1e-3)

    rConcordance <- unname(summary(rModel)$concordance)[1]
    hexConcordance <- hexModel@model$concordance
    
    
    expect_equal(rConcordance, hexConcordance, tolerance = 1e-5)

    # with multiple columns and interactions and multiple stratification

    hexModel <- h2o.coxph(x=c("age", "year", "surgery", "transplant" ), event_column="event",
                          stop_column="stop", training_frame=heart.hex, ties="breslow",
                          interaction_pairs=list(c("year", "surgery") ),
                          stratify_by=c("surgery", "transplant"))

    rModel <- survival::coxph(Surv(stop, event) ~ age + year  + transplant + year:surgery + strata(surgery) + strata(transplant), 
                              data = heart.df, ties="breslow")

    rPredictor <- rModel$linear.predictors
    hexPredictor <- pred(hexModel, heart.hex)
    
    expect_equal(rPredictor, hexPredictor, scale = 1, tolerance = 1e-3)

    rConcordance <- unname(summary(rModel)$concordance)[1]
    hexConcordance <- hexModel@model$concordance
    
    
    expect_equal(rConcordance, hexConcordance, tolerance = 1e-5)

    # with start column

    hexModel <- h2o.coxph(x="age", event_column="event", start_column="start", 
                          stop_column="stop", training_frame=heart.hex)
    rModel <- survival::coxph(Surv(start, stop, event) ~ age, data = heart.df)

    rPredictor <- rModel$linear.predictors
    hexPredictor <- pred(hexModel, heart.hex)
    
    expect_equal(rPredictor, hexPredictor, scale = 1, tolerance = 1e-3)

    rConcordance <- unname(summary(rModel)$concordance)[1]
    hexConcordance <- hexModel@model$concordance
    
    
    expect_equal(rConcordance, hexConcordance)
}

doTest("CoxPH: Predict Test", test.CoxPH.predict)
