################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE FOR divoRce PACKAGE                   ##
##                                                                            ##
################################################################################

# =============================================================================
# SETUP AND CONFIGURATION
# =============================================================================

library(divoRce)  # Adjust package name as needed
library(rcdd)
library(ROI)



# Record start time
start_time <- Sys.time()

# Initialize test results storage
test_results <- list(
  passed = 0,
  failed = 0,
  skipped = 0,
  log = list()
)


## Set rational flag
rational <- FALSE

## Backend/solver combinations to test
backend_solver_combos <- list(
  list(backend = "rcdd", solver = NULL),
  list(backend = "rcdd", solver = "CrissCross"),
  list(backend = "ROI", solver = NULL),
  list(backend = "ROI", solver = "lpsolve"),
  list(backend = "ROI", solver = "highs"),
  list(backend = "ROI", solver = "glpk")
)

## Test runner with all backend/solver combinations (with tracking)
run_test <- function(test_name, test_fn) {
  cat(" ", test_name, "
", sep = "")
  
  for (combo in backend_solver_combos) {
    backend <- combo$backend
    solver <- combo$solver
    solver_str <- if (is.null(solver)) "default" else solver
    
    tryCatch({
      result <- test_fn(backend, solver)
      cat(sprintf("    [%s/%s] ✓ PASSED", backend, solver_str))
      cat("
", "Result:", "
")
      print(result)
      
      # Track passed test
      test_results$passed <<- test_results$passed + 1
      test_results$log[[length(test_results$log) + 1]] <<- list(
        test_name = test_name,
        backend = backend,
        solver = solver_str,
        status = "PASS"
      )
    },
    error = function(e) {
      cat(sprintf("    [%s/%s] ✗ FAILED: %s
", backend, solver_str, conditionMessage(e)))
      
      # Track failed test
      test_results$failed <<- test_results$failed + 1
      test_results$log[[length(test_results$log) + 1]] <<- list(
        test_name = test_name,
        backend = backend,
        solver = solver_str,
        status = "FAIL",
        error = conditionMessage(e)
      )
    },
    warning = function(w) {
      cat(sprintf("    [%s/%s] ⚠ WARNING: %s
", backend, solver_str, conditionMessage(w)))
    })
  }
}

## Simple test runner (no backend/solver) with tracking
run_simple_test <- function(test_name, test_fn) {
  cat("", test_name, "
", sep = "")
  tryCatch({
    result <- test_fn()
    cat("    ✓ PASSED
")
    cat("Result:
")
    print(result)
    
    # Track passed test
    test_results$passed <<- test_results$passed + 1
    test_results$log[[length(test_results$log) + 1]] <<- list(
      test_name = test_name,
      status = "PASS"
    )
  }, error = function(e) {
    cat(sprintf("    ✗ FAILED: %s
", conditionMessage(e)))
    
    # Track failed test
    test_results$failed <<- test_results$failed + 1
    test_results$log[[length(test_results$log) + 1]] <<- list(
      test_name = test_name,
      status = "FAIL",
      error = conditionMessage(e)
    )
  })
}


## Section header printer
print_section <- function(title, level = 1) {
  if (level == 1) {
    cat("
", paste(rep("#", 78), collapse = ""), "
")
    cat("##  ", toupper(title), "
")
    cat(paste(rep("#", 78), collapse = ""), "
")
  } else if (level == 2) {
    cat("
", paste(rep("=", 70), collapse = ""), "
")
    cat("  ", title, "
")
    cat(paste(rep("=", 70), collapse = ""), "
")
  } else {
    cat("
", paste(rep("-", 60), collapse = ""), "
")
    cat("    ", title, "
")
    cat(paste(rep("-", 60), collapse = ""), "
")
  }
}

# =============================================================================
# LOAD TEST DATA
# =============================================================================

print_section("Loading Test Data")

## Binary Data
data(endometrial, package="detectseparation")
endo_glm <- glm(HG ~ NV + PI + EH, family = binomial(), data = endometrial)
y_b_endo <- endometrial$HG
X_b_endo <- model.matrix(endo_glm)
cat("✓ Binary: endometrial (quasi-complete separation)")

data(nsduh2019)
nsduh_glm <- glm(her_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex, 
                 family = binomial(), data = nsduh2019)
y_b_qcs <- nsduh2019$her_lifetime
X_b_qcs <- model.matrix(nsduh_glm)
cat("✓ Binary: nsduh2019 (quasi-complete separation)")

data(Silvapulle)
silv_glm <- glm(case ~ sex + ghq + sex:ghq, family = binomial(), data = Silvapulle)
y_b_silv <- Silvapulle$case
X_b_silv <- model.matrix(silv_glm)
cat("✓ Binary: Silvapulle (quasi-complete separation)")

data(titanic3)
tita_glm <- glm(Survived ~ Pclass + Sex, family = binomial(), data = titanic3)
y_b_tita <- y_b_cs <- tita_glm$y
X_b_tita <- X_b_cs <- model.matrix(tita_glm)
cat("✓ Binary: titanic3 (complete separation)")

data(ovldat1)
ovl_glm <- glm(y ~ x1 + x2, family = binomial(), data = ovldat1)
y_b_ol <- ovldat1$y
X_b_ol <- model.matrix(ovl_glm)
cat("✓ Binary: ovldat (overlap)")

## BCL / Multinomial Data
data(csepdatm)
csep_bcl <- nnet::multinom(y ~ x1 + x2, data = csepdatm, model = TRUE, trace = FALSE)
y_bcl_cs <- model.response(csep_bcl$model)
X_bcl_cs <- model.matrix(csep_bcl)
cat("✓ BCL: csepdatm (complete separation)")

data(qcsepdatm)
qcsep_bcl <- nnet::multinom(y ~ x1 + x2, data = qcsepdatm, trace = FALSE)
y_bcl_qcs <- qcsepdatm$y
X_bcl_qcs <- model.matrix(qcsep_bcl)
cat("✓ BCL: qcsepdatm (quasi-complete separation)")

data(ovldatm)
ovl_bcl <- nnet::multinom(y ~ x1 + x2, data = ovldatm, model = TRUE, trace = FALSE)
y_bcl_ol <- ovl_bcl$model$y
X_bcl_ol <- model.matrix(ovl_bcl)
cat("✓ BCL: ovldatm (overlap)")

data(Alligators)
allgm1 <- nnet::multinom(foodchoice ~ size + lake + sex, data = Alligators, trace = FALSE)
y_bcl_allig <- Alligators$foodchoice
X_bcl_allig <- model.matrix(allgm1)
cat("✓ BCL: Alligators (no separation)")

allgm2 <- nnet::multinom(foodchoice ~ size + lake * sex, data = Alligators, trace = FALSE)
y_bcl_allig2 <- Alligators$foodchoice
X_bcl_allig2 <- model.matrix(allgm2)
cat("✓ BCL: Alligators with interaction (quasi-complete separation)")



allgm3 <- brglm2::brmultinom(y ~ x1 + x2, data = qcsepdatm, trace = FALSE)
#allgm3 <- brglm2::brmultinom(foodchoice ~ size + lake * sex, data = Alligators)

## CL / Ordinal Data
data(HDSS)
hdss_clm <- ordinal::clm(WTSSHI ~ trustSHI * knowledge, data = HDSS)
hdss_polr <- MASS::polr(WTSSHI ~ trustSHI * knowledge, data = HDSS)
y_cl_hdss <- HDSS$WTSSHI
X_cl_hdss <- model.matrix(hdss_clm)$X
cat("✓ CL: HDSS")

data(wine, package = "ordinal")
wine_clm <- ordinal::clm(rating ~ temp + contact, data = wine)
y_cl_wine <- wine$rating
X_cl_wine <- model.matrix(wine_clm)$X
cat("✓ CL: wine")

wine_clm2 <- ordinal::clm(rating ~ temp + contact + bottle, data = wine)
y_cl_wine2 <- wine$rating
X_cl_wine2 <- model.matrix(wine_clm2)$X
cat("✓ CL: wine with bottle (singularities)")

## ACL / Ordinal Data
data(csepdato)
y_acl_cs <- as.ordered(csepdato$y)
X_acl_cs <- as.matrix(csepdato[, 2:ncol(csepdato)])
cat("✓ ACL: csepdato (complete separation)")

data(qcsepdato)
y_acl_qcs <- as.ordered(qcsepdato$y)
X_acl_qcs <- as.matrix(qcsepdato[, 2:ncol(qcsepdato)])
cat("✓ ACL: qcsepdato (quasi-complete separation)")

data(ovldato)
y_acl_ol <- as.ordered(ovldato$y)
X_acl_ol <- as.matrix(ovldato[, 2:ncol(ovldato)])
cat("✓ ACL: ovldato (overlap)")

data(HDSS)
hdss_npacl <- brglm2::bracl(WTSSHI ~ trustSHI * knowledge, data = HDSS, parallel = FALSE)
hdss_pacl <-brglm2::bracl(WTSSHI ~ trustSHI * knowledge, data = HDSS, parallel = TRUE)

## OS Data
y_os_qcs <- HDSS$WTSSHI
X_os_qcs <- model.matrix(~ trustSHI * knowledge, data = HDSS)

y_os_wine <- as.ordered(wine$rating)
X_os_wine <- model.matrix(~ temp * contact, data = wine)
cat("✓ OS: wine")

wine_os <- clustord::osm(rating~ temp * contact, data = wine)

y_os_ol <- as.ordered(ovldatm$y)
X_os_ol <- as.matrix(ovldatm[, 2:ncol(ovldatm)])
cat("✓ OS: ovldatm (overlap)")

## SL Data
y_sl_cs <- y_acl_cs
X_sl_cs <- X_acl_cs
y_sl_qcs <- y_acl_qcs
X_sl_qcs <- X_acl_qcs
y_sl_ol <- y_acl_ol
X_sl_ol <- X_acl_ol
cat("✓ SL: using ACL datasets")

## S matrix versions

S_cs <- as.matrix(X_b_cs)
S_cs[y_b_cs == 0, ] <- -1 * S_cs[y_b_cs == 0, ]

S_qcs <- as.matrix(X_b_qcs)
S_qcs[y_b_qcs == "No", ] <- -1 * S_qcs[y_b_qcs == "No", ]

S_ol <- as.matrix(X_b_ol)
S_ol[y_b_ol == 0, ] <- -1 * S_ol[y_b_ol == 0, ]

cat("✓ All test data loaded successfully")


################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE - PART 1: BINARY MODEL TESTS                    ##
##                                                                            ##
################################################################################

print_section("Binary Model Tests (b)")

## =============================================================================
## 1.1 checksep - Binary
## =============================================================================

print_section("checksep (Binary)", 2)

# --- Mid level: checksep with model="b" ---
print_section("checksep with model='b' (mid level)", 3)

run_test("checksep(model='b') - complete separation", function(backend, solver) {
  checksep_worker(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("checksep(model='b') - quasi-complete separation", function(backend, solver) {
  checksep_worker(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("checksep(model='b') - overlap", function(backend, solver) {
  checksep_worker(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})

# --- Mid level: with S matrix ---
print_section("checksep with S matrix (mid level)", 3)

run_test("checksep(S=) - complete separation", function(backend, solver) {
  checksep_worker(S = S_cs, rational = rational, backend = backend, solver = solver)
})

# --- Generic: check_separation.glm ---
print_section("check_separation (generic)", 3)

run_test("check_separation (model='b') not quick - quasi-complete separation", function(backend, solver) {
  check_separation(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("check_separation (model='b') quick - quasi-complete separation", function(backend, solver) {
  check_separation(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver, quick = TRUE)
})

run_test("check_separation.glm - complete separation (endometrial)", function(backend, solver) {
  check_separation(endo_glm, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.glm - quasi-complete separation (nsduh)", function(backend, solver) {
  check_separation(nsduh_glm, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.glm - quasi-complete separation (nsduh)", function(backend, solver) {
  check_separation(nsduh_glm, rational = rational, backend = backend, solver = solver, quick = TRUE)
})

run_test("check_separation.glm - quasi-complete separation (Silvapulle)", function(backend, solver) {
  check_separation(silv_glm, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.glm - complete separation (titanic)", function(backend, solver) {
  check_separation(tita_glm, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.glm - overlap", function(backend, solver) {
  check_separation(ovl_glm, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.formula ", function(backend, solver) {
  check_separation(y ~ x1 + x2, data = ovldat1, model="b", rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.matrix - complete separation", function(backend, solver) {
  check_separation(S = S_cs, rational = rational, backend = backend, solver = solver)
})


run_test("check_separation.glm quick - overlap", function(backend, solver) {
  check_separation(ovl_glm, rational = rational, backend = backend, solver = solver, quick = TRUE)
})

run_test("check_separation.formula quick", function(backend, solver) {
  check_separation(y ~ x1 + x2, data = ovldat1,  model="b", rational = rational, backend = backend, solver = solver, quick = TRUE)
})

run_test("check_separation.matrix quick - complete separation", function(backend, solver) {
  check_separation(S = S_cs, rational = rational, backend = backend, solver = solver, quick = TRUE)
})

run_test("check_separation.formula not quick - long - quasi-complete separation", function(backend, solver) {
  check_separation(her_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex, data = nsduh2019, model="b", rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.formula quick - long - quasi-complete separation", function(backend, solver) {
  check_separation(her_lifetime ~ alc_agefirst + demog_age_cat6 + demog_sex, data = nsduh2019, model="b", rational = rational, backend = backend, solver = solver, quick = TRUE)
})

## check default method
t <- rep(1:19)
class(t) <- "nudlaug"

run_simple_test("check_separation.default", function(backend, solver) {
  check_separation(t, rational = rational, model = "b", backend = backend, solver = solver)
})


###############
## check_overlap
#################


print_section("check_overlap (Binary)", 2)

# --- Mid level: check_overlap with model="b" ---
print_section("check_overlap with model='b' (mid level)", 3)

run_test("check_overlap(model='b') - complete separation", function(backend, solver) {
  check_overlap(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("check_overlap(model='b') - quasi-complete separation", function(backend, solver) {
  check_overlap(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("check_overlap(model='b') - overlap", function(backend, solver) {
  check_overlap(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("check_overlap(S=) - complete separation", function(backend, solver) {
  check_overlap(S = S_cs, rational = rational, backend=backend, solver=solver)
})


## =============================================================================
## 1.3 diagsep - Binary
## =============================================================================

print_section("diagsep (Binary)", 2)

# --- Mid level: diagsep with model="b" ---
print_section("diagsep_worker with model='b' (mid level)", 3)

run_test("diagsep_worker(model='b') - complete separation", function(backend, solver) {
  diagsep_worker(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='b') - quasi-complete separation", function(backend, solver) {
  diagsep_worker(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='b') - overlap", function(backend, solver) {
  diagsep_worker(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})


run_test("diagsep_worker(S=) - complete separation", function(backend, solver) {
  diagsep_worker(S = S_cs, rational = rational, backend = backend, solver = solver)
})



# --- Generic: diagnose_separation.glm ---
print_section("diagnose_separation.glm (generic)", 3)

run_test("diagnose_separation.glm - complete separation", function(backend, solver) {
  diagnose_separation(endo_glm, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.glm - quasi-complete separation", function(backend, solver) {
  diagnose_separation(nsduh_glm, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.glm - overlap", function(backend, solver) {
  diagnose_separation(ovl_glm, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.formula ", function(backend, solver) {
  diagnose_separation(y ~ x1 + x2, data = ovldat1, model="b", rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.matrix - complete separation", function(backend, solver) {
  diagnose_separation(S = S_cs, rational = rational, backend = backend, solver = solver)
})

## check default method
t <- rep(1:19)
class(t) <- "nudlaug"

run_simple_test("diagnose_separation.default", function(backend, solver) {
  diagnose_separation(t, rational = rational, model = "b", backend = backend, solver = solver)
})



# --- print.sepmod ---
print_section("print.sepmod (Binary)", 3)

run_simple_test("print.sepmod - default", function() {
  sd1 <- diagsep_worker (y_b_qcs, X_b_qcs, rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full", function() {
  sd1 <- diagsep_worker(y_b_qcs, X_b_qcs, rational = rational)
  print(sd1, info = "full")
})

## =============================================================================
## 1.4 sepcols / detect_sepcols - Binary
## =============================================================================

print_section("sepcols_worker (Binary)", 2)

# --- Mid level: detect_sepcols with model="b" ---
print_section("sepcols_worker with model='b' (mid level)", 3)

run_test("sepcols_worker(model='b') - complete separation", function(backend, solver) {
  sepcols_worker(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
}) ##Whats going on here? ROI issue.

run_test("sepcols_worker(model='b') - quasi-complete separation", function(backend, solver) {
  sepcols_worker(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("sepcols_worker(model='b') - overlap", function(backend, solver) {
  sepcols_worker(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})


run_test("sepcols_worker(S) - complete separation", function(backend, solver) {
  sepcols_worker(S = S_cs, rational = rational, backend = backend, solver = solver)
})


# --- Generic: separation_columns.glm ---
print_section("separation_columns.glm (generic)", 3)

run_test("separation_columns.glm - complete separation", function(backend, solver) {
  separation_columns(endo_glm, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.glm - quasi-complete separation", function(backend, solver) {
  separation_columns(nsduh_glm, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.glm - overlap", function(backend, solver) {
  separation_columns(ovl_glm, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.formula ", function(backend, solver) {
  separation_columns(y ~ x1 + x2, data = ovldat1, model="b", rational = rational, backend = backend, solver = solver)
})


run_test("separation_columns.matrix - complete separation", function(backend, solver) {
  separation_columns(S = S_cs, rational = rational, backend = backend, solver = solver)
})

## check default method
t <- rep(1:19)
class(t) <- "nudlaug"

run_test("separation_columns.default", function(backend, solver) {
  separation_columns(t, rational = rational, model = "b", backend = backend, solver = solver)
})



## =============================================================================
## 1.5 seprows - Binary
## =============================================================================

print_section("seprows (Binary)", 2)

# --- Mid level: seprows with model="b" ---
print_section("seprows_worker with model='b' (mid level)", 3)

run_simple_test("seprows_worker(model='b') - complete separation", function(backend, solver) {
  seprows_worker(y_b_cs, X_b_cs, rational = rational, model = "b")
})

run_simple_test("seprows_worker(model='b') - quasi-complete separation", function(backend, solver) {
  seprows_worker(y_b_qcs, X_b_qcs, rational = rational, model = "b")
})

run_simple_test("seprows_worker(model='b') - overlap", function(backend, solver) {
  seprows_worker(y_b_ol, X_b_ol, rational = rational, model = "b")
})

run_simple_test("seprows_worker(S=) - complete separation", function(backend, solver) {
  seprows_worker(S = S_qcs, rational = rational)
})

run_simple_test("seprows_worker(S=) - overlap", function(backend, solver) {
  seprows_worker(S = S_ol, rational = rational)
})

# --- Generic: separation_rows.glm ---
print_section("separation_rows.glm (generic)", 3)

run_simple_test("separation_rows.glm - complete separation", function(backend, solver) {
  separation_rows(endo_glm, rational = rational)
})

run_simple_test("separation_rows.glm - quasi-complete separation", function(backend, solver) {
  separation_rows(nsduh_glm, rational = rational)
})

run_simple_test("separation_rows.glm - overlap", function(backend, solver) {
  separation_rows(ovl_glm, rational = rational)
})

run_simple_test("separation_rows.formula ", function(backend, solver) {
  separation_rows(y ~ x1 + x2, data = ovldat1, model="b", rational = rational)
})

run_simple_test("separation_rows.matrix - complete separation", function(backend, solver) {
  separation_rows(S = S_cs, rational = rational)
})

## check default method
t <- rep(1:19)
class(t) <- "nudlaug"

run_simple_test("separation_rows.default", function(backend, solver) {
  separation_rows(t, rational = rational, model = "b", backend = backend, solver = solver)
})


## =============================================================================
## 1.6 linearities - Binary
## =============================================================================

print_section("linearities (Binary)", 2)


# --- Mid level: linearities with model="b" ---
print_section("linearities with model='b' (mid level)", 3)

run_simple_test("linearities(model='b') - complete separation", function(backend, solver) {
  linearities(y_b_cs, X_b_cs, rational = rational, model = "b")
})

run_simple_test("linearities(model='b') - quasi-complete separation", function(backend, solver) {
  linearities(y_b_qcs, X_b_qcs, rational = rational, model = "b")
})

run_simple_test("linearities(model='b') - overlap", function(backend, solver) {
  linearities(y_b_ol, X_b_ol, rational = rational, model = "b")
})

run_simple_test("linearities(S=) - complete separation", function(backend, solver) {
  linearities(S = S_cs, rational = rational)
})



## =============================================================================
## 1.7 reccone / rec_cone - Binary
## =============================================================================

print_section("reccone_worker ", 2)

# --- Mid level: reccone with model="b" ---
print_section("reccone_worker with model='b' (mid level)", 3)

run_simple_test("reccone_worker(model='b') - complete separation", function(backend, solver) {
  reccone_worker(y_b_cs, X_b_cs, rational = rational, model = "b")
})

run_simple_test("reccone_worker(model='b') - quasi-complete separation", function(backend, solver) {
  reccone_worker(y_b_qcs, X_b_qcs, rational = rational, model = "b")
})

run_simple_test("reccone_worker(model='b') - overlap", function(backend, solver) {
  reccone_worker(y_b_ol, X_b_ol, rational = rational, model = "b")
})

run_simple_test("reccone_worker(S=) - complete separation", function(backend, solver) {
  reccone_worker(S = S_cs, rational = rational)
})

run_simple_test("reccone_worker(S=) - quasi-complete separation", function(backend, solver) {
  reccone_worker(S = S_qcs, rational = rational)
})

# --- Generic: recession_cone.glm ---
print_section("recession_cone.glm (generic)", 3)

run_simple_test("recession_cone.glm - complete separation", function(backend, solver) {
  recession_cone(endo_glm, rational = rational)
})

run_simple_test("recession_cone.glm - quasi-complete separation", function(backend, solver) {
  recession_cone(nsduh_glm, rational = rational)
})

run_simple_test("recession_cone.glm - overlap", function(backend, solver) {
  recession_cone(ovl_glm, rational = rational)
})

run_simple_test("recession_cone.formula ", function(backend, solver) {
  recession_cone(y ~ x1 + x2, data = ovldat1, model="b", rational = rational)
})

run_simple_test("recession_cone.matrix - quasi-complete separation", function(backend, solver) {
  recession_cone(S = S_qcs, rational = rational)
})


## check default method
t <- rep(1:19)
class(t) <- "nudlaug"

run_simple_test("recession_cone.default", function(backend, solver) {
  recession_cone(t, rational = rational, model = "b")
})


## =============================================================================
## 1.8 overlap_fraction_check - Binary
## =============================================================================

print_section("overlap_fraction_check (Binary)", 2)


# --- Mid level: overlap_fraction_check with model="b" ---
print_section("overlap_fraction_check with model='b' (mid level)", 3)

run_test("overlap_fraction_check(model='b') - complete separation", function(backend, solver) {
  overlap_fraction_check(y_b_cs, X_b_cs, frac = 10, verbose = 0, rational = rational, 
             model = "b", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='b') - quasi-complete separation", function(backend, solver) {
  overlap_fraction_check(y_b_qcs, X_b_qcs, frac = 10, verbose = 0, rational = rational, 
             model = "b", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='b') - overlap", function(backend, solver) {
  overlap_fraction_check(y_b_ol, X_b_ol, frac = 10, verbose = 0, rational = rational, 
             model = "b", backend = backend, solver = solver)
})

# --- Mid level: overlap_fraction_check default (no model specified) ---
print_section("overlap_fraction_check default (mid level)", 3)

run_test("overlap_fraction_check - complete separation", function(backend, solver) {
  overlap_fraction_check(y_b_cs, X_b_cs, frac = 10, verbose = 0, rational = rational, 
             backend = backend, solver = solver)
})

run_test("overlap_fraction_check - quasi-complete separation", function(backend, solver) {
  overlap_fraction_check(y_b_qcs, X_b_qcs, frac = 10, verbose = 0, rational = rational, 
             backend = backend, solver = solver)
})

run_test("overlap_fraction_check(S) - quasi-complete separation", function(backend, solver) {
  overlap_fraction_check(S=S_qcs, frac = 10, verbose = 0, rational = rational, 
             backend = backend, solver = solver)
})


## =============================================================================
## 1.9 overlap_quick_check - Binary
## =============================================================================

print_section("overlap_quick_check (Binary)", 2)


# --- Mid level: overlap_quick_check with model="b" ---
print_section("overlap_quick_check with model='b' (mid level)", 3)

run_test("overlap_quick_check(model='b') - complete separation", function(backend, solver) {
  overlap_quick_check(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='b') - quasi-complete separation", function(backend, solver) {
  overlap_quick_check(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='b') - overlap", function(backend, solver) {
  overlap_quick_check(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})

# --- Mid level: overlap_quick_check default ---
print_section("overlap_quick_check default (mid level)", 3)

run_test("overlap_quick_check - complete separation", function(backend, solver) {
  overlap_quick_check(y_b_cs, X_b_cs, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_quick_check - quasi-complete separation", function(backend, solver) {
  overlap_quick_check(y_b_qcs, X_b_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_quick_check - overlap", function(backend, solver) {
  overlap_quick_check(y_b_ol, X_b_ol, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_quick_check(S) - quasi-complete separation", function(backend, solver) {
  overlap_quick_check(S=S_qcs, rational = rational, 
             backend = backend, solver = solver)
})

## =============================================================================
## 1.10 separation_quick_check - Binary
## =============================================================================

print_section("separation_quick_check (Binary)", 2)


# --- Mid level: separation_quick_check with model="b" ---
print_section("separation_quick_check with model='b' (mid level)", 3)

run_test("separation_quick_check(model='b') - complete separation", function(backend, solver) {
  separation_quick_check(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='b') - quasi-complete separation", function(backend, solver) {
  separation_quick_check(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='b') - overlap", function(backend, solver) {
  separation_quick_check(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})

# --- Mid level: separation_quick_check default ---
print_section("separation_quick_check default (mid level)", 3)

run_test("separation_quick_check - complete separation", function(backend, solver) {
  separation_quick_check(y_b_cs, X_b_cs, rational = rational, backend = backend, solver = solver)
})

run_test("separation_quick_check - quasi-complete separation", function(backend, solver) {
  separation_quick_check(y_b_qcs, X_b_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("separation_quick_check - overlap", function(backend, solver) {
  separation_quick_check(y_b_ol, X_b_ol, rational = rational, backend = backend, solver = solver)
})

run_test("separation_quick_check(S) - quasi-complete separation", function(backend, solver) {
  separation_quick_check(S=S_qcs, rational = rational, 
             backend = backend, solver = solver)
})


## =============================================================================
## structure vectors 
## =============================================================================

print_section("structure_vectors (Binary)", 2)


run_simple_test("structure_vectors with label", function() {
  structure_vectors(y_b_cs, X_b_cs, model = "b", rational = rational)
})

run_simple_test("structure_vectors without labels", function() {
  structure_vectors(y_b_cs, X_b_cs, model = "b", rational = rational, label = FALSE)
})

run_simple_test("structure_vectors with label from formula", function() {
  structure_vectors(HG ~ NV + PI + EH, data = endometrial ,  model = "b", rational = rational)
})


cat("")
cat(paste(rep("#", 78), collapse = ""), "")
cat("##  END OF BINARY MODEL TESTS")
cat(paste(rep("#", 78), collapse = ""), "")


################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE - PART 2: BCL MODEL TESTS                       ##             
##                                                                            ##
################################################################################

print_section("Baseline-Category Logit Model Tests (bcl)")

## =============================================================================
## 2.1 checksep - BCL
## =============================================================================

print_section("checksep_worker (BCL)", 2)

# --- Mid level: checksep_worker with model="bcl" ---
print_section("checksep_worker with model='bcl' (mid level)", 3)

run_test("checksep_worker(model='bcl') - complete separation", function(backend, solver) {
  checksep_worker(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("checksep_worker(model='bcl') - quasi-complete separation", function(backend, solver) {
  checksep_worker(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("checksep_worker(model='bcl') - overlap", function(backend, solver) {
  checksep_worker(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
})

# --- Generic: check_separation.multinom ---
print_section("check_separation.multinom (generic)", 3)

run_test("check_separation.multinom - complete separation", function(backend, solver) {
  check_separation(csep_bcl, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.multinom - quasi-complete separation", function(backend, solver) {
  check_separation(qcsep_bcl, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.multinom - overlap", function(backend, solver) {
  check_separation(ovl_bcl, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.multinom - no separation (Alligators)", function(backend, solver) {
  check_separation(allgm1, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.multinom - quasi-complete (Alligators interaction)", function(backend, solver) {
  check_separation(allgm2, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.multinom - quick quasi-complete (Alligators interaction)", function(backend, solver) {
  check_separation(allgm2, rational = rational, backend = backend, solver = solver, quick = TRUE)
})

run_test("check_separation.brmultinom - quasi-complete", function(backend, solver) {
  check_separation(allgm3, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.formula ", function(backend, solver) {
  check_separation(y ~ x1 + x2, data = qcsepdatm, model="bcl", rational = rational, backend = backend, solver = solver)
})


###########
### check_overlap                                       
#################

print_section("check_overlap (BCL)", 2)

# --- Mid level: check_overlap with model="b" ---
print_section("check_overlap with model='bcl' (mid level)", 3)

run_test("check_overlap(model='bcl') - complete separation", function(backend, solver) {
  check_overlap(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("check_overlap(model='bcl') - quasi-complete separation", function(backend, solver) {
  check_overlap(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("check_overlap(model='bcl') - overlap", function(backend, solver) {
  check_overlap(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
})



## =============================================================================
## 2.3 diagsep_worker - BCL
## =============================================================================

print_section("diagsep_worker (BCL)", 2)

# --- Mid level: diagsep_worker with model="bcl" ---
print_section("diagsep_worker with model='bcl' (mid level)", 3)

run_test("diagsep_worker(model='bcl') - complete separation", function(backend, solver) {
  diagsep_worker(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='bcl') - quasi-complete separation", function(backend, solver) {
  diagsep_worker(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='bcl') - overlap", function(backend, solver) {
  diagsep_worker(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
})

# --- Generic: diagnose_separation.multinom ---
print_section("diagnose_separation.multinom (generic)", 3)

run_test("diagnose_separation.multinom - complete separation", function(backend, solver) {
  diagnose_separation(csep_bcl, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.multinom - quasi-complete separation", function(backend, solver) {
  diagnose_separation(qcsep_bcl, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.multinom - overlap", function(backend, solver) {
  diagnose_separation(ovl_bcl, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.brmultinom - quasi-complete separation", function(backend, solver) {
  diagnose_separation(allgm3, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.formula ", function(backend, solver) {
  diagnose_separation(y ~ x1 + x2, data = qcsepdatm, model="bcl", rational = rational, backend = backend, solver = solver)
})

# --- print.sepmod for BCL ---
print_section("print.sepmod (BCL)", 3)

run_simple_test("print.sepmod - default (BCL)", function() {
  sd1 <- diagsep_worker(y_bcl_qcs, X_bcl_qcs, model= "bcl", rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full (BCL)", function() {
  sd1 <- diagsep_worker(y_bcl_qcs, X_bcl_qcs, model="bcl", rational = rational)
  print(sd1, info = "full")
})

## =============================================================================
## 2.4 sepcols / sepcols_worker - BCL
## =============================================================================

print_section("sepcols / sepcols_worker (BCL)", 2)

# --- Mid level: sepcols_worker with model="bcl" ---
print_section("sepcols_worker with model='bcl' (mid level)", 3)

run_test("sepcols_worker(model='bcl') - complete separation", function(backend, solver) {
  sepcols_worker(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("sepcols_worker(model='bcl') - quasi-complete separation", function(backend, solver) {
  sepcols_worker(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("sepcols_worker(model='bcl') - overlap", function(backend, solver) {
  sepcols_worker(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
})

# --- Generic: separation_columns.multinom ---
print_section("separation_columns.multinom (generic)", 3)

run_test("separation_columns.multinom - complete separation", function(backend, solver) {
  separation_columns(csep_bcl, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.multinom - quasi-complete separation", function(backend, solver) {
  separation_columns(qcsep_bcl, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.multinom - overlap", function(backend, solver) {
  separation_columns(ovl_bcl, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.multinom - no separation (Alligators)", function(backend, solver) {
  separation_columns(allgm1, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.brmultinom - no separation (Alligators)", function(backend, solver) {
  separation_columns(allgm3, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.formula ", function(backend, solver) {
  separation_columns(y ~ x1 + x2, data = qcsepdatm, model="bcl", rational = rational, backend = backend, solver = solver)
})

## =============================================================================
## 2.5 seprows - BCL
## =============================================================================

print_section("seprows_worker (BCL)", 2)


# --- Mid level: seprows_worker with model="bcl" ---
print_section("seprows_worker with model='bcl' (mid level)", 3)

run_simple_test("seprows_worker(model='bcl') - complete separation", function(backend, solver) {
  seprows_worker(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl")
})

run_simple_test("seprows_worker(model='bcl') - quasi-complete separation", function(backend, solver) {
  seprows_worker(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl")
})

run_simple_test("seprows_worker(model='bcl') - overlap", function(backend, solver) {
  seprows_worker(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl")
})

# --- Generic: separation_rows.multinom ---
print_section("separation_rows.multinom (generic)", 3)

run_simple_test("separation_rows.multinom - complete separation", function(backend, solver) {
  separation_rows(csep_bcl, rational = rational)
})

run_simple_test("separation_rows.multinom - quasi-complete separation", function(backend, solver) {
  separation_rows(qcsep_bcl, rational = rational)
})

run_simple_test("separation_rows.multinom - overlap", function(backend, solver) {
  separation_rows(ovl_bcl, rational = rational)
})

run_simple_test("separation_rows.multinom - no separation (Alligators)", function(backend, solver) {
  separation_rows(allgm1, rational = rational)
})


run_simple_test("separation_rows.brmultinom - quasi-complete separation (Alligators)", function(backend, solver) {
  separation_rows(allgm3, rational = rational)
})


run_simple_test("separation_rows.formula ", function(backend, solver) {
  separation_rows(y ~ x1 + x2, data = qcsepdatm, model="bcl", rational = rational)
})

## =============================================================================
## 2.6 linearities - BCL
## =============================================================================

print_section("linearities (BCL)", 2)


# --- Mid level: linearities with model="bcl" ---
print_section("linearities with model='bcl' (mid level)", 3)

run_simple_test("linearities(model='bcl') - complete separation", function(backend, solver) {
  linearities(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl")
})

run_simple_test("linearities(model='bcl') - quasi-complete separation", function(backend, solver) {
  linearities(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl")
})

run_simple_test("linearities(model='bcl') - overlap", function(backend, solver) {
  linearities(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl")
})

## =============================================================================
## 2.7 reccone_worker / rec_cone - BCL
## =============================================================================

print_section("reccone_worker / rec_cone (BCL)", 2)


# --- Mid level: reccone_worker with model="bcl" ---
print_section("reccone_worker with model='bcl' (mid level)", 3)

run_simple_test("reccone_worker(model='bcl') - complete separation", function(backend, solver) {
  reccone_worker(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl")
})

run_simple_test("reccone_worker(model='bcl') - quasi-complete separation", function(backend, solver) {
  reccone_worker(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl")
})

run_simple_test("reccone_worker(model='bcl') - overlap", function(backend, solver) {
  reccone_worker(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl")
})

# --- Generic: recession_cone.multinom ---
print_section("recession_cone.multinom (generic)", 3)

run_simple_test("recession_cone.multinom - complete separation", function(backend, solver) {
  recession_cone(csep_bcl, rational = rational)
})

run_simple_test("recession_cone.multinom - quasi-complete separation", function(backend, solver) {
  recession_cone(qcsep_bcl, rational = rational)
})

run_simple_test("recession_cone.multinom - overlap", function(backend, solver) {
  recession_cone(ovl_bcl, rational = rational)
})

run_simple_test("recession_cone.brmultinom - overlap", function(backend, solver) {
  recession_cone(allgm3, rational = rational)
})


run_simple_test("recession_cone.formula ", function(backend, solver) {
  recession_cone(y ~ x1 + x2, data = qcsepdatm, model="bcl", rational = rational)
})

## =============================================================================
## 2.8 overlap_fraction_check - BCL
## =============================================================================

print_section("overlap_fraction_check (BCL)", 2)


# --- Mid level: overlap_fraction_check with model="bcl" ---
print_section("overlap_fraction_check with model='bcl' (mid level)", 3)

run_test("overlap_fraction_check(model='bcl') - complete separation", function(backend, solver) {
  overlap_fraction_check(y_bcl_cs, X_bcl_cs, frac = 1, verbose = 0, rational = rational, 
             model = "bcl", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='bcl') - quasi-complete separation", function(backend, solver) {
  overlap_fraction_check(y_bcl_qcs, X_bcl_qcs, frac = 1, verbose = 0, rational = rational, 
             model = "bcl", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='bcl') - overlap", function(backend, solver) {
  overlap_fraction_check(y_bcl_ol, X_bcl_ol, frac = 1, verbose = 0, rational = rational, 
             model = "bcl", backend = backend, solver = solver)
})

## =============================================================================
## 2.9 overlap_qc - BCL
## =============================================================================

print_section("overlap_qc (BCL)", 2)


# --- Mid level: overlap_qc with model="bcl" ---
print_section("overlap_quick_check with model='bcl' (mid level)", 3)

run_test("overlap_quick_check(model='bcl') - complete separation", function(backend, solver) {
  overlap_quick_check(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='bcl') - quasi-complete separation", function(backend, solver) {
  overlap_quick_check(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='bcl') - overlap", function(backend, solver) {
  overlap_quick_check(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
})


## =============================================================================
## 2.10 separation_quick_check - BCL
## =============================================================================

print_section("separation_quick_check (BCL)", 2)


# --- Mid level: separation_quick_check with model="bcl" ---
print_section("separation_quick_check with model='bcl' (mid level)", 3)

run_test("separation_quick_check(model='bcl') - complete separation", function(backend, solver) {
  separation_quick_check(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='bcl') - quasi-complete separation", function(backend, solver) {
  separation_quick_check(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='bcl') - overlap", function(backend, solver) {
  separation_quick_check(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
})


# --- Mid level: separation_quick_check default ---
print_section("separation_quick_check default (mid level)", 3)

run_test("separation_quick_check - complete separation", function(backend, solver) {
  separation_quick_check(y_bcl_cs, X_bcl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("separation_quick_check - quasi-complete separation", function(backend, solver) {
  separation_quick_check(y_bcl_qcs, X_bcl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("separation_quick_check - overlap", function(backend, solver) {
  separation_quick_check(y_bcl_ol, X_bcl_ol, rational = rational, backend = backend, solver = solver)
})


## =============================================================================
## structure vectors 
## =============================================================================

print_section("structure_vectors (BCL)", 2)


run_simple_test("structure_vectors with label", function() {
  structure_vectors(y_bcl_ol, X_bcl_ol, model = "bcl", rational = rational)
})

run_simple_test("structure_vectors without labels", function() {
  structure_vectors(y_bcl_ol, X_bcl_ol, model = "bcl", rational = rational, label = FALSE)
})

run_simple_test("structure_vectors with label from formula", function() {
  structure_vectors(y ~ x1 + x2, data = ovldatm,  model = "bcl", rational = rational)
})


cat("")
cat(paste(rep("#", 78), collapse = ""), "")
cat("##  END OF BCL MODEL TESTS")
cat(paste(rep("#", 78), collapse = ""), "")

################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE - PART 3: CL MODEL TESTS                        ##
##                                                                            ##
################################################################################

print_section("Cumulative Logit Model Tests (cl)")

## =============================================================================
## 3.1 checksep_worker - CL
## =============================================================================

print_section("checksep_worker (CL)", 2)


# --- Mid level: checksep_worker with model="cl" ---
print_section("checksep_worker with model='cl' (mid level)", 3)

run_test("checksep_worker(model='cl') - HDSS", function(backend, solver) {
  checksep_worker(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("checksep_worker(model='cl') - wine", function(backend, solver) {
  checksep_worker(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("checksep_worker(model='cl') - wine with bottle", function(backend, solver) {
  checksep_worker(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
})

# --- Generic: check_separation.clm ---
print_section("check_separation.clm (generic)", 3)

run_test("check_separation.clm - HDSS", function(backend, solver) {
  check_separation(hdss_clm, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.clm - HDSS quick", function(backend, solver) {
  check_separation(hdss_clm, rational = rational, backend = backend, solver = solver, quick = TRUE)
})

run_test("check_separation.clm - wine", function(backend, solver) {
  check_separation(wine_clm, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.clm - wine with bottle (singularities)", function(backend, solver) {
  check_separation(wine_clm2, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.plor - HDSS", function(backend, solver) {
  check_separation(hdss_polr, rational = rational, backend = backend, solver = solver)
})


run_test("check_separation.plor - HDSS", function(backend, solver) {
  check_separation(hdss_polr, rational = rational, backend = backend, solver = solver, quick = TRUE)
})

## =============================================================================
## 3.2 check_overlap - CL
## =============================================================================

print_section("check_overlap (CL)", 2)

# --- Mid level: check_overlap with model="cl" ---
print_section("check_overlap with model='cl' (mid level)", 3)

run_test("check_overlap(model='cl') - HDSS", function(backend, solver) {
  check_overlap(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("check_overlap(model='cl') - wine", function(backend, solver) {
  check_overlap(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("check_overlap(model='cl') - wine with bottle", function(backend, solver) {
  check_overlap(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
})

## =============================================================================
## 3.3 diagsep_worker - CL
## =============================================================================

print_section("diagsep_worker (CL)", 2)


# --- Mid level: diagsep_worker with model="cl" ---
print_section("diagsep_worker with model='cl' (mid level)", 3)

run_test("diagsep_worker(model='cl') - HDSS", function(backend, solver) {
  diagsep_worker(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='cl') - wine", function(backend, solver) {
  diagsep_worker(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='cl') - wine with bottle", function(backend, solver) {
  diagsep_worker(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
})

# --- Generic: diagnose_separation.clm ---
print_section("diagnose_separation.clm (generic)", 3)

run_test("diagnose_separation.clm - HDSS", function(backend, solver) {
  diagnose_separation(hdss_clm, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.clm - wine", function(backend, solver) {
  diagnose_separation(wine_clm, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.clm - wine with bottle", function(backend, solver) {
  diagnose_separation(wine_clm2, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.polr - HDSS", function(backend, solver) {
  diagnose_separation(hdss_polr, rational = rational, backend = backend, solver = solver)
})

# --- print.sepmod for CL ---
print_section("print.sepmod (CL)", 3)

run_simple_test("print.sepmod - default (CL)", function() {
  sd1 <- diagsep_worker(y_cl_hdss, X_cl_hdss, model="cl", rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full (CL)", function() {
  sd1 <- diagsep_worker(y_cl_hdss, X_cl_hdss, model="cl", rational = rational)
  print(sd1, info = "full")
})

## =============================================================================
## 3.4 sepcols / sepcols_worker - CL
## =============================================================================

print_section("sepcols / sepcols_worker (CL)", 2)


# --- Mid level: sepcols_worker with model="cl" ---
print_section("sepcols_worker with model='cl' (mid level)", 3)

run_test("sepcols_worker(model='cl') - HDSS", function(backend, solver) {
  sepcols_worker(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("sepcols_worker(model='cl') - wine", function(backend, solver) {
  sepcols_worker(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("sepcols_worker(model='cl') - wine with bottle", function(backend, solver) {
  sepcols_worker(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
})

# --- Generic: separation_columns.clm ---
print_section("separation_columns.clm (generic)", 3)

run_test("separation_columns.clm - HDSS", function(backend, solver) {
  separation_columns(hdss_clm, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.clm - wine", function(backend, solver) {
  separation_columns(wine_clm, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.clm - wine with bottle", function(backend, solver) {
  separation_columns(wine_clm2, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.polr - HDSS", function(backend, solver) {
  separation_columns(hdss_polr, rational = rational, backend = backend, solver = solver)
})

## =============================================================================
## 3.5 seprows_worker - CL
## =============================================================================

print_section("seprows_worker (CL)", 2)


# --- Mid level: seprows_worker with model="cl" ---
print_section("seprows_worker with model='cl' (mid level)", 3)

run_simple_test("seprows_worker(model='cl') - HDSS", function(backend, solver) {
  seprows_worker(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl")
})

run_simple_test("seprows_worker(model='cl') - wine", function(backend, solver) {
  seprows_worker(y_cl_wine, X_cl_wine, rational = rational, model = "cl")
})

run_simple_test("seprows_worker(model='cl') - wine with bottle", function(backend, solver) {
  seprows_worker(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl")
})

# --- Generic: separation_rows.clm ---
print_section("separation_rows.clm (generic)", 3)

run_simple_test("separation_rows.clm - HDSS", function(backend, solver) {
  separation_rows(hdss_clm, rational = rational)
})

run_simple_test("separation_rows.clm - wine", function(backend, solver) {
  separation_rows(wine_clm, rational = rational)
})

run_simple_test("separation_rows.clm - wine with bottle", function(backend, solver) {
  separation_rows(wine_clm2, rational = rational)
})

run_simple_test("separation_rows.polr - HDSS", function(backend, solver) {
  separation_rows(hdss_polr, rational = rational)
})

## =============================================================================
## 3.6 linearities - CL
## =============================================================================

print_section("linearities (CL)", 2)


# --- Mid level: linearities with model="cl" ---
print_section("linearities with model='cl' (mid level)", 3)

run_simple_test("linearities(model='cl') - HDSS", function(backend, solver) {
  linearities(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl")
})

run_simple_test("linearities(model='cl') - wine", function(backend, solver) {
  linearities(y_cl_wine, X_cl_wine, rational = rational, model = "cl")
})

run_test("linearities(model='cl') - wine with bottle", function(backend, solver) {
  linearities(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl")
})
## =============================================================================
## 3.7 reccone_worker / rec_cone - CL
## =============================================================================

print_section("reccone_worker / rec_cone (CL)", 2)

# --- Mid level: reccone_worker with model="cl" ---
print_section("reccone_worker with model='cl' (mid level)", 3)

run_simple_test("reccone_worker(model='cl') - HDSS", function(backend, solver) {
  reccone_worker(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl")
})

run_simple_test("reccone_worker(model='cl') - wine", function(backend, solver) {
  reccone_worker(y_cl_wine, X_cl_wine, rational = rational, model = "cl")
})

run_simple_test("reccone_worker(model='cl') - wine with bottle", function(backend, solver) {
  reccone_worker(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl")
})

# --- Generic: recession_cone.clm ---
print_section("recession_cone.clm (generic)", 3)

run_simple_test("recession_cone.clm - HDSS", function(backend, solver) {
  recession_cone(hdss_clm, rational = rational)
})

run_simple_test("recession_cone.clm - wine", function(backend, solver) {
  recession_cone(wine_clm, rational = rational)
})

run_simple_test("recession_cone.clm - wine with bottle", function(backend, solver) {
  recession_cone(wine_clm2, rational = rational)
})

run_simple_test("recession_cone.polr - HDSS", function(backend, solver) {
  recession_cone(hdss_polr, rational = rational)
})

## =============================================================================
## 3.8 overlap_fraction_check - CL
## =============================================================================

print_section("overlap_fraction_check (CL)", 2)

# --- Mid level: overlap_fraction_check with model="cl" ---
print_section("overlap_fraction_check with model='cl' (mid level)", 3)

run_test("overlap_fraction_check(model='cl') - HDSS", function(backend, solver) {
  overlap_fraction_check(y_cl_hdss, X_cl_hdss, frac = 1, verbose = 0, rational = rational, 
             model = "cl", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='cl') - wine", function(backend, solver) {
  overlap_fraction_check(y_cl_wine, X_cl_wine, frac = 1, verbose = 0, rational = rational, 
             model = "cl", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='cl') - wine with bottle", function(backend, solver) {
  overlap_fraction_check(y_cl_wine2, X_cl_wine2, frac = 1, verbose = 0, rational = rational, 
             model = "cl", backend = backend, solver = solver)
})

## =============================================================================
## 3.9 overlap_quick_check - CL
## =============================================================================

print_section("overlap_quick_check (CL)", 2)


# --- Mid level: overlap_quick_check with model="cl" ---
print_section("overlap_quick_check with model='cl' (mid level)", 3)

run_test("overlap_quick_check(model='cl') - HDSS", function(backend, solver) {
  overlap_quick_check(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='cl') - wine", function(backend, solver) {
  overlap_quick_check(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='cl') - wine with bottle", function(backend, solver) {
  overlap_quick_check(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
})

## =============================================================================
## 3.10 separation_quick_check - CL
## =============================================================================

print_section("separation_quick_check (CL)", 2)


# --- Mid level: separation_quick_check with model="cl" ---
print_section("separation_quick_check with model='cl' (mid level)", 3)

run_test("separation_quick_check(model='cl') - HDSS", function(backend, solver) {
  separation_quick_check(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='cl') - wine", function(backend, solver) {
  separation_quick_check(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='cl') - wine with bottle", function(backend, solver) {
  separation_quick_check(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
})


## =============================================================================
## structure vectors 
## =============================================================================

print_section("structure_vectors (CL)", 2)


run_simple_test("structure_vectors with label", function() {
  structure_vectors(y_cl_wine2, X_cl_wine2, model = "cl", rational = rational)
})

run_simple_test("structure_vectors without labels", function() {
  structure_vectors(y_cl_wine2, X_cl_wine2, model = "cl", rational = rational, label = FALSE)
})

run_simple_test("structure_vectors with label from formula", function() {
  structure_vectors( rating ~ temp + contact + bottle, data = wine, model = "cl", rational = rational)
})


cat("")
cat(paste(rep("#", 78), collapse = ""), "")
cat("##  END OF CL MODEL TESTS")
cat(paste(rep("#", 78), collapse = ""), "")



################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE - PART 4: ACL MODEL TESTS                       ##
##                                                                            ##
################################################################################

print_section("Adjacent-Category Logit Model Tests (acl)")

## =============================================================================
## 4.1 checksep_worker - ACL
## =============================================================================

print_section("checksep_worker (ACL)", 2)


# --- Mid level: checksep_worker with model="acl" ---
print_section("checksep_worker with model='acl' (mid level)", 3)

run_test("checksep_worker(model='acl') - complete separation", function(backend, solver) {
  checksep_worker(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("checksep_worker(model='acl') - quasi-complete separation", function(backend, solver) {
  checksep_worker(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("checksep_worker(model='acl') - overlap", function(backend, solver) {
  checksep_worker(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})

### generic
run_test("check_separation.bracl - quasi-complete", function(backend, solver) {
  check_separation(hdss_pacl, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.bracl - quasi-complete ", function(backend, solver) {
  check_separation(hdss_npacl, rational = rational, backend = backend, solver = solver)
})


run_test("check_separation.bracl - quasi-complete quick", function(backend, solver) {
  check_separation(hdss_pacl, rational = rational, backend = backend, solver = solver, quick = TRUE)
})

run_test("check_separation.bracl - quasi-complete quick", function(backend, solver) {
  check_separation(hdss_npacl, rational = rational, backend = backend, solver = solver, quick = TRUE)
})

## =============================================================================
## 4.2 check_overlap - ACL
## =============================================================================

print_section("check_overlap (ACL)", 2)



# --- Mid level: check_overlap with model="acl" ---
print_section("check_overlap with model='acl' (mid level)", 3)

run_test("check_overlap(model='acl') - complete separation", function(backend, solver) {
  check_overlap(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("check_overlap(model='acl') - quasi-complete separation", function(backend, solver) {
  check_overlap(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("check_overlap(model='acl') - overlap", function(backend, solver) {
  check_overlap(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})

## =============================================================================
## 4.3 diagsep_worker - ACL
## =============================================================================

print_section("diagsep_worker (ACL)", 2)


# --- Mid level: diagsep_worker with model="acl" ---
print_section("diagsep_worker with model='acl' (mid level)", 3)

run_test("diagsep_worker(model='acl') - complete separation", function(backend, solver) {
  diagsep_worker(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='acl') - quasi-complete separation", function(backend, solver) {
  diagsep_worker(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='acl') - overlap", function(backend, solver) {
  diagsep_worker(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})


### generic
run_test("diagnose_separation.bracl parallel - quasi-complete", function(backend, solver) {
  diagnose_separation(hdss_pacl, rational = rational, backend = backend, solver = solver)
})

run_test("diagnose_separation.bracl nonparallel - quasi-complete ", function(backend, solver) {
  diagnose_separation(hdss_npacl, rational = rational, backend = backend, solver = solver)
})


# --- print.sepmod for ACL ---
print_section("print.sepmod (ACL)", 3)

run_simple_test("print.sepmod - default (ACL)", function() {
  sd1 <- diagsep_worker(y_acl_qcs, X_acl_qcs, model="acl", rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full (ACL)", function() {
  sd1 <- diagsep_worker(y_acl_qcs, X_acl_qcs, model="acl", rational = rational)
  print(sd1, info = "full")
})


## =============================================================================
## 4.4 sepcols / sepcols_worker - ACL
## =============================================================================

print_section("sepcols / sepcols_worker (ACL)", 2)


# --- Mid level: sepcols_worker with model="acl" ---
print_section("sepcols_worker with model='acl' (mid level)", 3)

run_test("sepcols_worker(model='acl') - complete separation", function(backend, solver) {
  sepcols_worker(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("sepcols_worker(model='acl') - quasi-complete separation", function(backend, solver) {
  sepcols_worker(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("sepcols_worker(model='acl') - overlap", function(backend, solver) {
  sepcols_worker(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})

### generic
run_test("separation_columns.bracl parallel - quasi-complete", function(backend, solver) {
  separation_columns(hdss_pacl, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.bracl nonparallel - quasi-complete ", function(backend, solver) {
  separation_columns(hdss_npacl, rational = rational, backend = backend, solver = solver)
})



## =============================================================================
## 4.5 seprows_worker - ACL
## =============================================================================

print_section("seprows_worker (ACL)", 2)


# --- Mid level: seprows_worker with model="acl" ---
print_section("seprows_worker with model='acl' (mid level)", 3)

run_simple_test("seprows_worker(model='acl') - complete separation", function(backend, solver) {
  seprows_worker(y_acl_cs, X_acl_cs, rational = rational, model = "acl")
})

run_simple_test("seprows_worker(model='acl') - quasi-complete separation", function(backend, solver) {
  seprows_worker(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl")
})

run_simple_test("seprows_worker(model='acl') - overlap", function(backend, solver) {
  seprows_worker(y_acl_ol, X_acl_ol, rational = rational, model = "acl")
})

### generic
run_simple_test("separation_rows.bracl parallel - quasi-complete", function(backend, solver) {
  separation_rows(hdss_pacl, rational = rational)
})

run_simple_test("separation_rows.bracl nonparallel - quasi-complete ", function(backend, solver) {
  separation_rows(hdss_npacl, rational = rational)
})

## =============================================================================
## 4.6 linearities - ACL
## =============================================================================

print_section("linearities (ACL)", 2)


# --- Mid level: linearities with model="acl" ---
print_section("linearities with model='acl' (mid level)", 3)

run_simple_test("linearities(model='acl') - complete separation", function(backend, solver) {
  linearities(y_acl_cs, X_acl_cs, rational = rational, model = "acl")
})

run_simple_test("linearities(model='acl') - quasi-complete separation", function(backend, solver) {
  linearities(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl")
})

run_simple_test("linearities(model='acl') - overlap", function(backend, solver) {
  linearities(y_acl_ol, X_acl_ol, rational = rational, model = "acl")
})


## =============================================================================
## 4.7 reccone_worker / rec_cone - ACL
## =============================================================================

print_section("reccone_worker / rec_cone (ACL)", 2)


# --- Mid level: reccone_worker with model="acl" ---
print_section("reccone_worker with model='acl' (mid level)", 3)

run_simple_test("reccone_worker(model='acl') - complete separation", function(backend, solver) {
  reccone_worker(y_acl_cs, X_acl_cs, rational = rational, model = "acl")
})

run_simple_test("reccone_worker(model='acl') - quasi-complete separation", function(backend, solver) {
  reccone_worker(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl")
})

run_simple_test("reccone_worker(model='acl') - overlap", function(backend, solver) {
  reccone_worker(y_acl_ol, X_acl_ol, rational = rational, model = "acl")
})


### generic
run_simple_test("recession_cone.bracl parallel - quasi-complete", function(backend, solver) {
  recession_cone(hdss_pacl, rational = rational)
})

run_simple_test("recession_cone.bracl nonparallel - quasi-complete ", function(backend, solver) {
  recession_cone(hdss_npacl, rational = rational)
})

## =============================================================================
## 4.8 overlap_fraction_check - ACL
## =============================================================================

print_section("overlap_fraction_check (ACL)", 2)

# --- Mid level: overlap_fraction_check with model="acl" ---
print_section("overlap_fraction_check with model='acl' (mid level)", 3)

run_test("overlap_fraction_check(model='acl') - complete separation", function(backend, solver) {
  overlap_fraction_check(y_acl_cs, X_acl_cs, frac = 1, verbose = 0, rational = rational, 
             model = "acl", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='acl') - quasi-complete separation", function(backend, solver) {
  overlap_fraction_check(y_acl_qcs, X_acl_qcs, frac = 1, verbose = 0, rational = rational, 
             model = "acl", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='acl') - overlap", function(backend, solver) {
  overlap_fraction_check(y_acl_ol, X_acl_ol, frac = 1, verbose = 0, rational = rational, 
             model = "acl", backend = backend, solver = solver)
})

## =============================================================================
## 4.9 overlap_qc - ACL
## =============================================================================

print_section("overlap_quick_check (ACL)", 2)

# --- Mid level: overlap_quick_check with model="acl" ---
print_section("overlap_quick_check with model='acl' (mid level)", 3)

run_test("overlap_quick_check(model='acl') - complete separation", function(backend, solver) {
  overlap_quick_check(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='acl') - quasi-complete separation", function(backend, solver) {
  overlap_quick_check(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='acl') - overlap", function(backend, solver) {
  overlap_quick_check(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})

## =============================================================================
## 4.10 separation_quick_check - ACL
## =============================================================================

print_section("separation_quick_check (ACL)", 2)

# --- Mid level: separation_quick_check with model="acl" ---
print_section("separation_quick_check with model='acl' (mid level)", 3)

run_test("separation_quick_check(model='acl') - complete separation", function(backend, solver) {
  separation_quick_check(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='acl') - quasi-complete separation", function(backend, solver) {
  separation_quick_check(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='acl') - overlap", function(backend, solver) {
  separation_quick_check(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})

## =============================================================================
## structure vectors 
## =============================================================================

print_section("structure_vectors (AL)", 2)


run_simple_test("structure_vectors with label", function() {
  structure_vectors(y_cl_wine2, X_cl_wine2, model = "acl", rational = rational)
})

run_simple_test("structure_vectors without labels", function() {
  structure_vectors(y_cl_wine2, X_cl_wine2, model = "acl", rational = rational, label = FALSE)
})

run_simple_test("structure_vectors with label from formula", function() {
  structure_vectors( rating ~ temp + contact + bottle, data = wine, model = "acl", rational = rational)
})


cat("")
cat(paste(rep("#", 78), collapse = ""), "")
cat("##  END OF ACL MODEL TESTS")
cat(paste(rep("#", 78), collapse = ""), "")


################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE - PART 5: OS MODEL TESTS                       ##
##                                                                            ##
################################################################################

print_section("Ordered Stereotype Model Tests (os)")

## TODO: Are these results weird? 


## =============================================================================
## 5.1 checksep_worker - OS
## =============================================================================

print_section("checksep_worker (OS)", 2)


# --- Mid level: checksep_worker with model="os" ---
print_section("checksep_worker with model='os' (mid level)", 3)

run_test("checksep_worker(model='os') - quasi-complete separation", function(backend, solver) {
  checksep_worker(y_os_qcs, X_os_qcs, rational = rational, model = "os", backend = backend, solver = solver)
})

run_test("checksep_worker(model='os') - overlap", function(backend, solver) {
  checksep_worker(y_os_ol, X_os_ol, rational = rational, model = "os", backend = backend, solver = solver)
})



# --- Generic: check_separation.osm ---
print_section("check_separation.osm (generic)", 3)

run_test("check_separation.osm - wine data", function(backend, solver) {
  check_separation(wine_os, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.osm - wine data", function(backend, solver) {
  check_separation(wine_os, rational = rational, backend = backend, solver = solver, quick = TRUE)
})

run_test("checksep_worker(model='os') -  quick", function(backend, solver) {
  check_separation(y_os_qcs, X_os_qcs, rational = rational, model = "os", backend = backend, solver = solver, quick = TRUE)
})

run_test("checksep_worker(model='os') - overlap quick", function(backend, solver) {
  check_separation(y_os_ol, X_os_ol, rational = rational, model = "os", backend = backend, solver = solver, quick = TRUE)
})

run_test("checksep_worker(model='os') - ", function(backend, solver) {
  check_separation(y_os_qcs, X_os_qcs, rational = rational, model = "os", backend = backend, solver = solver, quick = FALSE)
})

run_test("checksep_worker(model='os') - overlap", function(backend, solver) {
  check_separation(y_os_ol, X_os_ol, rational = rational, model = "os", backend = backend, solver = solver, quick = FALSE)
})


## =============================================================================
## 5.2 check_overlap - OS
## =============================================================================

print_section("check_overlap (OS)", 2)


# --- Mid level: check_overlap with model="os" ---
print_section("check_overlap with model='os' (mid level)", 3)

run_test("check_overlap(model='os') - quasi-complete separation", function(backend, solver) {
  check_overlap(y_os_qcs, X_os_qcs, rational = rational, model = "os", backend = backend, solver = solver)
})

run_test("check_overlap(model='os') - quasi-complete separation", function(backend, solver) {
  check_overlap(y_os_qcs, X_os_qcs, rational = rational, model = "os", backend = backend, solver = solver)
})

run_test("check_overlap(model='os') - overlap", function(backend, solver) {
  check_overlap(y_os_ol, X_os_ol, rational = rational, model = "os", backend = backend, solver = solver)
})

## =============================================================================
## 5.3 diagsep_worker - OS
## =============================================================================

print_section("diagsep_worker (OS)", 2)

# --- Mid level: diagsep_worker with model="os" ---
print_section("diagsep_worker with model='os' (mid level)", 3)

run_test("diagsep_worker(model='os') - quasi-complete separation", function(backend, solver) {
  diagsep_worker(y_os_qcs, X_os_qcs, rational = rational, model = "os", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='os') - overlap", function(backend, solver) {
  diagsep_worker(y_os_ol, X_os_ol, rational = rational, model = "os", backend = backend, solver = solver)
})

# --- Generic: diagnose_separation.osm ---
print_section("diagnose_separation.osm (generic)", 3)

run_test("diagnose_separation.osm - wine data", function(backend, solver) {
  diagnose_separation(wine_os, rational = rational, backend = backend, solver = solver)
})

# --- print.sepmod for OS ---
print_section("print.sepmod (OS)", 3)

run_simple_test("print.sepmod - default (OS)", function() {
  sd1 <- diagsep_worker(y_os_qcs, X_os_qcs, model="os", rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full (OS)", function() {
  sd1 <- diagsep_worker(y_os_qcs, X_os_qcs, model="os", rational = rational)
  print(sd1, info = "full")
})

## =============================================================================
## 5.4 sepcols / sepcols_worker - OS
## =============================================================================

print_section("sepcols / sepcols_worker (OS)", 2)


# --- Mid level: sepcols_worker with model="os" ---
print_section("sepcols_worker with model='os' (mid level)", 3)


run_test("sepcols_worker(model='os') - quasi-complete separation", function(backend, solver) {
  sepcols_worker(y_os_qcs, X_os_qcs, rational = rational, model = "os", backend = backend, solver = solver)
})

run_test("sepcols_worker(model='os') - overlap", function(backend, solver) {
  sepcols_worker(y_os_ol, X_os_ol, rational = rational, model = "os", backend = backend, solver = solver)
})

# --- Generic: separation_columns.osm ---
print_section("separation_columns.osm (generic)", 3)

run_test("separation_columns.osm - wine data", function(backend, solver) {
  separation_columns(wine_os, rational = rational, backend = backend, solver = solver)
})

## =============================================================================
## 5.5 seprows_worker - OS
## =============================================================================

print_section("seprows_worker (OS)", 2)


# --- Mid level: seprows_worker with model="os" ---
print_section("seprows_worker with model='os' (mid level)", 3)

run_simple_test("seprows_worker(model='os') - quasi-complete separation", function(backend, solver) {
  seprows_worker(y_os_qcs, X_os_qcs, rational = rational, model = "os")
})

run_simple_test("seprows_worker(model='os') - overlap", function(backend, solver) {
  seprows_worker(y_os_ol, X_os_ol, rational = rational, model = "os")
})

# --- Generic: separation_rows.osm ---
print_section("separation_rows.osm (generic)", 3)

run_simple_test("separation_rows.osm - wine data", function(backend, solver) {
  separation_rows(wine_os, rational = rational)
})

## =============================================================================
## 5.6 linearities - OS
## =============================================================================

print_section("linearities (OS)", 2)


# --- Mid level: linearities with model="os" ---
print_section("linearities with model='os' (mid level)", 3)

run_simple_test("linearities(model='os') - quasi-complete separation", function(backend, solver) {
  linearities(y_os_qcs, X_os_qcs, rational = rational, model = "os")
})

run_simple_test("linearities(model='os') - overlap", function(backend, solver) {
  linearities(y_os_ol, X_os_ol, rational = rational, model = "os")
})

## =============================================================================
## 5.7 reccone_worker / rec_cone - OS
## =============================================================================

print_section("reccone_worker / rec_cone (OS)", 2)


# --- Mid level: reccone_worker with model="os" ---
print_section("reccone_worker with model='os' (mid level)", 3)

run_simple_test("reccone_worker(model='os') - quasi-complete separation", function(backend, solver) {
  reccone_worker(y_os_qcs, X_os_qcs, rational = rational, model = "os")
})

run_simple_test("reccone_worker(model='os') - overlap", function(backend, solver) {
  reccone_worker(y_os_ol, X_os_ol, rational = rational, model = "os")
})

# --- Generic: recession_cone.osm ---
print_section("recession_cone.osm (generic)", 3)

run_simple_test("recession_cone.osm - wine data", function(backend, solver) {
  recession_cone(wine_os, rational = rational)
})

## =============================================================================
## 5.8 overlap_fraction_check - OS
## =============================================================================

print_section("overlap_fraction_check (OS)", 2)

# --- Mid level: overlap_fraction_check with model="os" ---
print_section("overlap_fraction_check with model='os' (mid level)", 3)


run_test("overlap_fraction_check(model='os') - quasi-complete separation", function(backend, solver) {
  overlap_fraction_check(y_os_qcs, X_os_qcs, frac = 1, verbose = 0, rational = rational, 
             model = "os", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='os') - overlap", function(backend, solver) {
  overlap_fraction_check(y_os_ol, X_os_ol, frac = 1, verbose = 0, rational = rational, 
             model = "os", backend = backend, solver = solver)
})

## =============================================================================
## 5.9 overlap_quick_check - OS
## =============================================================================

# --- Mid level: overlap_quick_check with model="os" ---
print_section("overlap_quick_check with model='os' (mid level)", 3)


run_test("overlap_quick_check(model='os') - quasi-complete separation", function(backend, solver) {
  overlap_quick_check(y_os_qcs, X_os_qcs, rational = rational, model = "os", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='os') - overlap", function(backend, solver) {
  overlap_quick_check(y_os_ol, X_os_ol, rational = rational, model = "os", backend = backend, solver = solver)
})

## =============================================================================
## 5.10 separation_quick_check - OS
## =============================================================================

print_section("separation_quick_check (OS)", 2)

# --- Mid level: separation_quick_check with model="os" ---
print_section("separation_quick_check with model='os' (mid level)", 3)


run_test("separation_quick_check(model='os') - quasi-complete separation", function(backend, solver) {
  separation_quick_check(y_os_qcs, X_os_qcs, rational = rational, model = "os", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='os') - overlap", function(backend, solver) {
  separation_quick_check(y_os_ol, X_os_ol, rational = rational, model = "os", backend = backend, solver = solver)
})

#####################
##  structure vectors
##############################

print_section("structure_vectors (OS)", 2)


run_simple_test("structure_vectors with label", function() {
  structure_vectors(y_cl_wine2, X_cl_wine2, model = "os", rational = rational)
})

run_simple_test("structure_vectors without labels", function() {
  structure_vectors(y_cl_wine2, X_cl_wine2, model = "os", rational = rational, label = FALSE)
})

run_simple_test("structure_vectors with label from formula", function() {
  structure_vectors( rating ~ temp + contact + bottle, data = wine, model = "os", rational = rational)
})


cat("")
cat(paste(rep("#", 78), collapse = ""), "")
cat("##  END OF OS MODEL TESTS")
cat(paste(rep("#", 78), collapse = ""), "")

################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE - PART 6: SL MODEL TESTS                        ##
##                                                                            ##
################################################################################

print_section("Sequential Logit Model Tests (sl)")

## =============================================================================
## 6.1 checksep_worker - SL
## =============================================================================

print_section("checksep_worker (SL)", 2)

# --- Mid level: checksep_worker with model="sl" ---
print_section("checksep_worker with model='sl' (mid level)", 3)

run_test("checksep_worker(model='sl') - complete separation", function(backend, solver) {
  checksep_worker(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("checksep_worker(model='sl') - quasi-complete separation", function(backend, solver) {
  checksep_worker(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("checksep_worker(model='sl') - overlap", function(backend, solver) {
  checksep_worker(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

## =============================================================================
## 6.2 check_overlap - SL
## =============================================================================

print_section("check_overlap (SL)", 2)


# --- Mid level: check_overlap with model="sl" ---
print_section("check_overlap with model='sl' (mid level)", 3)

run_test("check_overlap(model='sl') - complete separation", function(backend, solver) {
  check_overlap(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("check_overlap(model='sl') - quasi-complete separation", function(backend, solver) {
  check_overlap(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("check_overlap(model='sl') - overlap", function(backend, solver) {
  check_overlap(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

## =============================================================================
## 6.3 diagsep_worker - SL
## =============================================================================

print_section("diagsep_worker (SL)", 2)


# --- Mid level: diagsep_worker with model="sl" (continued) ---
run_test("diagsep_worker(model='sl') - complete separation", function(backend, solver) {
  diagsep_worker(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='sl') - quasi-complete separation", function(backend, solver) {
  diagsep_worker(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("diagsep_worker(model='sl') - overlap", function(backend, solver) {
  diagsep_worker(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

# --- print.sepmod for SL ---
print_section("print.sepmod (SL)", 3)

run_simple_test("print.sepmod - default (SL)", function() {
  sd1 <- diagsep_worker(y_sl_qcs, X_sl_qcs, model="sl", rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full (SL)", function() {
  sd1 <- diagsep_worker(y_sl_qcs, X_sl_qcs, model="sl", rational = rational)
  print(sd1, info = "full")
})

## =============================================================================
## 6.4 sepcols / sepcols_worker - SL
## =============================================================================

print_section("sepcols / sepcols_worker (SL)", 2)

# --- Mid level: sepcols_worker with model="sl" ---
print_section("sepcols_worker with model='sl' (mid level)", 3)

run_test("sepcols_worker(model='sl') - complete separation", function(backend, solver) {
  sepcols_worker(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("sepcols_worker(model='sl') - quasi-complete separation", function(backend, solver) {
  sepcols_worker(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("sepcols_worker(model='sl') - overlap", function(backend, solver) {
  sepcols_worker(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

## =============================================================================
## 6.5 seprows_worker - SL
## =============================================================================

print_section("seprows_worker (SL)", 2)


# --- Mid level: seprows_worker with model="sl" ---
print_section("seprows_worker with model='sl' (mid level)", 3)

run_simple_test("seprows_worker(model='sl') - complete separation", function(backend, solver) {
  seprows_worker(y_sl_cs, X_sl_cs, rational = rational, model = "sl")
})

run_simple_test("seprows_worker(model='sl') - quasi-complete separation", function(backend, solver) {
  seprows_worker(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl")
})

run_simple_test("seprows_worker(model='sl') - overlap", function(backend, solver) {
  seprows_worker(y_sl_ol, X_sl_ol, rational = rational, model = "sl")
})

## =============================================================================
## 6.6 linearities - SL
## =============================================================================

print_section("linearities (SL)", 2)


# --- Mid level: linearities with model="sl" ---
print_section("linearities with model='sl' (mid level)", 3)

run_simple_test("linearities(model='sl') - complete separation", function(backend, solver) {
  linearities(y_sl_cs, X_sl_cs, rational = rational, model = "sl")
})

run_simple_test("linearities(model='sl') - quasi-complete separation", function(backend, solver) {
  linearities(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl")
})

run_simple_test("linearities(model='sl') - overlap", function(backend, solver) {
  linearities(y_sl_ol, X_sl_ol, rational = rational, model = "sl")
})


## =============================================================================
## 6.7 reccone_worker / rec_cone - SL
## =============================================================================

print_section("reccone_worker / rec_cone (SL)", 2)


# --- Mid level: reccone_worker with model="sl" ---
print_section("reccone_worker with model='sl' (mid level)", 3)

run_simple_test("reccone_worker(model='sl') - complete separation", function(backend, solver) {
  reccone_worker(y_sl_cs, X_sl_cs, rational = rational, model = "sl")
})

run_simple_test("reccone_worker(model='sl') - quasi-complete separation", function(backend, solver) {
  reccone_worker(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl")
})

run_simple_test("reccone_worker(model='sl') - overlap", function(backend, solver) {
  reccone_worker(y_sl_ol, X_sl_ol, rational = rational, model = "sl")
})

## =============================================================================
## 6.8 overlap_fraction_check - SL
## =============================================================================

print_section("overlap_fraction_check (SL)", 2)


# --- Mid level: overlap_fraction_check with model="sl" ---
print_section("overlap_fraction_check with model='sl' (mid level)", 3)

run_test("overlap_fraction_check(model='sl') - complete separation", function(backend, solver) {
  overlap_fraction_check(y_sl_cs, X_sl_cs, frac = 1, verbose = 0, rational = rational, 
             model = "sl", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='sl') - quasi-complete separation", function(backend, solver) {
  overlap_fraction_check(y_sl_qcs, X_sl_qcs, frac = 1, verbose = 0, rational = rational, 
             model = "sl", backend = backend, solver = solver)
})

run_test("overlap_fraction_check(model='sl') - overlap", function(backend, solver) {
  overlap_fraction_check(y_sl_ol, X_sl_ol, frac = 1, verbose = 0, rational = rational, 
             model = "sl", backend = backend, solver = solver)
})

## =============================================================================
## 6.9 overlap_quick_check - SL
## =============================================================================

print_section("overlap_quick_check (SL)", 2)

# --- Mid level: overlap_quick_check with model="sl" ---
print_section("overlap_quick_check with model='sl' (mid level)", 3)

run_test("overlap_quick_check(model='sl') - complete separation", function(backend, solver) {
  overlap_quick_check(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='sl') - quasi-complete separation", function(backend, solver) {
  overlap_quick_check(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("overlap_quick_check(model='sl') - overlap", function(backend, solver) {
  overlap_quick_check(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

## =============================================================================
## 6.10 separation_quick_check - SL
## =============================================================================

print_section("separation_quick_check (SL)", 2)


# --- Mid level: separation_quick_check with model="sl" (continued) ---
run_test("separation_quick_check(model='sl') - complete separation", function(backend, solver) {
  separation_quick_check(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='sl') - quasi-complete separation", function(backend, solver) {
  separation_quick_check(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("separation_quick_check(model='sl') - overlap", function(backend, solver) {
  separation_quick_check(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

#####################
##  structure vectors
##############################

print_section("structure_vectors (SL)", 2)


run_simple_test("structure_vectors with label", function() {
  structure_vectors(y_cl_wine2, X_cl_wine2, model = "sl", rational = rational)
})

run_simple_test("structure_vectors without labels", function() {
  structure_vectors(y_cl_wine2, X_cl_wine2, model = "sl", rational = rational, label = FALSE)
})

run_simple_test("structure_vectors with label from formula", function() {
  structure_vectors( rating ~ temp + contact + bottle, data = wine, model = "sl", rational = rational)
})


cat("")
cat(paste(rep("#", 78), collapse = ""), "")
cat("##  END OF SL MODEL TESTS")
cat(paste(rep("#", 78), collapse = ""), "")

################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE - PART 8: FINAL SUMMARY AND CLEANUP              ##
##                                                                            ##
################################################################################

print_section("FINAL TEST SUMMARY")

## =============================================================================
## 8.1 Summary Statistics
## =============================================================================

cat("
")
cat("================================================================================
")
cat("                           TEST EXECUTION SUMMARY                               
")
cat("================================================================================
")
cat("
")

# Calculate summary statistics
total_tests <- test_results$passed + test_results$failed + test_results$skipped
pass_rate <- if (total_tests > 0) round(100 * test_results$passed / total_tests, 1) else 0

cat(sprintf("  Total tests executed:    %d
", total_tests))
cat(sprintf("  Passed:                  %d (%.1f%%)
", test_results$passed, pass_rate))
cat(sprintf("  Failed:                  %d
", test_results$failed))
cat(sprintf("  Skipped:                 %d
", test_results$skipped))
cat("
")

# Timing information
end_time <- Sys.time()
total_duration <- difftime(end_time, start_time, units = "mins")
cat(sprintf("  Total execution time:    %.2f minutes
", as.numeric(total_duration)))
cat("
")

## =============================================================================
## 8.3 Model Type Coverage Summary
## =============================================================================

cat("--------------------------------------------------------------------------------
")
cat("                         MODEL TYPE COVERAGE                                    
")
cat("--------------------------------------------------------------------------------
")
cat("
")

model_types <- c(
  "Binary (b)"                    = "glm with binomial family",
  "Baseline-Category Logit (bcl)" = "nnet::multinom, brglm2::brmultinom",
  "Cumulative Logit (cl)"         = "ordinal::clm, MASS::polr",
  "Adjacent-Category Logit (acl)" = "brglm2::bracl",
  "Ordered Stereotype Model (os)"= "clustord::osm",
  "Sequential Logit (sl)"         = "none"
)

for (model_name in names(model_types)) {
  cat(sprintf("  %-35s -> %s
", model_name, model_types[model_name]))
}
cat("
")

## =============================================================================
## 8.4 Function Coverage Summary
## =============================================================================

cat("--------------------------------------------------------------------------------
")
cat("                         FUNCTION COVERAGE                                      
")
cat("--------------------------------------------------------------------------------
")
cat("
")

functions_tested <- c(
  "Core Detection Functions" = c(
    "checksep_worker / checksep_worker_*",
    "check_overlap / check_overlap_*",
    "diagsep_worker / diagsep_worker_*"
  ),
  "Column/Row Analysis" = c(
    "sepcols_worker / sepcols_worker_*",
    "seprows_worker / seprows_worker_*",
    "linearities / linearities_*"
  ),
  "Cone Operations" = c(
    "reccone_worker / reccone_worker_*",
    "overlap_fraction_check",
    "overlap_quick_check",
    "separation_quick_check"
  ),
  "Generic S3 Methods" = c(
    "check_separation.*",
    "diagnose_separation.*",
    "separation_columns.*",
    "separation_rows.*",
    "recession_cone.*"
  ),
  "Utility Functions" = c(
    "print.sepmod",
    "*_Xstar functions"
  )
)

for (category in names(functions_tested)) {
  cat(sprintf("  %s:
", category))
  for (func in functions_tested[[category]]) {
    cat(sprintf("    - %s
", func))
  }
  cat("
")
}

## =============================================================================
## 8.5 Failed Tests Details
## =============================================================================

if (test_results$failed > 0) {
  cat("--------------------------------------------------------------------------------
")
  cat("                           FAILED TESTS DETAILS                                 
")
  cat("--------------------------------------------------------------------------------
")
  cat("
")
  
  # Filter failed tests from log
  failed_entries <- test_results$log[sapply(test_results$log, function(x) x$status == "FAIL")]
  
  for (i in seq_along(failed_entries)) {
    entry <- failed_entries[[i]]
    cat(sprintf("  [%d] %s
", i, entry$test_name))
    if (!is.null(entry$backend)) {
      cat(sprintf("      Backend: %s, Solver: %s
", entry$backend, entry$solver))
    }
    cat(sprintf("      Error: %s
", entry$error))
    cat("
")
  }
}

## =============================================================================
## 8.6 Skipped Tests Details
## =============================================================================

if (test_results$skipped > 0) {
  cat("--------------------------------------------------------------------------------
")
  cat("                          SKIPPED TESTS SUMMARY                                 
")
  cat("--------------------------------------------------------------------------------
")
  cat("
")
  
  # Filter skipped tests from log
  skipped_entries <- test_results$log[sapply(test_results$log, function(x) x$status == "SKIP")]
  
  # Group by reason
  skip_reasons <- table(sapply(skipped_entries, function(x) x$reason))
  
  cat("  Skip reasons:
")
  for (reason in names(skip_reasons)) {
    cat(sprintf("    - %s: %d tests
", reason, skip_reasons[reason]))
  }
  cat("
")
}

## ## =============================================================================
## ## 8.7 Export Results (Optional)
## ## =============================================================================

## print_section("Export Results", 2)

## # Create results data frame for export
## results_df <- do.call(rbind, lapply(test_results$log, function(x) {
##   data.frame(
##     test_name = x$test_name,
##     status = x$status,
##     backend = ifelse(is.null(x$backend), NA, x$backend),
##     solver = ifelse(is.null(x$solver), NA, x$solver),
##     duration = ifelse(is.null(x$duration), NA, x$duration),
##     error = ifelse(is.null(x$error), NA, x$error),
##     stringsAsFactors = FALSE
##   )
## }))

## # Save results to file
## results_file <- sprintf("test_results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
## tryCatch({
##   write.csv(results_df, results_file, row.names = FALSE)
##   cat(sprintf("  Results exported to: %s
## ", results_file))
## }, error = function(e) {
##   cat(sprintf("  Warning: Could not export results: %s
## ", e$message))
## })

## # Save detailed log as RDS
## log_file <- sprintf("test_log_%s.rds", format(Sys.time(), "%Y%m%d_%H%M%S"))
## tryCatch({
##   saveRDS(test_results, log_file)
##   cat(sprintf("  Detailed log saved to: %s
## ", log_file))
## }, error = function(e) {
##   cat(sprintf("  Warning: Could not save log: %s
## ", e$message))
## })

## =============================================================================
## 8.8 Final Status
## =============================================================================

cat("
")
cat("================================================================================
")
if (test_results$failed == 0) {
  cat("                     ALL TESTS PASSED SUCCESSFULLY!                            
")
} else {
  cat(sprintf("                     %d TEST(S) FAILED - REVIEW REQUIRED                     
", 
              test_results$failed))
}
cat("================================================================================
")
cat("
")

# Return exit code for CI/CD integration
if (test_results$failed > 0) {
  cat("Exiting with status 1 (failures detected)
")
  # quit(status = 1)  # Uncomment for CI/CD
} else {
  cat("Exiting with status 0 (all tests passed)
")
  # quit(status = 0)  # Uncomment for CI/CD
}

cat("
")
cat(paste(rep("#", 78), collapse = ""), "
")
cat("##  END OF COMPREHENSIVE TEST SUITE
")
cat(paste(rep("#", 78), collapse = ""), "
")
cat("
")


