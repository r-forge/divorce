################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE FOR divoRce PACKAGE                   ##
##                                                                            ##
################################################################################

# =============================================================================
# SETUP AND CONFIGURATION
# =============================================================================

library(divoRce)  # Adjust package name as needed
library(nnet)
library(MASS)
library(ordinal)
library(rcdd)
library(ROI)

## Set rational flag
rational <- FALSE

## Backend/solver combinations to test
backend_solver_combos <- list(
  list(backend = "rcdd", solver = NULL),
  list(backend = "rcdd", solver = "CrissCross"),
  list(backend = "ROI", solver = NULL),
  list(backend = "ROI", solver = "lpsolve"),
  list(backend = "ROI", solver = "highs")
)

## Test runner with all backend/solver combinations
run_test <- function(test_name, test_fn) {
  cat(" ", test_name, "\n", sep = "")
  
  for (combo in backend_solver_combos) {
    backend <- combo$backend
    solver <- combo$solver
    solver_str <- if (is.null(solver)) "default" else solver
    
   tryCatch({
    result <- test_fn(backend, solver)
    cat(sprintf("    [%s/%s] ✓ PASSED", backend, solver_str))
    cat("\n","Result:","\n")
    print(result)
    },
    error = function(e) { cat(sprintf("    [%s/%s] ✗ FAILED: %s", backend, solver_str, conditionMessage(e)))},
    warning = function(w) {cat(sprintf("    [%s/%s] ⚠ WARNING: %s", backend, solver_str, conditionMessage(w)))}
    )
  }
}

## Simple test runner (no backend/solver)
run_simple_test <- function(test_name, test_fn) {
  cat("", test_name, "\n", sep = "")
  tryCatch({
    result <- test_fn()
    cat("    ✓ PASSED")
    cat("\n","Result:","\n")
    print(result)
  }, error = function(e) {
    cat(sprintf("    ✗ FAILED: %s
", conditionMessage(e)))
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
csep_bcl <- multinom(y ~ x1 + x2, data = csepdatm, model = TRUE, trace = FALSE)
y_bcl_cs <- model.response(csep_bcl$model)
X_bcl_cs <- model.matrix(csep_bcl)
cat("✓ BCL: csepdatm (complete separation)")

data(qcsepdatm)
qcsep_bcl <- multinom(y ~ x1 + x2, data = qcsepdatm, trace = FALSE)
y_bcl_qcs <- qcsepdatm$y
X_bcl_qcs <- model.matrix(qcsep_bcl)
cat("✓ BCL: qcsepdatm (quasi-complete separation)")

data(ovldatm)
ovl_bcl <- multinom(y ~ x1 + x2, data = ovldatm, model = TRUE, trace = FALSE)
y_bcl_ol <- ovl_bcl$model$y
X_bcl_ol <- model.matrix(ovl_bcl)
cat("✓ BCL: ovldatm (overlap)")

data(Alligators)
allgm1 <- multinom(foodchoice ~ size + lake + sex, data = Alligators, trace = FALSE)
y_bcl_allig <- Alligators$foodchoice
X_bcl_allig <- model.matrix(allgm1)
cat("✓ BCL: Alligators (no separation)")

allgm2 <- multinom(foodchoice ~ size + lake * sex, data = Alligators, trace = FALSE)
y_bcl_allig2 <- Alligators$foodchoice
X_bcl_allig2 <- model.matrix(allgm2)
cat("✓ BCL: Alligators with interaction (quasi-complete separation)")

allgm3 <- brglm2::brmultinom(foodchoice ~ size + lake * sex, data = Alligators)
cat("✓ BCL: Alligators with interaction (quasi-complete separation)")

## CL / Ordinal Data
data(HDSS)
hdss_clm <- clm(WTSSHI ~ trustSHI * knowledge, data = HDSS)
hdss_polr <- MASS::polr(WTSSHI ~ trustSHI * knowledge, data = HDSS)
y_cl_hdss <- HDSS$WTSSHI
X_cl_hdss <- model.matrix(hdss_clm)$X
cat("✓ CL: HDSS")

data(wine, package = "ordinal")
wine_clm <- clm(rating ~ temp + contact, data = wine)
y_cl_wine <- wine$rating
X_cl_wine <- model.matrix(wine_clm)$X
cat("✓ CL: wine")

wine_clm2 <- clm(rating ~ temp + contact + bottle, data = wine)
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

## OSM Data
y_osm_qcs <- y_osm_wine <- as.ordered(wine$rating)
X_osm_qcs <- X_osm_wine <- model.matrix(~ temp * contact, data = wine)[, -1]
cat("✓ OSM: wine")

wine_osm <- clustord::osm(rating~ temp * contact, data = wine)

y_osm_ol <- as.ordered(ovldatm$y)
X_osm_ol <- as.matrix(ovldatm[, 2:ncol(ovldatm)])
cat("✓ OSM: ovldatm (overlap)")

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

# --- Lowest level: checksep_b ---
print_section("checksep_b (lowest level)", 3)

run_test("checksep_b - complete separation (endometrial)", function(backend, solver) {
  checksep_b(y_b_cs, X_b_cs, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_b - quasi-complete separation (nsduh)", function(backend, solver) {
  checksep_b(y_b_qcs, X_b_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_b - quasi-complete separation (Silvapulle)", function(backend, solver) {
  checksep_b(y_b_silv, X_b_silv, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_b - complete separation (titanic)", function(backend, solver) {
  checksep_b(y_b_tita, X_b_tita, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_b - overlap", function(backend, solver) {
  checksep_b(y_b_ol, X_b_ol, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: checksep with model="b" ---
print_section("checksep with model='b' (mid level)", 3)

run_test("checksep(model='b') - complete separation", function(backend, solver) {
  checksep(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("checksep(model='b') - quasi-complete separation", function(backend, solver) {
  checksep(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("checksep(model='b') - overlap", function(backend, solver) {
  checksep(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})

# --- Mid level: with S matrix ---
print_section("checksep with S matrix (mid level)", 3)

run_test("checksep(S=) - complete separation", function(backend, solver) {
  checksep(S = S_cs, rational = rational, backend = backend, solver = solver)
})

# --- Generic: check_separation.glm ---
print_section("check_separation.glm (generic)", 3)

run_test("check_separation.glm - complete separation (endometrial)", function(backend, solver) {
  check_separation(endo_glm, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.glm - quasi-complete separation (nsduh)", function(backend, solver) {
  check_separation(nsduh_glm, rational = rational, backend = backend, solver = solver)
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



###############
## checkovl
#################


print_section("checkovl (Binary)", 2)

# --- Mid level: checkovl with model="b" ---
print_section("checkovl with model='b' (mid level)", 3)

run_test("checkovl(model='b') - complete separation", function(backend, solver) {
  checkovl(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("checkovl(model='b') - quasi-complete separation", function(backend, solver) {
  checkovl(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("checkovl(model='b') - overlap", function(backend, solver) {
  checkovl(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("checkovl(S=) - complete separation", function(backend, solver) {
  checkovl(S = S_cs, rational = rational, backend=backend, solver=solver)
})


## =============================================================================
## 1.3 diagsep - Binary
## =============================================================================

print_section("diagsep (Binary)", 2)

# --- Lowest level: diagsep_b ---
print_section("diagsep_b (lowest level)", 3)

run_test("diagsep_b - complete separation", function(backend, solver) {
  diagsep_b(y_b_tita, X_b_tita, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_b - quasi-complete separation", function(backend, solver) {
  diagsep_b(y_b_qcs, X_b_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_b - overlap", function(backend, solver) {
  diagsep_b(y_b_ol, X_b_ol, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: diagsep with model="b" ---
print_section("diagsep with model='b' (mid level)", 3)

run_test("diagsep(model='b') - complete separation", function(backend, solver) {
  diagsep(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("diagsep(model='b') - quasi-complete separation", function(backend, solver) {
  diagsep(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("diagsep(model='b') - overlap", function(backend, solver) {
  diagsep(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})


run_test("diagsep(S=) - complete separation", function(backend, solver) {
  diagsep(S = S_cs, rational = rational, backend = backend, solver = solver)
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


# --- print.sepmod ---
print_section("print.sepmod (Binary)", 3)

run_simple_test("print.sepmod - default", function() {
  sd1 <- diagsep_b(y_b_qcs, X_b_qcs, rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full", function() {
  sd1 <- diagsep_b(y_b_qcs, X_b_qcs, rational = rational)
  print(sd1, info = "full")
})

## =============================================================================
## 1.4 sepcols / detect_sepcols - Binary
## =============================================================================

print_section("sepcols / detect_sepcols (Binary)", 2)

# --- Lowest level: detect_sepcols_b ---
print_section("detect_sepcols_b (lowest level)", 3)

run_test("detect_sepcols_b - complete separation", function(backend, solver) {
  detect_sepcols_b(y_b_cs, X_b_cs, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_b - quasi-complete separation", function(backend, solver) {
  detect_sepcols_b(y_b_qcs, X_b_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_b - overlap", function(backend, solver) {
  detect_sepcols_b(y_b_ol, X_b_ol, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: detect_sepcols with model="b" ---
print_section("detect_sepcols with model='b' (mid level)", 3)

run_test("detect_sepcols(model='b') - complete separation", function(backend, solver) {
  detect_sepcols(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
}) ##Whats going on here? ROI issue.

run_test("detect_sepcols(model='b') - quasi-complete separation", function(backend, solver) {
  detect_sepcols(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("detect_sepcols(model='b') - overlap", function(backend, solver) {
  detect_sepcols(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})


run_test("detect_sepcols (S=) - complete separation", function(backend, solver) {
  sepcols(S = S_cs, rational = rational, backend = backend, solver = solver)
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


## =============================================================================
## 1.5 seprows - Binary
## =============================================================================

print_section("seprows (Binary)", 2)

# --- Lowest level: seprows_b ---
print_section("seprows_b (lowest level)", 3)

run_simple_test("seprows_b - complete separation", function() {
  seprows_b(y_b_cs, X_b_cs, rational = rational)
})

run_simple_test("seprows_b - quasi-complete separation", function(backend, solver) {
  seprows_b(y_b_qcs, X_b_qcs, rational = rational)
})

run_simple_test("seprows_b - overlap", function(backend, solver) {
  seprows_b(y_b_ol, X_b_ol, rational = rational)
})

# --- Mid level: seprows with model="b" ---
print_section("seprows with model='b' (mid level)", 3)

run_simple_test("seprows(model='b') - complete separation", function(backend, solver) {
  seprows(y_b_cs, X_b_cs, rational = rational, model = "b")
})

run_simple_test("seprows(model='b') - quasi-complete separation", function(backend, solver) {
  seprows(y_b_qcs, X_b_qcs, rational = rational, model = "b")
})

run_simple_test("seprows(model='b') - overlap", function(backend, solver) {
  seprows(y_b_ol, X_b_ol, rational = rational, model = "b")
})

run_simple_test("seprows(S=) - complete separation", function(backend, solver) {
  seprows(S = S_qcs, rational = rational)
})

run_simple_test("seprows(S=) - overlap", function(backend, solver) {
  seprows(S = S_ol, rational = rational)
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

## =============================================================================
## 1.6 linearities - Binary
## =============================================================================

print_section("linearities (Binary)", 2)

# --- Lowest level: linearities_b ---
print_section("linearities_b (lowest level)", 3)

run_simple_test("linearities_b - complete separation", function(backend, solver) {
  linearities_b(y_b_cs, X_b_cs, rational = rational)
})

run_simple_test("linearities_b - quasi-complete separation", function(backend, solver) {
  linearities_b(y_b_qcs, X_b_qcs, rational = rational)
})

run_simple_test("linearities_b - overlap", function(backend, solver) {
  linearities_b(y_b_ol, X_b_ol, rational = rational)
})

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

print_section("reccone / rec_cone (Binary)", 2)

# --- Lowest level: reccone_b ---
print_section("reccone_b (lowest level)", 3)

run_simple_test("reccone_b - complete separation", function(backend, solver) {
  reccone_b(y_b_cs, X_b_cs, rational = rational)
})

run_simple_test("reccone_b - quasi-complete separation", function(backend, solver) {
  reccone_b(y_b_qcs, X_b_qcs, rational = rational)
})

run_simple_test("reccone_b - overlap", function(backend, solver) {
  reccone_b(y_b_ol, X_b_ol, rational = rational)
})

# --- Mid level: reccone with model="b" ---
print_section("reccone with model='b' (mid level)", 3)

run_simple_test("reccone(model='b') - complete separation", function(backend, solver) {
  reccone(y_b_cs, X_b_cs, rational = rational, model = "b")
})

run_simple_test("reccone(model='b') - quasi-complete separation", function(backend, solver) {
  reccone(y_b_qcs, X_b_qcs, rational = rational, model = "b")
})

run_simple_test("reccone(model='b') - overlap", function(backend, solver) {
  reccone(y_b_ol, X_b_ol, rational = rational, model = "b")
})

run_simple_test("reccone(S=) - complete separation", function(backend, solver) {
  reccone(S = S_cs, rational = rational)
})

run_simple_test("reccone(S=) - quasi-complete separation", function(backend, solver) {
  reccone(S = S_qcs, rational = rational)
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


## =============================================================================
## 1.8 overlap_fc - Binary
## =============================================================================

print_section("overlap_fc (Binary)", 2)


# --- Mid level: overlap_fc with model="b" ---
print_section("overlap_fc with model='b' (mid level)", 3)

run_test("overlap_fc(model='b') - complete separation", function(backend, solver) {
  overlap_fc(y_b_cs, X_b_cs, frac = 10, verbose = 0, rational = rational, 
             model = "b", backend = backend, solver = solver)
})

run_test("overlap_fc(model='b') - quasi-complete separation", function(backend, solver) {
  overlap_fc(y_b_qcs, X_b_qcs, frac = 10, verbose = 0, rational = rational, 
             model = "b", backend = backend, solver = solver)
})

run_test("overlap_fc(model='b') - overlap", function(backend, solver) {
  overlap_fc(y_b_ol, X_b_ol, frac = 10, verbose = 0, rational = rational, 
             model = "b", backend = backend, solver = solver)
})

# --- Mid level: overlap_fc default (no model specified) ---
print_section("overlap_fc default (mid level)", 3)

run_test("overlap_fc - complete separation", function(backend, solver) {
  overlap_fc(y_b_cs, X_b_cs, frac = 10, verbose = 0, rational = rational, 
             backend = backend, solver = solver)
})

run_test("overlap_fc - quasi-complete separation", function(backend, solver) {
  overlap_fc(y_b_qcs, X_b_qcs, frac = 10, verbose = 0, rational = rational, 
             backend = backend, solver = solver)
})

run_test("overlap_fc(S) - quasi-complete separation", function(backend, solver) {
  overlap_fc(S_qcs, frac = 10, verbose = 0, rational = rational, 
             backend = backend, solver = solver)
})

## =============================================================================
## 1.9 overlap_qc - Binary
## =============================================================================

print_section("overlap_qc (Binary)", 2)


# --- Mid level: overlap_qc with model="b" ---
print_section("overlap_qc with model='b' (mid level)", 3)

run_test("overlap_qc(model='b') - complete separation", function(backend, solver) {
  overlap_qc(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("overlap_qc(model='b') - quasi-complete separation", function(backend, solver) {
  overlap_qc(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("overlap_qc(model='b') - overlap", function(backend, solver) {
  overlap_qc(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})

# --- Mid level: overlap_qc default ---
print_section("overlap_qc default (mid level)", 3)

run_test("overlap_qc - complete separation", function(backend, solver) {
  overlap_qc(y_b_cs, X_b_cs, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_qc - quasi-complete separation", function(backend, solver) {
  overlap_qc(y_b_qcs, X_b_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_qc - overlap", function(backend, solver) {
  overlap_qc(y_b_ol, X_b_ol, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_qc(S) - quasi-complete separation", function(backend, solver) {
  overlap_qc(S_qcs, frac = 10, verbose = 0, rational = rational, 
             backend = backend, solver = solver)
})

## =============================================================================
## 1.10 separation_qc - Binary
## =============================================================================

print_section("separation_qc (Binary)", 2)


# --- Mid level: separation_qc with model="b" ---
print_section("separation_qc with model='b' (mid level)", 3)

run_test("separation_qc(model='b') - complete separation", function(backend, solver) {
  separation_qc(y_b_cs, X_b_cs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("separation_qc(model='b') - quasi-complete separation", function(backend, solver) {
  separation_qc(y_b_qcs, X_b_qcs, rational = rational, model = "b", backend = backend, solver = solver)
})

run_test("separation_qc(model='b') - overlap", function(backend, solver) {
  separation_qc(y_b_ol, X_b_ol, rational = rational, model = "b", backend = backend, solver = solver)
})

# --- Mid level: separation_qc default ---
print_section("separation_qc default (mid level)", 3)

run_test("separation_qc - complete separation", function(backend, solver) {
  separation_qc(y_b_cs, X_b_cs, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc - quasi-complete separation", function(backend, solver) {
  separation_qc(y_b_qcs, X_b_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc - overlap", function(backend, solver) {
  separation_qc(y_b_ol, X_b_ol, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc(S) - quasi-complete separation", function(backend, solver) {
  separation_qc(S_qcs, frac = 10, verbose = 0, rational = rational, 
             backend = backend, solver = solver)
})

cat("")
cat(paste(rep("#", 78), collapse = ""), "")
cat("##  END OF BINARY MODEL TESTS")
cat(paste(rep("#", 78), collapse = ""), "")


## TODO: Update so that warning is cool.

################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE - PART 2: BCL MODEL TESTS                       ##             
##                                                                            ##
################################################################################

print_section("Baseline-Category Logit Model Tests (bcl)")

## =============================================================================
## 2.1 checksep - BCL
## =============================================================================

print_section("checksep (BCL)", 2)

# --- Lowest level: checksep_bcl ---
print_section("checksep_bcl (lowest level)", 3)

run_test("checksep_bcl - complete separation (csepdatm)", function(backend, solver) {
  checksep_bcl(y_bcl_cs, X_bcl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_bcl - quasi-complete separation (qcsepdatm)", function(backend, solver) {
  checksep_bcl(y_bcl_qcs, X_bcl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_bcl - overlap (ovldatm)", function(backend, solver) {
  checksep_bcl(y_bcl_ol, X_bcl_ol, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_bcl - no separation (Alligators)", function(backend, solver) {
  checksep_bcl(y_bcl_allig, X_bcl_allig, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_bcl - quasi-complete separation (Alligators interaction)", function(backend, solver) {
  checksep_bcl(y_bcl_allig2, X_bcl_allig2, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: checksep with model="bcl" ---
print_section("checksep with model='bcl' (mid level)", 3)

run_test("checksep(model='bcl') - complete separation", function(backend, solver) {
  checksep(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("checksep(model='bcl') - quasi-complete separation", function(backend, solver) {
  checksep(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("checksep(model='bcl') - overlap", function(backend, solver) {
  checksep(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
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

run_test("check_separation.brmultinom - quasi-complete (Alligators interaction)", function(backend, solver) {
  check_separation(allgm3, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.formula ", function(backend, solver) {
  check_separation(y ~ x1 + x2, data = qcsepdatm, model="bcl", rational = rational, backend = backend, solver = solver)
})


###########
### checkovl                                       
#################

print_section("checkovl (BCL)", 2)

# --- Mid level: checkovl with model="b" ---
print_section("checkovl with model='bcl' (mid level)", 3)

run_test("checkovl(model='bcl') - complete separation", function(backend, solver) {
  checkovl(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("checkovl(model='bcl') - quasi-complete separation", function(backend, solver) {
  checkovl(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("checkovl(model='bcl') - overlap", function(backend, solver) {
  checkovl(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
})



## =============================================================================
## 2.3 diagsep - BCL
## =============================================================================

print_section("diagsep (BCL)", 2)

# --- Lowest level: diagsep_bcl ---
print_section("diagsep_bcl (lowest level)", 3)

run_test("diagsep_bcl - complete separation", function(backend, solver) {
  diagsep_bcl(y_bcl_cs, X_bcl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_bcl - quasi-complete separation", function(backend, solver) {
  diagsep_bcl(y_bcl_qcs, X_bcl_qcs, rational = rational, backend = backend, solver = solver)
})
 
run_test("diagsep_bcl - overlap", function(backend, solver) {
  diagsep_bcl(y_bcl_ol, X_bcl_ol, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_bcl - no separation (Alligators)", function(backend, solver) {
  diagsep_bcl(y_bcl_allig, X_bcl_allig, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_bcl - quasi-complete (Alligators interaction)", function(backend, solver) {
  diagsep_bcl(y_bcl_allig2, X_bcl_allig2, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: diagsep with model="bcl" ---
print_section("diagsep with model='bcl' (mid level)", 3)

run_test("diagsep(model='bcl') - complete separation", function(backend, solver) {
  diagsep(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("diagsep(model='bcl') - quasi-complete separation", function(backend, solver) {
  diagsep(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("diagsep(model='bcl') - overlap", function(backend, solver) {
  diagsep(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
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
  sd1 <- diagsep_bcl(y_bcl_qcs, X_bcl_qcs, rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full (BCL)", function() {
  sd1 <- diagsep_bcl(y_bcl_qcs, X_bcl_qcs, rational = rational)
  print(sd1, info = "full")
})

## =============================================================================
## 2.4 sepcols / detect_sepcols - BCL
## =============================================================================

print_section("sepcols / detect_sepcols (BCL)", 2)

# --- Lowest level: detect_sepcols_bcl ---
print_section("detect_sepcols_bcl (lowest level)", 3)

run_test("detect_sepcols_bcl - complete separation", function(backend, solver) {
  detect_sepcols_bcl(y_bcl_cs, X_bcl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_bcl - quasi-complete separation", function(backend, solver) {
  detect_sepcols_bcl(y_bcl_qcs, X_bcl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_bcl - overlap", function(backend, solver) {
  detect_sepcols_bcl(y_bcl_ol, X_bcl_ol, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_bcl - no separation (Alligators)", function(backend, solver) {
  detect_sepcols_bcl(y_bcl_allig, X_bcl_allig, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_bcl - quasi-complete (Alligators interaction)", function(backend, solver) {
  detect_sepcols_bcl(y_bcl_allig2, X_bcl_allig2, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: detect_sepcols with model="bcl" ---
print_section("detect_sepcols with model='bcl' (mid level)", 3)

run_test("detect_sepcols(model='bcl') - complete separation", function(backend, solver) {
  detect_sepcols(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("detect_sepcols(model='bcl') - quasi-complete separation", function(backend, solver) {
  detect_sepcols(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("detect_sepcols(model='bcl') - overlap", function(backend, solver) {
  detect_sepcols(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
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
  separation_columns(allgm1, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.formula ", function(backend, solver) {
  separation_columns(y ~ x1 + x2, data = qcsepdatm, model="bcl", rational = rational, backend = backend, solver = solver)
})

## =============================================================================
## 2.5 seprows - BCL
## =============================================================================

print_section("seprows (BCL)", 2)

# --- Lowest level: seprows_bcl ---
print_section("seprows_bcl (lowest level)", 3)

run_simple_test("seprows_bcl - complete separation", function(backend, solver) {
  seprows_bcl(y_bcl_cs, X_bcl_cs, rational = rational)
})

run_simple_test("seprows_bcl - quasi-complete separation", function(backend, solver) {
  seprows_bcl(y_bcl_qcs, X_bcl_qcs, rational = rational)
})

run_simple_test("seprows_bcl - overlap", function(backend, solver) {
  seprows_bcl(y_bcl_ol, X_bcl_ol, rational = rational)
})

run_simple_test("seprows_bcl - no separation (Alligators)", function(backend, solver) {
  seprows_bcl(y_bcl_allig, X_bcl_allig, rational = rational)
})

run_simple_test("seprows_bcl - quasi-complete (Alligators interaction)", function(backend, solver) {
  seprows_bcl(y_bcl_allig2, X_bcl_allig2, rational = rational)
})

# --- Mid level: seprows with model="bcl" ---
print_section("seprows with model='bcl' (mid level)", 3)

run_simple_test("seprows(model='bcl') - complete separation", function(backend, solver) {
  seprows(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl")
})

run_simple_test("seprows(model='bcl') - quasi-complete separation", function(backend, solver) {
  seprows(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl")
})

run_simple_test("seprows(model='bcl') - overlap", function(backend, solver) {
  seprows(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl")
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
  separation_rows(y ~ x1 + x2, data = qcsepdatm, model="bcl", rational = rational, backend = backend, solver = solver)
})

## =============================================================================
## 2.6 linearities - BCL
## =============================================================================

print_section("linearities (BCL)", 2)

# --- Lowest level: linearities_bcl ---
print_section("linearities_bcl (lowest level)", 3)

run_simple_test("linearities_bcl - complete separation", function(backend, solver) {
  linearities_bcl(y_bcl_cs, X_bcl_cs, rational = rational)
})

run_simple_test("linearities_bcl - quasi-complete separation", function(backend, solver) {
  linearities_bcl(y_bcl_qcs, X_bcl_qcs, rational = rational)
})

run_simple_test("linearities_bcl - overlap", function(backend, solver) {
  linearities_bcl(y_bcl_ol, X_bcl_ol, rational = rational)
})

run_simple_test("linearities_bcl - no separation (Alligators)", function(backend, solver) {
  linearities_bcl(y_bcl_allig, X_bcl_allig, rational = rational)
})

run_simple_test("linearities_bcl - quasi-complete (Alligators interaction)", function(backend, solver) {
  linearities_bcl(y_bcl_allig2, X_bcl_allig2, rational = rational)
})

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
## 2.7 reccone / rec_cone - BCL
## =============================================================================

print_section("reccone / rec_cone (BCL)", 2)

# --- Lowest level: reccone_bcl ---
print_section("reccone_bcl (lowest level)", 3)

run_simple_test("reccone_bcl - complete separation", function(backend, solver) {
  reccone_bcl(y_bcl_cs, X_bcl_cs, rational = rational)
})

run_simple_test("reccone_bcl - quasi-complete separation", function(backend, solver) {
  reccone_bcl(y_bcl_qcs, X_bcl_qcs, rational = rational)
})

run_simple_test("reccone_bcl - overlap", function(backend, solver) {
  reccone_bcl(y_bcl_ol, X_bcl_ol, rational = rational)
})

run_simple_test("reccone_bcl - no separation (Alligators)", function(backend, solver) {
  reccone_bcl(y_bcl_allig, X_bcl_allig, rational = rational)
})

run_simple_test("reccone_bcl - quasi-complete (Alligators interaction)", function(backend, solver) {
  reccone_bcl(y_bcl_allig2, X_bcl_allig2, rational = rational)
})

# --- Mid level: reccone with model="bcl" ---
print_section("reccone with model='bcl' (mid level)", 3)

run_simple_test("reccone(model='bcl') - complete separation", function(backend, solver) {
  reccone(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl")
})

run_simple_test("reccone(model='bcl') - quasi-complete separation", function(backend, solver) {
  reccone(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl")
})

run_simple_test("reccone(model='bcl') - overlap", function(backend, solver) {
  reccone(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl")
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
  recession_cone(y ~ x1 + x2, data = qcsepdatm, model="bcl", rational = rational, backend = backend, solver = solver)
})

## =============================================================================
## 2.8 overlap_fc - BCL
## =============================================================================

print_section("overlap_fc (BCL)", 2)

# --- Lowest level: overlap_fc_bcl ---
print_section("overlap_fc_bcl (lowest level)", 3)

run_test("overlap_fc_bcl - complete separation (frac=1)", function(backend, solver) {
  overlap_fc_bcl(y_bcl_cs, X_bcl_cs, frac = 1, verbose = 0, rational = rational, 
                 backend = backend, solver = solver)
})

run_test("overlap_fc_bcl - quasi-complete separation (frac=1)", function(backend, solver) {
  overlap_fc_bcl(y_bcl_qcs, X_bcl_qcs, frac = 1, verbose = 0, rational = rational, 
                 backend = backend, solver = solver)
})

run_test("overlap_fc_bcl - quasi-complete separation (frac=5)", function(backend, solver) {
  overlap_fc_bcl(y_bcl_qcs, X_bcl_qcs, frac = 5, verbose = 0, rational = rational, 
                 backend = backend, solver = solver)
})

run_test("overlap_fc_bcl - overlap (frac=1)", function(backend, solver) {
  overlap_fc_bcl(y_bcl_ol, X_bcl_ol, frac = 1, verbose = 0, rational = rational, 
                 backend = backend, solver = solver)
})

run_test("overlap_fc_bcl - no separation (Alligators, frac=1)", function(backend, solver) {
  overlap_fc_bcl(y_bcl_allig, X_bcl_allig, frac = 1, verbose = 0, rational = rational, 
                 backend = backend, solver = solver)
})

run_test("overlap_fc_bcl - no separation (Alligators, frac=3)", function(backend, solver) {
  overlap_fc_bcl(y_bcl_allig, X_bcl_allig, frac = 3, verbose = 0, rational = rational, 
                 backend = backend, solver = solver)
})

# --- Mid level: overlap_fc with model="bcl" ---
print_section("overlap_fc with model='bcl' (mid level)", 3)

run_test("overlap_fc(model='bcl') - complete separation", function(backend, solver) {
  overlap_fc(y_bcl_cs, X_bcl_cs, frac = 1, verbose = 0, rational = rational, 
             model = "bcl", backend = backend, solver = solver)
})

run_test("overlap_fc(model='bcl') - quasi-complete separation", function(backend, solver) {
  overlap_fc(y_bcl_qcs, X_bcl_qcs, frac = 1, verbose = 0, rational = rational, 
             model = "bcl", backend = backend, solver = solver)
})

run_test("overlap_fc(model='bcl') - overlap", function(backend, solver) {
  overlap_fc(y_bcl_ol, X_bcl_ol, frac = 1, verbose = 0, rational = rational, 
             model = "bcl", backend = backend, solver = solver)
})

## =============================================================================
## 2.9 overlap_qc - BCL
## =============================================================================

print_section("overlap_qc (BCL)", 2)

# --- Lowest level: overlap_qc_bcl ---
print_section("overlap_qc_bcl (lowest level)", 3)

run_test("overlap_qc_bcl - complete separation", function(backend, solver) {
  overlap_qc_bcl(y_bcl_cs, X_bcl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_qc_bcl - quasi-complete separation", function(backend, solver) {
  overlap_qc_bcl(y_bcl_qcs, X_bcl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_qc_bcl - overlap", function(backend, solver) {
  overlap_qc_bcl(y_bcl_ol, X_bcl_ol, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_qc_bcl - no separation (Alligators)", function(backend, solver) {
  overlap_qc_bcl(y_bcl_allig, X_bcl_allig, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: overlap_qc with model="bcl" ---
print_section("overlap_qc with model='bcl' (mid level)", 3)

run_test("overlap_qc(model='bcl') - complete separation", function(backend, solver) {
  overlap_qc(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("overlap_qc(model='bcl') - quasi-complete separation", function(backend, solver) {
  overlap_qc(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("overlap_qc(model='bcl') - overlap", function(backend, solver) {
  overlap_qc(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
})


## =============================================================================
## 2.10 separation_qc - BCL
## =============================================================================

print_section("separation_qc (BCL)", 2)

# --- Lowest level: separation_qc_bcl ---
print_section("separation_qc_bcl (lowest level)", 3)

run_test("separation_qc_bcl - complete separation", function(backend, solver) {
  separation_qc_bcl(y_bcl_cs, X_bcl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc_bcl - quasi-complete separation", function(backend, solver) {
  separation_qc_bcl(y_bcl_qcs, X_bcl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc_bcl - overlap", function(backend, solver) {
  separation_qc_bcl(y_bcl_ol, X_bcl_ol, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc_bcl - no separation (Alligators)", function(backend, solver) {
  separation_qc_bcl(y_bcl_allig, X_bcl_allig, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc_bcl - quasi-complete (Alligators interaction)", function(backend, solver) {
  separation_qc_bcl(y_bcl_allig2, X_bcl_allig2, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: separation_qc with model="bcl" ---
print_section("separation_qc with model='bcl' (mid level)", 3)

run_test("separation_qc(model='bcl') - complete separation", function(backend, solver) {
  separation_qc(y_bcl_cs, X_bcl_cs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("separation_qc(model='bcl') - quasi-complete separation", function(backend, solver) {
  separation_qc(y_bcl_qcs, X_bcl_qcs, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("separation_qc(model='bcl') - overlap", function(backend, solver) {
  separation_qc(y_bcl_ol, X_bcl_ol, rational = rational, model = "bcl", backend = backend, solver = solver)
})

run_test("separation_qc(model='bcl') - no separation (Alligators)", function(backend, solver) {
  separation_qc(y_bcl_allig, X_bcl_allig, rational = rational, model = "bcl", backend = backend, solver = solver)
})

# --- Mid level: separation_qc default ---
print_section("separation_qc default (mid level)", 3)

run_test("separation_qc - complete separation", function(backend, solver) {
  separation_qc(y_bcl_cs, X_bcl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc - quasi-complete separation", function(backend, solver) {
  separation_qc(y_bcl_qcs, X_bcl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc - overlap", function(backend, solver) {
  separation_qc(y_bcl_ol, X_bcl_ol, rational = rational, backend = backend, solver = solver)
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
## 3.1 checksep - CL
## =============================================================================

print_section("checksep (CL)", 2)

# --- Lowest level: checksep_cl ---
print_section("checksep_cl (lowest level)", 3)

run_test("checksep_cl - HDSS", function(backend, solver) {
  checksep_cl(y_cl_hdss, X_cl_hdss, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_cl - wine", function(backend, solver) {
  checksep_cl(y_cl_wine, X_cl_wine, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_cl - wine with bottle (singularities)", function(backend, solver) {
  checksep_cl(y_cl_wine2, X_cl_wine2, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: checksep with model="cl" ---
print_section("checksep with model='cl' (mid level)", 3)

run_test("checksep(model='cl') - HDSS", function(backend, solver) {
  checksep(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("checksep(model='cl') - wine", function(backend, solver) {
  checksep(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("checksep(model='cl') - wine with bottle", function(backend, solver) {
  checksep(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
})

# --- Generic: check_separation.clm ---
print_section("check_separation.clm (generic)", 3)

run_test("check_separation.clm - HDSS", function(backend, solver) {
  check_separation(hdss_clm, rational = rational, backend = backend, solver = solver)
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

## =============================================================================
## 3.2 checkovl - CL
## =============================================================================

print_section("checkovl (CL)", 2)

# --- Mid level: checkovl with model="cl" ---
print_section("checkovl with model='cl' (mid level)", 3)

run_test("checkovl(model='cl') - HDSS", function(backend, solver) {
  checkovl(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("checkovl(model='cl') - wine", function(backend, solver) {
  checkovl(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("checkovl(model='cl') - wine with bottle", function(backend, solver) {
  checkovl(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
})

## =============================================================================
## 3.3 diagsep - CL
## =============================================================================

print_section("diagsep (CL)", 2)

# --- Lowest level: diagsep_cl ---
print_section("diagsep_cl (lowest level)", 3)

run_test("diagsep_cl - HDSS", function(backend, solver) {
  diagsep_cl(y_cl_hdss, X_cl_hdss, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_cl - wine", function(backend, solver) {
  diagsep_cl(y_cl_wine, X_cl_wine, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_cl - wine with bottle (singularities)", function(backend, solver) {
  diagsep_cl(y_cl_wine2, X_cl_wine2, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: diagsep with model="cl" ---
print_section("diagsep with model='cl' (mid level)", 3)

run_test("diagsep(model='cl') - HDSS", function(backend, solver) {
  diagsep(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("diagsep(model='cl') - wine", function(backend, solver) {
  diagsep(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("diagsep(model='cl') - wine with bottle", function(backend, solver) {
  diagsep(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
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
  sd1 <- diagsep_cl(y_cl_hdss, X_cl_hdss, rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full (CL)", function() {
  sd1 <- diagsep_cl(y_cl_hdss, X_cl_hdss, rational = rational)
  print(sd1, info = "full")
})

## =============================================================================
## 3.4 sepcols / detect_sepcols - CL
## =============================================================================

print_section("sepcols / detect_sepcols (CL)", 2)

# --- Lowest level: detect_sepcols_cl ---
print_section("detect_sepcols_cl (lowest level)", 3)

run_test("detect_sepcols_cl - HDSS", function(backend, solver) {
  detect_sepcols_cl(y_cl_hdss, X_cl_hdss, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_cl - wine", function(backend, solver) {
  detect_sepcols_cl(y_cl_wine, X_cl_wine, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_cl - wine with bottle (singularities)", function(backend, solver) {
  detect_sepcols_cl(y_cl_wine2, X_cl_wine2, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: detect_sepcols with model="cl" ---
print_section("detect_sepcols with model='cl' (mid level)", 3)

run_test("detect_sepcols(model='cl') - HDSS", function(backend, solver) {
  detect_sepcols(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("detect_sepcols(model='cl') - wine", function(backend, solver) {
  detect_sepcols(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("detect_sepcols(model='cl') - wine with bottle", function(backend, solver) {
  detect_sepcols(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
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
## 3.5 seprows - CL
## =============================================================================

print_section("seprows (CL)", 2)

# --- Lowest level: seprows_cl ---
print_section("seprows_cl (lowest level)", 3)

run_simple_test("seprows_cl - HDSS", function(backend, solver) {
  seprows_cl(y_cl_hdss, X_cl_hdss, rational = rational)
})

run_simple_test("seprows_cl - wine", function(backend, solver) {
  seprows_cl(y_cl_wine, X_cl_wine, rational = rational)
})

run_simple_test("seprows_cl - wine with bottle", function(backend, solver) {
  seprows_cl(y_cl_wine2, X_cl_wine2, rational = rational)
})

# --- Mid level: seprows with model="cl" ---
print_section("seprows with model='cl' (mid level)", 3)

run_simple_test("seprows(model='cl') - HDSS", function(backend, solver) {
  seprows(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl")
})

run_simple_test("seprows(model='cl') - wine", function(backend, solver) {
  seprows(y_cl_wine, X_cl_wine, rational = rational, model = "cl")
})

run_simple_test("seprows(model='cl') - wine with bottle", function(backend, solver) {
  seprows(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl")
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

# --- Lowest level: linearities_cl ---
print_section("linearities_cl (lowest level)", 3)

run_simple_test("linearities_cl - HDSS", function(backend, solver) {
  linearities_cl(y_cl_hdss, X_cl_hdss, rational = rational)
})

run_simple_test("linearities_cl - wine", function(backend, solver) {
  linearities_cl(y_cl_wine, X_cl_wine, rational = rational)
})

run_simple_test("linearities_cl - wine with bottle", function(backend, solver) {
  linearities_cl(y_cl_wine2, X_cl_wine2, rational = rational)
})

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
## 3.7 reccone / rec_cone - CL
## =============================================================================

print_section("reccone / rec_cone (CL)", 2)

# --- Lowest level: reccone_cl ---
print_section("reccone_cl (lowest level)", 3)

run_simple_test("reccone_cl - HDSS", function(backend, solver) {
  reccone_cl(y_cl_hdss, X_cl_hdss, rational = rational)
})

run_simple_test("reccone_cl - wine", function(backend, solver) {
  reccone_cl(y_cl_wine, X_cl_wine, rational = rational)
})

run_simple_test("reccone_cl - wine with bottle", function(backend, solver) {
  reccone_cl(y_cl_wine2, X_cl_wine2, rational = rational)
})

# --- Mid level: reccone with model="cl" ---
print_section("reccone with model='cl' (mid level)", 3)

run_simple_test("reccone(model='cl') - HDSS", function(backend, solver) {
  reccone(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl")
})

run_simple_test("reccone(model='cl') - wine", function(backend, solver) {
  reccone(y_cl_wine, X_cl_wine, rational = rational, model = "cl")
})

run_simple_test("reccone(model='cl') - wine with bottle", function(backend, solver) {
  reccone(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl")
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
## 3.8 overlap_fc - CL
## =============================================================================

print_section("overlap_fc (CL)", 2)

# --- Lowest level: overlap_fc_cl ---
print_section("overlap_fc_cl (lowest level)", 3)

run_test("overlap_fc_cl - HDSS (frac=1)", function(backend, solver) {
  overlap_fc_cl(y_cl_hdss, X_cl_hdss, frac = 1, verbose = 0, rational = rational, 
                backend = backend, solver = solver)
})

run_test("overlap_fc_cl - HDSS (frac=3)", function(backend, solver) {
  overlap_fc_cl(y_cl_hdss, X_cl_hdss, frac = 3, verbose = 0, rational = rational, 
                backend = backend, solver = solver)
})

run_test("overlap_fc_cl - wine (frac=1)", function(backend, solver) {
  overlap_fc_cl(y_cl_wine, X_cl_wine, frac = 1, verbose = 0, rational = rational, 
                backend = backend, solver = solver)
})

run_test("overlap_fc_cl - wine with bottle (frac=1)", function(backend, solver) {
  overlap_fc_cl(y_cl_wine2, X_cl_wine2, frac = 1, verbose = 0, rational = rational, 
                backend = backend, solver = solver)
})

# --- Mid level: overlap_fc with model="cl" ---
print_section("overlap_fc with model='cl' (mid level)", 3)

run_test("overlap_fc(model='cl') - HDSS", function(backend, solver) {
  overlap_fc(y_cl_hdss, X_cl_hdss, frac = 1, verbose = 0, rational = rational, 
             model = "cl", backend = backend, solver = solver)
})

run_test("overlap_fc(model='cl') - wine", function(backend, solver) {
  overlap_fc(y_cl_wine, X_cl_wine, frac = 1, verbose = 0, rational = rational, 
             model = "cl", backend = backend, solver = solver)
})

run_test("overlap_fc(model='cl') - wine with bottle", function(backend, solver) {
  overlap_fc(y_cl_wine2, X_cl_wine2, frac = 1, verbose = 0, rational = rational, 
             model = "cl", backend = backend, solver = solver)
})

## =============================================================================
## 3.9 overlap_qc - CL
## =============================================================================

print_section("overlap_qc (CL)", 2)

# --- Lowest level: overlap_qc_cl ---
print_section("overlap_qc_cl (lowest level)", 3)

run_test("overlap_qc_cl - HDSS", function(backend, solver) {
  overlap_qc_cl(y_cl_hdss, X_cl_hdss, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_qc_cl - wine", function(backend, solver) {
  overlap_qc_cl(y_cl_wine, X_cl_wine, rational = rational, backend = backend, solver = solver)
})

run_test("overlap_qc_cl - wine with bottle", function(backend, solver) {
  overlap_qc_cl(y_cl_wine2, X_cl_wine2, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: overlap_qc with model="cl" ---
print_section("overlap_qc with model='cl' (mid level)", 3)

run_test("overlap_qc(model='cl') - HDSS", function(backend, solver) {
  overlap_qc(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("overlap_qc(model='cl') - wine", function(backend, solver) {
  overlap_qc(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("overlap_qc(model='cl') - wine with bottle", function(backend, solver) {
  overlap_qc(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
})

## =============================================================================
## 3.10 separation_qc - CL
## =============================================================================

print_section("separation_qc (CL)", 2)

# --- Lowest level: separation_qc_cl ---
print_section("separation_qc_cl (lowest level)", 3)

run_test("separation_qc_cl - HDSS", function(backend, solver) {
  separation_qc_cl(y_cl_hdss, X_cl_hdss, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc_cl - wine", function(backend, solver) {
  separation_qc_cl(y_cl_wine, X_cl_wine, rational = rational, backend = backend, solver = solver)
})

run_test("separation_qc_cl - wine with bottle", function(backend, solver) {
  separation_qc_cl(y_cl_wine2, X_cl_wine2, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: separation_qc with model="cl" ---
print_section("separation_qc with model='cl' (mid level)", 3)

run_test("separation_qc(model='cl') - HDSS", function(backend, solver) {
  separation_qc(y_cl_hdss, X_cl_hdss, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("separation_qc(model='cl') - wine", function(backend, solver) {
  separation_qc(y_cl_wine, X_cl_wine, rational = rational, model = "cl", backend = backend, solver = solver)
})

run_test("separation_qc(model='cl') - wine with bottle", function(backend, solver) {
  separation_qc(y_cl_wine2, X_cl_wine2, rational = rational, model = "cl", backend = backend, solver = solver)
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
## 4.1 checksep - ACL
## =============================================================================

print_section("checksep (ACL)", 2)

# --- Lowest level: checksep_acl ---
print_section("checksep_acl (lowest level)", 3)

run_test("checksep_acl - complete separation", function(backend, solver) {
  checksep_acl(y_acl_cs, X_acl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_acl - quasi-complete separation", function(backend, solver) {
  checksep_acl(y_acl_qcs, X_acl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_acl - overlap", function(backend, solver) {
  checksep_acl(y_acl_ol, X_acl_ol, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: checksep with model="acl" ---
print_section("checksep with model='acl' (mid level)", 3)

run_test("checksep(model='acl') - complete separation", function(backend, solver) {
  checksep(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("checksep(model='acl') - quasi-complete separation", function(backend, solver) {
  checksep(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("checksep(model='acl') - overlap", function(backend, solver) {
  checksep(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})

### generic
run_test("check_separation.bracl - quasi-complete", function(backend, solver) {
  check_separation(hdss_pacl, rational = rational, backend = backend, solver = solver)
})

run_test("check_separation.bracl - quasi-complete ", function(backend, solver) {
  check_separation(hdss_npacl, rational = rational, backend = backend, solver = solver)
})

## =============================================================================
## 4.2 checkovl - ACL
## =============================================================================

print_section("checkovl (ACL)", 2)



# --- Mid level: checkovl with model="acl" ---
print_section("checkovl with model='acl' (mid level)", 3)

run_test("checkovl(model='acl') - complete separation", function(backend, solver) {
  checkovl(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("checkovl(model='acl') - quasi-complete separation", function(backend, solver) {
  checkovl(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("checkovl(model='acl') - overlap", function(backend, solver) {
  checkovl(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})

## =============================================================================
## 4.3 diagsep - ACL
## =============================================================================

print_section("diagsep (ACL)", 2)

# --- Lowest level: diagsep_acl ---
print_section("diagsep_acl (lowest level)", 3)

run_test("diagsep_acl - complete separation", function(backend, solver) {
  diagsep_acl(y_acl_cs, X_acl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_acl - quasi-complete separation", function(backend, solver) {
  diagsep_acl(y_acl_qcs, X_acl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_acl - overlap", function(backend, solver) {
  diagsep_acl(y_acl_ol, X_acl_ol, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: diagsep with model="acl" ---
print_section("diagsep with model='acl' (mid level)", 3)

run_test("diagsep(model='acl') - complete separation", function(backend, solver) {
  diagsep(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("diagsep(model='acl') - quasi-complete separation", function(backend, solver) {
  diagsep(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("diagsep(model='acl') - overlap", function(backend, solver) {
  diagsep(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
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
  sd1 <- diagsep_acl(y_acl_qcs, X_acl_qcs, rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full (ACL)", function() {
  sd1 <- diagsep_acl(y_acl_qcs, X_acl_qcs, rational = rational)
  print(sd1, info = "full")
})


## =============================================================================
## 4.4 sepcols / detect_sepcols - ACL
## =============================================================================

print_section("sepcols / detect_sepcols (ACL)", 2)

# --- Lowest level: detect_sepcols_acl ---
print_section("detect_sepcols_acl (lowest level)", 3)

run_test("detect_sepcols_acl - complete separation", function(backend, solver) {
  detect_sepcols_acl(y_acl_cs, X_acl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_acl - quasi-complete separation", function(backend, solver) {
  detect_sepcols_acl(y_acl_qcs, X_acl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_acl - overlap", function(backend, solver) {
  detect_sepcols_acl(y_acl_ol, X_acl_ol, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: detect_sepcols with model="acl" ---
print_section("detect_sepcols with model='acl' (mid level)", 3)

run_test("detect_sepcols(model='acl') - complete separation", function(backend, solver) {
  detect_sepcols(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("detect_sepcols(model='acl') - quasi-complete separation", function(backend, solver) {
  detect_sepcols(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("detect_sepcols(model='acl') - overlap", function(backend, solver) {
  detect_sepcols(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})

### generic
run_test("separation_columns.bracl parallel - quasi-complete", function(backend, solver) {
  separation_columns(hdss_pacl, rational = rational, backend = backend, solver = solver)
})

run_test("separation_columns.bracl nonparallel - quasi-complete ", function(backend, solver) {
  separation_columns(hdss_npacl, rational = rational, backend = backend, solver = solver)
})



## =============================================================================
## 4.5 seprows - ACL
## =============================================================================

print_section("seprows (ACL)", 2)

# --- Lowest level: seprows_acl ---
print_section("seprows_acl (lowest level)", 3)

run_simple_test("seprows_acl - complete separation", function(backend, solver) {
  seprows_acl(y_acl_cs, X_acl_cs, rational = rational)
})

run_simple_test("seprows_acl - quasi-complete separation", function(backend, solver) {
  seprows_acl(y_acl_qcs, X_acl_qcs, rational = rational)
})

run_simple_test("seprows_acl - overlap", function(backend, solver) {
  seprows_acl(y_acl_ol, X_acl_ol, rational = rational)
})

# --- Mid level: seprows with model="acl" ---
print_section("seprows with model='acl' (mid level)", 3)

run_simple_test("seprows(model='acl') - complete separation", function(backend, solver) {
  seprows(y_acl_cs, X_acl_cs, rational = rational, model = "acl")
})

run_simple_test("seprows(model='acl') - quasi-complete separation", function(backend, solver) {
  seprows(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl")
})

run_simple_test("seprows(model='acl') - overlap", function(backend, solver) {
  seprows(y_acl_ol, X_acl_ol, rational = rational, model = "acl")
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

# --- Lowest level: linearities_acl ---
print_section("linearities_acl (lowest level)", 3)

run_simple_test("linearities_acl - complete separation", function(backend, solver) {
  linearities_acl(y_acl_cs, X_acl_cs, rational = rational)
})

run_simple_test("linearities_acl - quasi-complete separation", function(backend, solver) {
  linearities_acl(y_acl_qcs, X_acl_qcs, rational = rational)
})

run_simple_test("linearities_acl - overlap", function(backend, solver) {
  linearities_acl(y_acl_ol, X_acl_ol, rational = rational)
})

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
## 4.7 reccone / rec_cone - ACL
## =============================================================================

print_section("reccone / rec_cone (ACL)", 2)

# --- Lowest level: reccone_acl ---
print_section("reccone_acl (lowest level)", 3)

run_simple_test("reccone_acl - complete separation", function(backend, solver) {
  reccone_acl(y_acl_cs, X_acl_cs, rational = rational)
})

run_simple_test("reccone_acl - quasi-complete separation", function(backend, solver) {
  reccone_acl(y_acl_qcs, X_acl_qcs, rational = rational)
})

run_simple_test("reccone_acl - overlap", function(backend, solver) {
  reccone_acl(y_acl_ol, X_acl_ol, rational = rational)
})

# --- Mid level: reccone with model="acl" ---
print_section("reccone with model='acl' (mid level)", 3)

run_simple_test("reccone(model='acl') - complete separation", function(backend, solver) {
  reccone(y_acl_cs, X_acl_cs, rational = rational, model = "acl")
})

run_simple_test("reccone(model='acl') - quasi-complete separation", function(backend, solver) {
  reccone(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl")
})

run_simple_test("reccone(model='acl') - overlap", function(backend, solver) {
  reccone(y_acl_ol, X_acl_ol, rational = rational, model = "acl")
})


### generic
run_simple_test("recession_cone.bracl parallel - quasi-complete", function(backend, solver) {
  recession_cone(hdss_pacl, rational = rational)
})

run_simple_test("recession_cone.bracl nonparallel - quasi-complete ", function(backend, solver) {
  recession_cone(hdss_npacl, rational = rational)
})

## =============================================================================
## 4.8 overlap_fc - ACL
## =============================================================================

print_section("overlap_fc (ACL)", 2)

# --- Mid level: overlap_fc with model="acl" ---
print_section("overlap_fc with model='acl' (mid level)", 3)

run_test("overlap_fc(model='acl') - complete separation", function(backend, solver) {
  overlap_fc(y_acl_cs, X_acl_cs, frac = 1, verbose = 0, rational = rational, 
             model = "acl", backend = backend, solver = solver)
})

run_test("overlap_fc(model='acl') - quasi-complete separation", function(backend, solver) {
  overlap_fc(y_acl_qcs, X_acl_qcs, frac = 1, verbose = 0, rational = rational, 
             model = "acl", backend = backend, solver = solver)
})

run_test("overlap_fc(model='acl') - overlap", function(backend, solver) {
  overlap_fc(y_acl_ol, X_acl_ol, frac = 1, verbose = 0, rational = rational, 
             model = "acl", backend = backend, solver = solver)
})

## =============================================================================
## 4.9 overlap_qc - ACL
## =============================================================================

print_section("overlap_qc (ACL)", 2)

# --- Mid level: overlap_qc with model="acl" ---
print_section("overlap_qc with model='acl' (mid level)", 3)

run_test("overlap_qc(model='acl') - complete separation", function(backend, solver) {
  overlap_qc(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("overlap_qc(model='acl') - quasi-complete separation", function(backend, solver) {
  overlap_qc(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("overlap_qc(model='acl') - overlap", function(backend, solver) {
  overlap_qc(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})

## =============================================================================
## 4.10 separation_qc - ACL
## =============================================================================

print_section("separation_qc (ACL)", 2)

# --- Mid level: separation_qc with model="acl" ---
print_section("separation_qc with model='acl' (mid level)", 3)

run_test("separation_qc(model='acl') - complete separation", function(backend, solver) {
  separation_qc(y_acl_cs, X_acl_cs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("separation_qc(model='acl') - quasi-complete separation", function(backend, solver) {
  separation_qc(y_acl_qcs, X_acl_qcs, rational = rational, model = "acl", backend = backend, solver = solver)
})

run_test("separation_qc(model='acl') - overlap", function(backend, solver) {
  separation_qc(y_acl_ol, X_acl_ol, rational = rational, model = "acl", backend = backend, solver = solver)
})

cat("")
cat(paste(rep("#", 78), collapse = ""), "")
cat("##  END OF ACL MODEL TESTS")
cat(paste(rep("#", 78), collapse = ""), "")


################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE - PART 5: OSM MODEL TESTS                       ##
##                                                                            ##
################################################################################

print_section("Ordered Stereotype Model Tests (osm)")

## TODO: Are these results weird? 


## =============================================================================
## 5.1 checksep - OSM
## =============================================================================

print_section("checksep (OSM)", 2)

# --- Lowest level: checksep_osm ---
print_section("checksep_osm (lowest level)", 3)

run_test("checksep_osm - quasi-complete separation", function(backend, solver) {
  checksep_osm(y_osm_qcs, X_osm_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_osm - overlap", function(backend, solver) {
  checksep_osm(y_osm_ol, X_osm_ol, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_osm - wine data", function(backend, solver) {
  checksep_osm(y_osm_wine, X_osm_wine, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: checksep with model="osm" ---
print_section("checksep with model='osm' (mid level)", 3)

run_test("checksep(model='osm') - quasi-complete separation", function(backend, solver) {
  checksep(y_osm_qcs, X_osm_qcs, rational = rational, model = "osm", backend = backend, solver = solver)
})

run_test("checksep(model='osm') - overlap", function(backend, solver) {
  checksep(y_osm_ol, X_osm_ol, rational = rational, model = "osm", backend = backend, solver = solver)
})

# --- Generic: check_separation.osm ---
print_section("check_separation.osm (generic)", 3)

run_test("check_separation.osm - wine data", function(backend, solver) {
  check_separation(wine_osm, rational = rational, backend = backend, solver = solver)
})

## =============================================================================
## 5.2 checkovl - OSM
## =============================================================================

print_section("checkovl (OSM)", 2)


# --- Mid level: checkovl with model="osm" ---
print_section("checkovl with model='osm' (mid level)", 3)

run_test("checkovl(model='osm') - quasi-complete separation", function(backend, solver) {
  checkovl(y_osm_qcs, X_osm_qcs, rational = rational, model = "osm", backend = backend, solver = solver)
})

run_test("checkovl(model='osm') - overlap", function(backend, solver) {
  checkovl(y_osm_ol, X_osm_ol, rational = rational, model = "osm", backend = backend, solver = solver)
})

## =============================================================================
## 5.3 diagsep - OSM
## =============================================================================

print_section("diagsep (OSM)", 2)

# --- Lowest level: diagsep_osm ---
print_section("diagsep_osm (lowest level)", 3)

run_test("diagsep_osm - quasi-complete separation", function(backend, solver) {
  diagsep_osm(y_osm_qcs, X_osm_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_osm - overlap", function(backend, solver) {
  diagsep_osm(y_osm_ol, X_osm_ol, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_osm - wine data", function(backend, solver) {
  diagsep_osm(y_osm_wine, X_osm_wine, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: diagsep with model="osm" ---
print_section("diagsep with model='osm' (mid level)", 3)

run_test("diagsep(model='osm') - quasi-complete separation", function(backend, solver) {
  diagsep(y_osm_qcs, X_osm_qcs, rational = rational, model = "osm", backend = backend, solver = solver)
})

run_test("diagsep(model='osm') - overlap", function(backend, solver) {
  diagsep(y_osm_ol, X_osm_ol, rational = rational, model = "osm", backend = backend, solver = solver)
})

# --- Generic: diagnose_separation.osm ---
print_section("diagnose_separation.osm (generic)", 3)

run_test("diagnose_separation.osm - wine data", function(backend, solver) {
  diagnose_separation(wine_osm, rational = rational, backend = backend, solver = solver)
})

# --- print.sepmod for OSM ---
print_section("print.sepmod (OSM)", 3)

run_simple_test("print.sepmod - default (OSM)", function() {
  sd1 <- diagsep_osm(y_osm_qcs, X_osm_qcs, rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full (OSM)", function() {
  sd1 <- diagsep_osm(y_osm_qcs, X_osm_qcs, rational = rational)
  print(sd1, info = "full")
})

## =============================================================================
## 5.4 sepcols / detect_sepcols - OSM
## =============================================================================

print_section("sepcols / detect_sepcols (OSM)", 2)

# --- Lowest level: detect_sepcols_osm ---
print_section("detect_sepcols_osm (lowest level)", 3)


run_test("detect_sepcols_osm - quasi-complete separation", function(backend, solver) {
  detect_sepcols_osm(y_osm_qcs, X_osm_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_osm - overlap", function(backend, solver) {
  detect_sepcols_osm(y_osm_ol, X_osm_ol, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_osm - wine data", function(backend, solver) {
  detect_sepcols_osm(y_osm_wine, X_osm_wine, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: detect_sepcols with model="osm" ---
print_section("detect_sepcols with model='osm' (mid level)", 3)


run_test("detect_sepcols(model='osm') - quasi-complete separation", function(backend, solver) {
  detect_sepcols(y_osm_qcs, X_osm_qcs, rational = rational, model = "osm", backend = backend, solver = solver)
})

run_test("detect_sepcols(model='osm') - overlap", function(backend, solver) {
  detect_sepcols(y_osm_ol, X_osm_ol, rational = rational, model = "osm", backend = backend, solver = solver)
})

# --- Generic: separation_columns.osm ---
print_section("separation_columns.osm (generic)", 3)

run_test("separation_columns.osm - wine data", function(backend, solver) {
  separation_columns(wine_osm, rational = rational, backend = backend, solver = solver)
})

## =============================================================================
## 5.5 seprows - OSM
## =============================================================================

print_section("seprows (OSM)", 2)

# --- Lowest level: seprows_osm ---
print_section("seprows_osm (lowest level)", 3)


run_simple_test("seprows_osm - quasi-complete separation", function(backend, solver) {
  seprows_osm(y_osm_qcs, X_osm_qcs, rational = rational)
})

run_simple_test("seprows_osm - overlap", function(backend, solver) {
  seprows_osm(y_osm_ol, X_osm_ol, rational = rational)
})

run_simple_test("seprows_osm - wine data", function(backend, solver) {
  seprows_osm(y_osm_wine, X_osm_wine, rational = rational)
})

# --- Mid level: seprows with model="osm" ---
print_section("seprows with model='osm' (mid level)", 3)

run_simple_test("seprows(model='osm') - quasi-complete separation", function(backend, solver) {
  seprows(y_osm_qcs, X_osm_qcs, rational = rational, model = "osm")
})

run_simple_test("seprows(model='osm') - overlap", function(backend, solver) {
  seprows(y_osm_ol, X_osm_ol, rational = rational, model = "osm")
})

# --- Generic: separation_rows.osm ---
print_section("separation_rows.osm (generic)", 3)

run_simple_test("separation_rows.osm - wine data", function(backend, solver) {
  separation_rows(wine_osm, rational = rational)
})

## =============================================================================
## 5.6 linearities - OSM
## =============================================================================

print_section("linearities (OSM)", 2)

# --- Lowest level: linearities_osm ---
print_section("linearities_osm (lowest level)", 3)


run_simple_test("linearities_osm - quasi-complete separation", function(backend, solver) {
  linearities_osm(y_osm_qcs, X_osm_qcs, rational = rational)
})

run_simple_test("linearities_osm - overlap", function(backend, solver) {
  linearities_osm(y_osm_ol, X_osm_ol, rational = rational)
})

run_simple_test("linearities_osm - wine data", function(backend, solver) {
  linearities_osm(y_osm_wine, X_osm_wine, rational = rational)
})

# --- Mid level: linearities with model="osm" ---
print_section("linearities with model='osm' (mid level)", 3)

run_simple_test("linearities(model='osm') - quasi-complete separation", function(backend, solver) {
  linearities(y_osm_qcs, X_osm_qcs, rational = rational, model = "osm")
})

run_simple_test("linearities(model='osm') - overlap", function(backend, solver) {
  linearities(y_osm_ol, X_osm_ol, rational = rational, model = "osm")
})

## =============================================================================
## 5.7 reccone / rec_cone - OSM
## =============================================================================

print_section("reccone / rec_cone (OSM)", 2)

# --- Lowest level: reccone_osm ---
print_section("reccone_osm (lowest level)", 3)

run_simple_test("reccone_osm - quasi-complete separation", function(backend, solver) {
  reccone_osm(y_osm_qcs, X_osm_qcs, rational = rational)
})

run_simple_test("reccone_osm - overlap", function(backend, solver) {
  reccone_osm(y_osm_ol, X_osm_ol, rational = rational)
})

run_simple_test("reccone_osm - wine data", function(backend, solver) {
  reccone_osm(y_osm_wine, X_osm_wine, rational = rational)
})

# --- Mid level: reccone with model="osm" ---
print_section("reccone with model='osm' (mid level)", 3)

run_simple_test("reccone(model='osm') - quasi-complete separation", function(backend, solver) {
  reccone(y_osm_qcs, X_osm_qcs, rational = rational, model = "osm")
})

run_simple_test("reccone(model='osm') - overlap", function(backend, solver) {
  reccone(y_osm_ol, X_osm_ol, rational = rational, model = "osm")
})

# --- Generic: recession_cone.osm ---
print_section("recession_cone.osm (generic)", 3)

run_simple_test("recession_cone.osm - wine data", function(backend, solver) {
  recession_cone(wine_osm, rational = rational)
})

## =============================================================================
## 5.8 overlap_fc - OSM
## =============================================================================

print_section("overlap_fc (OSM)", 2)

# --- Mid level: overlap_fc with model="osm" ---
print_section("overlap_fc with model='osm' (mid level)", 3)


run_test("overlap_fc(model='osm') - quasi-complete separation", function(backend, solver) {
  overlap_fc(y_osm_qcs, X_osm_qcs, frac = 1, verbose = 0, rational = rational, 
             model = "osm", backend = backend, solver = solver)
})

run_test("overlap_fc(model='osm') - overlap", function(backend, solver) {
  overlap_fc(y_osm_ol, X_osm_ol, frac = 1, verbose = 0, rational = rational, 
             model = "osm", backend = backend, solver = solver)
})

## =============================================================================
## 5.9 overlap_qc - OSM
## =============================================================================

# --- Mid level: overlap_qc with model="osm" ---
print_section("overlap_qc with model='osm' (mid level)", 3)


run_test("overlap_qc(model='osm') - quasi-complete separation", function(backend, solver) {
  overlap_qc(y_osm_qcs, X_osm_qcs, rational = rational, model = "osm", backend = backend, solver = solver)
})

run_test("overlap_qc(model='osm') - overlap", function(backend, solver) {
  overlap_qc(y_osm_ol, X_osm_ol, rational = rational, model = "osm", backend = backend, solver = solver)
})

## =============================================================================
## 5.10 separation_qc - OSM
## =============================================================================

print_section("separation_qc (OSM)", 2)

# --- Mid level: separation_qc with model="osm" ---
print_section("separation_qc with model='osm' (mid level)", 3)


run_test("separation_qc(model='osm') - quasi-complete separation", function(backend, solver) {
  separation_qc(y_osm_qcs, X_osm_qcs, rational = rational, model = "osm", backend = backend, solver = solver)
})

run_test("separation_qc(model='osm') - overlap", function(backend, solver) {
  separation_qc(y_osm_ol, X_osm_ol, rational = rational, model = "osm", backend = backend, solver = solver)
})

cat("")
cat(paste(rep("#", 78), collapse = ""), "")
cat("##  END OF OSM MODEL TESTS")
cat(paste(rep("#", 78), collapse = ""), "")

################################################################################
##                                                                            ##
##  COMPREHENSIVE TEST SUITE - PART 6: SL MODEL TESTS                        ##
##                                                                            ##
################################################################################

print_section("Sequential Logit Model Tests (sl)")

## =============================================================================
## 6.1 checksep - SL
## =============================================================================

print_section("checksep (SL)", 2)

# --- Lowest level: checksep_sl ---
print_section("checksep_sl (lowest level)", 3)

run_test("checksep_sl - complete separation", function(backend, solver) {
  checksep_sl(y_sl_cs, X_sl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_sl - quasi-complete separation", function(backend, solver) {
  checksep_sl(y_sl_qcs, X_sl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("checksep_sl - overlap", function(backend, solver) {
  checksep_sl(y_sl_ol, X_sl_ol, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: checksep with model="sl" ---
print_section("checksep with model='sl' (mid level)", 3)

run_test("checksep(model='sl') - complete separation", function(backend, solver) {
  checksep(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("checksep(model='sl') - quasi-complete separation", function(backend, solver) {
  checksep(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("checksep(model='sl') - overlap", function(backend, solver) {
  checksep(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

## =============================================================================
## 6.2 checkovl - SL
## =============================================================================

print_section("checkovl (SL)", 2)


# --- Mid level: checkovl with model="sl" ---
print_section("checkovl with model='sl' (mid level)", 3)

run_test("checkovl(model='sl') - complete separation", function(backend, solver) {
  checkovl(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("checkovl(model='sl') - quasi-complete separation", function(backend, solver) {
  checkovl(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("checkovl(model='sl') - overlap", function(backend, solver) {
  checkovl(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

## =============================================================================
## 6.3 diagsep - SL
## =============================================================================

print_section("diagsep (SL)", 2)

# --- Lowest level: diagsep_sl ---
print_section("diagsep_sl (lowest level)", 3)

run_test("diagsep_sl - complete separation", function(backend, solver) {
  diagsep_sl(y_sl_cs, X_sl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_sl - quasi-complete separation", function(backend, solver) {
  diagsep_sl(y_sl_qcs, X_sl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("diagsep_sl - overlap", function(backend, solver) {
  diagsep_sl(y_sl_ol, X_sl_ol, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: diagsep with model="sl" (continued) ---
run_test("diagsep(model='sl') - complete separation", function(backend, solver) {
  diagsep(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("diagsep(model='sl') - quasi-complete separation", function(backend, solver) {
  diagsep(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("diagsep(model='sl') - overlap", function(backend, solver) {
  diagsep(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

# --- print.sepmod for SL ---
print_section("print.sepmod (SL)", 3)

run_simple_test("print.sepmod - default (SL)", function() {
  sd1 <- diagsep_sl(y_sl_qcs, X_sl_qcs, rational = rational)
  print(sd1)
})

run_simple_test("print.sepmod - full (SL)", function() {
  sd1 <- diagsep_sl(y_sl_qcs, X_sl_qcs, rational = rational)
  print(sd1, info = "full")
})

## =============================================================================
## 6.4 sepcols / detect_sepcols - SL
## =============================================================================

print_section("sepcols / detect_sepcols (SL)", 2)

# --- Lowest level: detect_sepcols_sl ---
print_section("detect_sepcols_sl (lowest level)", 3)

run_test("detect_sepcols_sl - complete separation", function(backend, solver) {
  detect_sepcols_sl(y_sl_cs, X_sl_cs, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_sl - quasi-complete separation", function(backend, solver) {
  detect_sepcols_sl(y_sl_qcs, X_sl_qcs, rational = rational, backend = backend, solver = solver)
})

run_test("detect_sepcols_sl - overlap", function(backend, solver) {
  detect_sepcols_sl(y_sl_ol, X_sl_ol, rational = rational, backend = backend, solver = solver)
})

# --- Mid level: detect_sepcols with model="sl" ---
print_section("detect_sepcols with model='sl' (mid level)", 3)

run_test("detect_sepcols(model='sl') - complete separation", function(backend, solver) {
  detect_sepcols(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("detect_sepcols(model='sl') - quasi-complete separation", function(backend, solver) {
  detect_sepcols(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("detect_sepcols(model='sl') - overlap", function(backend, solver) {
  detect_sepcols(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

## =============================================================================
## 6.5 seprows - SL
## =============================================================================

print_section("seprows (SL)", 2)

# --- Lowest level: seprows_sl ---
print_section("seprows_sl (lowest level)", 3)

run_simple_test("seprows_sl - complete separation", function(backend, solver) {
  seprows_sl(y_sl_cs, X_sl_cs, rational = rational)
})

run_simple_test("seprows_sl - quasi-complete separation", function(backend, solver) {
  seprows_sl(y_sl_qcs, X_sl_qcs, rational = rational)
})

run_simple_test("seprows_sl - overlap", function(backend, solver) {
  seprows_sl(y_sl_ol, X_sl_ol, rational = rational)
})

# --- Mid level: seprows with model="sl" ---
print_section("seprows with model='sl' (mid level)", 3)

run_simple_test("seprows(model='sl') - complete separation", function(backend, solver) {
  seprows(y_sl_cs, X_sl_cs, rational = rational, model = "sl")
})

run_simple_test("seprows(model='sl') - quasi-complete separation", function(backend, solver) {
  seprows(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl")
})

run_simple_test("seprows(model='sl') - overlap", function(backend, solver) {
  seprows(y_sl_ol, X_sl_ol, rational = rational, model = "sl")
})

## =============================================================================
## 6.6 linearities - SL
## =============================================================================

print_section("linearities (SL)", 2)

# --- Lowest level: linearities_sl ---
print_section("linearities_sl (lowest level)", 3)

run_simple_test("linearities_sl - complete separation", function(backend, solver) {
  linearities_sl(y_sl_cs, X_sl_cs, rational = rational)
})

run_simple_test("linearities_sl - quasi-complete separation", function(backend, solver) {
  linearities_sl(y_sl_qcs, X_sl_qcs, rational = rational)
})

run_simple_test("linearities_sl - overlap", function(backend, solver) {
  linearities_sl(y_sl_ol, X_sl_ol, rational = rational)
})

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
## 6.7 reccone / rec_cone - SL
## =============================================================================

print_section("reccone / rec_cone (SL)", 2)

# --- Lowest level: reccone_sl ---
print_section("reccone_sl (lowest level)", 3)

run_simple_test("reccone_sl - complete separation", function(backend, solver) {
  reccone_sl(y_sl_cs, X_sl_cs, rational = rational)
})

run_simple_test("reccone_sl - quasi-complete separation", function(backend, solver) {
  reccone_sl(y_sl_qcs, X_sl_qcs, rational = rational)
})

run_simple_test("reccone_sl - overlap", function(backend, solver) {
  reccone_sl(y_sl_ol, X_sl_ol, rational = rational)
})

# --- Mid level: reccone with model="sl" ---
print_section("reccone with model='sl' (mid level)", 3)

run_simple_test("reccone(model='sl') - complete separation", function(backend, solver) {
  reccone(y_sl_cs, X_sl_cs, rational = rational, model = "sl")
})

run_simple_test("reccone(model='sl') - quasi-complete separation", function(backend, solver) {
  reccone(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl")
})

run_simple_test("reccone(model='sl') - overlap", function(backend, solver) {
  reccone(y_sl_ol, X_sl_ol, rational = rational, model = "sl")
})

## =============================================================================
## 6.8 overlap_fc - SL
## =============================================================================

print_section("overlap_fc (SL)", 2)


# --- Mid level: overlap_fc with model="sl" ---
print_section("overlap_fc with model='sl' (mid level)", 3)

run_test("overlap_fc(model='sl') - complete separation", function(backend, solver) {
  overlap_fc(y_sl_cs, X_sl_cs, frac = 1, verbose = 0, rational = rational, 
             model = "sl", backend = backend, solver = solver)
})

run_test("overlap_fc(model='sl') - quasi-complete separation", function(backend, solver) {
  overlap_fc(y_sl_qcs, X_sl_qcs, frac = 1, verbose = 0, rational = rational, 
             model = "sl", backend = backend, solver = solver)
})

run_test("overlap_fc(model='sl') - overlap", function(backend, solver) {
  overlap_fc(y_sl_ol, X_sl_ol, frac = 1, verbose = 0, rational = rational, 
             model = "sl", backend = backend, solver = solver)
})

## =============================================================================
## 6.9 overlap_qc - SL
## =============================================================================

print_section("overlap_qc (SL)", 2)

# --- Mid level: overlap_qc with model="sl" ---
print_section("overlap_qc with model='sl' (mid level)", 3)

run_test("overlap_qc(model='sl') - complete separation", function(backend, solver) {
  overlap_qc(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("overlap_qc(model='sl') - quasi-complete separation", function(backend, solver) {
  overlap_qc(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("overlap_qc(model='sl') - overlap", function(backend, solver) {
  overlap_qc(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
})

## =============================================================================
## 6.10 separation_qc - SL
## =============================================================================

print_section("separation_qc (SL)", 2)


# --- Mid level: separation_qc with model="sl" (continued) ---
run_test("separation_qc(model='sl') - complete separation", function(backend, solver) {
  separation_qc(y_sl_cs, X_sl_cs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("separation_qc(model='sl') - quasi-complete separation", function(backend, solver) {
  separation_qc(y_sl_qcs, X_sl_qcs, rational = rational, model = "sl", backend = backend, solver = solver)
})

run_test("separation_qc(model='sl') - overlap", function(backend, solver) {
  separation_qc(y_sl_ol, X_sl_ol, rational = rational, model = "sl", backend = backend, solver = solver)
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
## 8.2 Backend/Solver Coverage Summary
## =============================================================================

cat("--------------------------------------------------------------------------------
")
cat("                        BACKEND/SOLVER COVERAGE                                 
")
cat("--------------------------------------------------------------------------------
")
cat("
")

cat("  Backends tested:
")
for (be in backends) {
  status <- if (be %in% available_backends) "AVAILABLE" else "SKIPPED"
  cat(sprintf("    - %-12s [%s]
", be, status))
}
cat("
")

cat("  Solvers tested:
")
for (sol in solvers) {
  status <- if (sol %in% available_solvers) "AVAILABLE" else "SKIPPED"
  cat(sprintf("    - %-12s [%s]
", sol, status))
}
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
  "Baseline-Category Logit (bcl)" = "nnet::multinom, mlogit, mclogit::mblogit",
  "Cumulative Logit (cl)"         = "ordinal::clm",
  "Adjacent-Category Logit (acl)" = "nnet::multinom (ordered)",
  "Ordered Stereotype Model (osm)"= "clustord::osm",
  "Sequential Logit (sl)"         = "custom implementation"
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
    "checksep / checksep_*",
    "checkovl / checkovl_*",
    "diagsep / diagsep_*"
  ),
  "Column/Row Analysis" = c(
    "detect_sepcols / detect_sepcols_*",
    "seprows / seprows_*",
    "linearities / linearities_*"
  ),
  "Cone Operations" = c(
    "reccone / reccone_*",
    "overlap_fc / overlap_fc_*",
    "overlap_qc / overlap_qc_*",
    "separation_qc / separation_qc_*"
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

## =============================================================================
## 8.7 Export Results (Optional)
## =============================================================================

print_section("Export Results", 2)

# Create results data frame for export
results_df <- do.call(rbind, lapply(test_results$log, function(x) {
  data.frame(
    test_name = x$test_name,
    status = x$status,
    backend = ifelse(is.null(x$backend), NA, x$backend),
    solver = ifelse(is.null(x$solver), NA, x$solver),
    duration = ifelse(is.null(x$duration), NA, x$duration),
    error = ifelse(is.null(x$error), NA, x$error),
    stringsAsFactors = FALSE
  )
}))

# Save results to file
results_file <- sprintf("test_results_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
tryCatch({
  write.csv(results_df, results_file, row.names = FALSE)
  cat(sprintf("  Results exported to: %s
", results_file))
}, error = function(e) {
  cat(sprintf("  Warning: Could not export results: %s
", e$message))
})

# Save detailed log as RDS
log_file <- sprintf("test_log_%s.rds", format(Sys.time(), "%Y%m%d_%H%M%S"))
tryCatch({
  saveRDS(test_results, log_file)
  cat(sprintf("  Detailed log saved to: %s
", log_file))
}, error = function(e) {
  cat(sprintf("  Warning: Could not save log: %s
", e$message))
})

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
