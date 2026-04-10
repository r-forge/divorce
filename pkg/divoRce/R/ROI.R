.divorce_match_backend <- function(backend) {
    match.arg(backend, c("rcdd", "ROI"))
}

.divorce_stop_if_roi_rational <- function(rational, x = NULL) {
    if (isTRUE(rational) || (!is.null(x) && rat_cols(x))) {
        stop(
            "The ROI backend only supports numeric LP input. Use backend = \"rcdd\" for rational arithmetic.",
            call. = FALSE
        )
    }
}

.divorce_require_roi <- function() {
    if (!requireNamespace("ROI", quietly = TRUE)) {
        stop(
            "Package 'ROI' is required for backend = \"ROI\". Install 'ROI' and an LP-capable ROI plugin such as 'ROI.plugin.highs' (recommended), 'ROI.plugin.glpk' or 'ROI.plugin.lpsolve'.",
            call. = FALSE
        )
    }
}

.divorce_load_roi_plugins <- function() {
    .divorce_require_roi()
    ip <- rownames(utils::installed.packages())
    plugin_pkgs <- grep("^ROI\\.plugin\\.", ip, value = TRUE)
    for (pkg in plugin_pkgs) {
        requireNamespace(pkg, quietly = TRUE)
    }
    invisible(plugin_pkgs)
}

.divorce_roi_make_op <- function(objective, L, dir, rhs, maximum = FALSE) {
    objective <- as.numeric(objective)
    L <- as.matrix(L)
    storage.mode(L) <- "double"
    rhs <- as.numeric(rhs)
    ROI::OP(
        objective = ROI::L_objective(L = objective),
        constraints = ROI::L_constraint(L = L, dir = dir, rhs = rhs),
        maximum = maximum
    )
}

.divorce_roi_choose_solver <- function(op, solver = NULL) {
    .divorce_require_roi()
    .divorce_load_roi_plugins()
    available_solvers <- names(ROI::ROI_registered_solvers())
    applicable_solvers <- ROI::ROI_applicable_solvers(op)

    if (is.null(solver)) {
        if (length(applicable_solvers) < 1L) {
            stop(
                "No applicable ROI solver plugin for linear programs is installed. Install an LP-capable ROI plugin such as 'ROI.plugin.highs' (recommended), 'ROI.plugin.glpk' or 'ROI.plugin.lpsolve'.",
                call. = FALSE
            )
        }
       return(applicable_solvers[[1L]])
    }

    if (!(solver %in% available_solvers)) {
        stop(
            sprintf(
                "ROI solver '%s' is not registered. Registered solvers: %s.",
                solver,
                if (length(available_solvers)) paste(available_solvers, collapse = ", ") else "<none>"
            ),
            call. = FALSE
        )
    }

    if (!(solver %in% applicable_solvers)) {
        stop(
            sprintf(
                "ROI solver '%s' is not applicable to this linear program. Applicable solvers: %s.",
                solver,
                if (length(applicable_solvers)) paste(applicable_solvers, collapse = ", ") else "<none>"
            ),
            call. = FALSE
        )
    }

    solver
}

.divorce_roi_status_code <- function(result) {
    out <- tryCatch(
        ROI::solution(result, type = "status_code", force = TRUE),
        error = function(e) NA_integer_
    )
    if (length(out) < 1L) return(NA_integer_)
    as.integer(out[[1L]])
}

.divorce_roi_status_message <- function(result) {
    msg <- tryCatch(
        ROI::solution(result, type = "status", force = TRUE),
        error = function(e) NULL
    )
    if (is.null(msg) || length(msg) < 1L) {
        msg <- tryCatch(
            ROI::solution(result, type = "msg", force = TRUE),
            error = function(e) NULL
        )
    }
    if (is.null(msg) || length(msg) < 1L) return("")
    paste(msg, collapse = " ")
}

.divorce_roi_assert_success <- function(result, solver) {
    status_code <- .divorce_roi_status_code(result)
    if (!identical(status_code, 0L)) {
        status_msg <- .divorce_roi_status_message(result)
        if (nzchar(status_msg)) {
            stop(
                sprintf(
                    "ROI solver '%s' did not return an optimal solution (status code %s: %s).",
                    solver,
                    status_code,
                    status_msg
                ),
                call. = FALSE
            )
        }
        stop(
            sprintf(
                "ROI solver '%s' did not return an optimal solution (status code %s).",
                solver,
                status_code
            ),
            call. = FALSE
        )
    }
}

.divorce_roi_extract_objective <- function(result, solver) {
    .divorce_roi_assert_success(result, solver)
    out <- tryCatch(
        ROI::solution(result, type = "objval", force = TRUE),
        error = function(e) NULL
    )
    if (is.null(out) || length(out) < 1L || is.na(out[[1L]])) {
        stop(
            sprintf("ROI solver '%s' did not return an objective value.", solver),
            call. = FALSE
        )
    }
    as.numeric(out[[1L]])
}

.divorce_roi_extract_primal <- function(result, solver) {
    .divorce_roi_assert_success(result, solver)
    out <- tryCatch(
        ROI::solution(result, type = "primal", force = TRUE),
        error = function(e) NULL
    )
    if (is.null(out) || length(out) < 1L || anyNA(out)) {
        stop(
            sprintf("ROI solver '%s' did not return a primal solution.", solver),
            call. = FALSE
        )
    }
    as.numeric(out)
}

.divorce_roi_solve <- function(op, solver = NULL) {
    solver <- .divorce_roi_choose_solver(op, solver = solver)
    result <- ROI::ROI_solve(op, solver = solver)
    list(result = result, solver = solver)
}

.divorce_check_sep_lp <- function(Xstar, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL) {
    backend <- .divorce_match_backend(backend)
    
    if (backend == "ROI") {
        .divorce_stop_if_roi_rational(rational, Xstar)
        a1 <- rbind(cbind(-diag(nrow(Xstar)), -1), c(rep(0, nrow(Xstar)), -1))
        b1 <- c(rep(-1, each = nrow(Xstar)), 0)
        a2 <- cbind(t(Xstar), 0)
        b2 <- rep(0, ncol(Xstar))
        objgrd <- c(rep(0, nrow(Xstar)), 1)
        op <- .divorce_roi_make_op(
            objective = objgrd,
            L = rbind(a1, a2),
            dir = c(rep("<=", nrow(a1)), rep("==", nrow(a2))),
            rhs = c(b1, b2),
            maximum = FALSE
        )
        roi_fit <- .divorce_roi_solve(op, solver = solver)
        return(.divorce_roi_extract_objective(roi_fit$result, roi_fit$solver))
    }
    if (backend == "rcdd") {
         if(is.null(solver)) solver <- "DualSimplex"
         a1 <- rbind(cbind(-diag(nrow(Xstar)), -1), c(rep(0, nrow(Xstar)), -1))
         if (rational) a1 <- rcdd::d2q(a1)
         b1 <- c(rep(-1, each = nrow(Xstar)), 0)
         if (rational) b1 <- rcdd::d2q(b1)
         a2 <- cbind(t(Xstar), 0)
         if (rational && !rat_cols(Xstar)) a2 <- rcdd::d2q(a2)
         b2 <- rep(0, ncol(Xstar))
         if (rational) b2 <- rcdd::d2q(b2)
         objgrd <- c(rep(0, nrow(Xstar)), 1)
         if (rational) objgrd <- rcdd::d2q(objgrd)
         cali <- rcdd::lpcdd(
                           rcdd::makeH(a1 = a1, b1 = b1, a2 = a2, b2 = b2),
                           objgrd = objgrd, minimize = TRUE, solver = solver)
         if(cali$solution.type != "Optimal") stop(sprintf(
                                                   "rcdd solver '%s' did not return an optimal solution (status code %s).",
                                                   solver, cali$solution.type),
                                             call. = FALSE)
         cal <- cali$optimal.value
         if (rational) cal <- rcdd::q2d(cal)
         return(cal)
    }
}

.divorce_detect_sepcols_lp <- function(Xstar, rational = FALSE, backend = c("rcdd", "ROI"), solver = NULL) {
    backend <- .divorce_match_backend(backend)

    if (backend == "ROI") {
        .divorce_stop_if_roi_rational(rational, Xstar)
        A1 <- rbind(
            -Xstar,
            -diag(ncol(Xstar)),
            diag(ncol(Xstar))
        )
        b1 <- c(
            rep(0, nrow(Xstar)),
            rep(1, ncol(Xstar)),
            rep(1, ncol(Xstar))
        )
        a <- as.numeric(t(rep(1, nrow(Xstar))) %*% Xstar)
        op <- .divorce_roi_make_op(
            objective = a,
            L = A1,
            dir = rep("<=", nrow(A1)),
            rhs = b1,
            maximum = TRUE
        )
        roi_fit <- .divorce_roi_solve(op, solver = solver)
        return(.divorce_roi_extract_primal(roi_fit$result, roi_fit$solver))
    }
    if (backend == "rcdd") {
    if(is.null(solver)) solver <- "DualSimplex"
    if (rational) Xstar <- rcdd::q2d(Xstar)
    A1 <- rbind(
        -Xstar,
        -diag(ncol(Xstar)),
        diag(ncol(Xstar))
    )
    b1 <- c(
        rep(0, nrow(Xstar)),
        rep(1, ncol(Xstar)),
        rep(1, ncol(Xstar))
    )
    if (rational) {
        A1 <- rcdd::d2q(A1)
        b1 <- rcdd::d2q(b1)
    }
    hrep <- rcdd::makeH(a1 = A1, b1 = b1)
    a <- as.numeric(t(rep(1, nrow(Xstar))) %*% Xstar)
    if (rational) a <- rcdd::d2q(a)
    lsoi <- rcdd::lpcdd(hrep, a, minimize = FALSE, solver = solver)
    if(lsoi$solution.type != "Optimal") stop(sprintf(
                                                   "rcdd solver '%s' did not return a primal solution (status code %s).",
                                                   solver, lsoi$solution.type),
                                             call. = FALSE)
    lso <- lsoi$primal.solution
    if (rational) lso <- rcdd::q2d(lso)
    return(lso)
    }
}
