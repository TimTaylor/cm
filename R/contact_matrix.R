#' @importFrom stats setNames
NULL

#' Create a social contact matrix
#'
#' `social_contact_matrix()` creates a social contract matrix from `i`, `j` and
#' `value` vectors following the definition of given in \cite{Klepac et al}.
#'
#' @param i `[character]`.
#' Vector of grouping that represent the *ith* (row) in \eqn{m_{ij}} contacts.
#'
#' @param j `[character]`.
#' Vector of grouping that represent the *jth* (column) in \eqn{m_{ij}} contacts.
#'
#' @param value `[numeric]`.
#' Vector of values corresponding corresponding to inputs `i` and `j`.
#'
#' @param groups `[character]`.
#' Possible groups that can appear in `i` and `j`. This will be used as the
#' `dimnames()` of the resulting contact matrix. By default this is set to
#' unique(c(i, j)).
#'
#' @param from `[character]`.
#' Should the contact matrix be interpreted as from "columns" to rows or from
#' "rows" to columns. Defaults to `from = "columns"`.
#'
#' @param fill `[numeric]`.
#' Numeric scalar used to fill empty values in the resulting contact matrix.
#' Defaults to 0.
#'
#' @export
social_contact_matrix <- function(
    i = character(),
    j = character(),
    value = numeric(),
    groups = unique(c(i, j)),
    from = c("columns", "rows"),
    fill = 0
) {
    # check rows are both characters or both factors
    if (is.factor(i) && is.factor(j)) {
        i <- as.character(i)
        j <- as.character(j)
    } else if (!(is.character(i) && is.character(j))) {
        stop("`i` and `j` must both be character or factor vectors.")
    }

    # ensure from is valid
    from <- match.arg(from)

    # check fill
    if (!(is.numeric(fill) && length(fill) == 1L))
        stop("`fill` must be a numeric scalar value.")

    # check for compatible row and column lengths
    ni <- length(i)
    nj <- length(j)
    if (ni != nj)
        stop("`i` and `j` must be of equal length.")

    # check values are valid
    if (!is.numeric(value) || length(value) != ni)
        stop("`value` must be numeric and of the same length as `i` (and `j`).")

    # check groups or generate if NULL
    possible_groups <- unique(c(i, j))
    if (is.null(groups)) {
        groups <- possible_groups
    } else if (!(is.factor(groups) || is.character(groups))) {
        stop("`groups` must be a factor or character vector.")
    } else {
        if (anyDuplicated(groups))
            stop("`groups` must be unique.")
        missing <- possible_groups[!possible_groups %in% groups]
        if (length(missing)) {
            stop(sprintf(
                "%s is present in `i` / `j` but not in `groups`",
                sQuote(missing[1L])
            ))
        }
    }

    size <- length(groups)
    out <- matrix(data = fill, nrow = size, ncol = size, dimnames = list(groups, groups))
    index <- cbind(i, j)
    out[index] <- value
    .new_contact_matrix(out, from = from, class = "social_contact_matrix")
}

#' Create a population contact matrix
#'
#' `population_contact_matrix()` creates a social contract matrix from `i`, `j`
#' and `value` vectors following the definition of given in \cite{Klepac et al}.
#'
#' @param i `[character]`.
#' Vector of grouping that represent the *ith* (row) in \eqn{C_{ij}} contacts.
#'
#' @param j `[character]`.
#' Vector of grouping that represent the *jth* (column) in \eqn{C_{ij}} contacts.
#'
#' @param value `[numeric]`.
#' Vector of values corresponding corresponding to inputs `i` and `j`.
#'
#' @param groups `[character]`.
#' Possible groups that can appear in `i` and `j`. This will be used as the
#' `dimnames()` of the resulting contact matrix. By default this is set to
#' unique(c(i, j)).
#'
#' @param population `[numeric]`.
#' Vector of length the same as `groups` with value corresponding to the
#' population size of group with the *kth* value corresponding to the *kth*
#' entry in `groups`. Used to validate the correctness of the resulting
#' contact matrix, that is \eqn{C_{ij} * w_{j} == C_{ji} * w_{i}}, where
#' \eqn{w_{k}} represents the population in group *k*.
#'
#' @param from `[character]`.
#' Should the contact matrix be interpreted as from "columns" to rows or from
#' "rows" to columns. Defaults to `from = "columns"`.
#'
#' @param fill `[numeric]`.
#' Numeric scalar used to fill empty values in the resulting contact matrix.
#' Defaults to 0.
#'
#' @export
population_contact_matrix <- function(
    i = character(),
    j = character(),
    value,
    groups = unique(c(i, j)),
    population,
    from = c("columns", "rows"),
    fill = 0
) {
    # check rows are both characters or both factors
    if (is.factor(i) && is.factor(j)) {
        i <- as.character(i)
        j <- as.character(j)
    } else if (!(is.character(i) && is.character(j))) {
        stop("`i` and `j` must both be character or factor vectors.")
    }

    # ensure from is valid
    from <- match.arg(from)

    # check fill
    if (!(is.numeric(fill) && length(fill) == 1L))
        stop("`fill` must be a numeric scalar value.")

    # check for compatible row and column lengths
    ni <- length(i)
    nj <- length(j)
    if (ni != nj)
        stop("`i` and `j` must be of equal length.")

    # check values are valid
    if (!is.numeric(value) || length(value) != ni)
        stop("`value` must be numeric and of the same length as `i` (and `j`).")

    # check groups or generate if NULL
    possible_groups <- unique(c(i, j))
    if (is.null(groups)) {
        groups <- possible_groups
    } else if (!(is.factor(groups) || is.character(groups))) {
        stop("`groups` must be a factor or character vector.")
    } else {
        groups <- unique(as.character(groups))
        missing <- possible_groups[!possible_groups %in% groups]
        if (length(missing)) {
            stop(sprintf(
                "%s is present in `i` / `j` but not in `groups`",
                sQuote(missing[1L])
            ))
        }
    }

    # create matrix
    size <- length(groups)
    out <- matrix(data = fill, nrow = size, ncol = size, dimnames = list(groups, groups))
    index <- cbind(i, j)
    out[index] <- value

    # check population
    if (!is.numeric(population) || length(population) != size)
        stop("`value` must be numeric and of the same length as `groups`.")
    if (from == "columns") {
        weighted_out <- out * population[col(out)] / population[row(out)]
    } else {
        weighted_out <- out * population[row(out)] / population[col(out)]
    }
    if (!isTRUE(all.equal(out, t(weighted_out))))
        stop("`population` values incompatible with specified `i`, `j` and `values`.")

    # add population information as attribute and return
    out <- .new_contact_matrix(out, from = from, class = "population_contact_matrix")
    attr(out, "population") <- setNames(population, groups)
    out
}


#' @export
print.contact_matrix <- function(x, ...) {

    # copy so we can return invisibly at end
    original <- x

    # format from and to
    from <- attr(x, "from")
    if (from == "columns") {
        to <- "rows"
    } else if (from == "rows") {
        to <- "columns"
    } else {
        stop("Corrupt contract_matrix: invalid `from` attribute")
    }

    # get dimensions
    d <- dim(x)

    # print meta
    cat(sprintf("From: %s\n", from))
    cat(sprintf("  To: %s\n", to))
    cat(sprintf(" dim: %d x %d\n\n", d[1], d[2]))

    # strip class and from attribute prior to printing
    attr(x, "from") <- NULL
    print(unclass(x), ...)

    # return input invisibly
    invisible(original)
}

#' @export
print.population_contact_matrix <- function(x, ...) {

    # copy so we can return invisibly at the end
    original <- x

    # strip population attribute for printing purposes
    attr(x, "population") <- NULL

    # dispatch to contact_matrix printing
    print.contact_matrix(x, ...)

    # print population
    cat("\nPopulation:\n")
    print(attr(original, "population"))

    # return input invisibly
    invisible(original)
}



#' Coerce to a population contact matrix
#'
#' `as_population_matrix()` is a generic for coercing an input to a population
#' contact matrix.
#'
#' @param x An \R object..
#' Vector of grouping that represent the *ith* (row) in \eqn{m_{ij}} contacts.
#'
#' @param population `[numeric]`.
#' Vector of length equal to the number of rows (columns) in `x`. The *kth*
#' entry corresponds to the group represented by the *kth* row of `x`.
#'
#' @param ...
#' Additional arguments to be passed to or from methods.
#'
#' @export
as_population_matrix <- function(x, ...) UseMethod("as_population_matrix")

#' @rdname as_population_matrix
#' @export
as_population_matrix.social_contact_matrix <- function(x, population, ...) {
    from = attr(x, "from")
    if (from == "columns") {
        out <- (x + t(x) * population[row(x)] / population[col(x)]) / 2
    } else {
        # x has equal dimensions so row(x) == row(t(x)) and col(x) == col(t(x))
        out <- t((t(x) + x * population[row(x)] / population[col(x)]) / 2)
    }

    # add population information as attribute and return
    groups <- dimnames(out)[[1]]
    out <- .new_contact_matrix(out, from = from, class = "population_contact_matrix")
    attr(out, "population") <- setNames(population, groups)
    out

}

#' @export
as.matrix.contact_matrix <- function(x, ...) {
    dim <- dim(x)
    attributes(x) <- NULL
    dim(x) <- dim
    x
}

# WIP - this would be the external function for other developers building
#       contact matrices. Has a few additional checks on input. I've not used
#       for the the social and raw contact matrices construction but this could
#       change. Not yet exported.
#
# Create a contact matrix
new_contact_matrix <- function(m, from = c("columns", "rows"), class = NULL) {

    # check matrix input
    if (!is.matrix(m))
        stop("`x` must be a matrix")

    # check valid from
    from <- match.arg(from)

    # check valid class
    if (!is.character(class) || length(class) != 1L)
        stop("`class` must be a string (character vector of length 1).")

    # must be square
    d <- dim(m)
    if (d[1L] != d[2L])
        stop(sprintf("contact matrices must be square but `m` is %d x %d.", d[1], d[2]))

    # if it has dimnames they must be identical
    nms <- dimnames(m)
    if (!is.null(nms[[1L]]) && !identical(nms[[1]], nms[[2L]]))
        stop("the row and column names of `m` must be identical (or NULL).")

    # return contact matrix
    .new_contact_matrix(m)

}



# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# Internals ---------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
.new_contact_matrix <- function(m, from, class) {
    new_attributes <- list(
        dim = dim(m),
        dimnames = dimnames(m),
        from = from,
        class = c(class, "contact_matrix")
    )
    attributes(m) <- new_attributes
    m
}
