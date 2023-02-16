#' @importFrom stats setNames
#' @importFrom withr with_collate
#' @import data.table
NULL

#' Contact matrix constructor
#'
# -------------------------------------------------------------------------
#' `contact_matrix()` creates a social contract matrix from long form input.
#'
# -------------------------------------------------------------------------
#' @param from `[list]`
#'
#' @param to `[list]`
#'
#' @param value `[numeric]`
#'
#' @param order `[character]`
#'
#' @param fill `[numeric]`
#'
# -------------------------------------------------------------------------
#'
#' @export
contact_matrix <- function(
    from = list(),
    to = list(),
    value = numeric(),
    order = NULL,
    fill = 0
) {

    # from checks
    if (!is.list(from))
        stop("`from` must be a list.")

    length_from <- length(from)
    if (!length(from))
        stop("`from` cannot be empty.")

    nrows_from <- unique(lengths(from))
    if (length(nrows_from) > 1L)
        stop("all variables in `from` must have the same length.")

    names_from <- names(from)
    if (is.null(names(from)))
        stop("all variables in `from` must be named.")
    if (length(unique(names_from)) != length(names_from))
        stop("all variables in `from` must be uniquely named.")
    if (any(names_from == ""))
        stop("all variables in `from` must be uniquely named.")

    # to checks
    if (!is.list(to))
        stop("`to` must be a list.")
    length_to <- length(to)
    if (length_from != length_to)
        stop("`from` and `to` must have equal length.")

    names_to <- names(to)
    if (!setequal(names_to, names_from))
        stop("`from` and `to` variables must have identical names.")

    nrows_to <- unique(lengths(to))
    if (length(nrows_to) > 1L || nrows_to != nrows_from)
        stop("all variables in `to` must have the same length as those in `from`.")

    # value checking
    if(!is.numeric(value))
        stop("`value` must be <numeric>.")
    if (length(value) != nrows_from)
        stop("`value` must be the same length as the variables in `from`.")

    # alias names
    nms <- names_from

    # TODO - custom high level ordering
    if (!is.null(order)) {
        stop("TODO - custom ordering is not yet implemented")
    } else {
        # use C locale
        nms <- withr::with_collate("C", sort(nms))
        from <- from[nms]
        to <- to[nms]
    }

    # combine to and from to get possible groupings
    # note using data.table should ensures C Locale sorting
    .id <- NULL
    possible <- .mapply(c, dots = list(from, to), MoreArgs = NULL)
    possible <- do.call(CJ, c(unname(possible), sorted = TRUE, unique = TRUE))
    setnames(possible, nms)
    possible[, .id := .I]

    # index from
    from <- as.data.table(from)
    from_idx <- possible[from, on = nms]

    # index to
    to <- as.data.table(to)
    to_idx <- possible[to, on = nms]

    # pull out groups
    setDF(possible)
    possible$.id <- NULL

    # create matrix
    dimnames <- do.call(paste, c(.subset(possible, nms), sep = " / "))
    dimnames <- sprintf("[%s]", dimnames)
    dimnames <- list(dimnames, dimnames)
    nrows <- nrow(possible)
    out <- matrix(fill, nrow = nrows, ncol = nrows, dimnames = dimnames)
    idx <- cbind(from_idx$.id, to_idx$.id)
    out[idx] <- value

    # encode groups in attribute
    structure(out, groups = possible, class = "contact_matrix")
}

#' @export
print.contact_matrix <- function(x, ...) {

    # copy so we can return invisibly at end
    original <- x

    # strip groups attribute for printing purposes
    attr(x, "groups") <- NULL

    # print meta information
    d <- dim(x)
    cat(sprintf("dimensions: %d x %d\n", d[1], d[2]))
    groups <- attr(original, "groups")
    groups <- paste(names(groups), collapse = ", ")
    cat(sprintf("groups:     %s\n\n", groups))

    # strip class and from attribute prior to printing
    print(unclass(x), ...)

    # return input invisibly
    invisible(original)
}

