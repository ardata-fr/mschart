# utils/tools ----
get_series_names <- function(x) {
  names_ <- character(0)
  if (!is.null(x$group)) {
    data_grps <- x$data[[x$group]]
    if (is.factor(data_grps)) {
      names_ <- levels(data_grps)
    } else if (is.character(data_grps)) {
      names_ <- sort(unique(data_grps))
    } else {
      names_ <- as.character(sort(unique(data_grps)))
    }
  } else {
    names_ <- x$y
  }
  names_
}

get_label_names <- function(x) {
  if (is.null(x$label_cols)) {
    return(character(0))
  }

  names_ <- character(0)
  if (!is.null(x$group)) {
    data_grps <- x$data[[x$group]]
    if (is.factor(data_grps)) {
      names_ <- levels(data_grps)
    } else if (is.character(data_grps)) {
      names_ <- sort(unique(data_grps))
    } else {
      names_ <- as.character(sort(unique(data_grps)))
    }
    names_ <- paste0(x$label_cols[1], "-", names_)
  } else {
    names_ <- x$label_cols[1]
  }
}

# 2 different transpose ops -----
#' @importFrom data.table setorderv rbindlist setDF
transpose_data <- function(data, vars, group) {
  list_data <- split(data[vars], data[[group]])
  group_names <- names(list_data)
  list_data <- mapply(
    function(name, x) {
      colnames(x)[2] <- name
      x
    },
    name = group_names,
    x = list_data,
    SIMPLIFY = FALSE
  )
  out <- rbindlist(list_data, use.names = TRUE, fill = TRUE)
  setDF(out)
  out
}


#' @importFrom data.table := .N
dcast_data <- function(data, x, y, group) {
  dataset <- as.data.table(data)
  dataset <- dataset[, c(".fake_id.") := list(seq_len(.N)), by = c(x, group)]
  form_str <- sprintf("%s + .fake_id. ~ %s", x, group)
  out <- dcast.data.table(
    dataset,
    formula = as.formula(form_str),
    fill = NA, value.var = y
  )
  out$.fake_id. <- NULL
  setDF(out)
  out
}

# main functions -----
transpose_series_bysplit <- function(x) {
  if (!is.null(x$group)) {
    vars <- c(x$x, x$y)
    out <- transpose_data(x$data, vars, x$group)
    group_names <- setdiff(colnames(out), x$x)
    if (!is.null(x$label_cols)) {
      for (lab in x$label_cols) {
        data_label <- transpose_data(x$data, c(x$x, lab), x$group)
        data_label[[1]] <- NULL
        names(data_label) <- paste0(lab, "-", names(data_label))
        out <- cbind(out, data_label)
      }
    }
  } else {
    vars <- c(x$x, x$y, x$label_cols)
    out <- x$data[, vars]
  }
  out
}




#' @importFrom stats as.formula
#' @importFrom data.table as.data.table dcast.data.table setorderv setnames
shape_as_series <- function(x) {
  if (!is.null(x$group)) {
    out <- dcast_data(data = x$data, x = x$x, y = x$y, group = x$group)
    if (!is.null(x$label_cols)) {
      for (lab in x$label_cols) {
        data_label <- dcast_data(data = x$data, x = x$x, y = lab, group = x$group)
        data_label[[1]] <- NULL
        names(data_label) <- paste0(lab, "-", names(data_label))
        out <- cbind(out, data_label)
      }
    }
  } else {
    out <- x$data[, c(x$x, x$y, x$label_cols)]
    # if( !is.null(x$label_cols)){
    #   wlabs <- which(names(out) %in% x$label_cols)
    #   names(out)[wlabs] <- paste0(label_cols, "-", names(out)[wlabs])
    # }
  }
  as.data.frame(out)
}
