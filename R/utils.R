#' Return a Value in a Nested List by Searching for Its Name
#'
#' @param nested_list List with elements that are nested lists
#' @param target_variable_name The name of the element to get the value
#'
#' @returns Value associated with name
#' @export
#'
find_variable_in_nested_list <- function(nested_list, target_variable_name) {
  # Initialize a variable to store the found element, if any
  found_element <- NULL

  # Iterate through each element in the current level of the list
  for (item in nested_list) {
    # Check if the current item is a list (indicating further nesting)
    if (is.list(item)) {
      # If it's a list, recursively call the function to search within it
      found_element <- find_variable_in_nested_list(item, target_variable_name)
      # If the variable is found in the nested list, return it immediately
      if (!is.null(found_element)) {
        return(found_element)
      }
    } else {
      # If it's not a list, check if it's the target variable
      # This assumes the target is a named element within a list
      if (!is.null(names(nested_list)) && target_variable_name %in% names(nested_list)) {
        # If the current item's name matches the target variable name
        # and the item itself is part of the current 'nested_list'
        # (this handles cases where the target_variable_name might be a value, not a name)
        if (identical(item, nested_list[[target_variable_name]])) {
          return(item)
        }
      }
    }
  }
  # If the loop finishes and the variable is not found in this level or its nested lists
  return(found_element)
}


#' Search a Nested List to Get a List and its Respective Elements
#'
#' @param nested_list a list with nested elements of lists
#' @param target_name identifier for list(s)
#'
#' @returns list with elements all lists matching the target_name
#' @export
#'
#' @examples nested_list_example <- list(
#'   teamA = list(
#'     offense = list(
#'       quarterback = "Patrick Mahomes",
#'       running_back = "Isiah Pacheco",
#'       wide_receivers = c("Rashee Rice", "Hollywood Brown")
#'     ),
#'     defense = list(
#'       linebacker = "Nick Bolton",
#'       cornerbacks = c("Trent McDuffie", "Joshua Williams")
#'     )
#'   ),
#'   teamB = list(
#'     offense = list(
#'       quarterback = "Josh Allen",
#'       running_back = "James Cook",
#'       wide_receivers = c("Stefon Diggs", "Khalil Shakir")
#'     ),
#'     defense = list(
#'       linebacker = "Matt Milano",
#'       cornerbacks = c("Kaiir Elam", "Taron Johnson")
#'     )
#'   )
#' )
#' find_lists_with_name(nested_list_example, "offense")
find_lists_with_name <- function(nested_list, target_name) {
  # If not a list, stop recursion
  if (!is.list(nested_list)) {
    return(list())
  }

  results <- list()

  # If current list has the target name, add that sublist
  if (!is.null(names(nested_list)) && target_name %in% names(nested_list)) {
    results <- c(results, list(nested_list[[target_name]]))
  }

  # Recurse into all sublists
  for (item in nested_list) {
    if (is.list(item)) {
      sub_results <- find_lists_with_name(item, target_name)
      if (length(sub_results) > 0) {
        results <- c(results, sub_results)
      }
    }
  }

  return(results)
}


#' Bind Rows of List Elements
#'
#' @param input_list a list of tibbles that will be appended by row
#'
#' @returns Dataframe of each list element as a row
#' @export
#'
bindRows <- function(input_list) {
  # 1. Find all unique column names
  all_cols <- unique(unlist(lapply(input_list, names)))

  # 2. Add missing columns to each data frame
  df_list <- lapply(input_list, function(df) {
    missing <- setdiff(all_cols, names(df))
    if (length(missing) > 0) df[missing] <- NA
    df[all_cols] # reorder columns consistently
  })

  # 3. Combine using rbind
  df <- do.call(rbind, df_list)
  rownames(df) <- NULL # optional, reset rownames

  return(df)
}


#' Clean and Flatten Yahoo API List
#'
#' Safely converts nested Yahoo Fantasy API lists to a tibble,
#' replacing NULL values with NA.
#'
#' @param input_list A nested list returned from a Yahoo Fantasy API endpoint.
#' @returns A tibble with NULLs replaced by NAs.
#' @keywords internal
#'
clean_yahoo_list <- function(input_list) {
  return_list <- purrr::modify(input_list, ~ if (is.null(.x)) NA else .x)
  return_list <- purrr::lmap(return_list, tibble::as_tibble) |>
    purrr::list_cbind()
  return(return_list)
}
