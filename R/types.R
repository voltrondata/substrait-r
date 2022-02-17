#' Substrait types
#'
#' @name substrait-type
#' @return An Substrait type object
#' @export
substrait_boolean <- function(){
  substrait$Type$create(bool_ = list())
}

#' @rdname substrait-type
#' @export
substrait_i8 <- function(){
   substrait$Type$create(i8 = list())
}

#' @rdname substrait-type
#' @export
substrait_i16 <- function(){
   substrait$Type$create(i16 = list())
}

#' @rdname substrait-type
#' @export
substrait_i32 <- function(){
   substrait$Type$create(i32 = list())
}

#' @rdname substrait-type
#' @export
substrait_i64 <- function(){
   substrait$Type$create(i64 = list())
}

#' @rdname substrait-type
#' @export
substrait_fp32 <- function(){
   substrait$Type$create(fp32 = list())
}

#' @rdname substrait-type
#' @export
substrait_fp64 <- function(){
   substrait$Type$create(fp64 = list())
}

#' @rdname substrait-type
#' @export
substrait_string <- function(){
  substrait$Type$create(string = list())
}

#' @rdname substrait-type
#' @export
substrait_binary <- function(){
  substrait$Type$create(binary = list())
}

#' @rdname substrait-type
#' @export
substrait_timestamp <- function(){
  substrait$Type$create(timestamp = list())
}

#' @rdname substrait-type
#' @export
substrait_timestamp_tz <- function(){
  substrait$Type$create(timestamp_tz = list())
}

#' @rdname substrait-type
#' @export
substrait_date <- function(){
  substrait$Type$create(date = list())
}

#' @rdname substrait-type
#' @export
substrait_time <- function(){
  substrait$Type$create(time = list())
}

#' @rdname substrait-type
#' @export
substrait_interval_year <- function(){
  substrait$Type$create(interval_year = list())
}

#' @rdname substrait-type
#' @export
substrait_interval_day <- function(){
  substrait$Type$create(interval_day = list())
}

#' @rdname substrait-type
#' @export
substrait_uuid <- function(){
  substrait$Type$create(uuid = list())
}
