
#' Substrait types
#'
#' @export
#' @name substrait-type
#' @return A substrait.Type proto object
#'
#' @examples
#' substrait_boolean()
#' substrait_i32()
#' substrait_fp64()
#' substrait_string()
#'
substrait_boolean <- function() {
  substrait$Type$create(bool_ = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_i8 <- function() {
  substrait$Type$create(i8 = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_i16 <- function() {
  substrait$Type$create(i16 = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_i32 <- function() {
  substrait$Type$create(i32 = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_i64 <- function() {
  substrait$Type$create(i64 = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_fp32 <- function() {
  substrait$Type$create(fp32 = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_fp64 <- function() {
  substrait$Type$create(fp64 = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_string <- function() {
  substrait$Type$create(string = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_binary <- function() {
  substrait$Type$create(binary = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_timestamp <- function() {
  substrait$Type$create(timestamp = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_timestamp_tz <- function() {
  substrait$Type$create(timestamp_tz = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_date <- function() {
  substrait$Type$create(date = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_time <- function() {
  substrait$Type$create(time = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_interval_year <- function() {
  substrait$Type$create(interval_year = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_interval_day <- function() {
  substrait$Type$create(interval_day = substrait_proto_auto())
}

#' @rdname substrait-type
#' @export
substrait_uuid <- function() {
  substrait$Type$create(uuid = substrait_proto_auto())
}
