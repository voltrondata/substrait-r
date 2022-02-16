boolean <- function(){
  substrait$Type$create(bool_ = substrait$Type$Boolean$create())
}

i8 <- function(){
   substrait$Type$create(i8 = substrait$Type$I8$create())
}

i16 <- function(){
   substrait$Type$create(i16 = substrait$Type$I16$create())
}

i32 <- function(){
   substrait$Type$create(i32 = substrait$Type$I32$create())
}

i64 <- function(){
   substrait$Type$create(i64 = substrait$Type$I64$create())
}

fp32 <- function(){
   substrait$Type$create(fp32 = substrait$Type$FP32$create())
}

fp64 <- function(){
   substrait$Type$create(fp64 = substrait$Type$FP64$create())
}

# but rlang::string
string <- function(){
  substrait$Type$create(string = substrait$Type$String$create())
}

# arrow also has binary type
binary <- function(){
  substrait$Type$create(binary = substrait$Type$Binary$create())
}

# but utils::timestamp
timestamp <- function(){
  substrait$Type$create(timestamp = substrait$Type$Timestamp$create())
}

timestamp_tz <- function(){
  substrait$Type$create(timestamp_tz = substrait$Type$TimestampTZ$create())
}

# but base::date
date <- function(){
  substrait$Type$create(date = substrait$Type$Date$create())
}

# but stats::time
time <- function(){
  substrait$Type$create(time = substrait$Type$Time$create())
}

interval_year <- function(){
  substrait$Type$create(interval_year = substrait$Type$IntervalYear$create())
}

interval_day <- function(){
  substrait$Type$create(interval_day = substrait$Type$IntervalDay$create())
}

uuid <- function(){
  substrait$Type$create(uuid = substrait$Type$UUID$create())
}
