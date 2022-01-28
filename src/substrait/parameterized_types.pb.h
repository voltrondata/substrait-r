/* Automatically generated nanopb header */
/* Generated by nanopb-0.4.5 */

#ifndef PB_SUBSTRAIT_SUBSTRAIT_PARAMETERIZED_TYPES_PB_H_INCLUDED
#define PB_SUBSTRAIT_SUBSTRAIT_PARAMETERIZED_TYPES_PB_H_INCLUDED
#include <pb.h>
#include "substrait/type.pb.h"

#if PB_PROTO_HEADER_VERSION != 40
#error Regenerate this file with the current version of nanopb generator.
#endif

/* Struct definitions */
typedef struct _substrait_ParameterizedType { 
    pb_size_t which_kind;
    union {
        pb_callback_t bool_;
        pb_callback_t i8;
        pb_callback_t i16;
        pb_callback_t i32;
        pb_callback_t i64;
        pb_callback_t fp32;
        pb_callback_t fp64;
        pb_callback_t string;
        pb_callback_t binary;
        pb_callback_t timestamp;
        pb_callback_t date;
        pb_callback_t time;
        pb_callback_t interval_year;
        pb_callback_t interval_day;
        pb_callback_t fixed_char;
        pb_callback_t varchar;
        pb_callback_t fixed_binary;
        pb_callback_t decimal;
        pb_callback_t struct_;
        pb_callback_t list;
        pb_callback_t map;
        pb_callback_t timestamp_tz;
        pb_callback_t user_defined_pointer;
        pb_callback_t uuid;
        pb_callback_t type_parameter;
    } kind; 
} substrait_ParameterizedType;

typedef struct _substrait_ParameterizedType_IntegerOption { 
    pb_size_t which_integer_type;
    union {
        pb_callback_t literal;
        pb_callback_t parameter;
    } integer_type; 
} substrait_ParameterizedType_IntegerOption;

typedef struct _substrait_ParameterizedType_IntegerParameter { 
    pb_callback_t name; 
    pb_callback_t range_start_inclusive; 
    pb_callback_t range_end_exclusive; 
} substrait_ParameterizedType_IntegerParameter;

typedef struct _substrait_ParameterizedType_NullableInteger { 
    pb_callback_t value; 
} substrait_ParameterizedType_NullableInteger;

typedef struct _substrait_ParameterizedType_ParameterizedDecimal { 
    pb_callback_t scale; 
    pb_callback_t precision; 
    pb_callback_t variation_pointer; 
    pb_callback_t nullability; 
} substrait_ParameterizedType_ParameterizedDecimal;

typedef struct _substrait_ParameterizedType_ParameterizedFixedBinary { 
    pb_callback_t length; 
    pb_callback_t variation_pointer; 
    pb_callback_t nullability; 
} substrait_ParameterizedType_ParameterizedFixedBinary;

typedef struct _substrait_ParameterizedType_ParameterizedFixedChar { 
    pb_callback_t length; 
    pb_callback_t variation_pointer; 
    pb_callback_t nullability; 
} substrait_ParameterizedType_ParameterizedFixedChar;

typedef struct _substrait_ParameterizedType_ParameterizedList { 
    pb_callback_t type; 
    pb_callback_t variation_pointer; 
    pb_callback_t nullability; 
} substrait_ParameterizedType_ParameterizedList;

typedef struct _substrait_ParameterizedType_ParameterizedMap { 
    pb_callback_t key; 
    pb_callback_t value; 
    pb_callback_t variation_pointer; 
    pb_callback_t nullability; 
} substrait_ParameterizedType_ParameterizedMap;

typedef struct _substrait_ParameterizedType_ParameterizedNamedStruct { 
    pb_callback_t names; 
    pb_callback_t struct_; 
} substrait_ParameterizedType_ParameterizedNamedStruct;

typedef struct _substrait_ParameterizedType_ParameterizedStruct { 
    pb_callback_t types; 
    pb_callback_t variation_pointer; 
    pb_callback_t nullability; 
} substrait_ParameterizedType_ParameterizedStruct;

typedef struct _substrait_ParameterizedType_ParameterizedVarChar { 
    pb_callback_t length; 
    pb_callback_t variation_pointer; 
    pb_callback_t nullability; 
} substrait_ParameterizedType_ParameterizedVarChar;

typedef struct _substrait_ParameterizedType_TypeParameter { 
    pb_callback_t name; 
    pb_callback_t bounds; 
} substrait_ParameterizedType_TypeParameter;


#ifdef __cplusplus
extern "C" {
#endif

/* Initializer values for message structs */
#define substrait_ParameterizedType_init_default {0, {{{NULL}, NULL}}}
#define substrait_ParameterizedType_TypeParameter_init_default {{{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_IntegerParameter_init_default {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_NullableInteger_init_default {{{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedFixedChar_init_default {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedVarChar_init_default {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedFixedBinary_init_default {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedDecimal_init_default {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedStruct_init_default {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedNamedStruct_init_default {{{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedList_init_default {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedMap_init_default {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_IntegerOption_init_default {0, {{{NULL}, NULL}}}
#define substrait_ParameterizedType_init_zero    {0, {{{NULL}, NULL}}}
#define substrait_ParameterizedType_TypeParameter_init_zero {{{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_IntegerParameter_init_zero {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_NullableInteger_init_zero {{{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedFixedChar_init_zero {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedVarChar_init_zero {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedFixedBinary_init_zero {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedDecimal_init_zero {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedStruct_init_zero {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedNamedStruct_init_zero {{{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedList_init_zero {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_ParameterizedMap_init_zero {{{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}, {{NULL}, NULL}}
#define substrait_ParameterizedType_IntegerOption_init_zero {0, {{{NULL}, NULL}}}

/* Field tags (for use in manual encoding/decoding) */
#define substrait_ParameterizedType_bool__tag    1
#define substrait_ParameterizedType_i8_tag       2
#define substrait_ParameterizedType_i16_tag      3
#define substrait_ParameterizedType_i32_tag      5
#define substrait_ParameterizedType_i64_tag      7
#define substrait_ParameterizedType_fp32_tag     10
#define substrait_ParameterizedType_fp64_tag     11
#define substrait_ParameterizedType_string_tag   12
#define substrait_ParameterizedType_binary_tag   13
#define substrait_ParameterizedType_timestamp_tag 14
#define substrait_ParameterizedType_date_tag     16
#define substrait_ParameterizedType_time_tag     17
#define substrait_ParameterizedType_interval_year_tag 19
#define substrait_ParameterizedType_interval_day_tag 20
#define substrait_ParameterizedType_fixed_char_tag 21
#define substrait_ParameterizedType_varchar_tag  22
#define substrait_ParameterizedType_fixed_binary_tag 23
#define substrait_ParameterizedType_decimal_tag  24
#define substrait_ParameterizedType_struct__tag  25
#define substrait_ParameterizedType_list_tag     27
#define substrait_ParameterizedType_map_tag      28
#define substrait_ParameterizedType_timestamp_tz_tag 29
#define substrait_ParameterizedType_user_defined_pointer_tag 31
#define substrait_ParameterizedType_uuid_tag     32
#define substrait_ParameterizedType_type_parameter_tag 33
#define substrait_ParameterizedType_IntegerOption_literal_tag 1
#define substrait_ParameterizedType_IntegerOption_parameter_tag 2
#define substrait_ParameterizedType_IntegerParameter_name_tag 1
#define substrait_ParameterizedType_IntegerParameter_range_start_inclusive_tag 2
#define substrait_ParameterizedType_IntegerParameter_range_end_exclusive_tag 3
#define substrait_ParameterizedType_NullableInteger_value_tag 1
#define substrait_ParameterizedType_ParameterizedDecimal_scale_tag 1
#define substrait_ParameterizedType_ParameterizedDecimal_precision_tag 2
#define substrait_ParameterizedType_ParameterizedDecimal_variation_pointer_tag 3
#define substrait_ParameterizedType_ParameterizedDecimal_nullability_tag 4
#define substrait_ParameterizedType_ParameterizedFixedBinary_length_tag 1
#define substrait_ParameterizedType_ParameterizedFixedBinary_variation_pointer_tag 2
#define substrait_ParameterizedType_ParameterizedFixedBinary_nullability_tag 3
#define substrait_ParameterizedType_ParameterizedFixedChar_length_tag 1
#define substrait_ParameterizedType_ParameterizedFixedChar_variation_pointer_tag 2
#define substrait_ParameterizedType_ParameterizedFixedChar_nullability_tag 3
#define substrait_ParameterizedType_ParameterizedList_type_tag 1
#define substrait_ParameterizedType_ParameterizedList_variation_pointer_tag 2
#define substrait_ParameterizedType_ParameterizedList_nullability_tag 3
#define substrait_ParameterizedType_ParameterizedMap_key_tag 1
#define substrait_ParameterizedType_ParameterizedMap_value_tag 2
#define substrait_ParameterizedType_ParameterizedMap_variation_pointer_tag 3
#define substrait_ParameterizedType_ParameterizedMap_nullability_tag 4
#define substrait_ParameterizedType_ParameterizedNamedStruct_names_tag 1
#define substrait_ParameterizedType_ParameterizedNamedStruct_struct__tag 2
#define substrait_ParameterizedType_ParameterizedStruct_types_tag 1
#define substrait_ParameterizedType_ParameterizedStruct_variation_pointer_tag 2
#define substrait_ParameterizedType_ParameterizedStruct_nullability_tag 3
#define substrait_ParameterizedType_ParameterizedVarChar_length_tag 1
#define substrait_ParameterizedType_ParameterizedVarChar_variation_pointer_tag 2
#define substrait_ParameterizedType_ParameterizedVarChar_nullability_tag 3
#define substrait_ParameterizedType_TypeParameter_name_tag 1
#define substrait_ParameterizedType_TypeParameter_bounds_tag 2

/* Struct field encoding specification for nanopb */
#define substrait_ParameterizedType_FIELDLIST(X, a) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,bool_,kind.bool_),   1) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,i8,kind.i8),   2) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,i16,kind.i16),   3) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,i32,kind.i32),   5) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,i64,kind.i64),   7) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,fp32,kind.fp32),  10) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,fp64,kind.fp64),  11) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,string,kind.string),  12) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,binary,kind.binary),  13) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,timestamp,kind.timestamp),  14) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,date,kind.date),  16) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,time,kind.time),  17) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,interval_year,kind.interval_year),  19) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,interval_day,kind.interval_day),  20) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,fixed_char,kind.fixed_char),  21) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,varchar,kind.varchar),  22) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,fixed_binary,kind.fixed_binary),  23) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,decimal,kind.decimal),  24) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,struct_,kind.struct_),  25) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,list,kind.list),  27) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,map,kind.map),  28) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,timestamp_tz,kind.timestamp_tz),  29) \
X(a, CALLBACK, ONEOF,    UINT32,   (kind,user_defined_pointer,kind.user_defined_pointer),  31) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,uuid,kind.uuid),  32) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (kind,type_parameter,kind.type_parameter),  33)
#define substrait_ParameterizedType_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_DEFAULT NULL
#define substrait_ParameterizedType_kind_bool__MSGTYPE substrait_Type_Boolean
#define substrait_ParameterizedType_kind_i8_MSGTYPE substrait_Type_I8
#define substrait_ParameterizedType_kind_i16_MSGTYPE substrait_Type_I16
#define substrait_ParameterizedType_kind_i32_MSGTYPE substrait_Type_I32
#define substrait_ParameterizedType_kind_i64_MSGTYPE substrait_Type_I64
#define substrait_ParameterizedType_kind_fp32_MSGTYPE substrait_Type_FP32
#define substrait_ParameterizedType_kind_fp64_MSGTYPE substrait_Type_FP64
#define substrait_ParameterizedType_kind_string_MSGTYPE substrait_Type_String
#define substrait_ParameterizedType_kind_binary_MSGTYPE substrait_Type_Binary
#define substrait_ParameterizedType_kind_timestamp_MSGTYPE substrait_Type_Timestamp
#define substrait_ParameterizedType_kind_date_MSGTYPE substrait_Type_Date
#define substrait_ParameterizedType_kind_time_MSGTYPE substrait_Type_Time
#define substrait_ParameterizedType_kind_interval_year_MSGTYPE substrait_Type_IntervalYear
#define substrait_ParameterizedType_kind_interval_day_MSGTYPE substrait_Type_IntervalDay
#define substrait_ParameterizedType_kind_fixed_char_MSGTYPE substrait_ParameterizedType_ParameterizedFixedChar
#define substrait_ParameterizedType_kind_varchar_MSGTYPE substrait_ParameterizedType_ParameterizedVarChar
#define substrait_ParameterizedType_kind_fixed_binary_MSGTYPE substrait_ParameterizedType_ParameterizedFixedBinary
#define substrait_ParameterizedType_kind_decimal_MSGTYPE substrait_ParameterizedType_ParameterizedDecimal
#define substrait_ParameterizedType_kind_struct__MSGTYPE substrait_ParameterizedType_ParameterizedStruct
#define substrait_ParameterizedType_kind_list_MSGTYPE substrait_ParameterizedType_ParameterizedList
#define substrait_ParameterizedType_kind_map_MSGTYPE substrait_ParameterizedType_ParameterizedMap
#define substrait_ParameterizedType_kind_timestamp_tz_MSGTYPE substrait_Type_TimestampTZ
#define substrait_ParameterizedType_kind_uuid_MSGTYPE substrait_Type_UUID
#define substrait_ParameterizedType_kind_type_parameter_MSGTYPE substrait_ParameterizedType_TypeParameter

#define substrait_ParameterizedType_TypeParameter_FIELDLIST(X, a) \
X(a, CALLBACK, SINGULAR, STRING,   name,              1) \
X(a, CALLBACK, REPEATED, MESSAGE,  bounds,            2)
#define substrait_ParameterizedType_TypeParameter_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_TypeParameter_DEFAULT NULL
#define substrait_ParameterizedType_TypeParameter_bounds_MSGTYPE substrait_ParameterizedType

#define substrait_ParameterizedType_IntegerParameter_FIELDLIST(X, a) \
X(a, CALLBACK, SINGULAR, STRING,   name,              1) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  range_start_inclusive,   2) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  range_end_exclusive,   3)
#define substrait_ParameterizedType_IntegerParameter_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_IntegerParameter_DEFAULT NULL
#define substrait_ParameterizedType_IntegerParameter_range_start_inclusive_MSGTYPE substrait_ParameterizedType_NullableInteger
#define substrait_ParameterizedType_IntegerParameter_range_end_exclusive_MSGTYPE substrait_ParameterizedType_NullableInteger

#define substrait_ParameterizedType_NullableInteger_FIELDLIST(X, a) \
X(a, CALLBACK, SINGULAR, INT64,    value,             1)
#define substrait_ParameterizedType_NullableInteger_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_NullableInteger_DEFAULT NULL

#define substrait_ParameterizedType_ParameterizedFixedChar_FIELDLIST(X, a) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  length,            1) \
X(a, CALLBACK, SINGULAR, UINT32,   variation_pointer,   2) \
X(a, CALLBACK, SINGULAR, UENUM,    nullability,       3)
#define substrait_ParameterizedType_ParameterizedFixedChar_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_ParameterizedFixedChar_DEFAULT NULL
#define substrait_ParameterizedType_ParameterizedFixedChar_length_MSGTYPE substrait_ParameterizedType_IntegerOption

#define substrait_ParameterizedType_ParameterizedVarChar_FIELDLIST(X, a) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  length,            1) \
X(a, CALLBACK, SINGULAR, UINT32,   variation_pointer,   2) \
X(a, CALLBACK, SINGULAR, UENUM,    nullability,       3)
#define substrait_ParameterizedType_ParameterizedVarChar_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_ParameterizedVarChar_DEFAULT NULL
#define substrait_ParameterizedType_ParameterizedVarChar_length_MSGTYPE substrait_ParameterizedType_IntegerOption

#define substrait_ParameterizedType_ParameterizedFixedBinary_FIELDLIST(X, a) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  length,            1) \
X(a, CALLBACK, SINGULAR, UINT32,   variation_pointer,   2) \
X(a, CALLBACK, SINGULAR, UENUM,    nullability,       3)
#define substrait_ParameterizedType_ParameterizedFixedBinary_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_ParameterizedFixedBinary_DEFAULT NULL
#define substrait_ParameterizedType_ParameterizedFixedBinary_length_MSGTYPE substrait_ParameterizedType_IntegerOption

#define substrait_ParameterizedType_ParameterizedDecimal_FIELDLIST(X, a) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  scale,             1) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  precision,         2) \
X(a, CALLBACK, SINGULAR, UINT32,   variation_pointer,   3) \
X(a, CALLBACK, SINGULAR, UENUM,    nullability,       4)
#define substrait_ParameterizedType_ParameterizedDecimal_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_ParameterizedDecimal_DEFAULT NULL
#define substrait_ParameterizedType_ParameterizedDecimal_scale_MSGTYPE substrait_ParameterizedType_IntegerOption
#define substrait_ParameterizedType_ParameterizedDecimal_precision_MSGTYPE substrait_ParameterizedType_IntegerOption

#define substrait_ParameterizedType_ParameterizedStruct_FIELDLIST(X, a) \
X(a, CALLBACK, REPEATED, MESSAGE,  types,             1) \
X(a, CALLBACK, SINGULAR, UINT32,   variation_pointer,   2) \
X(a, CALLBACK, SINGULAR, UENUM,    nullability,       3)
#define substrait_ParameterizedType_ParameterizedStruct_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_ParameterizedStruct_DEFAULT NULL
#define substrait_ParameterizedType_ParameterizedStruct_types_MSGTYPE substrait_ParameterizedType

#define substrait_ParameterizedType_ParameterizedNamedStruct_FIELDLIST(X, a) \
X(a, CALLBACK, REPEATED, STRING,   names,             1) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  struct_,           2)
#define substrait_ParameterizedType_ParameterizedNamedStruct_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_ParameterizedNamedStruct_DEFAULT NULL
#define substrait_ParameterizedType_ParameterizedNamedStruct_struct__MSGTYPE substrait_ParameterizedType_ParameterizedStruct

#define substrait_ParameterizedType_ParameterizedList_FIELDLIST(X, a) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  type,              1) \
X(a, CALLBACK, SINGULAR, UINT32,   variation_pointer,   2) \
X(a, CALLBACK, SINGULAR, UENUM,    nullability,       3)
#define substrait_ParameterizedType_ParameterizedList_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_ParameterizedList_DEFAULT NULL
#define substrait_ParameterizedType_ParameterizedList_type_MSGTYPE substrait_ParameterizedType

#define substrait_ParameterizedType_ParameterizedMap_FIELDLIST(X, a) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  key,               1) \
X(a, CALLBACK, OPTIONAL, MESSAGE,  value,             2) \
X(a, CALLBACK, SINGULAR, UINT32,   variation_pointer,   3) \
X(a, CALLBACK, SINGULAR, UENUM,    nullability,       4)
#define substrait_ParameterizedType_ParameterizedMap_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_ParameterizedMap_DEFAULT NULL
#define substrait_ParameterizedType_ParameterizedMap_key_MSGTYPE substrait_ParameterizedType
#define substrait_ParameterizedType_ParameterizedMap_value_MSGTYPE substrait_ParameterizedType

#define substrait_ParameterizedType_IntegerOption_FIELDLIST(X, a) \
X(a, CALLBACK, ONEOF,    INT32,    (integer_type,literal,integer_type.literal),   1) \
X(a, CALLBACK, ONEOF,    MESSAGE,  (integer_type,parameter,integer_type.parameter),   2)
#define substrait_ParameterizedType_IntegerOption_CALLBACK pb_default_field_callback
#define substrait_ParameterizedType_IntegerOption_DEFAULT NULL
#define substrait_ParameterizedType_IntegerOption_integer_type_parameter_MSGTYPE substrait_ParameterizedType_IntegerParameter

extern const pb_msgdesc_t substrait_ParameterizedType_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_TypeParameter_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_IntegerParameter_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_NullableInteger_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_ParameterizedFixedChar_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_ParameterizedVarChar_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_ParameterizedFixedBinary_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_ParameterizedDecimal_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_ParameterizedStruct_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_ParameterizedNamedStruct_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_ParameterizedList_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_ParameterizedMap_msg;
extern const pb_msgdesc_t substrait_ParameterizedType_IntegerOption_msg;

/* Defines for backwards compatibility with code written before nanopb-0.4.0 */
#define substrait_ParameterizedType_fields &substrait_ParameterizedType_msg
#define substrait_ParameterizedType_TypeParameter_fields &substrait_ParameterizedType_TypeParameter_msg
#define substrait_ParameterizedType_IntegerParameter_fields &substrait_ParameterizedType_IntegerParameter_msg
#define substrait_ParameterizedType_NullableInteger_fields &substrait_ParameterizedType_NullableInteger_msg
#define substrait_ParameterizedType_ParameterizedFixedChar_fields &substrait_ParameterizedType_ParameterizedFixedChar_msg
#define substrait_ParameterizedType_ParameterizedVarChar_fields &substrait_ParameterizedType_ParameterizedVarChar_msg
#define substrait_ParameterizedType_ParameterizedFixedBinary_fields &substrait_ParameterizedType_ParameterizedFixedBinary_msg
#define substrait_ParameterizedType_ParameterizedDecimal_fields &substrait_ParameterizedType_ParameterizedDecimal_msg
#define substrait_ParameterizedType_ParameterizedStruct_fields &substrait_ParameterizedType_ParameterizedStruct_msg
#define substrait_ParameterizedType_ParameterizedNamedStruct_fields &substrait_ParameterizedType_ParameterizedNamedStruct_msg
#define substrait_ParameterizedType_ParameterizedList_fields &substrait_ParameterizedType_ParameterizedList_msg
#define substrait_ParameterizedType_ParameterizedMap_fields &substrait_ParameterizedType_ParameterizedMap_msg
#define substrait_ParameterizedType_IntegerOption_fields &substrait_ParameterizedType_IntegerOption_msg

/* Maximum encoded size of messages (where known) */
/* substrait_ParameterizedType_size depends on runtime parameters */
/* substrait_ParameterizedType_TypeParameter_size depends on runtime parameters */
/* substrait_ParameterizedType_IntegerParameter_size depends on runtime parameters */
/* substrait_ParameterizedType_NullableInteger_size depends on runtime parameters */
/* substrait_ParameterizedType_ParameterizedFixedChar_size depends on runtime parameters */
/* substrait_ParameterizedType_ParameterizedVarChar_size depends on runtime parameters */
/* substrait_ParameterizedType_ParameterizedFixedBinary_size depends on runtime parameters */
/* substrait_ParameterizedType_ParameterizedDecimal_size depends on runtime parameters */
/* substrait_ParameterizedType_ParameterizedStruct_size depends on runtime parameters */
/* substrait_ParameterizedType_ParameterizedNamedStruct_size depends on runtime parameters */
/* substrait_ParameterizedType_ParameterizedList_size depends on runtime parameters */
/* substrait_ParameterizedType_ParameterizedMap_size depends on runtime parameters */
/* substrait_ParameterizedType_IntegerOption_size depends on runtime parameters */

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif