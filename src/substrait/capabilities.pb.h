/* Automatically generated nanopb header */
/* Generated by nanopb-0.4.6 */

#ifndef PB_SUBSTRAIT_SUBSTRAIT_CAPABILITIES_PB_H_INCLUDED
#define PB_SUBSTRAIT_SUBSTRAIT_CAPABILITIES_PB_H_INCLUDED
#include <pb.h>

#if PB_PROTO_HEADER_VERSION != 40
#error Regenerate this file with the current version of nanopb generator.
#endif

/* Struct definitions */
/* Defines a set of Capabilities that a system (producer or consumer) supports. */
typedef struct _substrait_Capabilities { 
    /* List of Substrait versions this system supports */
    pb_size_t substrait_versions_count;
    char **substrait_versions;
    /* list of com.google.Any message types this system supports for advanced
 extensions. */
    pb_size_t advanced_extension_type_urls_count;
    char **advanced_extension_type_urls;
    /* list of simple extensions this system supports. */
    pb_size_t simple_extensions_count;
    struct _substrait_Capabilities_SimpleExtension *simple_extensions;
} substrait_Capabilities;

typedef struct _substrait_Capabilities_SimpleExtension { 
    char *uri;
    pb_size_t function_keys_count;
    char **function_keys;
    pb_size_t type_keys_count;
    char **type_keys;
    pb_size_t type_variation_keys_count;
    char **type_variation_keys;
} substrait_Capabilities_SimpleExtension;


#ifdef __cplusplus
extern "C" {
#endif

/* Initializer values for message structs */
#define substrait_Capabilities_init_default      {0, NULL, 0, NULL, 0, NULL}
#define substrait_Capabilities_SimpleExtension_init_default {NULL, 0, NULL, 0, NULL, 0, NULL}
#define substrait_Capabilities_init_zero         {0, NULL, 0, NULL, 0, NULL}
#define substrait_Capabilities_SimpleExtension_init_zero {NULL, 0, NULL, 0, NULL, 0, NULL}

/* Field tags (for use in manual encoding/decoding) */
#define substrait_Capabilities_substrait_versions_tag 1
#define substrait_Capabilities_advanced_extension_type_urls_tag 2
#define substrait_Capabilities_simple_extensions_tag 3
#define substrait_Capabilities_SimpleExtension_uri_tag 1
#define substrait_Capabilities_SimpleExtension_function_keys_tag 2
#define substrait_Capabilities_SimpleExtension_type_keys_tag 3
#define substrait_Capabilities_SimpleExtension_type_variation_keys_tag 4

/* Struct field encoding specification for nanopb */
#define substrait_Capabilities_FIELDLIST(X, a) \
X(a, POINTER,  REPEATED, STRING,   substrait_versions,   1) \
X(a, POINTER,  REPEATED, STRING,   advanced_extension_type_urls,   2) \
X(a, POINTER,  REPEATED, MESSAGE,  simple_extensions,   3)
#define substrait_Capabilities_CALLBACK NULL
#define substrait_Capabilities_DEFAULT NULL
#define substrait_Capabilities_simple_extensions_MSGTYPE substrait_Capabilities_SimpleExtension

#define substrait_Capabilities_SimpleExtension_FIELDLIST(X, a) \
X(a, POINTER,  SINGULAR, STRING,   uri,               1) \
X(a, POINTER,  REPEATED, STRING,   function_keys,     2) \
X(a, POINTER,  REPEATED, STRING,   type_keys,         3) \
X(a, POINTER,  REPEATED, STRING,   type_variation_keys,   4)
#define substrait_Capabilities_SimpleExtension_CALLBACK NULL
#define substrait_Capabilities_SimpleExtension_DEFAULT NULL

extern const pb_msgdesc_t substrait_Capabilities_msg;
extern const pb_msgdesc_t substrait_Capabilities_SimpleExtension_msg;

/* Defines for backwards compatibility with code written before nanopb-0.4.0 */
#define substrait_Capabilities_fields &substrait_Capabilities_msg
#define substrait_Capabilities_SimpleExtension_fields &substrait_Capabilities_SimpleExtension_msg

/* Maximum encoded size of messages (where known) */
/* substrait_Capabilities_size depends on runtime parameters */
/* substrait_Capabilities_SimpleExtension_size depends on runtime parameters */

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
