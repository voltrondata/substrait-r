
#' @rdname substrait_create
#' @export
substrait <- list(
  AggregationPhase = list(
    AGGREGATION_PHASE_UNSPECIFIED = structure(0L, class = c("substrait_AggregationPhase", "substrait_proto_enum", "substrait_proto")),
    AGGREGATION_PHASE_INITIAL_TO_INTERMEDIATE = structure(1L, class = c("substrait_AggregationPhase", "substrait_proto_enum", "substrait_proto")),
    AGGREGATION_PHASE_INTERMEDIATE_TO_INTERMEDIATE = structure(2L, class = c("substrait_AggregationPhase", "substrait_proto_enum", "substrait_proto")),
    AGGREGATION_PHASE_INITIAL_TO_RESULT = structure(3L, class = c("substrait_AggregationPhase", "substrait_proto_enum", "substrait_proto")),
    AGGREGATION_PHASE_INTERMEDIATE_TO_RESULT = structure(4L, class = c("substrait_AggregationPhase", "substrait_proto_enum", "substrait_proto")),
    create = function(value) {
      create_substrait_enum(
        value,
        .qualified_name = "substrait.AggregationPhase"
      )
    }
  ),
  AggregateFunction = list(
    AggregationInvocation = list(
      AGGREGATION_INVOCATION_UNSPECIFIED = structure(0L, class = c("substrait_AggregateFunction_AggregationInvocation", "substrait_proto_enum", "substrait_proto")),
      AGGREGATION_INVOCATION_ALL = structure(1L, class = c("substrait_AggregateFunction_AggregationInvocation", "substrait_proto_enum", "substrait_proto")),
      AGGREGATION_INVOCATION_DISTINCT = structure(2L, class = c("substrait_AggregateFunction_AggregationInvocation", "substrait_proto_enum", "substrait_proto")),
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.AggregateFunction.AggregationInvocation"
        )
      }
    ),
    ReferenceRel = list(
      create = function(..., `subtree_ordinal` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `subtree_ordinal` = clean_value(`subtree_ordinal`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          .qualified_name = "substrait.AggregateFunction.ReferenceRel"
        )
      }
    ),
    create = function(..., `function_reference` = arg_unspecified(), `arguments` = arg_unspecified(), `options` = arg_unspecified(), `output_type` = arg_unspecified(), `phase` = arg_unspecified(), `sorts` = arg_unspecified(), `invocation` = arg_unspecified(), `args` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `function_reference` = clean_value(`function_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        `arguments` = clean_value(`arguments`, "TYPE_MESSAGE", "substrait.FunctionArgument", repeated = TRUE),
        `options` = clean_value(`options`, "TYPE_MESSAGE", "substrait.FunctionOption", repeated = TRUE),
        `output_type` = clean_value(`output_type`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
        `phase` = clean_value(`phase`, "TYPE_ENUM", "substrait.AggregationPhase", repeated = FALSE),
        `sorts` = clean_value(`sorts`, "TYPE_MESSAGE", "substrait.SortField", repeated = TRUE),
        `invocation` = clean_value(`invocation`, "TYPE_ENUM", "substrait.AggregateFunction.AggregationInvocation", repeated = FALSE),
        `args` = clean_value(`args`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
        .qualified_name = "substrait.AggregateFunction"
      )
    }
  ),
  AggregateRel = list(
    Grouping = list(
      create = function(..., `grouping_expressions` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `grouping_expressions` = clean_value(`grouping_expressions`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          .qualified_name = "substrait.AggregateRel.Grouping"
        )
      }
    ),
    Measure = list(
      create = function(..., `measure` = arg_unspecified(), `filter` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `measure` = clean_value(`measure`, "TYPE_MESSAGE", "substrait.AggregateFunction", repeated = FALSE),
          `filter` = clean_value(`filter`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.AggregateRel.Measure"
        )
      }
    ),
    create = function(..., `common` = arg_unspecified(), `input` = arg_unspecified(), `groupings` = arg_unspecified(), `measures` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `groupings` = clean_value(`groupings`, "TYPE_MESSAGE", "substrait.AggregateRel.Grouping", repeated = TRUE),
        `measures` = clean_value(`measures`, "TYPE_MESSAGE", "substrait.AggregateRel.Measure", repeated = TRUE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.AggregateRel"
      )
    }
  ),
  Capabilities = list(
    SimpleExtension = list(
      create = function(..., `uri` = arg_unspecified(), `function_keys` = arg_unspecified(), `type_keys` = arg_unspecified(), `type_variation_keys` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `uri` = clean_value(`uri`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `function_keys` = clean_value(`function_keys`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          `type_keys` = clean_value(`type_keys`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          `type_variation_keys` = clean_value(`type_variation_keys`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          .qualified_name = "substrait.Capabilities.SimpleExtension"
        )
      }
    ),
    create = function(..., `substrait_versions` = arg_unspecified(), `advanced_extension_type_urls` = arg_unspecified(), `simple_extensions` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `substrait_versions` = clean_value(`substrait_versions`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        `advanced_extension_type_urls` = clean_value(`advanced_extension_type_urls`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        `simple_extensions` = clean_value(`simple_extensions`, "TYPE_MESSAGE", "substrait.Capabilities.SimpleExtension", repeated = TRUE),
        .qualified_name = "substrait.Capabilities"
      )
    }
  ),
  CrossRel = list(
    create = function(..., `common` = arg_unspecified(), `left` = arg_unspecified(), `right` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `left` = clean_value(`left`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `right` = clean_value(`right`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.CrossRel"
      )
    }
  ),
  DdlRel = list(
    DdlObject = list(
      DDL_OBJECT_UNSPECIFIED = structure(0L, class = c("substrait_DdlRel_DdlObject", "substrait_proto_enum", "substrait_proto")),
      DDL_OBJECT_TABLE = structure(1L, class = c("substrait_DdlRel_DdlObject", "substrait_proto_enum", "substrait_proto")),
      DDL_OBJECT_VIEW = structure(2L, class = c("substrait_DdlRel_DdlObject", "substrait_proto_enum", "substrait_proto")),
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.DdlRel.DdlObject"
        )
      }
    ),
    DdlOp = list(
      DDL_OP_UNSPECIFIED = structure(0L, class = c("substrait_DdlRel_DdlOp", "substrait_proto_enum", "substrait_proto")),
      DDL_OP_CREATE = structure(1L, class = c("substrait_DdlRel_DdlOp", "substrait_proto_enum", "substrait_proto")),
      DDL_OP_CREATE_OR_REPLACE = structure(2L, class = c("substrait_DdlRel_DdlOp", "substrait_proto_enum", "substrait_proto")),
      DDL_OP_ALTER = structure(3L, class = c("substrait_DdlRel_DdlOp", "substrait_proto_enum", "substrait_proto")),
      DDL_OP_DROP = structure(4L, class = c("substrait_DdlRel_DdlOp", "substrait_proto_enum", "substrait_proto")),
      DDL_OP_DROP_IF_EXIST = structure(5L, class = c("substrait_DdlRel_DdlOp", "substrait_proto_enum", "substrait_proto")),
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.DdlRel.DdlOp"
        )
      }
    ),
    create = function(..., `named_object` = arg_unspecified(), `extension_object` = arg_unspecified(), `table_schema` = arg_unspecified(), `table_defaults` = arg_unspecified(), `object` = arg_unspecified(), `op` = arg_unspecified(), `view_definition` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `named_object` = clean_value(`named_object`, "TYPE_MESSAGE", "substrait.NamedObjectWrite", repeated = FALSE),
        `extension_object` = clean_value(`extension_object`, "TYPE_MESSAGE", "substrait.ExtensionObject", repeated = FALSE),
        `table_schema` = clean_value(`table_schema`, "TYPE_MESSAGE", "substrait.NamedStruct", repeated = FALSE),
        `table_defaults` = clean_value(`table_defaults`, "TYPE_MESSAGE", "substrait.Expression.Literal.Struct", repeated = FALSE),
        `object` = clean_value(`object`, "TYPE_ENUM", "substrait.DdlRel.DdlObject", repeated = FALSE),
        `op` = clean_value(`op`, "TYPE_ENUM", "substrait.DdlRel.DdlOp", repeated = FALSE),
        `view_definition` = clean_value(`view_definition`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        .qualified_name = "substrait.DdlRel"
      )
    }
  ),
  DerivationExpression = list(
    BinaryOp = list(
      BinaryOpType = list(
        BINARY_OP_TYPE_UNSPECIFIED = structure(0L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_PLUS = structure(1L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_MINUS = structure(2L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_MULTIPLY = structure(3L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_DIVIDE = structure(4L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_MIN = structure(5L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_MAX = structure(6L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_GREATER_THAN = structure(7L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_LESS_THAN = structure(8L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_AND = structure(9L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_OR = structure(10L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_EQUALS = structure(11L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        BINARY_OP_TYPE_COVERS = structure(12L, class = c("substrait_DerivationExpression_BinaryOp_BinaryOpType", "substrait_proto_enum", "substrait_proto")),
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.DerivationExpression.BinaryOp.BinaryOpType"
          )
        }
      ),
      create = function(..., `op_type` = arg_unspecified(), `arg1` = arg_unspecified(), `arg2` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `op_type` = clean_value(`op_type`, "TYPE_ENUM", "substrait.DerivationExpression.BinaryOp.BinaryOpType", repeated = FALSE),
          `arg1` = clean_value(`arg1`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `arg2` = clean_value(`arg2`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.BinaryOp"
        )
      }
    ),
    ExpressionDecimal = list(
      create = function(..., `scale` = arg_unspecified(), `precision` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `scale` = clean_value(`scale`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `precision` = clean_value(`precision`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionDecimal"
        )
      }
    ),
    ExpressionFixedBinary = list(
      create = function(..., `length` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `length` = clean_value(`length`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionFixedBinary"
        )
      }
    ),
    ExpressionFixedChar = list(
      create = function(..., `length` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `length` = clean_value(`length`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionFixedChar"
        )
      }
    ),
    ExpressionList = list(
      create = function(..., `type` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type` = clean_value(`type`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionList"
        )
      }
    ),
    ExpressionMap = list(
      create = function(..., `key` = arg_unspecified(), `value` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `key` = clean_value(`key`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `value` = clean_value(`value`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionMap"
        )
      }
    ),
    ExpressionNamedStruct = list(
      create = function(..., `names` = arg_unspecified(), `struct` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `names` = clean_value(`names`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          `struct` = clean_value(`struct`, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionStruct", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionNamedStruct"
        )
      }
    ),
    ExpressionStruct = list(
      create = function(..., `types` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `types` = clean_value(`types`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = TRUE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionStruct"
        )
      }
    ),
    ExpressionUserDefined = list(
      create = function(..., `type_pointer` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_pointer` = clean_value(`type_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionUserDefined"
        )
      }
    ),
    ExpressionVarChar = list(
      create = function(..., `length` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `length` = clean_value(`length`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionVarChar"
        )
      }
    ),
    IfElse = list(
      create = function(..., `if_condition` = arg_unspecified(), `if_return` = arg_unspecified(), `else_return` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `if_condition` = clean_value(`if_condition`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `if_return` = clean_value(`if_return`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `else_return` = clean_value(`else_return`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.IfElse"
        )
      }
    ),
    ReturnProgram = list(
      Assignment = list(
        create = function(..., `name` = arg_unspecified(), `expression` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            `expression` = clean_value(`expression`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
            .qualified_name = "substrait.DerivationExpression.ReturnProgram.Assignment"
          )
        }
      ),
      create = function(..., `assignments` = arg_unspecified(), `final_expression` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `assignments` = clean_value(`assignments`, "TYPE_MESSAGE", "substrait.DerivationExpression.ReturnProgram.Assignment", repeated = TRUE),
          `final_expression` = clean_value(`final_expression`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ReturnProgram"
        )
      }
    ),
    UnaryOp = list(
      UnaryOpType = list(
        UNARY_OP_TYPE_UNSPECIFIED = structure(0L, class = c("substrait_DerivationExpression_UnaryOp_UnaryOpType", "substrait_proto_enum", "substrait_proto")),
        UNARY_OP_TYPE_BOOLEAN_NOT = structure(1L, class = c("substrait_DerivationExpression_UnaryOp_UnaryOpType", "substrait_proto_enum", "substrait_proto")),
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.DerivationExpression.UnaryOp.UnaryOpType"
          )
        }
      ),
      create = function(..., `op_type` = arg_unspecified(), `arg` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `op_type` = clean_value(`op_type`, "TYPE_ENUM", "substrait.DerivationExpression.UnaryOp.UnaryOpType", repeated = FALSE),
          `arg` = clean_value(`arg`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.UnaryOp"
        )
      }
    ),
    create = function(..., `bool` = arg_unspecified(), `i8` = arg_unspecified(), `i16` = arg_unspecified(), `i32` = arg_unspecified(), `i64` = arg_unspecified(), `fp32` = arg_unspecified(), `fp64` = arg_unspecified(), `string` = arg_unspecified(), `binary` = arg_unspecified(), `timestamp` = arg_unspecified(), `date` = arg_unspecified(), `time` = arg_unspecified(), `interval_year` = arg_unspecified(), `interval_day` = arg_unspecified(), `timestamp_tz` = arg_unspecified(), `uuid` = arg_unspecified(), `fixed_char` = arg_unspecified(), `varchar` = arg_unspecified(), `fixed_binary` = arg_unspecified(), `decimal` = arg_unspecified(), `struct` = arg_unspecified(), `list` = arg_unspecified(), `map` = arg_unspecified(), `user_defined` = arg_unspecified(), `user_defined_pointer` = arg_unspecified(), `type_parameter_name` = arg_unspecified(), `integer_parameter_name` = arg_unspecified(), `integer_literal` = arg_unspecified(), `unary_op` = arg_unspecified(), `binary_op` = arg_unspecified(), `if_else` = arg_unspecified(), `return_program` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `bool` = clean_value(`bool`, "TYPE_MESSAGE", "substrait.Type.Boolean", repeated = FALSE),
        `i8` = clean_value(`i8`, "TYPE_MESSAGE", "substrait.Type.I8", repeated = FALSE),
        `i16` = clean_value(`i16`, "TYPE_MESSAGE", "substrait.Type.I16", repeated = FALSE),
        `i32` = clean_value(`i32`, "TYPE_MESSAGE", "substrait.Type.I32", repeated = FALSE),
        `i64` = clean_value(`i64`, "TYPE_MESSAGE", "substrait.Type.I64", repeated = FALSE),
        `fp32` = clean_value(`fp32`, "TYPE_MESSAGE", "substrait.Type.FP32", repeated = FALSE),
        `fp64` = clean_value(`fp64`, "TYPE_MESSAGE", "substrait.Type.FP64", repeated = FALSE),
        `string` = clean_value(`string`, "TYPE_MESSAGE", "substrait.Type.String", repeated = FALSE),
        `binary` = clean_value(`binary`, "TYPE_MESSAGE", "substrait.Type.Binary", repeated = FALSE),
        `timestamp` = clean_value(`timestamp`, "TYPE_MESSAGE", "substrait.Type.Timestamp", repeated = FALSE),
        `date` = clean_value(`date`, "TYPE_MESSAGE", "substrait.Type.Date", repeated = FALSE),
        `time` = clean_value(`time`, "TYPE_MESSAGE", "substrait.Type.Time", repeated = FALSE),
        `interval_year` = clean_value(`interval_year`, "TYPE_MESSAGE", "substrait.Type.IntervalYear", repeated = FALSE),
        `interval_day` = clean_value(`interval_day`, "TYPE_MESSAGE", "substrait.Type.IntervalDay", repeated = FALSE),
        `timestamp_tz` = clean_value(`timestamp_tz`, "TYPE_MESSAGE", "substrait.Type.TimestampTZ", repeated = FALSE),
        `uuid` = clean_value(`uuid`, "TYPE_MESSAGE", "substrait.Type.UUID", repeated = FALSE),
        `fixed_char` = clean_value(`fixed_char`, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionFixedChar", repeated = FALSE),
        `varchar` = clean_value(`varchar`, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionVarChar", repeated = FALSE),
        `fixed_binary` = clean_value(`fixed_binary`, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionFixedBinary", repeated = FALSE),
        `decimal` = clean_value(`decimal`, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionDecimal", repeated = FALSE),
        `struct` = clean_value(`struct`, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionStruct", repeated = FALSE),
        `list` = clean_value(`list`, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionList", repeated = FALSE),
        `map` = clean_value(`map`, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionMap", repeated = FALSE),
        `user_defined` = clean_value(`user_defined`, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionUserDefined", repeated = FALSE),
        `user_defined_pointer` = clean_value(`user_defined_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        `type_parameter_name` = clean_value(`type_parameter_name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
        `integer_parameter_name` = clean_value(`integer_parameter_name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
        `integer_literal` = clean_value(`integer_literal`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
        `unary_op` = clean_value(`unary_op`, "TYPE_MESSAGE", "substrait.DerivationExpression.UnaryOp", repeated = FALSE),
        `binary_op` = clean_value(`binary_op`, "TYPE_MESSAGE", "substrait.DerivationExpression.BinaryOp", repeated = FALSE),
        `if_else` = clean_value(`if_else`, "TYPE_MESSAGE", "substrait.DerivationExpression.IfElse", repeated = FALSE),
        `return_program` = clean_value(`return_program`, "TYPE_MESSAGE", "substrait.DerivationExpression.ReturnProgram", repeated = FALSE),
        .qualified_name = "substrait.DerivationExpression"
      )
    }
  ),
  ExchangeRel = list(
    Broadcast = list(
      create = function(...) {
        rlang::check_dots_empty()
        create_substrait_message(.qualified_name = "substrait.ExchangeRel.Broadcast")
      }
    ),
    ExchangeTarget = list(
      create = function(..., `partition_id` = arg_unspecified(), `uri` = arg_unspecified(), `extended` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `partition_id` = clean_value(`partition_id`, "TYPE_INT32", "TYPE_INT32", repeated = TRUE),
          `uri` = clean_value(`uri`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `extended` = clean_value(`extended`, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
          .qualified_name = "substrait.ExchangeRel.ExchangeTarget"
        )
      }
    ),
    MultiBucketExpression = list(
      create = function(..., `expression` = arg_unspecified(), `constrained_to_count` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `expression` = clean_value(`expression`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          `constrained_to_count` = clean_value(`constrained_to_count`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          .qualified_name = "substrait.ExchangeRel.MultiBucketExpression"
        )
      }
    ),
    RoundRobin = list(
      create = function(..., `exact` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `exact` = clean_value(`exact`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          .qualified_name = "substrait.ExchangeRel.RoundRobin"
        )
      }
    ),
    ScatterFields = list(
      create = function(..., `fields` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `fields` = clean_value(`fields`, "TYPE_MESSAGE", "substrait.Expression.FieldReference", repeated = TRUE),
          .qualified_name = "substrait.ExchangeRel.ScatterFields"
        )
      }
    ),
    SingleBucketExpression = list(
      create = function(..., `expression` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `expression` = clean_value(`expression`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.ExchangeRel.SingleBucketExpression"
        )
      }
    ),
    create = function(..., `common` = arg_unspecified(), `input` = arg_unspecified(), `partition_count` = arg_unspecified(), `targets` = arg_unspecified(), `scatter_by_fields` = arg_unspecified(), `single_target` = arg_unspecified(), `multi_target` = arg_unspecified(), `round_robin` = arg_unspecified(), `broadcast` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `partition_count` = clean_value(`partition_count`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
        `targets` = clean_value(`targets`, "TYPE_MESSAGE", "substrait.ExchangeRel.ExchangeTarget", repeated = TRUE),
        `scatter_by_fields` = clean_value(`scatter_by_fields`, "TYPE_MESSAGE", "substrait.ExchangeRel.ScatterFields", repeated = FALSE),
        `single_target` = clean_value(`single_target`, "TYPE_MESSAGE", "substrait.ExchangeRel.SingleBucketExpression", repeated = FALSE),
        `multi_target` = clean_value(`multi_target`, "TYPE_MESSAGE", "substrait.ExchangeRel.MultiBucketExpression", repeated = FALSE),
        `round_robin` = clean_value(`round_robin`, "TYPE_MESSAGE", "substrait.ExchangeRel.RoundRobin", repeated = FALSE),
        `broadcast` = clean_value(`broadcast`, "TYPE_MESSAGE", "substrait.ExchangeRel.Broadcast", repeated = FALSE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.ExchangeRel"
      )
    }
  ),
  Expression = list(
    Cast = list(
      FailureBehavior = list(
        FAILURE_BEHAVIOR_UNSPECIFIED = structure(0L, class = c("substrait_Expression_Cast_FailureBehavior", "substrait_proto_enum", "substrait_proto")),
        FAILURE_BEHAVIOR_RETURN_NULL = structure(1L, class = c("substrait_Expression_Cast_FailureBehavior", "substrait_proto_enum", "substrait_proto")),
        FAILURE_BEHAVIOR_THROW_EXCEPTION = structure(2L, class = c("substrait_Expression_Cast_FailureBehavior", "substrait_proto_enum", "substrait_proto")),
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.Expression.Cast.FailureBehavior"
          )
        }
      ),
      create = function(..., `type` = arg_unspecified(), `input` = arg_unspecified(), `failure_behavior` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type` = clean_value(`type`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          `failure_behavior` = clean_value(`failure_behavior`, "TYPE_ENUM", "substrait.Expression.Cast.FailureBehavior", repeated = FALSE),
          .qualified_name = "substrait.Expression.Cast"
        )
      }
    ),
    EmbeddedFunction = list(
      PythonPickleFunction = list(
        create = function(..., `function` = arg_unspecified(), `prerequisite` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `function` = clean_value(`function`, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
            `prerequisite` = clean_value(`prerequisite`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
            .qualified_name = "substrait.Expression.EmbeddedFunction.PythonPickleFunction"
          )
        }
      ),
      WebAssemblyFunction = list(
        create = function(..., `script` = arg_unspecified(), `prerequisite` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `script` = clean_value(`script`, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
            `prerequisite` = clean_value(`prerequisite`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
            .qualified_name = "substrait.Expression.EmbeddedFunction.WebAssemblyFunction"
          )
        }
      ),
      create = function(..., `arguments` = arg_unspecified(), `output_type` = arg_unspecified(), `python_pickle_function` = arg_unspecified(), `web_assembly_function` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `arguments` = clean_value(`arguments`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          `output_type` = clean_value(`output_type`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          `python_pickle_function` = clean_value(`python_pickle_function`, "TYPE_MESSAGE", "substrait.Expression.EmbeddedFunction.PythonPickleFunction", repeated = FALSE),
          `web_assembly_function` = clean_value(`web_assembly_function`, "TYPE_MESSAGE", "substrait.Expression.EmbeddedFunction.WebAssemblyFunction", repeated = FALSE),
          .qualified_name = "substrait.Expression.EmbeddedFunction"
        )
      }
    ),
    Enum = list(
      Empty = list(
        create = function(...) {
          rlang::check_dots_empty()
          create_substrait_message(.qualified_name = "substrait.Expression.Enum.Empty")
        }
      ),
      create = function(..., `specified` = arg_unspecified(), `unspecified` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `specified` = clean_value(`specified`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `unspecified` = clean_value(`unspecified`, "TYPE_MESSAGE", "substrait.Expression.Enum.Empty", repeated = FALSE),
          .qualified_name = "substrait.Expression.Enum"
        )
      }
    ),
    FieldReference = list(
      OuterReference = list(
        create = function(..., `steps_out` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `steps_out` = clean_value(`steps_out`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.FieldReference.OuterReference"
          )
        }
      ),
      RootReference = list(
        create = function(...) {
          rlang::check_dots_empty()
          create_substrait_message(.qualified_name = "substrait.Expression.FieldReference.RootReference")
        }
      ),
      create = function(..., `direct_reference` = arg_unspecified(), `masked_reference` = arg_unspecified(), `expression` = arg_unspecified(), `root_reference` = arg_unspecified(), `outer_reference` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `direct_reference` = clean_value(`direct_reference`, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment", repeated = FALSE),
          `masked_reference` = clean_value(`masked_reference`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression", repeated = FALSE),
          `expression` = clean_value(`expression`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          `root_reference` = clean_value(`root_reference`, "TYPE_MESSAGE", "substrait.Expression.FieldReference.RootReference", repeated = FALSE),
          `outer_reference` = clean_value(`outer_reference`, "TYPE_MESSAGE", "substrait.Expression.FieldReference.OuterReference", repeated = FALSE),
          .qualified_name = "substrait.Expression.FieldReference"
        )
      }
    ),
    IfThen = list(
      IfClause = list(
        create = function(..., `if` = arg_unspecified(), `then` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `if` = clean_value(`if`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
            `then` = clean_value(`then`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
            .qualified_name = "substrait.Expression.IfThen.IfClause"
          )
        }
      ),
      create = function(..., `ifs` = arg_unspecified(), `else` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `ifs` = clean_value(`ifs`, "TYPE_MESSAGE", "substrait.Expression.IfThen.IfClause", repeated = TRUE),
          `else` = clean_value(`else`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.Expression.IfThen"
        )
      }
    ),
    Literal = list(
      Decimal = list(
        create = function(..., `value` = arg_unspecified(), `precision` = arg_unspecified(), `scale` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `value` = clean_value(`value`, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
            `precision` = clean_value(`precision`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            `scale` = clean_value(`scale`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.Decimal"
          )
        }
      ),
      IntervalDayToSecond = list(
        create = function(..., `days` = arg_unspecified(), `seconds` = arg_unspecified(), `microseconds` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `days` = clean_value(`days`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            `seconds` = clean_value(`seconds`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            `microseconds` = clean_value(`microseconds`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.IntervalDayToSecond"
          )
        }
      ),
      IntervalYearToMonth = list(
        create = function(..., `years` = arg_unspecified(), `months` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `years` = clean_value(`years`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            `months` = clean_value(`months`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.IntervalYearToMonth"
          )
        }
      ),
      List = list(
        create = function(..., `values` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `values` = clean_value(`values`, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = TRUE),
            .qualified_name = "substrait.Expression.Literal.List"
          )
        }
      ),
      Map = list(
        KeyValue = list(
          create = function(..., `key` = arg_unspecified(), `value` = arg_unspecified()) {
            rlang::check_dots_empty()
            create_substrait_message(
              `key` = clean_value(`key`, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = FALSE),
              `value` = clean_value(`value`, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = FALSE),
              .qualified_name = "substrait.Expression.Literal.Map.KeyValue"
            )
          }
        ),
        create = function(..., `key_values` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `key_values` = clean_value(`key_values`, "TYPE_MESSAGE", "substrait.Expression.Literal.Map.KeyValue", repeated = TRUE),
            .qualified_name = "substrait.Expression.Literal.Map"
          )
        }
      ),
      Struct = list(
        create = function(..., `fields` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `fields` = clean_value(`fields`, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = TRUE),
            .qualified_name = "substrait.Expression.Literal.Struct"
          )
        }
      ),
      UserDefined = list(
        create = function(..., `type_reference` = arg_unspecified(), `type_parameters` = arg_unspecified(), `value` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `type_reference` = clean_value(`type_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
            `type_parameters` = clean_value(`type_parameters`, "TYPE_MESSAGE", "substrait.Type.Parameter", repeated = TRUE),
            `value` = clean_value(`value`, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.UserDefined"
          )
        }
      ),
      VarChar = list(
        create = function(..., `value` = arg_unspecified(), `length` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `value` = clean_value(`value`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            `length` = clean_value(`length`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.VarChar"
          )
        }
      ),
      create = function(..., `boolean` = arg_unspecified(), `i8` = arg_unspecified(), `i16` = arg_unspecified(), `i32` = arg_unspecified(), `i64` = arg_unspecified(), `fp32` = arg_unspecified(), `fp64` = arg_unspecified(), `string` = arg_unspecified(), `binary` = arg_unspecified(), `timestamp` = arg_unspecified(), `date` = arg_unspecified(), `time` = arg_unspecified(), `interval_year_to_month` = arg_unspecified(), `interval_day_to_second` = arg_unspecified(), `fixed_char` = arg_unspecified(), `var_char` = arg_unspecified(), `fixed_binary` = arg_unspecified(), `decimal` = arg_unspecified(), `struct` = arg_unspecified(), `map` = arg_unspecified(), `timestamp_tz` = arg_unspecified(), `uuid` = arg_unspecified(), `null` = arg_unspecified(), `list` = arg_unspecified(), `empty_list` = arg_unspecified(), `empty_map` = arg_unspecified(), `user_defined` = arg_unspecified(), `nullable` = arg_unspecified(), `type_variation_reference` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `boolean` = clean_value(`boolean`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `i8` = clean_value(`i8`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          `i16` = clean_value(`i16`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          `i32` = clean_value(`i32`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          `i64` = clean_value(`i64`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          `fp32` = clean_value(`fp32`, "TYPE_FLOAT", "TYPE_FLOAT", repeated = FALSE),
          `fp64` = clean_value(`fp64`, "TYPE_DOUBLE", "TYPE_DOUBLE", repeated = FALSE),
          `string` = clean_value(`string`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `binary` = clean_value(`binary`, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
          `timestamp` = clean_value(`timestamp`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          `date` = clean_value(`date`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          `time` = clean_value(`time`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          `interval_year_to_month` = clean_value(`interval_year_to_month`, "TYPE_MESSAGE", "substrait.Expression.Literal.IntervalYearToMonth", repeated = FALSE),
          `interval_day_to_second` = clean_value(`interval_day_to_second`, "TYPE_MESSAGE", "substrait.Expression.Literal.IntervalDayToSecond", repeated = FALSE),
          `fixed_char` = clean_value(`fixed_char`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `var_char` = clean_value(`var_char`, "TYPE_MESSAGE", "substrait.Expression.Literal.VarChar", repeated = FALSE),
          `fixed_binary` = clean_value(`fixed_binary`, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
          `decimal` = clean_value(`decimal`, "TYPE_MESSAGE", "substrait.Expression.Literal.Decimal", repeated = FALSE),
          `struct` = clean_value(`struct`, "TYPE_MESSAGE", "substrait.Expression.Literal.Struct", repeated = FALSE),
          `map` = clean_value(`map`, "TYPE_MESSAGE", "substrait.Expression.Literal.Map", repeated = FALSE),
          `timestamp_tz` = clean_value(`timestamp_tz`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          `uuid` = clean_value(`uuid`, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
          `null` = clean_value(`null`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          `list` = clean_value(`list`, "TYPE_MESSAGE", "substrait.Expression.Literal.List", repeated = FALSE),
          `empty_list` = clean_value(`empty_list`, "TYPE_MESSAGE", "substrait.Type.List", repeated = FALSE),
          `empty_map` = clean_value(`empty_map`, "TYPE_MESSAGE", "substrait.Type.Map", repeated = FALSE),
          `user_defined` = clean_value(`user_defined`, "TYPE_MESSAGE", "substrait.Expression.Literal.UserDefined", repeated = FALSE),
          `nullable` = clean_value(`nullable`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          .qualified_name = "substrait.Expression.Literal"
        )
      }
    ),
    MaskExpression = list(
      ListSelect = list(
        ListSelectItem = list(
          ListElement = list(
            create = function(..., `field` = arg_unspecified()) {
              rlang::check_dots_empty()
              create_substrait_message(
                `field` = clean_value(`field`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
                .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListElement"
              )
            }
          ),
          ListSlice = list(
            create = function(..., `start` = arg_unspecified(), `end` = arg_unspecified()) {
              rlang::check_dots_empty()
              create_substrait_message(
                `start` = clean_value(`start`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
                `end` = clean_value(`end`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
                .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListSlice"
              )
            }
          ),
          create = function(..., `item` = arg_unspecified(), `slice` = arg_unspecified()) {
            rlang::check_dots_empty()
            create_substrait_message(
              `item` = clean_value(`item`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListElement", repeated = FALSE),
              `slice` = clean_value(`slice`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListSlice", repeated = FALSE),
              .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem"
            )
          }
        ),
        create = function(..., `selection` = arg_unspecified(), `child` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `selection` = clean_value(`selection`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.ListSelect.ListSelectItem", repeated = TRUE),
            `child` = clean_value(`child`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.Select", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.ListSelect"
          )
        }
      ),
      MapSelect = list(
        MapKey = list(
          create = function(..., `map_key` = arg_unspecified()) {
            rlang::check_dots_empty()
            create_substrait_message(
              `map_key` = clean_value(`map_key`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
              .qualified_name = "substrait.Expression.MaskExpression.MapSelect.MapKey"
            )
          }
        ),
        MapKeyExpression = list(
          create = function(..., `map_key_expression` = arg_unspecified()) {
            rlang::check_dots_empty()
            create_substrait_message(
              `map_key_expression` = clean_value(`map_key_expression`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
              .qualified_name = "substrait.Expression.MaskExpression.MapSelect.MapKeyExpression"
            )
          }
        ),
        create = function(..., `key` = arg_unspecified(), `expression` = arg_unspecified(), `child` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `key` = clean_value(`key`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.MapSelect.MapKey", repeated = FALSE),
            `expression` = clean_value(`expression`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.MapSelect.MapKeyExpression", repeated = FALSE),
            `child` = clean_value(`child`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.Select", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.MapSelect"
          )
        }
      ),
      Select = list(
        create = function(..., `struct` = arg_unspecified(), `list` = arg_unspecified(), `map` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `struct` = clean_value(`struct`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.StructSelect", repeated = FALSE),
            `list` = clean_value(`list`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.ListSelect", repeated = FALSE),
            `map` = clean_value(`map`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.MapSelect", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.Select"
          )
        }
      ),
      StructItem = list(
        create = function(..., `field` = arg_unspecified(), `child` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `field` = clean_value(`field`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            `child` = clean_value(`child`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.Select", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.StructItem"
          )
        }
      ),
      StructSelect = list(
        create = function(..., `struct_items` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `struct_items` = clean_value(`struct_items`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.StructItem", repeated = TRUE),
            .qualified_name = "substrait.Expression.MaskExpression.StructSelect"
          )
        }
      ),
      create = function(..., `select` = arg_unspecified(), `maintain_singular_struct` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `select` = clean_value(`select`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.StructSelect", repeated = FALSE),
          `maintain_singular_struct` = clean_value(`maintain_singular_struct`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          .qualified_name = "substrait.Expression.MaskExpression"
        )
      }
    ),
    MultiOrList = list(
      Record = list(
        create = function(..., `fields` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `fields` = clean_value(`fields`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
            .qualified_name = "substrait.Expression.MultiOrList.Record"
          )
        }
      ),
      create = function(..., `value` = arg_unspecified(), `options` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `value` = clean_value(`value`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          `options` = clean_value(`options`, "TYPE_MESSAGE", "substrait.Expression.MultiOrList.Record", repeated = TRUE),
          .qualified_name = "substrait.Expression.MultiOrList"
        )
      }
    ),
    ReferenceSegment = list(
      ListElement = list(
        create = function(..., `offset` = arg_unspecified(), `child` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `offset` = clean_value(`offset`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            `child` = clean_value(`child`, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment", repeated = FALSE),
            .qualified_name = "substrait.Expression.ReferenceSegment.ListElement"
          )
        }
      ),
      MapKey = list(
        create = function(..., `map_key` = arg_unspecified(), `child` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `map_key` = clean_value(`map_key`, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = FALSE),
            `child` = clean_value(`child`, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment", repeated = FALSE),
            .qualified_name = "substrait.Expression.ReferenceSegment.MapKey"
          )
        }
      ),
      StructField = list(
        create = function(..., `field` = arg_unspecified(), `child` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `field` = clean_value(`field`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            `child` = clean_value(`child`, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment", repeated = FALSE),
            .qualified_name = "substrait.Expression.ReferenceSegment.StructField"
          )
        }
      ),
      create = function(..., `map_key` = arg_unspecified(), `struct_field` = arg_unspecified(), `list_element` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `map_key` = clean_value(`map_key`, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment.MapKey", repeated = FALSE),
          `struct_field` = clean_value(`struct_field`, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment.StructField", repeated = FALSE),
          `list_element` = clean_value(`list_element`, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment.ListElement", repeated = FALSE),
          .qualified_name = "substrait.Expression.ReferenceSegment"
        )
      }
    ),
    ScalarFunction = list(
      create = function(..., `function_reference` = arg_unspecified(), `arguments` = arg_unspecified(), `options` = arg_unspecified(), `output_type` = arg_unspecified(), `args` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `function_reference` = clean_value(`function_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `arguments` = clean_value(`arguments`, "TYPE_MESSAGE", "substrait.FunctionArgument", repeated = TRUE),
          `options` = clean_value(`options`, "TYPE_MESSAGE", "substrait.FunctionOption", repeated = TRUE),
          `output_type` = clean_value(`output_type`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          `args` = clean_value(`args`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          .qualified_name = "substrait.Expression.ScalarFunction"
        )
      }
    ),
    SingularOrList = list(
      create = function(..., `value` = arg_unspecified(), `options` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `value` = clean_value(`value`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          `options` = clean_value(`options`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          .qualified_name = "substrait.Expression.SingularOrList"
        )
      }
    ),
    Subquery = list(
      InPredicate = list(
        create = function(..., `needles` = arg_unspecified(), `haystack` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `needles` = clean_value(`needles`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
            `haystack` = clean_value(`haystack`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
            .qualified_name = "substrait.Expression.Subquery.InPredicate"
          )
        }
      ),
      Scalar = list(
        create = function(..., `input` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
            .qualified_name = "substrait.Expression.Subquery.Scalar"
          )
        }
      ),
      SetComparison = list(
        ComparisonOp = list(
          COMPARISON_OP_UNSPECIFIED = structure(0L, class = c("substrait_Expression_Subquery_SetComparison_ComparisonOp", "substrait_proto_enum", "substrait_proto")),
          COMPARISON_OP_EQ = structure(1L, class = c("substrait_Expression_Subquery_SetComparison_ComparisonOp", "substrait_proto_enum", "substrait_proto")),
          COMPARISON_OP_NE = structure(2L, class = c("substrait_Expression_Subquery_SetComparison_ComparisonOp", "substrait_proto_enum", "substrait_proto")),
          COMPARISON_OP_LT = structure(3L, class = c("substrait_Expression_Subquery_SetComparison_ComparisonOp", "substrait_proto_enum", "substrait_proto")),
          COMPARISON_OP_GT = structure(4L, class = c("substrait_Expression_Subquery_SetComparison_ComparisonOp", "substrait_proto_enum", "substrait_proto")),
          COMPARISON_OP_LE = structure(5L, class = c("substrait_Expression_Subquery_SetComparison_ComparisonOp", "substrait_proto_enum", "substrait_proto")),
          COMPARISON_OP_GE = structure(6L, class = c("substrait_Expression_Subquery_SetComparison_ComparisonOp", "substrait_proto_enum", "substrait_proto")),
          create = function(value) {
            create_substrait_enum(
              value,
              .qualified_name = "substrait.Expression.Subquery.SetComparison.ComparisonOp"
            )
          }
        ),
        ReductionOp = list(
          REDUCTION_OP_UNSPECIFIED = structure(0L, class = c("substrait_Expression_Subquery_SetComparison_ReductionOp", "substrait_proto_enum", "substrait_proto")),
          REDUCTION_OP_ANY = structure(1L, class = c("substrait_Expression_Subquery_SetComparison_ReductionOp", "substrait_proto_enum", "substrait_proto")),
          REDUCTION_OP_ALL = structure(2L, class = c("substrait_Expression_Subquery_SetComparison_ReductionOp", "substrait_proto_enum", "substrait_proto")),
          create = function(value) {
            create_substrait_enum(
              value,
              .qualified_name = "substrait.Expression.Subquery.SetComparison.ReductionOp"
            )
          }
        ),
        create = function(..., `reduction_op` = arg_unspecified(), `comparison_op` = arg_unspecified(), `left` = arg_unspecified(), `right` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `reduction_op` = clean_value(`reduction_op`, "TYPE_ENUM", "substrait.Expression.Subquery.SetComparison.ReductionOp", repeated = FALSE),
            `comparison_op` = clean_value(`comparison_op`, "TYPE_ENUM", "substrait.Expression.Subquery.SetComparison.ComparisonOp", repeated = FALSE),
            `left` = clean_value(`left`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
            `right` = clean_value(`right`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
            .qualified_name = "substrait.Expression.Subquery.SetComparison"
          )
        }
      ),
      SetPredicate = list(
        PredicateOp = list(
          PREDICATE_OP_UNSPECIFIED = structure(0L, class = c("substrait_Expression_Subquery_SetPredicate_PredicateOp", "substrait_proto_enum", "substrait_proto")),
          PREDICATE_OP_EXISTS = structure(1L, class = c("substrait_Expression_Subquery_SetPredicate_PredicateOp", "substrait_proto_enum", "substrait_proto")),
          PREDICATE_OP_UNIQUE = structure(2L, class = c("substrait_Expression_Subquery_SetPredicate_PredicateOp", "substrait_proto_enum", "substrait_proto")),
          create = function(value) {
            create_substrait_enum(
              value,
              .qualified_name = "substrait.Expression.Subquery.SetPredicate.PredicateOp"
            )
          }
        ),
        create = function(..., `predicate_op` = arg_unspecified(), `tuples` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `predicate_op` = clean_value(`predicate_op`, "TYPE_ENUM", "substrait.Expression.Subquery.SetPredicate.PredicateOp", repeated = FALSE),
            `tuples` = clean_value(`tuples`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
            .qualified_name = "substrait.Expression.Subquery.SetPredicate"
          )
        }
      ),
      create = function(..., `scalar` = arg_unspecified(), `in_predicate` = arg_unspecified(), `set_predicate` = arg_unspecified(), `set_comparison` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `scalar` = clean_value(`scalar`, "TYPE_MESSAGE", "substrait.Expression.Subquery.Scalar", repeated = FALSE),
          `in_predicate` = clean_value(`in_predicate`, "TYPE_MESSAGE", "substrait.Expression.Subquery.InPredicate", repeated = FALSE),
          `set_predicate` = clean_value(`set_predicate`, "TYPE_MESSAGE", "substrait.Expression.Subquery.SetPredicate", repeated = FALSE),
          `set_comparison` = clean_value(`set_comparison`, "TYPE_MESSAGE", "substrait.Expression.Subquery.SetComparison", repeated = FALSE),
          .qualified_name = "substrait.Expression.Subquery"
        )
      }
    ),
    SwitchExpression = list(
      IfValue = list(
        create = function(..., `if` = arg_unspecified(), `then` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `if` = clean_value(`if`, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = FALSE),
            `then` = clean_value(`then`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
            .qualified_name = "substrait.Expression.SwitchExpression.IfValue"
          )
        }
      ),
      create = function(..., `match` = arg_unspecified(), `ifs` = arg_unspecified(), `else` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `match` = clean_value(`match`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          `ifs` = clean_value(`ifs`, "TYPE_MESSAGE", "substrait.Expression.SwitchExpression.IfValue", repeated = TRUE),
          `else` = clean_value(`else`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.Expression.SwitchExpression"
        )
      }
    ),
    WindowFunction = list(
      Bound = list(
        CurrentRow = list(
          create = function(...) {
            rlang::check_dots_empty()
            create_substrait_message(.qualified_name = "substrait.Expression.WindowFunction.Bound.CurrentRow")
          }
        ),
        Following = list(
          create = function(..., `offset` = arg_unspecified()) {
            rlang::check_dots_empty()
            create_substrait_message(
              `offset` = clean_value(`offset`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
              .qualified_name = "substrait.Expression.WindowFunction.Bound.Following"
            )
          }
        ),
        Preceding = list(
          create = function(..., `offset` = arg_unspecified()) {
            rlang::check_dots_empty()
            create_substrait_message(
              `offset` = clean_value(`offset`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
              .qualified_name = "substrait.Expression.WindowFunction.Bound.Preceding"
            )
          }
        ),
        Unbounded = list(
          create = function(...) {
            rlang::check_dots_empty()
            create_substrait_message(.qualified_name = "substrait.Expression.WindowFunction.Bound.Unbounded")
          }
        ),
        create = function(..., `preceding` = arg_unspecified(), `following` = arg_unspecified(), `current_row` = arg_unspecified(), `unbounded` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `preceding` = clean_value(`preceding`, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound.Preceding", repeated = FALSE),
            `following` = clean_value(`following`, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound.Following", repeated = FALSE),
            `current_row` = clean_value(`current_row`, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound.CurrentRow", repeated = FALSE),
            `unbounded` = clean_value(`unbounded`, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound.Unbounded", repeated = FALSE),
            .qualified_name = "substrait.Expression.WindowFunction.Bound"
          )
        }
      ),
      create = function(..., `function_reference` = arg_unspecified(), `arguments` = arg_unspecified(), `options` = arg_unspecified(), `output_type` = arg_unspecified(), `phase` = arg_unspecified(), `sorts` = arg_unspecified(), `invocation` = arg_unspecified(), `partitions` = arg_unspecified(), `lower_bound` = arg_unspecified(), `upper_bound` = arg_unspecified(), `args` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `function_reference` = clean_value(`function_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `arguments` = clean_value(`arguments`, "TYPE_MESSAGE", "substrait.FunctionArgument", repeated = TRUE),
          `options` = clean_value(`options`, "TYPE_MESSAGE", "substrait.FunctionOption", repeated = TRUE),
          `output_type` = clean_value(`output_type`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          `phase` = clean_value(`phase`, "TYPE_ENUM", "substrait.AggregationPhase", repeated = FALSE),
          `sorts` = clean_value(`sorts`, "TYPE_MESSAGE", "substrait.SortField", repeated = TRUE),
          `invocation` = clean_value(`invocation`, "TYPE_ENUM", "substrait.AggregateFunction.AggregationInvocation", repeated = FALSE),
          `partitions` = clean_value(`partitions`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          `lower_bound` = clean_value(`lower_bound`, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound", repeated = FALSE),
          `upper_bound` = clean_value(`upper_bound`, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound", repeated = FALSE),
          `args` = clean_value(`args`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          .qualified_name = "substrait.Expression.WindowFunction"
        )
      }
    ),
    create = function(..., `literal` = arg_unspecified(), `selection` = arg_unspecified(), `scalar_function` = arg_unspecified(), `window_function` = arg_unspecified(), `if_then` = arg_unspecified(), `switch_expression` = arg_unspecified(), `singular_or_list` = arg_unspecified(), `multi_or_list` = arg_unspecified(), `cast` = arg_unspecified(), `subquery` = arg_unspecified(), `enum` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `literal` = clean_value(`literal`, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = FALSE),
        `selection` = clean_value(`selection`, "TYPE_MESSAGE", "substrait.Expression.FieldReference", repeated = FALSE),
        `scalar_function` = clean_value(`scalar_function`, "TYPE_MESSAGE", "substrait.Expression.ScalarFunction", repeated = FALSE),
        `window_function` = clean_value(`window_function`, "TYPE_MESSAGE", "substrait.Expression.WindowFunction", repeated = FALSE),
        `if_then` = clean_value(`if_then`, "TYPE_MESSAGE", "substrait.Expression.IfThen", repeated = FALSE),
        `switch_expression` = clean_value(`switch_expression`, "TYPE_MESSAGE", "substrait.Expression.SwitchExpression", repeated = FALSE),
        `singular_or_list` = clean_value(`singular_or_list`, "TYPE_MESSAGE", "substrait.Expression.SingularOrList", repeated = FALSE),
        `multi_or_list` = clean_value(`multi_or_list`, "TYPE_MESSAGE", "substrait.Expression.MultiOrList", repeated = FALSE),
        `cast` = clean_value(`cast`, "TYPE_MESSAGE", "substrait.Expression.Cast", repeated = FALSE),
        `subquery` = clean_value(`subquery`, "TYPE_MESSAGE", "substrait.Expression.Subquery", repeated = FALSE),
        `enum` = clean_value(`enum`, "TYPE_MESSAGE", "substrait.Expression.Enum", repeated = FALSE),
        .qualified_name = "substrait.Expression"
      )
    }
  ),
  ExtensionLeafRel = list(
    create = function(..., `common` = arg_unspecified(), `detail` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `detail` = clean_value(`detail`, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
        .qualified_name = "substrait.ExtensionLeafRel"
      )
    }
  ),
  ExtensionMultiRel = list(
    create = function(..., `common` = arg_unspecified(), `inputs` = arg_unspecified(), `detail` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `inputs` = clean_value(`inputs`, "TYPE_MESSAGE", "substrait.Rel", repeated = TRUE),
        `detail` = clean_value(`detail`, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
        .qualified_name = "substrait.ExtensionMultiRel"
      )
    }
  ),
  ExtensionObject = list(
    create = function(..., `detail` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `detail` = clean_value(`detail`, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
        .qualified_name = "substrait.ExtensionObject"
      )
    }
  ),
  ExtensionSingleRel = list(
    create = function(..., `common` = arg_unspecified(), `input` = arg_unspecified(), `detail` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `detail` = clean_value(`detail`, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
        .qualified_name = "substrait.ExtensionSingleRel"
      )
    }
  ),
  FetchRel = list(
    create = function(..., `common` = arg_unspecified(), `input` = arg_unspecified(), `offset` = arg_unspecified(), `count` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `offset` = clean_value(`offset`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
        `count` = clean_value(`count`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.FetchRel"
      )
    }
  ),
  FilterRel = list(
    create = function(..., `common` = arg_unspecified(), `input` = arg_unspecified(), `condition` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `condition` = clean_value(`condition`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.FilterRel"
      )
    }
  ),
  FunctionArgument = list(
    create = function(..., `enum` = arg_unspecified(), `type` = arg_unspecified(), `value` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `enum` = clean_value(`enum`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
        `type` = clean_value(`type`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
        `value` = clean_value(`value`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        .qualified_name = "substrait.FunctionArgument"
      )
    }
  ),
  FunctionOption = list(
    create = function(..., `name` = arg_unspecified(), `preference` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
        `preference` = clean_value(`preference`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        .qualified_name = "substrait.FunctionOption"
      )
    }
  ),
  FunctionSignature = list(
    Aggregate = list(
      create = function(..., `arguments` = arg_unspecified(), `name` = arg_unspecified(), `description` = arg_unspecified(), `deterministic` = arg_unspecified(), `session_dependent` = arg_unspecified(), `output_type` = arg_unspecified(), `variadic` = arg_unspecified(), `normal` = arg_unspecified(), `ordered` = arg_unspecified(), `max_set` = arg_unspecified(), `intermediate_type` = arg_unspecified(), `implementations` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `arguments` = clean_value(`arguments`, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument", repeated = TRUE),
          `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `description` = clean_value(`description`, "TYPE_MESSAGE", "substrait.FunctionSignature.Description", repeated = FALSE),
          `deterministic` = clean_value(`deterministic`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `session_dependent` = clean_value(`session_dependent`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `output_type` = clean_value(`output_type`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `variadic` = clean_value(`variadic`, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgVariadic", repeated = FALSE),
          `normal` = clean_value(`normal`, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgNormal", repeated = FALSE),
          `ordered` = clean_value(`ordered`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `max_set` = clean_value(`max_set`, "TYPE_UINT64", "TYPE_UINT64", repeated = FALSE),
          `intermediate_type` = clean_value(`intermediate_type`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          `implementations` = clean_value(`implementations`, "TYPE_MESSAGE", "substrait.FunctionSignature.Implementation", repeated = TRUE),
          .qualified_name = "substrait.FunctionSignature.Aggregate"
        )
      }
    ),
    Argument = list(
      EnumArgument = list(
        create = function(..., `options` = arg_unspecified(), `optional` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `options` = clean_value(`options`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
            `optional` = clean_value(`optional`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
            .qualified_name = "substrait.FunctionSignature.Argument.EnumArgument"
          )
        }
      ),
      TypeArgument = list(
        create = function(..., `type` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `type` = clean_value(`type`, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = FALSE),
            .qualified_name = "substrait.FunctionSignature.Argument.TypeArgument"
          )
        }
      ),
      ValueArgument = list(
        create = function(..., `type` = arg_unspecified(), `constant` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `type` = clean_value(`type`, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = FALSE),
            `constant` = clean_value(`constant`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
            .qualified_name = "substrait.FunctionSignature.Argument.ValueArgument"
          )
        }
      ),
      create = function(..., `name` = arg_unspecified(), `value` = arg_unspecified(), `type` = arg_unspecified(), `enum` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `value` = clean_value(`value`, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument.ValueArgument", repeated = FALSE),
          `type` = clean_value(`type`, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument.TypeArgument", repeated = FALSE),
          `enum` = clean_value(`enum`, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument.EnumArgument", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.Argument"
        )
      }
    ),
    Description = list(
      create = function(..., `language` = arg_unspecified(), `body` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `language` = clean_value(`language`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `body` = clean_value(`body`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.Description"
        )
      }
    ),
    FinalArgNormal = list(
      create = function(...) {
        rlang::check_dots_empty()
        create_substrait_message(.qualified_name = "substrait.FunctionSignature.FinalArgNormal")
      }
    ),
    FinalArgVariadic = list(
      ParameterConsistency = list(
        PARAMETER_CONSISTENCY_UNSPECIFIED = structure(0L, class = c("substrait_FunctionSignature_FinalArgVariadic_ParameterConsistency", "substrait_proto_enum", "substrait_proto")),
        PARAMETER_CONSISTENCY_CONSISTENT = structure(1L, class = c("substrait_FunctionSignature_FinalArgVariadic_ParameterConsistency", "substrait_proto_enum", "substrait_proto")),
        PARAMETER_CONSISTENCY_INCONSISTENT = structure(2L, class = c("substrait_FunctionSignature_FinalArgVariadic_ParameterConsistency", "substrait_proto_enum", "substrait_proto")),
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.FunctionSignature.FinalArgVariadic.ParameterConsistency"
          )
        }
      ),
      create = function(..., `min_args` = arg_unspecified(), `max_args` = arg_unspecified(), `consistency` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `min_args` = clean_value(`min_args`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          `max_args` = clean_value(`max_args`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          `consistency` = clean_value(`consistency`, "TYPE_ENUM", "substrait.FunctionSignature.FinalArgVariadic.ParameterConsistency", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.FinalArgVariadic"
        )
      }
    ),
    Implementation = list(
      Type = list(
        TYPE_UNSPECIFIED = structure(0L, class = c("substrait_FunctionSignature_Implementation_Type", "substrait_proto_enum", "substrait_proto")),
        TYPE_WEB_ASSEMBLY = structure(1L, class = c("substrait_FunctionSignature_Implementation_Type", "substrait_proto_enum", "substrait_proto")),
        TYPE_TRINO_JAR = structure(2L, class = c("substrait_FunctionSignature_Implementation_Type", "substrait_proto_enum", "substrait_proto")),
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.FunctionSignature.Implementation.Type"
          )
        }
      ),
      create = function(..., `type` = arg_unspecified(), `uri` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type` = clean_value(`type`, "TYPE_ENUM", "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          `uri` = clean_value(`uri`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.Implementation"
        )
      }
    ),
    Scalar = list(
      create = function(..., `arguments` = arg_unspecified(), `name` = arg_unspecified(), `description` = arg_unspecified(), `deterministic` = arg_unspecified(), `session_dependent` = arg_unspecified(), `output_type` = arg_unspecified(), `variadic` = arg_unspecified(), `normal` = arg_unspecified(), `implementations` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `arguments` = clean_value(`arguments`, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument", repeated = TRUE),
          `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          `description` = clean_value(`description`, "TYPE_MESSAGE", "substrait.FunctionSignature.Description", repeated = FALSE),
          `deterministic` = clean_value(`deterministic`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `session_dependent` = clean_value(`session_dependent`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `output_type` = clean_value(`output_type`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `variadic` = clean_value(`variadic`, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgVariadic", repeated = FALSE),
          `normal` = clean_value(`normal`, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgNormal", repeated = FALSE),
          `implementations` = clean_value(`implementations`, "TYPE_MESSAGE", "substrait.FunctionSignature.Implementation", repeated = TRUE),
          .qualified_name = "substrait.FunctionSignature.Scalar"
        )
      }
    ),
    Window = list(
      WindowType = list(
        WINDOW_TYPE_UNSPECIFIED = structure(0L, class = c("substrait_FunctionSignature_Window_WindowType", "substrait_proto_enum", "substrait_proto")),
        WINDOW_TYPE_STREAMING = structure(1L, class = c("substrait_FunctionSignature_Window_WindowType", "substrait_proto_enum", "substrait_proto")),
        WINDOW_TYPE_PARTITION = structure(2L, class = c("substrait_FunctionSignature_Window_WindowType", "substrait_proto_enum", "substrait_proto")),
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.FunctionSignature.Window.WindowType"
          )
        }
      ),
      create = function(..., `arguments` = arg_unspecified(), `name` = arg_unspecified(), `description` = arg_unspecified(), `deterministic` = arg_unspecified(), `session_dependent` = arg_unspecified(), `intermediate_type` = arg_unspecified(), `output_type` = arg_unspecified(), `variadic` = arg_unspecified(), `normal` = arg_unspecified(), `ordered` = arg_unspecified(), `max_set` = arg_unspecified(), `window_type` = arg_unspecified(), `implementations` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `arguments` = clean_value(`arguments`, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument", repeated = TRUE),
          `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          `description` = clean_value(`description`, "TYPE_MESSAGE", "substrait.FunctionSignature.Description", repeated = FALSE),
          `deterministic` = clean_value(`deterministic`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `session_dependent` = clean_value(`session_dependent`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `intermediate_type` = clean_value(`intermediate_type`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `output_type` = clean_value(`output_type`, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          `variadic` = clean_value(`variadic`, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgVariadic", repeated = FALSE),
          `normal` = clean_value(`normal`, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgNormal", repeated = FALSE),
          `ordered` = clean_value(`ordered`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `max_set` = clean_value(`max_set`, "TYPE_UINT64", "TYPE_UINT64", repeated = FALSE),
          `window_type` = clean_value(`window_type`, "TYPE_ENUM", "substrait.FunctionSignature.Window.WindowType", repeated = FALSE),
          `implementations` = clean_value(`implementations`, "TYPE_MESSAGE", "substrait.FunctionSignature.Implementation", repeated = TRUE),
          .qualified_name = "substrait.FunctionSignature.Window"
        )
      }
    ),
    create = function(...) {
      rlang::check_dots_empty()
      create_substrait_message(.qualified_name = "substrait.FunctionSignature")
    }
  ),
  JoinRel = list(
    JoinType = list(
      JOIN_TYPE_UNSPECIFIED = structure(0L, class = c("substrait_JoinRel_JoinType", "substrait_proto_enum", "substrait_proto")),
      JOIN_TYPE_INNER = structure(1L, class = c("substrait_JoinRel_JoinType", "substrait_proto_enum", "substrait_proto")),
      JOIN_TYPE_OUTER = structure(2L, class = c("substrait_JoinRel_JoinType", "substrait_proto_enum", "substrait_proto")),
      JOIN_TYPE_LEFT = structure(3L, class = c("substrait_JoinRel_JoinType", "substrait_proto_enum", "substrait_proto")),
      JOIN_TYPE_RIGHT = structure(4L, class = c("substrait_JoinRel_JoinType", "substrait_proto_enum", "substrait_proto")),
      JOIN_TYPE_SEMI = structure(5L, class = c("substrait_JoinRel_JoinType", "substrait_proto_enum", "substrait_proto")),
      JOIN_TYPE_ANTI = structure(6L, class = c("substrait_JoinRel_JoinType", "substrait_proto_enum", "substrait_proto")),
      JOIN_TYPE_SINGLE = structure(7L, class = c("substrait_JoinRel_JoinType", "substrait_proto_enum", "substrait_proto")),
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.JoinRel.JoinType"
        )
      }
    ),
    create = function(..., `common` = arg_unspecified(), `left` = arg_unspecified(), `right` = arg_unspecified(), `expression` = arg_unspecified(), `post_join_filter` = arg_unspecified(), `type` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `left` = clean_value(`left`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `right` = clean_value(`right`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `expression` = clean_value(`expression`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        `post_join_filter` = clean_value(`post_join_filter`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        `type` = clean_value(`type`, "TYPE_ENUM", "substrait.JoinRel.JoinType", repeated = FALSE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.JoinRel"
      )
    }
  ),
  NamedObjectWrite = list(
    create = function(..., `names` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `names` = clean_value(`names`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.NamedObjectWrite"
      )
    }
  ),
  NamedStruct = list(
    create = function(..., `names` = arg_unspecified(), `struct` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `names` = clean_value(`names`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        `struct` = clean_value(`struct`, "TYPE_MESSAGE", "substrait.Type.Struct", repeated = FALSE),
        .qualified_name = "substrait.NamedStruct"
      )
    }
  ),
  ParameterizedType = list(
    IntegerOption = list(
      create = function(..., `literal` = arg_unspecified(), `parameter` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `literal` = clean_value(`literal`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          `parameter` = clean_value(`parameter`, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerParameter", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.IntegerOption"
        )
      }
    ),
    IntegerParameter = list(
      create = function(..., `name` = arg_unspecified(), `range_start_inclusive` = arg_unspecified(), `range_end_exclusive` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `range_start_inclusive` = clean_value(`range_start_inclusive`, "TYPE_MESSAGE", "substrait.ParameterizedType.NullableInteger", repeated = FALSE),
          `range_end_exclusive` = clean_value(`range_end_exclusive`, "TYPE_MESSAGE", "substrait.ParameterizedType.NullableInteger", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.IntegerParameter"
        )
      }
    ),
    NullableInteger = list(
      create = function(..., `value` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `value` = clean_value(`value`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.NullableInteger"
        )
      }
    ),
    ParameterizedDecimal = list(
      create = function(..., `scale` = arg_unspecified(), `precision` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `scale` = clean_value(`scale`, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          `precision` = clean_value(`precision`, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedDecimal"
        )
      }
    ),
    ParameterizedFixedBinary = list(
      create = function(..., `length` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `length` = clean_value(`length`, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedFixedBinary"
        )
      }
    ),
    ParameterizedFixedChar = list(
      create = function(..., `length` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `length` = clean_value(`length`, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedFixedChar"
        )
      }
    ),
    ParameterizedList = list(
      create = function(..., `type` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type` = clean_value(`type`, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedList"
        )
      }
    ),
    ParameterizedMap = list(
      create = function(..., `key` = arg_unspecified(), `value` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `key` = clean_value(`key`, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = FALSE),
          `value` = clean_value(`value`, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedMap"
        )
      }
    ),
    ParameterizedNamedStruct = list(
      create = function(..., `names` = arg_unspecified(), `struct` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `names` = clean_value(`names`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          `struct` = clean_value(`struct`, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedStruct", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedNamedStruct"
        )
      }
    ),
    ParameterizedStruct = list(
      create = function(..., `types` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `types` = clean_value(`types`, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = TRUE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedStruct"
        )
      }
    ),
    ParameterizedUserDefined = list(
      create = function(..., `type_pointer` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_pointer` = clean_value(`type_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedUserDefined"
        )
      }
    ),
    ParameterizedVarChar = list(
      create = function(..., `length` = arg_unspecified(), `variation_pointer` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `length` = clean_value(`length`, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          `variation_pointer` = clean_value(`variation_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedVarChar"
        )
      }
    ),
    TypeParameter = list(
      create = function(..., `name` = arg_unspecified(), `bounds` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `bounds` = clean_value(`bounds`, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = TRUE),
          .qualified_name = "substrait.ParameterizedType.TypeParameter"
        )
      }
    ),
    create = function(..., `bool` = arg_unspecified(), `i8` = arg_unspecified(), `i16` = arg_unspecified(), `i32` = arg_unspecified(), `i64` = arg_unspecified(), `fp32` = arg_unspecified(), `fp64` = arg_unspecified(), `string` = arg_unspecified(), `binary` = arg_unspecified(), `timestamp` = arg_unspecified(), `date` = arg_unspecified(), `time` = arg_unspecified(), `interval_year` = arg_unspecified(), `interval_day` = arg_unspecified(), `timestamp_tz` = arg_unspecified(), `uuid` = arg_unspecified(), `fixed_char` = arg_unspecified(), `varchar` = arg_unspecified(), `fixed_binary` = arg_unspecified(), `decimal` = arg_unspecified(), `struct` = arg_unspecified(), `list` = arg_unspecified(), `map` = arg_unspecified(), `user_defined` = arg_unspecified(), `user_defined_pointer` = arg_unspecified(), `type_parameter` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `bool` = clean_value(`bool`, "TYPE_MESSAGE", "substrait.Type.Boolean", repeated = FALSE),
        `i8` = clean_value(`i8`, "TYPE_MESSAGE", "substrait.Type.I8", repeated = FALSE),
        `i16` = clean_value(`i16`, "TYPE_MESSAGE", "substrait.Type.I16", repeated = FALSE),
        `i32` = clean_value(`i32`, "TYPE_MESSAGE", "substrait.Type.I32", repeated = FALSE),
        `i64` = clean_value(`i64`, "TYPE_MESSAGE", "substrait.Type.I64", repeated = FALSE),
        `fp32` = clean_value(`fp32`, "TYPE_MESSAGE", "substrait.Type.FP32", repeated = FALSE),
        `fp64` = clean_value(`fp64`, "TYPE_MESSAGE", "substrait.Type.FP64", repeated = FALSE),
        `string` = clean_value(`string`, "TYPE_MESSAGE", "substrait.Type.String", repeated = FALSE),
        `binary` = clean_value(`binary`, "TYPE_MESSAGE", "substrait.Type.Binary", repeated = FALSE),
        `timestamp` = clean_value(`timestamp`, "TYPE_MESSAGE", "substrait.Type.Timestamp", repeated = FALSE),
        `date` = clean_value(`date`, "TYPE_MESSAGE", "substrait.Type.Date", repeated = FALSE),
        `time` = clean_value(`time`, "TYPE_MESSAGE", "substrait.Type.Time", repeated = FALSE),
        `interval_year` = clean_value(`interval_year`, "TYPE_MESSAGE", "substrait.Type.IntervalYear", repeated = FALSE),
        `interval_day` = clean_value(`interval_day`, "TYPE_MESSAGE", "substrait.Type.IntervalDay", repeated = FALSE),
        `timestamp_tz` = clean_value(`timestamp_tz`, "TYPE_MESSAGE", "substrait.Type.TimestampTZ", repeated = FALSE),
        `uuid` = clean_value(`uuid`, "TYPE_MESSAGE", "substrait.Type.UUID", repeated = FALSE),
        `fixed_char` = clean_value(`fixed_char`, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedFixedChar", repeated = FALSE),
        `varchar` = clean_value(`varchar`, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedVarChar", repeated = FALSE),
        `fixed_binary` = clean_value(`fixed_binary`, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedFixedBinary", repeated = FALSE),
        `decimal` = clean_value(`decimal`, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedDecimal", repeated = FALSE),
        `struct` = clean_value(`struct`, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedStruct", repeated = FALSE),
        `list` = clean_value(`list`, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedList", repeated = FALSE),
        `map` = clean_value(`map`, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedMap", repeated = FALSE),
        `user_defined` = clean_value(`user_defined`, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedUserDefined", repeated = FALSE),
        `user_defined_pointer` = clean_value(`user_defined_pointer`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        `type_parameter` = clean_value(`type_parameter`, "TYPE_MESSAGE", "substrait.ParameterizedType.TypeParameter", repeated = FALSE),
        .qualified_name = "substrait.ParameterizedType"
      )
    }
  ),
  Plan = list(
    create = function(..., `version` = arg_unspecified(), `extension_uris` = arg_unspecified(), `extensions` = arg_unspecified(), `relations` = arg_unspecified(), `advanced_extensions` = arg_unspecified(), `expected_type_urls` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `version` = clean_value(`version`, "TYPE_MESSAGE", "substrait.Version", repeated = FALSE),
        `extension_uris` = clean_value(`extension_uris`, "TYPE_MESSAGE", "substrait.extensions.SimpleExtensionURI", repeated = TRUE),
        `extensions` = clean_value(`extensions`, "TYPE_MESSAGE", "substrait.extensions.SimpleExtensionDeclaration", repeated = TRUE),
        `relations` = clean_value(`relations`, "TYPE_MESSAGE", "substrait.PlanRel", repeated = TRUE),
        `advanced_extensions` = clean_value(`advanced_extensions`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        `expected_type_urls` = clean_value(`expected_type_urls`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        .qualified_name = "substrait.Plan"
      )
    }
  ),
  PlanRel = list(
    create = function(..., `rel` = arg_unspecified(), `root` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `rel` = clean_value(`rel`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `root` = clean_value(`root`, "TYPE_MESSAGE", "substrait.RelRoot", repeated = FALSE),
        .qualified_name = "substrait.PlanRel"
      )
    }
  ),
  PlanVersion = list(
    create = function(..., `version` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `version` = clean_value(`version`, "TYPE_MESSAGE", "substrait.Version", repeated = FALSE),
        .qualified_name = "substrait.PlanVersion"
      )
    }
  ),
  ProjectRel = list(
    create = function(..., `common` = arg_unspecified(), `input` = arg_unspecified(), `expressions` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `expressions` = clean_value(`expressions`, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.ProjectRel"
      )
    }
  ),
  ReadRel = list(
    ExtensionTable = list(
      create = function(..., `detail` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `detail` = clean_value(`detail`, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
          .qualified_name = "substrait.ReadRel.ExtensionTable"
        )
      }
    ),
    LocalFiles = list(
      FileOrFiles = list(
        ArrowReadOptions = list(
          create = function(...) {
            rlang::check_dots_empty()
            create_substrait_message(.qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles.ArrowReadOptions")
          }
        ),
        DwrfReadOptions = list(
          create = function(...) {
            rlang::check_dots_empty()
            create_substrait_message(.qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles.DwrfReadOptions")
          }
        ),
        OrcReadOptions = list(
          create = function(...) {
            rlang::check_dots_empty()
            create_substrait_message(.qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles.OrcReadOptions")
          }
        ),
        ParquetReadOptions = list(
          create = function(...) {
            rlang::check_dots_empty()
            create_substrait_message(.qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles.ParquetReadOptions")
          }
        ),
        create = function(..., `uri_path` = arg_unspecified(), `uri_path_glob` = arg_unspecified(), `uri_file` = arg_unspecified(), `uri_folder` = arg_unspecified(), `partition_index` = arg_unspecified(), `start` = arg_unspecified(), `length` = arg_unspecified(), `parquet` = arg_unspecified(), `arrow` = arg_unspecified(), `orc` = arg_unspecified(), `extension` = arg_unspecified(), `dwrf` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `uri_path` = clean_value(`uri_path`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            `uri_path_glob` = clean_value(`uri_path_glob`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            `uri_file` = clean_value(`uri_file`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            `uri_folder` = clean_value(`uri_folder`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            `partition_index` = clean_value(`partition_index`, "TYPE_UINT64", "TYPE_UINT64", repeated = FALSE),
            `start` = clean_value(`start`, "TYPE_UINT64", "TYPE_UINT64", repeated = FALSE),
            `length` = clean_value(`length`, "TYPE_UINT64", "TYPE_UINT64", repeated = FALSE),
            `parquet` = clean_value(`parquet`, "TYPE_MESSAGE", "substrait.ReadRel.LocalFiles.FileOrFiles.ParquetReadOptions", repeated = FALSE),
            `arrow` = clean_value(`arrow`, "TYPE_MESSAGE", "substrait.ReadRel.LocalFiles.FileOrFiles.ArrowReadOptions", repeated = FALSE),
            `orc` = clean_value(`orc`, "TYPE_MESSAGE", "substrait.ReadRel.LocalFiles.FileOrFiles.OrcReadOptions", repeated = FALSE),
            `extension` = clean_value(`extension`, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
            `dwrf` = clean_value(`dwrf`, "TYPE_MESSAGE", "substrait.ReadRel.LocalFiles.FileOrFiles.DwrfReadOptions", repeated = FALSE),
            .qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles"
          )
        }
      ),
      create = function(..., `items` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `items` = clean_value(`items`, "TYPE_MESSAGE", "substrait.ReadRel.LocalFiles.FileOrFiles", repeated = TRUE),
          `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
          .qualified_name = "substrait.ReadRel.LocalFiles"
        )
      }
    ),
    NamedTable = list(
      create = function(..., `names` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `names` = clean_value(`names`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
          .qualified_name = "substrait.ReadRel.NamedTable"
        )
      }
    ),
    VirtualTable = list(
      create = function(..., `values` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `values` = clean_value(`values`, "TYPE_MESSAGE", "substrait.Expression.Literal.Struct", repeated = TRUE),
          .qualified_name = "substrait.ReadRel.VirtualTable"
        )
      }
    ),
    create = function(..., `common` = arg_unspecified(), `base_schema` = arg_unspecified(), `filter` = arg_unspecified(), `best_effort_filter` = arg_unspecified(), `projection` = arg_unspecified(), `advanced_extension` = arg_unspecified(), `virtual_table` = arg_unspecified(), `local_files` = arg_unspecified(), `named_table` = arg_unspecified(), `extension_table` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `base_schema` = clean_value(`base_schema`, "TYPE_MESSAGE", "substrait.NamedStruct", repeated = FALSE),
        `filter` = clean_value(`filter`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        `best_effort_filter` = clean_value(`best_effort_filter`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        `projection` = clean_value(`projection`, "TYPE_MESSAGE", "substrait.Expression.MaskExpression", repeated = FALSE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        `virtual_table` = clean_value(`virtual_table`, "TYPE_MESSAGE", "substrait.ReadRel.VirtualTable", repeated = FALSE),
        `local_files` = clean_value(`local_files`, "TYPE_MESSAGE", "substrait.ReadRel.LocalFiles", repeated = FALSE),
        `named_table` = clean_value(`named_table`, "TYPE_MESSAGE", "substrait.ReadRel.NamedTable", repeated = FALSE),
        `extension_table` = clean_value(`extension_table`, "TYPE_MESSAGE", "substrait.ReadRel.ExtensionTable", repeated = FALSE),
        .qualified_name = "substrait.ReadRel"
      )
    }
  ),
  Rel = list(
    create = function(..., `read` = arg_unspecified(), `filter` = arg_unspecified(), `fetch` = arg_unspecified(), `aggregate` = arg_unspecified(), `sort` = arg_unspecified(), `join` = arg_unspecified(), `project` = arg_unspecified(), `set` = arg_unspecified(), `extension_single` = arg_unspecified(), `extension_multi` = arg_unspecified(), `extension_leaf` = arg_unspecified(), `cross` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `read` = clean_value(`read`, "TYPE_MESSAGE", "substrait.ReadRel", repeated = FALSE),
        `filter` = clean_value(`filter`, "TYPE_MESSAGE", "substrait.FilterRel", repeated = FALSE),
        `fetch` = clean_value(`fetch`, "TYPE_MESSAGE", "substrait.FetchRel", repeated = FALSE),
        `aggregate` = clean_value(`aggregate`, "TYPE_MESSAGE", "substrait.AggregateRel", repeated = FALSE),
        `sort` = clean_value(`sort`, "TYPE_MESSAGE", "substrait.SortRel", repeated = FALSE),
        `join` = clean_value(`join`, "TYPE_MESSAGE", "substrait.JoinRel", repeated = FALSE),
        `project` = clean_value(`project`, "TYPE_MESSAGE", "substrait.ProjectRel", repeated = FALSE),
        `set` = clean_value(`set`, "TYPE_MESSAGE", "substrait.SetRel", repeated = FALSE),
        `extension_single` = clean_value(`extension_single`, "TYPE_MESSAGE", "substrait.ExtensionSingleRel", repeated = FALSE),
        `extension_multi` = clean_value(`extension_multi`, "TYPE_MESSAGE", "substrait.ExtensionMultiRel", repeated = FALSE),
        `extension_leaf` = clean_value(`extension_leaf`, "TYPE_MESSAGE", "substrait.ExtensionLeafRel", repeated = FALSE),
        `cross` = clean_value(`cross`, "TYPE_MESSAGE", "substrait.CrossRel", repeated = FALSE),
        .qualified_name = "substrait.Rel"
      )
    }
  ),
  RelCommon = list(
    Direct = list(
      create = function(...) {
        rlang::check_dots_empty()
        create_substrait_message(.qualified_name = "substrait.RelCommon.Direct")
      }
    ),
    Emit = list(
      create = function(..., `output_mapping` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `output_mapping` = clean_value(`output_mapping`, "TYPE_INT32", "TYPE_INT32", repeated = TRUE),
          .qualified_name = "substrait.RelCommon.Emit"
        )
      }
    ),
    Hint = list(
      RuntimeConstraint = list(
        create = function(..., `advanced_extension` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
            .qualified_name = "substrait.RelCommon.Hint.RuntimeConstraint"
          )
        }
      ),
      Stats = list(
        create = function(..., `row_count` = arg_unspecified(), `record_size` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `row_count` = clean_value(`row_count`, "TYPE_DOUBLE", "TYPE_DOUBLE", repeated = FALSE),
            `record_size` = clean_value(`record_size`, "TYPE_DOUBLE", "TYPE_DOUBLE", repeated = FALSE),
            `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
            .qualified_name = "substrait.RelCommon.Hint.Stats"
          )
        }
      ),
      create = function(..., `stats` = arg_unspecified(), `constraint` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `stats` = clean_value(`stats`, "TYPE_MESSAGE", "substrait.RelCommon.Hint.Stats", repeated = FALSE),
          `constraint` = clean_value(`constraint`, "TYPE_MESSAGE", "substrait.RelCommon.Hint.RuntimeConstraint", repeated = FALSE),
          `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
          .qualified_name = "substrait.RelCommon.Hint"
        )
      }
    ),
    create = function(..., `direct` = arg_unspecified(), `emit` = arg_unspecified(), `hint` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `direct` = clean_value(`direct`, "TYPE_MESSAGE", "substrait.RelCommon.Direct", repeated = FALSE),
        `emit` = clean_value(`emit`, "TYPE_MESSAGE", "substrait.RelCommon.Emit", repeated = FALSE),
        `hint` = clean_value(`hint`, "TYPE_MESSAGE", "substrait.RelCommon.Hint", repeated = FALSE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.RelCommon"
      )
    }
  ),
  RelRoot = list(
    create = function(..., `input` = arg_unspecified(), `names` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `names` = clean_value(`names`, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        .qualified_name = "substrait.RelRoot"
      )
    }
  ),
  SetRel = list(
    SetOp = list(
      SET_OP_UNSPECIFIED = structure(0L, class = c("substrait_SetRel_SetOp", "substrait_proto_enum", "substrait_proto")),
      SET_OP_MINUS_PRIMARY = structure(1L, class = c("substrait_SetRel_SetOp", "substrait_proto_enum", "substrait_proto")),
      SET_OP_MINUS_MULTISET = structure(2L, class = c("substrait_SetRel_SetOp", "substrait_proto_enum", "substrait_proto")),
      SET_OP_INTERSECTION_PRIMARY = structure(3L, class = c("substrait_SetRel_SetOp", "substrait_proto_enum", "substrait_proto")),
      SET_OP_INTERSECTION_MULTISET = structure(4L, class = c("substrait_SetRel_SetOp", "substrait_proto_enum", "substrait_proto")),
      SET_OP_UNION_DISTINCT = structure(5L, class = c("substrait_SetRel_SetOp", "substrait_proto_enum", "substrait_proto")),
      SET_OP_UNION_ALL = structure(6L, class = c("substrait_SetRel_SetOp", "substrait_proto_enum", "substrait_proto")),
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.SetRel.SetOp"
        )
      }
    ),
    create = function(..., `common` = arg_unspecified(), `inputs` = arg_unspecified(), `op` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `inputs` = clean_value(`inputs`, "TYPE_MESSAGE", "substrait.Rel", repeated = TRUE),
        `op` = clean_value(`op`, "TYPE_ENUM", "substrait.SetRel.SetOp", repeated = FALSE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.SetRel"
      )
    }
  ),
  SortField = list(
    SortDirection = list(
      SORT_DIRECTION_UNSPECIFIED = structure(0L, class = c("substrait_SortField_SortDirection", "substrait_proto_enum", "substrait_proto")),
      SORT_DIRECTION_ASC_NULLS_FIRST = structure(1L, class = c("substrait_SortField_SortDirection", "substrait_proto_enum", "substrait_proto")),
      SORT_DIRECTION_ASC_NULLS_LAST = structure(2L, class = c("substrait_SortField_SortDirection", "substrait_proto_enum", "substrait_proto")),
      SORT_DIRECTION_DESC_NULLS_FIRST = structure(3L, class = c("substrait_SortField_SortDirection", "substrait_proto_enum", "substrait_proto")),
      SORT_DIRECTION_DESC_NULLS_LAST = structure(4L, class = c("substrait_SortField_SortDirection", "substrait_proto_enum", "substrait_proto")),
      SORT_DIRECTION_CLUSTERED = structure(5L, class = c("substrait_SortField_SortDirection", "substrait_proto_enum", "substrait_proto")),
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.SortField.SortDirection"
        )
      }
    ),
    create = function(..., `expr` = arg_unspecified(), `direction` = arg_unspecified(), `comparison_function_reference` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `expr` = clean_value(`expr`, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        `direction` = clean_value(`direction`, "TYPE_ENUM", "substrait.SortField.SortDirection", repeated = FALSE),
        `comparison_function_reference` = clean_value(`comparison_function_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        .qualified_name = "substrait.SortField"
      )
    }
  ),
  SortRel = list(
    create = function(..., `common` = arg_unspecified(), `input` = arg_unspecified(), `sorts` = arg_unspecified(), `advanced_extension` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `common` = clean_value(`common`, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `sorts` = clean_value(`sorts`, "TYPE_MESSAGE", "substrait.SortField", repeated = TRUE),
        `advanced_extension` = clean_value(`advanced_extension`, "TYPE_MESSAGE", "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.SortRel"
      )
    }
  ),
  Type = list(
    Nullability = list(
      NULLABILITY_UNSPECIFIED = structure(0L, class = c("substrait_Type_Nullability", "substrait_proto_enum", "substrait_proto")),
      NULLABILITY_NULLABLE = structure(1L, class = c("substrait_Type_Nullability", "substrait_proto_enum", "substrait_proto")),
      NULLABILITY_REQUIRED = structure(2L, class = c("substrait_Type_Nullability", "substrait_proto_enum", "substrait_proto")),
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.Type.Nullability"
        )
      }
    ),
    Binary = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Binary"
        )
      }
    ),
    Boolean = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Boolean"
        )
      }
    ),
    Date = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Date"
        )
      }
    ),
    Decimal = list(
      create = function(..., `scale` = arg_unspecified(), `precision` = arg_unspecified(), `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `scale` = clean_value(`scale`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          `precision` = clean_value(`precision`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Decimal"
        )
      }
    ),
    FixedBinary = list(
      create = function(..., `length` = arg_unspecified(), `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `length` = clean_value(`length`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FixedBinary"
        )
      }
    ),
    FixedChar = list(
      create = function(..., `length` = arg_unspecified(), `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `length` = clean_value(`length`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FixedChar"
        )
      }
    ),
    FP32 = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FP32"
        )
      }
    ),
    FP64 = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FP64"
        )
      }
    ),
    I16 = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I16"
        )
      }
    ),
    I32 = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I32"
        )
      }
    ),
    I64 = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I64"
        )
      }
    ),
    I8 = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I8"
        )
      }
    ),
    IntervalDay = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.IntervalDay"
        )
      }
    ),
    IntervalYear = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.IntervalYear"
        )
      }
    ),
    List = list(
      create = function(..., `type` = arg_unspecified(), `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type` = clean_value(`type`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.List"
        )
      }
    ),
    Map = list(
      create = function(..., `key` = arg_unspecified(), `value` = arg_unspecified(), `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `key` = clean_value(`key`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          `value` = clean_value(`value`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Map"
        )
      }
    ),
    Parameter = list(
      create = function(..., `null` = arg_unspecified(), `data_type` = arg_unspecified(), `boolean` = arg_unspecified(), `integer` = arg_unspecified(), `enum` = arg_unspecified(), `string` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `null` = clean_value(`null`, "TYPE_MESSAGE", "substrait.Empty", repeated = FALSE),
          `data_type` = clean_value(`data_type`, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          `boolean` = clean_value(`boolean`, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          `integer` = clean_value(`integer`, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          `enum` = clean_value(`enum`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          `string` = clean_value(`string`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          .qualified_name = "substrait.Type.Parameter"
        )
      }
    ),
    String = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.String"
        )
      }
    ),
    Struct = list(
      create = function(..., `types` = arg_unspecified(), `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `types` = clean_value(`types`, "TYPE_MESSAGE", "substrait.Type", repeated = TRUE),
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Struct"
        )
      }
    ),
    Time = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Time"
        )
      }
    ),
    Timestamp = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Timestamp"
        )
      }
    ),
    TimestampTZ = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.TimestampTZ"
        )
      }
    ),
    UserDefined = list(
      create = function(..., `type_reference` = arg_unspecified(), `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified(), `type_parameters` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_reference` = clean_value(`type_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          `type_parameters` = clean_value(`type_parameters`, "TYPE_MESSAGE", "substrait.Type.Parameter", repeated = TRUE),
          .qualified_name = "substrait.Type.UserDefined"
        )
      }
    ),
    UUID = list(
      create = function(..., `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.UUID"
        )
      }
    ),
    VarChar = list(
      create = function(..., `length` = arg_unspecified(), `type_variation_reference` = arg_unspecified(), `nullability` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `length` = clean_value(`length`, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          `type_variation_reference` = clean_value(`type_variation_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `nullability` = clean_value(`nullability`, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.VarChar"
        )
      }
    ),
    create = function(..., `bool` = arg_unspecified(), `i8` = arg_unspecified(), `i16` = arg_unspecified(), `i32` = arg_unspecified(), `i64` = arg_unspecified(), `fp32` = arg_unspecified(), `fp64` = arg_unspecified(), `string` = arg_unspecified(), `binary` = arg_unspecified(), `timestamp` = arg_unspecified(), `date` = arg_unspecified(), `time` = arg_unspecified(), `interval_year` = arg_unspecified(), `interval_day` = arg_unspecified(), `timestamp_tz` = arg_unspecified(), `uuid` = arg_unspecified(), `fixed_char` = arg_unspecified(), `varchar` = arg_unspecified(), `fixed_binary` = arg_unspecified(), `decimal` = arg_unspecified(), `struct` = arg_unspecified(), `list` = arg_unspecified(), `map` = arg_unspecified(), `user_defined` = arg_unspecified(), `user_defined_type_reference` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `bool` = clean_value(`bool`, "TYPE_MESSAGE", "substrait.Type.Boolean", repeated = FALSE),
        `i8` = clean_value(`i8`, "TYPE_MESSAGE", "substrait.Type.I8", repeated = FALSE),
        `i16` = clean_value(`i16`, "TYPE_MESSAGE", "substrait.Type.I16", repeated = FALSE),
        `i32` = clean_value(`i32`, "TYPE_MESSAGE", "substrait.Type.I32", repeated = FALSE),
        `i64` = clean_value(`i64`, "TYPE_MESSAGE", "substrait.Type.I64", repeated = FALSE),
        `fp32` = clean_value(`fp32`, "TYPE_MESSAGE", "substrait.Type.FP32", repeated = FALSE),
        `fp64` = clean_value(`fp64`, "TYPE_MESSAGE", "substrait.Type.FP64", repeated = FALSE),
        `string` = clean_value(`string`, "TYPE_MESSAGE", "substrait.Type.String", repeated = FALSE),
        `binary` = clean_value(`binary`, "TYPE_MESSAGE", "substrait.Type.Binary", repeated = FALSE),
        `timestamp` = clean_value(`timestamp`, "TYPE_MESSAGE", "substrait.Type.Timestamp", repeated = FALSE),
        `date` = clean_value(`date`, "TYPE_MESSAGE", "substrait.Type.Date", repeated = FALSE),
        `time` = clean_value(`time`, "TYPE_MESSAGE", "substrait.Type.Time", repeated = FALSE),
        `interval_year` = clean_value(`interval_year`, "TYPE_MESSAGE", "substrait.Type.IntervalYear", repeated = FALSE),
        `interval_day` = clean_value(`interval_day`, "TYPE_MESSAGE", "substrait.Type.IntervalDay", repeated = FALSE),
        `timestamp_tz` = clean_value(`timestamp_tz`, "TYPE_MESSAGE", "substrait.Type.TimestampTZ", repeated = FALSE),
        `uuid` = clean_value(`uuid`, "TYPE_MESSAGE", "substrait.Type.UUID", repeated = FALSE),
        `fixed_char` = clean_value(`fixed_char`, "TYPE_MESSAGE", "substrait.Type.FixedChar", repeated = FALSE),
        `varchar` = clean_value(`varchar`, "TYPE_MESSAGE", "substrait.Type.VarChar", repeated = FALSE),
        `fixed_binary` = clean_value(`fixed_binary`, "TYPE_MESSAGE", "substrait.Type.FixedBinary", repeated = FALSE),
        `decimal` = clean_value(`decimal`, "TYPE_MESSAGE", "substrait.Type.Decimal", repeated = FALSE),
        `struct` = clean_value(`struct`, "TYPE_MESSAGE", "substrait.Type.Struct", repeated = FALSE),
        `list` = clean_value(`list`, "TYPE_MESSAGE", "substrait.Type.List", repeated = FALSE),
        `map` = clean_value(`map`, "TYPE_MESSAGE", "substrait.Type.Map", repeated = FALSE),
        `user_defined` = clean_value(`user_defined`, "TYPE_MESSAGE", "substrait.Type.UserDefined", repeated = FALSE),
        `user_defined_type_reference` = clean_value(`user_defined_type_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        .qualified_name = "substrait.Type"
      )
    }
  ),
  Version = list(
    create = function(..., `major_number` = arg_unspecified(), `minor_number` = arg_unspecified(), `patch_number` = arg_unspecified(), `git_hash` = arg_unspecified(), `producer` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `major_number` = clean_value(`major_number`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        `minor_number` = clean_value(`minor_number`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        `patch_number` = clean_value(`patch_number`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        `git_hash` = clean_value(`git_hash`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
        `producer` = clean_value(`producer`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
        .qualified_name = "substrait.Version"
      )
    }
  ),
  WriteRel = list(
    OutputMode = list(
      OUTPUT_MODE_UNSPECIFIED = structure(0L, class = c("substrait_WriteRel_OutputMode", "substrait_proto_enum", "substrait_proto")),
      OUTPUT_MODE_NO_OUTPUT = structure(1L, class = c("substrait_WriteRel_OutputMode", "substrait_proto_enum", "substrait_proto")),
      OUTPUT_MODE_MODIFIED_TUPLES = structure(2L, class = c("substrait_WriteRel_OutputMode", "substrait_proto_enum", "substrait_proto")),
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.WriteRel.OutputMode"
        )
      }
    ),
    WriteOp = list(
      WRITE_OP_UNSPECIFIED = structure(0L, class = c("substrait_WriteRel_WriteOp", "substrait_proto_enum", "substrait_proto")),
      WRITE_OP_INSERT = structure(1L, class = c("substrait_WriteRel_WriteOp", "substrait_proto_enum", "substrait_proto")),
      WRITE_OP_DELETE = structure(2L, class = c("substrait_WriteRel_WriteOp", "substrait_proto_enum", "substrait_proto")),
      WRITE_OP_UPDATE = structure(3L, class = c("substrait_WriteRel_WriteOp", "substrait_proto_enum", "substrait_proto")),
      WRITE_OP_CTAS = structure(4L, class = c("substrait_WriteRel_WriteOp", "substrait_proto_enum", "substrait_proto")),
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.WriteRel.WriteOp"
        )
      }
    ),
    create = function(..., `named_table` = arg_unspecified(), `extension_table` = arg_unspecified(), `table_schema` = arg_unspecified(), `op` = arg_unspecified(), `input` = arg_unspecified(), `output` = arg_unspecified()) {
      rlang::check_dots_empty()
      create_substrait_message(
        `named_table` = clean_value(`named_table`, "TYPE_MESSAGE", "substrait.NamedObjectWrite", repeated = FALSE),
        `extension_table` = clean_value(`extension_table`, "TYPE_MESSAGE", "substrait.ExtensionObject", repeated = FALSE),
        `table_schema` = clean_value(`table_schema`, "TYPE_MESSAGE", "substrait.NamedStruct", repeated = FALSE),
        `op` = clean_value(`op`, "TYPE_ENUM", "substrait.WriteRel.WriteOp", repeated = FALSE),
        `input` = clean_value(`input`, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        `output` = clean_value(`output`, "TYPE_ENUM", "substrait.WriteRel.OutputMode", repeated = FALSE),
        .qualified_name = "substrait.WriteRel"
      )
    }
  ),
  extensions = list(
    AdvancedExtension = list(
      create = function(..., `optimization` = arg_unspecified(), `enhancement` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `optimization` = clean_value(`optimization`, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
          `enhancement` = clean_value(`enhancement`, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
          .qualified_name = "substrait.extensions.AdvancedExtension"
        )
      }
    ),
    SimpleExtensionDeclaration = list(
      ExtensionFunction = list(
        create = function(..., `extension_uri_reference` = arg_unspecified(), `function_anchor` = arg_unspecified(), `name` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `extension_uri_reference` = clean_value(`extension_uri_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
            `function_anchor` = clean_value(`function_anchor`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
            `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            .qualified_name = "substrait.extensions.SimpleExtensionDeclaration.ExtensionFunction"
          )
        }
      ),
      ExtensionType = list(
        create = function(..., `extension_uri_reference` = arg_unspecified(), `type_anchor` = arg_unspecified(), `name` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `extension_uri_reference` = clean_value(`extension_uri_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
            `type_anchor` = clean_value(`type_anchor`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
            `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            .qualified_name = "substrait.extensions.SimpleExtensionDeclaration.ExtensionType"
          )
        }
      ),
      ExtensionTypeVariation = list(
        create = function(..., `extension_uri_reference` = arg_unspecified(), `type_variation_anchor` = arg_unspecified(), `name` = arg_unspecified()) {
          rlang::check_dots_empty()
          create_substrait_message(
            `extension_uri_reference` = clean_value(`extension_uri_reference`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
            `type_variation_anchor` = clean_value(`type_variation_anchor`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
            `name` = clean_value(`name`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            .qualified_name = "substrait.extensions.SimpleExtensionDeclaration.ExtensionTypeVariation"
          )
        }
      ),
      create = function(..., `extension_type` = arg_unspecified(), `extension_type_variation` = arg_unspecified(), `extension_function` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `extension_type` = clean_value(`extension_type`, "TYPE_MESSAGE", "substrait.extensions.SimpleExtensionDeclaration.ExtensionType", repeated = FALSE),
          `extension_type_variation` = clean_value(`extension_type_variation`, "TYPE_MESSAGE", "substrait.extensions.SimpleExtensionDeclaration.ExtensionTypeVariation", repeated = FALSE),
          `extension_function` = clean_value(`extension_function`, "TYPE_MESSAGE", "substrait.extensions.SimpleExtensionDeclaration.ExtensionFunction", repeated = FALSE),
          .qualified_name = "substrait.extensions.SimpleExtensionDeclaration"
        )
      }
    ),
    SimpleExtensionURI = list(
      create = function(..., `extension_uri_anchor` = arg_unspecified(), `uri` = arg_unspecified()) {
        rlang::check_dots_empty()
        create_substrait_message(
          `extension_uri_anchor` = clean_value(`extension_uri_anchor`, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          `uri` = clean_value(`uri`, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          .qualified_name = "substrait.extensions.SimpleExtensionURI"
        )
      }
    )
  )
)
