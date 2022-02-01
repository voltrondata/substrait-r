
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
  Any = list(
    create = function(type_url = unspecified(), value = unspecified()) {
      create_substrait_message(
        type_url = clean_value(type_url, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
        value = clean_value(value, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
        .qualified_name = "substrait.Any"
      )
    }
  ),
  Capabilities = list(
    SimpleExtension = list(
      create = function(uri = unspecified(), function_keys = unspecified(), type_keys = unspecified(), type_variation_keys = unspecified()) {
        create_substrait_message(
          uri = clean_value(uri, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          function_keys = clean_value(function_keys, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          type_keys = clean_value(type_keys, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          type_variation_keys = clean_value(type_variation_keys, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          .qualified_name = "substrait.Capabilities.SimpleExtension"
        )
      }
    ),
    create = function(substrait_versions = unspecified(), advanced_extension_type_urls = unspecified(), simple_extensions = unspecified()) {
      create_substrait_message(
        substrait_versions = clean_value(substrait_versions, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        advanced_extension_type_urls = clean_value(advanced_extension_type_urls, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        simple_extensions = clean_value(simple_extensions, "TYPE_MESSAGE", "substrait.Capabilities.SimpleExtension", repeated = TRUE),
        .qualified_name = "substrait.Capabilities"
      )
    }
  ),
  AggregateFunction = list(
    create = function(function_reference = unspecified(), args = unspecified(), sorts = unspecified(), phase = unspecified(), output_type = unspecified()) {
      create_substrait_message(
        function_reference = clean_value(function_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        args = clean_value(args, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
        sorts = clean_value(sorts, "TYPE_MESSAGE", "substrait.SortField", repeated = TRUE),
        phase = clean_value(phase, "TYPE_ENUM", "substrait.AggregationPhase", repeated = FALSE),
        output_type = clean_value(output_type, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
        .qualified_name = "substrait.AggregateFunction"
      )
    }
  ),
  Expression = list(
    Cast = list(
      create = function(type = unspecified(), input = unspecified()) {
        create_substrait_message(
          type = clean_value(type, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          input = clean_value(input, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.Expression.Cast"
        )
      }
    ),
    EmbeddedFunction = list(
      PythonPickleFunction = list(
        create = function(function_ = unspecified(), prerequisite = unspecified()) {
          create_substrait_message(
            function_ = clean_value(function_, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
            prerequisite = clean_value(prerequisite, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
            .qualified_name = "substrait.Expression.EmbeddedFunction.PythonPickleFunction"
          )
        }
      ),
      WebAssemblyFunction = list(
        create = function(script = unspecified(), prerequisite = unspecified()) {
          create_substrait_message(
            script = clean_value(script, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
            prerequisite = clean_value(prerequisite, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
            .qualified_name = "substrait.Expression.EmbeddedFunction.WebAssemblyFunction"
          )
        }
      ),
      create = function(arguments = unspecified(), output_type = unspecified(), python_pickle_function = unspecified(), web_assembly_function = unspecified()) {
        create_substrait_message(
          arguments = clean_value(arguments, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          output_type = clean_value(output_type, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          python_pickle_function = clean_value(python_pickle_function, "TYPE_MESSAGE", "substrait.Expression.EmbeddedFunction.PythonPickleFunction", repeated = FALSE),
          web_assembly_function = clean_value(web_assembly_function, "TYPE_MESSAGE", "substrait.Expression.EmbeddedFunction.WebAssemblyFunction", repeated = FALSE),
          .qualified_name = "substrait.Expression.EmbeddedFunction"
        )
      }
    ),
    Enum = list(
      Empty = list(
        create = function() create_substrait_message(.qualified_name = "substrait.Expression.Enum.Empty")
      ),
      create = function(specified = unspecified(), unspecified = unspecified()) {
        create_substrait_message(
          specified = clean_value(specified, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          unspecified = clean_value(unspecified, "TYPE_MESSAGE", "substrait.Expression.Enum.Empty", repeated = FALSE),
          .qualified_name = "substrait.Expression.Enum"
        )
      }
    ),
    FieldReference = list(
      RootReference = list(
        create = function() create_substrait_message(.qualified_name = "substrait.Expression.FieldReference.RootReference")
      ),
      create = function(direct_reference = unspecified(), masked_reference = unspecified(), expression = unspecified(), root_reference = unspecified()) {
        create_substrait_message(
          direct_reference = clean_value(direct_reference, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment", repeated = FALSE),
          masked_reference = clean_value(masked_reference, "TYPE_MESSAGE", "substrait.Expression.MaskExpression", repeated = FALSE),
          expression = clean_value(expression, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          root_reference = clean_value(root_reference, "TYPE_MESSAGE", "substrait.Expression.FieldReference.RootReference", repeated = FALSE),
          .qualified_name = "substrait.Expression.FieldReference"
        )
      }
    ),
    IfThen = list(
      IfClause = list(
        create = function(if_ = unspecified(), then = unspecified()) {
          create_substrait_message(
            if_ = clean_value(if_, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
            then = clean_value(then, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
            .qualified_name = "substrait.Expression.IfThen.IfClause"
          )
        }
      ),
      create = function(ifs = unspecified(), else_ = unspecified()) {
        create_substrait_message(
          ifs = clean_value(ifs, "TYPE_MESSAGE", "substrait.Expression.IfThen.IfClause", repeated = TRUE),
          else_ = clean_value(else_, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.Expression.IfThen"
        )
      }
    ),
    Literal = list(
      Decimal = list(
        create = function(value = unspecified(), precision = unspecified(), scale = unspecified()) {
          create_substrait_message(
            value = clean_value(value, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
            precision = clean_value(precision, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            scale = clean_value(scale, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.Decimal"
          )
        }
      ),
      IntervalDayToSecond = list(
        create = function(days = unspecified(), seconds = unspecified()) {
          create_substrait_message(
            days = clean_value(days, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            seconds = clean_value(seconds, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.IntervalDayToSecond"
          )
        }
      ),
      IntervalYearToMonth = list(
        create = function(years = unspecified(), months = unspecified()) {
          create_substrait_message(
            years = clean_value(years, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            months = clean_value(months, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.IntervalYearToMonth"
          )
        }
      ),
      List = list(
        create = function(values = unspecified()) {
          create_substrait_message(
            values = clean_value(values, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = TRUE),
            .qualified_name = "substrait.Expression.Literal.List"
          )
        }
      ),
      Map = list(
        KeyValue = list(
          create = function(key = unspecified(), value = unspecified()) {
            create_substrait_message(
              key = clean_value(key, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = FALSE),
              value = clean_value(value, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = FALSE),
              .qualified_name = "substrait.Expression.Literal.Map.KeyValue"
            )
          }
        ),
        create = function(key_values = unspecified()) {
          create_substrait_message(
            key_values = clean_value(key_values, "TYPE_MESSAGE", "substrait.Expression.Literal.Map.KeyValue", repeated = TRUE),
            .qualified_name = "substrait.Expression.Literal.Map"
          )
        }
      ),
      Struct = list(
        create = function(fields = unspecified()) {
          create_substrait_message(
            fields = clean_value(fields, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = TRUE),
            .qualified_name = "substrait.Expression.Literal.Struct"
          )
        }
      ),
      VarChar = list(
        create = function(value = unspecified(), length = unspecified()) {
          create_substrait_message(
            value = clean_value(value, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            length = clean_value(length, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.VarChar"
          )
        }
      ),
      create = function(boolean = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year_to_month = unspecified(), interval_day_to_second = unspecified(), fixed_char = unspecified(), var_char = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), map = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), null = unspecified(), list = unspecified(), empty_list = unspecified(), empty_map = unspecified(), nullable = unspecified()) {
        create_substrait_message(
          boolean = clean_value(boolean, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          i8 = clean_value(i8, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          i16 = clean_value(i16, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          i32 = clean_value(i32, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          i64 = clean_value(i64, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          fp32 = clean_value(fp32, "TYPE_FLOAT", "TYPE_FLOAT", repeated = FALSE),
          fp64 = clean_value(fp64, "TYPE_DOUBLE", "TYPE_DOUBLE", repeated = FALSE),
          string = clean_value(string, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          binary = clean_value(binary, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
          timestamp = clean_value(timestamp, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          date = clean_value(date, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          time = clean_value(time, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          interval_year_to_month = clean_value(interval_year_to_month, "TYPE_MESSAGE", "substrait.Expression.Literal.IntervalYearToMonth", repeated = FALSE),
          interval_day_to_second = clean_value(interval_day_to_second, "TYPE_MESSAGE", "substrait.Expression.Literal.IntervalDayToSecond", repeated = FALSE),
          fixed_char = clean_value(fixed_char, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          var_char = clean_value(var_char, "TYPE_MESSAGE", "substrait.Expression.Literal.VarChar", repeated = FALSE),
          fixed_binary = clean_value(fixed_binary, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
          decimal = clean_value(decimal, "TYPE_MESSAGE", "substrait.Expression.Literal.Decimal", repeated = FALSE),
          struct_ = clean_value(struct_, "TYPE_MESSAGE", "substrait.Expression.Literal.Struct", repeated = FALSE),
          map = clean_value(map, "TYPE_MESSAGE", "substrait.Expression.Literal.Map", repeated = FALSE),
          timestamp_tz = clean_value(timestamp_tz, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          uuid = clean_value(uuid, "TYPE_BYTES", "TYPE_BYTES", repeated = FALSE),
          null = clean_value(null, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          list = clean_value(list, "TYPE_MESSAGE", "substrait.Expression.Literal.List", repeated = FALSE),
          empty_list = clean_value(empty_list, "TYPE_MESSAGE", "substrait.Type.List", repeated = FALSE),
          empty_map = clean_value(empty_map, "TYPE_MESSAGE", "substrait.Type.Map", repeated = FALSE),
          nullable = clean_value(nullable, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          .qualified_name = "substrait.Expression.Literal"
        )
      }
    ),
    MaskExpression = list(
      ListSelect = list(
        ListSelectItem = list(
          ListElement = list(
            create = function(field = unspecified()) {
              create_substrait_message(
                field = clean_value(field, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
                .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListElement"
              )
            }
          ),
          ListSlice = list(
            create = function(start = unspecified(), end = unspecified()) {
              create_substrait_message(
                start = clean_value(start, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
                end = clean_value(end, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
                .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListSlice"
              )
            }
          ),
          create = function(item = unspecified(), slice = unspecified()) {
            create_substrait_message(
              item = clean_value(item, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListElement", repeated = FALSE),
              slice = clean_value(slice, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListSlice", repeated = FALSE),
              .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem"
            )
          }
        ),
        create = function(selection = unspecified(), child = unspecified()) {
          create_substrait_message(
            selection = clean_value(selection, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.ListSelect.ListSelectItem", repeated = TRUE),
            child = clean_value(child, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.Select", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.ListSelect"
          )
        }
      ),
      MapSelect = list(
        MapKey = list(
          create = function(map_key = unspecified()) {
            create_substrait_message(
              map_key = clean_value(map_key, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
              .qualified_name = "substrait.Expression.MaskExpression.MapSelect.MapKey"
            )
          }
        ),
        MapKeyExpression = list(
          create = function(map_key_expression = unspecified()) {
            create_substrait_message(
              map_key_expression = clean_value(map_key_expression, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
              .qualified_name = "substrait.Expression.MaskExpression.MapSelect.MapKeyExpression"
            )
          }
        ),
        create = function(key = unspecified(), expression = unspecified(), child = unspecified()) {
          create_substrait_message(
            key = clean_value(key, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.MapSelect.MapKey", repeated = FALSE),
            expression = clean_value(expression, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.MapSelect.MapKeyExpression", repeated = FALSE),
            child = clean_value(child, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.Select", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.MapSelect"
          )
        }
      ),
      Select = list(
        create = function(struct_ = unspecified(), list = unspecified(), map = unspecified()) {
          create_substrait_message(
            struct_ = clean_value(struct_, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.StructSelect", repeated = FALSE),
            list = clean_value(list, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.ListSelect", repeated = FALSE),
            map = clean_value(map, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.MapSelect", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.Select"
          )
        }
      ),
      StructItem = list(
        create = function(field = unspecified(), child = unspecified()) {
          create_substrait_message(
            field = clean_value(field, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            child = clean_value(child, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.Select", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.StructItem"
          )
        }
      ),
      StructSelect = list(
        create = function(struct_items = unspecified()) {
          create_substrait_message(
            struct_items = clean_value(struct_items, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.StructItem", repeated = TRUE),
            .qualified_name = "substrait.Expression.MaskExpression.StructSelect"
          )
        }
      ),
      create = function(select = unspecified(), maintain_singular_struct = unspecified()) {
        create_substrait_message(
          select = clean_value(select, "TYPE_MESSAGE", "substrait.Expression.MaskExpression.StructSelect", repeated = FALSE),
          maintain_singular_struct = clean_value(maintain_singular_struct, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          .qualified_name = "substrait.Expression.MaskExpression"
        )
      }
    ),
    MultiOrList = list(
      Record = list(
        create = function(fields = unspecified()) {
          create_substrait_message(
            fields = clean_value(fields, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
            .qualified_name = "substrait.Expression.MultiOrList.Record"
          )
        }
      ),
      create = function(value = unspecified(), options = unspecified()) {
        create_substrait_message(
          value = clean_value(value, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          options = clean_value(options, "TYPE_MESSAGE", "substrait.Expression.MultiOrList.Record", repeated = TRUE),
          .qualified_name = "substrait.Expression.MultiOrList"
        )
      }
    ),
    ReferenceSegment = list(
      ListElement = list(
        create = function(offset = unspecified(), child = unspecified()) {
          create_substrait_message(
            offset = clean_value(offset, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            child = clean_value(child, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment", repeated = FALSE),
            .qualified_name = "substrait.Expression.ReferenceSegment.ListElement"
          )
        }
      ),
      MapKey = list(
        create = function(map_key = unspecified(), child = unspecified()) {
          create_substrait_message(
            map_key = clean_value(map_key, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = FALSE),
            child = clean_value(child, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment", repeated = FALSE),
            .qualified_name = "substrait.Expression.ReferenceSegment.MapKey"
          )
        }
      ),
      StructField = list(
        create = function(field = unspecified(), child = unspecified()) {
          create_substrait_message(
            field = clean_value(field, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
            child = clean_value(child, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment", repeated = FALSE),
            .qualified_name = "substrait.Expression.ReferenceSegment.StructField"
          )
        }
      ),
      create = function(map_key = unspecified(), struct_field = unspecified(), list_element = unspecified()) {
        create_substrait_message(
          map_key = clean_value(map_key, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment.MapKey", repeated = FALSE),
          struct_field = clean_value(struct_field, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment.StructField", repeated = FALSE),
          list_element = clean_value(list_element, "TYPE_MESSAGE", "substrait.Expression.ReferenceSegment.ListElement", repeated = FALSE),
          .qualified_name = "substrait.Expression.ReferenceSegment"
        )
      }
    ),
    ScalarFunction = list(
      create = function(function_reference = unspecified(), args = unspecified(), output_type = unspecified()) {
        create_substrait_message(
          function_reference = clean_value(function_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          args = clean_value(args, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          output_type = clean_value(output_type, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          .qualified_name = "substrait.Expression.ScalarFunction"
        )
      }
    ),
    SingularOrList = list(
      create = function(value = unspecified(), options = unspecified()) {
        create_substrait_message(
          value = clean_value(value, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          options = clean_value(options, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          .qualified_name = "substrait.Expression.SingularOrList"
        )
      }
    ),
    SwitchExpression = list(
      IfValue = list(
        create = function(if_ = unspecified(), then = unspecified()) {
          create_substrait_message(
            if_ = clean_value(if_, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = FALSE),
            then = clean_value(then, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
            .qualified_name = "substrait.Expression.SwitchExpression.IfValue"
          )
        }
      ),
      create = function(ifs = unspecified(), else_ = unspecified()) {
        create_substrait_message(
          ifs = clean_value(ifs, "TYPE_MESSAGE", "substrait.Expression.SwitchExpression.IfValue", repeated = TRUE),
          else_ = clean_value(else_, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.Expression.SwitchExpression"
        )
      }
    ),
    WindowFunction = list(
      Bound = list(
        CurrentRow = list(
          create = function() create_substrait_message(.qualified_name = "substrait.Expression.WindowFunction.Bound.CurrentRow")
        ),
        Following = list(
          create = function(offset = unspecified()) {
            create_substrait_message(
              offset = clean_value(offset, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
              .qualified_name = "substrait.Expression.WindowFunction.Bound.Following"
            )
          }
        ),
        Preceding = list(
          create = function(offset = unspecified()) {
            create_substrait_message(
              offset = clean_value(offset, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
              .qualified_name = "substrait.Expression.WindowFunction.Bound.Preceding"
            )
          }
        ),
        Unbounded = list(
          create = function() create_substrait_message(.qualified_name = "substrait.Expression.WindowFunction.Bound.Unbounded")
        ),
        create = function(preceding = unspecified(), following = unspecified(), current_row = unspecified(), unbounded = unspecified()) {
          create_substrait_message(
            preceding = clean_value(preceding, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound.Preceding", repeated = FALSE),
            following = clean_value(following, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound.Following", repeated = FALSE),
            current_row = clean_value(current_row, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound.CurrentRow", repeated = FALSE),
            unbounded = clean_value(unbounded, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound.Unbounded", repeated = FALSE),
            .qualified_name = "substrait.Expression.WindowFunction.Bound"
          )
        }
      ),
      create = function(function_reference = unspecified(), partitions = unspecified(), sorts = unspecified(), upper_bound = unspecified(), lower_bound = unspecified(), phase = unspecified(), output_type = unspecified(), args = unspecified()) {
        create_substrait_message(
          function_reference = clean_value(function_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          partitions = clean_value(partitions, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          sorts = clean_value(sorts, "TYPE_MESSAGE", "substrait.SortField", repeated = TRUE),
          upper_bound = clean_value(upper_bound, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound", repeated = FALSE),
          lower_bound = clean_value(lower_bound, "TYPE_MESSAGE", "substrait.Expression.WindowFunction.Bound", repeated = FALSE),
          phase = clean_value(phase, "TYPE_ENUM", "substrait.AggregationPhase", repeated = FALSE),
          output_type = clean_value(output_type, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          args = clean_value(args, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          .qualified_name = "substrait.Expression.WindowFunction"
        )
      }
    ),
    create = function(literal = unspecified(), selection = unspecified(), scalar_function = unspecified(), window_function = unspecified(), if_then = unspecified(), switch_expression = unspecified(), singular_or_list = unspecified(), multi_or_list = unspecified(), enum_ = unspecified(), cast = unspecified()) {
      create_substrait_message(
        literal = clean_value(literal, "TYPE_MESSAGE", "substrait.Expression.Literal", repeated = FALSE),
        selection = clean_value(selection, "TYPE_MESSAGE", "substrait.Expression.FieldReference", repeated = FALSE),
        scalar_function = clean_value(scalar_function, "TYPE_MESSAGE", "substrait.Expression.ScalarFunction", repeated = FALSE),
        window_function = clean_value(window_function, "TYPE_MESSAGE", "substrait.Expression.WindowFunction", repeated = FALSE),
        if_then = clean_value(if_then, "TYPE_MESSAGE", "substrait.Expression.IfThen", repeated = FALSE),
        switch_expression = clean_value(switch_expression, "TYPE_MESSAGE", "substrait.Expression.SwitchExpression", repeated = FALSE),
        singular_or_list = clean_value(singular_or_list, "TYPE_MESSAGE", "substrait.Expression.SingularOrList", repeated = FALSE),
        multi_or_list = clean_value(multi_or_list, "TYPE_MESSAGE", "substrait.Expression.MultiOrList", repeated = FALSE),
        enum_ = clean_value(enum_, "TYPE_MESSAGE", "substrait.Expression.Enum", repeated = FALSE),
        cast = clean_value(cast, "TYPE_MESSAGE", "substrait.Expression.Cast", repeated = FALSE),
        .qualified_name = "substrait.Expression"
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
    create = function(expr = unspecified(), direction = unspecified(), comparison_function_reference = unspecified()) {
      create_substrait_message(
        expr = clean_value(expr, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        direction = clean_value(direction, "TYPE_ENUM", "substrait.SortField.SortDirection", repeated = FALSE),
        comparison_function_reference = clean_value(comparison_function_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        .qualified_name = "substrait.SortField"
      )
    }
  ),
  FunctionSignature = list(
    Aggregate = list(
      create = function(arguments = unspecified(), name = unspecified(), description = unspecified(), deterministic = unspecified(), session_dependent = unspecified(), output_type = unspecified(), variadic = unspecified(), normal = unspecified(), ordered = unspecified(), max_set = unspecified(), intermediate_type = unspecified(), implementations = unspecified()) {
        create_substrait_message(
          arguments = clean_value(arguments, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument", repeated = TRUE),
          name = clean_value(name, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          description = clean_value(description, "TYPE_MESSAGE", "substrait.FunctionSignature.Description", repeated = FALSE),
          deterministic = clean_value(deterministic, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          session_dependent = clean_value(session_dependent, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          output_type = clean_value(output_type, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          variadic = clean_value(variadic, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgVariadic", repeated = FALSE),
          normal = clean_value(normal, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgNormal", repeated = FALSE),
          ordered = clean_value(ordered, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          max_set = clean_value(max_set, "TYPE_UINT64", "TYPE_UINT64", repeated = FALSE),
          intermediate_type = clean_value(intermediate_type, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          implementations = clean_value(implementations, "TYPE_MESSAGE", "substrait.FunctionSignature.Implementation", repeated = TRUE),
          .qualified_name = "substrait.FunctionSignature.Aggregate"
        )
      }
    ),
    Argument = list(
      EnumArgument = list(
        create = function(options = unspecified(), optional = unspecified()) {
          create_substrait_message(
            options = clean_value(options, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
            optional = clean_value(optional, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
            .qualified_name = "substrait.FunctionSignature.Argument.EnumArgument"
          )
        }
      ),
      TypeArgument = list(
        create = function(type = unspecified()) {
          create_substrait_message(
            type = clean_value(type, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = FALSE),
            .qualified_name = "substrait.FunctionSignature.Argument.TypeArgument"
          )
        }
      ),
      ValueArgument = list(
        create = function(type = unspecified(), constant = unspecified()) {
          create_substrait_message(
            type = clean_value(type, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = FALSE),
            constant = clean_value(constant, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
            .qualified_name = "substrait.FunctionSignature.Argument.ValueArgument"
          )
        }
      ),
      create = function(name = unspecified(), value = unspecified(), type = unspecified(), enum_ = unspecified()) {
        create_substrait_message(
          name = clean_value(name, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          value = clean_value(value, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument.ValueArgument", repeated = FALSE),
          type = clean_value(type, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument.TypeArgument", repeated = FALSE),
          enum_ = clean_value(enum_, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument.EnumArgument", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.Argument"
        )
      }
    ),
    Description = list(
      create = function(language = unspecified(), body = unspecified()) {
        create_substrait_message(
          language = clean_value(language, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          body = clean_value(body, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.Description"
        )
      }
    ),
    FinalArgNormal = list(
      create = function() create_substrait_message(.qualified_name = "substrait.FunctionSignature.FinalArgNormal")
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
      create = function(min_args = unspecified(), max_args = unspecified(), consistency = unspecified()) {
        create_substrait_message(
          min_args = clean_value(min_args, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          max_args = clean_value(max_args, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          consistency = clean_value(consistency, "TYPE_ENUM", "substrait.FunctionSignature.FinalArgVariadic.ParameterConsistency", repeated = FALSE),
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
      create = function(type = unspecified(), uri = unspecified()) {
        create_substrait_message(
          type = clean_value(type, "TYPE_ENUM", "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          uri = clean_value(uri, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.Implementation"
        )
      }
    ),
    Scalar = list(
      create = function(arguments = unspecified(), name = unspecified(), description = unspecified(), deterministic = unspecified(), session_dependent = unspecified(), output_type = unspecified(), variadic = unspecified(), normal = unspecified(), implementations = unspecified()) {
        create_substrait_message(
          arguments = clean_value(arguments, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument", repeated = TRUE),
          name = clean_value(name, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          description = clean_value(description, "TYPE_MESSAGE", "substrait.FunctionSignature.Description", repeated = FALSE),
          deterministic = clean_value(deterministic, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          session_dependent = clean_value(session_dependent, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          output_type = clean_value(output_type, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          variadic = clean_value(variadic, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgVariadic", repeated = FALSE),
          normal = clean_value(normal, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgNormal", repeated = FALSE),
          implementations = clean_value(implementations, "TYPE_MESSAGE", "substrait.FunctionSignature.Implementation", repeated = TRUE),
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
      create = function(arguments = unspecified(), name = unspecified(), description = unspecified(), deterministic = unspecified(), session_dependent = unspecified(), intermediate_type = unspecified(), output_type = unspecified(), variadic = unspecified(), normal = unspecified(), ordered = unspecified(), max_set = unspecified(), window_type = unspecified(), implementations = unspecified()) {
        create_substrait_message(
          arguments = clean_value(arguments, "TYPE_MESSAGE", "substrait.FunctionSignature.Argument", repeated = TRUE),
          name = clean_value(name, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          description = clean_value(description, "TYPE_MESSAGE", "substrait.FunctionSignature.Description", repeated = FALSE),
          deterministic = clean_value(deterministic, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          session_dependent = clean_value(session_dependent, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          intermediate_type = clean_value(intermediate_type, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          output_type = clean_value(output_type, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          variadic = clean_value(variadic, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgVariadic", repeated = FALSE),
          normal = clean_value(normal, "TYPE_MESSAGE", "substrait.FunctionSignature.FinalArgNormal", repeated = FALSE),
          ordered = clean_value(ordered, "TYPE_BOOL", "TYPE_BOOL", repeated = FALSE),
          max_set = clean_value(max_set, "TYPE_UINT64", "TYPE_UINT64", repeated = FALSE),
          window_type = clean_value(window_type, "TYPE_ENUM", "substrait.FunctionSignature.Window.WindowType", repeated = FALSE),
          implementations = clean_value(implementations, "TYPE_MESSAGE", "substrait.FunctionSignature.Implementation", repeated = TRUE),
          .qualified_name = "substrait.FunctionSignature.Window"
        )
      }
    ),
    create = function() create_substrait_message(.qualified_name = "substrait.FunctionSignature")
  ),
  ParameterizedType = list(
    IntegerOption = list(
      create = function(literal = unspecified(), parameter = unspecified()) {
        create_substrait_message(
          literal = clean_value(literal, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          parameter = clean_value(parameter, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerParameter", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.IntegerOption"
        )
      }
    ),
    IntegerParameter = list(
      create = function(name = unspecified(), range_start_inclusive = unspecified(), range_end_exclusive = unspecified()) {
        create_substrait_message(
          name = clean_value(name, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          range_start_inclusive = clean_value(range_start_inclusive, "TYPE_MESSAGE", "substrait.ParameterizedType.NullableInteger", repeated = FALSE),
          range_end_exclusive = clean_value(range_end_exclusive, "TYPE_MESSAGE", "substrait.ParameterizedType.NullableInteger", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.IntegerParameter"
        )
      }
    ),
    NullableInteger = list(
      create = function(value = unspecified()) {
        create_substrait_message(
          value = clean_value(value, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.NullableInteger"
        )
      }
    ),
    ParameterizedDecimal = list(
      create = function(scale = unspecified(), precision = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          scale = clean_value(scale, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          precision = clean_value(precision, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedDecimal"
        )
      }
    ),
    ParameterizedFixedBinary = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedFixedBinary"
        )
      }
    ),
    ParameterizedFixedChar = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedFixedChar"
        )
      }
    ),
    ParameterizedList = list(
      create = function(type = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type = clean_value(type, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedList"
        )
      }
    ),
    ParameterizedMap = list(
      create = function(key = unspecified(), value = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          key = clean_value(key, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = FALSE),
          value = clean_value(value, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedMap"
        )
      }
    ),
    ParameterizedNamedStruct = list(
      create = function(names = unspecified(), struct_ = unspecified()) {
        create_substrait_message(
          names = clean_value(names, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          struct_ = clean_value(struct_, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedStruct", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedNamedStruct"
        )
      }
    ),
    ParameterizedStruct = list(
      create = function(types = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          types = clean_value(types, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = TRUE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedStruct"
        )
      }
    ),
    ParameterizedVarChar = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_MESSAGE", "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedVarChar"
        )
      }
    ),
    TypeParameter = list(
      create = function(name = unspecified(), bounds = unspecified()) {
        create_substrait_message(
          name = clean_value(name, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
          bounds = clean_value(bounds, "TYPE_MESSAGE", "substrait.ParameterizedType", repeated = TRUE),
          .qualified_name = "substrait.ParameterizedType.TypeParameter"
        )
      }
    ),
    create = function(bool_ = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year = unspecified(), interval_day = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), fixed_char = unspecified(), varchar = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), list = unspecified(), map = unspecified(), user_defined_pointer = unspecified(), type_parameter = unspecified()) {
      create_substrait_message(
        bool_ = clean_value(bool_, "TYPE_MESSAGE", "substrait.Type.Boolean", repeated = FALSE),
        i8 = clean_value(i8, "TYPE_MESSAGE", "substrait.Type.I8", repeated = FALSE),
        i16 = clean_value(i16, "TYPE_MESSAGE", "substrait.Type.I16", repeated = FALSE),
        i32 = clean_value(i32, "TYPE_MESSAGE", "substrait.Type.I32", repeated = FALSE),
        i64 = clean_value(i64, "TYPE_MESSAGE", "substrait.Type.I64", repeated = FALSE),
        fp32 = clean_value(fp32, "TYPE_MESSAGE", "substrait.Type.FP32", repeated = FALSE),
        fp64 = clean_value(fp64, "TYPE_MESSAGE", "substrait.Type.FP64", repeated = FALSE),
        string = clean_value(string, "TYPE_MESSAGE", "substrait.Type.String", repeated = FALSE),
        binary = clean_value(binary, "TYPE_MESSAGE", "substrait.Type.Binary", repeated = FALSE),
        timestamp = clean_value(timestamp, "TYPE_MESSAGE", "substrait.Type.Timestamp", repeated = FALSE),
        date = clean_value(date, "TYPE_MESSAGE", "substrait.Type.Date", repeated = FALSE),
        time = clean_value(time, "TYPE_MESSAGE", "substrait.Type.Time", repeated = FALSE),
        interval_year = clean_value(interval_year, "TYPE_MESSAGE", "substrait.Type.IntervalYear", repeated = FALSE),
        interval_day = clean_value(interval_day, "TYPE_MESSAGE", "substrait.Type.IntervalDay", repeated = FALSE),
        timestamp_tz = clean_value(timestamp_tz, "TYPE_MESSAGE", "substrait.Type.TimestampTZ", repeated = FALSE),
        uuid = clean_value(uuid, "TYPE_MESSAGE", "substrait.Type.UUID", repeated = FALSE),
        fixed_char = clean_value(fixed_char, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedFixedChar", repeated = FALSE),
        varchar = clean_value(varchar, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedVarChar", repeated = FALSE),
        fixed_binary = clean_value(fixed_binary, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedFixedBinary", repeated = FALSE),
        decimal = clean_value(decimal, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedDecimal", repeated = FALSE),
        struct_ = clean_value(struct_, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedStruct", repeated = FALSE),
        list = clean_value(list, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedList", repeated = FALSE),
        map = clean_value(map, "TYPE_MESSAGE", "substrait.ParameterizedType.ParameterizedMap", repeated = FALSE),
        user_defined_pointer = clean_value(user_defined_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        type_parameter = clean_value(type_parameter, "TYPE_MESSAGE", "substrait.ParameterizedType.TypeParameter", repeated = FALSE),
        .qualified_name = "substrait.ParameterizedType"
      )
    }
  ),
  Plan = list(
    create = function(extension_uris = unspecified(), extensions = unspecified(), relations = unspecified(), advanced_extensions = unspecified(), expected_type_urls = unspecified()) {
      create_substrait_message(
        extension_uris = clean_value(extension_uris, "TYPE_MESSAGE", "substrait.SimpleExtensionURI", repeated = TRUE),
        extensions = clean_value(extensions, "TYPE_MESSAGE", "substrait.SimpleExtensionDeclaration", repeated = TRUE),
        relations = clean_value(relations, "TYPE_MESSAGE", "substrait.PlanRel", repeated = TRUE),
        advanced_extensions = clean_value(advanced_extensions, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
        expected_type_urls = clean_value(expected_type_urls, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        .qualified_name = "substrait.Plan"
      )
    }
  ),
  PlanRel = list(
    create = function(rel = unspecified(), root = unspecified()) {
      create_substrait_message(
        rel = clean_value(rel, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        root = clean_value(root, "TYPE_MESSAGE", "substrait.RelRoot", repeated = FALSE),
        .qualified_name = "substrait.PlanRel"
      )
    }
  ),
  AggregateRel = list(
    Grouping = list(
      create = function(grouping_expressions = unspecified()) {
        create_substrait_message(
          grouping_expressions = clean_value(grouping_expressions, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
          .qualified_name = "substrait.AggregateRel.Grouping"
        )
      }
    ),
    Measure = list(
      create = function(measure = unspecified(), filter = unspecified()) {
        create_substrait_message(
          measure = clean_value(measure, "TYPE_MESSAGE", "substrait.AggregateFunction", repeated = FALSE),
          filter = clean_value(filter, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.AggregateRel.Measure"
        )
      }
    ),
    create = function(common = unspecified(), input = unspecified(), groupings = unspecified(), measures = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        groupings = clean_value(groupings, "TYPE_MESSAGE", "substrait.AggregateRel.Grouping", repeated = TRUE),
        measures = clean_value(measures, "TYPE_MESSAGE", "substrait.AggregateRel.Measure", repeated = TRUE),
        advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.AggregateRel"
      )
    }
  ),
  ExtensionLeafRel = list(
    create = function(common = unspecified(), detail = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        detail = clean_value(detail, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
        .qualified_name = "substrait.ExtensionLeafRel"
      )
    }
  ),
  ExtensionMultiRel = list(
    create = function(common = unspecified(), inputs = unspecified(), detail = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        inputs = clean_value(inputs, "TYPE_MESSAGE", "substrait.Rel", repeated = TRUE),
        detail = clean_value(detail, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
        .qualified_name = "substrait.ExtensionMultiRel"
      )
    }
  ),
  ExtensionSingleRel = list(
    create = function(common = unspecified(), input = unspecified(), detail = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        detail = clean_value(detail, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
        .qualified_name = "substrait.ExtensionSingleRel"
      )
    }
  ),
  FetchRel = list(
    create = function(common = unspecified(), input = unspecified(), offset = unspecified(), count = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        offset = clean_value(offset, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
        count = clean_value(count, "TYPE_INT64", "TYPE_INT64", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.FetchRel"
      )
    }
  ),
  FilterRel = list(
    create = function(common = unspecified(), input = unspecified(), condition = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        condition = clean_value(condition, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.FilterRel"
      )
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
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.JoinRel.JoinType"
        )
      }
    ),
    create = function(common = unspecified(), left = unspecified(), right = unspecified(), expression = unspecified(), post_join_filter = unspecified(), type = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        left = clean_value(left, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        right = clean_value(right, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        expression = clean_value(expression, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        post_join_filter = clean_value(post_join_filter, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        type = clean_value(type, "TYPE_ENUM", "substrait.JoinRel.JoinType", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.JoinRel"
      )
    }
  ),
  ProjectRel = list(
    create = function(common = unspecified(), input = unspecified(), expressions = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        expressions = clean_value(expressions, "TYPE_MESSAGE", "substrait.Expression", repeated = TRUE),
        advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.ProjectRel"
      )
    }
  ),
  ReadRel = list(
    ExtensionTable = list(
      create = function(detail = unspecified()) {
        create_substrait_message(
          detail = clean_value(detail, "TYPE_MESSAGE", "substrait.Any", repeated = FALSE),
          .qualified_name = "substrait.ReadRel.ExtensionTable"
        )
      }
    ),
    LocalFiles = list(
      FileOrFiles = list(
        FileFormat = list(
          FILE_FORMAT_UNSPECIFIED = structure(0L, class = c("substrait_ReadRel_LocalFiles_FileOrFiles_FileFormat", "substrait_proto_enum", "substrait_proto")),
          FILE_FORMAT_PARQUET = structure(1L, class = c("substrait_ReadRel_LocalFiles_FileOrFiles_FileFormat", "substrait_proto_enum", "substrait_proto")),
          create = function(value) {
            create_substrait_enum(
              value,
              .qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles.FileFormat"
            )
          }
        ),
        create = function(uri_path = unspecified(), uri_path_glob = unspecified(), uri_file = unspecified(), uri_folder = unspecified(), format = unspecified(), partition_index = unspecified(), start = unspecified(), length = unspecified()) {
          create_substrait_message(
            uri_path = clean_value(uri_path, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            uri_path_glob = clean_value(uri_path_glob, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            uri_file = clean_value(uri_file, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            uri_folder = clean_value(uri_folder, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            format = clean_value(format, "TYPE_ENUM", "substrait.ReadRel.LocalFiles.FileOrFiles.FileFormat", repeated = FALSE),
            partition_index = clean_value(partition_index, "TYPE_UINT64", "TYPE_UINT64", repeated = FALSE),
            start = clean_value(start, "TYPE_UINT64", "TYPE_UINT64", repeated = FALSE),
            length = clean_value(length, "TYPE_UINT64", "TYPE_UINT64", repeated = FALSE),
            .qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles"
          )
        }
      ),
      create = function(items = unspecified(), advanced_extension = unspecified()) {
        create_substrait_message(
          items = clean_value(items, "TYPE_MESSAGE", "substrait.ReadRel.LocalFiles.FileOrFiles", repeated = TRUE),
          advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
          .qualified_name = "substrait.ReadRel.LocalFiles"
        )
      }
    ),
    NamedTable = list(
      create = function(names = unspecified(), advanced_extension = unspecified()) {
        create_substrait_message(
          names = clean_value(names, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
          .qualified_name = "substrait.ReadRel.NamedTable"
        )
      }
    ),
    VirtualTable = list(
      create = function(values = unspecified()) {
        create_substrait_message(
          values = clean_value(values, "TYPE_MESSAGE", "substrait.Expression.Literal.Struct", repeated = TRUE),
          .qualified_name = "substrait.ReadRel.VirtualTable"
        )
      }
    ),
    create = function(common = unspecified(), base_schema = unspecified(), filter = unspecified(), projection = unspecified(), advanced_extension = unspecified(), virtual_table = unspecified(), local_files = unspecified(), named_table = unspecified(), extension_table = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        base_schema = clean_value(base_schema, "TYPE_MESSAGE", "substrait.NamedStruct", repeated = FALSE),
        filter = clean_value(filter, "TYPE_MESSAGE", "substrait.Expression", repeated = FALSE),
        projection = clean_value(projection, "TYPE_MESSAGE", "substrait.Expression.MaskExpression", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
        virtual_table = clean_value(virtual_table, "TYPE_MESSAGE", "substrait.ReadRel.VirtualTable", repeated = FALSE),
        local_files = clean_value(local_files, "TYPE_MESSAGE", "substrait.ReadRel.LocalFiles", repeated = FALSE),
        named_table = clean_value(named_table, "TYPE_MESSAGE", "substrait.ReadRel.NamedTable", repeated = FALSE),
        extension_table = clean_value(extension_table, "TYPE_MESSAGE", "substrait.ReadRel.ExtensionTable", repeated = FALSE),
        .qualified_name = "substrait.ReadRel"
      )
    }
  ),
  Rel = list(
    create = function(read = unspecified(), filter = unspecified(), fetch = unspecified(), aggregate = unspecified(), sort = unspecified(), join = unspecified(), project = unspecified(), set = unspecified(), extension_single = unspecified(), extension_multi = unspecified(), extension_leaf = unspecified()) {
      create_substrait_message(
        read = clean_value(read, "TYPE_MESSAGE", "substrait.ReadRel", repeated = FALSE),
        filter = clean_value(filter, "TYPE_MESSAGE", "substrait.FilterRel", repeated = FALSE),
        fetch = clean_value(fetch, "TYPE_MESSAGE", "substrait.FetchRel", repeated = FALSE),
        aggregate = clean_value(aggregate, "TYPE_MESSAGE", "substrait.AggregateRel", repeated = FALSE),
        sort = clean_value(sort, "TYPE_MESSAGE", "substrait.SortRel", repeated = FALSE),
        join = clean_value(join, "TYPE_MESSAGE", "substrait.JoinRel", repeated = FALSE),
        project = clean_value(project, "TYPE_MESSAGE", "substrait.ProjectRel", repeated = FALSE),
        set = clean_value(set, "TYPE_MESSAGE", "substrait.SetRel", repeated = FALSE),
        extension_single = clean_value(extension_single, "TYPE_MESSAGE", "substrait.ExtensionSingleRel", repeated = FALSE),
        extension_multi = clean_value(extension_multi, "TYPE_MESSAGE", "substrait.ExtensionMultiRel", repeated = FALSE),
        extension_leaf = clean_value(extension_leaf, "TYPE_MESSAGE", "substrait.ExtensionLeafRel", repeated = FALSE),
        .qualified_name = "substrait.Rel"
      )
    }
  ),
  RelCommon = list(
    Direct = list(
      create = function() create_substrait_message(.qualified_name = "substrait.RelCommon.Direct")
    ),
    Emit = list(
      create = function(output_mapping = unspecified()) {
        create_substrait_message(
          output_mapping = clean_value(output_mapping, "TYPE_INT32", "TYPE_INT32", repeated = TRUE),
          .qualified_name = "substrait.RelCommon.Emit"
        )
      }
    ),
    Hint = list(
      RuntimeConstraint = list(
        create = function(advanced_extension = unspecified()) {
          create_substrait_message(
            advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
            .qualified_name = "substrait.RelCommon.Hint.RuntimeConstraint"
          )
        }
      ),
      Stats = list(
        create = function(row_count = unspecified(), record_size = unspecified(), advanced_extension = unspecified()) {
          create_substrait_message(
            row_count = clean_value(row_count, "TYPE_DOUBLE", "TYPE_DOUBLE", repeated = FALSE),
            record_size = clean_value(record_size, "TYPE_DOUBLE", "TYPE_DOUBLE", repeated = FALSE),
            advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
            .qualified_name = "substrait.RelCommon.Hint.Stats"
          )
        }
      ),
      create = function(stats = unspecified(), constraint = unspecified(), advanced_extension = unspecified()) {
        create_substrait_message(
          stats = clean_value(stats, "TYPE_MESSAGE", "substrait.RelCommon.Hint.Stats", repeated = FALSE),
          constraint = clean_value(constraint, "TYPE_MESSAGE", "substrait.RelCommon.Hint.RuntimeConstraint", repeated = FALSE),
          advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
          .qualified_name = "substrait.RelCommon.Hint"
        )
      }
    ),
    create = function(direct = unspecified(), emit = unspecified(), hint = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        direct = clean_value(direct, "TYPE_MESSAGE", "substrait.RelCommon.Direct", repeated = FALSE),
        emit = clean_value(emit, "TYPE_MESSAGE", "substrait.RelCommon.Emit", repeated = FALSE),
        hint = clean_value(hint, "TYPE_MESSAGE", "substrait.RelCommon.Hint", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.RelCommon"
      )
    }
  ),
  RelRoot = list(
    create = function(input = unspecified(), names = unspecified()) {
      create_substrait_message(
        input = clean_value(input, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        names = clean_value(names, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
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
    create = function(common = unspecified(), inputs = unspecified(), op = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        inputs = clean_value(inputs, "TYPE_MESSAGE", "substrait.Rel", repeated = TRUE),
        op = clean_value(op, "TYPE_ENUM", "substrait.SetRel.SetOp", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.SetRel"
      )
    }
  ),
  SortRel = list(
    create = function(common = unspecified(), input = unspecified(), sorts = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "TYPE_MESSAGE", "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "TYPE_MESSAGE", "substrait.Rel", repeated = FALSE),
        sorts = clean_value(sorts, "TYPE_MESSAGE", "substrait.SortField", repeated = TRUE),
        advanced_extension = clean_value(advanced_extension, "TYPE_MESSAGE", "substrait.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.SortRel"
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
      create = function(op_type = unspecified(), arg1 = unspecified(), arg2 = unspecified()) {
        create_substrait_message(
          op_type = clean_value(op_type, "TYPE_ENUM", "substrait.DerivationExpression.BinaryOp.BinaryOpType", repeated = FALSE),
          arg1 = clean_value(arg1, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          arg2 = clean_value(arg2, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.BinaryOp"
        )
      }
    ),
    ExpressionDecimal = list(
      create = function(scale = unspecified(), precision = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          scale = clean_value(scale, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          precision = clean_value(precision, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionDecimal"
        )
      }
    ),
    ExpressionFixedBinary = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionFixedBinary"
        )
      }
    ),
    ExpressionFixedChar = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionFixedChar"
        )
      }
    ),
    ExpressionList = list(
      create = function(type = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type = clean_value(type, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionList"
        )
      }
    ),
    ExpressionMap = list(
      create = function(key = unspecified(), value = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          key = clean_value(key, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          value = clean_value(value, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionMap"
        )
      }
    ),
    ExpressionNamedStruct = list(
      create = function(names = unspecified(), struct_ = unspecified()) {
        create_substrait_message(
          names = clean_value(names, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
          struct_ = clean_value(struct_, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionStruct", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionNamedStruct"
        )
      }
    ),
    ExpressionStruct = list(
      create = function(types = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          types = clean_value(types, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = TRUE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionStruct"
        )
      }
    ),
    ExpressionVarChar = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionVarChar"
        )
      }
    ),
    IfElse = list(
      create = function(if_condition = unspecified(), if_return = unspecified(), else_return = unspecified()) {
        create_substrait_message(
          if_condition = clean_value(if_condition, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          if_return = clean_value(if_return, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          else_return = clean_value(else_return, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.IfElse"
        )
      }
    ),
    ReturnProgram = list(
      Assignment = list(
        create = function(name = unspecified(), expression = unspecified()) {
          create_substrait_message(
            name = clean_value(name, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
            expression = clean_value(expression, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
            .qualified_name = "substrait.DerivationExpression.ReturnProgram.Assignment"
          )
        }
      ),
      create = function(assignments = unspecified(), final_expression = unspecified()) {
        create_substrait_message(
          assignments = clean_value(assignments, "TYPE_MESSAGE", "substrait.DerivationExpression.ReturnProgram.Assignment", repeated = TRUE),
          final_expression = clean_value(final_expression, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
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
      create = function(op_type = unspecified(), arg = unspecified()) {
        create_substrait_message(
          op_type = clean_value(op_type, "TYPE_ENUM", "substrait.DerivationExpression.UnaryOp.UnaryOpType", repeated = FALSE),
          arg = clean_value(arg, "TYPE_MESSAGE", "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.UnaryOp"
        )
      }
    ),
    create = function(bool_ = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year = unspecified(), interval_day = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), fixed_char = unspecified(), varchar = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), list = unspecified(), map = unspecified(), user_defined_pointer = unspecified(), type_parameter_name = unspecified(), integer_parameter_name = unspecified(), integer_literal = unspecified(), unary_op = unspecified(), binary_op = unspecified(), if_else = unspecified(), return_program = unspecified()) {
      create_substrait_message(
        bool_ = clean_value(bool_, "TYPE_MESSAGE", "substrait.Type.Boolean", repeated = FALSE),
        i8 = clean_value(i8, "TYPE_MESSAGE", "substrait.Type.I8", repeated = FALSE),
        i16 = clean_value(i16, "TYPE_MESSAGE", "substrait.Type.I16", repeated = FALSE),
        i32 = clean_value(i32, "TYPE_MESSAGE", "substrait.Type.I32", repeated = FALSE),
        i64 = clean_value(i64, "TYPE_MESSAGE", "substrait.Type.I64", repeated = FALSE),
        fp32 = clean_value(fp32, "TYPE_MESSAGE", "substrait.Type.FP32", repeated = FALSE),
        fp64 = clean_value(fp64, "TYPE_MESSAGE", "substrait.Type.FP64", repeated = FALSE),
        string = clean_value(string, "TYPE_MESSAGE", "substrait.Type.String", repeated = FALSE),
        binary = clean_value(binary, "TYPE_MESSAGE", "substrait.Type.Binary", repeated = FALSE),
        timestamp = clean_value(timestamp, "TYPE_MESSAGE", "substrait.Type.Timestamp", repeated = FALSE),
        date = clean_value(date, "TYPE_MESSAGE", "substrait.Type.Date", repeated = FALSE),
        time = clean_value(time, "TYPE_MESSAGE", "substrait.Type.Time", repeated = FALSE),
        interval_year = clean_value(interval_year, "TYPE_MESSAGE", "substrait.Type.IntervalYear", repeated = FALSE),
        interval_day = clean_value(interval_day, "TYPE_MESSAGE", "substrait.Type.IntervalDay", repeated = FALSE),
        timestamp_tz = clean_value(timestamp_tz, "TYPE_MESSAGE", "substrait.Type.TimestampTZ", repeated = FALSE),
        uuid = clean_value(uuid, "TYPE_MESSAGE", "substrait.Type.UUID", repeated = FALSE),
        fixed_char = clean_value(fixed_char, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionFixedChar", repeated = FALSE),
        varchar = clean_value(varchar, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionVarChar", repeated = FALSE),
        fixed_binary = clean_value(fixed_binary, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionFixedBinary", repeated = FALSE),
        decimal = clean_value(decimal, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionDecimal", repeated = FALSE),
        struct_ = clean_value(struct_, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionStruct", repeated = FALSE),
        list = clean_value(list, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionList", repeated = FALSE),
        map = clean_value(map, "TYPE_MESSAGE", "substrait.DerivationExpression.ExpressionMap", repeated = FALSE),
        user_defined_pointer = clean_value(user_defined_pointer, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        type_parameter_name = clean_value(type_parameter_name, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
        integer_parameter_name = clean_value(integer_parameter_name, "TYPE_STRING", "TYPE_STRING", repeated = FALSE),
        integer_literal = clean_value(integer_literal, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
        unary_op = clean_value(unary_op, "TYPE_MESSAGE", "substrait.DerivationExpression.UnaryOp", repeated = FALSE),
        binary_op = clean_value(binary_op, "TYPE_MESSAGE", "substrait.DerivationExpression.BinaryOp", repeated = FALSE),
        if_else = clean_value(if_else, "TYPE_MESSAGE", "substrait.DerivationExpression.IfElse", repeated = FALSE),
        return_program = clean_value(return_program, "TYPE_MESSAGE", "substrait.DerivationExpression.ReturnProgram", repeated = FALSE),
        .qualified_name = "substrait.DerivationExpression"
      )
    }
  ),
  NamedStruct = list(
    create = function(names = unspecified(), struct_ = unspecified()) {
      create_substrait_message(
        names = clean_value(names, "TYPE_STRING", "TYPE_STRING", repeated = TRUE),
        struct_ = clean_value(struct_, "TYPE_MESSAGE", "substrait.Type.Struct", repeated = FALSE),
        .qualified_name = "substrait.NamedStruct"
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
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Binary"
        )
      }
    ),
    Boolean = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Boolean"
        )
      }
    ),
    Date = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Date"
        )
      }
    ),
    Decimal = list(
      create = function(scale = unspecified(), precision = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          scale = clean_value(scale, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          precision = clean_value(precision, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Decimal"
        )
      }
    ),
    FP32 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FP32"
        )
      }
    ),
    FP64 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FP64"
        )
      }
    ),
    FixedBinary = list(
      create = function(length = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FixedBinary"
        )
      }
    ),
    FixedChar = list(
      create = function(length = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FixedChar"
        )
      }
    ),
    I16 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I16"
        )
      }
    ),
    I32 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I32"
        )
      }
    ),
    I64 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I64"
        )
      }
    ),
    I8 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I8"
        )
      }
    ),
    IntervalDay = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.IntervalDay"
        )
      }
    ),
    IntervalYear = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.IntervalYear"
        )
      }
    ),
    List = list(
      create = function(type = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type = clean_value(type, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.List"
        )
      }
    ),
    Map = list(
      create = function(key = unspecified(), value = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          key = clean_value(key, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          value = clean_value(value, "TYPE_MESSAGE", "substrait.Type", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Map"
        )
      }
    ),
    String = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.String"
        )
      }
    ),
    Struct = list(
      create = function(types = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          types = clean_value(types, "TYPE_MESSAGE", "substrait.Type", repeated = TRUE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Struct"
        )
      }
    ),
    Time = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Time"
        )
      }
    ),
    Timestamp = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Timestamp"
        )
      }
    ),
    TimestampTZ = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.TimestampTZ"
        )
      }
    ),
    UUID = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.UUID"
        )
      }
    ),
    VarChar = list(
      create = function(length = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_INT32", "TYPE_INT32", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "TYPE_ENUM", "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.VarChar"
        )
      }
    ),
    create = function(bool_ = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year = unspecified(), interval_day = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), fixed_char = unspecified(), varchar = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), list = unspecified(), map = unspecified(), user_defined_type_reference = unspecified()) {
      create_substrait_message(
        bool_ = clean_value(bool_, "TYPE_MESSAGE", "substrait.Type.Boolean", repeated = FALSE),
        i8 = clean_value(i8, "TYPE_MESSAGE", "substrait.Type.I8", repeated = FALSE),
        i16 = clean_value(i16, "TYPE_MESSAGE", "substrait.Type.I16", repeated = FALSE),
        i32 = clean_value(i32, "TYPE_MESSAGE", "substrait.Type.I32", repeated = FALSE),
        i64 = clean_value(i64, "TYPE_MESSAGE", "substrait.Type.I64", repeated = FALSE),
        fp32 = clean_value(fp32, "TYPE_MESSAGE", "substrait.Type.FP32", repeated = FALSE),
        fp64 = clean_value(fp64, "TYPE_MESSAGE", "substrait.Type.FP64", repeated = FALSE),
        string = clean_value(string, "TYPE_MESSAGE", "substrait.Type.String", repeated = FALSE),
        binary = clean_value(binary, "TYPE_MESSAGE", "substrait.Type.Binary", repeated = FALSE),
        timestamp = clean_value(timestamp, "TYPE_MESSAGE", "substrait.Type.Timestamp", repeated = FALSE),
        date = clean_value(date, "TYPE_MESSAGE", "substrait.Type.Date", repeated = FALSE),
        time = clean_value(time, "TYPE_MESSAGE", "substrait.Type.Time", repeated = FALSE),
        interval_year = clean_value(interval_year, "TYPE_MESSAGE", "substrait.Type.IntervalYear", repeated = FALSE),
        interval_day = clean_value(interval_day, "TYPE_MESSAGE", "substrait.Type.IntervalDay", repeated = FALSE),
        timestamp_tz = clean_value(timestamp_tz, "TYPE_MESSAGE", "substrait.Type.TimestampTZ", repeated = FALSE),
        uuid = clean_value(uuid, "TYPE_MESSAGE", "substrait.Type.UUID", repeated = FALSE),
        fixed_char = clean_value(fixed_char, "TYPE_MESSAGE", "substrait.Type.FixedChar", repeated = FALSE),
        varchar = clean_value(varchar, "TYPE_MESSAGE", "substrait.Type.VarChar", repeated = FALSE),
        fixed_binary = clean_value(fixed_binary, "TYPE_MESSAGE", "substrait.Type.FixedBinary", repeated = FALSE),
        decimal = clean_value(decimal, "TYPE_MESSAGE", "substrait.Type.Decimal", repeated = FALSE),
        struct_ = clean_value(struct_, "TYPE_MESSAGE", "substrait.Type.Struct", repeated = FALSE),
        list = clean_value(list, "TYPE_MESSAGE", "substrait.Type.List", repeated = FALSE),
        map = clean_value(map, "TYPE_MESSAGE", "substrait.Type.Map", repeated = FALSE),
        user_defined_type_reference = clean_value(user_defined_type_reference, "TYPE_UINT32", "TYPE_UINT32", repeated = FALSE),
        .qualified_name = "substrait.Type"
      )
    }
  )
)
