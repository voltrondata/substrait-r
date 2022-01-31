
#' Create raw Substrait types
#'
#' @export
substrait <- list(
  AggregationPhase = list(
    AGGREGATION_PHASE_UNSPECIFIED = 0,
    AGGREGATION_PHASE_INITIAL_TO_INTERMEDIATE = 1,
    AGGREGATION_PHASE_INTERMEDIATE_TO_INTERMEDIATE = 2,
    AGGREGATION_PHASE_INITIAL_TO_RESULT = 3,
    AGGREGATION_PHASE_INTERMEDIATE_TO_RESULT = 4,
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
        type_url = clean_value(type_url, "TYPE_STRING", repeated = FALSE),
        value = clean_value(value, "TYPE_BYTES", repeated = FALSE),
        .qualified_name = "substrait.Any"
      )
    }
  ),
  Capabilities = list(
    SimpleExtension = list(
      create = function(uri = unspecified(), function_keys = unspecified(), type_keys = unspecified(), type_variation_keys = unspecified()) {
        create_substrait_message(
          uri = clean_value(uri, "TYPE_STRING", repeated = FALSE),
          function_keys = clean_value(function_keys, "TYPE_STRING", repeated = TRUE),
          type_keys = clean_value(type_keys, "TYPE_STRING", repeated = TRUE),
          type_variation_keys = clean_value(type_variation_keys, "TYPE_STRING", repeated = TRUE),
          .qualified_name = "substrait.Capabilities.SimpleExtension"
        )
      }
    ),
    create = function(substrait_versions = unspecified(), advanced_extension_type_urls = unspecified(), simple_extensions = unspecified()) {
      create_substrait_message(
        substrait_versions = clean_value(substrait_versions, "TYPE_STRING", repeated = TRUE),
        advanced_extension_type_urls = clean_value(advanced_extension_type_urls, "TYPE_STRING", repeated = TRUE),
        simple_extensions = clean_value(simple_extensions, "substrait.Capabilities.SimpleExtension", repeated = TRUE),
        .qualified_name = "substrait.Capabilities"
      )
    }
  ),
  AggregateFunction = list(
    create = function(function_reference = unspecified(), args = unspecified(), sorts = unspecified(), phase = unspecified(), output_type = unspecified()) {
      create_substrait_message(
        function_reference = clean_value(function_reference, "TYPE_UINT32", repeated = FALSE),
        args = clean_value(args, "substrait.Expression", repeated = TRUE),
        sorts = clean_value(sorts, "substrait.SortField", repeated = TRUE),
        phase = clean_value(phase, "substrait.AggregationPhase", repeated = FALSE),
        output_type = clean_value(output_type, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
        .qualified_name = "substrait.AggregateFunction"
      )
    }
  ),
  Expression = list(
    Cast = list(
      create = function(type = unspecified(), input = unspecified()) {
        create_substrait_message(
          type = clean_value(type, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          input = clean_value(input, "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.Expression.Cast"
        )
      }
    ),
    EmbeddedFunction = list(
      PythonPickleFunction = list(
        create = function(function_ = unspecified(), prerequisite = unspecified()) {
          create_substrait_message(
            function_ = clean_value(function_, "TYPE_BYTES", repeated = FALSE),
            prerequisite = clean_value(prerequisite, "TYPE_STRING", repeated = TRUE),
            .qualified_name = "substrait.Expression.EmbeddedFunction.PythonPickleFunction"
          )
        }
      ),
      WebAssemblyFunction = list(
        create = function(script = unspecified(), prerequisite = unspecified()) {
          create_substrait_message(
            script = clean_value(script, "TYPE_BYTES", repeated = FALSE),
            prerequisite = clean_value(prerequisite, "TYPE_STRING", repeated = TRUE),
            .qualified_name = "substrait.Expression.EmbeddedFunction.WebAssemblyFunction"
          )
        }
      ),
      create = function(arguments = unspecified(), output_type = unspecified(), python_pickle_function = unspecified(), web_assembly_function = unspecified()) {
        create_substrait_message(
          arguments = clean_value(arguments, "substrait.Expression", repeated = TRUE),
          output_type = clean_value(output_type, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          python_pickle_function = clean_value(python_pickle_function, "substrait.Expression.EmbeddedFunction.PythonPickleFunction", repeated = FALSE),
          web_assembly_function = clean_value(web_assembly_function, "substrait.Expression.EmbeddedFunction.WebAssemblyFunction", repeated = FALSE),
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
          specified = clean_value(specified, "TYPE_STRING", repeated = FALSE),
          unspecified = clean_value(unspecified, "substrait.Expression.Enum.Empty", repeated = FALSE),
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
          direct_reference = clean_value(direct_reference, "substrait.Expression.ReferenceSegment", repeated = FALSE),
          masked_reference = clean_value(masked_reference, "substrait.Expression.MaskExpression", repeated = FALSE),
          expression = clean_value(expression, "substrait.Expression", repeated = FALSE),
          root_reference = clean_value(root_reference, "substrait.Expression.FieldReference.RootReference", repeated = FALSE),
          .qualified_name = "substrait.Expression.FieldReference"
        )
      }
    ),
    IfThen = list(
      IfClause = list(
        create = function(if_ = unspecified(), then = unspecified()) {
          create_substrait_message(
            if_ = clean_value(if_, "substrait.Expression", repeated = FALSE),
            then = clean_value(then, "substrait.Expression", repeated = FALSE),
            .qualified_name = "substrait.Expression.IfThen.IfClause"
          )
        }
      ),
      create = function(ifs = unspecified(), else_ = unspecified()) {
        create_substrait_message(
          ifs = clean_value(ifs, "substrait.Expression.IfThen.IfClause", repeated = TRUE),
          else_ = clean_value(else_, "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.Expression.IfThen"
        )
      }
    ),
    Literal = list(
      Decimal = list(
        create = function(value = unspecified(), precision = unspecified(), scale = unspecified()) {
          create_substrait_message(
            value = clean_value(value, "TYPE_BYTES", repeated = FALSE),
            precision = clean_value(precision, "TYPE_INT32", repeated = FALSE),
            scale = clean_value(scale, "TYPE_INT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.Decimal"
          )
        }
      ),
      IntervalDayToSecond = list(
        create = function(days = unspecified(), seconds = unspecified()) {
          create_substrait_message(
            days = clean_value(days, "TYPE_INT32", repeated = FALSE),
            seconds = clean_value(seconds, "TYPE_INT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.IntervalDayToSecond"
          )
        }
      ),
      IntervalYearToMonth = list(
        create = function(years = unspecified(), months = unspecified()) {
          create_substrait_message(
            years = clean_value(years, "TYPE_INT32", repeated = FALSE),
            months = clean_value(months, "TYPE_INT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.IntervalYearToMonth"
          )
        }
      ),
      List = list(
        create = function(values = unspecified()) {
          create_substrait_message(
            values = clean_value(values, "substrait.Expression.Literal", repeated = TRUE),
            .qualified_name = "substrait.Expression.Literal.List"
          )
        }
      ),
      Map = list(
        KeyValue = list(
          create = function(key = unspecified(), value = unspecified()) {
            create_substrait_message(
              key = clean_value(key, "substrait.Expression.Literal", repeated = FALSE),
              value = clean_value(value, "substrait.Expression.Literal", repeated = FALSE),
              .qualified_name = "substrait.Expression.Literal.Map.KeyValue"
            )
          }
        ),
        create = function(key_values = unspecified()) {
          create_substrait_message(
            key_values = clean_value(key_values, "substrait.Expression.Literal.Map.KeyValue", repeated = TRUE),
            .qualified_name = "substrait.Expression.Literal.Map"
          )
        }
      ),
      Struct = list(
        create = function(fields = unspecified()) {
          create_substrait_message(
            fields = clean_value(fields, "substrait.Expression.Literal", repeated = TRUE),
            .qualified_name = "substrait.Expression.Literal.Struct"
          )
        }
      ),
      VarChar = list(
        create = function(value = unspecified(), length = unspecified()) {
          create_substrait_message(
            value = clean_value(value, "TYPE_STRING", repeated = FALSE),
            length = clean_value(length, "TYPE_UINT32", repeated = FALSE),
            .qualified_name = "substrait.Expression.Literal.VarChar"
          )
        }
      ),
      create = function(boolean = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year_to_month = unspecified(), interval_day_to_second = unspecified(), fixed_char = unspecified(), var_char = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), map = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), null = unspecified(), list = unspecified(), empty_list = unspecified(), empty_map = unspecified(), nullable = unspecified()) {
        create_substrait_message(
          boolean = clean_value(boolean, "TYPE_BOOL", repeated = FALSE),
          i8 = clean_value(i8, "TYPE_INT32", repeated = FALSE),
          i16 = clean_value(i16, "TYPE_INT32", repeated = FALSE),
          i32 = clean_value(i32, "TYPE_INT32", repeated = FALSE),
          i64 = clean_value(i64, "TYPE_INT64", repeated = FALSE),
          fp32 = clean_value(fp32, "TYPE_FLOAT", repeated = FALSE),
          fp64 = clean_value(fp64, "TYPE_DOUBLE", repeated = FALSE),
          string = clean_value(string, "TYPE_STRING", repeated = FALSE),
          binary = clean_value(binary, "TYPE_BYTES", repeated = FALSE),
          timestamp = clean_value(timestamp, "TYPE_INT64", repeated = FALSE),
          date = clean_value(date, "TYPE_INT32", repeated = FALSE),
          time = clean_value(time, "TYPE_INT64", repeated = FALSE),
          interval_year_to_month = clean_value(interval_year_to_month, "substrait.Expression.Literal.IntervalYearToMonth", repeated = FALSE),
          interval_day_to_second = clean_value(interval_day_to_second, "substrait.Expression.Literal.IntervalDayToSecond", repeated = FALSE),
          fixed_char = clean_value(fixed_char, "TYPE_STRING", repeated = FALSE),
          var_char = clean_value(var_char, "substrait.Expression.Literal.VarChar", repeated = FALSE),
          fixed_binary = clean_value(fixed_binary, "TYPE_BYTES", repeated = FALSE),
          decimal = clean_value(decimal, "substrait.Expression.Literal.Decimal", repeated = FALSE),
          struct_ = clean_value(struct_, "substrait.Expression.Literal.Struct", repeated = FALSE),
          map = clean_value(map, "substrait.Expression.Literal.Map", repeated = FALSE),
          timestamp_tz = clean_value(timestamp_tz, "TYPE_INT64", repeated = FALSE),
          uuid = clean_value(uuid, "TYPE_BYTES", repeated = FALSE),
          null = clean_value(null, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          list = clean_value(list, "substrait.Expression.Literal.List", repeated = FALSE),
          empty_list = clean_value(empty_list, "substrait.Expression.Literal.List", repeated = FALSE),
          empty_map = clean_value(empty_map, "substrait.Expression.Literal.Map", repeated = FALSE),
          nullable = clean_value(nullable, "TYPE_BOOL", repeated = FALSE),
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
                field = clean_value(field, "TYPE_INT32", repeated = FALSE),
                .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListElement"
              )
            }
          ),
          ListSlice = list(
            create = function(start = unspecified(), end = unspecified()) {
              create_substrait_message(
                start = clean_value(start, "TYPE_INT32", repeated = FALSE),
                end = clean_value(end, "TYPE_INT32", repeated = FALSE),
                .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListSlice"
              )
            }
          ),
          create = function(item = unspecified(), slice = unspecified()) {
            create_substrait_message(
              item = clean_value(item, "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListElement", repeated = FALSE),
              slice = clean_value(slice, "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListSlice", repeated = FALSE),
              .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem"
            )
          }
        ),
        create = function(selection = unspecified(), child = unspecified()) {
          create_substrait_message(
            selection = clean_value(selection, "substrait.Expression.MaskExpression.ListSelect.ListSelectItem", repeated = TRUE),
            child = clean_value(child, "substrait.Expression.MaskExpression.Select", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.ListSelect"
          )
        }
      ),
      MapSelect = list(
        MapKey = list(
          create = function(map_key = unspecified()) {
            create_substrait_message(
              map_key = clean_value(map_key, "TYPE_STRING", repeated = FALSE),
              .qualified_name = "substrait.Expression.MaskExpression.MapSelect.MapKey"
            )
          }
        ),
        MapKeyExpression = list(
          create = function(map_key_expression = unspecified()) {
            create_substrait_message(
              map_key_expression = clean_value(map_key_expression, "TYPE_STRING", repeated = FALSE),
              .qualified_name = "substrait.Expression.MaskExpression.MapSelect.MapKeyExpression"
            )
          }
        ),
        create = function(key = unspecified(), expression = unspecified(), child = unspecified()) {
          create_substrait_message(
            key = clean_value(key, "substrait.Expression.MaskExpression.MapSelect.MapKey", repeated = FALSE),
            expression = clean_value(expression, "substrait.Expression.MaskExpression.MapSelect.MapKeyExpression", repeated = FALSE),
            child = clean_value(child, "substrait.Expression.MaskExpression.Select", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.MapSelect"
          )
        }
      ),
      Select = list(
        create = function(struct_ = unspecified(), list = unspecified(), map = unspecified()) {
          create_substrait_message(
            struct_ = clean_value(struct_, "substrait.Expression.MaskExpression.StructSelect", repeated = FALSE),
            list = clean_value(list, "substrait.Expression.MaskExpression.ListSelect", repeated = FALSE),
            map = clean_value(map, "substrait.Expression.MaskExpression.MapSelect", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.Select"
          )
        }
      ),
      StructItem = list(
        create = function(field = unspecified(), child = unspecified()) {
          create_substrait_message(
            field = clean_value(field, "TYPE_INT32", repeated = FALSE),
            child = clean_value(child, "substrait.Expression.MaskExpression.Select", repeated = FALSE),
            .qualified_name = "substrait.Expression.MaskExpression.StructItem"
          )
        }
      ),
      StructSelect = list(
        create = function(struct_items = unspecified()) {
          create_substrait_message(
            struct_items = clean_value(struct_items, "substrait.Expression.MaskExpression.StructItem", repeated = TRUE),
            .qualified_name = "substrait.Expression.MaskExpression.StructSelect"
          )
        }
      ),
      create = function(select = unspecified(), maintain_singular_struct = unspecified()) {
        create_substrait_message(
          select = clean_value(select, "substrait.Expression.MaskExpression.StructSelect", repeated = FALSE),
          maintain_singular_struct = clean_value(maintain_singular_struct, "TYPE_BOOL", repeated = FALSE),
          .qualified_name = "substrait.Expression.MaskExpression"
        )
      }
    ),
    MultiOrList = list(
      Record = list(
        create = function(fields = unspecified()) {
          create_substrait_message(
            fields = clean_value(fields, "substrait.Expression", repeated = TRUE),
            .qualified_name = "substrait.Expression.MultiOrList.Record"
          )
        }
      ),
      create = function(value = unspecified(), options = unspecified()) {
        create_substrait_message(
          value = clean_value(value, "substrait.Expression", repeated = TRUE),
          options = clean_value(options, "substrait.Expression.MultiOrList.Record", repeated = TRUE),
          .qualified_name = "substrait.Expression.MultiOrList"
        )
      }
    ),
    ReferenceSegment = list(
      ListElement = list(
        create = function(offset = unspecified(), child = unspecified()) {
          create_substrait_message(
            offset = clean_value(offset, "TYPE_INT32", repeated = FALSE),
            child = clean_value(child, "substrait.Expression.ReferenceSegment", repeated = FALSE),
            .qualified_name = "substrait.Expression.ReferenceSegment.ListElement"
          )
        }
      ),
      MapKey = list(
        create = function(map_key = unspecified(), child = unspecified()) {
          create_substrait_message(
            map_key = clean_value(map_key, "substrait.Expression.Literal", repeated = FALSE),
            child = clean_value(child, "substrait.Expression.ReferenceSegment", repeated = FALSE),
            .qualified_name = "substrait.Expression.ReferenceSegment.MapKey"
          )
        }
      ),
      StructField = list(
        create = function(field = unspecified(), child = unspecified()) {
          create_substrait_message(
            field = clean_value(field, "TYPE_INT32", repeated = FALSE),
            child = clean_value(child, "substrait.Expression.ReferenceSegment", repeated = FALSE),
            .qualified_name = "substrait.Expression.ReferenceSegment.StructField"
          )
        }
      ),
      create = function(map_key = unspecified(), struct_field = unspecified(), list_element = unspecified()) {
        create_substrait_message(
          map_key = clean_value(map_key, "substrait.Expression.MaskExpression.MapSelect.MapKey", repeated = FALSE),
          struct_field = clean_value(struct_field, "substrait.Expression.ReferenceSegment.StructField", repeated = FALSE),
          list_element = clean_value(list_element, "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListElement", repeated = FALSE),
          .qualified_name = "substrait.Expression.ReferenceSegment"
        )
      }
    ),
    ScalarFunction = list(
      create = function(function_reference = unspecified(), args = unspecified(), output_type = unspecified()) {
        create_substrait_message(
          function_reference = clean_value(function_reference, "TYPE_UINT32", repeated = FALSE),
          args = clean_value(args, "substrait.Expression", repeated = TRUE),
          output_type = clean_value(output_type, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          .qualified_name = "substrait.Expression.ScalarFunction"
        )
      }
    ),
    SingularOrList = list(
      create = function(value = unspecified(), options = unspecified()) {
        create_substrait_message(
          value = clean_value(value, "substrait.Expression", repeated = FALSE),
          options = clean_value(options, "substrait.Expression", repeated = TRUE),
          .qualified_name = "substrait.Expression.SingularOrList"
        )
      }
    ),
    SwitchExpression = list(
      IfValue = list(
        create = function(if_ = unspecified(), then = unspecified()) {
          create_substrait_message(
            if_ = clean_value(if_, "substrait.Expression.Literal", repeated = FALSE),
            then = clean_value(then, "substrait.Expression", repeated = FALSE),
            .qualified_name = "substrait.Expression.SwitchExpression.IfValue"
          )
        }
      ),
      create = function(ifs = unspecified(), else_ = unspecified()) {
        create_substrait_message(
          ifs = clean_value(ifs, "substrait.Expression.SwitchExpression.IfValue", repeated = TRUE),
          else_ = clean_value(else_, "substrait.Expression", repeated = FALSE),
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
              offset = clean_value(offset, "TYPE_INT64", repeated = FALSE),
              .qualified_name = "substrait.Expression.WindowFunction.Bound.Following"
            )
          }
        ),
        Preceding = list(
          create = function(offset = unspecified()) {
            create_substrait_message(
              offset = clean_value(offset, "TYPE_INT64", repeated = FALSE),
              .qualified_name = "substrait.Expression.WindowFunction.Bound.Preceding"
            )
          }
        ),
        Unbounded = list(
          create = function() create_substrait_message(.qualified_name = "substrait.Expression.WindowFunction.Bound.Unbounded")
        ),
        create = function(preceding = unspecified(), following = unspecified(), current_row = unspecified(), unbounded = unspecified()) {
          create_substrait_message(
            preceding = clean_value(preceding, "substrait.Expression.WindowFunction.Bound.Preceding", repeated = FALSE),
            following = clean_value(following, "substrait.Expression.WindowFunction.Bound.Following", repeated = FALSE),
            current_row = clean_value(current_row, "substrait.Expression.WindowFunction.Bound.CurrentRow", repeated = FALSE),
            unbounded = clean_value(unbounded, "substrait.Expression.WindowFunction.Bound.Unbounded", repeated = FALSE),
            .qualified_name = "substrait.Expression.WindowFunction.Bound"
          )
        }
      ),
      create = function(function_reference = unspecified(), partitions = unspecified(), sorts = unspecified(), upper_bound = unspecified(), lower_bound = unspecified(), phase = unspecified(), output_type = unspecified(), args = unspecified()) {
        create_substrait_message(
          function_reference = clean_value(function_reference, "TYPE_UINT32", repeated = FALSE),
          partitions = clean_value(partitions, "substrait.Expression", repeated = TRUE),
          sorts = clean_value(sorts, "substrait.SortField", repeated = TRUE),
          upper_bound = clean_value(upper_bound, "substrait.Expression.WindowFunction.Bound", repeated = FALSE),
          lower_bound = clean_value(lower_bound, "substrait.Expression.WindowFunction.Bound", repeated = FALSE),
          phase = clean_value(phase, "substrait.AggregationPhase", repeated = FALSE),
          output_type = clean_value(output_type, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          args = clean_value(args, "substrait.Expression", repeated = TRUE),
          .qualified_name = "substrait.Expression.WindowFunction"
        )
      }
    ),
    create = function(literal = unspecified(), selection = unspecified(), scalar_function = unspecified(), window_function = unspecified(), if_then = unspecified(), switch_expression = unspecified(), singular_or_list = unspecified(), multi_or_list = unspecified(), enum_ = unspecified(), cast = unspecified()) {
      create_substrait_message(
        literal = clean_value(literal, "substrait.Expression.Literal", repeated = FALSE),
        selection = clean_value(selection, "substrait.Expression.FieldReference", repeated = FALSE),
        scalar_function = clean_value(scalar_function, "substrait.Expression.ScalarFunction", repeated = FALSE),
        window_function = clean_value(window_function, "substrait.Expression.WindowFunction", repeated = FALSE),
        if_then = clean_value(if_then, "substrait.Expression.IfThen", repeated = FALSE),
        switch_expression = clean_value(switch_expression, "substrait.Expression.SwitchExpression", repeated = FALSE),
        singular_or_list = clean_value(singular_or_list, "substrait.Expression.SingularOrList", repeated = FALSE),
        multi_or_list = clean_value(multi_or_list, "substrait.Expression.MultiOrList", repeated = FALSE),
        enum_ = clean_value(enum_, "substrait.Expression.Enum", repeated = FALSE),
        cast = clean_value(cast, "substrait.Expression.Cast", repeated = FALSE),
        .qualified_name = "substrait.Expression"
      )
    }
  ),
  SortField = list(
    SortDirection = list(
      SORT_DIRECTION_UNSPECIFIED = 0,
      SORT_DIRECTION_ASC_NULLS_FIRST = 1,
      SORT_DIRECTION_ASC_NULLS_LAST = 2,
      SORT_DIRECTION_DESC_NULLS_FIRST = 3,
      SORT_DIRECTION_DESC_NULLS_LAST = 4,
      SORT_DIRECTION_CLUSTERED = 5,
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.SortField.SortDirection"
        )
      }
    ),
    create = function(expr = unspecified(), direction = unspecified(), comparison_function_reference = unspecified()) {
      create_substrait_message(
        expr = clean_value(expr, "substrait.Expression", repeated = FALSE),
        direction = clean_value(direction, "substrait.SortField.SortDirection", repeated = FALSE),
        comparison_function_reference = clean_value(comparison_function_reference, "TYPE_UINT32", repeated = FALSE),
        .qualified_name = "substrait.SortField"
      )
    }
  ),
  FunctionSignature = list(
    Aggregate = list(
      create = function(arguments = unspecified(), name = unspecified(), description = unspecified(), deterministic = unspecified(), session_dependent = unspecified(), output_type = unspecified(), variadic = unspecified(), normal = unspecified(), ordered = unspecified(), max_set = unspecified(), intermediate_type = unspecified(), implementations = unspecified()) {
        create_substrait_message(
          arguments = clean_value(arguments, "substrait.FunctionSignature.Argument", repeated = TRUE),
          name = clean_value(name, "TYPE_STRING", repeated = FALSE),
          description = clean_value(description, "substrait.FunctionSignature.Description", repeated = FALSE),
          deterministic = clean_value(deterministic, "TYPE_BOOL", repeated = FALSE),
          session_dependent = clean_value(session_dependent, "TYPE_BOOL", repeated = FALSE),
          output_type = clean_value(output_type, "substrait.DerivationExpression", repeated = FALSE),
          variadic = clean_value(variadic, "substrait.FunctionSignature.FinalArgVariadic", repeated = FALSE),
          normal = clean_value(normal, "substrait.FunctionSignature.FinalArgNormal", repeated = FALSE),
          ordered = clean_value(ordered, "TYPE_BOOL", repeated = FALSE),
          max_set = clean_value(max_set, "TYPE_UINT64", repeated = FALSE),
          intermediate_type = clean_value(intermediate_type, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          implementations = clean_value(implementations, "substrait.FunctionSignature.Implementation", repeated = TRUE),
          .qualified_name = "substrait.FunctionSignature.Aggregate"
        )
      }
    ),
    Argument = list(
      EnumArgument = list(
        create = function(options = unspecified(), optional = unspecified()) {
          create_substrait_message(
            options = clean_value(options, "TYPE_STRING", repeated = TRUE),
            optional = clean_value(optional, "TYPE_BOOL", repeated = FALSE),
            .qualified_name = "substrait.FunctionSignature.Argument.EnumArgument"
          )
        }
      ),
      TypeArgument = list(
        create = function(type = unspecified()) {
          create_substrait_message(
            type = clean_value(type, "substrait.ParameterizedType", repeated = FALSE),
            .qualified_name = "substrait.FunctionSignature.Argument.TypeArgument"
          )
        }
      ),
      ValueArgument = list(
        create = function(type = unspecified(), constant = unspecified()) {
          create_substrait_message(
            type = clean_value(type, "substrait.ParameterizedType", repeated = FALSE),
            constant = clean_value(constant, "TYPE_BOOL", repeated = FALSE),
            .qualified_name = "substrait.FunctionSignature.Argument.ValueArgument"
          )
        }
      ),
      create = function(name = unspecified(), value = unspecified(), type = unspecified(), enum_ = unspecified()) {
        create_substrait_message(
          name = clean_value(name, "TYPE_STRING", repeated = FALSE),
          value = clean_value(value, "substrait.FunctionSignature.Argument.ValueArgument", repeated = FALSE),
          type = clean_value(type, "substrait.FunctionSignature.Argument.TypeArgument", repeated = FALSE),
          enum_ = clean_value(enum_, "substrait.FunctionSignature.Argument.EnumArgument", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.Argument"
        )
      }
    ),
    Description = list(
      create = function(language = unspecified(), body = unspecified()) {
        create_substrait_message(
          language = clean_value(language, "TYPE_STRING", repeated = FALSE),
          body = clean_value(body, "TYPE_STRING", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.Description"
        )
      }
    ),
    FinalArgNormal = list(
      create = function() create_substrait_message(.qualified_name = "substrait.FunctionSignature.FinalArgNormal")
    ),
    FinalArgVariadic = list(
      ParameterConsistency = list(
        PARAMETER_CONSISTENCY_UNSPECIFIED = 0,
        PARAMETER_CONSISTENCY_CONSISTENT = 1,
        PARAMETER_CONSISTENCY_INCONSISTENT = 2,
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.FunctionSignature.FinalArgVariadic.ParameterConsistency"
          )
        }
      ),
      create = function(min_args = unspecified(), max_args = unspecified(), consistency = unspecified()) {
        create_substrait_message(
          min_args = clean_value(min_args, "TYPE_INT64", repeated = FALSE),
          max_args = clean_value(max_args, "TYPE_INT64", repeated = FALSE),
          consistency = clean_value(consistency, "substrait.FunctionSignature.FinalArgVariadic.ParameterConsistency", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.FinalArgVariadic"
        )
      }
    ),
    Implementation = list(
      Type = list(
        TYPE_UNSPECIFIED = 0,
        TYPE_WEB_ASSEMBLY = 1,
        TYPE_TRINO_JAR = 2,
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.FunctionSignature.Implementation.Type"
          )
        }
      ),
      create = function(type = unspecified(), uri = unspecified()) {
        create_substrait_message(
          type = clean_value(type, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          uri = clean_value(uri, "TYPE_STRING", repeated = FALSE),
          .qualified_name = "substrait.FunctionSignature.Implementation"
        )
      }
    ),
    Scalar = list(
      create = function(arguments = unspecified(), name = unspecified(), description = unspecified(), deterministic = unspecified(), session_dependent = unspecified(), output_type = unspecified(), variadic = unspecified(), normal = unspecified(), implementations = unspecified()) {
        create_substrait_message(
          arguments = clean_value(arguments, "substrait.FunctionSignature.Argument", repeated = TRUE),
          name = clean_value(name, "TYPE_STRING", repeated = TRUE),
          description = clean_value(description, "substrait.FunctionSignature.Description", repeated = FALSE),
          deterministic = clean_value(deterministic, "TYPE_BOOL", repeated = FALSE),
          session_dependent = clean_value(session_dependent, "TYPE_BOOL", repeated = FALSE),
          output_type = clean_value(output_type, "substrait.DerivationExpression", repeated = FALSE),
          variadic = clean_value(variadic, "substrait.FunctionSignature.FinalArgVariadic", repeated = FALSE),
          normal = clean_value(normal, "substrait.FunctionSignature.FinalArgNormal", repeated = FALSE),
          implementations = clean_value(implementations, "substrait.FunctionSignature.Implementation", repeated = TRUE),
          .qualified_name = "substrait.FunctionSignature.Scalar"
        )
      }
    ),
    Window = list(
      WindowType = list(
        WINDOW_TYPE_UNSPECIFIED = 0,
        WINDOW_TYPE_STREAMING = 1,
        WINDOW_TYPE_PARTITION = 2,
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.FunctionSignature.Window.WindowType"
          )
        }
      ),
      create = function(arguments = unspecified(), name = unspecified(), description = unspecified(), deterministic = unspecified(), session_dependent = unspecified(), intermediate_type = unspecified(), output_type = unspecified(), variadic = unspecified(), normal = unspecified(), ordered = unspecified(), max_set = unspecified(), window_type = unspecified(), implementations = unspecified()) {
        create_substrait_message(
          arguments = clean_value(arguments, "substrait.FunctionSignature.Argument", repeated = TRUE),
          name = clean_value(name, "TYPE_STRING", repeated = TRUE),
          description = clean_value(description, "substrait.FunctionSignature.Description", repeated = FALSE),
          deterministic = clean_value(deterministic, "TYPE_BOOL", repeated = FALSE),
          session_dependent = clean_value(session_dependent, "TYPE_BOOL", repeated = FALSE),
          intermediate_type = clean_value(intermediate_type, "substrait.DerivationExpression", repeated = FALSE),
          output_type = clean_value(output_type, "substrait.DerivationExpression", repeated = FALSE),
          variadic = clean_value(variadic, "substrait.FunctionSignature.FinalArgVariadic", repeated = FALSE),
          normal = clean_value(normal, "substrait.FunctionSignature.FinalArgNormal", repeated = FALSE),
          ordered = clean_value(ordered, "TYPE_BOOL", repeated = FALSE),
          max_set = clean_value(max_set, "TYPE_UINT64", repeated = FALSE),
          window_type = clean_value(window_type, "substrait.FunctionSignature.Window.WindowType", repeated = FALSE),
          implementations = clean_value(implementations, "substrait.FunctionSignature.Implementation", repeated = TRUE),
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
          literal = clean_value(literal, "TYPE_INT32", repeated = FALSE),
          parameter = clean_value(parameter, "substrait.ParameterizedType.IntegerParameter", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.IntegerOption"
        )
      }
    ),
    IntegerParameter = list(
      create = function(name = unspecified(), range_start_inclusive = unspecified(), range_end_exclusive = unspecified()) {
        create_substrait_message(
          name = clean_value(name, "TYPE_STRING", repeated = FALSE),
          range_start_inclusive = clean_value(range_start_inclusive, "substrait.ParameterizedType.NullableInteger", repeated = FALSE),
          range_end_exclusive = clean_value(range_end_exclusive, "substrait.ParameterizedType.NullableInteger", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.IntegerParameter"
        )
      }
    ),
    NullableInteger = list(
      create = function(value = unspecified()) {
        create_substrait_message(
          value = clean_value(value, "TYPE_INT64", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.NullableInteger"
        )
      }
    ),
    ParameterizedDecimal = list(
      create = function(scale = unspecified(), precision = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          scale = clean_value(scale, "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          precision = clean_value(precision, "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedDecimal"
        )
      }
    ),
    ParameterizedFixedBinary = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedFixedBinary"
        )
      }
    ),
    ParameterizedFixedChar = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedFixedChar"
        )
      }
    ),
    ParameterizedList = list(
      create = function(type = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type = clean_value(type, "substrait.ParameterizedType", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedList"
        )
      }
    ),
    ParameterizedMap = list(
      create = function(key = unspecified(), value = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          key = clean_value(key, "substrait.ParameterizedType", repeated = FALSE),
          value = clean_value(value, "substrait.ParameterizedType", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedMap"
        )
      }
    ),
    ParameterizedNamedStruct = list(
      create = function(names = unspecified(), struct_ = unspecified()) {
        create_substrait_message(
          names = clean_value(names, "TYPE_STRING", repeated = TRUE),
          struct_ = clean_value(struct_, "substrait.ParameterizedType.ParameterizedStruct", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedNamedStruct"
        )
      }
    ),
    ParameterizedStruct = list(
      create = function(types = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          types = clean_value(types, "substrait.ParameterizedType", repeated = TRUE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedStruct"
        )
      }
    ),
    ParameterizedVarChar = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "substrait.ParameterizedType.IntegerOption", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.ParameterizedType.ParameterizedVarChar"
        )
      }
    ),
    TypeParameter = list(
      create = function(name = unspecified(), bounds = unspecified()) {
        create_substrait_message(
          name = clean_value(name, "TYPE_STRING", repeated = FALSE),
          bounds = clean_value(bounds, "substrait.ParameterizedType", repeated = TRUE),
          .qualified_name = "substrait.ParameterizedType.TypeParameter"
        )
      }
    ),
    create = function(bool_ = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year = unspecified(), interval_day = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), fixed_char = unspecified(), varchar = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), list = unspecified(), map = unspecified(), user_defined_pointer = unspecified(), type_parameter = unspecified()) {
      create_substrait_message(
        bool_ = clean_value(bool_, "substrait.Type.Boolean", repeated = FALSE),
        i8 = clean_value(i8, "substrait.Type.I8", repeated = FALSE),
        i16 = clean_value(i16, "substrait.Type.I16", repeated = FALSE),
        i32 = clean_value(i32, "substrait.Type.I32", repeated = FALSE),
        i64 = clean_value(i64, "substrait.Type.I64", repeated = FALSE),
        fp32 = clean_value(fp32, "substrait.Type.FP32", repeated = FALSE),
        fp64 = clean_value(fp64, "substrait.Type.FP64", repeated = FALSE),
        string = clean_value(string, "substrait.Type.String", repeated = FALSE),
        binary = clean_value(binary, "substrait.Type.Binary", repeated = FALSE),
        timestamp = clean_value(timestamp, "substrait.Type.Timestamp", repeated = FALSE),
        date = clean_value(date, "substrait.Type.Date", repeated = FALSE),
        time = clean_value(time, "substrait.Type.Time", repeated = FALSE),
        interval_year = clean_value(interval_year, "substrait.Type.IntervalYear", repeated = FALSE),
        interval_day = clean_value(interval_day, "substrait.Type.IntervalDay", repeated = FALSE),
        timestamp_tz = clean_value(timestamp_tz, "substrait.Type.TimestampTZ", repeated = FALSE),
        uuid = clean_value(uuid, "substrait.Type.UUID", repeated = FALSE),
        fixed_char = clean_value(fixed_char, "substrait.ParameterizedType.ParameterizedFixedChar", repeated = FALSE),
        varchar = clean_value(varchar, "substrait.ParameterizedType.ParameterizedVarChar", repeated = FALSE),
        fixed_binary = clean_value(fixed_binary, "substrait.ParameterizedType.ParameterizedFixedBinary", repeated = FALSE),
        decimal = clean_value(decimal, "substrait.ParameterizedType.ParameterizedDecimal", repeated = FALSE),
        struct_ = clean_value(struct_, "substrait.ParameterizedType.ParameterizedStruct", repeated = FALSE),
        list = clean_value(list, "substrait.ParameterizedType.ParameterizedList", repeated = FALSE),
        map = clean_value(map, "substrait.ParameterizedType.ParameterizedMap", repeated = FALSE),
        user_defined_pointer = clean_value(user_defined_pointer, "TYPE_UINT32", repeated = FALSE),
        type_parameter = clean_value(type_parameter, "substrait.ParameterizedType.TypeParameter", repeated = FALSE),
        .qualified_name = "substrait.ParameterizedType"
      )
    }
  ),
  Plan = list(
    create = function(extension_uris = unspecified(), extensions = unspecified(), relations = unspecified(), advanced_extensions = unspecified(), expected_type_urls = unspecified()) {
      create_substrait_message(
        extension_uris = clean_value(extension_uris, "substrait.extensions.SimpleExtensionURI", repeated = TRUE),
        extensions = clean_value(extensions, "substrait.extensions.SimpleExtensionDeclaration", repeated = TRUE),
        relations = clean_value(relations, "substrait.PlanRel", repeated = TRUE),
        advanced_extensions = clean_value(advanced_extensions, "substrait.extensions.AdvancedExtension", repeated = FALSE),
        expected_type_urls = clean_value(expected_type_urls, "TYPE_STRING", repeated = TRUE),
        .qualified_name = "substrait.Plan"
      )
    }
  ),
  PlanRel = list(
    create = function(rel = unspecified(), root = unspecified()) {
      create_substrait_message(
        rel = clean_value(rel, "substrait.Rel", repeated = FALSE),
        root = clean_value(root, "substrait.RelRoot", repeated = FALSE),
        .qualified_name = "substrait.PlanRel"
      )
    }
  ),
  AggregateRel = list(
    Grouping = list(
      create = function(grouping_expressions = unspecified()) {
        create_substrait_message(
          grouping_expressions = clean_value(grouping_expressions, "substrait.Expression", repeated = TRUE),
          .qualified_name = "substrait.AggregateRel.Grouping"
        )
      }
    ),
    Measure = list(
      create = function(measure = unspecified(), filter = unspecified()) {
        create_substrait_message(
          measure = clean_value(measure, "substrait.AggregateFunction", repeated = FALSE),
          filter = clean_value(filter, "substrait.Expression", repeated = FALSE),
          .qualified_name = "substrait.AggregateRel.Measure"
        )
      }
    ),
    create = function(common = unspecified(), input = unspecified(), groupings = unspecified(), measures = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "substrait.Rel", repeated = FALSE),
        groupings = clean_value(groupings, "substrait.AggregateRel.Grouping", repeated = TRUE),
        measures = clean_value(measures, "substrait.AggregateRel.Measure", repeated = TRUE),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.AggregateRel"
      )
    }
  ),
  ExtensionLeafRel = list(
    create = function(common = unspecified(), detail = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        detail = clean_value(detail, "substrait.Any", repeated = FALSE),
        .qualified_name = "substrait.ExtensionLeafRel"
      )
    }
  ),
  ExtensionMultiRel = list(
    create = function(common = unspecified(), inputs = unspecified(), detail = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        inputs = clean_value(inputs, "substrait.Rel", repeated = TRUE),
        detail = clean_value(detail, "substrait.Any", repeated = FALSE),
        .qualified_name = "substrait.ExtensionMultiRel"
      )
    }
  ),
  ExtensionSingleRel = list(
    create = function(common = unspecified(), input = unspecified(), detail = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "substrait.Rel", repeated = FALSE),
        detail = clean_value(detail, "substrait.Any", repeated = FALSE),
        .qualified_name = "substrait.ExtensionSingleRel"
      )
    }
  ),
  FetchRel = list(
    create = function(common = unspecified(), input = unspecified(), offset = unspecified(), count = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "substrait.Rel", repeated = FALSE),
        offset = clean_value(offset, "TYPE_INT64", repeated = FALSE),
        count = clean_value(count, "TYPE_INT64", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.FetchRel"
      )
    }
  ),
  FilterRel = list(
    create = function(common = unspecified(), input = unspecified(), condition = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "substrait.Rel", repeated = FALSE),
        condition = clean_value(condition, "substrait.Expression", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.FilterRel"
      )
    }
  ),
  JoinRel = list(
    JoinType = list(
      JOIN_TYPE_UNSPECIFIED = 0,
      JOIN_TYPE_INNER = 1,
      JOIN_TYPE_OUTER = 2,
      JOIN_TYPE_LEFT = 3,
      JOIN_TYPE_RIGHT = 4,
      JOIN_TYPE_SEMI = 5,
      JOIN_TYPE_ANTI = 6,
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.JoinRel.JoinType"
        )
      }
    ),
    create = function(common = unspecified(), left = unspecified(), right = unspecified(), expression = unspecified(), post_join_filter = unspecified(), type = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        left = clean_value(left, "substrait.Rel", repeated = FALSE),
        right = clean_value(right, "substrait.Rel", repeated = FALSE),
        expression = clean_value(expression, "substrait.Expression", repeated = FALSE),
        post_join_filter = clean_value(post_join_filter, "substrait.Expression", repeated = FALSE),
        type = clean_value(type, "substrait.JoinRel.JoinType", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.JoinRel"
      )
    }
  ),
  ProjectRel = list(
    create = function(common = unspecified(), input = unspecified(), expressions = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "substrait.Rel", repeated = FALSE),
        expressions = clean_value(expressions, "substrait.Expression", repeated = TRUE),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.ProjectRel"
      )
    }
  ),
  ReadRel = list(
    ExtensionTable = list(
      create = function(detail = unspecified()) {
        create_substrait_message(
          detail = clean_value(detail, "substrait.Any", repeated = FALSE),
          .qualified_name = "substrait.ReadRel.ExtensionTable"
        )
      }
    ),
    LocalFiles = list(
      FileOrFiles = list(
        FileFormat = list(
          FILE_FORMAT_UNSPECIFIED = 0,
          FILE_FORMAT_PARQUET = 1,
          create = function(value) {
            create_substrait_enum(
              value,
              .qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles.FileFormat"
            )
          }
        ),
        create = function(uri_path = unspecified(), uri_path_glob = unspecified(), uri_file = unspecified(), uri_folder = unspecified(), format = unspecified(), partition_index = unspecified(), start = unspecified(), length = unspecified()) {
          create_substrait_message(
            uri_path = clean_value(uri_path, "TYPE_STRING", repeated = FALSE),
            uri_path_glob = clean_value(uri_path_glob, "TYPE_STRING", repeated = FALSE),
            uri_file = clean_value(uri_file, "TYPE_STRING", repeated = FALSE),
            uri_folder = clean_value(uri_folder, "TYPE_STRING", repeated = FALSE),
            format = clean_value(format, "substrait.ReadRel.LocalFiles.FileOrFiles.FileFormat", repeated = FALSE),
            partition_index = clean_value(partition_index, "TYPE_UINT64", repeated = FALSE),
            start = clean_value(start, "TYPE_UINT64", repeated = FALSE),
            length = clean_value(length, "TYPE_UINT64", repeated = FALSE),
            .qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles"
          )
        }
      ),
      create = function(items = unspecified(), advanced_extension = unspecified()) {
        create_substrait_message(
          items = clean_value(items, "substrait.ReadRel.LocalFiles.FileOrFiles", repeated = TRUE),
          advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
          .qualified_name = "substrait.ReadRel.LocalFiles"
        )
      }
    ),
    NamedTable = list(
      create = function(names = unspecified(), advanced_extension = unspecified()) {
        create_substrait_message(
          names = clean_value(names, "TYPE_STRING", repeated = TRUE),
          advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
          .qualified_name = "substrait.ReadRel.NamedTable"
        )
      }
    ),
    VirtualTable = list(
      create = function(values = unspecified()) {
        create_substrait_message(
          values = clean_value(values, "substrait.Expression.Literal.Struct", repeated = TRUE),
          .qualified_name = "substrait.ReadRel.VirtualTable"
        )
      }
    ),
    create = function(common = unspecified(), base_schema = unspecified(), filter = unspecified(), projection = unspecified(), advanced_extension = unspecified(), virtual_table = unspecified(), local_files = unspecified(), named_table = unspecified(), extension_table = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        base_schema = clean_value(base_schema, "substrait.NamedStruct", repeated = FALSE),
        filter = clean_value(filter, "substrait.Expression", repeated = FALSE),
        projection = clean_value(projection, "substrait.Expression.MaskExpression", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
        virtual_table = clean_value(virtual_table, "substrait.ReadRel.VirtualTable", repeated = FALSE),
        local_files = clean_value(local_files, "substrait.ReadRel.LocalFiles", repeated = FALSE),
        named_table = clean_value(named_table, "substrait.ReadRel.NamedTable", repeated = FALSE),
        extension_table = clean_value(extension_table, "substrait.ReadRel.ExtensionTable", repeated = FALSE),
        .qualified_name = "substrait.ReadRel"
      )
    }
  ),
  Rel = list(
    create = function(read = unspecified(), filter = unspecified(), fetch = unspecified(), aggregate = unspecified(), sort = unspecified(), join = unspecified(), project = unspecified(), set = unspecified(), extension_single = unspecified(), extension_multi = unspecified(), extension_leaf = unspecified()) {
      create_substrait_message(
        read = clean_value(read, "substrait.ReadRel", repeated = FALSE),
        filter = clean_value(filter, "substrait.FilterRel", repeated = FALSE),
        fetch = clean_value(fetch, "substrait.FetchRel", repeated = FALSE),
        aggregate = clean_value(aggregate, "substrait.AggregateRel", repeated = FALSE),
        sort = clean_value(sort, "substrait.SortRel", repeated = FALSE),
        join = clean_value(join, "substrait.JoinRel", repeated = FALSE),
        project = clean_value(project, "substrait.ProjectRel", repeated = FALSE),
        set = clean_value(set, "substrait.SetRel", repeated = FALSE),
        extension_single = clean_value(extension_single, "substrait.ExtensionSingleRel", repeated = FALSE),
        extension_multi = clean_value(extension_multi, "substrait.ExtensionMultiRel", repeated = FALSE),
        extension_leaf = clean_value(extension_leaf, "substrait.ExtensionLeafRel", repeated = FALSE),
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
          output_mapping = clean_value(output_mapping, "TYPE_INT32", repeated = TRUE),
          .qualified_name = "substrait.RelCommon.Emit"
        )
      }
    ),
    Hint = list(
      RuntimeConstraint = list(
        create = function(advanced_extension = unspecified()) {
          create_substrait_message(
            advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
            .qualified_name = "substrait.RelCommon.Hint.RuntimeConstraint"
          )
        }
      ),
      Stats = list(
        create = function(row_count = unspecified(), record_size = unspecified(), advanced_extension = unspecified()) {
          create_substrait_message(
            row_count = clean_value(row_count, "TYPE_DOUBLE", repeated = FALSE),
            record_size = clean_value(record_size, "TYPE_DOUBLE", repeated = FALSE),
            advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
            .qualified_name = "substrait.RelCommon.Hint.Stats"
          )
        }
      ),
      create = function(stats = unspecified(), constraint = unspecified(), advanced_extension = unspecified()) {
        create_substrait_message(
          stats = clean_value(stats, "substrait.RelCommon.Hint.Stats", repeated = FALSE),
          constraint = clean_value(constraint, "substrait.RelCommon.Hint.RuntimeConstraint", repeated = FALSE),
          advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
          .qualified_name = "substrait.RelCommon.Hint"
        )
      }
    ),
    create = function(direct = unspecified(), emit = unspecified(), hint = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        direct = clean_value(direct, "substrait.RelCommon.Direct", repeated = FALSE),
        emit = clean_value(emit, "substrait.RelCommon.Emit", repeated = FALSE),
        hint = clean_value(hint, "substrait.RelCommon.Hint", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.RelCommon"
      )
    }
  ),
  RelRoot = list(
    create = function(input = unspecified(), names = unspecified()) {
      create_substrait_message(
        input = clean_value(input, "substrait.Rel", repeated = FALSE),
        names = clean_value(names, "TYPE_STRING", repeated = TRUE),
        .qualified_name = "substrait.RelRoot"
      )
    }
  ),
  SetRel = list(
    SetOp = list(
      SET_OP_UNSPECIFIED = 0,
      SET_OP_MINUS_PRIMARY = 1,
      SET_OP_MINUS_MULTISET = 2,
      SET_OP_INTERSECTION_PRIMARY = 3,
      SET_OP_INTERSECTION_MULTISET = 4,
      SET_OP_UNION_DISTINCT = 5,
      SET_OP_UNION_ALL = 6,
      create = function(value) {
        create_substrait_enum(
          value,
          .qualified_name = "substrait.SetRel.SetOp"
        )
      }
    ),
    create = function(common = unspecified(), inputs = unspecified(), op = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        inputs = clean_value(inputs, "substrait.Rel", repeated = TRUE),
        op = clean_value(op, "substrait.SetRel.SetOp", repeated = FALSE),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.SetRel"
      )
    }
  ),
  SortRel = list(
    create = function(common = unspecified(), input = unspecified(), sorts = unspecified(), advanced_extension = unspecified()) {
      create_substrait_message(
        common = clean_value(common, "substrait.RelCommon", repeated = FALSE),
        input = clean_value(input, "substrait.Rel", repeated = FALSE),
        sorts = clean_value(sorts, "substrait.SortField", repeated = TRUE),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension", repeated = FALSE),
        .qualified_name = "substrait.SortRel"
      )
    }
  ),
  DerivationExpression = list(
    BinaryOp = list(
      BinaryOpType = list(
        BINARY_OP_TYPE_UNSPECIFIED = 0,
        BINARY_OP_TYPE_PLUS = 1,
        BINARY_OP_TYPE_MINUS = 2,
        BINARY_OP_TYPE_MULTIPLY = 3,
        BINARY_OP_TYPE_DIVIDE = 4,
        BINARY_OP_TYPE_MIN = 5,
        BINARY_OP_TYPE_MAX = 6,
        BINARY_OP_TYPE_GREATER_THAN = 7,
        BINARY_OP_TYPE_LESS_THAN = 8,
        BINARY_OP_TYPE_AND = 9,
        BINARY_OP_TYPE_OR = 10,
        BINARY_OP_TYPE_EQUALS = 11,
        BINARY_OP_TYPE_COVERS = 12,
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.DerivationExpression.BinaryOp.BinaryOpType"
          )
        }
      ),
      create = function(op_type = unspecified(), arg1 = unspecified(), arg2 = unspecified()) {
        create_substrait_message(
          op_type = clean_value(op_type, "substrait.DerivationExpression.BinaryOp.BinaryOpType", repeated = FALSE),
          arg1 = clean_value(arg1, "substrait.DerivationExpression", repeated = FALSE),
          arg2 = clean_value(arg2, "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.BinaryOp"
        )
      }
    ),
    ExpressionDecimal = list(
      create = function(scale = unspecified(), precision = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          scale = clean_value(scale, "substrait.DerivationExpression", repeated = FALSE),
          precision = clean_value(precision, "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionDecimal"
        )
      }
    ),
    ExpressionFixedBinary = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionFixedBinary"
        )
      }
    ),
    ExpressionFixedChar = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionFixedChar"
        )
      }
    ),
    ExpressionList = list(
      create = function(type = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type = clean_value(type, "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionList"
        )
      }
    ),
    ExpressionMap = list(
      create = function(key = unspecified(), value = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          key = clean_value(key, "substrait.DerivationExpression", repeated = FALSE),
          value = clean_value(value, "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionMap"
        )
      }
    ),
    ExpressionNamedStruct = list(
      create = function(names = unspecified(), struct_ = unspecified()) {
        create_substrait_message(
          names = clean_value(names, "TYPE_STRING", repeated = TRUE),
          struct_ = clean_value(struct_, "substrait.DerivationExpression.ExpressionStruct", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionNamedStruct"
        )
      }
    ),
    ExpressionStruct = list(
      create = function(types = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          types = clean_value(types, "substrait.DerivationExpression", repeated = TRUE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionStruct"
        )
      }
    ),
    ExpressionVarChar = list(
      create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "substrait.DerivationExpression", repeated = FALSE),
          variation_pointer = clean_value(variation_pointer, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ExpressionVarChar"
        )
      }
    ),
    IfElse = list(
      create = function(if_condition = unspecified(), if_return = unspecified(), else_return = unspecified()) {
        create_substrait_message(
          if_condition = clean_value(if_condition, "substrait.DerivationExpression", repeated = FALSE),
          if_return = clean_value(if_return, "substrait.DerivationExpression", repeated = FALSE),
          else_return = clean_value(else_return, "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.IfElse"
        )
      }
    ),
    ReturnProgram = list(
      Assignment = list(
        create = function(name = unspecified(), expression = unspecified()) {
          create_substrait_message(
            name = clean_value(name, "TYPE_STRING", repeated = FALSE),
            expression = clean_value(expression, "substrait.DerivationExpression", repeated = FALSE),
            .qualified_name = "substrait.DerivationExpression.ReturnProgram.Assignment"
          )
        }
      ),
      create = function(assignments = unspecified(), final_expression = unspecified()) {
        create_substrait_message(
          assignments = clean_value(assignments, "substrait.DerivationExpression.ReturnProgram.Assignment", repeated = TRUE),
          final_expression = clean_value(final_expression, "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.ReturnProgram"
        )
      }
    ),
    UnaryOp = list(
      UnaryOpType = list(
        UNARY_OP_TYPE_UNSPECIFIED = 0,
        UNARY_OP_TYPE_BOOLEAN_NOT = 1,
        create = function(value) {
          create_substrait_enum(
            value,
            .qualified_name = "substrait.DerivationExpression.UnaryOp.UnaryOpType"
          )
        }
      ),
      create = function(op_type = unspecified(), arg = unspecified()) {
        create_substrait_message(
          op_type = clean_value(op_type, "substrait.DerivationExpression.UnaryOp.UnaryOpType", repeated = FALSE),
          arg = clean_value(arg, "substrait.DerivationExpression", repeated = FALSE),
          .qualified_name = "substrait.DerivationExpression.UnaryOp"
        )
      }
    ),
    create = function(bool_ = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year = unspecified(), interval_day = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), fixed_char = unspecified(), varchar = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), list = unspecified(), map = unspecified(), user_defined_pointer = unspecified(), type_parameter_name = unspecified(), integer_parameter_name = unspecified(), integer_literal = unspecified(), unary_op = unspecified(), binary_op = unspecified(), if_else = unspecified(), return_program = unspecified()) {
      create_substrait_message(
        bool_ = clean_value(bool_, "substrait.Type.Boolean", repeated = FALSE),
        i8 = clean_value(i8, "substrait.Type.I8", repeated = FALSE),
        i16 = clean_value(i16, "substrait.Type.I16", repeated = FALSE),
        i32 = clean_value(i32, "substrait.Type.I32", repeated = FALSE),
        i64 = clean_value(i64, "substrait.Type.I64", repeated = FALSE),
        fp32 = clean_value(fp32, "substrait.Type.FP32", repeated = FALSE),
        fp64 = clean_value(fp64, "substrait.Type.FP64", repeated = FALSE),
        string = clean_value(string, "substrait.Type.String", repeated = FALSE),
        binary = clean_value(binary, "substrait.Type.Binary", repeated = FALSE),
        timestamp = clean_value(timestamp, "substrait.Type.Timestamp", repeated = FALSE),
        date = clean_value(date, "substrait.Type.Date", repeated = FALSE),
        time = clean_value(time, "substrait.Type.Time", repeated = FALSE),
        interval_year = clean_value(interval_year, "substrait.Type.IntervalYear", repeated = FALSE),
        interval_day = clean_value(interval_day, "substrait.Type.IntervalDay", repeated = FALSE),
        timestamp_tz = clean_value(timestamp_tz, "substrait.Type.TimestampTZ", repeated = FALSE),
        uuid = clean_value(uuid, "substrait.Type.UUID", repeated = FALSE),
        fixed_char = clean_value(fixed_char, "substrait.DerivationExpression.ExpressionFixedChar", repeated = FALSE),
        varchar = clean_value(varchar, "substrait.DerivationExpression.ExpressionVarChar", repeated = FALSE),
        fixed_binary = clean_value(fixed_binary, "substrait.DerivationExpression.ExpressionFixedBinary", repeated = FALSE),
        decimal = clean_value(decimal, "substrait.DerivationExpression.ExpressionDecimal", repeated = FALSE),
        struct_ = clean_value(struct_, "substrait.DerivationExpression.ExpressionStruct", repeated = FALSE),
        list = clean_value(list, "substrait.DerivationExpression.ExpressionList", repeated = FALSE),
        map = clean_value(map, "substrait.DerivationExpression.ExpressionMap", repeated = FALSE),
        user_defined_pointer = clean_value(user_defined_pointer, "TYPE_UINT32", repeated = FALSE),
        type_parameter_name = clean_value(type_parameter_name, "TYPE_STRING", repeated = FALSE),
        integer_parameter_name = clean_value(integer_parameter_name, "TYPE_STRING", repeated = FALSE),
        integer_literal = clean_value(integer_literal, "TYPE_INT32", repeated = FALSE),
        unary_op = clean_value(unary_op, "substrait.DerivationExpression.UnaryOp", repeated = FALSE),
        binary_op = clean_value(binary_op, "substrait.DerivationExpression.BinaryOp", repeated = FALSE),
        if_else = clean_value(if_else, "substrait.DerivationExpression.IfElse", repeated = FALSE),
        return_program = clean_value(return_program, "substrait.DerivationExpression.ReturnProgram", repeated = FALSE),
        .qualified_name = "substrait.DerivationExpression"
      )
    }
  ),
  NamedStruct = list(
    create = function(names = unspecified(), struct_ = unspecified()) {
      create_substrait_message(
        names = clean_value(names, "TYPE_STRING", repeated = TRUE),
        struct_ = clean_value(struct_, "substrait.Expression.Literal.Struct", repeated = FALSE),
        .qualified_name = "substrait.NamedStruct"
      )
    }
  ),
  Type = list(
    Nullability = list(
      NULLABILITY_UNSPECIFIED = 0,
      NULLABILITY_NULLABLE = 1,
      NULLABILITY_REQUIRED = 2,
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
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Binary"
        )
      }
    ),
    Boolean = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Boolean"
        )
      }
    ),
    Date = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Date"
        )
      }
    ),
    Decimal = list(
      create = function(scale = unspecified(), precision = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          scale = clean_value(scale, "TYPE_INT32", repeated = FALSE),
          precision = clean_value(precision, "TYPE_INT32", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Decimal"
        )
      }
    ),
    FP32 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FP32"
        )
      }
    ),
    FP64 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FP64"
        )
      }
    ),
    FixedBinary = list(
      create = function(length = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_INT32", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FixedBinary"
        )
      }
    ),
    FixedChar = list(
      create = function(length = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_INT32", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.FixedChar"
        )
      }
    ),
    I16 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I16"
        )
      }
    ),
    I32 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I32"
        )
      }
    ),
    I64 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I64"
        )
      }
    ),
    I8 = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.I8"
        )
      }
    ),
    IntervalDay = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.IntervalDay"
        )
      }
    ),
    IntervalYear = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.IntervalYear"
        )
      }
    ),
    List = list(
      create = function(type = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type = clean_value(type, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.List"
        )
      }
    ),
    Map = list(
      create = function(key = unspecified(), value = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          key = clean_value(key, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          value = clean_value(value, "substrait.FunctionSignature.Implementation.Type", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Map"
        )
      }
    ),
    String = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.String"
        )
      }
    ),
    Struct = list(
      create = function(types = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          types = clean_value(types, "substrait.FunctionSignature.Implementation.Type", repeated = TRUE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Struct"
        )
      }
    ),
    Time = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Time"
        )
      }
    ),
    Timestamp = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.Timestamp"
        )
      }
    ),
    TimestampTZ = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.TimestampTZ"
        )
      }
    ),
    UUID = list(
      create = function(type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.UUID"
        )
      }
    ),
    VarChar = list(
      create = function(length = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) {
        create_substrait_message(
          length = clean_value(length, "TYPE_INT32", repeated = FALSE),
          type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32", repeated = FALSE),
          nullability = clean_value(nullability, "substrait.Type.Nullability", repeated = FALSE),
          .qualified_name = "substrait.Type.VarChar"
        )
      }
    ),
    create = function(bool_ = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year = unspecified(), interval_day = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), fixed_char = unspecified(), varchar = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), list = unspecified(), map = unspecified(), user_defined_type_reference = unspecified()) {
      create_substrait_message(
        bool_ = clean_value(bool_, "substrait.Type.Boolean", repeated = FALSE),
        i8 = clean_value(i8, "substrait.Type.I8", repeated = FALSE),
        i16 = clean_value(i16, "substrait.Type.I16", repeated = FALSE),
        i32 = clean_value(i32, "substrait.Type.I32", repeated = FALSE),
        i64 = clean_value(i64, "substrait.Type.I64", repeated = FALSE),
        fp32 = clean_value(fp32, "substrait.Type.FP32", repeated = FALSE),
        fp64 = clean_value(fp64, "substrait.Type.FP64", repeated = FALSE),
        string = clean_value(string, "substrait.Type.String", repeated = FALSE),
        binary = clean_value(binary, "substrait.Type.Binary", repeated = FALSE),
        timestamp = clean_value(timestamp, "substrait.Type.Timestamp", repeated = FALSE),
        date = clean_value(date, "substrait.Type.Date", repeated = FALSE),
        time = clean_value(time, "substrait.Type.Time", repeated = FALSE),
        interval_year = clean_value(interval_year, "substrait.Type.IntervalYear", repeated = FALSE),
        interval_day = clean_value(interval_day, "substrait.Type.IntervalDay", repeated = FALSE),
        timestamp_tz = clean_value(timestamp_tz, "substrait.Type.TimestampTZ", repeated = FALSE),
        uuid = clean_value(uuid, "substrait.Type.UUID", repeated = FALSE),
        fixed_char = clean_value(fixed_char, "substrait.Type.FixedChar", repeated = FALSE),
        varchar = clean_value(varchar, "substrait.Expression.Literal.VarChar", repeated = FALSE),
        fixed_binary = clean_value(fixed_binary, "substrait.Type.FixedBinary", repeated = FALSE),
        decimal = clean_value(decimal, "substrait.Expression.Literal.Decimal", repeated = FALSE),
        struct_ = clean_value(struct_, "substrait.Expression.Literal.Struct", repeated = FALSE),
        list = clean_value(list, "substrait.Expression.Literal.List", repeated = FALSE),
        map = clean_value(map, "substrait.Expression.Literal.Map", repeated = FALSE),
        user_defined_type_reference = clean_value(user_defined_type_reference, "TYPE_UINT32", repeated = FALSE),
        .qualified_name = "substrait.Type"
      )
    }
  )
)
