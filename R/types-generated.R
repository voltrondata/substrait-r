substrait = list(
  AggregationPhase = list(
    AGGREGATION_PHASE_UNSPECIFIED = 0,
    AGGREGATION_PHASE_INITIAL_TO_INTERMEDIATE = 1,
    AGGREGATION_PHASE_INTERMEDIATE_TO_INTERMEDIATE = 2,
    AGGREGATION_PHASE_INITIAL_TO_RESULT = 3,
    AGGREGATION_PHASE_INTERMEDIATE_TO_RESULT = 4,
    create = function(value) create_substrait_enum(
      value,
      .qualified_name = "substrait.AggregationPhase"
    )
  ),
  Any = list(
      create = function(type_url = unspecified(), value = unspecified()) create_substrait_message(
        type_url = clean_value(type_url, "TYPE_STRING"),
        value = clean_value(value, "TYPE_BYTES"),
        .qualified_name = "substrait.Any"
      )
  ),
  Capabilities = list(
      SimpleExtension = list(
            create = function(uri = unspecified(), function_keys = unspecified(), type_keys = unspecified(), type_variation_keys = unspecified()) create_substrait_message(
              uri = clean_value(uri, "TYPE_STRING"),
              function_keys = clean_value(function_keys, "TYPE_STRING"),
              type_keys = clean_value(type_keys, "TYPE_STRING"),
              type_variation_keys = clean_value(type_variation_keys, "TYPE_STRING"),
              .qualified_name = "substrait.Capabilities.SimpleExtension"
            )
      ),
      create = function(substrait_versions = unspecified(), advanced_extension_type_urls = unspecified(), simple_extensions = unspecified()) create_substrait_message(
        substrait_versions = clean_value(substrait_versions, "TYPE_STRING"),
        advanced_extension_type_urls = clean_value(advanced_extension_type_urls, "TYPE_STRING"),
        simple_extensions = clean_value(simple_extensions, "substrait.Capabilities.SimpleExtension"),
        .qualified_name = "substrait.Capabilities"
      )
  ),
  AggregateFunction = list(
      create = function(function_reference = unspecified(), args = unspecified(), sorts = unspecified(), phase = unspecified(), output_type = unspecified()) create_substrait_message(
        function_reference = clean_value(function_reference, "TYPE_UINT32"),
        args = clean_value(args, "substrait.Expression"),
        sorts = clean_value(sorts, "substrait.SortField"),
        phase = clean_value(phase, "substrait.AggregationPhase"),
        output_type = clean_value(output_type, "substrait.FunctionSignature.Implementation.Type"),
        .qualified_name = "substrait.AggregateFunction"
      )
  ),
  Expression = list(
      Cast = list(
            create = function(type = unspecified(), input = unspecified()) create_substrait_message(
              type = clean_value(type, "substrait.FunctionSignature.Implementation.Type"),
              input = clean_value(input, "substrait.Expression"),
              .qualified_name = "substrait.Expression.Cast"
            )
      ),
      EmbeddedFunction = list(
            PythonPickleFunction = list(
                    create = function(function_ = unspecified(), prerequisite = unspecified()) create_substrait_message(
                      function_ = clean_value(function_, "TYPE_BYTES"),
                      prerequisite = clean_value(prerequisite, "TYPE_STRING"),
                      .qualified_name = "substrait.Expression.EmbeddedFunction.PythonPickleFunction"
                    )
            ),
            WebAssemblyFunction = list(
                    create = function(script = unspecified(), prerequisite = unspecified()) create_substrait_message(
                      script = clean_value(script, "TYPE_BYTES"),
                      prerequisite = clean_value(prerequisite, "TYPE_STRING"),
                      .qualified_name = "substrait.Expression.EmbeddedFunction.WebAssemblyFunction"
                    )
            ),
            create = function(arguments = unspecified(), output_type = unspecified(), python_pickle_function = unspecified(), web_assembly_function = unspecified()) create_substrait_message(
              arguments = clean_value(arguments, "substrait.Expression"),
              output_type = clean_value(output_type, "substrait.FunctionSignature.Implementation.Type"),
              python_pickle_function = clean_value(python_pickle_function, "substrait.Expression.EmbeddedFunction.PythonPickleFunction"),
              web_assembly_function = clean_value(web_assembly_function, "substrait.Expression.EmbeddedFunction.WebAssemblyFunction"),
              .qualified_name = "substrait.Expression.EmbeddedFunction"
            )
      ),
      Enum = list(
            Empty = list(
                    create = function() create_substrait_message(
                    ,
                      .qualified_name = "substrait.Expression.Enum.Empty"
                    )
            ),
            create = function(specified = unspecified(), unspecified = unspecified()) create_substrait_message(
              specified = clean_value(specified, "TYPE_STRING"),
              unspecified = clean_value(unspecified, "substrait.Expression.Enum.Empty"),
              .qualified_name = "substrait.Expression.Enum"
            )
      ),
      FieldReference = list(
            RootReference = list(
                    create = function() create_substrait_message(
                      .qualified_name = "substrait.Expression.FieldReference.RootReference"
                    )
            ),
            create = function(direct_reference = unspecified(), masked_reference = unspecified(), expression = unspecified(), root_reference = unspecified()) create_substrait_message(
              direct_reference = clean_value(direct_reference, "substrait.Expression.ReferenceSegment"),
              masked_reference = clean_value(masked_reference, "substrait.Expression.MaskExpression"),
              expression = clean_value(expression, "substrait.Expression"),
              root_reference = clean_value(root_reference, "substrait.Expression.FieldReference.RootReference"),
              .qualified_name = "substrait.Expression.FieldReference"
            )
      ),
      IfThen = list(
            IfClause = list(
                    create = function(if_ = unspecified(), then = unspecified()) create_substrait_message(
                      if_ = clean_value(if_, "substrait.Expression"),
                      then = clean_value(then, "substrait.Expression"),
                      .qualified_name = "substrait.Expression.IfThen.IfClause"
                    )
            ),
            create = function(ifs = unspecified(), else_ = unspecified()) create_substrait_message(
              ifs = clean_value(ifs, "substrait.Expression.IfThen.IfClause"),
              else_ = clean_value(else_, "substrait.Expression"),
              .qualified_name = "substrait.Expression.IfThen"
            )
      ),
      Literal = list(
            Decimal = list(
                    create = function(value = unspecified(), precision = unspecified(), scale = unspecified()) create_substrait_message(
                      value = clean_value(value, "TYPE_BYTES"),
                      precision = clean_value(precision, "TYPE_INT32"),
                      scale = clean_value(scale, "TYPE_INT32"),
                      .qualified_name = "substrait.Expression.Literal.Decimal"
                    )
            ),
            IntervalDayToSecond = list(
                    create = function(days = unspecified(), seconds = unspecified()) create_substrait_message(
                      days = clean_value(days, "TYPE_INT32"),
                      seconds = clean_value(seconds, "TYPE_INT32"),
                      .qualified_name = "substrait.Expression.Literal.IntervalDayToSecond"
                    )
            ),
            IntervalYearToMonth = list(
                    create = function(years = unspecified(), months = unspecified()) create_substrait_message(
                      years = clean_value(years, "TYPE_INT32"),
                      months = clean_value(months, "TYPE_INT32"),
                      .qualified_name = "substrait.Expression.Literal.IntervalYearToMonth"
                    )
            ),
            List = list(
                    create = function(values = unspecified()) create_substrait_message(
                      values = clean_value(values, "substrait.Expression.Literal"),
                      .qualified_name = "substrait.Expression.Literal.List"
                    )
            ),
            Map = list(
                    KeyValue = list(
                              create = function(key = unspecified(), value = unspecified()) create_substrait_message(
                                key = clean_value(key, "substrait.Expression.Literal"),
                                value = clean_value(value, "substrait.Expression.Literal"),
                                .qualified_name = "substrait.Expression.Literal.Map.KeyValue"
                              )
                    ),
                    create = function(key_values = unspecified()) create_substrait_message(
                      key_values = clean_value(key_values, "substrait.Expression.Literal.Map.KeyValue"),
                      .qualified_name = "substrait.Expression.Literal.Map"
                    )
            ),
            Struct = list(
                    create = function(fields = unspecified()) create_substrait_message(
                      fields = clean_value(fields, "substrait.Expression.Literal"),
                      .qualified_name = "substrait.Expression.Literal.Struct"
                    )
            ),
            VarChar = list(
                    create = function(value = unspecified(), length = unspecified()) create_substrait_message(
                      value = clean_value(value, "TYPE_STRING"),
                      length = clean_value(length, "TYPE_UINT32"),
                      .qualified_name = "substrait.Expression.Literal.VarChar"
                    )
            ),
            create = function(boolean = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year_to_month = unspecified(), interval_day_to_second = unspecified(), fixed_char = unspecified(), var_char = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), map = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), null = unspecified(), list = unspecified(), empty_list = unspecified(), empty_map = unspecified(), nullable = unspecified()) create_substrait_message(
              boolean = clean_value(boolean, "TYPE_BOOL"),
              i8 = clean_value(i8, "TYPE_INT32"),
              i16 = clean_value(i16, "TYPE_INT32"),
              i32 = clean_value(i32, "TYPE_INT32"),
              i64 = clean_value(i64, "TYPE_INT64"),
              fp32 = clean_value(fp32, "TYPE_FLOAT"),
              fp64 = clean_value(fp64, "TYPE_DOUBLE"),
              string = clean_value(string, "TYPE_STRING"),
              binary = clean_value(binary, "TYPE_BYTES"),
              timestamp = clean_value(timestamp, "TYPE_INT64"),
              date = clean_value(date, "TYPE_INT32"),
              time = clean_value(time, "TYPE_INT64"),
              interval_year_to_month = clean_value(interval_year_to_month, "substrait.Expression.Literal.IntervalYearToMonth"),
              interval_day_to_second = clean_value(interval_day_to_second, "substrait.Expression.Literal.IntervalDayToSecond"),
              fixed_char = clean_value(fixed_char, "TYPE_STRING"),
              var_char = clean_value(var_char, "substrait.Expression.Literal.VarChar"),
              fixed_binary = clean_value(fixed_binary, "TYPE_BYTES"),
              decimal = clean_value(decimal, "substrait.Expression.Literal.Decimal"),
              struct_ = clean_value(struct_, "substrait.Expression.Literal.Struct"),
              map = clean_value(map, "substrait.Expression.Literal.Map"),
              timestamp_tz = clean_value(timestamp_tz, "TYPE_INT64"),
              uuid = clean_value(uuid, "TYPE_BYTES"),
              null = clean_value(null, "substrait.FunctionSignature.Implementation.Type"),
              list = clean_value(list, "substrait.Expression.Literal.List"),
              empty_list = clean_value(empty_list, "substrait.Expression.Literal.List"),
              empty_map = clean_value(empty_map, "substrait.Expression.Literal.Map"),
              nullable = clean_value(nullable, "TYPE_BOOL"),
              .qualified_name = "substrait.Expression.Literal"
            )
      ),
      MaskExpression = list(
            ListSelect = list(
                    ListSelectItem = list(
                              ListElement = list(
                                          create = function(field = unspecified()) create_substrait_message(
                                            field = clean_value(field, "TYPE_INT32"),
                                            .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListElement"
                                          )
                              ),
                              ListSlice = list(
                                          create = function(start = unspecified(), end = unspecified()) create_substrait_message(
                                            start = clean_value(start, "TYPE_INT32"),
                                            end = clean_value(end, "TYPE_INT32"),
                                            .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListSlice"
                                          )
                              ),
                              create = function(item = unspecified(), slice = unspecified()) create_substrait_message(
                                item = clean_value(item, "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListElement"),
                                slice = clean_value(slice, "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListSlice"),
                                .qualified_name = "substrait.Expression.MaskExpression.ListSelect.ListSelectItem"
                              )
                    ),
                    create = function(selection = unspecified(), child = unspecified()) create_substrait_message(
                      selection = clean_value(selection, "substrait.Expression.MaskExpression.ListSelect.ListSelectItem"),
                      child = clean_value(child, "substrait.Expression.MaskExpression.Select"),
                      .qualified_name = "substrait.Expression.MaskExpression.ListSelect"
                    )
            ),
            MapSelect = list(
                    MapKey = list(
                              create = function(map_key = unspecified()) create_substrait_message(
                                map_key = clean_value(map_key, "TYPE_STRING"),
                                .qualified_name = "substrait.Expression.MaskExpression.MapSelect.MapKey"
                              )
                    ),
                    MapKeyExpression = list(
                              create = function(map_key_expression = unspecified()) create_substrait_message(
                                map_key_expression = clean_value(map_key_expression, "TYPE_STRING"),
                                .qualified_name = "substrait.Expression.MaskExpression.MapSelect.MapKeyExpression"
                              )
                    ),
                    create = function(key = unspecified(), expression = unspecified(), child = unspecified()) create_substrait_message(
                      key = clean_value(key, "substrait.Expression.MaskExpression.MapSelect.MapKey"),
                      expression = clean_value(expression, "substrait.Expression.MaskExpression.MapSelect.MapKeyExpression"),
                      child = clean_value(child, "substrait.Expression.MaskExpression.Select"),
                      .qualified_name = "substrait.Expression.MaskExpression.MapSelect"
                    )
            ),
            Select = list(
                    create = function(struct_ = unspecified(), list = unspecified(), map = unspecified()) create_substrait_message(
                      struct_ = clean_value(struct_, "substrait.Expression.MaskExpression.StructSelect"),
                      list = clean_value(list, "substrait.Expression.MaskExpression.ListSelect"),
                      map = clean_value(map, "substrait.Expression.MaskExpression.MapSelect"),
                      .qualified_name = "substrait.Expression.MaskExpression.Select"
                    )
            ),
            StructItem = list(
                    create = function(field = unspecified(), child = unspecified()) create_substrait_message(
                      field = clean_value(field, "TYPE_INT32"),
                      child = clean_value(child, "substrait.Expression.MaskExpression.Select"),
                      .qualified_name = "substrait.Expression.MaskExpression.StructItem"
                    )
            ),
            StructSelect = list(
                    create = function(struct_items = unspecified()) create_substrait_message(
                      struct_items = clean_value(struct_items, "substrait.Expression.MaskExpression.StructItem"),
                      .qualified_name = "substrait.Expression.MaskExpression.StructSelect"
                    )
            ),
            create = function(select = unspecified(), maintain_singular_struct = unspecified()) create_substrait_message(
              select = clean_value(select, "substrait.Expression.MaskExpression.StructSelect"),
              maintain_singular_struct = clean_value(maintain_singular_struct, "TYPE_BOOL"),
              .qualified_name = "substrait.Expression.MaskExpression"
            )
      ),
      MultiOrList = list(
            Record = list(
                    create = function(fields = unspecified()) create_substrait_message(
                      fields = clean_value(fields, "substrait.Expression"),
                      .qualified_name = "substrait.Expression.MultiOrList.Record"
                    )
            ),
            create = function(value = unspecified(), options = unspecified()) create_substrait_message(
              value = clean_value(value, "substrait.Expression"),
              options = clean_value(options, "substrait.Expression.MultiOrList.Record"),
              .qualified_name = "substrait.Expression.MultiOrList"
            )
      ),
      ReferenceSegment = list(
            ListElement = list(
                    create = function(offset = unspecified(), child = unspecified()) create_substrait_message(
                      offset = clean_value(offset, "TYPE_INT32"),
                      child = clean_value(child, "substrait.Expression.ReferenceSegment"),
                      .qualified_name = "substrait.Expression.ReferenceSegment.ListElement"
                    )
            ),
            MapKey = list(
                    create = function(map_key = unspecified(), child = unspecified()) create_substrait_message(
                      map_key = clean_value(map_key, "substrait.Expression.Literal"),
                      child = clean_value(child, "substrait.Expression.ReferenceSegment"),
                      .qualified_name = "substrait.Expression.ReferenceSegment.MapKey"
                    )
            ),
            StructField = list(
                    create = function(field = unspecified(), child = unspecified()) create_substrait_message(
                      field = clean_value(field, "TYPE_INT32"),
                      child = clean_value(child, "substrait.Expression.ReferenceSegment"),
                      .qualified_name = "substrait.Expression.ReferenceSegment.StructField"
                    )
            ),
            create = function(map_key = unspecified(), struct_field = unspecified(), list_element = unspecified()) create_substrait_message(
              map_key = clean_value(map_key, "substrait.Expression.MaskExpression.MapSelect.MapKey"),
              struct_field = clean_value(struct_field, "substrait.Expression.ReferenceSegment.StructField"),
              list_element = clean_value(list_element, "substrait.Expression.MaskExpression.ListSelect.ListSelectItem.ListElement"),
              .qualified_name = "substrait.Expression.ReferenceSegment"
            )
      ),
      ScalarFunction = list(
            create = function(function_reference = unspecified(), args = unspecified(), output_type = unspecified()) create_substrait_message(
              function_reference = clean_value(function_reference, "TYPE_UINT32"),
              args = clean_value(args, "substrait.Expression"),
              output_type = clean_value(output_type, "substrait.FunctionSignature.Implementation.Type"),
              .qualified_name = "substrait.Expression.ScalarFunction"
            )
      ),
      SingularOrList = list(
            create = function(value = unspecified(), options = unspecified()) create_substrait_message(
              value = clean_value(value, "substrait.Expression"),
              options = clean_value(options, "substrait.Expression"),
              .qualified_name = "substrait.Expression.SingularOrList"
            )
      ),
      SwitchExpression = list(
            IfValue = list(
                    create = function(if_ = unspecified(), then = unspecified()) create_substrait_message(
                      if_ = clean_value(if_, "substrait.Expression.Literal"),
                      then = clean_value(then, "substrait.Expression"),
                      .qualified_name = "substrait.Expression.SwitchExpression.IfValue"
                    )
            ),
            create = function(ifs = unspecified(), else_ = unspecified()) create_substrait_message(
              ifs = clean_value(ifs, "substrait.Expression.SwitchExpression.IfValue"),
              else_ = clean_value(else_, "substrait.Expression"),
              .qualified_name = "substrait.Expression.SwitchExpression"
            )
      ),
      WindowFunction = list(
            Bound = list(
                    CurrentRow = list(
                              create = function() create_substrait_message(
                                .qualified_name = "substrait.Expression.WindowFunction.Bound.CurrentRow"
                              )
                    ),
                    Following = list(
                              create = function(offset = unspecified()) create_substrait_message(
                                offset = clean_value(offset, "TYPE_INT64"),
                                .qualified_name = "substrait.Expression.WindowFunction.Bound.Following"
                              )
                    ),
                    Preceding = list(
                              create = function(offset = unspecified()) create_substrait_message(
                                offset = clean_value(offset, "TYPE_INT64"),
                                .qualified_name = "substrait.Expression.WindowFunction.Bound.Preceding"
                              )
                    ),
                    Unbounded = list(
                              create = function() create_substrait_message(
                                .qualified_name = "substrait.Expression.WindowFunction.Bound.Unbounded"
                              )
                    ),
                    create = function(preceding = unspecified(), following = unspecified(), current_row = unspecified(), unbounded = unspecified()) create_substrait_message(
                      preceding = clean_value(preceding, "substrait.Expression.WindowFunction.Bound.Preceding"),
                      following = clean_value(following, "substrait.Expression.WindowFunction.Bound.Following"),
                      current_row = clean_value(current_row, "substrait.Expression.WindowFunction.Bound.CurrentRow"),
                      unbounded = clean_value(unbounded, "substrait.Expression.WindowFunction.Bound.Unbounded"),
                      .qualified_name = "substrait.Expression.WindowFunction.Bound"
                    )
            ),
            create = function(function_reference = unspecified(), partitions = unspecified(), sorts = unspecified(), upper_bound = unspecified(), lower_bound = unspecified(), phase = unspecified(), output_type = unspecified(), args = unspecified()) create_substrait_message(
              function_reference = clean_value(function_reference, "TYPE_UINT32"),
              partitions = clean_value(partitions, "substrait.Expression"),
              sorts = clean_value(sorts, "substrait.SortField"),
              upper_bound = clean_value(upper_bound, "substrait.Expression.WindowFunction.Bound"),
              lower_bound = clean_value(lower_bound, "substrait.Expression.WindowFunction.Bound"),
              phase = clean_value(phase, "substrait.AggregationPhase"),
              output_type = clean_value(output_type, "substrait.FunctionSignature.Implementation.Type"),
              args = clean_value(args, "substrait.Expression"),
              .qualified_name = "substrait.Expression.WindowFunction"
            )
      ),
      create = function(literal = unspecified(), selection = unspecified(), scalar_function = unspecified(), window_function = unspecified(), if_then = unspecified(), switch_expression = unspecified(), singular_or_list = unspecified(), multi_or_list = unspecified(), enum_ = unspecified(), cast = unspecified()) create_substrait_message(
        literal = clean_value(literal, "substrait.Expression.Literal"),
        selection = clean_value(selection, "substrait.Expression.FieldReference"),
        scalar_function = clean_value(scalar_function, "substrait.Expression.ScalarFunction"),
        window_function = clean_value(window_function, "substrait.Expression.WindowFunction"),
        if_then = clean_value(if_then, "substrait.Expression.IfThen"),
        switch_expression = clean_value(switch_expression, "substrait.Expression.SwitchExpression"),
        singular_or_list = clean_value(singular_or_list, "substrait.Expression.SingularOrList"),
        multi_or_list = clean_value(multi_or_list, "substrait.Expression.MultiOrList"),
        enum_ = clean_value(enum_, "substrait.Expression.Enum"),
        cast = clean_value(cast, "substrait.Expression.Cast"),
        .qualified_name = "substrait.Expression"
      )
  ),
  SortField = list(
      SortDirection = list(
        SORT_DIRECTION_UNSPECIFIED = 0,
        SORT_DIRECTION_ASC_NULLS_FIRST = 1,
        SORT_DIRECTION_ASC_NULLS_LAST = 2,
        SORT_DIRECTION_DESC_NULLS_FIRST = 3,
        SORT_DIRECTION_DESC_NULLS_LAST = 4,
        SORT_DIRECTION_CLUSTERED = 5,
        create = function(value) create_substrait_enum(
          value,
          .qualified_name = "substrait.SortField.SortDirection"
        )
      ),
      create = function(expr = unspecified(), direction = unspecified(), comparison_function_reference = unspecified()) create_substrait_message(
        expr = clean_value(expr, "substrait.Expression"),
        direction = clean_value(direction, "substrait.SortField.SortDirection"),
        comparison_function_reference = clean_value(comparison_function_reference, "TYPE_UINT32"),
        .qualified_name = "substrait.SortField"
      )
  ),
  FunctionSignature = list(
      Aggregate = list(
            create = function(arguments = unspecified(), name = unspecified(), description = unspecified(), deterministic = unspecified(), session_dependent = unspecified(), output_type = unspecified(), variadic = unspecified(), normal = unspecified(), ordered = unspecified(), max_set = unspecified(), intermediate_type = unspecified(), implementations = unspecified()) create_substrait_message(
              arguments = clean_value(arguments, "substrait.FunctionSignature.Argument"),
              name = clean_value(name, "TYPE_STRING"),
              description = clean_value(description, "substrait.FunctionSignature.Description"),
              deterministic = clean_value(deterministic, "TYPE_BOOL"),
              session_dependent = clean_value(session_dependent, "TYPE_BOOL"),
              output_type = clean_value(output_type, "substrait.DerivationExpression"),
              variadic = clean_value(variadic, "substrait.FunctionSignature.FinalArgVariadic"),
              normal = clean_value(normal, "substrait.FunctionSignature.FinalArgNormal"),
              ordered = clean_value(ordered, "TYPE_BOOL"),
              max_set = clean_value(max_set, "TYPE_UINT64"),
              intermediate_type = clean_value(intermediate_type, "substrait.FunctionSignature.Implementation.Type"),
              implementations = clean_value(implementations, "substrait.FunctionSignature.Implementation"),
              .qualified_name = "substrait.FunctionSignature.Aggregate"
            )
      ),
      Argument = list(
            EnumArgument = list(
                    create = function(options = unspecified(), optional = unspecified()) create_substrait_message(
                      options = clean_value(options, "TYPE_STRING"),
                      optional = clean_value(optional, "TYPE_BOOL"),
                      .qualified_name = "substrait.FunctionSignature.Argument.EnumArgument"
                    )
            ),
            TypeArgument = list(
                    create = function(type = unspecified()) create_substrait_message(
                      type = clean_value(type, "substrait.ParameterizedType"),
                      .qualified_name = "substrait.FunctionSignature.Argument.TypeArgument"
                    )
            ),
            ValueArgument = list(
                    create = function(type = unspecified(), constant = unspecified()) create_substrait_message(
                      type = clean_value(type, "substrait.ParameterizedType"),
                      constant = clean_value(constant, "TYPE_BOOL"),
                      .qualified_name = "substrait.FunctionSignature.Argument.ValueArgument"
                    )
            ),
            create = function(name = unspecified(), value = unspecified(), type = unspecified(), enum_ = unspecified()) create_substrait_message(
              name = clean_value(name, "TYPE_STRING"),
              value = clean_value(value, "substrait.FunctionSignature.Argument.ValueArgument"),
              type = clean_value(type, "substrait.FunctionSignature.Argument.TypeArgument"),
              enum_ = clean_value(enum_, "substrait.FunctionSignature.Argument.EnumArgument"),
              .qualified_name = "substrait.FunctionSignature.Argument"
            )
      ),
      Description = list(
            create = function(language = unspecified(), body = unspecified()) create_substrait_message(
              language = clean_value(language, "TYPE_STRING"),
              body = clean_value(body, "TYPE_STRING"),
              .qualified_name = "substrait.FunctionSignature.Description"
            )
      ),
      FinalArgNormal = list(
            create = function() create_substrait_message(
              .qualified_name = "substrait.FunctionSignature.FinalArgNormal"
            )
      ),
      FinalArgVariadic = list(
            ParameterConsistency = list(
              PARAMETER_CONSISTENCY_UNSPECIFIED = 0,
              PARAMETER_CONSISTENCY_CONSISTENT = 1,
              PARAMETER_CONSISTENCY_INCONSISTENT = 2,
              create = function(value) create_substrait_enum(
                value,
                .qualified_name = "substrait.FunctionSignature.FinalArgVariadic.ParameterConsistency"
              )
            ),
            create = function(min_args = unspecified(), max_args = unspecified(), consistency = unspecified()) create_substrait_message(
              min_args = clean_value(min_args, "TYPE_INT64"),
              max_args = clean_value(max_args, "TYPE_INT64"),
              consistency = clean_value(consistency, "substrait.FunctionSignature.FinalArgVariadic.ParameterConsistency"),
              .qualified_name = "substrait.FunctionSignature.FinalArgVariadic"
            )
      ),
      Implementation = list(
            Type = list(
              TYPE_UNSPECIFIED = 0,
              TYPE_WEB_ASSEMBLY = 1,
              TYPE_TRINO_JAR = 2,
              create = function(value) create_substrait_enum(
                value,
                .qualified_name = "substrait.FunctionSignature.Implementation.Type"
              )
            ),
            create = function(type = unspecified(), uri = unspecified()) create_substrait_message(
              type = clean_value(type, "substrait.FunctionSignature.Implementation.Type"),
              uri = clean_value(uri, "TYPE_STRING"),
              .qualified_name = "substrait.FunctionSignature.Implementation"
            )
      ),
      Scalar = list(
            create = function(arguments = unspecified(), name = unspecified(), description = unspecified(), deterministic = unspecified(), session_dependent = unspecified(), output_type = unspecified(), variadic = unspecified(), normal = unspecified(), implementations = unspecified()) create_substrait_message(
              arguments = clean_value(arguments, "substrait.FunctionSignature.Argument"),
              name = clean_value(name, "TYPE_STRING"),
              description = clean_value(description, "substrait.FunctionSignature.Description"),
              deterministic = clean_value(deterministic, "TYPE_BOOL"),
              session_dependent = clean_value(session_dependent, "TYPE_BOOL"),
              output_type = clean_value(output_type, "substrait.DerivationExpression"),
              variadic = clean_value(variadic, "substrait.FunctionSignature.FinalArgVariadic"),
              normal = clean_value(normal, "substrait.FunctionSignature.FinalArgNormal"),
              implementations = clean_value(implementations, "substrait.FunctionSignature.Implementation"),
              .qualified_name = "substrait.FunctionSignature.Scalar"
            )
      ),
      Window = list(
            WindowType = list(
              WINDOW_TYPE_UNSPECIFIED = 0,
              WINDOW_TYPE_STREAMING = 1,
              WINDOW_TYPE_PARTITION = 2,
              create = function(value) create_substrait_enum(
                value,
                .qualified_name = "substrait.FunctionSignature.Window.WindowType"
              )
            ),
            create = function(arguments = unspecified(), name = unspecified(), description = unspecified(), deterministic = unspecified(), session_dependent = unspecified(), intermediate_type = unspecified(), output_type = unspecified(), variadic = unspecified(), normal = unspecified(), ordered = unspecified(), max_set = unspecified(), window_type = unspecified(), implementations = unspecified()) create_substrait_message(
              arguments = clean_value(arguments, "substrait.FunctionSignature.Argument"),
              name = clean_value(name, "TYPE_STRING"),
              description = clean_value(description, "substrait.FunctionSignature.Description"),
              deterministic = clean_value(deterministic, "TYPE_BOOL"),
              session_dependent = clean_value(session_dependent, "TYPE_BOOL"),
              intermediate_type = clean_value(intermediate_type, "substrait.DerivationExpression"),
              output_type = clean_value(output_type, "substrait.DerivationExpression"),
              variadic = clean_value(variadic, "substrait.FunctionSignature.FinalArgVariadic"),
              normal = clean_value(normal, "substrait.FunctionSignature.FinalArgNormal"),
              ordered = clean_value(ordered, "TYPE_BOOL"),
              max_set = clean_value(max_set, "TYPE_UINT64"),
              window_type = clean_value(window_type, "substrait.FunctionSignature.Window.WindowType"),
              implementations = clean_value(implementations, "substrait.FunctionSignature.Implementation"),
              .qualified_name = "substrait.FunctionSignature.Window"
            )
      ),
      create = function() create_substrait_message(
        .qualified_name = "substrait.FunctionSignature"
      )
  ),
  ParameterizedType = list(
      IntegerOption = list(
            create = function(literal = unspecified(), parameter = unspecified()) create_substrait_message(
              literal = clean_value(literal, "TYPE_INT32"),
              parameter = clean_value(parameter, "substrait.ParameterizedType.IntegerParameter"),
              .qualified_name = "substrait.ParameterizedType.IntegerOption"
            )
      ),
      IntegerParameter = list(
            create = function(name = unspecified(), range_start_inclusive = unspecified(), range_end_exclusive = unspecified()) create_substrait_message(
              name = clean_value(name, "TYPE_STRING"),
              range_start_inclusive = clean_value(range_start_inclusive, "substrait.ParameterizedType.NullableInteger"),
              range_end_exclusive = clean_value(range_end_exclusive, "substrait.ParameterizedType.NullableInteger"),
              .qualified_name = "substrait.ParameterizedType.IntegerParameter"
            )
      ),
      NullableInteger = list(
            create = function(value = unspecified()) create_substrait_message(
              value = clean_value(value, "TYPE_INT64"),
              .qualified_name = "substrait.ParameterizedType.NullableInteger"
            )
      ),
      ParameterizedDecimal = list(
            create = function(scale = unspecified(), precision = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              scale = clean_value(scale, "substrait.ParameterizedType.IntegerOption"),
              precision = clean_value(precision, "substrait.ParameterizedType.IntegerOption"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.ParameterizedType.ParameterizedDecimal"
            )
      ),
      ParameterizedFixedBinary = list(
            create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              length = clean_value(length, "substrait.ParameterizedType.IntegerOption"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.ParameterizedType.ParameterizedFixedBinary"
            )
      ),
      ParameterizedFixedChar = list(
            create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              length = clean_value(length, "substrait.ParameterizedType.IntegerOption"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.ParameterizedType.ParameterizedFixedChar"
            )
      ),
      ParameterizedList = list(
            create = function(type = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              type = clean_value(type, "substrait.ParameterizedType"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.ParameterizedType.ParameterizedList"
            )
      ),
      ParameterizedMap = list(
            create = function(key = unspecified(), value = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              key = clean_value(key, "substrait.ParameterizedType"),
              value = clean_value(value, "substrait.ParameterizedType"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.ParameterizedType.ParameterizedMap"
            )
      ),
      ParameterizedNamedStruct = list(
            create = function(names = unspecified(), struct_ = unspecified()) create_substrait_message(
              names = clean_value(names, "TYPE_STRING"),
              struct_ = clean_value(struct_, "substrait.ParameterizedType.ParameterizedStruct"),
              .qualified_name = "substrait.ParameterizedType.ParameterizedNamedStruct"
            )
      ),
      ParameterizedStruct = list(
            create = function(types = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              types = clean_value(types, "substrait.ParameterizedType"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.ParameterizedType.ParameterizedStruct"
            )
      ),
      ParameterizedVarChar = list(
            create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              length = clean_value(length, "substrait.ParameterizedType.IntegerOption"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.ParameterizedType.ParameterizedVarChar"
            )
      ),
      TypeParameter = list(
            create = function(name = unspecified(), bounds = unspecified()) create_substrait_message(
              name = clean_value(name, "TYPE_STRING"),
              bounds = clean_value(bounds, "substrait.ParameterizedType"),
              .qualified_name = "substrait.ParameterizedType.TypeParameter"
            )
      ),
      create = function(bool_ = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year = unspecified(), interval_day = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), fixed_char = unspecified(), varchar = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), list = unspecified(), map = unspecified(), user_defined_pointer = unspecified(), type_parameter = unspecified()) create_substrait_message(
        bool_ = clean_value(bool_, "substrait.Type.Boolean"),
        i8 = clean_value(i8, "substrait.Type.I8"),
        i16 = clean_value(i16, "substrait.Type.I16"),
        i32 = clean_value(i32, "substrait.Type.I32"),
        i64 = clean_value(i64, "substrait.Type.I64"),
        fp32 = clean_value(fp32, "substrait.Type.FP32"),
        fp64 = clean_value(fp64, "substrait.Type.FP64"),
        string = clean_value(string, "substrait.Type.String"),
        binary = clean_value(binary, "substrait.Type.Binary"),
        timestamp = clean_value(timestamp, "substrait.Type.Timestamp"),
        date = clean_value(date, "substrait.Type.Date"),
        time = clean_value(time, "substrait.Type.Time"),
        interval_year = clean_value(interval_year, "substrait.Type.IntervalYear"),
        interval_day = clean_value(interval_day, "substrait.Type.IntervalDay"),
        timestamp_tz = clean_value(timestamp_tz, "substrait.Type.TimestampTZ"),
        uuid = clean_value(uuid, "substrait.Type.UUID"),
        fixed_char = clean_value(fixed_char, "substrait.ParameterizedType.ParameterizedFixedChar"),
        varchar = clean_value(varchar, "substrait.ParameterizedType.ParameterizedVarChar"),
        fixed_binary = clean_value(fixed_binary, "substrait.ParameterizedType.ParameterizedFixedBinary"),
        decimal = clean_value(decimal, "substrait.ParameterizedType.ParameterizedDecimal"),
        struct_ = clean_value(struct_, "substrait.ParameterizedType.ParameterizedStruct"),
        list = clean_value(list, "substrait.ParameterizedType.ParameterizedList"),
        map = clean_value(map, "substrait.ParameterizedType.ParameterizedMap"),
        user_defined_pointer = clean_value(user_defined_pointer, "TYPE_UINT32"),
        type_parameter = clean_value(type_parameter, "substrait.ParameterizedType.TypeParameter"),
        .qualified_name = "substrait.ParameterizedType"
      )
  ),
  Plan = list(
      create = function(extension_uris = unspecified(), extensions = unspecified(), relations = unspecified(), advanced_extensions = unspecified(), expected_type_urls = unspecified()) create_substrait_message(
        extension_uris = clean_value(extension_uris, "substrait.extensions.SimpleExtensionURI"),
        extensions = clean_value(extensions, "substrait.extensions.SimpleExtensionDeclaration"),
        relations = clean_value(relations, "substrait.PlanRel"),
        advanced_extensions = clean_value(advanced_extensions, "substrait.extensions.AdvancedExtension"),
        expected_type_urls = clean_value(expected_type_urls, "TYPE_STRING"),
        .qualified_name = "substrait.Plan"
      )
  ),
  PlanRel = list(
      create = function(rel = unspecified(), root = unspecified()) create_substrait_message(
        rel = clean_value(rel, "substrait.Rel"),
        root = clean_value(root, "substrait.RelRoot"),
        .qualified_name = "substrait.PlanRel"
      )
  ),
  AggregateRel = list(
      Grouping = list(
            create = function(grouping_expressions = unspecified()) create_substrait_message(
              grouping_expressions = clean_value(grouping_expressions, "substrait.Expression"),
              .qualified_name = "substrait.AggregateRel.Grouping"
            )
      ),
      Measure = list(
            create = function(measure = unspecified(), filter = unspecified()) create_substrait_message(
              measure = clean_value(measure, "substrait.AggregateFunction"),
              filter = clean_value(filter, "substrait.Expression"),
              .qualified_name = "substrait.AggregateRel.Measure"
            )
      ),
      create = function(common = unspecified(), input = unspecified(), groupings = unspecified(), measures = unspecified(), advanced_extension = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        input = clean_value(input, "substrait.Rel"),
        groupings = clean_value(groupings, "substrait.AggregateRel.Grouping"),
        measures = clean_value(measures, "substrait.AggregateRel.Measure"),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
        .qualified_name = "substrait.AggregateRel"
      )
  ),
  ExtensionLeafRel = list(
      create = function(common = unspecified(), detail = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        detail = clean_value(detail, "substrait.Any"),
        .qualified_name = "substrait.ExtensionLeafRel"
      )
  ),
  ExtensionMultiRel = list(
      create = function(common = unspecified(), inputs = unspecified(), detail = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        inputs = clean_value(inputs, "substrait.Rel"),
        detail = clean_value(detail, "substrait.Any"),
        .qualified_name = "substrait.ExtensionMultiRel"
      )
  ),
  ExtensionSingleRel = list(
      create = function(common = unspecified(), input = unspecified(), detail = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        input = clean_value(input, "substrait.Rel"),
        detail = clean_value(detail, "substrait.Any"),
        .qualified_name = "substrait.ExtensionSingleRel"
      )
  ),
  FetchRel = list(
      create = function(common = unspecified(), input = unspecified(), offset = unspecified(), count = unspecified(), advanced_extension = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        input = clean_value(input, "substrait.Rel"),
        offset = clean_value(offset, "TYPE_INT64"),
        count = clean_value(count, "TYPE_INT64"),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
        .qualified_name = "substrait.FetchRel"
      )
  ),
  FilterRel = list(
      create = function(common = unspecified(), input = unspecified(), condition = unspecified(), advanced_extension = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        input = clean_value(input, "substrait.Rel"),
        condition = clean_value(condition, "substrait.Expression"),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
        .qualified_name = "substrait.FilterRel"
      )
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
        create = function(value) create_substrait_enum(
          value,
          .qualified_name = "substrait.JoinRel.JoinType"
        )
      ),
      create = function(common = unspecified(), left = unspecified(), right = unspecified(), expression = unspecified(), post_join_filter = unspecified(), type = unspecified(), advanced_extension = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        left = clean_value(left, "substrait.Rel"),
        right = clean_value(right, "substrait.Rel"),
        expression = clean_value(expression, "substrait.Expression"),
        post_join_filter = clean_value(post_join_filter, "substrait.Expression"),
        type = clean_value(type, "substrait.JoinRel.JoinType"),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
        .qualified_name = "substrait.JoinRel"
      )
  ),
  ProjectRel = list(
      create = function(common = unspecified(), input = unspecified(), expressions = unspecified(), advanced_extension = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        input = clean_value(input, "substrait.Rel"),
        expressions = clean_value(expressions, "substrait.Expression"),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
        .qualified_name = "substrait.ProjectRel"
      )
  ),
  ReadRel = list(
      ExtensionTable = list(
            create = function(detail = unspecified()) create_substrait_message(
              detail = clean_value(detail, "substrait.Any"),
              .qualified_name = "substrait.ReadRel.ExtensionTable"
            )
      ),
      LocalFiles = list(
            FileOrFiles = list(
                    FileFormat = list(
                      FILE_FORMAT_UNSPECIFIED = 0,
                      FILE_FORMAT_PARQUET = 1,
                      create = function(value) create_substrait_enum(
                        value,
                        .qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles.FileFormat"
                      )
                    ),
                    create = function(uri_path = unspecified(), uri_path_glob = unspecified(), uri_file = unspecified(), uri_folder = unspecified(), format = unspecified(), partition_index = unspecified(), start = unspecified(), length = unspecified()) create_substrait_message(
                      uri_path = clean_value(uri_path, "TYPE_STRING"),
                      uri_path_glob = clean_value(uri_path_glob, "TYPE_STRING"),
                      uri_file = clean_value(uri_file, "TYPE_STRING"),
                      uri_folder = clean_value(uri_folder, "TYPE_STRING"),
                      format = clean_value(format, "substrait.ReadRel.LocalFiles.FileOrFiles.FileFormat"),
                      partition_index = clean_value(partition_index, "TYPE_UINT64"),
                      start = clean_value(start, "TYPE_UINT64"),
                      length = clean_value(length, "TYPE_UINT64"),
                      .qualified_name = "substrait.ReadRel.LocalFiles.FileOrFiles"
                    )
            ),
            create = function(items = unspecified(), advanced_extension = unspecified()) create_substrait_message(
              items = clean_value(items, "substrait.ReadRel.LocalFiles.FileOrFiles"),
              advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
              .qualified_name = "substrait.ReadRel.LocalFiles"
            )
      ),
      NamedTable = list(
            create = function(names = unspecified(), advanced_extension = unspecified()) create_substrait_message(
              names = clean_value(names, "TYPE_STRING"),
              advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
              .qualified_name = "substrait.ReadRel.NamedTable"
            )
      ),
      VirtualTable = list(
            create = function(values = unspecified()) create_substrait_message(
              values = clean_value(values, "substrait.Expression.Literal.Struct"),
              .qualified_name = "substrait.ReadRel.VirtualTable"
            )
      ),
      create = function(common = unspecified(), base_schema = unspecified(), filter = unspecified(), projection = unspecified(), advanced_extension = unspecified(), virtual_table = unspecified(), local_files = unspecified(), named_table = unspecified(), extension_table = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        base_schema = clean_value(base_schema, "substrait.NamedStruct"),
        filter = clean_value(filter, "substrait.Expression"),
        projection = clean_value(projection, "substrait.Expression.MaskExpression"),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
        virtual_table = clean_value(virtual_table, "substrait.ReadRel.VirtualTable"),
        local_files = clean_value(local_files, "substrait.ReadRel.LocalFiles"),
        named_table = clean_value(named_table, "substrait.ReadRel.NamedTable"),
        extension_table = clean_value(extension_table, "substrait.ReadRel.ExtensionTable"),
        .qualified_name = "substrait.ReadRel"
      )
  ),
  Rel = list(
      create = function(read = unspecified(), filter = unspecified(), fetch = unspecified(), aggregate = unspecified(), sort = unspecified(), join = unspecified(), project = unspecified(), set = unspecified(), extension_single = unspecified(), extension_multi = unspecified(), extension_leaf = unspecified()) create_substrait_message(
        read = clean_value(read, "substrait.ReadRel"),
        filter = clean_value(filter, "substrait.FilterRel"),
        fetch = clean_value(fetch, "substrait.FetchRel"),
        aggregate = clean_value(aggregate, "substrait.AggregateRel"),
        sort = clean_value(sort, "substrait.SortRel"),
        join = clean_value(join, "substrait.JoinRel"),
        project = clean_value(project, "substrait.ProjectRel"),
        set = clean_value(set, "substrait.SetRel"),
        extension_single = clean_value(extension_single, "substrait.ExtensionSingleRel"),
        extension_multi = clean_value(extension_multi, "substrait.ExtensionMultiRel"),
        extension_leaf = clean_value(extension_leaf, "substrait.ExtensionLeafRel"),
        .qualified_name = "substrait.Rel"
      )
  ),
  RelCommon = list(
      Direct = list(
            create = function() create_substrait_message(
              .qualified_name = "substrait.RelCommon.Direct"
            )
      ),
      Emit = list(
            create = function(output_mapping = unspecified()) create_substrait_message(
              output_mapping = clean_value(output_mapping, "TYPE_INT32"),
              .qualified_name = "substrait.RelCommon.Emit"
            )
      ),
      Hint = list(
            RuntimeConstraint = list(
                    create = function(advanced_extension = unspecified()) create_substrait_message(
                      advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
                      .qualified_name = "substrait.RelCommon.Hint.RuntimeConstraint"
                    )
            ),
            Stats = list(
                    create = function(row_count = unspecified(), record_size = unspecified(), advanced_extension = unspecified()) create_substrait_message(
                      row_count = clean_value(row_count, "TYPE_DOUBLE"),
                      record_size = clean_value(record_size, "TYPE_DOUBLE"),
                      advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
                      .qualified_name = "substrait.RelCommon.Hint.Stats"
                    )
            ),
            create = function(stats = unspecified(), constraint = unspecified(), advanced_extension = unspecified()) create_substrait_message(
              stats = clean_value(stats, "substrait.RelCommon.Hint.Stats"),
              constraint = clean_value(constraint, "substrait.RelCommon.Hint.RuntimeConstraint"),
              advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
              .qualified_name = "substrait.RelCommon.Hint"
            )
      ),
      create = function(direct = unspecified(), emit = unspecified(), hint = unspecified(), advanced_extension = unspecified()) create_substrait_message(
        direct = clean_value(direct, "substrait.RelCommon.Direct"),
        emit = clean_value(emit, "substrait.RelCommon.Emit"),
        hint = clean_value(hint, "substrait.RelCommon.Hint"),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
        .qualified_name = "substrait.RelCommon"
      )
  ),
  RelRoot = list(
      create = function(input = unspecified(), names = unspecified()) create_substrait_message(
        input = clean_value(input, "substrait.Rel"),
        names = clean_value(names, "TYPE_STRING"),
        .qualified_name = "substrait.RelRoot"
      )
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
        create = function(value) create_substrait_enum(
          value,
          .qualified_name = "substrait.SetRel.SetOp"
        )
      ),
      create = function(common = unspecified(), inputs = unspecified(), op = unspecified(), advanced_extension = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        inputs = clean_value(inputs, "substrait.Rel"),
        op = clean_value(op, "substrait.SetRel.SetOp"),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
        .qualified_name = "substrait.SetRel"
      )
  ),
  SortRel = list(
      create = function(common = unspecified(), input = unspecified(), sorts = unspecified(), advanced_extension = unspecified()) create_substrait_message(
        common = clean_value(common, "substrait.RelCommon"),
        input = clean_value(input, "substrait.Rel"),
        sorts = clean_value(sorts, "substrait.SortField"),
        advanced_extension = clean_value(advanced_extension, "substrait.extensions.AdvancedExtension"),
        .qualified_name = "substrait.SortRel"
      )
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
              create = function(value) create_substrait_enum(
                value,
                .qualified_name = "substrait.DerivationExpression.BinaryOp.BinaryOpType"
              )
            ),
            create = function(op_type = unspecified(), arg1 = unspecified(), arg2 = unspecified()) create_substrait_message(
              op_type = clean_value(op_type, "substrait.DerivationExpression.BinaryOp.BinaryOpType"),
              arg1 = clean_value(arg1, "substrait.DerivationExpression"),
              arg2 = clean_value(arg2, "substrait.DerivationExpression"),
              .qualified_name = "substrait.DerivationExpression.BinaryOp"
            )
      ),
      ExpressionDecimal = list(
            create = function(scale = unspecified(), precision = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              scale = clean_value(scale, "substrait.DerivationExpression"),
              precision = clean_value(precision, "substrait.DerivationExpression"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.DerivationExpression.ExpressionDecimal"
            )
      ),
      ExpressionFixedBinary = list(
            create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              length = clean_value(length, "substrait.DerivationExpression"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.DerivationExpression.ExpressionFixedBinary"
            )
      ),
      ExpressionFixedChar = list(
            create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              length = clean_value(length, "substrait.DerivationExpression"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.DerivationExpression.ExpressionFixedChar"
            )
      ),
      ExpressionList = list(
            create = function(type = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              type = clean_value(type, "substrait.DerivationExpression"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.DerivationExpression.ExpressionList"
            )
      ),
      ExpressionMap = list(
            create = function(key = unspecified(), value = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              key = clean_value(key, "substrait.DerivationExpression"),
              value = clean_value(value, "substrait.DerivationExpression"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.DerivationExpression.ExpressionMap"
            )
      ),
      ExpressionNamedStruct = list(
            create = function(names = unspecified(), struct_ = unspecified()) create_substrait_message(
              names = clean_value(names, "TYPE_STRING"),
              struct_ = clean_value(struct_, "substrait.DerivationExpression.ExpressionStruct"),
              .qualified_name = "substrait.DerivationExpression.ExpressionNamedStruct"
            )
      ),
      ExpressionStruct = list(
            create = function(types = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              types = clean_value(types, "substrait.DerivationExpression"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.DerivationExpression.ExpressionStruct"
            )
      ),
      ExpressionVarChar = list(
            create = function(length = unspecified(), variation_pointer = unspecified(), nullability = unspecified()) create_substrait_message(
              length = clean_value(length, "substrait.DerivationExpression"),
              variation_pointer = clean_value(variation_pointer, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.DerivationExpression.ExpressionVarChar"
            )
      ),
      IfElse = list(
            create = function(if_condition = unspecified(), if_return = unspecified(), else_return = unspecified()) create_substrait_message(
              if_condition = clean_value(if_condition, "substrait.DerivationExpression"),
              if_return = clean_value(if_return, "substrait.DerivationExpression"),
              else_return = clean_value(else_return, "substrait.DerivationExpression"),
              .qualified_name = "substrait.DerivationExpression.IfElse"
            )
      ),
      ReturnProgram = list(
            Assignment = list(
                    create = function(name = unspecified(), expression = unspecified()) create_substrait_message(
                      name = clean_value(name, "TYPE_STRING"),
                      expression = clean_value(expression, "substrait.DerivationExpression"),
                      .qualified_name = "substrait.DerivationExpression.ReturnProgram.Assignment"
                    )
            ),
            create = function(assignments = unspecified(), final_expression = unspecified()) create_substrait_message(
              assignments = clean_value(assignments, "substrait.DerivationExpression.ReturnProgram.Assignment"),
              final_expression = clean_value(final_expression, "substrait.DerivationExpression"),
              .qualified_name = "substrait.DerivationExpression.ReturnProgram"
            )
      ),
      UnaryOp = list(
            UnaryOpType = list(
              UNARY_OP_TYPE_UNSPECIFIED = 0,
              UNARY_OP_TYPE_BOOLEAN_NOT = 1,
              create = function(value) create_substrait_enum(
                value,
                .qualified_name = "substrait.DerivationExpression.UnaryOp.UnaryOpType"
              )
            ),
            create = function(op_type = unspecified(), arg = unspecified()) create_substrait_message(
              op_type = clean_value(op_type, "substrait.DerivationExpression.UnaryOp.UnaryOpType"),
              arg = clean_value(arg, "substrait.DerivationExpression"),
              .qualified_name = "substrait.DerivationExpression.UnaryOp"
            )
      ),
      create = function(bool_ = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year = unspecified(), interval_day = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), fixed_char = unspecified(), varchar = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), list = unspecified(), map = unspecified(), user_defined_pointer = unspecified(), type_parameter_name = unspecified(), integer_parameter_name = unspecified(), integer_literal = unspecified(), unary_op = unspecified(), binary_op = unspecified(), if_else = unspecified(), return_program = unspecified()) create_substrait_message(
        bool_ = clean_value(bool_, "substrait.Type.Boolean"),
        i8 = clean_value(i8, "substrait.Type.I8"),
        i16 = clean_value(i16, "substrait.Type.I16"),
        i32 = clean_value(i32, "substrait.Type.I32"),
        i64 = clean_value(i64, "substrait.Type.I64"),
        fp32 = clean_value(fp32, "substrait.Type.FP32"),
        fp64 = clean_value(fp64, "substrait.Type.FP64"),
        string = clean_value(string, "substrait.Type.String"),
        binary = clean_value(binary, "substrait.Type.Binary"),
        timestamp = clean_value(timestamp, "substrait.Type.Timestamp"),
        date = clean_value(date, "substrait.Type.Date"),
        time = clean_value(time, "substrait.Type.Time"),
        interval_year = clean_value(interval_year, "substrait.Type.IntervalYear"),
        interval_day = clean_value(interval_day, "substrait.Type.IntervalDay"),
        timestamp_tz = clean_value(timestamp_tz, "substrait.Type.TimestampTZ"),
        uuid = clean_value(uuid, "substrait.Type.UUID"),
        fixed_char = clean_value(fixed_char, "substrait.DerivationExpression.ExpressionFixedChar"),
        varchar = clean_value(varchar, "substrait.DerivationExpression.ExpressionVarChar"),
        fixed_binary = clean_value(fixed_binary, "substrait.DerivationExpression.ExpressionFixedBinary"),
        decimal = clean_value(decimal, "substrait.DerivationExpression.ExpressionDecimal"),
        struct_ = clean_value(struct_, "substrait.DerivationExpression.ExpressionStruct"),
        list = clean_value(list, "substrait.DerivationExpression.ExpressionList"),
        map = clean_value(map, "substrait.DerivationExpression.ExpressionMap"),
        user_defined_pointer = clean_value(user_defined_pointer, "TYPE_UINT32"),
        type_parameter_name = clean_value(type_parameter_name, "TYPE_STRING"),
        integer_parameter_name = clean_value(integer_parameter_name, "TYPE_STRING"),
        integer_literal = clean_value(integer_literal, "TYPE_INT32"),
        unary_op = clean_value(unary_op, "substrait.DerivationExpression.UnaryOp"),
        binary_op = clean_value(binary_op, "substrait.DerivationExpression.BinaryOp"),
        if_else = clean_value(if_else, "substrait.DerivationExpression.IfElse"),
        return_program = clean_value(return_program, "substrait.DerivationExpression.ReturnProgram"),
        .qualified_name = "substrait.DerivationExpression"
      )
  ),
  NamedStruct = list(
      create = function(names = unspecified(), struct_ = unspecified()) create_substrait_message(
        names = clean_value(names, "TYPE_STRING"),
        struct_ = clean_value(struct_, "substrait.Expression.Literal.Struct"),
        .qualified_name = "substrait.NamedStruct"
      )
  ),
  Type = list(
      Nullability = list(
        NULLABILITY_UNSPECIFIED = 0,
        NULLABILITY_NULLABLE = 1,
        NULLABILITY_REQUIRED = 2,
        create = function(value) create_substrait_enum(
          value,
          .qualified_name = "substrait.Type.Nullability"
        )
      ),
      Binary = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.Binary"
            )
      ),
      Boolean = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.Boolean"
            )
      ),
      Date = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.Date"
            )
      ),
      Decimal = list(
            create = function(scale = unspecified(), precision = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              scale = clean_value(scale, "TYPE_INT32"),
              precision = clean_value(precision, "TYPE_INT32"),
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.Decimal"
            )
      ),
      FP32 = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.FP32"
            )
      ),
      FP64 = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.FP64"
            )
      ),
      FixedBinary = list(
            create = function(length = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              length = clean_value(length, "TYPE_INT32"),
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.FixedBinary"
            )
      ),
      FixedChar = list(
            create = function(length = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              length = clean_value(length, "TYPE_INT32"),
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.FixedChar"
            )
      ),
      I16 = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.I16"
            )
      ),
      I32 = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.I32"
            )
      ),
      I64 = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.I64"
            )
      ),
      I8 = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.I8"
            )
      ),
      IntervalDay = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.IntervalDay"
            )
      ),
      IntervalYear = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.IntervalYear"
            )
      ),
      List = list(
            create = function(type = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type = clean_value(type, "substrait.FunctionSignature.Implementation.Type"),
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.List"
            )
      ),
      Map = list(
            create = function(key = unspecified(), value = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              key = clean_value(key, "substrait.FunctionSignature.Implementation.Type"),
              value = clean_value(value, "substrait.FunctionSignature.Implementation.Type"),
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.Map"
            )
      ),
      String = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.String"
            )
      ),
      Struct = list(
            create = function(types = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              types = clean_value(types, "substrait.FunctionSignature.Implementation.Type"),
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.Struct"
            )
      ),
      Time = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.Time"
            )
      ),
      Timestamp = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.Timestamp"
            )
      ),
      TimestampTZ = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.TimestampTZ"
            )
      ),
      UUID = list(
            create = function(type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.UUID"
            )
      ),
      VarChar = list(
            create = function(length = unspecified(), type_variation_reference = unspecified(), nullability = unspecified()) create_substrait_message(
              length = clean_value(length, "TYPE_INT32"),
              type_variation_reference = clean_value(type_variation_reference, "TYPE_UINT32"),
              nullability = clean_value(nullability, "substrait.Type.Nullability"),
              .qualified_name = "substrait.Type.VarChar"
            )
      ),
      create = function(bool_ = unspecified(), i8 = unspecified(), i16 = unspecified(), i32 = unspecified(), i64 = unspecified(), fp32 = unspecified(), fp64 = unspecified(), string = unspecified(), binary = unspecified(), timestamp = unspecified(), date = unspecified(), time = unspecified(), interval_year = unspecified(), interval_day = unspecified(), timestamp_tz = unspecified(), uuid = unspecified(), fixed_char = unspecified(), varchar = unspecified(), fixed_binary = unspecified(), decimal = unspecified(), struct_ = unspecified(), list = unspecified(), map = unspecified(), user_defined_type_reference = unspecified()) create_substrait_message(
        bool_ = clean_value(bool_, "substrait.Type.Boolean"),
        i8 = clean_value(i8, "substrait.Type.I8"),
        i16 = clean_value(i16, "substrait.Type.I16"),
        i32 = clean_value(i32, "substrait.Type.I32"),
        i64 = clean_value(i64, "substrait.Type.I64"),
        fp32 = clean_value(fp32, "substrait.Type.FP32"),
        fp64 = clean_value(fp64, "substrait.Type.FP64"),
        string = clean_value(string, "substrait.Type.String"),
        binary = clean_value(binary, "substrait.Type.Binary"),
        timestamp = clean_value(timestamp, "substrait.Type.Timestamp"),
        date = clean_value(date, "substrait.Type.Date"),
        time = clean_value(time, "substrait.Type.Time"),
        interval_year = clean_value(interval_year, "substrait.Type.IntervalYear"),
        interval_day = clean_value(interval_day, "substrait.Type.IntervalDay"),
        timestamp_tz = clean_value(timestamp_tz, "substrait.Type.TimestampTZ"),
        uuid = clean_value(uuid, "substrait.Type.UUID"),
        fixed_char = clean_value(fixed_char, "substrait.Type.FixedChar"),
        varchar = clean_value(varchar, "substrait.Expression.Literal.VarChar"),
        fixed_binary = clean_value(fixed_binary, "substrait.Type.FixedBinary"),
        decimal = clean_value(decimal, "substrait.Expression.Literal.Decimal"),
        struct_ = clean_value(struct_, "substrait.Expression.Literal.Struct"),
        list = clean_value(list, "substrait.Expression.Literal.List"),
        map = clean_value(map, "substrait.Expression.Literal.Map"),
        user_defined_type_reference = clean_value(user_defined_type_reference, "TYPE_UINT32"),
        .qualified_name = "substrait.Type"
      )
  )
)
