# substrait_eval_arrow() works

    Code
      substrait_eval_arrow(plan, list())
    Error <rlang_error>
      Named table 'the_name_of_the_table' not found in `tables`

---

    Code
      substrait_eval_arrow(plan, list(the_name_of_the_table = data.frame()))
    Error <rlang_error>
      Base schema for table 'the_name_of_the_table' does not match declared base schema

