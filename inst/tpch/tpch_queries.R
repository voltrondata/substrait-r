# The queries shown here are adapted by those created for arrowbench: https://github.com/voltrondata-labs/arrowbench/blob/main/R/tpch-queries.R
# There should be very few differences beyond adding in calls to `dplyr::collect()` to objects which are to be used as the right-hand side of joins

tpc_h_queries <- list()

tpc_h_queries[[1]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  input_func("lineitem") %>%
    select(l_shipdate, l_returnflag, l_linestatus, l_quantity,
           l_extendedprice, l_discount, l_tax) %>%
    # kludge, should be: filter(l_shipdate <= "1998-12-01" - interval x day) %>%
    # where x is between 60 and 120, 90 is the only one that will validate.
    filter(l_shipdate <= as.Date("1998-09-02")) %>%
    select(l_returnflag, l_linestatus, l_quantity, l_extendedprice, l_discount, l_tax) %>%
    group_by(l_returnflag, l_linestatus) %>%
    summarise(
      sum_qty = sum(na.rm = TRUE, l_quantity),
      sum_base_price = sum(na.rm = TRUE, l_extendedprice),
      sum_disc_price = sum(na.rm = TRUE, l_extendedprice * (1 - l_discount)),
      sum_charge = sum(na.rm = TRUE, l_extendedprice * (1 - l_discount) * (1 + l_tax)),
      avg_qty = mean(na.rm = TRUE, l_quantity),
      avg_price = mean(na.rm = TRUE, l_extendedprice),
      avg_disc = mean(na.rm = TRUE, l_discount),
      count_order = n()
    ) %>%
    ungroup() %>%
    arrange(l_returnflag, l_linestatus) %>%
    collect_func()
}

tpc_h_queries[[2]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  ps <- input_func("partsupp") %>% select(ps_partkey, ps_suppkey, ps_supplycost) %>%
    collect()

  p <- input_func("part") %>%
    select(p_partkey, p_type, p_size, p_mfgr) %>%
    filter(p_size == 15, grepl(".*BRASS$", p_type)) %>%
    select(p_partkey, p_mfgr)
  psp <- inner_join(p, ps, by = c("p_partkey" = "ps_partkey")) %>%
    collect()

  sp <- input_func("supplier") %>%
    select(s_suppkey, s_nationkey, s_acctbal, s_name,
           s_address, s_phone, s_comment) %>%
    collect()

  psps <- inner_join(psp, sp,
                     by = c("ps_suppkey" = "s_suppkey")) %>%
    select(p_partkey, ps_supplycost, p_mfgr, s_nationkey,
           s_acctbal, s_name, s_address, s_phone, s_comment)

  nr <- inner_join(input_func("nation"),
                   input_func("region") %>% collect() %>% filter(r_name == "EUROPE"),
                   by = c("n_regionkey" = "r_regionkey")) %>%
    select(n_nationkey, n_name) %>%
    collect()

  pspsnr <- inner_join(psps, nr,
                       by = c("s_nationkey" = "n_nationkey")) %>%
    select(p_partkey, ps_supplycost, p_mfgr, n_name, s_acctbal,
           s_name, s_address, s_phone, s_comment)

  aggr <- pspsnr %>%
    group_by(p_partkey) %>%
    summarise(min_ps_supplycost = min(ps_supplycost)) %>%
    collect()

  sj <- inner_join(pspsnr, aggr,
                   by=c("p_partkey" = "p_partkey", "ps_supplycost" = "min_ps_supplycost"))


  sj %>%
    select(s_acctbal, s_name, n_name, p_partkey, p_mfgr,
           s_address, s_phone, s_comment) %>%
    arrange(desc(s_acctbal), n_name, s_name, p_partkey) %>%
    head(100) %>%
    collect_func()
}

tpc_h_queries[[3]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  oc <- inner_join(
    input_func("orders") %>%
      select(o_orderkey, o_custkey, o_orderdate, o_shippriority) %>%
      # kludge, should be: filter(o_orderdate < "1995-03-15"),
      filter(o_orderdate < as.Date("1995-03-15")),
    input_func("customer") %>%
      select(c_custkey, c_mktsegment) %>%
      filter(c_mktsegment == "BUILDING") %>% collect(),
    by = c("o_custkey" = "c_custkey")
  ) %>%
    select(o_orderkey, o_orderdate, o_shippriority) %>%
    collect()

  loc <- inner_join(
    input_func("lineitem") %>%
      select(l_orderkey, l_shipdate, l_extendedprice, l_discount) %>%
      filter(l_shipdate > as.Date("1995-03-15")) %>%
      select(l_orderkey, l_extendedprice, l_discount),
    oc, by = c("l_orderkey" = "o_orderkey")
  )

  loc %>% mutate(volume=l_extendedprice * (1 - l_discount)) %>%
    group_by(l_orderkey, o_orderdate, o_shippriority) %>%
    summarise(revenue = sum(na.rm = TRUE, volume)) %>%
    select(l_orderkey, revenue, o_orderdate, o_shippriority) %>%
    ungroup() %>%
    arrange(desc(revenue), o_orderdate) %>%
    head(10) %>%
    collect_func()
}

tpc_h_queries[[4]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  l <- input_func("lineitem") %>%
    select(l_orderkey, l_commitdate, l_receiptdate) %>%
    filter(l_commitdate < l_receiptdate) %>%
    select(l_orderkey)

  o <- input_func("orders") %>%
    select(o_orderkey, o_orderdate, o_orderpriority) %>%
    # kludge: filter(o_orderdate >= "1993-07-01", o_orderdate < "1993-07-01" + interval '3' month) %>%
    filter(o_orderdate >= as.Date("1993-07-01"), o_orderdate < as.Date("1993-10-01")) %>%
    select(o_orderkey, o_orderpriority) %>%
    collect()

  # distinct after join, tested and indeed faster
  lo <- inner_join(l, o, by = c("l_orderkey" = "o_orderkey")) %>%
    distinct() %>%
    select(o_orderpriority)

  lo %>%
    group_by(o_orderpriority) %>%
    summarise(order_count = n()) %>%
    ungroup() %>%
    arrange(o_orderpriority) %>%
    collect_func()
}

tpc_h_queries[[5]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  nr <- inner_join(
    input_func("nation") %>%
      select(n_nationkey, n_regionkey, n_name),
    input_func("region") %>%
      select(r_regionkey, r_name) %>%
      filter(r_name == "ASIA") %>% collect(),
    by = c("n_regionkey" = "r_regionkey")
  ) %>%
    select(n_nationkey, n_name) %>%
    collect()

  snr <- inner_join(
    input_func("supplier") %>%
      select(s_suppkey, s_nationkey),
    nr,
    by = c("s_nationkey" = "n_nationkey")
  ) %>%
    select(s_suppkey, s_nationkey, n_name) %>%
    collect()

  lsnr <- inner_join(
    input_func("lineitem") %>% select(l_suppkey, l_orderkey, l_extendedprice, l_discount),
    snr, by = c("l_suppkey" = "s_suppkey")) %>%
    collect()

  o <- input_func("orders") %>%
    select(o_orderdate, o_orderkey, o_custkey) %>%
    # kludge: filter(o_orderdate >= "1994-01-01", o_orderdate < "1994-01-01" + interval '1' year) %>%
    filter(o_orderdate >= as.Date("1994-01-01"), o_orderdate < as.Date("1995-01-01")) %>%
    select(o_orderkey, o_custkey) %>%
    collect()

  oc <- inner_join(o, input_func("customer") %>% select(c_custkey, c_nationkey),
                   by = c("o_custkey" = "c_custkey")) %>%
    select(o_orderkey, c_nationkey) %>%
    collect()

  lsnroc <- inner_join(lsnr, oc,
                       by = c("l_orderkey" = "o_orderkey", "s_nationkey" = "c_nationkey")) %>%
    select(l_extendedprice, l_discount, n_name) %>% collect()

  lsnroc %>%
    mutate(volume=l_extendedprice * (1 - l_discount)) %>%
    group_by(n_name) %>%
    summarise(revenue = sum(na.rm = TRUE, volume)) %>%
    ungroup() %>%
    arrange(desc(revenue)) %>%
    collect_func()
}

tpc_h_queries[[6]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  input_func("lineitem") %>%
    select(l_shipdate, l_extendedprice, l_discount, l_quantity) %>%
    # kludge, should be: filter(l_shipdate >= "1994-01-01",
    filter(l_shipdate >= as.Date("1994-01-01"),
           # kludge: should be: l_shipdate < "1994-01-01" + interval '1' year,
           l_shipdate < as.Date("1995-01-01"),
           # Should be the following, but https://issues.apache.org/jira/browse/ARROW-14125
           # Need to round because 0.06 - 0.01 != 0.05
           l_discount >= round(0.06 - 0.01, digits = 2),
           l_discount <= round(0.06 + 0.01, digits = 2),
           # l_discount >= 0.05,
           # l_discount <= 0.07,
           l_quantity < 24) %>%
    select(l_extendedprice, l_discount) %>%
    summarise(revenue = sum(na.rm = TRUE, l_extendedprice * l_discount)) %>%
    collect_func()
}

tpc_h_queries[[7]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  sn <- inner_join(
    input_func("supplier") %>%
      select(s_nationkey, s_suppkey),
    input_func("nation") %>%
      select(n1_nationkey = n_nationkey, n1_name = n_name) %>%
      filter(n1_name %in% c("FRANCE", "GERMANY")) %>% collect(),
    by = c("s_nationkey" = "n1_nationkey")) %>%
    select(s_suppkey, n1_name) %>%
    collect()

  cn <- inner_join(
    input_func("customer") %>%
      select(c_custkey, c_nationkey),
    input_func("nation") %>%
      select(n2_nationkey = n_nationkey, n2_name = n_name) %>%
      filter(n2_name %in% c("FRANCE", "GERMANY"))  %>% collect(),
    by = c("c_nationkey" = "n2_nationkey")) %>%
    select(c_custkey, n2_name) %>%
    collect()

  cno <- inner_join(
    input_func("orders") %>%
      select(o_custkey, o_orderkey),
    cn, by = c("o_custkey" = "c_custkey")) %>%
    select(o_orderkey, n2_name) %>% collect()

  cnol <- inner_join(
    input_func("lineitem") %>%
      select(l_orderkey, l_suppkey, l_shipdate, l_extendedprice, l_discount) %>%
      # kludge, should be: filter(l_shipdate >= "1995-01-01", l_shipdate <= "1996-12-31"),
      filter(l_shipdate >= as.Date("1995-01-01"), l_shipdate <= as.Date("1996-12-31")),
    cno,
    by = c("l_orderkey" = "o_orderkey")) %>%
    select(l_suppkey, l_shipdate, l_extendedprice, l_discount, n2_name) %>% collect()

  all <- inner_join(cnol, sn, by = c("l_suppkey" = "s_suppkey"))

  all %>%
    filter((n1_name == "FRANCE" & n2_name == "GERMANY") |
             (n1_name == "GERMANY" & n2_name == "FRANCE")) %>%
    mutate(
      supp_nation = n1_name,
      cust_nation = n2_name,
      # kludge (?) l_year = as.integer(strftime(l_shipdate, "%Y")),
      l_year = year(l_shipdate),
      volume = l_extendedprice * (1 - l_discount)) %>%
    select(supp_nation, cust_nation, l_year, volume) %>%
    group_by(supp_nation, cust_nation, l_year) %>%
    summarise(revenue = sum(na.rm = TRUE, volume)) %>%
    ungroup() %>%
    arrange(supp_nation, cust_nation, l_year) %>%
    collect_func()
}

tpc_h_queries[[8]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  # kludge, swapped the table order around because of ARROW-14184
  # nr <- inner_join(
  #   input_func("nation") %>%
  #     select(n1_nationkey = n_nationkey, n1_regionkey = n_regionkey),
  #   input_func("region") %>%
  #     select(r_regionkey, r_name) %>%
  #     filter(r_name == "AMERICA") %>%
  #     select(r_regionkey),
  #   by = c("n1_regionkey" = "r_regionkey")) %>%
  #   select(n1_nationkey)
  nr <- inner_join(
    input_func("region") %>%
      select(r_regionkey, r_name) %>%
      filter(r_name == "AMERICA") %>%
      select(r_regionkey),
    input_func("nation") %>%
      select(n1_nationkey = n_nationkey, n1_regionkey = n_regionkey) %>% collect(),
    by = c("r_regionkey" = "n1_regionkey")) %>%
    select(n1_nationkey) %>% collect()

  cnr <- inner_join(
    input_func("customer") %>%
      select(c_custkey, c_nationkey),
    nr, by = c("c_nationkey" = "n1_nationkey")) %>%
    select(c_custkey) %>% collect()

  ocnr <- inner_join(
    input_func("orders") %>%
      select(o_orderkey, o_custkey, o_orderdate) %>%
      # bludge, should be: filter(o_orderdate >= "1995-01-01", o_orderdate <= "1996-12-31"),
      filter(o_orderdate >= as.Date("1995-01-01"), o_orderdate <= as.Date("1996-12-31")),
    cnr, by = c("o_custkey" = "c_custkey")) %>%
    select(o_orderkey, o_orderdate) %>% collect()

  locnr <- inner_join(
    input_func("lineitem") %>%
      select(l_orderkey, l_partkey, l_suppkey, l_extendedprice, l_discount),
    ocnr, by=c("l_orderkey" = "o_orderkey")) %>%
    select(l_partkey, l_suppkey, l_extendedprice, l_discount, o_orderdate)

  locnrp <- inner_join(locnr,
                       input_func("part") %>%
                         select(p_partkey, p_type) %>%
                         filter(p_type == "ECONOMY ANODIZED STEEL") %>%
                         select(p_partkey) %>% collect(),
                       by = c("l_partkey" = "p_partkey")) %>%
    select(l_suppkey, l_extendedprice, l_discount, o_orderdate)

  locnrps <- inner_join(locnrp,
                        input_func("supplier") %>%
                          select(s_suppkey, s_nationkey) %>% collect(),
                        by = c("l_suppkey" = "s_suppkey")) %>%
    select(l_extendedprice, l_discount, o_orderdate, s_nationkey)

  all <- inner_join(locnrps,
                    input_func("nation") %>%
                      select(n2_nationkey = n_nationkey, n2_name = n_name) %>% collect(),
                    by = c("s_nationkey" = "n2_nationkey")) %>%
    select(l_extendedprice, l_discount, o_orderdate, n2_name)

  all %>%
    mutate(
      # kludge(?), o_year = as.integer(strftime(o_orderdate, "%Y")),
      o_year = year(o_orderdate),
      volume = l_extendedprice * (1 - l_discount),
      nation = n2_name) %>%
    select(o_year, volume, nation) %>%
    group_by(o_year) %>%
    summarise(mkt_share = sum(na.rm = TRUE, ifelse(nation == "BRAZIL", volume, 0)) / sum(na.rm = TRUE, volume)) %>%
    ungroup() %>%
    arrange(o_year) %>%
    collect_func()
}

tpc_h_queries[[9]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  p <- input_func("part") %>%
    select(p_name, p_partkey) %>%
    filter(grepl(".*green.*", p_name)) %>%
    select(p_partkey)  %>% collect()

  psp <- inner_join(
    input_func("partsupp") %>%
      select(ps_suppkey, ps_partkey, ps_supplycost),
    p, by = c("ps_partkey" = "p_partkey"))

  sn <- inner_join(
    input_func("supplier") %>%
      select(s_suppkey, s_nationkey),
    input_func("nation") %>%
      select(n_nationkey, n_name) %>% collect(),
    by = c("s_nationkey" = "n_nationkey")) %>%
    select(s_suppkey, n_name) %>% collect()

  pspsn <- inner_join(psp, sn, by = c("ps_suppkey" = "s_suppkey")) %>% collect()

  lpspsn <- inner_join(
    input_func("lineitem") %>%
      select(l_suppkey, l_partkey, l_orderkey, l_extendedprice, l_discount, l_quantity),
    pspsn,
    by = c("l_suppkey" = "ps_suppkey", "l_partkey" = "ps_partkey")) %>%
    select(l_orderkey, l_extendedprice, l_discount, l_quantity, ps_supplycost, n_name) %>% collect()

  all <- inner_join(
    input_func("orders") %>%
      select(o_orderkey, o_orderdate),
    lpspsn,
    by = c("o_orderkey"= "l_orderkey" )) %>%
    select(l_extendedprice, l_discount, l_quantity, ps_supplycost, n_name, o_orderdate)

  all %>%
    mutate(
      nation = n_name,
      # kludge, o_year = as.integer(format(o_orderdate, "%Y")),
      # also ARROW-14200
      o_year = year(o_orderdate),
      amount = l_extendedprice * (1 - l_discount) - ps_supplycost * l_quantity) %>%
    select(nation, o_year, amount) %>%
    group_by(nation, o_year) %>%
    summarise(sum_profit = sum(na.rm = TRUE, amount)) %>%
    ungroup() %>%
    arrange(nation, desc(o_year)) %>%
    collect_func()
}

tpc_h_queries[[10]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  l <- input_func("lineitem") %>%
    select(l_orderkey, l_returnflag, l_extendedprice, l_discount) %>%
    filter(l_returnflag == "R") %>%
    select(l_orderkey, l_extendedprice, l_discount)

  o <- input_func("orders") %>%
    select(o_orderkey, o_custkey, o_orderdate) %>%
    # kludge, filter(o_orderdate >= "1993-10-01", o_orderdate < "1994-01-01") %>%
    filter(o_orderdate >= as.Date("1993-10-01"), o_orderdate < as.Date("1994-01-01")) %>%
    select(o_orderkey, o_custkey) %>% collect()

  lo <- inner_join(l, o,
                   by = c("l_orderkey" = "o_orderkey")) %>%
    select(l_extendedprice, l_discount, o_custkey)
  # first aggregate, then join with customer/nation,
  # otherwise we need to aggr over lots of cols

  lo_aggr <- lo %>% mutate(volume=l_extendedprice * (1 - l_discount)) %>%
    group_by(o_custkey) %>%
    summarise(revenue = sum(na.rm = TRUE, volume)) %>% collect()

  c <- input_func("customer") %>%
    select(c_custkey, c_nationkey, c_name, c_acctbal, c_phone, c_address, c_comment)

  loc <- inner_join(c, lo_aggr, by = c("c_custkey" = "o_custkey"))

  locn <- inner_join(loc, input_func("nation") %>% select(n_nationkey, n_name) %>% collect(),
                     by = c("c_nationkey" = "n_nationkey"))

  locn %>%
    select(c_custkey, c_name, revenue, c_acctbal, n_name,
           c_address, c_phone, c_comment) %>%
    arrange(desc(revenue)) %>%
    head(20) %>%
    collect_func()
}

tpc_h_queries[[11]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  nation <- input_func("nation") %>%
    filter(n_name == "GERMANY") %>% collect()

  joined_filtered <- input_func("partsupp") %>%
    inner_join(input_func("supplier") %>% collect(), by = c("ps_suppkey" = "s_suppkey")) %>%
    inner_join(nation, by = c("s_nationkey" = "n_nationkey"))

  global_agr <- joined_filtered %>%
    summarise(
      global_value = sum(na.rm = TRUE, ps_supplycost * ps_availqty) * 0.0001000000
    ) %>%
    mutate(global_agr_key = 1L) %>% collect()

  partkey_agr <- joined_filtered %>%
    group_by(ps_partkey) %>%
    summarise(value = sum(na.rm = TRUE, ps_supplycost * ps_availqty))

  partkey_agr %>%
    mutate(global_agr_key = 1L) %>%
    inner_join(global_agr, by = "global_agr_key") %>%
    filter(value > global_value) %>%
    arrange(desc(value)) %>%
    select(ps_partkey, value) %>%
    collect_func()
}

tpc_h_queries[[12]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  input_func("lineitem") %>%
    filter(
      l_shipmode %in% c("MAIL", "SHIP"),
      l_commitdate < l_receiptdate,
      l_shipdate < l_commitdate,
      l_receiptdate >= as.Date("1994-01-01"),
      l_receiptdate < as.Date("1995-01-01")
    ) %>%
    inner_join(
      input_func("orders") %>% collect(),
      by = c("l_orderkey" = "o_orderkey")
    ) %>%
    group_by(l_shipmode) %>%
    summarise(
      high_line_count = sum(na.rm = TRUE,
        if_else(
          (o_orderpriority == "1-URGENT") | (o_orderpriority == "2-HIGH"),
          1L,
          0L
        )
      ),
      low_line_count = sum(na.rm = TRUE,
        if_else(
          (o_orderpriority != "1-URGENT") & (o_orderpriority != "2-HIGH"),
          1L,
          0L
        )
      )
    ) %>%
    ungroup() %>%
    arrange(l_shipmode) %>%
    collect_func()
}

tpc_h_queries[[13]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  c_orders <- input_func("customer") %>%
    left_join(
      input_func("orders") %>%
        filter(!grepl("special.*?requests", o_comment)) %>% collect(),
      by = c("c_custkey" = "o_custkey")
    ) %>%
    group_by(c_custkey) %>%
    summarise(
      c_count = sum(na.rm = TRUE, !is.na(o_orderkey))
    )

  c_orders %>%
    group_by(c_count) %>%
    summarise(custdist = n()) %>%
    ungroup() %>%
    arrange(desc(custdist), desc(c_count)) %>%
    collect_func()
}

tpc_h_queries[[14]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  input_func("lineitem") %>%
    filter(
      l_shipdate >= as.Date("1995-01-01"),
      l_shipdate < as.Date("1995-10-01")
    ) %>%
    inner_join(input_func("part") %>% collect(), by = c("l_partkey" = "p_partkey")) %>%
    summarise(
      promo_revenue = 100 * sum(na.rm = TRUE,
        if_else(grepl("^PROMO", p_type), l_extendedprice * (1 - l_discount), 0)
      ) / sum(na.rm = TRUE, l_extendedprice * (1 - l_discount))
    ) %>%
    collect_func()
}

tpc_h_queries[[15]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  revenue_by_supplier <- input_func("lineitem") %>%
    filter(
      l_shipdate >= as.Date("1996-01-01"),
      l_shipdate < as.Date("1996-04-01")
    ) %>%
    group_by(l_suppkey) %>%
    summarise(
      total_revenue = sum(na.rm = TRUE, l_extendedprice * (1 - l_discount))
    )

  global_revenue <- revenue_by_supplier %>%
    mutate(global_agr_key = 1L) %>%
    group_by(global_agr_key) %>%
    summarise(
      max_total_revenue = max(total_revenue)
    )

  revenue_by_supplier %>%
    mutate(global_agr_key = 1L) %>%
    inner_join(global_revenue %>% collect(), by = "global_agr_key") %>%
    filter(abs(total_revenue - max_total_revenue) < 1e-9) %>%
    inner_join(input_func("supplier") %>% collect(), by = c("l_suppkey" = "s_suppkey")) %>%
    select(s_suppkey = l_suppkey, s_name, s_address, s_phone, total_revenue) %>%
    collect_func()
}

tpc_h_queries[[16]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  part_filtered <- input_func("part") %>%
    filter(
      p_brand != "Brand#45",
      !grepl("^MEDIUM POLISHED", p_type),
      p_size %in% c(49, 14, 23, 45, 19, 3, 36, 9)
    )

  supplier_filtered <- input_func("supplier") %>%
    filter(!grepl("Customer.*?Complaints", s_comment))

  partsupp_filtered <- input_func("partsupp") %>%
    inner_join(supplier_filtered %>% collect(), by = c("ps_suppkey" = "s_suppkey")) %>%
    select(ps_partkey, ps_suppkey)

  part_filtered %>%
    inner_join(partsupp_filtered %>% collect(), by = c("p_partkey" = "ps_partkey")) %>%
    group_by(p_brand, p_type, p_size) %>%
    summarise(supplier_cnt = n_distinct(ps_suppkey)) %>%
    ungroup() %>%
    select(p_brand, p_type, p_size, supplier_cnt) %>%
    arrange(desc(supplier_cnt), p_brand, p_type, p_size) %>%
    collect_func()
}

tpc_h_queries[[17]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  parts_filtered <- input_func("part") %>%
    filter(
      p_brand == "Brand#23",
      p_container == "MED BOX"
    )

  joined <- input_func("lineitem") %>%
    inner_join(parts_filtered %>% collect(), by = c("l_partkey" = "p_partkey"))

  quantity_by_part <- joined %>%
    group_by(l_partkey) %>%
    summarise(quantity_threshold = 0.2 * mean(na.rm = TRUE, l_quantity))

  joined %>%
    inner_join(quantity_by_part %>% collect(), by = "l_partkey") %>%
    filter(l_quantity < quantity_threshold) %>%
    summarise(avg_yearly = sum(na.rm = TRUE, l_extendedprice) / 7.0) %>%
    collect_func()
}

tpc_h_queries[[18]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  big_orders <- input_func("lineitem") %>%
    group_by(l_orderkey) %>%
    summarise(`sum(na.rm = TRUE, l_quantity)` = sum(na.rm = TRUE, l_quantity)) %>%
    filter(`sum(na.rm = TRUE, l_quantity)` > 300)

  input_func("orders") %>%
    inner_join(big_orders %>% collect(), by = c("o_orderkey" = "l_orderkey")) %>%
    inner_join(input_func("customer") %>% collect(), by = c("o_custkey" = "c_custkey")) %>%
    select(
      c_name, c_custkey = o_custkey, o_orderkey,
      o_orderdate, o_totalprice, `sum(na.rm = TRUE, l_quantity)`
    ) %>%
    arrange(desc(o_totalprice), o_orderdate) %>%
    head(100) %>%
    collect_func()
}

tpc_h_queries[[19]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  joined <- input_func("lineitem") %>%
    inner_join(input_func("part") %>% collect(), by = c("l_partkey" = "p_partkey"))

  result <- joined %>%
    filter(
      (
        p_brand == "Brand#12" &
          p_container %in% c('SM CASE', 'SM BOX', 'SM PACK', 'SM PKG') &
          l_quantity >= 1 &
          l_quantity <= (1 + 10) &
          p_size >= 1 &
          p_size <= 5 &
          l_shipmode %in% c("AIR", "AIR REG") &
          l_shipinstruct == "DELIVER IN PERSON"
      ) |
        (
          p_brand == "Brand#23" &
            p_container %in% c('MED BAG', 'MED BOX', 'MED PKG', 'MED PACK') &
            l_quantity >= 10 &
            l_quantity <= (10 + 10) &
            p_size >= 1 &
            p_size <= 10 &
            l_shipmode %in% c("AIR", "AIR REG") &
            l_shipinstruct == "DELIVER IN PERSON"
        ) |
        (
          p_brand == "Brand#34" &
            p_container %in% c('LG CASE', 'LG BOX', 'LG PACK', 'LG PKG') &
            l_quantity >= 20 &
            l_quantity <= (20 + 10) &
            p_size >= 1 &
            p_size <= 15 &
            l_shipmode %in% c("AIR", "AIR REG") &
            l_shipinstruct == "DELIVER IN PERSON"
        )
    )

  result %>%
    summarise(
      revenue = sum(na.rm = TRUE, l_extendedprice * (1 - l_discount))
    ) %>%
    collect_func()
}

tpc_h_queries[[20]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  supplier_ca <- input_func("supplier") %>%
    inner_join(
      input_func("nation") %>% filter(n_name == "CANADA") %>% collect(),
      by = c("s_nationkey" = "n_nationkey")
    ) %>%
    select(s_suppkey, s_name, s_address)

  part_forest <- input_func("part") %>%
    filter(grepl("^forest", p_name))

  partsupp_forest_ca <- input_func("partsupp") %>%
    semi_join(supplier_ca %>% collect(), c("ps_suppkey" = "s_suppkey")) %>%
    semi_join(part_forest %>% collect(), by = c("ps_partkey" = "p_partkey"))

  qty_threshold <- input_func("lineitem") %>%
    filter(
      l_shipdate >= as.Date("1994-01-01"),
      l_shipdate < as.Date("1995-01-01")
    ) %>%
    semi_join(partsupp_forest_ca %>% collect(), by = c("l_partkey" = "ps_partkey", "l_suppkey" = "ps_suppkey")) %>%
    group_by(l_suppkey) %>%
    summarise(qty_threshold = 0.5 * sum(na.rm = TRUE, l_quantity))

  partsupp_forest_ca_filtered <- partsupp_forest_ca %>%
    inner_join(
      qty_threshold %>% collect(),
      by = c("ps_suppkey" = "l_suppkey")
    ) %>%
    filter(ps_availqty > qty_threshold)

  supplier_ca %>%
    semi_join(partsupp_forest_ca_filtered %>% collect(), by = c("s_suppkey" = "ps_suppkey")) %>%
    select(s_name, s_address) %>%
    arrange(s_name) %>%
    collect_func()
}

tpc_h_queries[[21]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  orders_with_more_than_one_supplier <- input_func("lineitem") %>%
    group_by(l_orderkey) %>%
    count(l_suppkey) %>%
    group_by(l_orderkey) %>%
    summarise(n_supplier = n()) %>%
    filter(n_supplier > 1)

  line_items_needed <- input_func("lineitem") %>%
    semi_join(orders_with_more_than_one_supplier %>% collect()) %>%
    inner_join(input_func("orders") %>% collect(), by = c("l_orderkey" = "o_orderkey")) %>%
    filter(o_orderstatus == "F") %>%
    group_by(l_orderkey, l_suppkey) %>%
    summarise(failed_delivery_commit = sum(na.rm = TRUE, ifelse(l_receiptdate > l_commitdate, 1L, 0L))) %>%
    group_by(l_orderkey) %>%
    summarise(n_supplier = n(), num_failed = sum(na.rm = TRUE, failed_delivery_commit)) %>%
    filter(n_supplier > 1 & num_failed == 1)

  line_items <- input_func("lineitem") %>%
    semi_join(line_items_needed %>% collect())

  input_func("supplier") %>%
    inner_join(line_items %>% collect(), by = c("s_suppkey" = "l_suppkey")) %>%
    filter(l_receiptdate > l_commitdate) %>%
    inner_join(input_func("nation") %>% collect(), by = c("s_nationkey" = "n_nationkey")) %>%
    filter(n_name == "SAUDI ARABIA") %>%
    group_by(s_name) %>%
    summarise(numwait = n()) %>%
    ungroup() %>%
    arrange(desc(numwait), s_name) %>%
    head(100) %>%
    collect_func()
}

tpc_h_queries[[22]] <- function(input_func, collect_func = dplyr::collect, con = NULL) {
  acctbal_mins <- input_func("customer") %>%
    filter(
      substr(c_phone, 1, 2) %in% c("13", "31", "23", "29", "30", "18", "17") &
        c_acctbal > 0
    ) %>%
    summarise(acctbal_min = mean(c_acctbal, na.rm = TRUE), join_id = 1L)

  input_func("customer") %>%
    mutate(cntrycode = substr(c_phone, 1, 2), join_id = 1L) %>%
    left_join(acctbal_mins %>% collect(), by = "join_id") %>%
    filter(
      cntrycode %in% c("13", "31", "23", "29", "30", "18", "17") &
        c_acctbal > acctbal_min
    ) %>%
    anti_join(input_func("orders") %>% collect(), by = c("c_custkey" = "o_custkey")) %>%
    select(cntrycode, c_acctbal) %>%
    group_by(cntrycode) %>%
    summarise(
      numcust = n(),
      totacctbal = sum(na.rm = TRUE, c_acctbal)
    ) %>%
    ungroup() %>%
    arrange(cntrycode) %>%
    collect_func()
}

