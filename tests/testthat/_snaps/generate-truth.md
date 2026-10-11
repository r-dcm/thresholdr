# generated truth values include both 0 and 1

    Code
      generate_truth(rep(0, 10))
    Condition
      Error in `generate_truth()`:
      ! Could not generate truth values containing both 0 and 1 values after 100 attempts.
      i Check whether all `estimates` are close to 0 or 1.

---

    Code
      generate_logodds_wt_truth(rep(0, 10), threshold = 0.3)
    Condition
      Error in `generate_logodds_wt_truth()`:
      ! Could not generate truth values containing both 0 and 1 values after 100 attempts.
      i Check whether all `estimates` are close to 0 or 1.

