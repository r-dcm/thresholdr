# convergence warnings name the attributes

    Code
      warn_attribute_convergence(diagnostics)
    Condition
      Warning:
      Statistics did not converge (Rhat > 1.01) for 2 attributes:
      * "add": `threshold` and `roc_auc`
      * "mult": `threshold`
      i See `attr(x, "diagnostics")` for details.

---

    Code
      warn_attribute_convergence(diagnostics[5:6, ])
    Condition
      Warning:
      Statistics did not converge (Rhat > 1.01) for 1 attribute:
      * "mult": `threshold`
      i See `attr(x, "diagnostics")` for details.

