((magit-commit nil)
 (magit-log
  ("-n256" "--graph" "--decorate"))
 (magit-push
  ("--force-with-lease"))
 (magit-rebase
  ("--autostash"))
 (rg-menu nil))
